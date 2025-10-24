#' Build a UniProt query string compatible with the REST API.
#'
#' @param query Either a length-one character string or a non-empty list of
#'   character vectors describing query tokens.
#' @keywords internal
#' @noRd
build_uniprot_query <- function(query) {
  if (is.null(query)) {
    return(NULL)
  }

  if (is.character(query)) {
    stopifnot(length(query) == 1L, stringi::stri_trim_both(query) != "")
    return(query)
  }

  stopifnot(is.list(query), length(query) > 0)

  tokens <- vapply(
    seq_along(query),
    function(idx) {
      value <- query[[idx]]
      stopifnot(!is.null(value))
      value <- unlist(value, use.names = FALSE)
      stopifnot(length(value) > 0, is.character(value))

      value <- stringi::stri_trim_both(value)
      stopifnot(all(value != ""))

      field <- names(query)[idx]
      field_is_missing <- is.null(field) || is.na(field) || field == "" ||
        stringi::stri_trim_both(field) == ""

      joined_value <- paste(value, collapse = " OR ")
      if (field_is_missing) {
        joined_value
      } else {
        paste0(stringi::stri_trim_both(field), ":(", joined_value, ")")
      }
    },
    character(1)
  )

  paste(tokens, collapse = " AND ")
}

#' Retrieve data from UniProt using a query
#'
#' @description
#' Issue a UniProt REST query and return the result as a tibble. During testing
#' you can supply a mock responder via the `DTIs.mock_response` option to avoid
#' hitting the live service.
#'
#' @param query A character string or list describing the query to submit to
#'   UniProt (see Details).
#' @param columns Character vector of UniProt columns to retrieve.
#'
#' @return A tibble with one row per UniProt entry that matches the query, or
#'   `NULL`/an empty tibble when nothing is retrieved.
#'
#' @details
#' When `query` is a list, each element corresponds to a field. Named elements
#' generate fielded searches (e.g., `drug = "Aceclofenac"` becomes
#' `drug:(Aceclofenac)`), whereas unnamed elements are treated as raw tokens.
#'
#' @examples
#' old_options <- options(
#'   DTIs.mock_response = function(full_query, columns) {
#'     tibble::tibble(
#'       Entry = "P23219",
#'       `Gene Names` = "PTGS1",
#'       Organism = "Homo sapiens (Human)",
#'       Reviewed = "reviewed"
#'     )
#'   }
#' )
#' on.exit(options(old_options), add = TRUE)
#' get_uniprot_data("Aceclofenac")
#'
#' @seealso [uniprot_drug_data()]
#' @export
#' @importFrom httr2 request req_url_query req_perform resp_status resp_body_string
#' @importFrom stringi stri_trim_both
#' @importFrom tibble tibble as_tibble
#' @importFrom utils read.delim
get_uniprot_data <- function(query = NULL, columns = c("id", "genes", "organism",
                                    "reviewed"))
{
  full_query <- build_uniprot_query(query)
  if (is.null(full_query)) {
    return(NULL)
  }

  mock_fn <- getOption("DTIs.mock_response")
  if (is.function(mock_fn)) {
    mocked <- tryCatch(
      mock_fn(full_query, columns),
      error = function(err) {
        message("Mock UniProt responder failed: ", conditionMessage(err))
        structure(list(), class = "DTIs_mock_failure")
      }
    )
    if (inherits(mocked, "DTIs_mock_failure")) {
      return(NULL)
    }
    if (!is.null(mocked)) {
      if (!inherits(mocked, "tbl_df")) {
        mocked <- tibble::as_tibble(mocked)
      }
      return(mocked)
    }
  }

  cols <- paste(columns, collapse = ",")
  request <- httr2::request("https://rest.uniprot.org/uniprotkb/search")
  request <- httr2::req_url_query(
    request,
    query = full_query,
    format = "tsv",
    fields = cols
  )

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(err) {
      message("Failed to retrieve data from UniProt: ", conditionMessage(err))
      NULL
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  status <- httr2::resp_status(response)
  if (status >= 400) {
    message("UniProt request failed with status ", status)
    return(NULL)
  }

  body <- httr2::resp_body_string(response)
  if (!nzchar(body)) {
    return(tibble::tibble())
  }

  parsed <- tryCatch(
    utils::read.delim(
      text = body,
      check.names = FALSE,
      quote = "",
      stringsAsFactors = FALSE
    ),
    error = function(err) {
      message("Failed to parse UniProt response: ", conditionMessage(err))
      NULL
    }
  )

  if (is.null(parsed)) {
    return(NULL)
  }

  tibble::as_tibble(parsed)
}

#' Retrieve gene names and organism information of drugs from UniProt
#'
#' @description
#' Convenience wrapper that builds the recommended UniProt query for a set of
#' drugs and returns the matched entries and primary gene names.
#'
#' @param drug Character vector of drug names.
#'
#' @return A tibble containing the UniProt accession, primary gene symbol,
#'   organism, review status, and the originating drug for each hit.
#'
#' @examples
#' old_options <- options(
#'   DTIs.mock_response = function(full_query, columns) {
#'     if (full_query == "Aceclofenac AND organism_id:(9606) AND reviewed:(true)") {
#'       return(tibble::tibble(
#'         Entry = "P23219",
#'         `Gene Names` = "PTGS1",
#'         Organism = "Homo sapiens (Human)",
#'         Reviewed = "reviewed"
#'       ))
#'     }
#'     tibble::tibble()
#'   }
#' )
#' on.exit(options(old_options), add = TRUE)
#' uniprot_drug_data("Aceclofenac")
#'
#' @seealso [get_uniprot_data()]
#' @export
#' @importFrom stringr word
uniprot_drug_data <- function(drug) {
  query <- lapply(drug, function(x) list(x, "organism_id" = "9606", "reviewed" = "true"))
  res <- lapply(
    query,
    function(x) {
      get_uniprot_data(
        x,
        columns = c("accession", "gene_names", "organism_name", "reviewed")
      )
    }
  )
  mod_res <- lapply(seq_along(res), function(idx) {
    df <- res[[idx]]
    if (is.null(df) || nrow(df) == 0) {
      df <- tibble::tibble(
        Entry = character(),
        `Gene Names` = character(),
        Organism = character(),
        Reviewed = character()
      )
    } else {
      required_cols <- c("Entry", "Gene Names", "Organism", "Reviewed")
      missing_cols <- setdiff(required_cols, names(df))
      if (length(missing_cols) > 0) {
        df[missing_cols] <- replicate(
          length(missing_cols),
          rep(NA_character_, nrow(df)),
          simplify = FALSE
        )
      }
      df <- df[, required_cols, drop = FALSE]
      df$`Gene Names` <- stringr::word(df$`Gene Names`, 1)
    }
    df$Drug <- rep(drug[idx], length.out = nrow(df))
    df
  })
  df <- do.call(rbind, mod_res)
  if (!nrow(df)) {
    return(tibble::as_tibble(df))
  }
  return(tibble::as_tibble(df))
}
