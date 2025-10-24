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

#' @title Retrieve data from UniProt using a query
#' @description Retrieves data from UniProt using a query and returns the result as a data frame.
#' @param query A list or character containing the query for UniProt.
#' @param columns A vector of column names to be returned in the data frame.
#' @return A data frame containing the retrieved data.
#' @examples
#' query <- list(list("Aceclofenac", organism_id = "9606", reviewed = "true"))
#' columns <- c("accession", "gene_names", "organism_name", "reviewed")
#' get_uniprot_data(query[[1]], columns)

#' @import stringi

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

#' @title Retrieve gene names and organism information of drugs from UniProt
#' @description Retrieves gene names and organism information of drugs from UniProt and returns the result as a data frame.
#' @param drug A character vector containing the names of drugs.
#' @return A data frame containing the accession number, gene names, organism name, review status and the drug name.
#' @examples
#' drug <- c("Aceclofenac", "Benzthiazide")
#' uniprot_drug_data(drug)

#' @import queryup
#' @import magrittr
#' @import stringr

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
  result <- stats::na.omit(df)
  return(tibble::as_tibble(result))
}
