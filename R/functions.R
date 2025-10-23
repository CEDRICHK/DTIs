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
  df <- NULL
  if (!is.null(query)) {
    if (typeof(query) == "list") {
      formatted_queries <- sapply(1:length(query), function(x) {
        ifelse(stringi::stri_isempty(names(query)[x]), paste(names(query)[x], "(", paste(query[[x]],
                                           collapse = "+or+"), ")", sep = ""),
        paste(names(query)[x], ":(", paste(query[[x]],
                                           collapse = "+or+"), ")", sep = ""))
      })
      url <- "https://rest.uniprot.org/uniprotkb/search?query="
      full_query <- paste(formatted_queries, collapse = "+and+")
    }
    else if (typeof(query) == "character") {
      full_query <- query
    }
    else {
      message("Query not supported")
      return(NULL)
    }
    cols <- paste(columns, collapse = ",")
    full_url <- paste("https://rest.uniprot.org/uniprotkb/search?query=",
                      full_query, "&format=tsv&fields=", cols, sep = "")
    df <- tryCatch({
      read.table(full_url, sep = "\t", header = TRUE,
                 check.names = FALSE, quote = "", stringsAsFactors = FALSE)
    }, error = function(err) {
      message("reading url 'https://www.uniprot.org/...' failed")
    })
  }
  return(df)
}

#' @title Retrieve gene names and organism information of drugs from UniProt
#' @description Retrieves gene names and organism information of drugs from UniProt and returns the result as a data frame.
#' @param drug A character vector containing the names of drugs.
#' @return A data frame containing the accession number, gene names, organism name, review status and the drug name.
#' @examples
#' drug <- c("Aceclofenac", "Benzthiazide")
#' uniprot_drug_data(drug)

#' @import queryup
#' @import dplyr
#' @import magrittr
#' @import stringr

uniprot_drug_data <- function(drug) {
  query <- lapply(drug, function(x) list(x, "organism_id" = "9606", "reviewed" = "true"))
  res <- lapply(query, function(x) get_uniprot_data(x, columns = c("accession", "gene_names", "organism_name", "reviewed")))
  replace_genes <- lapply(1:length(query), function(x) stringr::word(str = res[[x]]$`Gene Names`,1))
  mod_res <- lapply(seq_along(res), function(idx) {
    df <- res[[idx]]
    if (is.null(df) || nrow(df) == 0) {
      df <- data.frame(
        Entry = character(),
        "Gene Names" = character(),
        Organism = character(),
        Reviewed = character(),
        stringsAsFactors = FALSE
      )
    } else {
      df$`Gene Names` <- replace_genes[[idx]]
    }
    df$Drug <- drug[idx]
    df
  })
  df <- do.call("rbind", mod_res)
  result <- na.omit(df)
  return(result)
}
