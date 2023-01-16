#' @title Retrieve data from UniProt using a query
#' @description Robust alternative to cbind that fills missing values and works on arbitrary data types.
#' @param ... any number of R data objects.
#' @param fill R object to fill empty rows in columns below the max size. If unspecified, repeats input rows in the same way as cbind.
#' @details  Originally written for the row.r package by Craig Varrichio. Included here because the rowr package was discontinued. I use these functions in my packages
#' @examples
#' cbind.fill(c(1,2,3),list(1,2,3),cbind(c(1,2,3)))
#' cbind.fill(rbind(1:2),rbind(3:4))

cbind.fill <- function(...){
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
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
  mod_res <- lapply(1:length(query), function(x) {
    if(nrow(res[[x]]) == 0) {
      res[[x]] <- cbind.fill(res[[x]], Drug = drug[x])
      colnames(res[[x]])[5] <- "Drug"
    }else{
      res[[x]]$`Gene Names` = replace_genes[[x]]
      res[[x]] <- cbind(res[[x]], Drug = drug[x])
    }
    return(res[[x]])
  })
  df <- do.call("rbind", mod_res)
  result <- na.omit(df)
  return(result)
}
