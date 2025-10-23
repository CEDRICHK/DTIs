mock_uniprot_fixture <- function(name) {
  full_path <- system.file("testthat", "fixtures", name, package = "DTIs")
  if (!nzchar(full_path)) {
    full_path <- file.path(testthat::test_path(), "fixtures", name)
  }

  if (!file.exists(full_path)) {
    stop("Missing fixture: ", name, call. = FALSE)
  }

  utils::read.delim(
    full_path,
    check.names = FALSE,
    quote = "",
    stringsAsFactors = FALSE
  )
}

mock_uniprot_responder <- function(full_query, columns) {
  fixtures <- list(
    "Aceclofenac AND organism_id:(9606) AND reviewed:(true)" = "uniprot_aceclofenac.tsv",
    "Benzthiazide AND organism_id:(9606) AND reviewed:(true)" = "uniprot_benzthiazide.tsv"
  )

  fixture_name <- fixtures[[full_query]]
  if (is.null(fixture_name)) {
    return(NULL)
  }

  mock_uniprot_fixture(fixture_name)
}
