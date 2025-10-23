test_that("get_uniprot_data() retrieves data from UniProt using a query", {
  withr::local_options(DTIs.mock_response = mock_uniprot_responder)
  query <- list(list("Aceclofenac", organism_id = "9606", reviewed = "true"))
  columns <- c("accession", "gene_names", "organism_name", "reviewed")
  output <- get_uniprot_data(query[[1]], columns)
  expected_output <- tibble::tibble(
    Entry = c("P23219", "P35354", "P11712"),
    `Gene Names` = c("PTGS1 COX1", "PTGS2 COX2", "CYP2C9 CYP2C10"),
    Organism = rep("Homo sapiens (Human)", 3),
    Reviewed = rep("reviewed", 3)
  )
  expect_equal(output, expected_output)
})
