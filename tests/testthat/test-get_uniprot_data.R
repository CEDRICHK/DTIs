skip_if_offline()
skip_on_cran()

test_that("get_uniprot_data() retrieves data from UniProt using a query", {
  query <- list(list("Aceclofenac", organism_id = "9606", reviewed = "true"))
  columns <- c("accession", "gene_names", "organism_name", "reviewed")
  output <- get_uniprot_data(query[[1]], columns)
  expect_s3_class(output, "data.frame")
  expect_named(output, c("Entry", "Gene Names", "Organism", "Reviewed"))
  expect_true(nrow(output) >= 1)
})
