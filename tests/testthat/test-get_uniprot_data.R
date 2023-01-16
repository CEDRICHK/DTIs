test_that("get_uniprot_data() retrieves data from UniProt using a query", {
  query <- list(list("Aceclofenac", organism_id = "9606", reviewed = "true"))
  columns <- c("accession", "gene_names", "organism_name", "reviewed")
  expected_output <- data.frame(Entry = c("P23219","P35354","P11712"),
                                genes = c("PTGS1 COX1","PTGS2 COX2","CYP2C9 CYP2C10"),
                                Organism = c("Homo sapiens (Human)",
                                             "Homo sapiens (Human)",
                                             "Homo sapiens (Human)"),
                                Reviewed = c("reviewed","reviewed","reviewed"))
  colnames(expected_output)[2] <- "Gene Names"
  output <- get_uniprot_data(query[[1]], columns)
  expect_equal(output, expected_output)
})
