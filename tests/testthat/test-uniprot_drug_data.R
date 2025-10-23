skip_if_offline()
skip_on_cran()

test_that("uniprot_drug_data() retrieves gene names and organism information of drugs from UniProt", {
  drug <- c("Aceclofenac", "Benzthiazide")
  expected_output <- data.frame(Entry = c("P23219","P35354","P11712","Q9Y2D0","Q9ULX7",
                                          "P43166","P35218","Q8N1Q1","P07451","P23280",
                                          "P00915","O43570","Q16790","P22748",
                                          "P00918","P55017"),
                                "Gene Names" = c("PTGS1","PTGS2","CYP2C9","CA5B",
                                                 "CA14","CA7","CA5A","CA13","CA3",
                                                 "CA6","CA1","CA12","CA9","CA4",
                                                 "CA2","SLC12A3"),
                                Organism = c("Homo sapiens (Human)", "Homo sapiens (Human)",
                                             "Homo sapiens (Human)", "Homo sapiens (Human)",
                                             "Homo sapiens (Human)", "Homo sapiens (Human)",
                                             "Homo sapiens (Human)", "Homo sapiens (Human)",
                                             "Homo sapiens (Human)", "Homo sapiens (Human)",
                                             "Homo sapiens (Human)", "Homo sapiens (Human)",
                                             "Homo sapiens (Human)", "Homo sapiens (Human)",
                                             "Homo sapiens (Human)", "Homo sapiens (Human)"),
                                Reviewed = c("reviewed", "reviewed", "reviewed",
                                             "reviewed", "reviewed", "reviewed",
                                             "reviewed", "reviewed", "reviewed",
                                             "reviewed", "reviewed", "reviewed",
                                             "reviewed", "reviewed", "reviewed",
                                             "reviewed"),
                                Drug = c("Aceclofenac","Aceclofenac","Aceclofenac",
                                         "Benzthiazide","Benzthiazide","Benzthiazide",
                                         "Benzthiazide","Benzthiazide","Benzthiazide",
                                         "Benzthiazide","Benzthiazide","Benzthiazide",
                                         "Benzthiazide","Benzthiazide","Benzthiazide","Benzthiazide"))
  colnames(expected_output)[2] <- "Gene Names"
  output <- uniprot_drug_data(drug)
  skip_if(is.null(output) || nrow(output) == 0, "UniProt query failed during test")
  expect_s3_class(output, "data.frame")
  expect_named(output, c("Entry", "Gene Names", "Organism", "Reviewed", "Drug"))
  expect_gt(nrow(output), 0)

  output_subset <- output[output$Drug %in% drug, ]
  expect_true(all(expected_output$Entry %in% output_subset$Entry))

  matched_genes <- output_subset[match(expected_output$Entry, output_subset$Entry), "Gene Names"]
  expect_equal(matched_genes, expected_output$`Gene Names`)
})
