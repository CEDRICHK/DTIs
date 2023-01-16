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
  expect_equal(output, expected_output)
})
