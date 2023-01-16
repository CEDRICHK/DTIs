#' @title Retrieve gene names and organism information of drugs from UniProt
#' @description Retrieves gene names and organism information of drugs from UniProt and returns the result as a data frame.
#' @param drug A character vector containing the names of drugs.
#' @return A data frame containing the accession number, gene names, organism name, review status and the drug name.
#' @examples
#' drug <- c("Aceclofenac", "Benzthiazide")
#' uniprot_drug_data(drug)
#'
#' @import queryup
#' @import dplyr

uniprot_drug_data(drug = c("Aceclofenac", "Benzthiazide", "Alverine", "Amikacin", "Azacytidine",
                  "Carbinoxamine", "Cefotetan", "Clidinium", "Clomifene",
                  "Clopidogrel", "Dexfenfluramine", "Diclofenamide", "Dipyrone",
                  "Dydrogesterone", "Etoposide", "Formoterol", "Haloperidol", "Hyoscyamine",
                  "Ifenprodil", "Indapamide", "Kanamycin", "Ketanserin", "Lomefloxacin",
                  "Methimazole", "Nimesulide", "Oxymetazoline", "Pargyline",
                  "Pentobarbital", "Phentolamine", "Prochlorperazine", "Raltitrexed",
                  "Roxithromycin", "Scopolamine", "Silodosin", "Spectinomycin",
                  "Tetryzoline", "Theobromine", "Theophylline", "Tranexamic",
                  "Trichlormethiazide", "Tropicamide", "Valdecoxib", "Xylometazoline"))

# write.csv(x = result, file = "./targets.csv", sep = ",", row.names = FALSE,
#           col.names = TRUE)
