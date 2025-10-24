# DTIs

Retrieve human UniProt targets for a list of drugs.

## Installation

```r
# Install development version from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("CEDRICHK/DTIs")
```

The package uses `renv`; after cloning the repository you can restore the
project library with:

```r
renv::restore()
```

## Quick start

```r
library(DTIs)

drug <- c("Aceclofenac", "Alverine")
result <- uniprot_drug_data(drug)

result
```

This returns a tibble with the UniProt accession, primary gene symbol, organism
and review status for each drug.

## Testing

Unit tests exercise the UniProt helpers using recorded fixtures.
Run the suite with:

```r
devtools::test()
```

When writing new tests that hit the network, prefer injecting mock responses via
the `DTIs.mock_response` option so that the suite remains deterministic.

## API limits and etiquette

The UniProt REST API imposes rate limits and may throttle excessive traffic.
Package examples and tests rely on mock responses to avoid automatic calls.
When running ad hoc analyses, consider batching queries and caching your
results to minimise repeated requests.
