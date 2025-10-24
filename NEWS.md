# DTIs News

## Unreleased

### Added
- Replace UniProt fetches with an `httr2` workflow returning tibbles.
- Add fixture-backed mocks for deterministic tests covering edge cases.
- Document reproducible workflows in the vignette and README.
- Provide pkgdown configuration scaffold (run `usethis::use_pkgdown_github_pages()` to set up the site).

### Changed
- Harden `uniprot_drug_data()` against empty or partial API responses.
- Refresh roxygen documentation and namespace exports.

### Removed
- Drop the `cbind.fill` helper in favour of tidy bindings and delete the old run script.
