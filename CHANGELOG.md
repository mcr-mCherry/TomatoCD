# Changelog

All notable changes to this repository are documented here. Versions
follow [Semantic Versioning](https://semver.org/).

## [Unreleased]

### Added

- `CITATION.cff` exposing the BibTeX entry from the GitHub sidebar.
- Conda reproducible environment (`environment.yml`) pinning R 4.3 +
  Python 3.11 + the Snakemake 8 toolchain.
- Snakemake pipeline (entry point `workflow/Snakefile`) with one
  executable rule, `fig1b_family_distribution`, rendering
  Figure 1B from a tabular TF summary.
- Smoke-test example (`examples/test/`) producing `Fig1B.pdf` from a
  10-row synthetic table; reference output shipped at
  `examples/test/expected/Fig1B.pdf`.
- Configuration module (`configs/config.yaml`) lifting paths and the
  RNG seed out of the scripts.
- Documentation: `docs/usage.md`, `docs/data_sources.md`,
  `docs/migration_status.md`, `CHANGELOG.md`, `CONTRIBUTING.md`,
  `CODE_OF_CONDUCT.md`.
- `.gitignore` extended to exclude raw sequencing artefacts,
  intermediate results, secrets, and pipeline internals.

### Changed

- `.gitignore` extended (additive; existing `.DS_Store` and `temp*.tsv`
  rules preserved).

### Known limitations

- Only Fig 1B has been migrated end-to-end. Other figures are tracked
  in `docs/migration_status.md` and remain on the legacy per-folder
  layout until they are lifted into the Snakemake pipeline.
- Public accession numbers will be added to `docs/data_sources.md`
  and the README when the cover letter for the manuscript goes out.
