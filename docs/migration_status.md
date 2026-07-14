# Migration status: manuscript figures -> Snakemake pipeline

This document tracks per-figure migration from the legacy
`Figure*/scripts/` folders (kept outside this repository) into
Snakemake rules under `workflow/rules/`.

| Figure | Source (legacy) | Pipeline rule | Status |
| --- | --- | --- | --- |
| Fig 1B | `Figure1Overview/scripts/Fig1B.R` | `workflow/Snakefile::fig1b_family_distribution` | scaffolded, smoke-tested |
| Fig 1C | `Figure1Overview/scripts/Fig1C.R` | TBD | not migrated |
| Fig 1D | `Figure1Overview/scripts/Fig1D.R` | TBD | not migrated |
| Fig 2A-E | `Figure2Motif/scripts/`           | TBD | not migrated |
| Fig 3G panels | `Figure3TF5mC/scripts/`        | TBD | not migrated |
| Fig 4 networks  | `Figure4Network/`              | TBD | not migrated |
| Fig 5 pathway   | `Figure5Pathway/`              | TBD | not migrated |
| Fig 6 ERF4      | `Figure6ERF4/scripts/`         | TBD | not migrated |

## Conventions for new rules

- One Snakemake rule per manuscript panel.
- All inputs from `configs/config.yaml`; no `/Users/...` absolute paths.
- A `logs/<rule>.log` declared per rule.
- A synthetic input under `examples/test/` so reviewers can exercise
  the rule end-to-end without the full dataset.
