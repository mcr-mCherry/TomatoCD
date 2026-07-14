# Contributing

Issues and pull requests are welcome.

## Bug reports

Please include:

- Output of `snakemake --version` and `R --version`.
- The minimal command needed to reproduce the failure.
- Relevant excerpt of `logs/<rule>.log`.

## Style

- R code follows `tidyverse` style; `styler` is recommended.
- Python code is formatted with `ruff format`.
- Shell scripts use `set -euo pipefail` and double-quote variables.
- Paths come from `configs/config.yaml`; no hard-coded absolute paths.
