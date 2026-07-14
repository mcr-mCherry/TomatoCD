#!/usr/bin/env bash
# Smoke test: render Fig1B from a tiny synthetic input.
# Should complete in <30 s on a developer laptop.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"
mkdir -p data/raw results/figures logs
cp examples/test/data/SupplementaryTable1.tsv data/raw/SupplementaryTable1.txt
Rscript scripts/fig1b_family_distribution.R \
  --input  data/raw/SupplementaryTable1.txt \
  --output results/figures/Fig1B.pdf
echo "Smoke test OK -> results/figures/Fig1B.pdf"
