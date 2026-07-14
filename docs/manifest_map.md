# Manuscript figures ↔ pipeline rules

This table maps every figure in the manuscript to the Snakemake rule
that produces it, the supporting code, the input assets, and the
manuscript section it appears in. Reviewers may consult this map to
locate each figure's reproducibility path without re-reading the
paper end-to-end.

| Manuscript figure | Manuscript section | Pipeline rule | Code | Inputs |
| --- | --- | --- | --- | --- |
| Fig 1A | Overview | (rendered from `Figure1Overview/Figures/Fig1.jpg` — schematic) | — | — |
| Fig 1B | Overview | `fig1b_family_distribution` | `scripts/fig1b_family_distribution.R` | `data/raw/SupplementaryTable1.txt` (TF summary) |
| Fig 1C | Overview | TBD (legacy `Figure1Overview/scripts/Fig1C.R`) | TBD | per-TF genomic distribution table |
| Fig 1D | Overview | TBD (legacy `Figure1Overview/scripts/Fig1D.R`) | TBD | per-TF trait table |
| Fig 2 (Motif) | Motif catalog | TBD (`Figure2Motif/scripts/...`) | TBD | MEME / BPNet output |
| Fig 3 (TF + 5mC) | Methylation sensitivity | TBD (`Figure3TF5mC/scripts/...`) | TBD | methylation calls + TF calls |
| Fig 4 (Network) | Regulatory networks | TBD (`Figure4Network/`) | TBD | network tables |
| Fig 5 (Pathway) | Pathway analysis | TBD (`Figure5Pathway/`) | TBD | DEG lists, KEGG/Reactome |
| Fig 6 (ERF4) | Case study | TBD (`Figure6ERF4/scripts/...`) | TBD | ERF4 TF + expression table |

## Status conventions

- **scaffolded, smoke-tested** — rule runs end-to-end on `examples/test/`.
- **TBD (not migrated)** — legacy `Figure*/scripts/` script is the
  only available implementation. Migrate to `workflow/rules/` before
  the next reviewer round.

## Re-execution

```bash
snakemake -n                     # dry run
snakemake --cores 4 --use-conda  # execute every rule in dependency order
```

A single figure is regenerated as:

```bash
snakemake --cores 4 results/figures/Fig1B.pdf
```
