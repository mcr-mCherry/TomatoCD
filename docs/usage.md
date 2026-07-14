# Usage: running the manuscript analysis pipeline

## 1. Clone and create the environment

```bash
git clone https://github.com/mcr-mCherry/TomatoCD.git
cd TomatoCD
conda env create -f environment.yml
conda activate tomATOCD
```

## 2. Edit `configs/config.yaml`

Override default paths to match your local layout:

```yaml
data_dir:    data/raw
figures_dir: results/figures
genome:
  fasta: /abs/path/to/HEINZ.fa.gz
  gtf:   /abs/path/to/ITAG4.1.gtf.gz
seed: 42
```

Reference genomes and raw sequencing data are **not** shipped in this
repository; see `docs/data_sources.md` for accession numbers and md5 sums.

## 3. Run the smoke test (no full data needed)

```bash
bash examples/test/run.sh
```

This renders **Figure 1B** from a 10-row synthetic table in under 30 s.

## 4. Reproduce all manuscript figures

```bash
snakemake -n                     # dry run
snakemake --cores 4 --use-conda  # execute
```

A single figure can be regenerated as:

```bash
snakemake --cores 4 results/figures/Fig1B.pdf
```

Logs land in `logs/<rule>.log`; final artefacts in
`results/figures/<Figure>.pdf`.
