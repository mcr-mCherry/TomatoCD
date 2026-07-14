# TomatoCD
Tomato Cis-Regulatory Database is an integrated discovery platform for Solanaceae regulatory genomics. Explore more at https://tomatocd.langlab.top/.

<img width="1791" height="969" alt="image" src="https://github.com/user-attachments/assets/a7c3f30d-87a2-42fa-83fc-c8be9bacc8b0" />
<img width="1759" height="703" alt="image" src="https://github.com/user-attachments/assets/258b010a-0c0b-40d0-ad22-3b21f05d3622" />
<img width="1769" height="588" alt="image" src="https://github.com/user-attachments/assets/e7d82143-8728-487a-80a9-be2500fa9270" />

TomatoCD provides six core functionalities that distinguish it from existing plant regulatory databases such as PlantTFDB or JASPAR:
(1) Network — an interactive visualization tool allowing the exploration of regulatory hierarchies and module structures;
(2) JBrowse2 — a genome browser supporting the visual exploration of transcription factor binding sites, binding sequences, and methylation sensitivity;
(3) Motif — supporting motif queries identified by BPNet and memeChIP for 84 transcription factors;
(4) Methylation Sensitivity — a comprehensive analysis of the impact of DNA methylation on transcription factor binding;
(5) Gene Search — comprehensive gene annotation retrieval and functional analysis;
(6) Metabolic Node Sub-Graphs — exploration of lycopene and ethylene pathway-specific regulatory networks and visualization of expression patterns.

## Data availability

The sequencing data newly generated in this study have been deposited
in the NCBI Gene Expression Omnibus (GEO) under accessions
[GSE173212](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE173212)
and
[GSE173213](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE173213).
These data will be publicly released upon publication.

Public datasets reused in this study were obtained from:

- the Tomato Expression Atlas (TEA) hosted on the Sol Genomics
  Network: https://tea.solgenomics.net/
- the NCBI Sequence Read Archive (SRA) under accession
  [SRR3110654](https://www.ncbi.nlm.nih.gov/sra/?term=SRR3110654)
  (NOR ChIP-seq).

Full per-layer deposits and accession cross-references are listed in
`docs/data_sources.md`.

## Repository layout (what reviewers see)

This repository hosts two layers:

- **Production app** (pre-existing, preserved untouched): the Shiny
  application under `R/`, `config.R`, `global.R`, `server.R`, `ui.R`,
  the JBrowse2 instance under `jbrowse2/`, and the static assets
  under `www/`.
- **Manuscript analysis pipeline** (added for reviewer
  reproducibility): `workflow/`, `scripts/`, `configs/`, `examples/`,
  `docs/`, `environment.yml`, `Dockerfile`, plus `CITATION.cff`,
  `CHANGELOG.md`, `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`.

See `docs/manifest_map.md` for the per-figure mapping and
`docs/data_sources.md` for sequencing data and reusable public
datasets.

## One-line reproduction

```bash
docker build -t tomatocd . && \
  docker run --rm -v "$PWD":/work -w /work tomatocd bash examples/test/run.sh
```

The first command compiles a self-contained image (R 4.3 + Python 3.11
+ Chromium + the pinned `environment.yml`); the second renders
`results/figures/Fig1B.pdf` against the bundled synthetic input.

## Code availability paragraph (drop into Methods)

`docs/Code_Availability_Statement.md` contains a Methods-section draft
ready to be pasted in verbatim (DOI placeholder to be replaced once
Zenodo mints the canonical release DOI).
