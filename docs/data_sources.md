# Data sources

This file lists, per data category, the public repository accession,
expected mirror URL or local deposit path, and (where applicable) the
MD5 checksum used to validate downloads.

## Reference genome and annotations

| Asset | Source | Version |
| --- | --- | --- |
| Heinz 1706 genome FASTA | Sol Genomics | ITAG 4.1 |
| Gene annotation GTF     | Sol Genomics | ITAG 4.1 |
| Chrom sizes             | derived      | —        |

## New sequencing data generated in this study (deposited at GEO)

The sequencing data newly generated in this study have been deposited in
the NCBI Gene Expression Omnibus (GEO) and are accessible through the
following GEO Series accessions:

| Layer / Assay | GEO Series | Public URL |
| --- | --- | --- |
| Primary series 1 | [GSE173212](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE173212) | https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE173212 |
| Primary series 2 | [GSE173213](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE173213) | https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE173213 |

These data will be publicly released upon publication of the manuscript.
Reviewer-controlled access details are managed through the editorial
system and are intentionally not stored in this repository.

## Public datasets reused in this study

| Asset | Source | Accession / URL |
| --- | --- | --- |
| Tomato Expression Atlas (TEA) | Sol Genomics Network | https://tea.solgenomics.net/ |
| NOR ChIP-seq | NCBI Sequence Read Archive (SRA) | [SRR3110654](https://www.ncbi.nlm.nih.gov/sra/?term=SRR3110654) |

## Notes on file size

Raw sequencing files (FASTQ, BAM, BigWig) are **not** stored in this
Git repository. The `.gitignore` rules exclude `.bam`, `.bw`, `.bed*`,
`.fastq*`, and `.tar*`. Local copies should be downloaded from the
public repositories above into the paths referenced by
`configs/config.yaml`.
