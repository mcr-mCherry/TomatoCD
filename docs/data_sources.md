# Data sources

This file lists, per data category, the public repository accession,
expected mirror URL or local deposit path, and (where applicable) the
MD5 checksum used to validate downloads. Reviewers requiring
reviewer-controlled access should consult the cover letter.

> Accession numbers are added during peer review and updated here on
> publication. Until then, the cover letter is the source of truth.

## Reference genome and annotations

| Asset | Source | Version |
| --- | --- | --- |
| Heinz 1706 genome FASTA | Sol Genomics | ITAG 4.1 |
| Gene annotation GTF     | Sol Genomics | ITAG 4.1 |
| Chrom sizes             | derived       | —        |

## Per-layer raw data

| Layer | Public repository | Manifest in `data/raw/` |
| --- | --- | --- |
| ATAC-seq / DAP-seq     | GEO (TBD)        | `data/raw/atac/manifest.tsv` |
| TF ChIP-seq / DAP-seq  | GEO (TBD)        | `data/raw/tf_chip/manifest.tsv` |
| 5mC profiling          | GEO / PRIDE (TBD)| `data/raw/methylation/manifest.tsv` |

## Notes on file size

Raw sequencing files and BAMs are **not** stored in this Git repository.
The `.gitignore` rules exclude `.bam`, `.bw`, `.bed*`, `.fastq*`, and
`.tar*`. Reviewers needing temporary access during peer review should
request the editor's reviewer token from the corresponding author.
