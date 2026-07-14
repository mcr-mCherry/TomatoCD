# Code availability statement — Methods draft

Drop the paragraph below verbatim (or with edits to match the in-text
wording) into the manuscript **Methods** section under the heading
*Code availability*. The DOI is left as a placeholder; it will be
filled in once the Zenodo-GitHub integration has minted one.

> The custom analysis code generated during this study, along with a
> Conda environment definition, a Snakemake pipeline, and a smoke-test
> example, is publicly available at
> https://github.com/mcr-mCherry/TomatoCD under the MIT License.
> The release DOI assigned via Zenodo is **10.5281/zenodo.XXXXXXX**
> (placeholder; see the release page on the repository for the
> minted DOI). The pipeline can be reproduced end-to-end by:
>
> ```bash
> git clone https://github.com/mcr-mCherry/TomatoCD.git
> cd TomatoCD
> conda env create -f environment.yml
> conda activate tomATOCD
> bash examples/test/run.sh          # smoke test
> snakemake --cores 4 --use-conda    # full manuscript figures
> ```
>
> Reviewer access to the underlying sequencing data is granted
> through the editorial system; see the cover letter accompanying
> the manuscript. Public datasets reused in this work are listed in
> `docs/data_sources.md`.
