# Code availability statement — Methods draft

The paragraph below is drop-in text for the manuscript's **Methods →
Code availability** section. The publication DOI is left as a
placeholder; it will be replaced once the Zenodo-GitHub integration
mints a canonical release DOI.

## Suggested wording

> All source code for the **Tomato Cis-Regulation Database** web
> application is publicly available under the MIT License. The
> front-end is implemented in **R (v4.4.x)** using the **Shiny**
> framework (v1.14.0) and deployed inside the official
> **`rocker/shiny:latest`** Docker image (image digest
> `sha256:00c997f263e3c3496f1bd6e91c402631cf7edc9c562f85f3259ee8a3efa20b22`)
> running on a Synology NAS through Container Manager. The
> application directory (`ui.R`, `server.R`, `global.R`, `config.R`,
> `R/utils.R`) is mounted from the host path
> `/volume3/cistrome_web/` into the container at
> `/srv/shiny-server/cistrome_web/` and is served by Shiny Server on
> container port `3838`, mapped to host port `3838`. Genomic datasets
> and the **JBrowse 2** (v2.x) instance are served as static files
> from the same host directory. Processed data are mirrored to the
> public Hugging Face Hub repository
> `TomatoCisRegDB/TomatoCisRegDB` using **rclone** (v1.74.4). The
> full R-package dependency list is recorded under
> `docs/dependencies.md`; the deployment host details under
> `docs/hardware.md`. To reproduce the deployment, users can run:
>
> ```bash
> docker run -d --name cistrome-shiny \
>   -p 3838:3838 \
>   -v /path/to/cistrome_web:/srv/shiny-server/cistrome_web \
>   rocker/shiny:latest
> ```
>
> Source code, Docker image, and processed data are deposited at
> Zenodo (DOI: `<待填>`) and archived at GitHub
> (URL: `https://github.com/mcr-mCherry/TomatoCD`).

## Reviewer-side cross-references

- Production deployment: `docs/deployment.md`
- Software dependencies: `docs/dependencies.md`
- Hardware / data mirror: `docs/hardware.md`
- Manuscript figure pipeline: `docs/usage.md`, `docs/manifest_map.md`
- Data availability: `docs/data_sources.md`
