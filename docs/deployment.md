# Production deployment

This page documents the deployment that serves the public site
`https://tomatocd.langlab.top/`. Reviewers and future maintainers can
use it to reproduce the production environment without re-reading the
paper.

## Runtime architecture

| Tier | Component | Version | Role |
| --- | --- | --- | --- |
| Container base image | `rocker/shiny:latest` | digest `sha256:00c997f263e3c3496f1bd6e91c402631cf7edc9c562f85f3259ee8a3efa20b22` | Provides R 4.4.x and Shiny Server |
| Web framework | Shiny (`server.R`, `ui.R`) | 1.14.0 | Serves the interactive UI |
| Layout shell | `shinydashboard` | 0.7.3 | Sidebar / box / tab-item layout |
| Genome browser | JBrowse 2 (static) | version string under `jbrowse2/version.txt` | Static asset served by Shiny Server |
| Data sync tool | `rclone` | 1.74.4 | Mirrors processed data to Hugging Face Hub |
| Hosting OS | Synology DSM | (see deployment host row below) | Container Manager / Docker Engine |

## Storage and mounts

- Host source directory: `/volume3/cistrome_web/` (Btrfs).
- Container mount: `/srv/shiny-server/cistrome_web/`.
- Port mapping: container `3838` → host `3838`.
- Reads: `ui.R`, `server.R`, `global.R`, `config.R`, `R/utils.R`.
- Reads (static): `data/`, `jbrowse2/`, `www/`, `gene_search/`.
- Reads (sync target): Hugging Face Hub repository
  `TomatoCisRegDB/TomatoCisRegDB`.

## One-line reproduction

```bash
docker run -d --name cistrome-shiny \
  -p 3838:3838 \
  -v /absolute/path/to/cistrome_web:/srv/shiny-server/cistrome_web \
  rocker/shiny:latest
```

Substitute the left-hand side of `-v` with the local checkout of this
repository. The container reads the application files directly from
the mount, so no rebuild is required.

## Container documentation

The base image is mirrored from https://rocker-project.org/, which
guarantees R, Shiny Server, and a consistent set of system libraries.
Application-level R packages (see `docs/dependencies.md`) are
installed at first boot via `global.R` and the helper scripts under
`R/`.
