# Software dependencies

This page records the software stack the production deployment was
built against. Versions were captured from the deployed container with
`R --no-save -e "packageVersion('...')"` and from the upstream
documentation pages linked below.

## Container and language runtime

| Software | Version | License | Source |
| --- | --- | --- | --- |
| Container base image | `rocker/shiny:latest` (digest `sha256:00c997f263e3c3496f1bd6e91c402631cf7edc9c562f85f3259ee8a3efa20b22`) | GPL-2 | https://rocker-project.org/ |
| R | 4.4.x (system-level inside the base image) | GPL-2 | https://www.r-project.org/ |
| Shiny | 1.14.0 | GPL-3 | https://shiny.posit.co/ |
| Container runtime | Synology Container Manager (Docker Engine 20.10+) | Apache-2.0 | https://www.synology.com/en-us/dsm/package/ContainerManager |

## Application-level R packages

| Package | Version | License | Source |
| --- | --- | --- | --- |
| `shinydashboard` | 0.7.3 | GPL-2 | https://rstudio.github.io/shinydashboard/ |
| `dplyr` | 1.2.1 | MIT | https://dplyr.tidyverse.org/ |
| `ggplot2` | 4.0.3 | MIT | https://ggplot2.tidyverse.org/ |
| `DT` | 0.34.0 | MIT | https://rstudio.github.io/DT/ |
| `tidyr` | 1.3.2 | MIT | https://tidyr.tidyverse.org/ |
| `data.table` | 1.18.4 | MPL-2.0 | https://r-datatable.com/ |
| `stringr` | 1.6.0 | MIT | https://stringr.tidyverse.org/ |
| `jsonlite` | 2.0.0 | MIT | https://jeroen.cran.dev/jsonlite/ |
| `httr` | 1.4.8 | MIT | https://httr.r-lib.org/ |
| `htmltools` | 0.5.9 | GPL-2 | https://rstudio.github.io/htmltools/ |
| `htmlwidgets` | 1.6.4 | MIT | https://www.htmlwidgets.org/ |
| `httpuv` | 1.6.17 | MIT | https://github.com/rstudio/httpuv |

## Companion tools

| Software | Version | License | Source |
| --- | --- | --- | --- |
| JBrowse 2 | version string under `jbrowse2/version.txt` | Apache-2.0 | https://jbrowse.org/jb2/ |
| `rclone` | 1.74.4 | MIT | https://rclone.org/ |

## Notes

- Indirect (transitive) dependencies installed by the `rocker/shiny`
  base image are not enumerated exhaustively here. The full library
  inventory at the time of submission is captured in
  `/tmp/r_deps.csv` inside the reference container.
- `rocker/shiny:latest` is a moving tag. For reproducible deployment,
  pin to the specific image digest listed above (or to the latest one
  validated by the maintainers).
