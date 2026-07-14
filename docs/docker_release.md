# Docker Hub release

This document records the procedure followed to publish a citeable
container image for the **TomatoCD** Shiny application. Reviewers can
reproduce the deployment with a single `docker pull`.

## What gets published

The production deployment uses the upstream `rocker/shiny:latest`
image unchanged. We do **not** ship a private custom image: the
application files (`ui.R`, `server.R`, `global.R`, `config.R`,
`R/utils.R`) are mounted from the host at container start, so any
reviewer can rebuild the deployment from the public base image plus
this repository's main branch.

To record provenance, we publish two references under the
`mcr-mCherry/cistrome-shiny` Docker Hub repository:

- **`<user>/cistrome-shiny:v1.0`** — a labelled pointer to
  `rocker/shiny:latest`. Update with every release.
- **`<user>/cistrome-shiny@sha256:00c997f2...`** — a digest-pinned
  reference that lets reviewers pull *exactly* the base image used
  in the production deployment.

## Publishing (manual)

```bash
export DOCKERHUB_USER=mcr-mCherry
export DOCKERHUB_TOKEN=...             # write:packages scope
bash scripts/push_image.sh
```

The script handles: pulling `rocker/shiny:latest`, tagging it,
attaching OCI provenance labels, logging in non-interactively, and
pushing both the version tag and the digest tag.

## Reviewer-side reproduction

```bash
docker pull mcr-mCherry/cistrome-shiny:v1.0

docker run -d --name cistrome-shiny \
  -p 3838:3838 \
  -v "$PWD":/srv/shiny-server/cistrome_web \
  mcr-mCherry/cistrome-shiny:v1.0
```

Visiting http://localhost:3838/ should display the production app
running against the local checkout.
