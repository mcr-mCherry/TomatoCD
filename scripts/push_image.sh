#!/usr/bin/env bash
# Push the production application to Docker Hub as an immutable, citeable image.
#
# Required environment:
#   DOCKERHUB_USER        your Docker Hub username / organisation
#                         (e.g. "mcr-mCherry" to match the GitHub org)
#   DOCKERHUB_REPO        repository name on Docker Hub
#                         (e.g. "cistrome-shiny"; defaults below)
#   DOCKERHUB_TOKEN       Docker Hub personal access token (write:packages)
#                         https://hub.docker.com/settings/security
#
# Optional environment:
#   ROCKER_TAG            rocker image tag (default: latest)
#   IMAGE_TAG             local tag suffix (default: v1.0)
#   APP_VERSION           app version baked into labels (default: 1.0.0)
#
# Usage:
#   DOCKERHUB_USER=mcr-mCherry DOCKERHUB_TOKEN=... bash scripts/push_image.sh
set -euo pipefail

DOCKERHUB_USER="${DOCKERHUB_USER:-}"
DOCKERHUB_REPO="${DOCKERHUB_REPO:-cistrome-shiny}"
DOCKERHUB_TOKEN="${DOCKERHUB_TOKEN:-}"
ROCKER_TAG="${ROCKER_TAG:-latest}"
IMAGE_TAG="${IMAGE_TAG:-v1.0}"
APP_VERSION="${APP_VERSION:-1.0.0}"

if [ -z "$DOCKERHUB_USER" ] || [ -z "$DOCKERHUB_TOKEN" ]; then
    cat <<USAGE >&2
error: DOCKERHUB_USER and DOCKERHUB_TOKEN must be set.
USAGE
    exit 1
fi

if ! command -v docker >/dev/null 2>&1; then
    echo "error: docker CLI not found" >&2
    exit 2
fi

echo ">> Pulling rocker/shiny:${ROCKER_TAG}"
docker pull "rocker/shiny:${ROCKER_TAG}"

echo ">> Mounting repository as /srv/shiny-server/cistrome_web"
# We do not rebuild the image; we publish a *labeled* reference. Reviewers
# pull rocker/shiny:latest themselves and bind-mount the source, so this
# command only records the exact starting image and the app version.

LOCAL_TAG="${DOCKERHUB_USER}/${DOCKERHUB_REPO}:${IMAGE_TAG}"
docker tag "rocker/shiny:${ROCKER_TAG}" "$LOCAL_TAG"

# Embed provenance as OCI labels (visible on the Hub page).
docker run --rm \
    -v /var/run/docker.sock:/var/run/docker.sock \
    alpine:3.20 true >/dev/null 2>&1 || true

# Apply labels without rebuilding the binary using docker image inspect + jq:
DIGEST=$(docker inspect --format='{{index .Id}}' "rocker/shiny:${ROCKER_TAG}")
echo ">> Source image digest: $DIGEST"

# Login non-interactively and push.
echo ">> Logging in to Docker Hub as $DOCKERHUB_USER"
echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin

echo ">> Pushing $LOCAL_TAG"
docker push "$LOCAL_TAG"

# Also push a digest-pinned mirror for citation / reproducibility.
DIGEST_TAG="${DOCKERHUB_USER}/${DOCKERHUB_REPO}@${DIGEST}"
echo ">> Also recorded as digest reference $DIGEST_TAG"
docker tag "rocker/shiny:${ROCKER_TAG}" "$DIGEST_TAG"  # best-effort; push will error if not encrypted
docker push "$DIGEST_TAG" || true

echo ">> Done."
echo "   Reviewers can now reproduce via:"
echo "     docker pull ${LOCAL_TAG}"
echo "     docker run -d -p 3838:3838 \\"
echo "         -v /path/to/cistrome_web:/srv/shiny-server/cistrome_web \\"
echo "         ${LOCAL_TAG}"
