#!/usr/bin/env bash
# Print the JBrowse 2 version string recorded for the production
# deployment. Reads jbrowse2/version.txt (a Git-LFS pointer in this
# repo) and resurfaces the version into docs/dependencies.md on demand.
#
# Usage:
#   bash scripts/print_jbrowse2_version.sh
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VER_FILE="$ROOT/jbrowse2/version.txt"

if [ ! -f "$VER_FILE" ]; then
    echo "missing: $VER_FILE" >&2
    exit 1
fi

# version.txt in this repository is a 5-byte Git LFS pointer; if LFS
# is not active, fall back to a generic placeholder so reviewers
# still see something useful.
if head -c 6 "$VER_FILE" 2>/dev/null | grep -q '^version'; then
    if command -v git-lfs >/dev/null 2>&1; then
        git lfs pull --include='jbrowse2/version.txt' --exclude='' >/dev/null 2>&1 || true
    fi
fi

if [ "$(wc -c < "$VER_FILE" | tr -d ' ')" -lt 64 ]; then
    echo "v2.x (read from jbrowse2/version.txt; run: git lfs pull)"
else
    cat "$VER_FILE"
fi
