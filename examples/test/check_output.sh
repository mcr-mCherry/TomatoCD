#!/usr/bin/env bash
# Compare the freshly rendered Fig1B.pdf against the reference PDF in
# examples/test/expected/. Returns exit code 0 when the smoke test is
# considered passed.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
NEW="$ROOT/results/figures/Fig1B.pdf"
REF="$ROOT/examples/test/expected/Fig1B.pdf"

if [ ! -f "$NEW" ]; then echo "FAIL: missing $NEW"; exit 2; fi
if [ ! -f "$REF" ]; then echo "FAIL: missing $REF"; exit 3; fi
if ! head -c 4 "$NEW" | grep -q "%PDF"; then echo "FAIL: not a PDF"; exit 4; fi
if ! head -c 4 "$REF" | grep -q "%PDF"; then echo "FAIL: reference not a PDF"; exit 5; fi

sizes=$(stat -f%z "$NEW" "$REF" | tr '\n' ' ')
echo "OK: smoke test produced $NEW ($sizes bytes)"
