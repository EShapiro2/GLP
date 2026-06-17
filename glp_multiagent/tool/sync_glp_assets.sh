#!/usr/bin/env bash
# Refresh the bundled GLP assets from the canonical sources in programs/.
# The bundle is what sandboxed platforms (iOS) load; the macOS app reads the
# repo directly. Run this before any iOS build so the on-device app runs the
# exact same program the macOS app and the headless tests do.
set -euo pipefail
cd "$(dirname "$0")/.."           # glp_multiagent/
SRC=../programs
DST=assets/glp/programs
mkdir -p "$DST/book/coins"
cp "$SRC/self.glp" "$DST/self.glp"
for f in self coins_agent coins_mediator play_coins_boot; do
  cp "$SRC/book/coins/$f.glp" "$DST/book/coins/$f.glp"
done
echo "Synced GLP assets from $SRC -> $DST"
