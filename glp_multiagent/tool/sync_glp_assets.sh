#!/usr/bin/env bash
# Refresh the bundled GLP assets from the canonical sources in programs/.
# The bundle is what sandboxed platforms (iOS) load; the macOS app reads the
# repo directly. Run this before any iOS build so the on-device app runs the
# exact same program the macOS app and the headless tests do.
# Keep the file list in sync with lib/glp_sources.dart and pubspec.yaml.
set -euo pipefail
cd "$(dirname "$0")/.."           # glp_multiagent/
SRC=../programs
DST=assets/glp/programs
rm -rf "$DST"
mkdir -p "$DST/social/graph/routing" "$DST/grassapp" "$DST/social/graph/ui" "$DST/cssn/ui"
cp "$SRC/self.glp" "$DST/self.glp"
for f in output inject intro befriend; do
  cp "$SRC/social/graph/routing/$f.glp" "$DST/social/graph/routing/$f.glp"
done
for f in self currency_txn grassapp_agent grassapp_mediator play_grassapp_boot play_grassapp_duo play_village_headless; do
  cp "$SRC/grassapp/$f.glp" "$DST/grassapp/$f.glp"
done
for f in self agent boot play_ui_boot; do
  cp "$SRC/social/graph/$f.glp" "$DST/social/graph/$f.glp"
done
for f in mediator actors; do
  cp "$SRC/social/graph/ui/$f.glp" "$DST/social/graph/ui/$f.glp"
done
for f in self agent child_agent boot play_ui_boot; do
  cp "$SRC/cssn/$f.glp" "$DST/cssn/$f.glp"
done
for f in mediator actors; do
  cp "$SRC/cssn/ui/$f.glp" "$DST/cssn/ui/$f.glp"
done
echo "Synced GLP assets from $SRC -> $DST"
