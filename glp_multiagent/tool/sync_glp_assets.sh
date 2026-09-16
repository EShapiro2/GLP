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
mkdir -p "$DST/social/graph/routing" "$DST/grassapp" "$DST/social/graph/ui" \
  "$DST/social/graph/core" "$DST/social/graph/pingapp" "$DST/cssn/ui" "$DST/cssn/childsafe"
cp "$SRC/self.glp" "$DST/self.glp"
for f in output inject intro befriend; do
  cp "$SRC/social/graph/routing/$f.glp" "$DST/social/graph/routing/$f.glp"
done
for f in self currency_txn grassapp_agent grassapp_mediator play_grassapp_boot play_grassapp_duo play_village_headless; do
  cp "$SRC/grassapp/$f.glp" "$DST/grassapp/$f.glp"
done
for f in self boot play_ui_boot; do
  cp "$SRC/social/graph/$f.glp" "$DST/social/graph/$f.glp"
done
# The Grassroots Super-App: core/ is the certified program, pingapp/ the
# mini-app it installs. The agent moved from social/graph/agent.glp to
# core/agent.glp (SGSG, 2026-09-08) and this script still copied the old path,
# so it failed under set -e and no iOS bundle could be built.
# home_ui.glp is what the Flutter agent runtime starts as superapp_ui/3, and
# home.glp is the compiled mediator it imports; neither was copied, so
# superapp_ui/3 could not be found on iOS (GSG, 2026-09-15).
for f in self agent superapp_plays home home_ui; do
  cp "$SRC/social/graph/core/$f.glp" "$DST/social/graph/core/$f.glp"
done
for f in self miniapp; do
  cp "$SRC/social/graph/pingapp/$f.glp" "$DST/social/graph/pingapp/$f.glp"
done
for f in mediator actors; do
  cp "$SRC/social/graph/ui/$f.glp" "$DST/social/graph/ui/$f.glp"
done
for f in self superapp boot play_ui_boot; do
  cp "$SRC/cssn/$f.glp" "$DST/cssn/$f.glp"
done
cp "$SRC/cssn/ui/actors.glp" "$DST/cssn/ui/actors.glp"
# childsafe/ is the certified program: the two agents, the mediator and the
# mini-app entry cssn/3 the super-app activates.
for f in self miniapp plays agent child_agent mediator; do
  cp "$SRC/cssn/childsafe/$f.glp" "$DST/cssn/childsafe/$f.glp"
done
# The certified mini-apps the super-app installs reach the phone as artefacts,
# not as sources: the agent's load_file/2 resolves a name only within its own
# directory, and what it reads there is a certified compiled program. A .glpw
# is a build product (gitignored), so it is built here from the canonical
# sources rather than copied --- the same :artefact the tests use, writing
# <name>.glpw into the super-app's directory in the bundle.
( cd ../glp_runtime && for prog in ../programs/social/graph/pingapp \
                                   ../programs/currencies/coins/currency \
                                   ../programs/cssn/childsafe; do
    printf ':artefact %s %s\n:quit\n' \
      "$prog" "../glp_multiagent/$DST/social/graph/core" | bin/glpc
  done ) | grep -E '(✓ Wrote|Artefact failed|Error:)' || true
for a in pingapp currency childsafe; do
  test -s "$DST/social/graph/core/$a.glpw" \
    || { echo "sync_glp_assets: $a.glpw was not written" >&2; exit 1; }
done

echo "Synced GLP assets from $SRC -> $DST"
