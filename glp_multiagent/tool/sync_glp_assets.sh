#!/usr/bin/env bash
# Build the bundled GLP assets from the canonical sources in programs/.
# The bundle is what sandboxed platforms (iOS) load; the macOS app reads the
# repo directly. Run this before any iOS build so the on-device app runs the
# exact same program the macOS app and the headless tests do.
#
# The bundle holds every .glp of the trees the app loads --- the directories
# GlpPaths names (lib/glp_sources.dart) --- read from the repository as
# discovery reads a program: every .glp file of the program's directory tree,
# together with the self.glp of each directory from the root down to it (TGLP
# modules.tex, Compilation, first step), and a .vglp where no .glp stands
# beside it, with the dispatcher's generic source it is compiled with
# (program_linker.dart, _addVglpModules).  No file is named here, so none
# drifts from the tree.  Until 2026-10-07 this script and lib/glp_sources.dart
# each listed the files, and both lacked what the trees had gained since:
# social/graph/ui/self.glp (GSG, gap abe6081f), grassapp/budget/,
# social/graph/endapp/, cssn/village/ and the currencies' two programs.  The
# coins and sovereign source trees went with their one-heap hosts,
# main_coins.dart and main_sovereign.dart, on 2026-11-03 (GLP #11, Code_inbox
# 2026-11-03 18:03 UTC): the super-app installs those mini-apps as the
# artefacts built below.
#
# The tree is staged under assets/glp/programs/ as the engine reads it, and
# the certified mini-apps' artefacts are built into it.  Flutter bundles the
# files of a directory pubspec.yaml names and not those of its subdirectories,
# so the staged tree is then laid out flat under assets/glp/bundle/, one
# numbered file per staged file, and assets/glp/manifest.txt, generated with
# it, gives each its path in the tree --- the list iOS needs at build time,
# generated from the tree.  pubspec.yaml names those two and nothing else.
set -euo pipefail
cd "$(dirname "$0")/.."           # glp_multiagent/
SRC=../programs
OUT=assets/glp
DST=$OUT/programs
# The trees the app loads: GlpPaths's directories, with the root self.glp, and
# the directories of the modules the root exposes, which are in every program's
# scope (TGLP modules.tex, "The -expose directive"), so the engine reads them
# wherever the program lies, with the directory's self.glp and what that names.
# Until 2026-11-06 those were not among them, and an agent on the bundle failed
# at its load: "-expose: module file not found: .../programs/sglp/monitor.glp".
EXPOSED=$(sed -n 's/^-expose(\(.*\))\.$/\1/p' "$SRC/self.glp" | tr '#' '/' \
  | while read -r m; do dirname "$m"; done | sort -u)
TREES="social/graph cssn currencies/bonds $(echo $EXPOSED)"
rm -rf "$OUT"
mkdir -p "$DST"

# Stage programs/<path> at the same path in the bundle's tree.
stage() {
  mkdir -p "$DST/$(dirname "$1")"
  cp "$SRC/$1" "$DST/$1"
}

stage self.glp
for t in $TREES; do
  # The self.glp of each directory between the root and the tree.
  d=$(dirname "$t")
  while [ "$d" != "." ]; do
    if [ -f "$SRC/$d/self.glp" ]; then stage "$d/self.glp"; fi
    d=$(dirname "$d")
  done
  # Every .glp of the tree, and every .vglp no hand-written .glp stands for.
  while read -r f; do
    case "$f" in
      *.vglp) if [ -f "$SRC/${f%.vglp}.glp" ]; then continue; fi ;;
    esac
    stage "$f"
  done < <(cd "$SRC" && find "$t" -type f \( -name '*.glp' -o -name '*.vglp' \) | sort)
done
# A .vglp is compiled with the dispatcher's generic source, programs/vglp/dispatcher.glp.
if [ -n "$(find "$DST" -name '*.vglp')" ]; then
  stage vglp/dispatcher.glp
fi

# The certified mini-apps the super-app installs reach the phone as artefacts,
# not as sources: the agent's load_file/2 resolves a name only within its own
# directory, and what it reads there is a certified compiled program. A .glpw
# is a build product (gitignored), so it is built here from the canonical
# sources rather than copied --- the same :artefact the tests use, writing
# <name>.glpw into the super-app's directory in the bundle.  The child-safe
# mini-app is not among them until GSG ports the super-app to CSSN's compiled
# platform (GLP #9 Cowork, Code_inbox 2026-10-20 09:22 UTC, item 1).
( cd ../glp_runtime && for prog in ../programs/social/graph/pingapp \
                                   ../programs/currencies/coins/currency \
                                   ../programs/currencies/sovereign/denominated \
                                   ../programs/currencies/bonds/dated \
                                   ../programs/apps/gsg \
                                   ../programs/apps/gsn; do
    printf ':artefact %s %s\n:quit\n' \
      "$prog" "../glp_multiagent/$DST/social/graph/core" | bin/glpc
  done ) | grep -E '(✓ Wrote|Artefact failed|Error:)' || true

# The staged tree laid out flat, with its manifest (above): written before the
# artefacts are asserted, so that a missing one leaves the rest of the bundle.
mkdir -p "$OUT/bundle"
: > "$OUT/manifest.txt"
n=0
while read -r p; do
  n=$((n + 1))
  name=$(printf '%04d' "$n")
  cp "$OUT/$p" "$OUT/bundle/$name"
  printf '%s\t%s\n' "$name" "$p" >> "$OUT/manifest.txt"
done < <(cd "$OUT" && find programs -type f | sort)

for a in pingapp currency denominated dated gsg gsn; do
  test -s "$DST/social/graph/core/$a.glpw" \
    || { echo "sync_glp_assets: $a.glpw was not written" >&2; exit 1; }
done

echo "Synced GLP assets from $SRC -> $OUT ($n files)"
