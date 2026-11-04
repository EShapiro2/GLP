#!/usr/bin/env bash
# Bundle what the phone app needs, from the repository, into assets/: the GLP
# files the live harnesses load (the root self.glp and the library it
# exposes, vGLP's dispatcher and the canonical platforms' scope), the GAS
# site (servers/gas/build/www, written by servers/gas/tools/build.py), and the
# starter contracts as packages (tool/packages.py: GKK, GC and AGORA; the app
# installs them on first start, and gets the others from the catalog).  Flutter bundles the
# files of a directory named in pubspec.yaml and not its subdirectories, so
# the files are laid out flat under assets/bundle/, and assets/manifest.txt
# gives each its path: programs/... for a GLP file, www/... for the site.
# Run servers/gas/tools/build.py first, then this, before every build.
set -euo pipefail
cd "$(dirname "$0")/.."                  # servers/gas/phone
REPO=../../..
PROGRAMS=$REPO/programs
WWW=../build/www
OUT=assets
test -d "$PROGRAMS/vglp/cva/gas_worlds" || { echo "no worlds: run python3 -I ../tools/build.py" >&2; exit 1; }
test -f "$WWW/app.html" || { echo "no site: run python3 -I ../tools/build.py" >&2; exit 1; }

rm -rf "$OUT"
mkdir -p "$OUT/tree/programs" "$OUT/bundle"
stage() { mkdir -p "$OUT/tree/programs/$(dirname "$1")"; cp "$PROGRAMS/$1" "$OUT/tree/programs/$1"; }
stage self.glp
stage vglp/dispatcher.glp
stage vglp/cva/self.glp
# The trees of the modules the root self.glp exposes (their directories'
# self.glp files call into the whole tree), the library, the canonical
# platforms' worlds, and the self.glp of each directory above a tree.
TREES="lib $(grep -o -- '-expose([a-z_#]*)' "$PROGRAMS/self.glp" | sed 's/-expose(//; s/)//' | awk -F'#' '{print ($1=="social" ? $1"/"$2 : $1)}' | sort -u | tr '\n' ' ')"
for t in $TREES; do
  d=$(dirname "$t")
  while [ "$d" != "." ]; do
    if [ -f "$PROGRAMS/$d/self.glp" ]; then stage "$d/self.glp"; fi
    d=$(dirname "$d")
  done
  while read -r f; do stage "$f"; done < <(cd "$PROGRAMS" && find "$t" -type f \( -name '*.glp' -o -name '*.vglp' \) | sort)
done
mkdir -p "$OUT/tree/www"
cp -R "$WWW/." "$OUT/tree/www/"
python3 -I tool/packages.py "$OUT/tree/packages" ${GAS_STARTERS:-gkk gc agora} > /dev/null

: > "$OUT/manifest.txt"
n=0
while read -r p; do
  n=$((n + 1))
  name=$(printf '%05d' "$n")
  cp "$OUT/tree/$p" "$OUT/bundle/$name"
  printf '%s\t%s\n' "$name" "$p" >> "$OUT/manifest.txt"
done < <(cd "$OUT/tree" && find programs www packages -type f | sort)
echo "sync_assets: $n files into $OUT (the staged tree is $OUT/tree)"
