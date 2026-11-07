#!/bin/bash
# The bundle GAS2's app carries: the GLP tree a device runs (tools/device_tree.py's
# output, its programs/) and the device's page (../device/web), laid out flat under
# assets/bundle with assets/manifest.txt naming each file's place, as GAS's phone
# app lays out its own.  Flutter bundles the files of a directory named in
# pubspec.yaml and not those of its subdirectories, hence the flat layout.
#
#   bash tool/sync_assets.sh <device tree directory>
set -euo pipefail
cd "$(dirname "$0")/.."
TREE="${1:?usage: sync_assets.sh <device tree directory, holding programs/>}"
rm -rf assets
mkdir -p assets/bundle
: > assets/manifest.txt
i=0
add() { # add <file> <its place in the bundle>
  i=$((i + 1))
  cp "$1" "assets/bundle/f$i"
  printf 'f%s\t%s\n' "$i" "$2" >> assets/manifest.txt
}
while IFS= read -r f; do
  add "$TREE/$f" "$f"
done < <(cd "$TREE" && find programs -type f | sort)
for f in ../device/web/*; do
  add "$f" "web/$(basename "$f")"
done
echo "assets: $i files"
