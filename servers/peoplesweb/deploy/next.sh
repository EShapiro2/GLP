#!/bin/sh
# peoplesweb.org/next: the site as it will be under the contracts' new names
# (calculus/NAMES.txt; Udi, 2026-11-16, to Website #2: "i want to confirm
# their layout ... before making them live").  The names are not yet made on
# main, so tools/names_tree.py writes from the live clone a tree in which each
# contract has its new name, and deploy/test.sh builds the test branch's site
# over that tree, into /next.  Nothing of the tree is run, and it has no
# world, so /next shows no running app.  Where main has no calculus/NAMES.txt
# /next is removed; where a step fails, /next stays as it was and the sweep
# says so.  deploy/sweep.sh runs this last.  Until 2026-11-16 20:55 UTC it was
# built over the branch pub2, the publication of that day before it reached
# main; when a branch has the renamed tree itself, this builds over it.
set -e
GLP=${GLP_ROOT:-/srv/glp}
WORK=${NEXT_WORK:-/var/lib/glp/next}
WWW=${NEXT_WWW:-/var/www/peoplesweb.org/next}
AS=${TEST_AS-sudo -u glp}
cd "$GLP"
URL=$(git remote get-url origin)
if [ ! -f calculus/NAMES.txt ]; then rm -rf "$WWW"; exit 0; fi
$AS mkdir -p "$WORK"
$AS rm -rf "$WORK/src"
$AS mkdir -p "$WORK/src"
$AS sh -c "git -C '$GLP' archive HEAD programs cascade cascade.sh calculus | tar -x -C '$WORK/src'"
$AS python3 -I servers/peoplesweb/tools/names_tree.py "$WORK/src" "$WORK/tree"
TEST_URL="$URL" GLP_ROOT="$WORK/tree" TEST_WORK="$WORK/site" TEST_WWW="$WWW" CERTIFY=0 CALCULUS_DIR="$WORK/none" \
	sh "$GLP/servers/peoplesweb/deploy/test.sh"
