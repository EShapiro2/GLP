#!/bin/sh
# peoplesweb.org/next: the site as it will be under the contracts' new names
# (calculus/NAMES.txt; Udi, 2026-11-16, to Website #2: "i want to confirm
# their layout ... before making them live").  The renamed tree is the branch
# names of GLP, which Code keeps, the contracts there having their new names,
# files and records; this checks it out apart from the live clone, in
# /var/lib/glp/next/names, and runs that branch's own build of the site over
# it, with SITE_TEST=1 (tools/build.py: the build writes no world, the pages
# calling the live server) and no question asked of GLP, into /next.  So
# /next is the site the merge of that branch into main will make live.  What
# is pushed to that branch runs here each hour as the user glp before it is on
# main, as the test site's branch does (deploy/test.sh): only Code pushes it.
# Where the branch is gone /next is removed; where it cannot be fetched or
# does not build, /next stays as it was and the sweep says so.
# deploy/sweep.sh runs this last.  Until 2026-11-17 the names were on no
# branch, and a tool, tools/names_tree.py, wrote a stand-in tree from main.
set -e
BRANCH=${NEXT_BRANCH:-names}
GLP=${GLP_ROOT:-/srv/glp}
WORK=${NEXT_WORK:-/var/lib/glp/next}
WWW=${NEXT_WWW:-/var/www/peoplesweb.org/next}
AS=${TEST_AS-sudo -u glp}
cd "$GLP"
URL=${NEXT_URL:-$(git remote get-url origin)}
if $AS git ls-remote --exit-code --heads "$URL" "$BRANCH" >/dev/null 2>&1; then :; else
	[ $? -eq 2 ] && rm -rf "$WWW"
	exit 0
fi
$AS mkdir -p "$WORK"
# What the stand-in tree left.
$AS rm -rf "$WORK/src" "$WORK/tree" "$WORK/site"
[ -d "$WORK/names/.git" ] || $AS git init -q "$WORK/names"
$AS git -C "$WORK/names" fetch -q --depth 1 "$URL" "$BRANCH"
$AS git -C "$WORK/names" checkout -q -f --detach FETCH_HEAD
$AS git -C "$WORK/names" clean -q -fdx
$AS env GLP_ROOT="$WORK/names" BUILD_DIR="$WORK/build" SITE_TEST=1 CERTIFY=0 \
	PAPERS_DIR="${PAPERS_DIR:-/var/lib/glp/papers}" \
	python3 -I "$WORK/names/servers/peoplesweb/tools/build.py"
mkdir -p "$WWW"
rsync -a --delete "$WORK/build/www/" "$WWW/"
