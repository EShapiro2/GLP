#!/bin/sh
# The test site, peoplesweb.org/next (under /test until 2026-11-17; Udi, to
# Website #3: "next is good"; Udi, 2026-11-12, to GSA #5: a change to the site
# is shown first on a test site, and made live on his approval).  It is the
# site as the branch Website-site of GLP has it, whole: that branch's
# servers/peoplesweb, its pages and its build, run over that branch's
# programs, records and Calculus, so that a branch that brings in another's
# work, as the Calculus's batch before it reaches main, shows it (Udi,
# 2026-11-17, to Code #33: the names line gone today, reported by Code #33).
# Until 2026-11-17 the branch's servers/peoplesweb was built over the live
# clone, main.  deploy/sweep.sh runs this after it publishes the live site.
# The branch is checked out apart from the live clone, in
# /var/lib/glp/test/tree, and never into the live clone: that clone is
# shallow, and a branch fetched into it with --depth 1 marked its commit
# shallow there, so that when main was moved to that commit the sweep's pull
# could not fast-forward and the sweep stopped (Code #28, 2026-11-12 10:01
# UTC).  It is built into /var/lib/glp/test/build with SITE_TEST=1
# (tools/build.py: the build writes no world, the test pages calling the live
# server) and asks nothing of GLP (CERTIFY=0): a relation is shown as its
# record's relations.txt has it.  Where the branch is gone the test site is
# removed; where it cannot be fetched or does not build, the test site stays as
# it was and the sweep says so.  What is pushed to the branch runs here each
# hour as the user glp before it is on main: only Website's session pushes it.
set -e
BRANCH=${TEST_BRANCH:-Website-site}
GLP=${GLP_ROOT:-/srv/glp}
WORK=${TEST_WORK:-/var/lib/glp/test}
WWW=${TEST_WWW:-/var/www/peoplesweb.org/next}
AS=${TEST_AS-sudo -u glp}
cd "$GLP"
URL=${TEST_URL:-$(git remote get-url origin)}
if $AS git ls-remote --exit-code --heads "$URL" "$BRANCH" >/dev/null 2>&1; then :; else
	[ $? -eq 2 ] && rm -rf "$WWW"
	exit 0
fi
$AS mkdir -p "$WORK"
# What the build of the branch's servers/peoplesweb over the live clone left.
$AS rm -rf "$WORK/repo" "$WORK/site" "$WORK/cache"
[ -d "$WORK/tree/.git" ] || $AS git init -q "$WORK/tree"
$AS git -C "$WORK/tree" fetch -q --depth 1 "$URL" "$BRANCH"
$AS git -C "$WORK/tree" checkout -q -f --detach FETCH_HEAD
$AS git -C "$WORK/tree" clean -q -fdx
$AS env GLP_ROOT="$WORK/tree" BUILD_DIR="$WORK/build" SITE_TEST=1 CERTIFY=0 \
	LIVE_WORLDS="$GLP/programs/vglp/cva/web_worlds" \
	PAPERS_DIR="${PAPERS_DIR:-/var/lib/glp/papers}" \
	python3 -I "$WORK/tree/servers/peoplesweb/tools/build.py"
mkdir -p "$WWW"
rsync -a --delete "$WORK/build/www/" "$WWW/"
# The commit /next is now built from, which deploy/next.sh compares with the branch's head.
$AS git -C "$WORK/tree" rev-parse HEAD | $AS tee "$WORK/built" >/dev/null
