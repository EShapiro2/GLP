#!/bin/sh
# The site of the Calculus's next publication, peoplesweb.org/next, before its
# contracts reach main (Udi, 2026-11-16, to Code #33: a test site of the
# contracts the Calculus is testing).  The branch NEXT_BRANCH of GLP has them
# as calculus/next/NAME/; it is fetched into a repository apart from the live
# clone, as deploy/test.sh fetches the test branch, tools/next_tree.py writes
# from it a tree in which each contract is as if adopted, and deploy/test.sh
# builds the test branch's site over that tree, into /next.  The files fetched
# are texts the build reads, and nothing of them is run.  Where the branch is
# gone, /next is removed; where a step fails, /next stays as it was and the
# sweep says so.  deploy/sweep.sh runs this last.
set -e
BRANCH=${NEXT_BRANCH:-calculus-8/test-site}
GLP=${GLP_ROOT:-/srv/glp}
WORK=${NEXT_WORK:-/var/lib/glp/next}
WWW=${NEXT_WWW:-/var/www/peoplesweb.org/next}
AS=${TEST_AS-sudo -u glp}
cd "$GLP"
URL=$(git remote get-url origin)
if $AS git ls-remote --exit-code --heads "$URL" "$BRANCH" >/dev/null 2>&1; then :; else
	[ $? -eq 2 ] && rm -rf "$WWW"
	exit 0
fi
$AS mkdir -p "$WORK"
[ -d "$WORK/repo" ] || $AS git init -q --bare "$WORK/repo"
$AS git -C "$WORK/repo" fetch -q --depth 1 "$URL" "$BRANCH"
$AS rm -rf "$WORK/src"
$AS mkdir -p "$WORK/src"
$AS sh -c "git -C '$WORK/repo' archive FETCH_HEAD calculus/next | tar -x -C '$WORK/src'"
$AS python3 -I servers/peoplesweb/tools/next_tree.py "$WORK/src/calculus/next" "$GLP" "$WORK/tree"
TEST_URL="$URL" GLP_ROOT="$WORK/tree" TEST_WORK="$WORK/site" TEST_WWW="$WWW" CERTIFY=0 CALCULUS_DIR="$WORK/none" \
	sh "$GLP/servers/peoplesweb/deploy/test.sh"
