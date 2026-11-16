#!/bin/sh
# The site of the Calculus's next publication, peoplesweb.org/next, before its
# contracts reach main (Udi, 2026-11-16, to Code #33: a test site of the
# contracts the Calculus is testing).  The branch NEXT_BRANCH of GLP, pub2,
# has them adopted, each owner's programs/<dir>/NAME.txt and cascade/NAME/
# (Code #33, 2026-11-16 16:45 UTC: 65 of the 66).  It is fetched into a
# repository apart from the live clone, as deploy/test.sh fetches the test
# branch; what the site's build reads of it, programs/, cascade/ and
# cascade.sh, is unpacked into a tree, and deploy/test.sh builds the test
# branch's site over that tree, into /next.  The files fetched are read, and
# nothing of them is run; the tree has no world, so /next shows no running
# app.  Where the branch is gone, /next is removed; where a step fails, /next
# stays as it was and the sweep says so.  deploy/sweep.sh runs this last.
# Until 2026-11-16 16:45 UTC it was built from calculus/next of the branch
# calculus-8/test-site, through tools/next_tree.py, which stays for a
# publication no branch has adopted.
set -e
BRANCH=${NEXT_BRANCH:-pub2}
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
$AS rm -rf "$WORK/tree"
$AS mkdir -p "$WORK/tree"
$AS sh -c "git -C '$WORK/repo' archive FETCH_HEAD programs cascade cascade.sh | tar -x -C '$WORK/tree'"
# The tree is made a repository of one commit, the build naming the commit it
# is built from and listing the tracked files.
$AS sh -c "cd '$WORK/tree' && git init -q && git add -A && git -c user.name=glp -c user.email=glp@peoplesweb.org commit -q -m \"$BRANCH \$(git -C '$WORK/repo' rev-parse --short=8 FETCH_HEAD)\""
TEST_URL="$URL" GLP_ROOT="$WORK/tree" TEST_WORK="$WORK/site" TEST_WWW="$WWW" CERTIFY=0 CALCULUS_DIR="$WORK/none" \
	sh "$GLP/servers/peoplesweb/deploy/test.sh"
