#!/bin/sh
# The test site, peoplesweb.org/test (Udi, 2026-11-12, to GSA #5: a change to
# the site is shown first on a test site under /test, and made live on his
# approval).  It is the site as the branch GSA-site of GLP has it: that
# branch's servers/peoplesweb, its pages and its build, run over the
# repository as main has it, so the test site shows the live site's contracts,
# papers and apps.  deploy/sweep.sh runs this after it publishes the live
# site.  The branch is fetched into a repository of its own,
# /var/lib/glp/test/repo, and never into the live clone: that clone is
# shallow, and a branch fetched into it with --depth 1 marked its commit
# shallow there, so that when main was moved to that commit the sweep's pull
# could not fast-forward and the sweep stopped (Code #28, 2026-11-12 10:01
# UTC).  The branch's directory is unpacked into /var/lib/glp/test/site and
# built into /var/lib/glp/test/build with SITE_TEST=1 (tools/build.py: the
# build writes no world, the test pages calling the live server); what the
# build asks of GLP is cached apart, in /var/lib/glp/test/cache, begun from
# the live build's cache, so a branch that changes what is asked does not
# write the live site's.  Where the branch is gone the test site is removed;
# where it cannot be fetched or does not build, the test site stays as it was
# and the sweep says so.
set -e
BRANCH=${TEST_BRANCH:-GSA-site}
GLP=${GLP_ROOT:-/srv/glp}
WORK=${TEST_WORK:-/var/lib/glp/test}
WWW=${TEST_WWW:-/var/www/peoplesweb.org/test}
AS=${TEST_AS-sudo -u glp}
cd "$GLP"
URL=$(git remote get-url origin)
if $AS git ls-remote --exit-code --heads "$URL" "$BRANCH" >/dev/null 2>&1; then :; else
	[ $? -eq 2 ] && rm -rf "$WWW"
	exit 0
fi
$AS mkdir -p "$WORK/cache"
[ -d "$WORK/repo" ] || $AS git init -q --bare "$WORK/repo"
$AS git -C "$WORK/repo" fetch -q --depth 1 "$URL" "$BRANCH"
$AS rm -rf "$WORK/site"
$AS mkdir -p "$WORK/site"
$AS sh -c "git -C '$WORK/repo' archive FETCH_HEAD servers/peoplesweb | tar -x -C '$WORK/site'"
if [ -d servers/peoplesweb/cache ]; then
	$AS cp -R servers/peoplesweb/cache/. "$WORK/cache/"
fi
$AS env GLP_ROOT="$GLP" BUILD_DIR="$WORK/build" SITE_TEST=1 CERTIFY_CACHE="$WORK/cache" \
	PAPERS_DIR="${PAPERS_DIR:-/var/lib/glp/papers}" \
	python3 -I "$WORK/site/servers/peoplesweb/tools/build.py"
mkdir -p "$WWW"
rsync -a --delete "$WORK/build/www/" "$WWW/"
