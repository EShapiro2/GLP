#!/bin/sh
# The Villagers site, peoplesweb.org/villagers (Udi, 2026-11-25 00:19 UTC: "i
# want a version of the website in /villagers that includes only the
# contracts in this repo, and one paper, the Villagers paper.  and the
# Villagers contract in the south pole"; and "follow updates of the Villagers
# repo as they arrive").  The repository EShapiro2/Villagers is pulled, with
# the sweep's read-only token, and its paper built (tools/papers_sync.py,
# PAPERS_ONLY=Villagers), into $WORK; the site is built by the live clone's
# tools/build.py with VILLAGERS_DIR naming the repository's contracts/, so it
# shows those contracts alone, the globe upside down with Villagers at the
# south pole, and the Villagers paper alone; it is copied to $WWW.  Run by the
# hourly sweep after the test site, and by deploy/villagers-watch.sh each
# minute when the repository has moved.  Where the repository cannot be
# fetched or the site does not build, the site stays as it was.
set -e
GLP=${GLP_ROOT:-/srv/glp}
WORK=${VILLAGERS_WORK:-/var/lib/glp/villagers}
WWW=${VILLAGERS_WWW:-/var/www/peoplesweb.org/villagers}
AS=${VILLAGERS_AS-sudo -u glp}
mkdir -p "$WORK"
PAPERS_ONLY=Villagers PAPERS_DIR="$WORK/papers" PAPERS_SRC="$WORK/src" \
	python3 -I "$GLP/servers/peoplesweb/tools/papers_sync.py"
REPO="$WORK/src/git/Villagers"
if [ ! -d "$REPO/contracts" ]; then
	echo "villagers: the repository EShapiro2/Villagers was not fetched; the site stays as it was" >&2
	exit 1
fi
chown -R glp:glp "$WORK" 2>/dev/null || true
$AS env GLP_ROOT="$GLP" BUILD_DIR="$WORK/build" VILLAGERS_DIR="$REPO/contracts" CERTIFY=0 \
	PAPERS_DIR="$WORK/papers" python3 -I "$GLP/servers/peoplesweb/tools/build.py"
mkdir -p "$WWW"
rsync -a --delete "$WORK/build/www/" "$WWW/"
# The commit of the Villagers repository the site is now built from, which deploy/villagers-watch.sh compares with its head.
git -C "$REPO" rev-parse HEAD > "$WORK/built"
