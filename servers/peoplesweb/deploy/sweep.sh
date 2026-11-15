#!/bin/sh
# The hourly sweep of peoplesweb.org: drop the phones' debugging logs older
# than thirty days, pull the clone, bring the papers up to date, rebuild the
# worlds and the site, copy the site to Caddy's root, and restart the live
# server so it serves the new worlds, and build the test site (deploy/test.sh).
# A pull that brings nothing still rebuilds (half a second) and restarts only
# if the build changed anything.
set -e
# The phones' debugging logs (servers/debuglog): a run whose last event, the
# file's last append, is more than thirty days old is deleted, and a phone left
# with no run.  First, so a failing build below does not keep them; a failure
# here is reported and the sweep goes on.
LOGS=/var/lib/glp/debuglog
if [ -d "$LOGS" ]; then
	{ find "$LOGS" -mindepth 2 -name '*.jsonl' -mmin +43200 -delete &&
	  find "$LOGS" -mindepth 1 -type d -empty -delete; } ||
	  echo "sweep: dropping the old debugging logs in $LOGS failed" >&2
fi
cd /srv/glp
# A pull that changes this script runs the new one at once, from here: the
# shell has read the old one, which would otherwise run to the end and leave
# the change to the next sweep (Code #28, 2026-11-12 09:34 UTC).  SWEEP_OLD
# carries the commit before the pull, so the new run restarts the server.
old=${SWEEP_OLD:-$(git rev-parse HEAD)}
sudo -u glp git pull -q --ff-only
new=$(git rev-parse HEAD)
if [ -z "$SWEEP_OLD" ] && ! git diff --quiet "$old" "$new" -- servers/peoplesweb/deploy/sweep.sh; then
	SWEEP_OLD=$old exec sh servers/peoplesweb/deploy/sweep.sh
fi
# The papers (tools/papers_sync.py): pulled from their repositories with the
# read-only token in /etc/glp/github-token, rebuilt where changed, into
# /var/lib/glp/papers; a failure there keeps the papers as last built and
# does not stop the sweep.
mkdir -p /var/lib/glp/papers
python3 -I servers/peoplesweb/tools/papers_sync.py || true
# The PDFs committed to the branch papers of GLP (tools/papers_publish.py),
# with the token in /etc/glp/github-papers-token, which Udi places; without it
# nothing is published, and the site links the PDFs it serves.
python3 -I servers/peoplesweb/tools/papers_publish.py || true
chown -R glp:glp /var/lib/glp/papers
# The Calculus (calculus/MIGRATION.md, the section Website): until each owner
# adopts its contract, the Calculus's texts are on the branch Calculus of GLP.
# It is fetched into a repository apart from the live clone, which is shallow
# (deploy/test.sh says why), and its calculus/ is unpacked into
# /var/lib/glp/calculus/site, which the live build and the test build read as
# CALCULUS_DIR.  These are texts the build reads and nothing it runs.  Where
# the branch is gone the directory is removed; where it cannot be fetched it
# stays as it was, and the sweep goes on.
CW=/var/lib/glp/calculus
URL=$(git remote get-url origin)
if sudo -u glp git ls-remote --exit-code --heads "$URL" Calculus >/dev/null 2>&1; then
	{ sudo -u glp mkdir -p "$CW" &&
	  { [ -d "$CW/repo" ] || sudo -u glp git init -q --bare "$CW/repo"; } &&
	  sudo -u glp git -C "$CW/repo" fetch -q --depth 1 "$URL" Calculus &&
	  sudo -u glp rm -rf "$CW/new" && sudo -u glp mkdir -p "$CW/new" &&
	  sudo -u glp sh -c "git -C '$CW/repo' archive FETCH_HEAD calculus | tar -x -C '$CW/new'" &&
	  sudo -u glp rm -rf "$CW/site" && sudo -u glp mv "$CW/new" "$CW/site"; } ||
	  echo "sweep: the branch Calculus was not fetched; the site shows it as last fetched" >&2
else
	[ $? -eq 2 ] && rm -rf "$CW/site"
fi
sudo -u glp env PAPERS_DIR=/var/lib/glp/papers CALCULUS_DIR="$CW/site/calculus" python3 -I servers/peoplesweb/tools/build.py
# The test site is under /test of Caddy's root and is not the live build's to
# delete (deploy/test.sh).
rsync -a --delete --exclude /test/ servers/peoplesweb/build/www/ /var/www/peoplesweb.org/
[ "$old" = "$new" ] || systemctl restart peoplesweb
# The test site, peoplesweb.org/test: the site as the branch Website-site has it,
# over the repository as main has it.  Last, so that a failure there leaves
# the live site published.
sh servers/peoplesweb/deploy/test.sh || echo "sweep: the test site, peoplesweb.org/test, was not built" >&2
