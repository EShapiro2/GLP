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
old=$(git rev-parse HEAD)
sudo -u glp git pull -q --ff-only
new=$(git rev-parse HEAD)
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
sudo -u glp env PAPERS_DIR=/var/lib/glp/papers python3 -I servers/peoplesweb/tools/build.py
# The test site is under /test of Caddy's root and is not the live build's to
# delete (deploy/test.sh).
rsync -a --delete --exclude /test/ servers/peoplesweb/build/www/ /var/www/peoplesweb.org/
[ "$old" = "$new" ] || systemctl restart peoplesweb
# The test site, peoplesweb.org/test: the site as the branch GSA-site has it,
# over the repository as main has it.  Last, so that a failure there leaves
# the live site published.
sh servers/peoplesweb/deploy/test.sh || echo "sweep: the test site, peoplesweb.org/test, was not built" >&2
