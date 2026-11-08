#!/bin/sh
# The hourly sweep of peoplesweb.org: pull the clone, bring the papers up to
# date, rebuild the worlds and the site, copy the site to Caddy's root, and restart the live server so it
# serves the new worlds.  A pull that brings nothing still rebuilds (half a
# second) and restarts only if the build changed anything.
set -e
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
chown -R glp:glp /var/lib/glp/papers
sudo -u glp env PAPERS_DIR=/var/lib/glp/papers python3 -I servers/peoplesweb/tools/build.py
rsync -a --delete servers/peoplesweb/build/www/ /var/www/peoplesweb.org/
[ "$old" = "$new" ] || systemctl restart peoplesweb
