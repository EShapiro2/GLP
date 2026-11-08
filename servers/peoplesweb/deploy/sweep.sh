#!/bin/sh
# The hourly sweep of peoplesweb.org: pull the clone, rebuild the worlds and
# the site, copy the site to Caddy's root, and restart the live server so it
# serves the new worlds.  A pull that brings nothing still rebuilds (half a
# second) and restarts only if the build changed anything.
set -e
cd /srv/glp
old=$(git rev-parse HEAD)
sudo -u glp git pull -q --ff-only
new=$(git rev-parse HEAD)
sudo -u glp python3 -I servers/peoplesweb/tools/build.py
rsync -a --delete servers/peoplesweb/build/www/ /var/www/peoplesweb.org/
[ "$old" = "$new" ] || systemctl restart peoplesweb
