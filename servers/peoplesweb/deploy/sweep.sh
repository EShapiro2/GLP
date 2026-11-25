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
# The units in deploy/ are installed from the clone, so that a change to them, as the test site's build on each push to Website-site (peoplesweb-next.timer, Udi, 2026-11-20), takes effect with no run by hand on the server.  A failure here is reported and the sweep goes on.
{ changed=
  for u in peoplesweb-sweep.service peoplesweb-sweep.timer peoplesweb-next.service peoplesweb-next.timer peoplesweb-villagers.service peoplesweb-villagers.timer; do
	cmp -s servers/peoplesweb/deploy/$u /etc/systemd/system/$u || { cp servers/peoplesweb/deploy/$u /etc/systemd/system/$u; changed=1; }
  done
  [ -z "$changed" ] || systemctl daemon-reload
  systemctl is-enabled -q peoplesweb-next.timer || systemctl enable -q --now peoplesweb-next.timer
  systemctl is-enabled -q peoplesweb-villagers.timer || systemctl enable -q --now peoplesweb-villagers.timer; } ||
  echo "sweep: installing the units of deploy/ failed" >&2
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
# The verified posts of the feedback form, committed to the branch feedback of
# GLP with the same token (tools/feedback_publish.py), from which Audit's
# session opens their pull requests.
python3 -I servers/peoplesweb/tools/feedback_publish.py || true
chown -R glp:glp /var/lib/glp/papers
# The Calculus's texts are on main, in calculus/ and in each contract's record,
# every contract being adopted there since 2026-11-16, and the build reads
# them from the clone.  Until 2026-11-17 the sweep also unpacked the branch
# Calculus of GLP into /var/lib/glp/calculus for the build to read; that
# branch is closed, and its seven contracts under names main no longer has
# showed on the site beside main's, 73 contracts for 66.  What it left on the
# server is removed.
rm -rf /var/lib/glp/calculus
sudo -u glp env PAPERS_DIR=/var/lib/glp/papers python3 -I servers/peoplesweb/tools/build.py
# The test site is under /next of Caddy's root and is not the live build's to
# delete (deploy/test.sh).  Until 2026-11-17 it was under /test, which this
# copy now removes.
# The Villagers site under /villagers is not the live build's to delete either (deploy/villagers.sh).
rsync -a --delete --exclude /next/ --exclude /villagers/ servers/peoplesweb/build/www/ /var/www/peoplesweb.org/
[ "$old" = "$new" ] || systemctl restart peoplesweb
# The test site, peoplesweb.org/next: the site as the branch Website-site has it,
# over the repository as main has it.  Last, so that a failure there leaves
# the live site published.
sh servers/peoplesweb/deploy/test.sh || echo "sweep: the test site, peoplesweb.org/next, was not built" >&2
# The Villagers site, peoplesweb.org/villagers (deploy/villagers.sh), after the
# test site, so that a failure there leaves both published.
sh servers/peoplesweb/deploy/villagers.sh || echo "sweep: the Villagers site, peoplesweb.org/villagers, was not built" >&2
# Until 2026-11-17 a second site, built by a script of its own, showed at
# /next the contracts' new names before they were made on main; what it left
# on the server is removed.
rm -rf /var/lib/glp/next
