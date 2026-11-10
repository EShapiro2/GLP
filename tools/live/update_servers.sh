#!/bin/bash
# update_servers.sh --- bring the two network servers on the peoplesweb VPS, the
# rendezvous server (GRV, unit rendezvous, UDP 9516) and the directory (GDS, unit
# directory, UDP 9518), to the commit of main given, in one step (Code #27,
# 2026-11-09, for the update to protocol 9, GNA's decisions 40 to 42: the layer
# refuses protocol 8 at the handshake, so the servers and the phones move
# together).  Run from the Mac by Udi; it changes the VPS.
#
#   bash tools/live/update_servers.sh <commit of main>          show what it would do
#   bash tools/live/update_servers.sh <commit of main> --go     do it
#
# The two servers run from a clone of their own, /srv/glp-servers, which this
# script alone moves; the site's hourly sweep pulls /srv/glp, the site's clone,
# to main's tip, and this script does not touch it (GSA #3, 2026-11-10: a
# restart of a server reads its program and root self.glp from its clone, so
# it must find there the commit last installed, not whatever main has become).
#
# What it does on the VPS, as root:
#   1. shows what runs now: the servers' clone's commit, the clone the installed
#      rendezvous unit runs from, the two units, the two queues;
#   2. creates /srv/glp-servers, owned by glp, as a clone of EShapiro2/GLP
#      standing at the commit given, if it is not there; else fetches it and
#      fast-forwards it to the commit given (not to main's tip, which may have
#      moved on); and checks it is there;
#   3. stops the two units;
#   4. moves each layer's queue file aside, into /var/lib/glp/queues-before-<commit>/:
#      a kept message saved by protocol 8 has no senderMessageId, and a queue
#      holding one is refused when read, so the server would not start (GNA's
#      appendix, The queue).  The keys, the agents' state and the listings stay.
#   5. installs the rendezvous unit from the clone (it carries --state-file) and
#      the directory's, resolves the glue's packages in the clone as glp, so the
#      rendezvous unit's first start compiles at once, and builds the
#      directory's executable from the clone into /srv/directory;
#   6. starts the two units and shows their state and last log lines.
# Then, on the Mac: bash tools/live/run_live.sh vps 1 2
set -euo pipefail
COMMIT="${1:?the commit of main the servers are to run}"
GO="${2:-}"
VPS="${VPS:-root@162.35.126.126}"
ssh -o ConnectTimeout=10 "$VPS" COMMIT="$COMMIT" GO="$GO" bash -s <<'REMOTE'
set -euo pipefail
export PATH=/opt/flutter/bin:$PATH
say() { printf '\n== %s\n' "$*"; }
S=/srv/glp-servers
if [ -d "$S/.git" ]; then
  say "now: the servers' clone $S at $(sudo -u glp git -C "$S" log -1 --format='%h %cd' --date=format:'%Y-%m-%d %H:%M')"
elif [ -e "$S" ]; then
  say "now: $S is there but is not a clone: stopping, nothing changed"; exit 1
else
  say "now: no clone $S yet; --go creates it at $COMMIT"
fi
echo "the installed rendezvous unit runs from: $(sed -n 's/^WorkingDirectory=//p' /etc/systemd/system/rendezvous.service 2>/dev/null || echo 'no unit installed')"
systemctl is-active rendezvous directory || true
for q in rv-queue.json gds-queue.json; do
  f=/var/lib/glp/$q
  if [ -f "$f" ]; then echo "$q: $(wc -c < "$f") bytes"; else echo "$q: none"; fi
done
if [ "$GO" != "--go" ]; then
  say "dry run: nothing changed; add --go to update to $COMMIT"
  exit 0
fi
say "bring the servers' clone $S to $COMMIT"
if [ ! -d "$S/.git" ]; then
  install -d -o glp -g glp "$S"
  sudo -u glp -H git clone -q --no-checkout https://github.com/EShapiro2/GLP "$S"
  sudo -u glp -H git -C "$S" checkout -q -B main "$COMMIT"
fi
sudo -u glp -H git -C "$S" fetch -q origin
sudo -u glp -H git -C "$S" merge -q --ff-only "$COMMIT"
HEAD=$(sudo -u glp git -C "$S" rev-parse HEAD)
case "$HEAD" in "$COMMIT"*) echo "clone at $HEAD" ;; *) echo "the clone is at $HEAD, not $COMMIT: stopping, nothing restarted"; exit 1 ;; esac
# The units at the commit given must run the servers from this clone: a commit
# from before the servers had a clone of their own installs units that read
# /srv/glp, the site's clone, which the sweep moves.
if ! grep -q "^WorkingDirectory=$S/" "$S/networking/glue/deploy/rendezvous.service" \
   || ! grep -q -- "--program $S/" "$S/networking/glue/deploy/rendezvous.service" \
   || ! grep -q -- "--program $S/" "$S/servers/directory/deploy/directory.service"; then
  echo "the units at $COMMIT do not run the servers from $S: give a commit of main that has them; stopping, nothing restarted"; exit 1
fi
say "stop the two units"
systemctl stop rendezvous directory
say "move the queues aside"
B=/var/lib/glp/queues-before-${COMMIT:0:8}
mkdir -p "$B"
for q in rv-queue.json gds-queue.json; do [ -f /var/lib/glp/$q ] && mv /var/lib/glp/$q "$B/" && echo "moved $q to $B"; done
chown -R glp:glp "$B"
say "install the units and build the directory"
cp "$S/networking/glue/deploy/rendezvous.service" /etc/systemd/system/rendezvous.service
cp "$S/servers/directory/deploy/directory.service" /etc/systemd/system/directory.service
systemctl daemon-reload
( cd "$S/networking/glue" && sudo -u glp -H /opt/flutter/bin/dart pub get >/dev/null )
( cd "$S/servers/directory" && dart pub get >/dev/null && dart build cli -t bin/directory_server.dart -o /srv/directory )
say "start the two units"
systemctl start rendezvous directory
sleep 20
systemctl is-active rendezvous directory || true
journalctl -u rendezvous --no-pager -n 8 --since -2min | cut -c1-200
journalctl -u directory --no-pager -n 8 --since -2min | cut -c1-200
say "done: the servers run $HEAD"
REMOTE
