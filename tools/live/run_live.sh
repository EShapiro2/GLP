#!/bin/bash
# run_live.sh --- the live test suite of the services installed on
# peoplesweb.org (GSA #2's task for GNA, Code_inbox.md 2026-11-08 12:51 UTC),
# run from the Mac against the live VPS.  It reads the VPS by SSH and changes
# nothing there, but that part 2 restarts the directory's unit once (check
# 2.9, the listings kept across a restart).
#
#   bash tools/live/run_live.sh [part ...]
#
# Parts, each under ten minutes, run in the order given (all, in this order,
# when none is given):
#   vps      what the VPS runs: the units, the clone's commit, whether the
#            running rendezvous agent is the clone's (started after the last
#            change to rv_agent.glp)
#   1        the rendezvous server at 162.35.126.126:9516: two agents on this
#            machine, each a madGLP agent over its own headless layer with a
#            fresh key (bin/live_rv.dart); registration and redemption are
#            sent only where the running agent has them
#   2 3      the directory (GDS) and the mail route (directory_mail_check.py);
#            part 2's flow, 2.2 to 2.10, by three persons on this machine
#            through the directory's agent and the rendezvous server
#            (bin/live_gds.dart), each verification link read from the
#            directory's journal
#   4static  every file the site serves; every listed program downloads
#   4render  every contract's page rendered by headless Chrome, its JS run
#   4live-1 ... 4live-7
#            the live demonstrations, three each (the server's MAX_WORLDS, so
#            a part is one round, about five minutes): each world started and
#            its app page rendered until Ready
#   summary  the counts over the part files
#
# Each part writes RESULT lines to $OUT/part-<part>.txt; $OUT defaults to
# /Users/udi/Grassroots/tmp/live.  MAIL_TO=<address> names the address part 3
# sends a verification mail to, once a relay and the directory's agent exist.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP="$(cd "$HERE/../.." && pwd)"
OUT="${OUT:-/Users/udi/Grassroots/tmp/live}"
BASE="${BASE:-https://peoplesweb.org}"
VPS="${VPS:-root@162.35.126.126}"
RV_ADDR="${RV_ADDR:-162.35.126.126:9516}"
RUN=/Users/udi/Grassroots/tmp/glp-run.sh
FLUTTER_DART="$(dirname "$(readlink -f "$(command -v flutter)")")/cache/dart-sdk/bin/dart"
mkdir -p "$OUT"
ssh_vps() { ssh -o BatchMode=yes -o ConnectTimeout=10 "$VPS" "$@"; }

# The live demonstrations, the Nth three of them, from data.js.
live_group() {
  curl -s "$BASE/data.js" | python3 -I -c '
import sys, json
t = sys.stdin.read(); d = json.loads(t[t.index("=") + 1:].strip().rstrip(";"))
ids = [c["id"] for c in d["contracts"] if c.get("live")]
k = int(sys.argv[1]) - 1
print(" ".join(ids[3 * k:3 * k + 3]))' "$1"
}

part_vps() {
  local f="$OUT/part-vps.txt"; : > "$f"
  for u in rendezvous peoplesweb caddy knot peoplesweb-sweep.timer directory; do
    s=$(ssh_vps "systemctl is-active $u 2>&1")
    v=PASS; [ "$u" = directory ] && v=INFO
    [ "$s" = active ] || [ "$v" = INFO ] || v=FAIL
    echo "RESULT 0-unit-$u $v $u: $s" >> "$f"
  done
  # The servers run from their own clone, /srv/glp-servers, which
  # update_servers.sh alone moves; /srv/glp is the site's (GSA #3, 2026-11-10).
  echo "RESULT 0-clone INFO /srv/glp-servers at $(ssh_vps 'sudo -u glp git -C /srv/glp-servers log -1 --format="%h %ci"')" >> "$f"
  started=$(ssh_vps 'date -d "$(systemctl show rendezvous -p ActiveEnterTimestamp --value)" +%s')
  changed=$(ssh_vps 'sudo -u glp git -C /srv/glp-servers log -1 --format=%ct -- programs/examples/rendezvous/rv_agent.glp')
  last=$(ssh_vps 'sudo -u glp git -C /srv/glp-servers log -1 --format="%h %ci" -- programs/examples/rendezvous/rv_agent.glp')
  if [ "$started" -ge "$changed" ]; then
    echo "RESULT 0-rv-agent-current PASS the rendezvous server started $(date -u -r "$started" '+%F %T') UTC, after rv_agent.glp's last change ($last)" >> "$f"
  else
    echo "RESULT 0-rv-agent-current FAIL the rendezvous server started $(date -u -r "$started" '+%F %T') UTC and runs the agent of then; the clone's rv_agent.glp changed since ($last): register and redeem are not served until the unit restarts" >> "$f"
  fi
  key=$(ssh_vps "journalctl -u rendezvous --no-pager | sed -n 's/.*READY [0-9]* \([0-9a-f]*\)$/\1/p' | tail -1")
  echo "RESULT 0-rv-key INFO server key $key" >> "$f"
  cat "$f"
}

part_1() {
  local f="$OUT/part-1.txt"
  [ -s "$OUT/part-vps.txt" ] || part_vps > /dev/null
  key=$(sed -n 's/^RESULT 0-rv-key INFO server key //p' "$OUT/part-vps.txt")
  reg=""; grep -q '0-rv-agent-current PASS' "$OUT/part-vps.txt" && reg=--registration
  (cd "$HERE" && bash "$RUN" live-pub flutter pub get > "$OUT/pub-get.txt" 2>&1)
  (cd "$HERE" && bash "$RUN" live-rv "$FLUTTER_DART" run bin/live_rv.dart \
      --server-key "$key" --server-address "$RV_ADDR" --glp-root "$GLP/programs" $reg \
      > "$OUT/raw-part-1.txt" 2>&1)
  grep -E '^(RESULT|INFO) ' "$OUT/raw-part-1.txt" > "$f"
  grep -q '^RESULT' "$f" || echo "RESULT 1-run FAIL live_rv.dart printed no result; see $OUT/raw-part-1.txt" >> "$f"
  cat "$f"
}

part_23() {
  python3 -I "$HERE/directory_mail_check.py" "$BASE" "$VPS" "$1" ${MAIL_TO:+--mail-to "$MAIL_TO"} \
    | tee "$OUT/part-$1.txt"
  [ "$1" = 2 ] && grep -q '^RESULT 2.0-deployed PASS' "$OUT/part-2.txt" && part_2flow
}

# Part 2's flow (bin/live_gds.dart): the directory's key from its journal's
# READY line, its address from /etc/glp/directory.env, the rendezvous
# server's key from part vps.
part_2flow() {
  local f="$OUT/part-2.txt"
  [ -s "$OUT/part-vps.txt" ] || part_vps > /dev/null
  rvkey=$(sed -n 's/^RESULT 0-rv-key INFO server key //p' "$OUT/part-vps.txt")
  dirkey=$(ssh_vps "journalctl -u directory --no-pager | sed -n 's/.*READY [0-9]* \([0-9a-f]*\) .*/\1/p' | tail -1")
  diraddr=$(ssh_vps "sed -n 's/^GDS_PUBLIC_ADDRESS=//p' /etc/glp/directory.env")
  if [ -z "$dirkey" ] || [ -z "$diraddr" ]; then
    echo "RESULT 2.2-flow FAIL the directory's agent is not running: key '$dirkey' (its journal's READY line), address '$diraddr' (/etc/glp/directory.env)" | tee -a "$f"
    return
  fi
  (cd "$HERE" && bash "$RUN" live-pub flutter pub get > "$OUT/pub-get.txt" 2>&1)
  (cd "$HERE" && bash "$RUN" live-gds "$FLUTTER_DART" run bin/live_gds.dart \
      --base "$BASE" --dir-key "$dirkey" --dir-address "$diraddr" \
      --rv-key "$rvkey" --rv-address "$RV_ADDR" --glp-root "$GLP/programs" \
      --log-cmd "ssh -o BatchMode=yes -o ConnectTimeout=10 $VPS journalctl -u directory --no-pager --since -2h" \
      --restart-cmd "ssh -o BatchMode=yes -o ConnectTimeout=10 $VPS systemctl restart directory" \
      > "$OUT/raw-part-2.txt" 2>&1)
  grep -E '^(RESULT|INFO) ' "$OUT/raw-part-2.txt" | tee -a "$f"
  grep -q '^RESULT 2\.2' "$OUT/raw-part-2.txt" || echo "RESULT 2.2-flow FAIL live_gds.dart printed no result; see $OUT/raw-part-2.txt" | tee -a "$f"
}

part_4() {
  case "$1" in
    static) python3 -I "$HERE/site_check.py" "$BASE" static | tee "$OUT/part-4static.txt" ;;
    render) python3 -I "$HERE/site_check.py" "$BASE" render | tee "$OUT/part-4render.txt" ;;
    live-*) g=$(live_group "${1#live-}")
            if [ -z "$g" ]; then echo "no live demonstrations in group ${1#live-}"; return; fi
            python3 -I "$HERE/site_check.py" "$BASE" live $g | tee "$OUT/part-4$1.txt" ;;
  esac
}

summary() {
  cat "$OUT"/part-*.txt 2>/dev/null | grep '^RESULT' | awk '{print $3}' | sort | uniq -c
  echo "FAIL:"; cat "$OUT"/part-*.txt 2>/dev/null | grep '^RESULT [^ ]* FAIL' | cut -c1-220
}

[ $# -eq 0 ] && set -- vps 1 2 3 4static 4render 4live-1 4live-2 4live-3 4live-4 4live-5 4live-6 4live-7 summary
for p in "$@"; do
  echo "== part $p  $(date -u '+%F %T') UTC"
  case "$p" in
    vps) part_vps ;;
    1) part_1 ;;
    2|3) part_23 "$p" ;;
    4static) part_4 static ;;
    4render) part_4 render ;;
    4live-[1-9]) part_4 "live-${p#4live-}" ;;
    summary) summary ;;
    *) echo "unknown part $p" >&2; exit 64 ;;
  esac
done
