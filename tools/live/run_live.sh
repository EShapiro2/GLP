#!/bin/bash
# run_live.sh --- the live test suite of the services installed on
# peoplesweb.org (GSA #2's task for GNA, Code_inbox.md 2026-11-08 12:51 UTC),
# run from the Mac against the live VPS.  It reads the VPS by SSH and changes
# nothing there.
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
#   2 3      the directory (GDS) and the mail route (directory_mail_check.py)
#   4static  every file the site serves; every listed program downloads
#   4render  every contract's page rendered by headless Chrome, its JS run
#   4live-a 4live-b 4live-c
#            the live demonstrations, a third each: each world started and
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

# The live demonstrations in three parts, from data.js.
live_third() {
  curl -s "$BASE/data.js" | python3 -I -c '
import sys, json
t = sys.stdin.read(); d = json.loads(t[t.index("=") + 1:].strip().rstrip(";"))
ids = [c["id"] for c in d["contracts"] if c.get("live")]
k = "abc".index(sys.argv[1]); n = -(-len(ids) // 3)
print(" ".join(ids[k * n:(k + 1) * n]))' "$1"
}

part_vps() {
  local f="$OUT/part-vps.txt"; : > "$f"
  for u in rendezvous peoplesweb caddy knot peoplesweb-sweep.timer directory; do
    s=$(ssh_vps "systemctl is-active $u 2>&1")
    v=PASS; [ "$u" = directory ] && v=INFO
    [ "$s" = active ] || [ "$v" = INFO ] || v=FAIL
    echo "RESULT 0-unit-$u $v $u: $s" >> "$f"
  done
  echo "RESULT 0-clone INFO /srv/glp at $(ssh_vps 'git -C /srv/glp log -1 --format="%h %ci"')" >> "$f"
  started=$(ssh_vps 'date -d "$(systemctl show rendezvous -p ActiveEnterTimestamp --value)" +%s')
  changed=$(ssh_vps 'git -C /srv/glp log -1 --format=%ct -- programs/examples/rendezvous/rv_agent.glp')
  last=$(ssh_vps 'git -C /srv/glp log -1 --format="%h %ci" -- programs/examples/rendezvous/rv_agent.glp')
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
      > "$OUT/part-1-raw.txt" 2>&1)
  grep -E '^(RESULT|INFO) ' "$OUT/part-1-raw.txt" > "$f"
  grep -q '^RESULT' "$f" || echo "RESULT 1-run FAIL live_rv.dart printed no result; see $OUT/part-1-raw.txt" >> "$f"
  cat "$f"
}

part_23() {
  python3 -I "$HERE/directory_mail_check.py" "$BASE" "$VPS" "$1" ${MAIL_TO:+--mail-to "$MAIL_TO"} \
    | tee "$OUT/part-$1.txt"
}

part_4() {
  case "$1" in
    static) python3 -I "$HERE/site_check.py" "$BASE" static | tee "$OUT/part-4static.txt" ;;
    render) python3 -I "$HERE/site_check.py" "$BASE" render | tee "$OUT/part-4render.txt" ;;
    live-?) python3 -I "$HERE/site_check.py" "$BASE" live $(live_third "${1#live-}") \
              | tee "$OUT/part-4$1.txt" ;;
  esac
}

summary() {
  cat "$OUT"/part-*.txt 2>/dev/null | grep '^RESULT' | awk '{print $3}' | sort | uniq -c
  echo "FAIL:"; cat "$OUT"/part-*.txt 2>/dev/null | grep '^RESULT [^ ]* FAIL' | cut -c1-220
}

[ $# -eq 0 ] && set -- vps 1 2 3 4static 4render 4live-a 4live-b 4live-c summary
for p in "$@"; do
  echo "== part $p  $(date -u '+%F %T') UTC"
  case "$p" in
    vps) part_vps ;;
    1) part_1 ;;
    2|3) part_23 "$p" ;;
    4static) part_4 static ;;
    4render) part_4 render ;;
    4live-a|4live-b|4live-c) part_4 "live-${p#4live-}" ;;
    summary) summary ;;
    *) echo "unknown part $p" >&2; exit 64 ;;
  esac
done
