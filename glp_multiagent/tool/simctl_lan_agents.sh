#!/usr/bin/env bash
# Two agents over the LAN, rung 4 of the phones plan: a cold call and an
# assignment between two machines, by the app's entry (lib/agent_entry.dart).
# From glp_multiagent/, under the Mac's run slots (its name holds "simctl", so
# that tmp/glp-run.sh gives it the simulator lock):
#
#     bash tool/simctl_lan_agents.sh mac-sim ["iPhone 16e"]
#     bash tool/simctl_lan_agents.sh sim-sim ["iPhone 16e" ["iPhone 17"]]
#
# mac-sim: the Mac's agent, the callee on the headless layer, at the Mac's LAN
# address (test/helpers/lan_mac_agent.dart), and the simulator's, the caller
# on the app's layer, which dials that address.  sim-sim: two simulators, each
# on the app's layer, each knowing the other's key and finding it by DNS-SD
# (integration_test/lan_agent_test.dart).  The harness gives each its peer's
# key and address over GNA's blackboard (networking/tool/e2e_blackboard.dart).
#
# Prints each agent's report, the underlay its peer's message came over and
# whether its second start had the same key; exits 0 where both report.  The
# headless layer runs the WAN alone, so the Mac reports wan and the simulator
# lan.  Two simulators share the Mac's addresses and DNS-SD resolves each to
# 127.0.0.1, which the layer's local prefixes leave out, so both report wan.  A run that never
# ends is stopped after LAN_WATCHDOG seconds, 900 unless given, the two builds
# included.
set -uo pipefail
cd "$(dirname "$0")/.."           # glp_multiagent/
MODE="${1:?mac-sim or sim-sim}"
DEV1="${2:-iPhone 16e}"
DEV2="${3:-iPhone 17}"
OUT="${LAN_OUT:-/Users/udi/Grassroots/tmp/glp-rung4-lan}"
PORT=8797
BB="http://127.0.0.1:$PORT"
RUN="r$(date -u +%H%M%S)"
HOST=$(ipconfig getifaddr en0)
[ -n "$HOST" ] || { echo "lan_agents: en0 has no address, so no LAN" >&2; exit 1; }

udid() { xcrun simctl list devices available | grep -E "^ +$1 \(" | head -1 | sed -E 's/.*\(([0-9A-F-]{36})\).*/\1/'; }
PIDS=()
SIMS=()
cleanup() {
  for p in "${PIDS[@]}"; do kill "$p" 2>/dev/null; done
  for s in "${SIMS[@]}"; do xcrun simctl shutdown "$s" 2>/dev/null; done
}
trap cleanup EXIT

boot() {
  xcrun simctl boot "$1" 2>/dev/null
  xcrun simctl bootstatus "$1" -b > /dev/null
  SIMS+=("$1")
}
sim_agent() { # <role> <udid>
  flutter test integration_test/lan_agent_test.dart -d "$2" \
    --dart-define=LAN_ROLE="$1" --dart-define=LAN_RUN="$RUN" \
    --dart-define=LAN_BB="$BB" > "$OUT-$MODE-$1.txt" 2>&1
}
await_key() { curl -s --max-time "${2:-890}" "$BB/wait/$RUN-$1?t=${2:-890}"; }
# Wait for both agents; the first that ends in a failure ends the other, which
# would wait for it to the watchdog.
both() { # <pid> <pid>
  local a=$1 b=$2 ra=x rb=x
  while [ "$ra" = x ] || [ "$rb" = x ]; do
    if [ "$ra" = x ] && ! kill -0 "$a" 2>/dev/null; then wait "$a"; ra=$?; [ "$ra" = 0 ] || kill "$b" 2>/dev/null; fi
    if [ "$rb" = x ] && ! kill -0 "$b" 2>/dev/null; then wait "$b"; rb=$?; [ "$rb" = 0 ] || kill "$a" 2>/dev/null; fi
    sleep 1
  done
  return $(( ra + rb ))
}

bash tool/sync_glp_assets.sh > "$OUT-$MODE-sync.txt" 2>&1 || { echo "lan_agents: assets" >&2; exit 1; }
dart ../networking/tool/e2e_blackboard.dart $PORT > "$OUT-$MODE-board.txt" 2>&1 &
PIDS+=($!)
until curl -s -o /dev/null "$BB/ping"; do sleep 0.2; done

( sleep "${LAN_WATCHDOG:-900}"; echo "lan_agents: stopped by the watchdog"; kill $$ ) &
PIDS+=($!)

case "$MODE" in
  mac-sim)
    SIM1=$(udid "$DEV1"); [ -n "$SIM1" ] || { echo "no simulator $DEV1" >&2; exit 1; }
    flutter test test/helpers/lan_mac_agent.dart --dart-define=LAN_RUN="$RUN" \
      --dart-define=LAN_BB="$BB" --dart-define=LAN_HOST="$HOST" \
      > "$OUT-$MODE-mac.txt" 2>&1 &
    M=$!; PIDS+=($M)
    await_key callee_key > /dev/null
    boot "$SIM1"
    sim_agent caller "$SIM1" &
    C=$!; PIDS+=($C)
    both $C $M; R=$?
    echo "callee: $(await_key callee_report 5)"
    echo "caller: $(await_key caller_report 5)"
    exit $R
    ;;
  sim-sim)
    SIM1=$(udid "$DEV1"); SIM2=$(udid "$DEV2")
    [ -n "$SIM1" ] && [ -n "$SIM2" ] || { echo "no simulators $DEV1, $DEV2" >&2; exit 1; }
    boot "$SIM1"; boot "$SIM2"
    sim_agent callee "$SIM1" &
    A=$!; PIDS+=($A)
    await_key callee_key > /dev/null   # the first build is done
    sim_agent caller "$SIM2" &
    C=$!; PIDS+=($C)
    both $C $A; R=$?
    echo "callee: $(await_key callee_report 5)"
    echo "caller: $(await_key caller_report 5)"
    exit $R
    ;;
  *) echo "mac-sim or sim-sim" >&2; exit 1 ;;
esac
