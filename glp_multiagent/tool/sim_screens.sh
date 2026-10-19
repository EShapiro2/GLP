#!/usr/bin/env bash
# The screens of the apps' figures, programs/apps/{gsg,gsn,gsnp,gai}/screens/,
# taken on the iOS Simulator.  From glp_multiagent/, under the Mac's one run
# lock (/Users/udi/Grassroots/tmp/glp-run.sh):
#
#     bash tool/sim_screens.sh ["iPhone 16e"]
#
# It builds the bundled GLP assets (tool/sync_glp_assets.sh), boots the
# simulator, sets its status bar to 9:41, full Wi-Fi and signal and a full
# battery, and starts the host's screenshot service (tool/sim_shot.py serve).
# It then runs integration_test/apps_screens_test.dart on the simulator: the
# scenario of test/apps_screens_test.dart, one person's phone on the screen at
# a time, which at each screen asks the service to take the simulator's
# screenshot; the service frames it as the phone, 1080 by 2220 in a black
# bezel, and writes it under the screen's name.  The raw screenshots are kept
# in build/sim_screens/.  The status bar is cleared and the simulator shut
# down on the way out.
#
# The device is an iPhone 16e, 390 by 844 points at 3 pixels a point, the
# size of the phone the headless test draws.
set -euo pipefail
cd "$(dirname "$0")/.."           # glp_multiagent/
DEVICE="${1:-iPhone 16e}"
UDID=$(xcrun simctl list devices available \
  | grep -E "^ +$DEVICE \(" | head -1 \
  | sed -E 's/.*\(([0-9A-F-]{36})\).*/\1/')
if [ -z "$UDID" ]; then
  echo "sim_screens: no available simulator named $DEVICE" >&2
  exit 1
fi
PROGRAMS=$(cd ../programs && pwd)
FLUTTER_ROOT=$(flutter --version --machine \
  | python3 -c 'import json, sys; print(json.load(sys.stdin)["flutterRoot"])')
RAW=build/sim_screens
rm -rf "$RAW"
mkdir -p "$RAW"

bash tool/sync_glp_assets.sh

SERVER=
cleanup() {
  if [ -n "$SERVER" ]; then kill "$SERVER" 2>/dev/null || true; fi
  xcrun simctl status_bar "$UDID" clear 2>/dev/null || true
  xcrun simctl shutdown "$UDID" 2>/dev/null || true
}
trap cleanup EXIT

echo "sim_screens: $DEVICE ($UDID)"
xcrun simctl boot "$UDID" 2>/dev/null || true
xcrun simctl bootstatus "$UDID" -b > /dev/null
xcrun simctl status_bar "$UDID" override --time 9:41 \
  --dataNetwork wifi --wifiMode active --wifiBars 3 \
  --cellularMode active --cellularBars 4 \
  --batteryState discharging --batteryLevel 100

python3 tool/sim_shot.py serve "$UDID" "$PROGRAMS" "$RAW" "$RAW/port" &
SERVER=$!
for _ in $(seq 100); do
  if [ -s "$RAW/port" ]; then break; fi
  sleep 0.1
done
PORT=$(cat "$RAW/port")

flutter test integration_test/apps_screens_test.dart -d "$UDID" \
  --dart-define=GLP_PROGRAMS="$PROGRAMS" \
  --dart-define=GLP_SHOTS="http://127.0.0.1:$PORT" \
  --dart-define=FLUTTER_ROOT="$FLUTTER_ROOT"
