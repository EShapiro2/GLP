#!/bin/bash
# devices.sh: the phones the Mac sees, and whether each has the Grassroots App
# (GLP/phones/docs/debugging.md, "The Mac"), so the session checks the phones
# itself before telling Udi anything.  It prints `xcrun devicectl list
# devices`, then, for each iPhone that is not unavailable, its name, its UDID
# (the argument of build.sh) and the app's line from `xcrun devicectl device
# info apps`, or that it has none; the app is GLP_BUNDLE_ID, by default the
# Xcode project's (common.sh).  Before them, when adb is on the path, the
# Android phones adb sees: each one's serial (the argument of build.sh), its
# state, model and Android version, and whether the app, the Gradle project's
# applicationId (glp_multiagent/android/app/build.gradle.kts), is installed.
set -u
ME=devices.sh
HERE=$(cd "$(dirname "$0")" && pwd)
. "$HERE/common.sh"
APP_ID=$BUNDLE_ID
command -v xcrun >/dev/null || { echo "devices.sh: no xcrun; this runs on the Mac" >&2; exit 2; }
if command -v adb >/dev/null; then
    ANDROID_ID=$(sed -n 's/^[[:space:]]*applicationId = "\([^"]*\)".*/\1/p' "$GLP/glp_multiagent/android/app/build.gradle.kts" | head -1)
    serials=$(adb devices | awk 'NR > 1 && NF >= 2 { print $1 "\t" $2 }')
    if [ -z "$serials" ]; then
        echo "devices.sh: no Android phone seen by adb"
    else
        while IFS=$'\t' read -r serial state; do
            if [ "$state" != device ]; then
                echo "== Android $serial ($state): unlock it and allow USB debugging for this Mac"
                continue
            fi
            model=$(adb -s "$serial" shell getprop ro.product.model </dev/null | tr -d '\r')
            version=$(adb -s "$serial" shell getprop ro.build.version.release </dev/null | tr -d '\r')
            echo "== Android $model, serial $serial (Android $version)"
            if adb -s "$serial" shell pm list packages "$ANDROID_ID" </dev/null | tr -d '\r' | grep -qx "package:$ANDROID_ID"; then
                echo "   $ANDROID_ID installed"
            else
                echo "   no Grassroots App ($ANDROID_ID)"
            fi
        done <<< "$serials"
    fi
    echo
fi
xcrun devicectl list devices
J=$(mktemp -d)
trap 'rm -rf "$J"' EXIT
if ! xcrun devicectl list devices --json-output "$J/devices.json" >/dev/null 2>&1; then
    echo "devices.sh: devicectl gave no device list" >&2
    exit 1
fi
python3 -I - "$J/devices.json" > "$J/phones.txt" <<'PYEOF'
import json, sys
for d in json.load(open(sys.argv[1])).get('result', {}).get('devices', []):
    hw, conn = d.get('hardwareProperties', {}), d.get('connectionProperties', {})
    if hw.get('deviceType') != 'iPhone' or conn.get('tunnelState') == 'unavailable':
        continue
    print('\t'.join([d.get('identifier', ''), hw.get('udid', ''), d.get('deviceProperties', {}).get('name', ''), conn.get('tunnelState', '')]))
PYEOF
if [ ! -s "$J/phones.txt" ]; then
    echo; echo "devices.sh: no iPhone available"
    exit 0
fi
while IFS=$'\t' read -r id udid name state; do
    echo
    echo "== $name, UDID $udid ($state)"
    apps=$(xcrun devicectl device info apps --device "$id" 2>&1) || { echo "   devicectl could not list its apps:"; echo "$apps" | tail -3 | sed 's/^/   /'; continue; }
    if line=$(printf '%s\n' "$apps" | grep -i "$APP_ID"); then
        printf '%s\n' "$line" | sed 's/^/   /'
    else
        echo "   no Grassroots App ($APP_ID)"
    fi
done < "$J/phones.txt"
