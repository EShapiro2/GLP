#!/bin/bash
# devices.sh: the phones the Mac sees, and whether each has the Grassroots App
# (GLP/phones/docs/debugging.md, "The Mac"), so the session checks the phones
# itself before telling Udi anything.  It prints `xcrun devicectl list
# devices`, then, for each iPhone that is not unavailable, its name, its UDID
# (the argument of build.sh) and the app's line from `xcrun devicectl device
# info apps`, or that it has none.
set -u
APP_ID=com.eshapiro.grassapp
command -v xcrun >/dev/null || { echo "devices.sh: no xcrun; this runs on the Mac" >&2; exit 2; }
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
