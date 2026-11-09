#!/bin/bash
# build.sh <udid> [<mode> [<known>]]: a build of the Grassroots App for one
# phone, its reporter on (GLP/phones/docs/debugging.md, "The phone side", 1,
# and "The Mac"), installed by ideviceinstaller, the path that worked on
# 2026-11-08 where Xcode's and devicectl's installs hung.  build_sim.sh builds
# for the simulator, in a clone of its own.
#
# With <mode>, the scripted build ("The scripted build"): its person is the
# scripted person <mode> of programs/social/graph/core/superapp_plays.glp,
# encounter_befriend, encounter, befriend_known or encounter_introduce, the
# Dart define GLP_PERSON; <known>, the keys the person knows at the start, in
# hex, separated by commas, the define GLP_KNOWN, none where it is not given.
# It builds lib/main_scripted.dart in place of lib/main_superapp.dart, and
# names the .ipa with the mode.
#
# It cleans the build tree first, every time, since a framework a simulator
# build leaves there is taken into the next phone build, and iOS refuses it
# (2026-11-08, objective_c.framework); syncs the GLP assets; and builds the
# entry in profile mode, since a debug build dies when launched from the home
# screen with no debugger attached, with the Dart define GLP_DEBUGLOG, the
# password in phones/tools/debuglog-password.  Flutter signs the build with
# the team of the Xcode project (ios/Runner.xcodeproj).  The defines are given
# in a file of mode 600, deleted after.  It then checks that the profile
# signing the build lists the phone, and that every binary under
# Runner.app/Frameworks is built for iOS and signed by the profile's team,
# stopping with the framework's name otherwise; zips Runner.app as
# Payload/Runner.app into $GRASSROOTS/tmp/phones/GrassApp-debug-<sha>.ipa,
# GrassApp-<mode>-<sha>.ipa for a scripted build; and installs it, stopping if
# the install has not finished within INSTALL_LIMIT seconds (default 300).
#
# The profile lists a phone only once the team has registered it: connect the
# phone and run, once, `flutter run -d <udid> -t lib/main_superapp.dart` from
# glp_multiagent, which registers it; the free team signs one phone per build.
# GRASSROOTS (default /Users/udi/Grassroots) changes where it writes.
set -euo pipefail
ME=build.sh
if [ $# -lt 1 ] || [ $# -gt 3 ]; then
    echo "usage: build.sh <udid> [<mode> [<known>]]   (devices.sh lists the phones)" >&2
    exit 2
fi
udid=$1
mode=${2:-}
known=${3:-}
[ -z "$known" ] || [ -n "$mode" ] || { echo "build.sh: <known> needs a <mode>" >&2; exit 2; }
if [ -n "$mode" ]; then
    entry=lib/main_scripted.dart
    kind=$mode
else
    entry=lib/main_superapp.dart
    kind=debug
fi
HERE=$(cd "$(dirname "$0")" && pwd)
. "$HERE/common.sh"
APP="$GLP/glp_multiagent"
LIMIT=${INSTALL_LIMIT:-300}
need flutter security codesign xcrun zip ideviceinstaller python3 perl git
sha=$(git -C "$GLP" rev-parse --short=8 HEAD)
[ -z "$(git -C "$GLP" status --porcelain -- glp_multiagent glp_runtime networking programs)" ] || sha="$sha-dirty"
OUT="$G/tmp/phones"
mkdir -p "$OUT"
IPA="$OUT/GrassApp-$kind-$sha.ipa"
W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT
write_defines "$W/defines.json" "$mode" "$known"

cd "$APP"
flutter clean
bash tool/sync_glp_assets.sh
flutter build ios --profile -t "$entry" --dart-define-from-file="$W/defines.json"

RUNNER="$APP/build/ios/iphoneos/Runner.app"
[ -d "$RUNNER" ] || die "no $RUNNER after the build"
PROFILE="$RUNNER/embedded.mobileprovision"
[ -f "$PROFILE" ] || die "the build has no profile, so it was not signed: open ios/Runner.xcworkspace in Xcode and set the team"
security cms -D -i "$PROFILE" > "$W/profile.plist" || die "security could not read $PROFILE"
# The profile's team, and whether it lists the phone.
read -r team listed < <(python3 -I -c '
import plistlib, sys
p = plistlib.load(open(sys.argv[1], "rb"))
devices = [d.lower() for d in p.get("ProvisionedDevices", [])]
print((p.get("TeamIdentifier") or ["none"])[0], "yes" if sys.argv[2].lower() in devices else "no")' "$W/profile.plist" "$udid")
if [ "$listed" != yes ]; then
    die "the profile signing this build does not list the phone $udid; connect it and run once, from $APP: flutter run -d $udid -t lib/main_superapp.dart, which registers it; then build.sh $udid again"
fi

# Every binary under Runner.app/Frameworks built for iOS (a simulator build's
# says IOSSIMULATOR) and signed by the profile's team (an ad hoc signature has
# none).
bad=0
shopt -s nullglob
for fw in "$RUNNER"/Frameworks/*.framework "$RUNNER"/Frameworks/*.dylib; do
    name=$(basename "$fw")
    bin=$fw
    if [ -d "$fw" ]; then
        exe=$(python3 -I -c '
import plistlib, sys
try:
    print(plistlib.load(open(sys.argv[1] + "/Info.plist", "rb")).get("CFBundleExecutable", ""))
except Exception:
    print("")' "$fw")
        bin="$fw/${exe:-${name%.framework}}"
    fi
    if [ ! -f "$bin" ]; then
        echo "build.sh: $name: no binary at $bin" >&2
        bad=1
        continue
    fi
    platforms=$(xcrun vtool -show-build "$bin" 2>/dev/null | awk '$1 == "platform" { print $2 }' | sort -u | tr '\n' ' ' | sed 's/ $//')
    if [ "$platforms" != IOS ]; then
        echo "build.sh: $name is built for ${platforms:-no recorded platform}, not IOS" >&2
        bad=1
    fi
    signer=$(codesign -dv "$fw" 2>&1 | sed -n 's/^TeamIdentifier=//p')
    [ "$signer" != "not set" ] || signer=""
    if [ "$signer" != "$team" ]; then
        echo "build.sh: $name is signed by ${signer:+team }${signer:-no team (ad hoc)}, not by the profile's team $team" >&2
        bad=1
    fi
done
shopt -u nullglob
[ $bad = 0 ] || die "the build is not fit for the phone; nothing installed"

mkdir "$W/Payload"
cp -R "$RUNNER" "$W/Payload/"
rm -f "$IPA"
(cd "$W" && zip -qry "$IPA" Payload)
echo "build.sh: $IPA"
# ideviceinstaller's newer releases take `install <file>`, the older `-i <file>`.
if ideviceinstaller --help 2>&1 | grep -qE '^[[:space:]]+install[[:space:]]'; then
    how=(install "$IPA")
else
    how=(-i "$IPA")
fi
status=0
within "$LIMIT" ideviceinstaller -u "$udid" "${how[@]}" || status=$?
if [ $status = 142 ]; then
    die "the install did not finish within $LIMIT s and was stopped; check the phone with devices.sh, unlock it, and run build.sh again"
elif [ $status != 0 ]; then
    die "ideviceinstaller failed, status $status"
fi
echo "build.sh: installed on $udid; open the Grassroots App there, and its run appears in GET /log/ (follow.sh follows it)"
