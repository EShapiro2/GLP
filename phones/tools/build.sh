#!/bin/bash
# build.sh <device> [<mode> [<known>]]: a build of the Grassroots App for one
# phone, its reporter on (GLP/phones/docs/debugging.md, "The phone side", 1,
# and "The Mac"), installed by ideviceinstaller, the path that worked on
# 2026-11-08 where Xcode's and devicectl's installs hung.  build_sim.sh builds
# for the simulator, in a clone of its own.
#
# <device> is an iPhone's UDID or an Android phone's adb serial, as
# devices.sh lists them, and the build follows the phone: Android when
# `adb devices` lists the serial as ready, iOS when devicectl lists an iOS
# device of that UDID, and it stops if neither does.  The Android build is
# described after the iOS one, below.
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
# GLP_TEAM and GLP_BUNDLE_ID give the Apple team and the bundle identifier,
# by default the Xcode project's, G374822FDT and com.eshapiro.grassapp
# (common.sh); the build is checked to carry both.  The profile lists a phone
# only once the team has registered it: connect the phone and run, once, the
# `flutter run` line build.sh prints when the phone is missing, which
# registers it; the free team signs one phone per build.  GRASSROOTS (default
# /Users/udi/Grassroots) changes where it writes.
#
# For an Android phone it cleans, syncs the assets and builds the same entry
# with the same defines, in profile mode, as an .apk
# (`flutter build apk --profile`), signed with the debug key as the Gradle
# project has it (android/app/build.gradle.kts), so no team or profile is
# needed and one build serves every phone; copies it to
# $GRASSROOTS/tmp/phones/GrassApp-debug-<sha>.apk, GrassApp-<mode>-<sha>.apk
# for a scripted build; and installs it by `adb install -r`, within
# INSTALL_LIMIT seconds as on iOS.  The app's id there is the Gradle
# project's applicationId; GLP_TEAM and GLP_BUNDLE_ID are iOS's only.  The
# phone must be unlocked with USB debugging allowed for this Mac, and the
# app's Bluetooth and location permissions are granted on the phone at its
# first launch.
set -euo pipefail
ME=build.sh
if [ $# -lt 1 ] || [ $# -gt 3 ]; then
    echo "usage: build.sh <device> [<mode> [<known>]]   (devices.sh lists the phones, iPhones by UDID and Android phones by adb serial)" >&2
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

# The phone's platform: android when adb lists the serial as a ready device,
# ios when devicectl lists an iOS device of that UDID.  An Android phone that
# adb lists in another state (unauthorized, offline) stops the build, since
# adb would not install on it.
platform=
if command -v adb >/dev/null; then
    state=$(adb devices | awk -v id="$udid" 'NR > 1 && $1 == id { print $2 }')
    case "$state" in
        device) platform=android ;;
        "") ;;
        *) die "adb lists the Android phone $udid as $state, not as device: unlock it and allow USB debugging for this Mac, then run build.sh again" ;;
    esac
fi
if [ -z "$platform" ] && command -v xcrun >/dev/null; then
    J=$(mktemp)
    if xcrun devicectl list devices --json-output "$J" >/dev/null 2>&1 && python3 -I -c '
import json, sys
want = sys.argv[2].lower()
for d in json.load(open(sys.argv[1])).get("result", {}).get("devices", []):
    hw = d.get("hardwareProperties", {})
    if hw.get("platform") == "iOS" and hw.get("udid", "").lower() == want:
        sys.exit(0)
sys.exit(1)' "$J" "$udid"; then
        platform=ios
    fi
    rm -f "$J"
fi
[ -n "$platform" ] || die "no connected iPhone with UDID $udid and no Android phone with adb serial $udid; devices.sh lists them"
echo "$ME: $udid is an $([ $platform = ios ] && echo iOS || echo Android) device; building for it"

if [ $platform = android ]; then
    need flutter adb python3 perl git
else
    need flutter security codesign xcrun zip ideviceinstaller python3 perl git
fi
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

if [ $platform = android ]; then
    flutter build apk --profile -t "$entry" --dart-define-from-file="$W/defines.json"
    BUILT="$APP/build/app/outputs/flutter-apk/app-profile.apk"
    [ -f "$BUILT" ] || die "no $BUILT after the build"
    APK="$OUT/GrassApp-$kind-$sha.apk"
    cp "$BUILT" "$APK"
    echo "build.sh: $APK"
    status=0
    within "$LIMIT" adb -s "$udid" install -r "$APK" || status=$?
    if [ $status = 142 ]; then
        die "the install did not finish within $LIMIT s and was stopped; unlock the phone, check it with adb devices, and run build.sh again"
    elif [ $status != 0 ]; then
        die "adb install failed, status $status; if it says INSTALL_FAILED_UPDATE_INCOMPATIBLE, the app installed there is signed with another key: remove it (adb -s $udid uninstall <the app's id>) and run build.sh again"
    fi
    echo "build.sh: installed on $udid; open the Grassroots App there, and its run appears in GET /log/ (follow.sh follows it)"
    exit 0
fi

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
[ "$team" = "$TEAM" ] || die "the build is signed with a profile of team $team, not of $TEAM"
built_id=$(python3 -I -c 'import plistlib, sys; print(plistlib.load(open(sys.argv[1], "rb")).get("CFBundleIdentifier", ""))' "$RUNNER/Info.plist")
[ "$built_id" = "$BUNDLE_ID" ] || die "the app's bundle identifier is $built_id, not $BUNDLE_ID"
if [ "$listed" != yes ]; then
    die "the profile signing this build does not list the phone $udid; connect it and run once, from $APP: FLUTTER_XCODE_DEVELOPMENT_TEAM=$TEAM FLUTTER_XCODE_GLP_BUNDLE_ID=$BUNDLE_ID flutter run -d $udid -t lib/main_superapp.dart, which registers it; then build.sh $udid again"
fi

# Every binary under Runner.app/Frameworks built for iOS (a simulator build's
# says IOSSIMULATOR) and signed by the team (an ad hoc signature has none).
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
