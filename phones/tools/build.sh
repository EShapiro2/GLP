#!/bin/bash
# build.sh <udid> [<mode> [<known>]]: a debug build of the Grassroots App for
# one phone, its reporter on (GLP/phones/docs/debugging.md, "The phone side",
# 1, and "The Mac"), installed by ideviceinstaller, the path that worked on
# 2026-11-08 where Xcode's and devicectl's installs hung.
#
# With <mode>, the scripted build ("The scripted build"): its person is the
# scripted person <mode> of programs/social/graph/core/superapp_plays.glp,
# encounter_befriend, encounter, befriend_known or encounter_introduce, the
# Dart define GLP_PERSON; <known>, the keys the person knows at the start, in
# hex, separated by commas, the define GLP_KNOWN, none where it is not given.
# It builds lib/main_scripted.dart in place of lib/main_superapp.dart, and
# names the .ipa with the mode.
#
# It syncs the GLP assets, builds the entry with the Dart define GLP_DEBUGLOG,
# the password in /Users/udi/Grassroots/.debuglog-password, and
# Flutter signs the build with the team of the Xcode project
# (ios/Runner.xcodeproj).  The defines are given in a file of mode 600,
# deleted after, so the password is on no command line.  It then checks that the
# profile signing the build lists the phone, zips Runner.app as
# Payload/Runner.app into /Users/udi/Grassroots/tmp/phones/GrassApp-debug-<sha>.ipa,
# GrassApp-<mode>-<sha>.ipa for a scripted build, and installs it.
#
# The profile lists a phone only once the team has registered it: connect the
# phone and run, once, `flutter run -d <udid> -t lib/main_superapp.dart` from
# glp_multiagent, which registers it; the free team signs one phone per build.
# GRASSROOTS (default /Users/udi/Grassroots) changes where it reads and writes.
set -euo pipefail
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
G=${GRASSROOTS:-/Users/udi/Grassroots}
HERE=$(cd "$(dirname "$0")" && pwd)
GLP=$(cd "$HERE/../.." && pwd)
APP="$GLP/glp_multiagent"
PW="$G/.debuglog-password"
die() { echo "build.sh: $*" >&2; exit 1; }
[ -s "$PW" ] || die "no password in $PW (servers/debuglog/README.md, \"The server\")"
for t in flutter security zip ideviceinstaller python3 git; do
    command -v "$t" >/dev/null || die "$t is not on PATH"
done
sha=$(git -C "$GLP" rev-parse --short=8 HEAD)
[ -z "$(git -C "$GLP" status --porcelain -- glp_multiagent glp_runtime networking programs)" ] || sha="$sha-dirty"
OUT="$G/tmp/phones"
mkdir -p "$OUT"
IPA="$OUT/GrassApp-$kind-$sha.ipa"
W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT
# The defines: the password, and for a scripted build the mode and the keys
# known; the app refuses a mode no play runs and a key that is not one.
(umask 077 && python3 -I -c '
import json, sys
d = {"GLP_DEBUGLOG": open(sys.argv[1]).read().strip()}
if sys.argv[2]:
    d["GLP_PERSON"] = sys.argv[2]
    d["GLP_KNOWN"] = sys.argv[3]
print(json.dumps(d))' "$PW" "$mode" "$known" > "$W/defines.json")

cd "$APP"
bash tool/sync_glp_assets.sh
flutter build ios --debug -t "$entry" --dart-define-from-file="$W/defines.json"

RUNNER="$APP/build/ios/iphoneos/Runner.app"
[ -d "$RUNNER" ] || die "no $RUNNER after the build"
PROFILE="$RUNNER/embedded.mobileprovision"
[ -f "$PROFILE" ] || die "the build has no profile, so it was not signed: open ios/Runner.xcworkspace in Xcode and set the team"
if ! security cms -D -i "$PROFILE" | grep -qi "<string>$udid</string>"; then
    die "the profile signing this build does not list the phone $udid; connect it and run once, from $APP: flutter run -d $udid -t lib/main_superapp.dart, which registers it; then build.sh $udid again"
fi

mkdir "$W/Payload"
cp -R "$RUNNER" "$W/Payload/"
rm -f "$IPA"
(cd "$W" && zip -qry "$IPA" Payload)
echo "build.sh: $IPA"
# ideviceinstaller's newer releases take `install <file>`, the older `-i <file>`.
if ideviceinstaller --help 2>&1 | grep -qE '^[[:space:]]+install[[:space:]]'; then
    ideviceinstaller -u "$udid" install "$IPA"
else
    ideviceinstaller -u "$udid" -i "$IPA"
fi
echo "build.sh: installed on $udid; open the Grassroots App there, and its run appears in GET /log/ (follow.sh follows it)"
