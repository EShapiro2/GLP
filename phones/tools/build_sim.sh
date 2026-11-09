#!/bin/bash
# build_sim.sh <mode> [<simulator> [<known>]]: the scripted build of the
# Grassroots App (GLP/phones/docs/debugging.md, "The scripted build") for a
# simulator, its reporter on, installed on the simulator named <simulator>
# (default "iPhone 16e") with xcrun simctl.  <mode> and <known> are build.sh's:
# the scripted person of programs/social/graph/core/superapp_plays.glp, and
# the keys it knows at the start, in hex, separated by commas.
#
# It builds in a clone of its own, $GRASSROOTS/tmp/phones/GLP-sim, made if
# missing, and never in the clone it is run from, the phones' build clone:
# a framework a simulator build leaves in a build tree is taken into the next
# phone build there, and iOS refuses it (2026-11-08, objective_c.framework).
# The clone is set to the commit of the clone this script is in, with that
# clone's uncommitted changes to tracked files; a file not yet added to git is
# not carried, and the script names it.  The defines are build.sh's, in a file
# of mode 600, deleted after.  The simulator is booted if it is not, and the
# install stopped if it has not finished within INSTALL_LIMIT seconds
# (default 300).  GRASSROOTS (default /Users/udi/Grassroots) changes where it
# writes.
set -euo pipefail
ME=build_sim.sh
if [ $# -lt 1 ] || [ $# -gt 3 ]; then
    echo "usage: build_sim.sh <mode> [<simulator> [<known>]]   (xcrun simctl list devices available lists the simulators)" >&2
    exit 2
fi
mode=$1
device=${2:-iPhone 16e}
known=${3:-}
HERE=$(cd "$(dirname "$0")" && pwd)
. "$HERE/common.sh"
LIMIT=${INSTALL_LIMIT:-300}
need flutter xcrun python3 perl git
SIM="$G/tmp/phones/GLP-sim"
mkdir -p "$G/tmp/phones"
[ "$(cd "$GLP" && pwd -P)" != "$(mkdir -p "$SIM" && cd "$SIM" && pwd -P)" ] || die "run from the phones' clone, not from $SIM"

udid=$(xcrun simctl list devices available | grep -E "^ +$device \(" | head -1 | sed -E 's/.*\(([0-9A-F-]{36})\).*/\1/') || true
[ -n "$udid" ] || die "no available simulator named $device; xcrun simctl list devices available lists them"

W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT
write_defines "$W/defines.json" "$mode" "$known"

# The clone: the source's commit, then its uncommitted changes.
if ! git -C "$SIM" rev-parse --git-dir >/dev/null 2>&1; then
    rmdir "$SIM" 2>/dev/null || die "$SIM is there and is not a clone"
    git clone -q "$GLP" "$SIM"
fi
git -C "$SIM" fetch -q "$GLP" HEAD
git -C "$SIM" checkout -q -f --detach FETCH_HEAD
git -C "$SIM" clean -fdq
git -C "$GLP" diff --binary HEAD > "$W/local.patch"
if [ -s "$W/local.patch" ]; then
    git -C "$SIM" apply "$W/local.patch" || die "the uncommitted changes of $GLP did not apply to $SIM"
    echo "build_sim.sh: with the uncommitted changes of $GLP"
fi
untracked=$(git -C "$GLP" ls-files --others --exclude-standard -- glp_multiagent glp_runtime networking programs)
if [ -n "$untracked" ]; then
    echo "build_sim.sh: not carried, being in no commit and not added to git:" >&2
    printf '%s\n' "$untracked" | sed 's/^/  /' >&2
fi
echo "build_sim.sh: building $(git -C "$SIM" rev-parse --short=8 HEAD) in $SIM for $device ($udid)"

cd "$SIM/glp_multiagent"
bash tool/sync_glp_assets.sh
flutter build ios --simulator --debug -t lib/main_scripted.dart --dart-define-from-file="$W/defines.json"
RUNNER="$SIM/glp_multiagent/build/ios/iphonesimulator/Runner.app"
[ -d "$RUNNER" ] || die "no $RUNNER after the build"

xcrun simctl boot "$udid" 2>/dev/null || true
status=0
within "$LIMIT" xcrun simctl bootstatus "$udid" -b >/dev/null || status=$?
[ $status = 0 ] || die "the simulator $device did not boot within $LIMIT s (status $status)"
open -a Simulator 2>/dev/null || true
status=0
within "$LIMIT" xcrun simctl install "$udid" "$RUNNER" || status=$?
if [ $status = 142 ]; then
    die "the install did not finish within $LIMIT s and was stopped"
elif [ $status != 0 ]; then
    die "simctl install failed, status $status"
fi
echo "build_sim.sh: installed on $device ($udid), scripted person $mode; open the Grassroots App there, and its run appears in GET /log/ (follow.sh follows it)"
