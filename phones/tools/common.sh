# common.sh: what build.sh and build_sim.sh share, read by both with `.`.
# The caller sets ME, its own name for its messages, and HERE, the directory
# of phones/tools.
#
# GLP, the clone the tools are in; PW, the debugging log's password, in the
# repository and not secret (phones/docs/debugging.md, "For Udi's approval",
# 1); G, under which the builds are written, $G/tmp/phones.  The caller has
# `set -u`; GRASSROOTS, GLP_TEAM and GLP_BUNDLE_ID may be unset.
GLP=$(cd "$HERE/../.." && pwd)
PW="$HERE/debuglog-password"
G=${GRASSROOTS:-/Users/udi/Grassroots}
die() { echo "$ME: $*" >&2; exit 1; }

# TEAM and BUNDLE_ID: the Apple team that signs the app and the app's bundle
# identifier, from GLP_TEAM and GLP_BUNDLE_ID, by default the Xcode project's,
# so that anyone builds with their own team without editing the project
# (phones/docs/contributors.md).  Flutter gives every FLUTTER_XCODE_<setting>
# variable to Xcode as a build setting: DEVELOPMENT_TEAM for every target, and
# GLP_BUNDLE_ID, which only the Runner target reads, its
# PRODUCT_BUNDLE_IDENTIFIER being $(GLP_BUNDLE_ID), so that the frameworks
# keep their own identifiers.
PBX="$GLP/glp_multiagent/ios/Runner.xcodeproj/project.pbxproj"
TEAM=${GLP_TEAM:-$(sed -n '/^[[:space:]]*DEVELOPMENT_TEAM = \([A-Z0-9]*\);$/{s//\1/p;q;}' "$PBX")}
BUNDLE_ID=${GLP_BUNDLE_ID:-$(sed -n '/^[[:space:]]*GLP_BUNDLE_ID = \([^;]*\);$/{s//\1/p;q;}' "$PBX")}
[ -n "$TEAM" ] || die "no team: set GLP_TEAM"
[ -n "$BUNDLE_ID" ] || die "no bundle identifier: set GLP_BUNDLE_ID"
export FLUTTER_XCODE_DEVELOPMENT_TEAM="$TEAM" FLUTTER_XCODE_GLP_BUNDLE_ID="$BUNDLE_ID"

# need <command>...: stops unless each command is on the path.
need() {
    local t
    for t in "$@"; do
        command -v "$t" >/dev/null || die "$t is not on PATH"
    done
}

# write_defines <file> <mode> <known>: the Dart defines, in a file of mode
# 600: the password, and for a scripted build the mode and the keys known
# (debugging.md, "The scripted build"); the app refuses a mode no play runs
# and a key that is not one.
write_defines() {
    [ -s "$PW" ] || die "no password in $PW"
    (umask 077 && python3 -I -c '
import json, sys
d = {"GLP_DEBUGLOG": open(sys.argv[1]).read().strip()}
if sys.argv[2]:
    d["GLP_PERSON"] = sys.argv[2]
    d["GLP_KNOWN"] = sys.argv[3]
print(json.dumps(d))' "$PW" "$2" "$3" > "$1")
}

# within <seconds> <command>...: runs the command, killed if it has not
# finished within the time; answers its status, 142 when it was killed.
within() {
    local s=$1
    shift
    perl -e 'alarm shift; exec @ARGV or die "exec $ARGV[0]: $!\n"' "$s" "$@"
}
