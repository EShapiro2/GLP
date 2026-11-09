# common.sh: what build.sh and build_sim.sh share, read by both with `.`.
# The caller sets ME, its own name for its messages, and HERE, the directory
# of phones/tools.
#
# GLP, the clone the tools are in; PW, the debugging log's password, in the
# repository and not secret (phones/docs/debugging.md, "For Udi's approval",
# 1); G, under which the builds are written, $G/tmp/phones.
GLP=$(cd "$HERE/../.." && pwd)
PW="$HERE/debuglog-password"
G=${GRASSROOTS:-/Users/udi/Grassroots}
die() { echo "$ME: $*" >&2; exit 1; }

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
