#!/bin/bash
# follow.sh <phone> <run>: follows a run of a phone's debugging log
# (servers/debuglog, GLP/phones/docs/debugging.md, "The log service", 3) into
# /Users/udi/Grassroots/tmp/phones/logs/<phone>-<run>.jsonl, appending as the
# phone posts, until Ctrl-C.  <phone> is the agent's key in hex and <run> the
# launch's identifier, as GET /log/ lists them.  When the connection drops it
# reconnects and asks only for what the file does not yet hold, so a run is
# never written twice; a refusal (a wrong password, no such name) stops it.
# The password is the repository's, phones/tools/debuglog-password, given to
# curl on its standard input.  GRASSROOTS (default /Users/udi/Grassroots) and
# DEBUGLOG_URL (default https://peoplesweb.org/log) change where it writes and
# where it reads, for a test.
set -u
if [ $# -ne 2 ] || ! [[ "$1" =~ ^[0-9a-fA-F]+$ ]] || ! [[ "$2" =~ ^[0-9]+$ ]]; then
    echo "usage: follow.sh <phone: the key in hex> <run: a number>" >&2
    exit 2
fi
phone=$1 run=$2
G=${GRASSROOTS:-/Users/udi/Grassroots}
URL=${DEBUGLOG_URL:-https://peoplesweb.org/log}
PW="$(cd "$(dirname "$0")" && pwd)/debuglog-password"
if [ ! -s "$PW" ]; then
    echo "follow.sh: no password in $PW" >&2
    exit 2
fi
LOGS="$G/tmp/phones/logs"
mkdir -p "$LOGS"
OUT="$LOGS/$phone-$run.jsonl"
touch "$OUT"
trap 'echo; echo "follow.sh: stopped; $(wc -l < "$OUT" | tr -d " ") lines in $OUT"; exit 0' INT TERM
echo "follow.sh: following $phone $run into $OUT; Ctrl-C stops"
while true; do
    from=$(wc -c < "$OUT" | tr -d ' ')
    printf 'header = "Authorization: Bearer %s"\n' "$(tr -d '\r\n' < "$PW")" \
        | curl -sS -N --fail -K - "$URL/$phone/$run?follow&from=$from" >> "$OUT"
    status=$?
    if [ $status -eq 22 ]; then
        # The server refused, a wrong password or no such name; curl's line
        # above gives the status, and nothing was written.
        echo "follow.sh: refused by $URL" >&2
        exit 1
    fi
    echo "follow.sh: the connection ended (curl $status); reconnecting from byte $(wc -c < "$OUT" | tr -d ' ')" >&2
    sleep 2
done
