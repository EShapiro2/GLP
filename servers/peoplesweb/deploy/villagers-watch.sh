#!/bin/sh
# The Villagers site, peoplesweb.org/villagers, on each push to EShapiro2/Villagers (Udi, 2026-11-25: "follow updates of the Villagers repo as they arrive").  Run each minute by peoplesweb-villagers.timer: it asks GitHub for the repository's head, with the sweep's read-only token, and runs deploy/villagers.sh only when that head is not the one the site was last built from, which villagers.sh records in $WORK/built.  The hourly sweep builds the site too, so a change to the build on main shows within the hour.
set -e
GLP=${GLP_ROOT:-/srv/glp}
WORK=${VILLAGERS_WORK:-/var/lib/glp/villagers}
TOKEN=${PAPERS_TOKEN:-/etc/glp/github-token}
basic=$(printf 'x-access-token:%s' "$(cat "$TOKEN")" | base64 | tr -d '\n')
# A failed ask of GitHub is reported and the run ends; the next minute asks again.
if ! out=$(git -c "http.https://github.com/.extraheader=AUTHORIZATION: basic $basic" ls-remote https://github.com/EShapiro2/Villagers.git HEAD); then
	echo "villagers: asking GitHub for EShapiro2/Villagers failed" >&2
	exit 0
fi
head=$(printf '%s\n' "$out" | cut -f1)
[ -n "$head" ] || exit 0
[ "$head" = "$(cat "$WORK/built" 2>/dev/null)" ] && exit 0
sh "$GLP/servers/peoplesweb/deploy/villagers.sh"
