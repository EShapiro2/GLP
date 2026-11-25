#!/bin/sh
# The live site on each push to GLP main (Udi, 2026-11-25 01:02 UTC: "when I am waiting for something, you do it pronto not wait for the hourly update").  Run each minute by peoplesweb-main.timer: it asks GitHub for main's head and, when the live clone is not at it, starts the sweep (peoplesweb-sweep.service), which pulls, builds and publishes the site, /next and /villagers.  The hourly sweep still runs.
set -e
cd /srv/glp
if ! out=$(git ls-remote --heads "$(git remote get-url origin)" main); then
	echo "main-watch: asking GitHub for main failed" >&2
	exit 0
fi
head=$(printf '%s\n' "$out" | cut -f1)
[ -n "$head" ] || exit 0
[ "$head" = "$(git rev-parse HEAD)" ] && exit 0
systemctl is-active -q peoplesweb-sweep.service || systemctl start --no-block peoplesweb-sweep.service
