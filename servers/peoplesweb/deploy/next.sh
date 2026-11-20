#!/bin/sh
# The test site, peoplesweb.org/next, on each push to Website-site (Udi, 2026-11-20 20:26 UTC, "yes" to the Optimiser's proposal, section 2 of its proposals of 2026-11-20).  Run each minute by peoplesweb-next.timer: it asks GitHub for the branch's head and runs deploy/test.sh only when that head is not the one /next was last built from, which test.sh records in $WORK/built.  The hourly sweep still builds /next too.
set -e
BRANCH=${TEST_BRANCH:-Website-site}
GLP=${GLP_ROOT:-/srv/glp}
WORK=${TEST_WORK:-/var/lib/glp/test}
AS=${TEST_AS-sudo -u glp}
cd "$GLP"
URL=${TEST_URL:-$(git remote get-url origin)}
# A failed ask of GitHub is reported and the run ends; the next minute asks again.
if ! out=$($AS git ls-remote --heads "$URL" "$BRANCH"); then
	echo "next: asking GitHub for $BRANCH failed" >&2
	exit 0
fi
head=$(printf '%s\n' "$out" | cut -f1)
[ -n "$head" ] || exit 0
[ "$head" = "$(cat "$WORK/built" 2>/dev/null)" ] && exit 0
sh servers/peoplesweb/deploy/test.sh
