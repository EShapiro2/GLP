#!/bin/bash
# The check at each merge on main (Udi, 2026-10-24, to Code #15: "70 minutes is
# bogus ... we should sample the tests, randomly or whatever, run a 10th of them
# or less"): a random tenth of the suite's sections, at least one, drawn afresh
# at every run, with no section given more weight than another.  The full suite,
# run_all_tests.sh with no GLP_SECTIONS, runs once a night at 02:00 UTC
# (test/nightly.sh).  Its totals line reads "PARTIAL Total: ..." and it prints
# the sections it drew first.
#     bash test/run_sample.sh
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ALL=$(sed -n 's/^ALL_SECTIONS="\(.*\)"$/\1/p' "$HERE/run_all_tests.sh")
N=$(echo $ALL | wc -w | tr -d ' ')
K=$(( (N + 9) / 10 ))
DRAWN=$(echo $ALL | tr ' ' '\n' | awk -v s="$RANDOM$$" 'BEGIN{srand(s)} {print rand() "\t" $0}' | sort -n | head -n $K | cut -f2 | tr '\n' ' ')
echo "Sample of $K of $N sections: $DRAWN"
GLP_SECTIONS="$DRAWN" exec bash "$HERE/run_all_tests.sh"
