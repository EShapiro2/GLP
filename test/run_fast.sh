#!/bin/bash
# The fast tier of the GLP suite (Udi, 2026-10-14: a fast suite per merge on
# main, the full suite on main every 3-4 hours): the sections FAST_SECTIONS
# names in run_all_tests.sh, with any sections given as arguments added ---
# a merge names the sections its branches touch:
#     bash test/run_fast.sh VG N3
# Its totals line reads "FAST Total: ..."; it is not the full suite, which is
# run_all_tests.sh with no GLP_SECTIONS.
GLP_SECTIONS="fast $*" exec bash "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/run_all_tests.sh"
