# The sGLP paper's runs on GLP main e92fb71d (Cloud #3, 2026-11-07)

Code #25's task 4 (Cloud_inbox 2026-11-07 08:26 UTC): the runs the sGLP paper reports that measure outcomes and not time, re-made on main e92fb71d, which holds sGLP's items 1 and 2.  The runs are in `runs/`, under October's names (`october-names.txt`): for each run its `.out` (run.sh's output), `.txt` (friendship.awk or circulation.awk on the log), `.draw` and, for the social graph, `.graph`.  The logs, 1.3 GB, are not committed; each run remakes its log from its seed.  `runs/sglp-sweep-tables.txt` is tables.sh's summary.

How: `runs-cloud.sh`, `meas-cloud.sh` and `tables-cloud.sh` are runs.sh, meas.sh and tables.sh with the container's paths, run without `/usr/bin/time`, which the container lacks, and without the Mac's run-slot wrapper (`nolock.sh` drops it); `meas-cloud.sh` leaves out the cost runs, which are the Mac's.  `drive-cloud.sh` ran them, three at a time, each stopped at ten minutes, from 13:13 to 14:31 UTC.  Container: Linux, 4 cores, Dart 3.9.4.

All 368 runs exited 0, and every log was read whole (unread 0).  No coins run was left out.

Not remade, of October's names: the cost runs (`sglp-cost-*`, the Mac's), the five-year runs (`sglp-k5y-*`) and the persons known (`sglp-r-100y-*.known`), which the task does not name.  Added: `sglp-sweep-h60-w70-20260927`, whose graph the grid's three runs at seed 20260927 read.

## The social graph, 100 agents, one year, 60/70 (Section 5)

| | seed 20260927 | seed 1 | 2026-11-04, 20260927 | 2026-11-04, 1 |
|---|---|---|---|---|
| draw | homophile 58 indifferent 42 wary 70 sociable 30 | homophile 60 indifferent 40 wary 77 sociable 23 | homophile 60 indifferent 40 wary 65 sociable 35 | homophile 55 indifferent 45 wary 68 sociable 32 |
| answers | 15055 | 14341 | 14788 | 14995 |
| menu_answers | 7532 | 7176 | 7399 | 7505 |
| approval_answers | 7523 | 7165 | 7389 | 7490 |
| yes | 3248 | 2699 | 3400 | 3193 |
| edges | 1899 | 1617 | 1916 | 1884 |
| month 1 | edges 191 components 8 isolated 7 largest 93 within 137 across 54 | edges 126 components 11 isolated 10 largest 90 within 88 across 38 | | |
| month 12 | edges 1899 components 1 isolated 0 largest 100 within 1257 across 642 | edges 1617 components 1 isolated 0 largest 100 within 1105 across 512 | | |
| one component from month | 5 | 4 | 5 | 3 |

## Coins among friends, 100 agents, one year, 50/50, over the seed's graph (Section 6)

| | seed 20260927 | seed 1 | 2026-11-04, 20260927 | 2026-11-04, 1 |
|---|---|---|---|---|
| answers | 23192 | 24464 | 23440 | 22783 |
| menu_answers | 20263 | 21343 | 20535 | 19883 |
| offer_answers | 2929 | 3121 | 2905 | 2900 |
| pays | 17334 | 18222 | 17630 | 16983 |
| proposals | 2929 | 3121 | 2905 | 2900 |
| accepted | 2859 | 3046 | 2861 | 2846 |
| declined | 70 | 75 | 44 | 54 |
| circulation | 10327 | 11862 | 9871 | 11108 |
| holdings | 726 | 701 | 694 | 705 |
| wallets | 96 | 96 | 97 | 97 |

Over seed 20260927's graph, at cautious 50: spenders 0 pay 4,488 times, circulation 1,451, declined 0; spenders 50 as above; spenders 100 pay 32,042 times, circulation 1,951, declined 1.

## The sweep and the grid, seeds 1 to 20

Tables 1 and 2, mean ± sample sd, and the values per seed: `runs/sglp-sweep-tables.txt`.

## Section 4, "The claim, tested"

tests/busy: loads, and prints after twice, before never (`runs/sglp-claim-busy.out`).  tests/law, law(10000, S): S = stats(10000, 0.21751575689391048, 0.04676802397673373); mean 2.12% under 1/4.5, variance 5.30% under 1/4.5^2 (`runs/sglp-claim-law.out`).
