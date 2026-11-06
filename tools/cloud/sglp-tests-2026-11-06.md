# sGLP on GLP's simulated time, tested in the cloud

Cloud #3, 2026-11-06, for Code #24's task of 10:51 UTC.  Code tested: GLP main 77ad65a2, which holds 1ab1e732.  The container has 4 cores and 15 GiB, Linux, Dart 3.13.7; it is about half the Mac's speed (the 100-agent year: 52.8 s here, 28 s on the Mac).  No run went over ten minutes.

## A fault, reported to Code

`programs/sglp/coins/run.sh` line 177 runs the REPL under `/usr/bin/time -l -p`, which is macOS's.  On Linux no coins run starts, and test (ix)'s nine checks fail.  For every run below, the container had a stand-in `/usr/bin/time` that drops the options and runs the command; nothing in the repository was changed.

## The tests

| What | Result |
|---|---|
| `programs/sglp/test_sglp.sh`, without the stand-in | 94 passed, 9 failed, all nine test (ix), the fault above |
| `programs/sglp/test_sglp.sh`, with it | 103 passed, 0 failed |
| Section SGLP, `GLP_SECTIONS=SGLP bash test/run_all_tests.sh` | `PARTIAL Total: 109 | Passed: 109 | Failed: 0` |

## The runs Code listed

Every run exited 0, ended at its clock, and was read whole by `friendship.awk` or `circulation.awk`: unread 0, and for coins unmatched 0 and unordered 0.

| Runs | Count | Wall-clock |
|---|---|---|
| Social graph, 100 agents, a year, 60/70, seeds 1 to 20 and 20260927 | 21 | 37 to 62 s, two at a time |
| Sweep, nine mixes, seeds 11 to 20 | 90, the 60/70 ten from the row above | 9 to 214 s, three at a time |
| Coins grid, nine mixes, seeds 1 to 20, each over the social graph of its seed | 180 | 4 to 23 s, three at a time |
| Coins s0-c50, s50-c50, s100-c50 over seed 20260927's graph | 3 | in the row above |
| Cost, 30 days at 100, 200, 500 and 1,000 agents; a year at 100 | 5 | alone |

The sweep, seeds 11 to 20, at the year's end; the paper's Table 1 is over seeds 1 to 20:

| homophile | wary | edges | across the sexes | one component by month, mean and latest |
|---|---|---|---|---|
| none | none | 4,175 ± 31 | 2,104 ± 21 | 1.0, 1 |
| none | 70% | 2,585 ± 173 | 1,296 ± 83 | 1.7, 3 |
| none | all | 1,535 ± 34 | 768 ± 22 | 2.3, 3 |
| 60% | none | 3,144 ± 107 | 1,101 ± 95 | 1.3, 2 |
| 60% | 70% | 1,846 ± 151 | 606 ± 71 | 2.8, 4 |
| 60% | all | 1,034 ± 69 | 333 ± 42 | 3.9, 6 |
| all | none | 2,113 ± 18 | 87 ± 12 | 1.2, 2 |
| all | 70% | 1,216 ± 92 | 46 ± 7 | 3.7, 5 |
| all | all | 671 ± 23 | 22 ± 6 | 4.8, 7 |

The 60/70 mix over seeds 1 to 20 gives 1,821 ± 157 edges, 590 ± 85 across the sexes, and one component by month 3.2 on average and 6 at the latest.

The coins grid, seeds 1 to 20, at the year's end, against the paper's Table 2:

| spenders | cautious | circulation | proposals | declined |
|---|---|---|---|---|
| none | none, half, all | 1,676 ± 108 | 693 ± 11 | 0 |
| half | none | 11,632 ± 652 | 2,959 ± 151 | 0 |
| half | half | 11,228 ± 609 | 2,999 ± 154 | 67.7 ± 18.8 |
| half | all | 10,072 ± 474 | 3,104 ± 174 | 242.2 ± 61.5 |
| all | none | 1,765 ± 173 | 4,384 ± 27 | 0 |
| all | half | 1,765 ± 173 | 4,383 ± 26 | 0.1 ± 0.2 |
| all | all | 1,765 ± 191 | 4,384 ± 27 | 1.8 ± 2.4 |

Every row of both tables lies within the paper's spread.

## A gap between the paper and the code

The Results paragraphs of Sections 5 and 6 do not come out of the code at main.  Seed 20260927's year draws 58 homophile and 70 wary agents, where the paper says 60 and 65.  It answers 7,532 menus and 7,523 offers, 3,248 of them yes, and ends with 1,899 edges, where the paper says 7,399, 7,389, 3,400 and 1,916.  The coins year at that seed pays 17,334 times and proposes 2,929 swaps, where the paper says 17,630 and 2,905.  The paper's seeds paragraph, Appendix B, dates from sGLP c92bb56 (2026-10-24).  Its Results date from 2bc479b (2026-11-05).  The code took that paragraph's seeds only at GLP 5bb79c68 (2026-11-06).  So the Results, the two tables, the figures and the cost paragraph were made with the earlier draws and must be rerun from main; Section SGLP's test (vii) confirms that the code's draw is Appendix B's.

## The change, before and after, on this machine

The before tree is GLP main bc7c4b49, the GLP monitor with its skew heap, run alone with the same seed.

| Run | Before | After |
|---|---|---|
| 100 agents, a year | 65.7 s, 14,788 answers, 4.4 ms an answer, peak 198 MiB | 52.8 s, 15,055 answers, 3.5 ms an answer, peak 76 MiB |
| 1,000 agents, ten minutes | 63,520 answers, 5.2 simulated months, peak 295 MiB | 66,986 answers, 5.5 simulated months, peak 305 MiB |
| 30 days, 100 / 200 / 500 / 1,000 agents | | 3.0 / 1.7 / 1.2 / 1.0 ms an answer, peak 74 to 90 MiB |

At 100 agents the engine's clock takes a fifth off the cost of an answer and three fifths off the peak memory.  At 1,000 agents it gains about 5%: there the cost is the agent's known list, walked by `one_of`, the `ground` guards and the log line, as the comparison of 2026-11-04 says (`sglp-vs-spifcp-2026-11-04.md`, items 6, 12 and 13).
