# sGLP #10 item 2(c): the paper's runs under SplitMix64

Made 2026-11-04 by Code #23's subagent sGLP-runs, on worktree /Users/udi/Grassroots/GLP-worktrees/sGLP-runs at f336d119 (main), clean, nothing edited or committed.  Every run through tmp/glp-run.sh, four at a time; the 1,000-agent run alone (GLP_RUN_EXCLUSIVE=1).  Other sessions' runs kept the load at 6 to 10 throughout, so every wall-clock figure here is under that load.

The commands are October's, as tmp/sglp-meet-runs.sh and tmp/sglp-meet-cost.sh made the runs of 2026-10-16 (tmp/sglp-meet/tmp/, the source of the paper's figures), in /Users/udi/Grassroots/tmp/sglp-splitmix/runs.sh, meas.sh and k1000.sh; progress in progress.txt.  Tables by tables.sh (tmp/sglp-meet-tables.sh adapted), whole in sglp-sweep-tables.txt.

Two departures from October's file names, as the brief asks: the coins grid over seeds 1 to 20 is named sglp-coins-grid-s<S>-c<C>-<seed> (October named it sglp-coins-sweep- and ran its grid over seeds 20260927 and 1 only), each over sglp-sweep-h60-w70-<seed>.graph, the social graph of its seed at the run declaration's mix; and coins-100y runs over sglp-r-100y-<seed>.graph (October: sglp-grid-h60-w70-<seed>.graph, the same run).  sglp-sweep-h60-w70-1's log, graph and draw are byte for byte sglp-r-100y-1's.

All 364 runs of 100 agents exited 0; no coins run was left out (no agent on no edge); unread 0 in every social-graph run, unread/unordered/unmatched 0/0/0 in every coins run.

## The social graph, 100 agents, one year, 60/70 (Section 5)

| | seed 20260927 | seed 1 | October 20260927 | October 1 |
|---|---|---|---|---|
| answers | 14788 | 14995 | 14459 | 15033 |
| menu_answers | 7399 | 7505 | 7235 | 7523 |
| approval_answers | 7389 | 7490 | 7224 | 7510 |
| yes | 3400 | 3193 | 2873 | 3675 |
| edges | 1916 | 1884 | 1709 | 2094 |
| draw | homophile 60 indifferent 40 wary 65 sociable 35 | homophile 55 indifferent 45 wary 68 sociable 32 | homophile 61 indifferent 39 wary 74 sociable 26 | homophile 55 indifferent 45 wary 61 sociable 39 |
| month 1, seed 20260927 | edges 182 components 7 isolated 5 largest 93 within 128 across 54 | | | |
| month 12, seed 20260927 | edges 1916 components 1 isolated 0 largest 100 within 1311 across 605 | | | |
| one component from month, seed 20260927 | 5 | | | |
| month 1, seed 1 | edges 179 components 6 isolated 5 largest 95 within 116 across 63 | | | |
| month 12, seed 1 | edges 1884 components 1 isolated 0 largest 100 within 1208 across 676 | | | |
| one component from month, seed 1 | 3 | | | |
| wall-clock (run.sh) | 32 s | 44 s | 25 s | 34 s |

Persons known and offers, seed 20260927 (sglp-k5y-known.awk, known.awk's rules, Approval read): known_at 2629800 mean 12.80 least 4 most 41 agents 100 no_menu 0;known_at 31557600 mean 97.67 least 91 most 99 agents 100 no_menu 0; offers 7399 other_sex 2189 share 0.2959;yes_within 2406 yes_across 994 yes_to_friend_within 1095 yes_to_friend_across 389;

Persons known and offers, seed 1 (sglp-k5y-known.awk, known.awk's rules, Approval read): known_at 2629800 mean 11.79 least 4 most 23 agents 100 no_menu 0;known_at 31557600 mean 98.04 least 92 most 99 agents 100 no_menu 0; offers 7505 other_sex 2429 share 0.3237;yes_within 2139 yes_across 1054 yes_to_friend_within 931 yes_to_friend_across 378;

## Coins among friends, 100 agents, one year, 50/50, over the seed's new graph (Section 6)

| | seed 20260927 | seed 1 | October 20260927 | October 1 |
|---|---|---|---|---|
| answers | 23440 | 22783 | 23238 | 21233 |
| menu_answers | 20535 | 19883 | 20240 | 18574 |
| offer_answers | 2905 | 2900 | 2998 | 2659 |
| pays | 17630 | 16983 | 17241 | 15914 |
| proposals | 2905 | 2900 | 2999 | 2660 |
| accepted | 2861 | 2846 | 2895 | 2630 |
| declined | 44 | 54 | 103 | 29 |
| circulation | 9871 | 11108 | 11716 | 9678 |
| holdings | 694 | 705 | 698 | 718 |
| wallets | 97 | 97 | 94 | 93 |
| draw | spender 48 saver 52 generous 58 cautious 42 | spender 47 saver 53 generous 48 cautious 52 | spender 48 saver 52 generous 41 cautious 59 | spender 43 saver 57 generous 55 cautious 45 |
| graph | 1916 edges, agents 1..100 each on one at least | 1884 edges, agents 1..100 each on one at least | | |
| wall-clock | 13 s | 14 s | | |

Monthly lines (circulation.awk) in sglp-coins-100y-<seed>.txt.

## The sweep and the grid, seeds 1 to 20 (mean ± sample sd; values per seed in sglp-sweep-tables.txt)

```
TABLE 1 (mean ± sd over the seeds): the social graph at the end of the year
homophile wary             edges           across   one by month            menus            cards              yes
0         0         4169.0±23.1     2102.7±19.4       1.0±0.0    10423.5±74.8    10422.6±74.9     9380.4±66.4
0         70       2635.3±115.4     1325.2±62.8       2.1±1.0    10423.5±74.8    10404.2±72.7    4535.9±282.7
0         100       1540.5±34.5      775.0±19.9       2.7±1.0    10423.5±74.8    10395.6±72.8     2077.0±34.4
60        0         3126.5±92.0     1083.4±89.4       1.1±0.3    7296.9±242.8    7296.2±242.9    6572.0±214.5
60        70        1864.2±95.7      600.3±62.5       3.3±1.4    7296.9±242.8    7283.2±242.4    3189.8±192.0
60        100       1030.6±61.7      329.2±37.7       4.7±1.4    7296.9±242.8    7277.4±243.5     1458.1±68.0
100       0         2110.8±25.3        88.9±9.5       1.4±0.5     5219.4±68.9     5219.0±69.2     4701.5±67.0
100       70        1253.5±73.1       48.3±11.4       4.3±1.6     5219.4±68.9     5209.6±68.5    2260.9±166.9
100       100        674.5±25.2        20.8±4.3       5.0±1.4     5219.4±68.9     5205.3±67.6     1046.6±35.6

TABLE 2 (mean ± sd over the seeds): coins among friends at the end of the year, over sglp-sweep-h60-w70-<seed>.graph
spenders cautious      circulation      proposals     declined             pays       accepted       holdings      wallets
0        0           1692.7±132.3    695.2±12.9     0.0±0.0     4553.2±75.9    695.2±13.0    245.4±13.0    94.3±2.3
0        50          1692.7±132.3    695.2±12.9     0.0±0.0     4553.2±75.9    695.2±13.0    245.4±13.0    94.3±2.3
0        100         1692.7±132.3    695.2±12.9     0.0±0.0     4553.2±75.9    695.2±13.0    245.4±13.0    94.3±2.3
50       0          11869.9±617.8  3004.7±153.6     0.0±0.0  17946.5±1069.5  3004.3±153.7    750.0±27.2    95.5±2.9
50       50         11453.8±592.6  3050.7±155.4   67.8±22.7  17911.0±1102.8  2982.4±148.6    744.2±27.7    95.3±2.1
50       100        10384.9±473.0  3147.2±151.7  233.9±40.3  17801.5±1099.2  2912.8±144.0    746.0±26.3    94.9±2.2
100      0           1791.4±199.4   4370.3±24.9     0.0±0.0   32114.7±171.9   4369.9±24.9    257.9±22.6    94.8±3.0
100      50          1759.3±160.8   4367.6±27.5     0.2±0.5   32112.0±172.1   4366.8±27.8    255.1±18.5    94.5±2.8
100      100         1727.0±125.7   4367.4±28.4     1.3±1.3   32111.2±153.0   4365.8±28.0    254.8±15.9    94.3±2.3

```

## Section 4, "The claim, tested"

tests/busy (test_sglp.sh (i), the same REPL lines): loads, and prints after twice, before never (sglp-claim-busy.out).

tests/law, law(10000, S) (test_sglp.sh (ii)): S = stats(10000, 0.21864305247736834, 0.04736824334608777); mean 0.218643 against 1/4.5 = 0.222222, 1.61% under; variance 0.047368 against 1/4.5^2 = 0.049383, 4.08% under (October: 0.2206 and 0.04934, 0.7% and 0.08% under).  Within test_sglp.sh's 10% (sglp-claim-law.out).

## Section 4, "The cost" (social graph 60/70, seed 20260927; /usr/bin/time -l)

| agents, horizon | answers | wall-clock (run.sh, whole s) | script real s | peak RSS MiB | ms an agent | ms an answer | October |
|---|---|---|---|---|---|---|---|
| 100, 1 year | 14788 | 32 | 35.82 | 85.4 | 320 | 2.16 | 14459 answers, 25 s, 81.1 MiB, 1.73 ms |
| 100, 30 days | 1112 | 3 | 5.11 | 94.2 | 30 | 2.70 | 1265, 3 s, 80.4 MiB, 2.37 ms |
| 200, 30 days | 2312 | 4 | 8.60 | 85.1 | 20 | 1.73 | 2445, 6 s, 81.0 MiB, 2.45 ms |
| 500, 30 days | 5926 | 12 | 21.61 | 89.4 | 24 | 2.02 | 5998, 14 s, 86.0 MiB, 2.33 ms |

The 30-day wall-clocks are run.sh's whole seconds (3, 4 and 12 s), so their ms figures are coarse; the draw of 100 and the load cost about 4 s besides (script real 35.82 s against run.sh's 32 s; October: about 2 s).

## The social graph at 1,000 agents, seed 20260927, 60/70 (Section 5; Section 4, "The cost")

Run until 4.4186 years (October's run of 5 years was stopped there), alone, by k1000.sh, a watcher every 10 s (UTC, seconds since run.sh started, the log's last time, the REPL's RSS KB).  Udi stopped it at 19:11:29 UTC, at simulated year 1.04 (32,892,059 s), allowing no simulation longer than ten minutes; a restart at 19:12 UTC was stopped within a minute and its files removed.  So year 1 is all there is; years 2 to 4.4 were not run.  Its files are sglp-k5y-20260927-killed1.{log,out,draw,watch.txt}; no .graph, run.sh having been killed.

Draw: homophile 576 indifferent 424 wary 683 sociable 317 (October: homophile 577 indifferent 423 wary 718 sociable 282).

Year 1 (the log's lines to 31,557,600 s, friendship.awk n=1000, sglp-k5y-20260927-year1.txt): answers 147983;menu_answers 74055;approval_answers 73928;yes 34981;edges 28939;components 1;isolated 0;largest 1000;unread 0; one component from month 4; month 1: edges 1768, components 54, isolated 46, largest 934, within 1176, across 592; month 12: within 19719, across 9220.  October: 147,952 answers, 74,046 menus, 73,906 approvals, 33,201 yes, 27,501 edges, one component from month 5.

Year 1's cost: crossed between 4,943 and 4,953 s after run.sh started (the draw session included), about 33 ms an answer; largest sampled RSS in the year 995,424 KB = 972 MiB.  October: 4,545 s, 31 ms an answer, 927 MiB.

## Wall-clock per family (run.sh's wall-clock line, whole seconds, the run's REPL; load 6 to 10 from other sessions)

The social graph's year, 2 runs: mean 38.0 s, max 44 s.

The sweep, 180 runs: mean 62.3 s, max 241 s (script real mean 66.7 s, max 245.8 s).

Coins' year, 2 runs: mean 13.5 s, max 14 s.

The coins grid, 180 runs: mean 14.3 s, max 28 s.

The cost's 30-day runs, 3: mean 6.3 s, max 12 s; the claim's two REPL tests, a few seconds each.

The 1,000-agent run: year 1 at about 4,950 s; stopped at 5,534 s.
