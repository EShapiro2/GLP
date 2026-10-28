# GFF, GFF-PS and GFF-FR in time, q = 1, fault rate 0.0001, mean down stretch 1000 steps

Kleinberg's 100 x 100 grid, 1 long-range friendship(s) per person; seeds 1, 2, 3, 4, 5, pooled; each author posts every 5000 steps; produced by programs/gff/paths/timed.py (see README.md).
Fraction of agent-steps down, per seed: 0.0903, 0.0898, 0.0901, 0.0901, 0.0901.

Per item: its followers are the decided followers following the author at posting; share is of them reached by the end of the run; hands are the signatures on the first copy received; steps the steps it took; messages are the item's messages sent, over the followers reached (items reaching none left out); waiting is the share of decided followers wanting to follow at posting who do not follow.

## GFF

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 917551 | 0.9983 | 0.9993 | 1.82 | 2 | 2 | 173.80 | 2 | 4843 | 5.97 | 0 |
| 10-99 | 56094 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 145.49 | 2 | 6556 | 5.67 | 0 |
| 100-999 | 5675 | 0.9981 | 1.0000 | 1.85 | 2 | 5 | 162.94 | 2 | 6200 | 4.13 | 0 |
| 1000-9999 | 754 | 0.5529 | 0.9783 | 46.24 | 45 | 140 | 3904.90 | 3161 | 26578 | 1.09 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 980074 | 0.8226 | 0.9784 | 45.97 | 45 | 140 | 3881.85 | 3132 | 26578 | 1.12 | 0 |

Update messages 1808819, messages to follow through or cease to follow through a friend 102934; run time 25 s over 5 seeds.

## GFF-PS

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 917551 | 0.9983 | 0.9993 | 1.82 | 2 | 2 | 173.80 | 2 | 4843 | 5.97 | 0 |
| 10-99 | 56094 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 145.49 | 2 | 6556 | 5.67 | 0 |
| 100-999 | 5675 | 0.9981 | 1.0000 | 1.85 | 2 | 5 | 162.94 | 2 | 6200 | 4.13 | 1 |
| 1000-9999 | 754 | 0.5529 | 0.9849 | 43.89 | 43 | 138 | 3717.42 | 2994 | 26578 | 1.09 | 5452 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 980074 | 0.8226 | 0.9850 | 43.63 | 43 | 138 | 3695.67 | 2975 | 26578 | 1.11 | 5453 |

Update messages 1808819, messages to follow through or cease to follow through a friend 113840; run time 28 s over 5 seeds.

## GFF-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 917551 | 0.9983 | 0.9993 | 1.82 | 2 | 2 | 158.49 | 2 | 4843 | 6.27 | 0 |
| 10-99 | 56094 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 137.40 | 2 | 6556 | 5.90 | 0 |
| 100-999 | 5675 | 0.9981 | 1.0000 | 1.85 | 2 | 5 | 158.08 | 2 | 6200 | 4.33 | 0 |
| 1000-9999 | 754 | 0.5529 | 1.0000 | 18.50 | 15 | 136 | 227.39 | 16 | 13943 | 2.19 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 980074 | 0.8226 | 1.0000 | 18.40 | 15 | 136 | 226.93 | 16 | 13943 | 2.21 | 0 |

Update messages 1808819, messages to follow through or cease to follow through a friend 199434; run time 29 s over 5 seeds.

## GFF-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 917551 | 0.9983 | 0.9993 | 1.82 | 2 | 2 | 158.49 | 2 | 4843 | 6.27 | 0 |
| 10-99 | 56094 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 137.40 | 2 | 6556 | 5.90 | 0 |
| 100-999 | 5675 | 0.9981 | 1.0000 | 1.85 | 2 | 5 | 158.08 | 2 | 6200 | 4.33 | 0 |
| 1000-9999 | 754 | 0.5529 | 1.0000 | 17.54 | 14 | 136 | 224.54 | 15 | 13943 | 2.19 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 980074 | 0.8226 | 1.0000 | 17.45 | 14 | 136 | 224.10 | 15 | 13943 | 2.21 | 0 |

Update messages 1808819, messages to follow through or cease to follow through a friend 199434; run time 30 s over 5 seeds.

## GFF-PS-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 917551 | 0.9983 | 0.9993 | 1.82 | 2 | 2 | 158.49 | 2 | 4843 | 6.27 | 0 |
| 10-99 | 56094 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 137.40 | 2 | 6556 | 5.90 | 0 |
| 100-999 | 5675 | 0.9981 | 1.0000 | 1.85 | 2 | 5 | 158.08 | 2 | 6200 | 4.33 | 2 |
| 1000-9999 | 754 | 0.5529 | 1.0000 | 16.92 | 13 | 136 | 225.20 | 14 | 13943 | 2.20 | 49906 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 980074 | 0.8226 | 1.0000 | 16.83 | 13 | 136 | 224.75 | 14 | 13943 | 2.22 | 49908 |

Update messages 1808819, messages to follow through or cease to follow through a friend 298276; run time 30 s over 5 seeds.

## GFF-PS-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 917551 | 0.9983 | 0.9993 | 1.82 | 2 | 2 | 158.49 | 2 | 4843 | 6.27 | 0 |
| 10-99 | 56094 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 137.40 | 2 | 6556 | 5.90 | 0 |
| 100-999 | 5675 | 0.9981 | 1.0000 | 1.85 | 2 | 5 | 158.08 | 2 | 6200 | 4.33 | 2 |
| 1000-9999 | 754 | 0.5529 | 1.0000 | 16.31 | 13 | 136 | 221.31 | 14 | 13943 | 2.21 | 47318 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 980074 | 0.8226 | 1.0000 | 16.22 | 13 | 136 | 220.90 | 14 | 13943 | 2.23 | 47320 |

Update messages 1808819, messages to follow through or cease to follow through a friend 293112; run time 190 s over 5 seeds.
