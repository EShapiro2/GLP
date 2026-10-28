# GFF, GFF-PS and GFF-FR in time, q = 2, fault rate 1e-05, mean down stretch 1000 steps

Kleinberg's 100 x 100 grid, 2 long-range friendship(s) per person; seeds 1, 2, 3, 4, 5, pooled; each author posts every 5000 steps; produced by programs/gff/paths/timed.py (see README.md).
Fraction of agent-steps down, per seed: 0.0098, 0.0098, 0.0099, 0.0097, 0.0099.

Per item: its followers are the decided followers following the author at posting; share is of them reached by the end of the run; hands are the signatures on the first copy received; steps the steps it took; messages are the item's messages sent, over the followers reached (items reaching none left out); waiting is the share of decided followers wanting to follow at posting who do not follow.

## GFF

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 913875 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 24.16 | 2 | 5365 | 7.71 | 0 |
| 10-99 | 55067 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 20.41 | 2 | 5078 | 7.07 | 0 |
| 100-999 | 5234 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 13.58 | 2 | 4266 | 4.14 | 0 |
| 1000-9999 | 375 | 0.6703 | 1.0000 | 37.50 | 35 | 120 | 447.09 | 44 | 8900 | 1.01 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 974551 | 0.9402 | 1.0000 | 35.64 | 33 | 120 | 424.76 | 41 | 8900 | 1.29 | 0 |

Update messages 2423542, messages to follow through or cease to follow through a friend 30680; run time 19 s over 5 seeds.

## GFF-PS

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 913875 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 24.16 | 2 | 5365 | 7.71 | 0 |
| 10-99 | 55067 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 20.41 | 2 | 5078 | 7.07 | 0 |
| 100-999 | 5234 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 13.58 | 2 | 4266 | 4.14 | 1 |
| 1000-9999 | 375 | 0.6703 | 1.0000 | 34.77 | 32 | 119 | 365.59 | 39 | 8900 | 1.01 | 1045 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 974551 | 0.9402 | 1.0000 | 33.06 | 30 | 119 | 347.51 | 36 | 8900 | 1.29 | 1046 |

Update messages 2423542, messages to follow through or cease to follow through a friend 32772; run time 20 s over 5 seeds.

## GFF-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 913875 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 22.94 | 2 | 5365 | 7.83 | 0 |
| 10-99 | 55067 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 19.20 | 2 | 5078 | 7.23 | 0 |
| 100-999 | 5234 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 13.57 | 2 | 4266 | 4.55 | 0 |
| 1000-9999 | 375 | 0.6703 | 1.0000 | 14.75 | 14 | 62 | 33.15 | 14 | 6163 | 2.57 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 974551 | 0.9402 | 1.0000 | 14.08 | 13 | 62 | 32.40 | 13 | 6163 | 2.78 | 0 |

Update messages 2423542, messages to follow through or cease to follow through a friend 57135; run time 20 s over 5 seeds.

## GFF-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 913875 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 22.94 | 2 | 5365 | 7.83 | 0 |
| 10-99 | 55067 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 19.20 | 2 | 5078 | 7.23 | 0 |
| 100-999 | 5234 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 13.57 | 2 | 4266 | 4.55 | 0 |
| 1000-9999 | 375 | 0.6703 | 1.0000 | 14.38 | 13 | 62 | 32.95 | 13 | 6163 | 2.57 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 974551 | 0.9402 | 1.0000 | 13.73 | 13 | 62 | 32.21 | 13 | 6163 | 2.78 | 0 |

Update messages 2423542, messages to follow through or cease to follow through a friend 57135; run time 20 s over 5 seeds.
