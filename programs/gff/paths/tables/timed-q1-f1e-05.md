# GFF, GFF-PS and GFF-FR in time, q = 1, fault rate 1e-05, mean down stretch 1000 steps

Kleinberg's 100 x 100 grid, 1 long-range friendship(s) per person; seeds 1, 2, 3, 4, 5, pooled; each author posts every 5000 steps; produced by programs/gff/paths/timed.py (see README.md).
Fraction of agent-steps down, per seed: 0.0097, 0.0097, 0.0098, 0.0097, 0.0098.

Per item: its followers are the decided followers following the author at posting; share is of them reached by the end of the run; hands are the signatures on the first copy received; steps the steps it took; messages are the item's messages sent, over the followers reached (items reaching none left out); waiting is the share of decided followers wanting to follow at posting who do not follow.

## GFF

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 998319 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 18.36 | 2 | 3277 | 5.94 | 0 |
| 10-99 | 61120 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 23.42 | 2 | 4922 | 5.67 | 0 |
| 100-999 | 6200 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 9.04 | 2 | 2955 | 4.09 | 0 |
| 1000-9999 | 830 | 0.5536 | 1.0000 | 49.07 | 46 | 150 | 424.84 | 58 | 11686 | 1.01 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1066469 | 0.8239 | 1.0000 | 48.78 | 46 | 150 | 422.32 | 57 | 11686 | 1.04 | 0 |

Update messages 1805479, messages to follow through or cease to follow through a friend 102934; run time 24 s over 5 seeds.

## GFF-PS

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 998319 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 18.36 | 2 | 3277 | 5.94 | 0 |
| 10-99 | 61120 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 23.42 | 2 | 4922 | 5.67 | 0 |
| 100-999 | 6200 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 9.04 | 2 | 2955 | 4.09 | 1 |
| 1000-9999 | 830 | 0.5536 | 1.0000 | 44.91 | 42 | 147 | 375.85 | 50 | 11686 | 1.01 | 5562 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1066469 | 0.8239 | 1.0000 | 44.65 | 41 | 147 | 373.64 | 50 | 11686 | 1.04 | 5563 |

Update messages 1805479, messages to follow through or cease to follow through a friend 114060; run time 29 s over 5 seeds.

## GFF-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 998319 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 16.75 | 2 | 3277 | 6.25 | 0 |
| 10-99 | 61120 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 22.41 | 2 | 4922 | 5.90 | 0 |
| 100-999 | 6200 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 8.28 | 2 | 2955 | 4.28 | 0 |
| 1000-9999 | 830 | 0.5536 | 1.0000 | 15.39 | 14 | 92 | 34.60 | 14 | 9023 | 2.12 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1066469 | 0.8239 | 1.0000 | 15.31 | 14 | 92 | 34.48 | 14 | 9023 | 2.14 | 0 |

Update messages 1805479, messages to follow through or cease to follow through a friend 199434; run time 28 s over 5 seeds.

## GFF-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 998319 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 16.75 | 2 | 3277 | 6.25 | 0 |
| 10-99 | 61120 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 22.41 | 2 | 4922 | 5.90 | 0 |
| 100-999 | 6200 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 8.28 | 2 | 2955 | 4.28 | 0 |
| 1000-9999 | 830 | 0.5536 | 1.0000 | 14.78 | 13 | 92 | 33.99 | 13 | 9023 | 2.13 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1066469 | 0.8239 | 1.0000 | 14.70 | 13 | 92 | 33.88 | 13 | 9023 | 2.15 | 0 |

Update messages 1805479, messages to follow through or cease to follow through a friend 199434; run time 29 s over 5 seeds.

## GFF-PS-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 998319 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 16.75 | 2 | 3277 | 6.25 | 0 |
| 10-99 | 61120 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 22.41 | 2 | 4922 | 5.90 | 0 |
| 100-999 | 6200 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 8.28 | 2 | 2955 | 4.28 | 2 |
| 1000-9999 | 830 | 0.5536 | 1.0000 | 14.11 | 12 | 92 | 33.22 | 12 | 9023 | 2.14 | 33673 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1066469 | 0.8239 | 1.0000 | 14.03 | 12 | 92 | 33.12 | 12 | 9023 | 2.16 | 33675 |

Update messages 1805479, messages to follow through or cease to follow through a friend 265951; run time 28 s over 5 seeds.

## GFF-PS-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 998319 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 16.75 | 2 | 3277 | 6.25 | 0 |
| 10-99 | 61120 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 22.41 | 2 | 4922 | 5.90 | 0 |
| 100-999 | 6200 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 8.28 | 2 | 2955 | 4.28 | 2 |
| 1000-9999 | 830 | 0.5536 | 1.0000 | 13.75 | 12 | 92 | 32.96 | 12 | 9023 | 2.15 | 32301 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1066469 | 0.8239 | 1.0000 | 13.67 | 12 | 92 | 32.85 | 12 | 9023 | 2.17 | 32303 |

Update messages 1805479, messages to follow through or cease to follow through a friend 263197; run time 148 s over 5 seeds.
