# GFF, GFF-PS and GFF-FR in time, q = 2, fault rate 0.0001, mean down stretch 1000 steps

Kleinberg's 100 x 100 grid, 2 long-range friendship(s) per person; seeds 1, 2, 3, 4, 5, pooled; each author posts every 5000 steps; produced by programs/gff/paths/timed.py (see README.md).
Fraction of agent-steps down, per seed: 0.0902, 0.0905, 0.0905, 0.0895, 0.0902.

Per item: its followers are the decided followers following the author at posting; share is of them reached by the end of the run; hands are the signatures on the first copy received; steps the steps it took; messages are the item's messages sent, over the followers reached (items reaching none left out); waiting is the share of decided followers wanting to follow at posting who do not follow.

## GFF

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 839359 | 0.9964 | 1.0000 | 1.84 | 2 | 3 | 186.62 | 2 | 8631 | 7.68 | 0 |
| 10-99 | 50502 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 169.42 | 2 | 6736 | 7.05 | 0 |
| 100-999 | 4827 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 221.58 | 2 | 7338 | 4.15 | 0 |
| 1000-9999 | 330 | 0.6707 | 0.9784 | 36.20 | 35 | 93 | 3069.18 | 2464 | 22461 | 1.08 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 895018 | 0.9419 | 0.9796 | 34.31 | 33 | 93 | 2910.94 | 2292 | 22461 | 1.37 | 0 |

Update messages 2427019, messages to follow through or cease to follow through a friend 30680; run time 20 s over 5 seeds.

## GFF-PS

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 839359 | 0.9964 | 1.0000 | 1.84 | 2 | 3 | 186.62 | 2 | 8631 | 7.68 | 0 |
| 10-99 | 50502 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 169.42 | 2 | 6736 | 7.05 | 0 |
| 100-999 | 4827 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 221.58 | 2 | 7338 | 4.15 | 1 |
| 1000-9999 | 330 | 0.6707 | 0.9821 | 34.64 | 33 | 93 | 2946.87 | 2390 | 22461 | 1.08 | 1334 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 895018 | 0.9419 | 0.9830 | 32.85 | 32 | 93 | 2795.90 | 2190 | 22461 | 1.37 | 1335 |

Update messages 2427019, messages to follow through or cease to follow through a friend 33350; run time 21 s over 5 seeds.

## GFF-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 839359 | 0.9964 | 1.0000 | 1.84 | 2 | 3 | 181.88 | 2 | 8631 | 7.81 | 0 |
| 10-99 | 50502 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 164.08 | 2 | 6736 | 7.20 | 0 |
| 100-999 | 4827 | 0.9959 | 1.0000 | 2.07 | 2 | 7 | 216.91 | 2 | 7338 | 4.56 | 0 |
| 1000-9999 | 330 | 0.6707 | 0.9999 | 18.17 | 16 | 118 | 220.90 | 17 | 9676 | 2.66 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 895018 | 0.9419 | 0.9999 | 17.29 | 15 | 118 | 219.11 | 17 | 9676 | 2.87 | 0 |

Update messages 2427019, messages to follow through or cease to follow through a friend 57135; run time 22 s over 5 seeds.

## GFF-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 839359 | 0.9964 | 1.0000 | 1.84 | 2 | 3 | 181.88 | 2 | 8631 | 7.81 | 0 |
| 10-99 | 50502 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 164.08 | 2 | 6736 | 7.20 | 0 |
| 100-999 | 4827 | 0.9959 | 1.0000 | 2.07 | 2 | 7 | 216.91 | 2 | 7338 | 4.56 | 0 |
| 1000-9999 | 330 | 0.6707 | 0.9999 | 17.52 | 15 | 118 | 221.45 | 16 | 9676 | 2.64 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 895018 | 0.9419 | 0.9999 | 16.68 | 15 | 118 | 219.64 | 16 | 9676 | 2.85 | 0 |

Update messages 2427019, messages to follow through or cease to follow through a friend 57135; run time 21 s over 5 seeds.
