# GFF, GFF-PS and GFF-FR in time, q = 4, fault rate 1e-05, mean down stretch 1000 steps

Kleinberg's 100 x 100 grid, 4 long-range friendship(s) per person; seeds 1, 2, 3, 4, 5, pooled; each author posts every 5000 steps; produced by programs/gff/paths/timed.py (see README.md).
Fraction of agent-steps down, per seed: 0.0096, 0.0098, 0.0098, 0.0100, 0.0098.

Per item: its followers are the decided followers following the author at posting; share is of them reached by the end of the run; hands are the signatures on the first copy received; steps the steps it took; messages are the item's messages sent, over the followers reached (items reaching none left out); waiting is the share of decided followers wanting to follow at posting who do not follow.

## GFF

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1178852 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 21.67 | 2 | 4731 | 11.01 | 0 |
| 10-99 | 71550 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 26.01 | 2 | 5369 | 9.49 | 0 |
| 100-999 | 7100 | 0.9894 | 1.0000 | 2.85 | 2 | 16 | 26.42 | 2 | 4592 | 3.57 | 0 |
| 1000-9999 | 753 | 0.4327 | 1.0000 | 31.41 | 30 | 103 | 333.74 | 34 | 8969 | 1.01 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1258255 | 0.8378 | 1.0000 | 30.32 | 29 | 103 | 322.18 | 33 | 8969 | 1.26 | 0 |

Update messages 5237167, messages to follow through or cease to follow through a friend 79328; run time 38 s over 5 seeds.

## GFF-PS

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1178852 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 21.67 | 2 | 4731 | 11.01 | 0 |
| 10-99 | 71550 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 26.01 | 2 | 5369 | 9.49 | 0 |
| 100-999 | 7100 | 0.9894 | 1.0000 | 2.85 | 2 | 16 | 26.42 | 2 | 4592 | 3.57 | 8 |
| 1000-9999 | 753 | 0.4327 | 1.0000 | 28.92 | 28 | 101 | 302.11 | 31 | 8969 | 1.01 | 3718 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1258255 | 0.8378 | 1.0000 | 27.92 | 27 | 101 | 291.74 | 30 | 8969 | 1.26 | 3726 |

Update messages 5237167, messages to follow through or cease to follow through a friend 86780; run time 43 s over 5 seeds.

## GFF-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1178852 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 20.76 | 2 | 4731 | 11.15 | 0 |
| 10-99 | 71550 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 25.05 | 2 | 5369 | 9.66 | 0 |
| 100-999 | 7100 | 0.9894 | 1.0000 | 2.85 | 2 | 16 | 25.46 | 2 | 4592 | 3.95 | 0 |
| 1000-9999 | 753 | 0.4327 | 1.0000 | 10.74 | 10 | 81 | 29.90 | 10 | 8969 | 2.16 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1258255 | 0.8378 | 1.0000 | 10.42 | 10 | 81 | 29.68 | 10 | 8969 | 2.37 | 0 |

Update messages 5237167, messages to follow through or cease to follow through a friend 149932; run time 42 s over 5 seeds.

## GFF-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1178852 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 20.76 | 2 | 4731 | 11.15 | 0 |
| 10-99 | 71550 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 25.05 | 2 | 5369 | 9.66 | 0 |
| 100-999 | 7100 | 0.9894 | 1.0000 | 2.85 | 2 | 16 | 25.46 | 2 | 4592 | 3.95 | 0 |
| 1000-9999 | 753 | 0.4327 | 1.0000 | 10.21 | 9 | 81 | 29.29 | 9 | 8969 | 2.18 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1258255 | 0.8378 | 1.0000 | 9.91 | 9 | 81 | 29.08 | 9 | 8969 | 2.39 | 0 |

Update messages 5237167, messages to follow through or cease to follow through a friend 149932; run time 44 s over 5 seeds.

## GFF-PS-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1178852 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 20.76 | 2 | 4731 | 11.15 | 0 |
| 10-99 | 71550 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 25.05 | 2 | 5369 | 9.66 | 0 |
| 100-999 | 7100 | 0.9894 | 1.0000 | 2.85 | 2 | 16 | 25.46 | 2 | 4592 | 3.94 | 37 |
| 1000-9999 | 753 | 0.4327 | 1.0000 | 10.00 | 9 | 86 | 28.81 | 9 | 8969 | 2.17 | 17949 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1258255 | 0.8378 | 1.0000 | 9.71 | 9 | 86 | 28.62 | 9 | 8969 | 2.39 | 17986 |

Update messages 5237167, messages to follow through or cease to follow through a friend 185263; run time 43 s over 5 seeds.

## GFF-PS-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1178852 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 20.76 | 2 | 4731 | 11.15 | 0 |
| 10-99 | 71550 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 25.05 | 2 | 5369 | 9.66 | 0 |
| 100-999 | 7100 | 0.9894 | 1.0000 | 2.85 | 2 | 16 | 25.46 | 2 | 4592 | 3.94 | 36 |
| 1000-9999 | 753 | 0.4327 | 1.0000 | 9.71 | 8 | 86 | 28.54 | 9 | 8969 | 2.19 | 16847 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1258255 | 0.8378 | 1.0000 | 9.43 | 8 | 86 | 28.36 | 8 | 8969 | 2.40 | 16883 |

Update messages 5237167, messages to follow through or cease to follow through a friend 183065; run time 129 s over 5 seeds.
