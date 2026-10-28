# GFF, GFF-PS and GFF-FR in time, q = 4, fault rate 0

Kleinberg's 100 x 100 grid, 4 long-range friendship(s) per person; seeds 1, 2, 3, 4, 5, pooled; each author posts every 5000 steps; produced by programs/gff/paths/timed.py (see README.md).

Per item: its followers are the decided followers following the author at posting; share is of them reached by the end of the run; hands are the signatures on the first copy received; steps the steps it took; messages are the item's messages sent, over the followers reached (items reaching none left out); waiting is the share of decided followers wanting to follow at posting who do not follow.

## GFF

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1190448 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 1.88 | 2 | 3 | 11.01 | 0 |
| 10-99 | 72274 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 1.91 | 2 | 4 | 9.49 | 0 |
| 100-999 | 7168 | 0.9894 | 1.0000 | 2.85 | 2 | 16 | 2.85 | 2 | 16 | 3.57 | 0 |
| 1000-9999 | 765 | 0.4285 | 1.0000 | 31.40 | 30 | 103 | 31.40 | 30 | 103 | 1.01 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1270655 | 0.8355 | 1.0000 | 30.33 | 29 | 103 | 30.33 | 29 | 103 | 1.25 | 0 |

Hands against the tree of shortest paths at posting, over the whole friendship graph and over the followers' subgraph (ratio of hands to distance):

| followers | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---|---:|---:|---:|---:|---:|---:|
| 1-9 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 10-99 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 100-999 | 1.094 | 1.000 | 4.667 | 1.004 | 1.000 | 2.500 |
| 1000-9999 | 6.530 | 6.000 | 42.000 | 5.186 | 4.500 | 42.000 |
| 10000 | - | - | - | - | - | - |

The same by the fifth of the arrivals in which the item was posted (the fifth includes the tail after the last arrival):

| fifth | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 2 | 4.709 | 4.429 | 25.500 | 2.284 | 1.891 | 25.500 |
| 3 | 5.820 | 5.571 | 38.500 | 4.268 | 3.900 | 38.500 |
| 4 | 6.302 | 6.000 | 38.500 | 5.015 | 4.600 | 38.500 |
| 5 | 6.518 | 6.000 | 42.000 | 5.320 | 4.500 | 42.000 |

Update messages 5236474, messages to follow through or cease to follow through a friend 79328; run time 35 s over 5 seeds.

## GFF-PS

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1190448 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 1.88 | 2 | 3 | 11.01 | 0 |
| 10-99 | 72274 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 1.91 | 2 | 4 | 9.49 | 0 |
| 100-999 | 7168 | 0.9894 | 1.0000 | 2.85 | 2 | 16 | 2.85 | 2 | 16 | 3.57 | 8 |
| 1000-9999 | 765 | 0.4285 | 1.0000 | 28.87 | 28 | 101 | 28.87 | 28 | 101 | 1.01 | 3717 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1270655 | 0.8355 | 1.0000 | 27.90 | 27 | 101 | 27.90 | 27 | 101 | 1.25 | 3725 |

Hands against the tree of shortest paths at posting, over the whole friendship graph and over the followers' subgraph (ratio of hands to distance):

| followers | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---|---:|---:|---:|---:|---:|---:|
| 1-9 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 10-99 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 100-999 | 1.092 | 1.000 | 4.667 | 1.003 | 1.000 | 2.500 |
| 1000-9999 | 5.987 | 5.600 | 37.000 | 4.701 | 4.167 | 37.000 |
| 10000 | - | - | - | - | - | - |

The same by the fifth of the arrivals in which the item was posted (the fifth includes the tail after the last arrival):

| fifth | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 2 | 4.704 | 4.429 | 25.500 | 2.281 | 1.889 | 25.500 |
| 3 | 5.632 | 5.333 | 36.500 | 4.132 | 3.778 | 36.500 |
| 4 | 5.836 | 5.500 | 33.667 | 4.606 | 4.250 | 30.500 |
| 5 | 5.873 | 5.500 | 37.000 | 4.733 | 4.143 | 37.000 |

Update messages 5236474, messages to follow through or cease to follow through a friend 86778; run time 34 s over 5 seeds.

## GFF-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1190448 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 1.88 | 2 | 3 | 11.15 | 0 |
| 10-99 | 72274 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 1.91 | 2 | 4 | 9.68 | 0 |
| 100-999 | 7168 | 0.9894 | 1.0000 | 2.84 | 2 | 16 | 2.84 | 2 | 16 | 4.20 | 0 |
| 1000-9999 | 765 | 0.4285 | 1.0000 | 10.52 | 10 | 73 | 10.52 | 10 | 73 | 2.66 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1270655 | 0.8355 | 1.0000 | 10.22 | 10 | 73 | 10.22 | 10 | 73 | 2.86 | 0 |

Update messages 5236474, messages to follow through or cease to follow through a friend 149932; run time 32 s over 5 seeds.

## GFF-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1190448 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 1.88 | 2 | 3 | 11.15 | 0 |
| 10-99 | 72274 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 1.91 | 2 | 4 | 9.68 | 0 |
| 100-999 | 7168 | 0.9894 | 1.0000 | 2.84 | 2 | 16 | 2.84 | 2 | 16 | 4.20 | 0 |
| 1000-9999 | 765 | 0.4285 | 1.0000 | 10.00 | 9 | 73 | 10.00 | 9 | 73 | 2.67 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1270655 | 0.8355 | 1.0000 | 9.71 | 9 | 73 | 9.71 | 9 | 73 | 2.86 | 0 |

Update messages 5236474, messages to follow through or cease to follow through a friend 149932; run time 33 s over 5 seeds.
