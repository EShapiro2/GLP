# GFF, GFF-PS and GFF-FR in time, q = 2, fault rate 0

Kleinberg's 100 x 100 grid, 2 long-range friendship(s) per person; seeds 1, 2, 3, 4, 5, pooled; each author posts every 5000 steps; produced by programs/gff/paths/timed.py (see README.md).

Per item: its followers are the decided followers following the author at posting; share is of them reached by the end of the run; hands are the signatures on the first copy received; steps the steps it took; messages are the item's messages sent, over the followers reached (items reaching none left out); waiting is the share of decided followers wanting to follow at posting who do not follow.

## GFF

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 922965 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 1.85 | 2 | 3 | 7.70 | 0 |
| 10-99 | 55593 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 1.86 | 2 | 4 | 7.08 | 0 |
| 100-999 | 5304 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 2.08 | 2 | 7 | 4.14 | 0 |
| 1000-9999 | 379 | 0.6634 | 1.0000 | 37.77 | 35 | 120 | 37.77 | 35 | 120 | 1.01 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 984241 | 0.9388 | 1.0000 | 35.94 | 33 | 120 | 35.94 | 33 | 120 | 1.28 | 0 |

Hands against the tree of shortest paths at posting, over the whole friendship graph and over the followers' subgraph (ratio of hands to distance):

| followers | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---|---:|---:|---:|---:|---:|---:|
| 1-9 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 10-99 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 100-999 | 1.007 | 1.000 | 2.000 | 1.002 | 1.000 | 1.500 |
| 1000-9999 | 6.556 | 5.857 | 47.000 | 4.082 | 3.333 | 47.000 |
| 10000 | - | - | - | - | - | - |

The same by the fifth of the arrivals in which the item was posted (the fifth includes the tail after the last arrival):

| fifth | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 2 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 3 | 2.803 | 1.429 | 16.200 | 1.358 | 1.000 | 5.250 |
| 4 | 5.629 | 5.125 | 47.000 | 2.920 | 2.364 | 47.000 |
| 5 | 6.555 | 5.857 | 47.000 | 4.302 | 3.556 | 47.000 |

Update messages 2423129, messages to follow through or cease to follow through a friend 30680; run time 21 s over 5 seeds.

## GFF-PS

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 922965 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 1.85 | 2 | 3 | 7.70 | 0 |
| 10-99 | 55593 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 1.86 | 2 | 4 | 7.08 | 0 |
| 100-999 | 5304 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 2.08 | 2 | 7 | 4.14 | 1 |
| 1000-9999 | 379 | 0.6634 | 1.0000 | 35.02 | 32 | 119 | 35.02 | 32 | 119 | 1.01 | 985 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 984241 | 0.9388 | 1.0000 | 33.34 | 31 | 119 | 33.34 | 31 | 119 | 1.28 | 986 |

Hands against the tree of shortest paths at posting, over the whole friendship graph and over the followers' subgraph (ratio of hands to distance):

| followers | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---|---:|---:|---:|---:|---:|---:|
| 1-9 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 10-99 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 100-999 | 1.007 | 1.000 | 2.000 | 1.002 | 1.000 | 1.500 |
| 1000-9999 | 6.062 | 5.429 | 44.000 | 3.746 | 3.111 | 44.000 |
| 10000 | - | - | - | - | - | - |

The same by the fifth of the arrivals in which the item was posted (the fifth includes the tail after the last arrival):

| fifth | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 2 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 3 | 2.803 | 1.429 | 16.200 | 1.358 | 1.000 | 5.250 |
| 4 | 5.427 | 4.944 | 44.000 | 2.807 | 2.296 | 44.000 |
| 5 | 5.989 | 5.333 | 44.000 | 3.910 | 3.267 | 44.000 |

Update messages 2423129, messages to follow through or cease to follow through a friend 32652; run time 20 s over 5 seeds.

## GFF-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 922965 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 1.85 | 2 | 3 | 7.82 | 0 |
| 10-99 | 55593 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 1.86 | 2 | 4 | 7.22 | 0 |
| 100-999 | 5304 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 2.08 | 2 | 7 | 4.42 | 0 |
| 1000-9999 | 379 | 0.6634 | 1.0000 | 14.48 | 13 | 81 | 14.48 | 13 | 81 | 1.99 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 984241 | 0.9388 | 1.0000 | 13.84 | 13 | 81 | 13.84 | 13 | 81 | 2.22 | 0 |

Update messages 2423129, messages to follow through or cease to follow through a friend 57135; run time 21 s over 5 seeds.

## GFF-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 922965 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 1.85 | 2 | 3 | 7.82 | 0 |
| 10-99 | 55593 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 1.86 | 2 | 4 | 7.22 | 0 |
| 100-999 | 5304 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 2.08 | 2 | 7 | 4.42 | 0 |
| 1000-9999 | 379 | 0.6634 | 1.0000 | 14.09 | 13 | 81 | 14.09 | 13 | 81 | 2.00 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 984241 | 0.9388 | 1.0000 | 13.47 | 13 | 81 | 13.47 | 13 | 81 | 2.23 | 0 |

Update messages 2423129, messages to follow through or cease to follow through a friend 57135; run time 21 s over 5 seeds.

## GFF-PS-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 922965 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 1.85 | 2 | 3 | 7.82 | 0 |
| 10-99 | 55593 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 1.86 | 2 | 4 | 7.22 | 0 |
| 100-999 | 5304 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 2.08 | 2 | 7 | 4.41 | 9 |
| 1000-9999 | 379 | 0.6634 | 1.0000 | 13.79 | 13 | 81 | 13.79 | 13 | 81 | 1.98 | 5486 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 984241 | 0.9388 | 1.0000 | 13.19 | 12 | 81 | 13.19 | 12 | 81 | 2.22 | 5495 |

Update messages 2423129, messages to follow through or cease to follow through a friend 67705; run time 21 s over 5 seeds.

## GFF-PS-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 922965 | 0.9964 | 1.0000 | 1.85 | 2 | 3 | 1.85 | 2 | 3 | 7.82 | 0 |
| 10-99 | 55593 | 0.9963 | 1.0000 | 1.86 | 2 | 4 | 1.86 | 2 | 4 | 7.22 | 0 |
| 100-999 | 5304 | 0.9959 | 1.0000 | 2.08 | 2 | 7 | 2.08 | 2 | 7 | 4.41 | 9 |
| 1000-9999 | 379 | 0.6634 | 1.0000 | 13.62 | 12 | 81 | 13.62 | 12 | 81 | 1.99 | 5177 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 984241 | 0.9388 | 1.0000 | 13.02 | 12 | 81 | 13.02 | 12 | 81 | 2.22 | 5186 |

Update messages 2423129, messages to follow through or cease to follow through a friend 67097; run time 35 s over 5 seeds.
