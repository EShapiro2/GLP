# GFF, GFF-PS and GFF-FR in time, q = 1, fault rate 0

Kleinberg's 100 x 100 grid, 1 long-range friendship(s) per person; seeds 1, 2, 3, 4, 5, pooled; each author posts every 5000 steps; produced by programs/gff/paths/timed.py (see README.md).

Per item: its followers are the decided followers following the author at posting; share is of them reached by the end of the run; hands are the signatures on the first copy received; steps the steps it took; messages are the item's messages sent, over the followers reached (items reaching none left out); waiting is the share of decided followers wanting to follow at posting who do not follow.

## GFF

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1008151 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 1.82 | 2 | 2 | 5.95 | 0 |
| 10-99 | 61687 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 1.81 | 2 | 3 | 5.66 | 0 |
| 100-999 | 6251 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 1.85 | 2 | 5 | 4.09 | 0 |
| 1000-9999 | 837 | 0.5510 | 1.0000 | 49.19 | 47 | 150 | 49.19 | 47 | 150 | 1.00 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1076926 | 0.8230 | 1.0000 | 48.89 | 47 | 150 | 48.89 | 47 | 150 | 1.03 | 0 |

Hands against the tree of shortest paths at posting, over the whole friendship graph and over the followers' subgraph (ratio of hands to distance):

| followers | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---|---:|---:|---:|---:|---:|---:|
| 1-9 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 10-99 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 100-999 | 1.000 | 1.000 | 1.500 | 1.000 | 1.000 | 1.500 |
| 1000-9999 | 6.738 | 6.167 | 54.000 | 5.520 | 4.778 | 54.000 |
| 10000 | - | - | - | - | - | - |

The same by the fifth of the arrivals in which the item was posted (the fifth includes the tail after the last arrival):

| fifth | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 2 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 3 | 5.165 | 4.778 | 36.000 | 2.889 | 2.462 | 30.333 |
| 4 | 6.316 | 5.818 | 54.000 | 4.962 | 4.429 | 54.000 |
| 5 | 7.063 | 6.500 | 54.000 | 6.046 | 5.375 | 54.000 |

Update messages 1805088, messages to follow through or cease to follow through a friend 102934; run time 25 s over 5 seeds.

## GFF-PS

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1008151 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 1.82 | 2 | 2 | 5.95 | 0 |
| 10-99 | 61687 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 1.81 | 2 | 3 | 5.66 | 0 |
| 100-999 | 6251 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 1.85 | 2 | 5 | 4.09 | 1 |
| 1000-9999 | 837 | 0.5510 | 1.0000 | 44.53 | 43 | 147 | 44.53 | 43 | 147 | 1.00 | 5539 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1076926 | 0.8230 | 1.0000 | 44.27 | 42 | 147 | 44.27 | 42 | 147 | 1.03 | 5540 |

Hands against the tree of shortest paths at posting, over the whole friendship graph and over the followers' subgraph (ratio of hands to distance):

| followers | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---|---:|---:|---:|---:|---:|---:|
| 1-9 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 10-99 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 100-999 | 1.000 | 1.000 | 1.500 | 1.000 | 1.000 | 1.500 |
| 1000-9999 | 6.088 | 5.571 | 53.500 | 4.925 | 4.300 | 53.500 |
| 10000 | - | - | - | - | - | - |

The same by the fifth of the arrivals in which the item was posted (the fifth includes the tail after the last arrival):

| fifth | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 2 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 3 | 5.044 | 4.636 | 36.000 | 2.812 | 2.400 | 27.000 |
| 4 | 5.859 | 5.364 | 53.500 | 4.578 | 4.091 | 53.500 |
| 5 | 6.271 | 5.778 | 50.000 | 5.302 | 4.714 | 50.000 |

Update messages 1805088, messages to follow through or cease to follow through a friend 114014; run time 23 s over 5 seeds.

## GFF-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1008151 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 1.82 | 2 | 2 | 6.25 | 0 |
| 10-99 | 61687 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 1.81 | 2 | 3 | 5.90 | 0 |
| 100-999 | 6251 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 1.85 | 2 | 5 | 4.35 | 0 |
| 1000-9999 | 837 | 0.5510 | 1.0000 | 15.07 | 14 | 90 | 15.07 | 14 | 90 | 2.69 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1076926 | 0.8230 | 1.0000 | 14.99 | 13 | 90 | 14.99 | 13 | 90 | 2.70 | 0 |

Update messages 1805088, messages to follow through or cease to follow through a friend 199434; run time 24 s over 5 seeds.

## GFF-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1008151 | 0.9983 | 1.0000 | 1.82 | 2 | 2 | 1.82 | 2 | 2 | 6.25 | 0 |
| 10-99 | 61687 | 0.9983 | 1.0000 | 1.81 | 2 | 3 | 1.81 | 2 | 3 | 5.90 | 0 |
| 100-999 | 6251 | 0.9980 | 1.0000 | 1.85 | 2 | 5 | 1.85 | 2 | 5 | 4.35 | 0 |
| 1000-9999 | 837 | 0.5510 | 1.0000 | 14.44 | 13 | 90 | 14.44 | 13 | 90 | 2.69 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1076926 | 0.8230 | 1.0000 | 14.36 | 13 | 90 | 14.36 | 13 | 90 | 2.70 | 0 |

Update messages 1805088, messages to follow through or cease to follow through a friend 199434; run time 25 s over 5 seeds.
