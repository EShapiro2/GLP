# GFF, GFF-PS and GFF-FR in time, q = 4, fault rate 0.0001, mean down stretch 1000 steps

Kleinberg's 100 x 100 grid, 4 long-range friendship(s) per person; seeds 1, 2, 3, 4, 5, pooled; each author posts every 5000 steps; produced by programs/gff/paths/timed.py (see README.md).
Fraction of agent-steps down, per seed: 0.0902, 0.0904, 0.0901, 0.0899, 0.0902.

Per item: its followers are the decided followers following the author at posting; share is of them reached by the end of the run; hands are the signatures on the first copy received; steps the steps it took; messages are the item's messages sent, over the followers reached (items reaching none left out); waiting is the share of decided followers wanting to follow at posting who do not follow.

## GFF

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1083076 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 169.01 | 2 | 9761 | 11.02 | 0 |
| 10-99 | 65723 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 179.99 | 2 | 8618 | 9.47 | 0 |
| 100-999 | 6513 | 0.9896 | 1.0000 | 2.79 | 2 | 16 | 233.16 | 2 | 9905 | 3.61 | 0 |
| 1000-9999 | 705 | 0.4285 | 0.9975 | 30.66 | 31 | 79 | 2652.56 | 1835 | 23309 | 1.05 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1156017 | 0.8343 | 0.9976 | 29.63 | 30 | 79 | 2563.30 | 1723 | 23309 | 1.29 | 0 |

Update messages 5243954, messages to follow through or cease to follow through a friend 79324; run time 42 s over 5 seeds.

## GFF-PS

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1083076 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 169.01 | 2 | 9761 | 11.02 | 0 |
| 10-99 | 65723 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 179.99 | 2 | 8618 | 9.47 | 0 |
| 100-999 | 6513 | 0.9896 | 1.0000 | 2.79 | 2 | 16 | 232.81 | 2 | 9905 | 3.61 | 8 |
| 1000-9999 | 705 | 0.4285 | 0.9973 | 28.48 | 28 | 78 | 2454.72 | 1654 | 23309 | 1.05 | 3661 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1156017 | 0.8343 | 0.9974 | 27.53 | 28 | 78 | 2372.64 | 1536 | 23309 | 1.29 | 3669 |

Update messages 5243954, messages to follow through or cease to follow through a friend 86662; run time 44 s over 5 seeds.

## GFF-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1083076 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 161.89 | 2 | 9761 | 11.16 | 0 |
| 10-99 | 65723 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 168.69 | 2 | 8618 | 9.65 | 0 |
| 100-999 | 6513 | 0.9896 | 1.0000 | 2.79 | 2 | 16 | 223.22 | 2 | 9905 | 3.99 | 0 |
| 1000-9999 | 705 | 0.4285 | 1.0000 | 12.09 | 11 | 69 | 178.63 | 11 | 12525 | 2.18 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1156017 | 0.8343 | 1.0000 | 11.73 | 10 | 69 | 178.95 | 11 | 12525 | 2.39 | 0 |

Update messages 5243954, messages to follow through or cease to follow through a friend 149926; run time 46 s over 5 seeds.

## GFF-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1083076 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 161.89 | 2 | 9761 | 11.16 | 0 |
| 10-99 | 65723 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 168.75 | 2 | 8618 | 9.65 | 0 |
| 100-999 | 6513 | 0.9896 | 1.0000 | 2.79 | 2 | 16 | 223.22 | 2 | 9905 | 3.99 | 0 |
| 1000-9999 | 705 | 0.4285 | 1.0000 | 11.35 | 10 | 69 | 177.44 | 10 | 12525 | 2.20 | 0 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1156017 | 0.8343 | 1.0000 | 11.02 | 10 | 69 | 177.81 | 10 | 12525 | 2.41 | 0 |

Update messages 5243954, messages to follow through or cease to follow through a friend 149926; run time 48 s over 5 seeds.

## GFF-PS-FR-random

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1083076 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 161.89 | 2 | 9761 | 11.16 | 0 |
| 10-99 | 65723 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 168.69 | 2 | 8618 | 9.65 | 0 |
| 100-999 | 6513 | 0.9896 | 1.0000 | 2.79 | 2 | 16 | 223.22 | 2 | 9905 | 3.98 | 40 |
| 1000-9999 | 705 | 0.4285 | 1.0000 | 11.19 | 9 | 78 | 176.77 | 10 | 12525 | 2.20 | 28335 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1156017 | 0.8343 | 1.0000 | 10.87 | 9 | 78 | 177.16 | 10 | 12525 | 2.41 | 28375 |

Update messages 5243954, messages to follow through or cease to follow through a friend 205948; run time 46 s over 5 seeds.

## GFF-PS-FR-far

| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1-9 | 1083076 | 0.9927 | 1.0000 | 1.88 | 2 | 3 | 161.89 | 2 | 9761 | 11.16 | 0 |
| 10-99 | 65723 | 0.9924 | 1.0000 | 1.91 | 2 | 4 | 168.75 | 2 | 8618 | 9.65 | 0 |
| 100-999 | 6513 | 0.9896 | 1.0000 | 2.79 | 2 | 16 | 223.22 | 2 | 9905 | 3.98 | 40 |
| 1000-9999 | 705 | 0.4285 | 1.0000 | 10.79 | 9 | 70 | 175.45 | 10 | 12525 | 2.21 | 26587 |
| 10000 | 0 | - | - | - | - | - | - | - | - | - | 0 |
| all | 1156017 | 0.8343 | 1.0000 | 10.48 | 9 | 70 | 175.89 | 9 | 12525 | 2.42 | 26627 |

Update messages 5243954, messages to follow through or cease to follow through a friend 202465; run time 172 s over 5 seeds.
