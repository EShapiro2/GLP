# gff

GFF's sGLP sources (an owner's own `programs/sglp/<owner>/`, `servers/peoplesweb/tools/directories.py`).  Yuval Lande, 2026-11-19.

- `Feeds_sglp.glp` --- Feeds (`programs/gff/Feeds.txt`, `cascade/Feeds`) on a social graph of one's choosing, on `programs/sglp/graphs`'s library, so that it runs on any graph `programs/sglp/graphs/gen.py` prints.  Each person builds its part of the graph (`graph_next/3`); then the author, agent 1, posts three items, a day on average before each (Feeds:1, Post), and every other person without a pointer labelled by the author proposes `labeled_point_friend(Me, J, Author)` through a friend J drawn uniformly (Labelled Overlay:1), an hour on average apart.  A friend of the author has such a pointer from befriending it (Labelled Overlay:4).  Everyone approves everything.  The platform delivers the items along the author-labelled pointers itself (Feeds:2, Deliver).
- `sim/spread.py <graph> <log>` --- a run's requests and refusals, the agents following the author's feed, each item's holders, and by hop distance from the author the agents following, holding every item, the median hour they came to follow and their follow attempts.
- `test_feeds.sh` --- a ring of 8 for four days: every agent follows, every item reaches all 8, and the author's friends make no follow attempt.

## A run

```bash
python3 -I programs/sglp/graphs/gen.py regular --n 32 --d 4 --seed 3 > reg32.graph
bash programs/sglp/graphs/run.sh Feeds reg32.graph '4 days' 1 reg32.log programs/sglp/gff/Feeds_sglp.glp
python3 -I programs/sglp/gff/sim/spread.py reg32.graph reg32.log
```

The source's run declaration is of `graph` agents, so one source runs on any graph.

## Five topologies of 32 agents

Four simulated days, seed 1, graph seed 3, on an Apple M5, GLP `912c76b9c` with `programs/sglp/graphs`; the five runs in parallel.  In every run all 31 agents follow the author and every item posted reaches all 32.  "Built" is when the last friendship of the graph appears.

| Graph | Diameter | Built | Items posted | Median hour followed, by distance | Follow attempts (refused) | Wall-clock |
|---|---|---|---|---|---|---|
| ring, 2 each side | 8 | 16.6 h | 2 | 10.3, 14.2, 14.7, 17.9, 19.7, 22.2, 22.7, 26.8 | 181 (154) | 325 s |
| Watts–Strogatz, k 2, beta 0.1 | 7 | 20.8 h | 3 | 17.0, 22.1, 24.3, 27.2, 26.9, 26.3 | 324 (297) | 609 s |
| 4-regular | 4 | 25.0 h | 3 | 13.8, 16.7, 19.0, 19.6 | 100 (73) | 534 s |
| Barabási–Albert, m 2 | 5 | 64.3 h | 3 | 48.8, 43.2, 37.9 | 282 (265) | 817 s |
| 4 x 8 grid | 10 | 13.5 h | 3 | 6.3, 12.1, 15.2, 17.5, 18.7, 20.0, 20.1, 20.2, 22.1, 22.0 | 235 (206) | 440 s |

1. Following spreads outward from the author: the median hour an agent comes to follow grows with its distance, by about two hours a hop on the ring and the grid.
2. The follow attempts grow with distance, and most are refused: an agent tries through a friend drawn uniformly, who does not yet follow.
3. On the Barabási–Albert graph the author is the hub, with 14 friends, and befriends them one at a time (an agent proposes nothing while one of its attempts is in flight), so the graph takes 64 hours; its 16 agents at distance 2 finish their own part long before, and make 257 attempts through friends the author has not yet befriended.
4. The ring's author posted 2 items in the four days: a post comes a day on average after the last, from when the author's part of the graph is built.

## Not yet

A follower that tried through a friend who refused tries again an hour later through a friend drawn again; retrying more slowly after a refusal, or only through a friend that has shown it follows, would cut the refused attempts, which are most of the run's acts at distance 2 and more.
