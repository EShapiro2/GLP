# Path lengths under GFF

A graph-theoretic study of the length of the paths through which followers follow an author under GFF's Algorithms pragmas, on Kleinberg's grid: GFF #1's task to Code (`Code_inbox.md`, 2026-10-27 18:52 UTC, items 1 to 3; item 4 sent whole at 21:30 UTC), approved by Udi on 2026-10-27.  The contract is `../GFF.txt`; the paper is GFF's.

## How to run it

    cd programs/gff/paths && /usr/bin/python3 paths.py

Python 3 with the standard library only (no numpy); written and run with Python 3.9.6.  It writes `tables/q1.md`, `tables/q2.md` and `tables/q4.md`.  Options: `--q 1 2 4`, `--seeds 1 2 3 4 5`, `--n 100`, `--jobs <processes>`, `--out <directory>`.

The run of all three q, five seeds each, took 16 seconds on Udi's Mac, sequentially, one to two seconds a run; each table gives the time of each of its runs.

## Seeds

Run (q, s), for q in 1, 2, 4 and s in 1 to 5, draws everything from one generator, Python's `random.Random(s)`: the graph, the numbers of followers, the followers, the order of deciding, and the order of each pass of shortening.  The same Python reproduces the run.

## The graph

N = 10000 persons on Kleinberg's 100 x 100 grid (Kleinberg 2000), not a torus, with lattice distance d((i,j),(k,l)) = |i-k| + |j-l|.  Each person is a friend of their grid neighbours --- four inside the grid, three on an edge, two at a corner --- and makes q long-range friendships.  Person u, taken in index order, draws a long-range contact v with probability

    P(v) = d(u,v)^-2 / sum over w != u of d(u,w)^-2,

Kleinberg's distribution with r = 2, and draws again while v is already a friend of u (a grid neighbour, a contact u drew, or a person who drew u), until u has made q new friendships.  Each contact is so drawn from Kleinberg's distribution conditioned on its not already being a friend.  Friendship is symmetric.  The draw is exact, by rejection: a distance d in 1 to 198 with probability proportional to 1/d, a point uniformly among the 4d lattice points at distance d, drawn again while it is outside the grid.

## The followers

Every person is an author.  An author's number of followers k is drawn from the discrete power law of exponent 2, P(k) = k^-2 / zeta(2) for k = 1, 2, ..., with zeta(2) = pi^2/6, capped at 9999, the population less the author: a draw above the cap is the cap.  The k followers are drawn uniformly at random without replacement from the 9999 persons other than the author.

## The first following: shortest path through a friend who follows

This is GFF's Algorithms pragma: "when Alice decides to follow Bob, Alice's machine asks every friend whether they follow Bob (GFF:3) and follows Bob through the friend with the shortest path to Bob (GFF:2)".  By GFF:2 friends follow each other, so before anyone decides, the author follows their own feed at path length 0 and every friend of the author follows the author at path length 1.

All author-follower pairs decide in one random order, one at a time.  A decider who is a friend of the author already follows, at 1.  Any other decider follows through the friend with the shortest path to the author, at one more than that friend's length, ties going to the first such friend in the decider's list of friends.  A decider with no friend following waits, in a queue kept per author.  When a person comes to follow an author, every friend of theirs waiting on that author joins a first-in first-out queue, and each, taken from it, follows through their friend with the shortest path at that time; the queue runs out before the next decider in the order decides.  A follower's path length is fixed when they follow.

## Path shortening, the second Algorithms pragma

After every decider has had their turn, repeatedly, a follower whose friend has a path to the author shorter by two or more than their own ends their following and follows through that friend, the one with the shortest path, ties as above.  A follower's path length is one more than that of the friend they follow through, so the lengths of all who follow through them, directly or through others, fall with theirs.  The followers are taken in a random order, a new one each pass, until a pass makes no change; the tables count the re-followings.  The waiting deciders stay waiting.

## The shortest-path trees, for comparison

1. Over the followers' subgraph: the friendship graph restricted to the author and to those who follow the author at the end of the first following, being the deciders who followed and the author's friends.  A follower's path is their distance to the author in it.
2. Over the whole friendship graph: a follower's path is their graph distance to the author, and its ratio is 1; the tables give the distance.

Shortening stops only when no follower has a friend following at two or more less than their own length, so then every follower's length is one more than the least length among their friends who follow, which is their distance to the author in the followers' subgraph.  So the tables after shortening and for the tree over the followers' subgraph agree in every entry, as they do in the run.

## The measures

Over all author-follower pairs, in bins by the author's number of followers --- 1-9, 10-99, 100-999, 1000-9999, and 10000, which is empty, k being capped at 9999 --- each table gives:

1. the path length of the follower to the author: its distribution, count by length, and its mean, median and max;
2. its ratio to the graph distance between the follower and the author, by breadth-first search in the friendship graph: mean, median and max;
3. the fraction of deciders still waiting, for the first following.

Path length and ratio are over the deciders who follow; a waiting decider has neither.  The tables give these for the first following, after shortening, and for the two shortest-path trees, the five seeds pooled, and then a line per seed and bin.
