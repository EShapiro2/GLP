# Path lengths under GFF

A graph-theoretic study of the length of the paths through which followers follow an author under GFF's Algorithms pragmas, on Kleinberg's grid: GFF #1's task to Code (`Code_inbox.md`, 2026-10-27 18:52 UTC, items 1 to 3; item 4 sent whole at 21:30 UTC, its shortening rule changed at 21:40 UTC to shortening by the signatures of the items received; two more rules at 21:50 UTC, discovery two hops deep, and three, and shortening by re-asking), approved by Udi on 2026-10-27.  The contract is `../GFF.txt`; the paper is GFF's.

## How to run it

    cd programs/gff/paths && /usr/bin/python3 paths.py

Python 3 with the standard library only (no numpy); written and run with Python 3.9.6.  It writes `tables/q1.md`, `tables/q2.md` and `tables/q4.md`.  Options: `--q 1 2 4`, `--seeds 1 2 3 4 5`, `--hops 1 2 3`, `--n 100`, `--jobs <processes>`, `--out <directory>`.

The run of all three q, five seeds each, at discovery one, two and three hops deep, took 170 seconds on Udi's Mac, sequentially.  A run took 4 to 9 seconds for q = 1 and 2 and 19 to 20 for q = 4; discovery three hops deep took 2 to 4 seconds of it for q = 1, 5 to 6 for q = 2 and 14 to 15 for q = 4.  Each table gives the time of each run at each depth.

## Seeds

Run (q, s), for q in 1, 2, 4 and s in 1 to 5, draws everything from one generator, Python's `random.Random(s)`: the graph, the numbers of followers, the followers, the order of deciding, and the order of each pass of the earlier shortening rule, for one hop, then two, then three; discovery and shortening by signatures and by re-asking draw nothing, so the numbers for one hop are those of the runs before the two rules were added.  The same Python reproduces the run.

## The graph

N = 10000 persons on Kleinberg's 100 x 100 grid (Kleinberg 2000), not a torus, with lattice distance d((i,j),(k,l)) = |i-k| + |j-l|.  Each person is a friend of their grid neighbours --- four inside the grid, three on an edge, two at a corner --- and makes q long-range friendships.  Person u, taken in index order, draws a long-range contact v with probability

    P(v) = d(u,v)^-2 / sum over w != u of d(u,w)^-2,

Kleinberg's distribution with r = 2, and draws again while v is already a friend of u (a grid neighbour, a contact u drew, or a person who drew u), until u has made q new friendships.  Each contact is so drawn from Kleinberg's distribution conditioned on its not already being a friend.  Friendship is symmetric.  The draw is exact, by rejection: a distance d in 1 to 198 with probability proportional to 1/d, a point uniformly among the 4d lattice points at distance d, drawn again while it is outside the grid.

## The followers

Every person is an author.  An author's number of followers k is drawn from the discrete power law of exponent 2, P(k) = k^-2 / zeta(2) for k = 1, 2, ..., with zeta(2) = pi^2/6, capped at 9999, the population less the author: a draw above the cap is the cap.  The k followers are drawn uniformly at random without replacement from the 9999 persons other than the author.

## The first following: shortest path through a friend who follows

This is GFF's Algorithms pragma: "when Alice decides to follow Bob, Alice's machine asks every friend whether they follow Bob (GFF:3) and follows Bob through the friend with the shortest path to Bob (GFF:2)".  By GFF:2 friends follow each other, so before anyone decides, the author follows their own feed at path length 0 and every friend of the author follows the author at path length 1.

All author-follower pairs decide in one random order, one at a time.  A decider who is a friend of the author already follows, at 1.  Any other decider follows through the friend with the shortest path to the author, at one more than that friend's length, ties going to the first such friend in the decider's list of friends.  A decider with no friend following waits, in a queue kept per author.  When a person comes to follow an author, every friend of theirs waiting on that author joins a first-in first-out queue, and each, taken from it, follows through their friend with the shortest path at that time; the queue runs out before the next decider in the order decides.  A follower's path length is fixed when they follow.

## Discovery two hops deep, and three

GFF's rule 1, verbatim: "a decider none of whose friends follows the author asks each friend to ask their friends; if a friend Carol has a friend Dave who follows, Carol follows the author through Dave on the decider's behalf, a relay following, and the decider follows through Carol; the shortest such path is taken; a decider with no path within two hops waits, and is tried again whenever a friend or a friend's friend comes to follow."

Ties go to the first path found, friends taken in list order.  Three hops deep, a decider with no path within two hops asks one hop further: Carol asks a friend Eve to ask Eve's friends, and if Eve has a friend Dave who follows, Eve follows through Dave and Carol through Eve, both relays, and the decider through Carol.  A waiter is tried again whenever a person within the depth of discovery of them comes to follow, relays included, by the same first-in first-out queue as one hop deep; the waiters within that many hops of the person, by paths through persons not following, join it.  A relay who is later a decider of the same author already follows.  The tables give, for each depth: the fraction of deciders waiting; the relay followings, a relay and an author, in all and per author; the relay followers, the persons relaying for some author, per seed; and the holders of a feed per decided follower, the decided followers who follow and the relays, summed over the authors of a bin, over the decided followers who follow, and the same with the author's friends added, who hold the feed by friendship.  One hop deep is the first following above.

## Shortening by re-asking

GFF's rule 2, verbatim: "after all have followed, repeatedly, every follower asks all their friends again and switches to the friend with the shortest path when it is shorter than their own; until none switches."

It is applied after the first following at each depth.  "Shorter than their own" is read as the path through that friend being shorter than the follower's own path, so a follower switches only to shorten their path.  Every follower asks: all who follow the author but the author, relays and the author's friends included.  In a pass every follower asks on the paths as they stand at the start of the pass and the switches take effect together.  The tables count the switches and the asks, one ask per friend asked per pass, the last pass, in which none switches, included.

Like the earlier rule, it stops only when no follower has a friend following at two or more less than their own length, so its result is the shortest-path tree over the followers' subgraph, and the tables after re-asking, after the earlier rule and for that tree agree in every entry at every depth.  The rules differ only in their counts.

## Shortening by the signatures of the items received, the second Algorithms pragma

This is GFF's Algorithms line Shortening: "when Alice receives an item of Bob's feed signed by a friend nearer to Bob than the friend Alice follows Bob through, Alice follows Bob through that friend instead (GFF:2)".  By GFF:4 an item the author posts reaches each follower along their following path, signed by the author, at position 0, and by each follower it passes, in order, the friend the follower follows through last.

After every decider has had their turn, the author posts an item.  A follower who finds among the signers of the item they received a friend of theirs at a position nearer the author than the friend they follow through switches to that friend, the nearest such if there are several.  A switch takes effect for the next item only, the items being what a follower receives: all the switches of a pass are decided on the item of that pass, and then the author posts again, until a pass changes nothing.  The tables count the switches and the posts, the last post being the one on which no follower switches.

No follower ever switches.  Every signer of the item a follower receives followed the author before that follower did, and was then a friend following, if a friend at all, when the follower chose the friend with the shortest path; so no signer who is a friend is nearer than the friend chosen.  The run confirms it for every q and seed: no switch, and one post per author.

## The earlier shortening rule, for comparison

This is item 4's rule as first sent.  After every decider has had their turn, repeatedly, a follower whose friend has a path to the author shorter by two or more than their own ends their following and follows through that friend, the one with the shortest path, ties as above.  A follower's path length is one more than that of the friend they follow through, so the lengths of all who follow through them, directly or through others, fall with theirs.  The followers are taken in a random order, a new one each pass, until a pass makes no change; the tables count the re-followings.  The waiting deciders stay waiting.

## The shortest-path trees, for comparison

1. Over the followers' subgraph: the friendship graph restricted to the author and to those who follow the author at the end of the first following, being the deciders who followed, the relays and the author's friends.  A follower's path is their distance to the author in it.
2. Over the whole friendship graph: a follower's path is their graph distance to the author, and its ratio is 1; the tables give the distance.

The earlier rule stops only when no follower has a friend following at two or more less than their own length, so then every follower's length is one more than the least length among their friends who follow, which is their distance to the author in the followers' subgraph.  So the tables after the earlier rule and for the tree over the followers' subgraph agree in every entry, as they do in the run.

## The measures

Over all author-follower pairs, in bins by the author's number of followers --- 1-9, 10-99, 100-999, 1000-9999, and 10000, which is empty, k being capped at 9999 --- each table gives:

1. the path length of the follower to the author: its distribution, count by length, and its mean, median and max;
2. its ratio to the graph distance between the follower and the author, by breadth-first search in the friendship graph: mean, median and max;
3. the fraction of deciders still waiting, for the first following.

Path length and ratio are over the deciders who follow; a waiting decider has neither.  The tables give these, for discovery one, two and three hops deep, for the first following, after shortening by re-asking, after the earlier rule, and for the two shortest-path trees, and, one hop deep, after shortening by signatures, the five seeds pooled, and then a line per seed and bin.
