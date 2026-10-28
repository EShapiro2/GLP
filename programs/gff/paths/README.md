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

# timed.py: GFF, GFF-PS and GFF-FR in time

A second script, for GFF #1's task of 2026-10-28 10:52 UTC (Udi approving): to compare GFF, GFF-PS and GFF-FR, with the same grid, seeds and authors as `paths.py`, now with time and with faults.  The contracts are `programs/gff/GFF.txt`, `GFF-PS.txt` and `GFF-FR.txt` on the branch GFF-chooser, their clauses and Algorithms lines, with GFF's revision of 12:10 UTC: an item is never sent to one who signed it, in Delivery (GFF:4 at `be482941`) and in redundant forwarding, and GFF-PS:1 switching the following through which the item arrived.

The tables are of that rule throughout.  Against the earlier tables of `timed.py` (`0c42baba`): GFF is unchanged, its followings forming a tree, so that no item ever went to a signer.  GFF-FR sends fewer messages, an item no longer being forwarded, directly or redundantly, to one who signed it; the other measures are unchanged.  GFF-PS is unchanged without faults and changes a little with them, since a late item that comes via a following Alice has already left now switches nothing.

## How to run it

    cd programs/gff/paths && /usr/bin/python3 timed.py

Options: `--q 1 2 4`, `--seeds 1 2 3 4 5`, `--n 100`, `--K 5000`, `--faults 0 0.00001 0.0001`, `--down 1000`, `--scenarios GFF GFF-PS GFF-FR-random GFF-FR-far GFF-PS-FR-random GFF-PS-FR-far`, `--out <directory>`.  It writes `tables/timed-q<q>-f<fault rate>.md`, one table per q and fault rate.  The grid is not scaled down: the run is at n = 100.

The run of all three q, five seeds each, the six scenarios at each of the three fault rates, took 2386 seconds on Udi's Mac, sequentially, with other runs beside it: 70 to 268 seconds a seed, 3 to 58 seconds a scenario at a fault rate.  Each table gives the run time of each scenario over the five seeds.

## The graph, the authors and the seeds

They are those of `paths.py`: `timed.py` draws them with its functions from `random.Random(s)` in the same order, so for the same q and s the grid, the friendships, the authors and their followers are the same.  From the same generator it then draws the order of arrivals, the posting offsets, and one integer from which are seeded the faults, one generator per fault rate, and each scenario's own choices, one generator per scenario.  Every scenario of a run therefore has the same arrivals, the same posting times and, at a fault rate, the same faults.

## Time

The steps are discrete.  The friendships, those of the grid and the long-range ones, and the following requests, a decided follower coming to want to follow their author (GFF:2), are put in one random order, interleaved, and one arrives at each step.  Each author posts an item every K = 5000 steps, at an offset of their own drawn uniformly in 0 to K - 1; with 80,000 to 150,000 arrivals, an author posts about 20 to 30 items while the graph grows.  A message --- an item, an update, a request to follow through a friend or to cease --- sent at one step is received at the next: a message takes one step per hand.  A friendship takes effect between the two at once; what each machine does on it is done by that machine.  The run ends 2K steps after the last arrival, and the items posted up to K steps after the last arrival are measured, each having had at least K steps to arrive.

## GFF

1. Friends follow each other (GFF:2), and the author sends every item they post to every friend (GFF:5).
2. Updating (GFF:6 and its Algorithms line): Carol updates every friend whenever Carol comes to follow an agent, by friendship or through a friend, and a new friend upon befriending, with the list of all Carol follows.  An update is a message.  Alice's machine learns from the updates which friends follow whom, in the order the updates arrive.
3. Delivery (GFF:4): the author sends every item to every friend; Carol forwards to Alice, who follows Bob through Carol, every item of Bob's feed not signed by Alice.  An item is never sent to one who signed it.
4. Following via Friend (GFF:3 and its Algorithms line): when Alice wants to follow Bob, Alice's machine follows Bob through the first friend known to follow Bob, and through no other while that following stands.  With no friend known to follow, Alice waits, and follows when an update tells her of one.  Alice follows through Carol by a message to Carol, after which Carol forwards Bob's items to Alice (GFF:5).  Alice comes to follow Bob, and updates her friends, when she sends it.
5. A follower who becomes a friend of the author ceases every following of the author through a friend and follows by friendship.  The mail and the contract leave it open; GFF:2 confines wanting to follow through a friend to one who is not a friend.
6. No one ceases to want to follow, so Unfollowing (GFF:4) is not exercised, and no following rests on a friend who has ceased to follow.
7. An item carries the signatures of its path, the author's first (GKK:4 on GFF-chooser).  The hands an item took to reach a follower are the signatures on the first copy the follower received.

## GFF-PS

GFF-PS:1 and its Algorithms line, on arrival: "Alice, who received via Carol an item of Bob's feed signed by Alice's friend Dave, follows Bob through Dave instead of via Carol", by a message to each of the two.  Carol is one Alice follows Bob through, the item having come by that following; an item that came from a friend Alice does not follow through, by redundant forwarding, switches nothing.  With several such friends, Alice takes the one nearest the author on the item's path; the contract leaves which one open.  Under GFF alone Alice receives Bob's items only through Carol, so every signer is nearer Bob than Carol.

## GFF-FR

GFF-FR:1 and its Algorithms line: Alice's machine follows Bob through two friends known to follow Bob when there are two, and through the one when there is one, adding a second when another becomes known.  Two variants of the choice, as the mail asks:

1. random: the two chosen uniformly at random among the friends known to follow;
2. far apart: the two whose paths to Bob share the fewest agents, the author not counted, and with one already followed through, the one whose path shares the fewest with its path.  A path is the chain of first followings from the friend to Bob at the time.  This is the simulator's knowledge: the contract gives Alice no means to learn a friend's path before items arrive, and the mail does not say how she learns it.

GFF-FR:2 and its Algorithms line, on arrival: Alice, who follows Bob through Carol and Dave and receives an item first from Carol, forwards it to Dave, unless Dave signed it.

## Comparison 3, GFF-PS and GFF-FR together

Both rules at once, with GFF-FR's random and far-apart choices.  GFF-PS:1 replaces the following through which the item arrived.  If Alice already follows Bob through Dave, the following via Carol ends and Alice is left with one; GFF-FR's Algorithms line then adds a second known friend, other than Carol.  The contracts leave open whether Carol may be taken again; if she may, Alice takes Carol again and leaves her again on every item that comes via Carol signed by Dave.  Before GFF's revision this comparison was not run: an item forwarded redundantly by Eve, who follows Bob through Alice, came to Alice signed by Eve, and GFF-PS:1 had Alice follow through Eve, a cycle; Delivery now sends no item to one who signed it, and Shortening names the following it replaces.

## Faults

Each agent, independently, stays up for a stretch drawn as ceil(Exp(mean 1/F)) steps and then goes down for ceil(Exp(mean D)) steps, F being the fault rate per agent per step and D the mean down stretch, here D = 1000 and F = 0, 0.00001 and 0.0001, about 0%, 1% and 9% of agent-steps down.  A down agent takes nothing: the messages sent to it, and the arrivals concerning it, are kept and taken, in the order sent, when it comes back.  A down author does not post.  Faults are applied to every scenario; comparison 2 is GFF against GFF-FR at the fault rates above 0, and comparison 1 is GFF against GFF-PS at 0, the tables giving every scenario at every rate.

## The measures

They are per item, by the author's number of decided followers in powers of ten as in `paths.py`, pooled over the seeds.  An item's followers are the decided followers who want to follow the author and follow them, by friendship or through a friend, when it is posted; the waiting fraction is of those who want to follow at posting and do not.  Over an item's followers:

1. the share reached by the end of the run;
2. the hands the first copy took to reach each;
3. the steps it took, which the mail does not ask for and which show the faults, hands being unchanged by a delay;
4. the messages of the item sent, to friends and forwarders as well as to the followers, over the followers reached, the items reaching none left out.

For GFF and GFF-PS without faults: the hands against the distance in the friendship graph as it stands at posting, which is the tree of shortest paths over the whole graph, and against the distance in the subgraph of the author and all following the author at posting, which is the tree over the followers; the same by the fifth of the arrivals in which the item was posted; and the switches.  The tables also give, per scenario, the update messages and the messages to follow through or cease to follow through a friend.
