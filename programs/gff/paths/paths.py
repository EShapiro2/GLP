#!/usr/bin/env python3
"""Path lengths under GFF: following through a friend on Kleinberg's grid.

GFF #1's task (Code_inbox.md, 2026-10-27 18:52 UTC, items 1-3; item 4 sent
whole at 21:30 UTC, its rule changed at 21:40 UTC; two more rules at 21:50 UTC,
discovery two (and three) hops deep and shortening by re-asking).  Python 3 standard library only; written and run with
/usr/bin/python3, Python 3.9.6.  Run:  python3 paths.py   (see README.md).

THE GRAPH (item 1).  N = n*n persons, n = 100, on Kleinberg's n x n grid
(Kleinberg 2000), not a torus: person (i, j), 0 <= i, j < n, and lattice
distance d((i,j),(k,l)) = |i-k| + |j-l|.  Each person is a friend of their
grid neighbours (four inside the grid, three on an edge, two at a corner) and
makes q long-range friendships.  Person u, taken in index order u = i*n + j,
draws a long-range contact v with probability
        P(v) = d(u,v)^-2 / sum_{w != u} d(u,w)^-2      (Kleinberg's r = 2),
and redraws while v is already a friend of u (a grid neighbour, a contact u
drew, or a person who drew u), until u has made q new friendships.  So the
distribution of each contact is Kleinberg's conditioned on not already being a
friend.  Friendship is symmetric: a contact u draws is a friend of u and u of
it.  The draw is exact, by rejection: d is drawn with P(d) proportional to 1/d
for d = 1 .. 2(n-1) (the 4d lattice points at distance d in Z^2 each having
weight d^-2), a point uniformly among those 4d, and the point is redrawn while
it is outside the grid.

THE FOLLOWERS (item 2).  Every person is an author.  An author's number of
followers k is drawn from the discrete power law of exponent 2,
        P(k) = k^-2 / zeta(2),  k = 1, 2, ...,   zeta(2) = pi^2/6,
and capped at N - 1 = 9999, the population less the author (a draw above the
cap is the cap).  The k followers are drawn uniformly at random without
replacement from the N - 1 persons other than the author.

THE FIRST FOLLOWING (item 2; GFF's Algorithms pragma: shortest path through a
friend who follows).  By GFF:2, friends follow each other, so before anyone
decides, the author follows their own feed at path length 0 and each friend
of the author follows the author at path length 1 (item 4: "the author's
friends at one").  All author-follower pairs decide in one random order, one
at a time.  A decider who already follows (a friend of the author) stays at 1.
Otherwise the decider follows through the friend with the shortest path to the
author, at one more than that friend's length (ties: the first such friend in
the decider's friend list); a decider with no friend following waits, in the
author's queue.  When a person comes to follow an author, every friend of
theirs waiting on that author is put on a FIFO work queue and, taken from it,
follows through their friend with the shortest path then; the cascade runs out
before the next decider in the order.  A follower's path length is fixed at
the time they follow.

DISCOVERY h HOPS DEEP (GFF, 21:50 UTC, rule 1), h in 1, 2, 3; h = 1 is the
first following above.  A decider none of whose friends follows the author asks
each friend to ask their friends; if a friend Carol has a friend Dave who
follows, Carol follows the author through Dave on the decider's behalf, a relay
following, and the decider follows through Carol; the shortest such path is
taken (ties: the first found, friends in list order).  At h = 3 a decider with
no path within two hops asks one hop further, Carol asking Eve to ask Eve's
friends, and Eve and Carol both follow as relays.  A decider with no path
within h hops waits, and is tried again whenever a person within h hops of
them comes to follow (relays included), by the same FIFO queue: when a person
comes to follow, the waiters within h hops of them, by paths through persons
not following, join it.  A relay who is later a decider of the same author
already follows.  Measured besides: the relay followings (a relay and an
author) per author, the relay followers (distinct persons relaying for some
author) per seed, and the holders of a feed per decided follower: the decided
followers who follow and the relays, summed over the authors of a bin, over
the decided followers who follow; and the same with the author's friends added.

SHORTENING BY RE-ASKING (GFF, 21:50 UTC, rule 2), after each depth of
discovery.  After all have followed, repeatedly, every follower (all following
the author but the author, relays and the author's friends included) asks all
their friends again, and switches to the friend with the shortest path when
the path through that friend is shorter than their own; the answers of a pass
are the paths at its start, and the switches take effect together; passes
until none switches.  Counted: the switches and the asks, one per friend asked
per pass, the pass in which none switches included.

SHORTENING BY SIGNATURES (item 4, as GFF changed it, 2026-10-27 21:40 UTC;
GFF.txt's Algorithms line Shortening).  An item the author posts reaches each
follower along their following path (GFF:4), signed by the author, at position
0, and by each follower it passes, in order, the friend the follower follows
through last.  After every decider has had their turn, the author posts an
item; a follower who finds among the signers of the item they received a
friend of theirs at a position nearer the author than the friend they follow
through switches to that friend, the nearest such if several.  All switches of
a pass are decided on the item of that pass and take effect for the next item
only; the author then posts again, until a pass changes nothing.  The tables
count the switches and the posts.  Every signer of the item a follower receives
followed before that follower did, and so was a friend following when the
follower chose the friend with the shortest path; so after the first following
no follower ever finds such a friend, and the run confirms it: no switch, one
post per author.

THE EARLIER SHORTENING RULE (item 4 as first sent, for comparison).  After
every decider has had their turn: repeatedly, a follower whose friend has a path to the author
shorter by two or more than their own ends their following and follows
through that friend (the friend with the shortest path, ties as above); a
follower's path length is one more than that of the friend they follow
through, so the lengths of all who follow through them, directly or not, fall
with it.  The followers are swept in a random order, a new order per pass,
until a pass makes no change.  The count of such re-followings is reported.
Waiting deciders stay waiting; the author's friends stay at 1.

THE SHORTEST-PATH TREES (item 4, for comparison).  (a) Over the followers'
subgraph: the friendship graph restricted to the author and those following
the author at the end of the first following (the deciders who followed, the
relays and the author's friends); a follower's path is their BFS distance to the author
in it.  (b) Over the whole friendship graph: a follower's path is their graph
distance to the author, so its ratio is 1; reported as the distance itself.

MEASURES (item 3).  Over all author-follower pairs, in bins by the author's
number of followers k (1-9, 10-99, 100-999, 1000-9999, 10000; the last is
empty, k being capped at 9999): the distribution of path length (count by
length; mean, median, max), its ratio to the graph distance between the
follower and the author in the friendship graph (BFS; mean, median, max), and
the fraction of deciders still waiting.  Path and ratio are over the deciders
who follow; a waiting decider has neither.

SEEDS.  A run (q, s) draws everything from one generator, random.Random(s):
graph, follower counts, followers, the order of deciding, the order of the
passes of the earlier shortening rule, for h = 1, then 2, then 3 (discovery,
shortening by signatures and by re-asking draw nothing).  q in 1, 2, 4;
s in 1 .. 5.
"""

import argparse
import math
import os
import random
import statistics
import sys
import time
from bisect import bisect_left
from collections import deque
from multiprocessing import Pool

N_SIDE = 100
BIN_LABELS = ["1-9", "10-99", "100-999", "1000-9999", "10000"]


def set_side(n):
    global N_SIDE
    N_SIDE = n


def bin_of(k):
    return min(len(str(k)) - 1, 4)


def cumulative(weights):
    out, s = [], 0.0
    for w in weights:
        s += w
        out.append(s)
    return out


def build_graph(rng, n, q):
    N = n * n
    adj = [[] for _ in range(N)]
    friends = [set() for _ in range(N)]

    def link(u, v):
        adj[u].append(v)
        adj[v].append(u)
        friends[u].add(v)
        friends[v].add(u)

    for i in range(n):
        for j in range(n):
            u = i * n + j
            if j + 1 < n:
                link(u, u + 1)
            if i + 1 < n:
                link(u, u + n)
    dmax = 2 * (n - 1)
    dcum = cumulative([1.0 / d for d in range(1, dmax + 1)])
    dtot = dcum[-1]
    for u in range(N):
        i, j = divmod(u, n)
        made = 0
        while made < q:
            d = bisect_left(dcum, rng.random() * dtot) + 1
            t = rng.randrange(4 * d)
            quad, r = divmod(t, d)
            if quad == 0:
                dx, dy = d - r, r
            elif quad == 1:
                dx, dy = -r, d - r
            elif quad == 2:
                dx, dy = -(d - r), -r
            else:
                dx, dy = r, -(d - r)
            k, l = i + dx, j + dy
            if not (0 <= k < n and 0 <= l < n):
                continue
            v = k * n + l
            if v in friends[u]:
                continue
            link(u, v)
            made += 1
    return adj


def draw_followers(rng, N):
    cap = N - 1
    z = math.pi ** 2 / 6
    kcum = cumulative([1.0 / (k * k * z) for k in range(1, cap)])  # k = 1 .. cap-1
    counts = []
    for _ in range(N):
        x = rng.random()
        idx = bisect_left(kcum, x)
        counts.append(idx + 1 if idx < len(kcum) else cap)
    followers = []
    for a in range(N):
        sample = rng.sample(range(N - 1), counts[a])
        followers.append([x + 1 if x >= a else x for x in sample])
    return counts, followers


def bfs_targets(adj, src, targets):
    """Distances from src to every person in targets (all reachable)."""
    dist = {src: 0}
    left = set(targets)
    left.discard(src)
    frontier = [src]
    d = 0
    while left and frontier:
        d += 1
        nxt = []
        for u in frontier:
            for v in adj[u]:
                if v not in dist:
                    dist[v] = d
                    nxt.append(v)
                    left.discard(v)
        frontier = nxt
    return dist


def bfs_within(adj, src, allowed):
    dist = {src: 0}
    frontier = [src]
    d = 0
    while frontier:
        d += 1
        nxt = []
        for u in frontier:
            for v in adj[u]:
                if v in allowed and v not in dist:
                    dist[v] = d
                    nxt.append(v)
        frontier = nxt
    return dist


HOPS = (1, 2, 3)


def first_following(adj, N, pairs, h):
    """The first following with discovery up to h hops deep.  Returns, per
    author, depth (path length of everyone following), parent (the friend
    followed through), the waiting deciders and the relay followers."""
    depth = [None] * N
    parent = [None] * N
    for b in range(N):
        depth[b] = {b: 0}
        parent[b] = {b: None}
        for f in adj[b]:
            depth[b][f] = 1
            parent[b][f] = b
    waiting = [set() for _ in range(N)]
    relays = [set() for _ in range(N)]

    def discover(a, db):
        # one hop: the friend with the shortest path
        best = None
        for c in adj[a]:
            dc = db.get(c)
            if dc is not None and (best is None or dc < best):
                best, via = dc, c
        if best is not None:
            return [(a, via, best + 1)]
        if h >= 2:
            # two hops: a friend c asks their friends; c follows through d
            for c in adj[a]:
                for d in adj[c]:
                    dd = db.get(d)
                    if dd is not None and (best is None or dd < best):
                        best, path = dd, (c, d)
            if best is not None:
                c, d = path
                return [(c, d, best + 1), (a, c, best + 2)]
        if h >= 3:
            seen = {a}
            for c in adj[a]:
                for e in adj[c]:
                    if e in seen:
                        continue
                    seen.add(e)
                    for d in adj[e]:
                        dd = db.get(d)
                        if dd is not None and (best is None or dd < best):
                            best, path = dd, (c, e, d)
            if best is not None:
                c, e, d = path
                return [(e, d, best + 1), (c, e, best + 2), (a, c, best + 3)]
        return None

    def come(added, b):
        db, pb, w, rb = depth[b], parent[b], waiting[b], relays[b]
        newq = deque()
        work = deque()
        while True:
            for x, p, dx in added:
                db[x] = dx
                pb[x] = p
                w.discard(x)
                newq.append(x)
            for x, _, _ in added[:-1]:
                rb.add(x)  # following on the decider's behalf
            while newq:
                x = newq.popleft()
                if not w:
                    continue
                # the waiters within h hops of x, by paths through persons
                # not following, join the work queue
                seen = {x}
                frontier = [x]
                for _ in range(h):
                    nxt = []
                    for u in frontier:
                        for v in adj[u]:
                            if v in seen:
                                continue
                            seen.add(v)
                            if v in db:
                                continue
                            if v in w:
                                w.discard(v)
                                work.append(v)
                            nxt.append(v)
                    frontier = nxt
            if not work:
                return
            x = work.popleft()
            if x in db:
                # came to follow meanwhile, as a relay for another
                added = []
                continue
            added = discover(x, db)
            assert added is not None

    for f, b in pairs:
        if f in depth[b]:
            continue
        added = discover(f, depth[b])
        if added is None:
            waiting[b].add(f)
        else:
            come(added, b)
    return depth, parent, waiting, relays


def depths_of(b, par):
    ch = {}
    for f, p in par.items():
        if p is not None:
            ch.setdefault(p, []).append(f)
    dep = {b: 0}
    stack = [b]
    while stack:
        x = stack.pop()
        for c in ch.get(x, ()):
            dep[c] = dep[x] + 1
            stack.append(c)
    return dep


def shorten_by_signatures(adj, friend_sets, b, par0, dep0):
    """Item 4 as GFF changed it: returns (depths, switches, posts)."""
    par = dict(par0)
    dep = dict(dep0)
    posts = switches = 0
    while True:
        posts += 1
        moves = []
        for f, p in par.items():
            if p is None or p == b:
                continue
            fs = friend_sets[f]
            best = None
            x = par[p]
            while x is not None:
                if x in fs:
                    best = x
                x = par[x]
            if best is not None:
                moves.append((f, best))
        if not moves:
            return dep, switches, posts
        for f, g in moves:
            par[f] = g
        switches += len(moves)
        dep = depths_of(b, par)


def shorten_by_reasking(adj, b, par0, dep0):
    """Every follower asks all their friends again and switches to the friend
    with the shortest path when the path through that friend is shorter than
    their own; passes until none switches.  Returns (depths, switches, asks,
    passes)."""
    par = dict(par0)
    dep = dict(dep0)
    switches = asks = passes = 0
    while True:
        passes += 1
        moves = []
        for f in par:
            if f == b:
                continue
            asks += len(adj[f])
            best = None
            for g in adj[f]:
                dg = dep.get(g)
                if dg is not None and (best is None or dg < best):
                    best, via = dg, g
            if best + 1 < dep[f]:
                moves.append((f, via))
        if not moves:
            return dep, switches, asks, passes
        for f, g in moves:
            par[f] = g
        switches += len(moves)
        dep = depths_of(b, par)


def shorten_earlier_rule(adj, N, depth0, parent0, rng):
    """Item 4 as first sent: returns (depths, re-followings per author, passes)."""
    depth = [dict(d) for d in depth0]
    parent = [dict(p) for p in parent0]
    children = []
    for b in range(N):
        ch = {}
        for p, par in parent[b].items():
            if par is not None:
                ch.setdefault(par, set()).add(p)
        children.append(ch)
    refollow_by_author = [0] * N
    order = [(p, b) for b in range(N) for p in depth[b] if p != b]
    passes = 0
    while True:
        passes += 1
        rng.shuffle(order)
        changed = 0
        for p, b in order:
            db = depth[b]
            dp = db[p]
            best, via = None, None
            for g in adj[p]:
                dg = db.get(g)
                if dg is not None and (best is None or dg < best):
                    best, via = dg, g
            if best is not None and best <= dp - 2:
                ch = children[b]
                ch[parent[b][p]].discard(p)
                ch.setdefault(via, set()).add(p)
                parent[b][p] = via
                delta = dp - (best + 1)
                stack = [p]
                while stack:
                    x = stack.pop()
                    db[x] -= delta
                    stack.extend(ch.get(x, ()))
                changed += 1
                refollow_by_author[b] += 1
        if changed == 0:
            return depth, refollow_by_author, passes


KEYS = ("first", "sig", "reask", "early", "sub")
COUNTERS = ("authors", "deciders", "waits", "following", "relay_followings",
            "holders", "holders_all", "sig_switches", "posts", "reask_switches",
            "asks", "refollows")


def run(args):
    q, seed, n, hops = args
    t0 = time.time()
    rng = random.Random(seed)
    N = n * n
    adj = build_graph(rng, n, q)
    counts, followers = draw_followers(rng, N)
    pairs = [(f, b) for b in range(N) for f in followers[b]]
    rng.shuffle(pairs)
    friend_sets = [set(a) for a in adj]
    nb = len(BIN_LABELS)
    out = {"q": q, "seed": seed, "pairs": len(pairs),
           "mean_degree": sum(len(a) for a in adj) / N, "h": {}}
    for h in hops:
        th = time.time()
        depth, parent, waiting, relays = first_following(adj, N, pairs, h)
        sig = None
        if h == 1:
            sig = [shorten_by_signatures(adj, friend_sets, b, parent[b], depth[b]) for b in range(N)]
        early, refollows, early_passes = shorten_earlier_rule(adj, N, depth, parent, rng)
        res = {k: [[] for _ in BIN_LABELS] for k in KEYS}
        rat = {k: [[] for _ in BIN_LABELS] for k in KEYS}
        dist = [[] for _ in BIN_LABELS]
        cnt = {k: [0] * nb for k in COUNTERS}
        posts_by = [[] for _ in BIN_LABELS]
        relay_persons = set()
        for b in range(N):
            k = counts[b]
            bi = bin_of(k)
            db = depth[b]
            fol = [f for f in followers[b] if f in db]
            ra = shorten_by_reasking(adj, b, parent[b], db)
            cnt["authors"][bi] += 1
            cnt["deciders"][bi] += k
            cnt["waits"][bi] += len(waiting[b])
            cnt["following"][bi] += len(fol)
            cnt["relay_followings"][bi] += len(relays[b])
            relay_persons |= relays[b]
            cnt["holders"][bi] += len(set(fol) | relays[b])
            cnt["holders_all"][bi] += len(db) - 1
            cnt["reask_switches"][bi] += ra[1]
            cnt["asks"][bi] += ra[2]
            cnt["refollows"][bi] += refollows[b]
            if sig is not None:
                cnt["sig_switches"][bi] += sig[b][1]
                cnt["posts"][bi] += sig[b][2]
                posts_by[bi].append(sig[b][2])
            if not fol:
                continue
            gd = bfs_targets(adj, b, fol)
            sub = bfs_within(adj, b, db)
            paths = {"first": db, "reask": ra[0], "early": early[b], "sub": sub}
            if sig is not None:
                paths["sig"] = sig[b][0]
            for f in fol:
                g = gd[f]
                dist[bi].append(g)
                for key, pd in paths.items():
                    res[key][bi].append(pd[f])
                    rat[key][bi].append(pd[f] / g)
        out["h"][h] = {"res": res, "rat": rat, "dist": dist, "cnt": cnt,
                       "posts_by": posts_by, "relay_persons": len(relay_persons),
                       "early_passes": early_passes, "seconds": time.time() - th}
    out["seconds"] = time.time() - t0
    return out


def stats(xs, ratio=False):
    if not xs:
        return "-", "-", "-"
    f = "{:.3f}" if ratio else "{:.2f}"
    med = statistics.median(xs)
    mx = max(xs)
    return (f.format(sum(xs) / len(xs)),
            f.format(med) if ratio or med != int(med) else str(int(med)),
            f.format(mx) if ratio else str(mx))


def hist(xs):
    h = {}
    for x in xs:
        h[x] = h.get(x, 0) + 1
    return " ".join("{}:{}".format(k, h[k]) for k in sorted(h))


def frac(a, b, fmt="{:.4f}"):
    return fmt.format(a / b) if b else "-"


TITLES = {
    "first": "First following (Algorithms pragma: shortest path through a friend who follows{})",
    "sig": "After shortening by the signatures of the items received (item 4)",
    "reask": "After shortening by re-asking",
    "early": "After the earlier shortening rule, a friend shorter by two or more (comparison)",
    "sub": "Shortest-path tree over the followers' subgraph (the author and all following at the end of the first following, relays included)",
}


def section(runs, h, lines):
    nb = len(BIN_LABELS)
    H = [r["h"][h] for r in runs]

    def merged(key, bi, ratio=False):
        out = []
        for x in H:
            out.extend((x["rat"] if ratio else x["res"])[key][bi])
        return out

    def c(name, bi=None):
        if bi is None:
            return sum(x["cnt"][name][i] for x in H for i in range(nb))
        return sum(x["cnt"][name][bi] for x in H)

    disc = "" if h == 1 else "; discovery {} hops deep".format(h)
    keys = ["first"] + (["sig"] if h == 1 else []) + ["reask", "early", "sub"]
    for key in keys:
        lines.append("### " + (TITLES[key].format(disc) if key == "first" else TITLES[key]))
        lines.append("")
        head = "| followers | authors | pairs | following |"
        sep = "|---|---:|---:|---:|"
        if key == "first":
            head += " waiting | relay followings | per author | holders per decided follower | incl. author's friends |"
            sep += "---:|---:|---:|---:|---:|"
        if key == "sig":
            head += " switches | posts per author mean | max |"
            sep += "---:|---:|---:|"
        if key == "reask":
            head += " switches | asks | asks per decided follower |"
            sep += "---:|---:|---:|"
        if key == "early":
            head += " re-followings |"
            sep += "---:|"
        head += " path mean | median | max | ratio mean | median | max |"
        sep += "---:|---:|---:|---:|---:|---:|"
        lines += [head, sep]
        for bi in list(range(nb)) + [None]:
            if bi is None:
                xs = [v for i in range(nb) for v in merged(key, i)]
                rs = [v for i in range(nb) for v in merged(key, i, True)]
                ps = [v for x in H for i in range(nb) for v in x["posts_by"][i]]
                label = "all"
            else:
                xs, rs = merged(key, bi), merged(key, bi, True)
                ps = [v for x in H for v in x["posts_by"][bi]]
                label = BIN_LABELS[bi]
            row = "| {} | {} | {} | {} |".format(label, c("authors", bi), c("deciders", bi), len(xs))
            if key == "first":
                row += " {} | {} | {} | {} | {} |".format(
                    frac(c("waits", bi), c("deciders", bi)), c("relay_followings", bi),
                    frac(c("relay_followings", bi), c("authors", bi), "{:.2f}"),
                    frac(c("holders", bi), c("following", bi), "{:.3f}"),
                    frac(c("holders_all", bi), c("following", bi), "{:.3f}"))
            if key == "sig":
                row += " {} | {} | {} |".format(c("sig_switches", bi),
                                                frac(sum(ps), len(ps), "{:.2f}"), max(ps) if ps else "-")
            if key == "reask":
                row += " {} | {} | {} |".format(c("reask_switches", bi), c("asks", bi),
                                                frac(c("asks", bi), c("following", bi), "{:.1f}"))
            if key == "early":
                row += " {} |".format(c("refollows", bi))
            row += " {} | {} | {} |".format(*stats(xs))
            row += " {} | {} | {} |".format(*stats(rs, True))
            lines.append(row)
        lines.append("")
    lines.append("### Graph distance from follower to author (the shortest-path tree over the whole friendship graph; ratio 1)")
    lines.append("")
    lines += ["| followers | following | mean | median | max |", "|---|---:|---:|---:|---:|"]
    for bi in range(nb):
        xs = [v for x in H for v in x["dist"][bi]]
        lines.append("| {} | {} | {} | {} | {} |".format(BIN_LABELS[bi], len(xs), *stats(xs)))
    lines.append("")
    lines.append("### Distribution of path length (length:count)")
    lines.append("")
    lines += ["| followers | measure | length:count |", "|---|---|---|"]
    names = {"first": "first following", "sig": "after shortening by signatures",
             "reask": "after shortening by re-asking", "early": "after the earlier rule",
             "sub": "tree, followers' subgraph"}
    for bi in range(nb):
        for key in keys:
            xs = merged(key, bi)
            if xs:
                lines.append("| {} | {} | {} |".format(BIN_LABELS[bi], names[key], hist(xs)))
        xs = [v for x in H for v in x["dist"][bi]]
        if xs:
            lines.append("| {} | graph distance | {} |".format(BIN_LABELS[bi], hist(xs)))
    lines.append("")


def write_q(q, runs, outdir, hops):
    runs = sorted(runs, key=lambda r: r["seed"])
    seeds = [r["seed"] for r in runs]
    lines = ["# GFF path lengths, q = {}".format(q), ""]
    lines.append("Kleinberg's {0} x {0} grid, {1} long-range friendship(s) per person; seeds {2}, pooled; "
                 "produced by programs/gff/paths/paths.py (see README.md).".format(N_SIDE, q, ", ".join(map(str, seeds))))
    lines.append("Path and ratio are over the deciders who follow; ratio is path length over graph distance; "
                 "waiting is the fraction of deciders still waiting at the end; holders per decided follower is "
                 "the decided followers who follow and the relays, over the decided followers who follow, and "
                 "incl. author's friends adds the author's friends, who follow by friendship.")
    lines.append("")
    for h in hops:
        lines.append("## Discovery {} hop{} deep, seeds pooled".format(h, "" if h == 1 else "s"))
        lines.append("")
        section(runs, h, lines)
    lines.append("## Per seed")
    lines.append("")
    lines += ["| seed | hops | followers | pairs | waiting | first: path mean / max, ratio mean / max | re-asking: path mean / max, ratio mean / max | switches | asks | relay followings |",
              "|---:|---:|---|---:|---:|---|---|---:|---:|---:|"]
    for r in runs:
        for h in hops:
            x = r["h"][h]
            for bi in range(len(BIN_LABELS)):
                if not x["cnt"]["deciders"][bi]:
                    continue
                cells = []
                for key in ("first", "reask"):
                    pm, _, px = stats(x["res"][key][bi])
                    rm, _, rx = stats(x["rat"][key][bi], True)
                    cells.append("{} / {}, {} / {}".format(pm, px, rm, rx))
                cn = x["cnt"]
                lines.append("| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |".format(
                    r["seed"], h, BIN_LABELS[bi], cn["deciders"][bi],
                    frac(cn["waits"][bi], cn["deciders"][bi]), cells[0], cells[1],
                    cn["reask_switches"][bi], cn["asks"][bi], cn["relay_followings"][bi]))
    lines.append("")
    lines += ["| seed | hops | author-follower pairs | mean friends per person | relay followers (persons) | relay followings | passes of the earlier rule | run time (s) |",
              "|---:|---:|---:|---:|---:|---:|---:|---:|"]
    for r in runs:
        for h in hops:
            x = r["h"][h]
            lines.append("| {} | {} | {} | {:.3f} | {} | {} | {} | {:.0f} |".format(
                r["seed"], h, r["pairs"], r["mean_degree"], x["relay_persons"],
                sum(x["cnt"]["relay_followings"]), x["early_passes"], x["seconds"]))
    lines.append("")
    os.makedirs(outdir, exist_ok=True)
    with open(os.path.join(outdir, "q{}.md".format(q)), "w") as fh:
        fh.write("\n".join(lines))


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--q", type=int, nargs="+", default=[1, 2, 4])
    ap.add_argument("--seeds", type=int, nargs="+", default=[1, 2, 3, 4, 5])
    ap.add_argument("--hops", type=int, nargs="+", default=list(HOPS), help="depths of discovery")
    ap.add_argument("--n", type=int, default=N_SIDE)
    ap.add_argument("--jobs", type=int, default=1, help="runs in parallel (processes)")
    ap.add_argument("--out", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "tables"))
    a = ap.parse_args()
    set_side(a.n)
    t0 = time.time()
    hops = sorted(a.hops)
    tasks = [(q, s, a.n, hops) for q in a.q for s in a.seeds]
    if a.jobs > 1:
        with Pool(a.jobs) as pool:
            results = pool.map(run, tasks)
    else:
        results = []
        for t in tasks:
            results.append(run(t))
            r = results[-1]
            print("q={} seed={} pairs={} {}  total {:.0f}s".format(
                r["q"], r["seed"], r["pairs"],
                " ".join("h{}={:.0f}s".format(h, r["h"][h]["seconds"]) for h in hops),
                r["seconds"]), flush=True)
    for q in a.q:
        write_q(q, [r for r in results if r["q"] == q], a.out, hops)
    print("total {:.0f}s".format(time.time() - t0))


if __name__ == "__main__":
    main()
