#!/usr/bin/env python3
"""Path lengths under GFF: following through a friend on Kleinberg's grid.

GFF #1's task (Code_inbox.md, 2026-10-27 18:52 UTC, items 1-3; item 4 sent
whole at 21:30 UTC).  Python 3 standard library only; written and run with
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

PATH SHORTENING (item 4, a second Algorithms pragma).  After every decider has
had their turn: repeatedly, a follower whose friend has a path to the author
shorter by two or more than their own ends their following and follows
through that friend (the friend with the shortest path, ties as above); a
follower's path length is one more than that of the friend they follow
through, so the lengths of all who follow through them, directly or not, fall
with it.  The followers are swept in a random order, a new order per pass,
until a pass makes no change.  The count of such re-followings is reported.
Waiting deciders stay waiting; the author's friends stay at 1.

THE SHORTEST-PATH TREES (item 4, for comparison).  (a) Over the followers'
subgraph: the friendship graph restricted to the author and those following
the author at the end of the first following (the deciders who followed and
the author's friends); a follower's path is their BFS distance to the author
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
shortening passes.  q in 1, 2, 4; s in 1 .. 5.
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


def run(args):
    q, seed, n = args
    t0 = time.time()
    rng = random.Random(seed)
    N = n * n
    adj = build_graph(rng, n, q)
    counts, followers = draw_followers(rng, N)

    # The first following.  depth[b][p]: p's path length to author b;
    # parent[b][p]: the friend p follows through (None for the author).
    depth = [None] * N
    parent = [None] * N
    for b in range(N):
        depth[b] = {b: 0}
        parent[b] = {b: None}
        for f in adj[b]:
            depth[b][f] = 1
            parent[b][f] = b
    waiting = [set() for _ in range(N)]

    def follow(p, b):
        db = depth[b]
        best, via = None, None
        for g in adj[p]:
            dg = db.get(g)
            if dg is not None and (best is None or dg < best):
                best, via = dg, g
        if best is None:
            return False
        db[p] = best + 1
        parent[b][p] = via
        w = waiting[b]
        work = deque()
        x = p
        while True:
            # x has just come to follow b: its waiting friends join the queue
            if w:
                for g in adj[x]:
                    if g in w:
                        w.discard(g)
                        work.append(g)
            if not work:
                break
            x = work.popleft()
            # x, taken from the queue, follows through its friend with the shortest path now
            bx, vx = None, None
            for h in adj[x]:
                dh = db.get(h)
                if dh is not None and (bx is None or dh < bx):
                    bx, vx = dh, h
            db[x] = bx + 1
            parent[b][x] = vx
        return True

    pairs = [(f, b) for b in range(N) for f in followers[b]]
    rng.shuffle(pairs)
    for f, b in pairs:
        if f in depth[b]:
            continue
        if not follow(f, b):
            waiting[b].add(f)

    first = [dict(depth[b]) for b in range(N)]

    # Path shortening.
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
            break

    # Measures.
    res = {key: [[] for _ in BIN_LABELS] for key in
           ("first", "rfirst", "short", "rshort", "sub", "rsub", "dist")}
    deciders = [0] * len(BIN_LABELS)
    waits = [0] * len(BIN_LABELS)
    authors = [0] * len(BIN_LABELS)
    refollows = [0] * len(BIN_LABELS)
    for b in range(N):
        k = counts[b]
        bi = bin_of(k)
        authors[bi] += 1
        deciders[bi] += k
        waits[bi] += len(waiting[b])
        refollows[bi] += refollow_by_author[b]
        fol = [f for f in followers[b] if f in first[b]]
        if not fol:
            continue
        gd = bfs_targets(adj, b, fol)
        sub = bfs_within(adj, b, first[b])
        for f in fol:
            g = gd[f]
            res["dist"][bi].append(g)
            res["first"][bi].append(first[b][f])
            res["rfirst"][bi].append(first[b][f] / g)
            res["short"][bi].append(depth[b][f])
            res["rshort"][bi].append(depth[b][f] / g)
            res["sub"][bi].append(sub[f])
            res["rsub"][bi].append(sub[f] / g)
    degs = [len(a) for a in adj]
    return {
        "q": q, "seed": seed, "res": res, "deciders": deciders,
        "waits": waits, "authors": authors, "refollows": refollows,
        "passes": passes, "seconds": time.time() - t0,
        "pairs": len(pairs), "mean_degree": sum(degs) / len(degs),
    }


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


MEASURES = [
    ("first", "rfirst", "First following (Algorithms pragma: shortest path through a friend who follows)"),
    ("short", "rshort", "After path shortening"),
    ("sub", "rsub", "Shortest-path tree over the followers' subgraph (the author and those following at the end of the first following)"),
]


def table(runs, lines, pooled_label):
    """Append the tables of one q to lines; runs: list of run results."""
    def merged(key, bi):
        out = []
        for r in runs:
            out.extend(r["res"][key][bi])
        return out

    nb = len(BIN_LABELS)
    A = [sum(r["authors"][bi] for r in runs) for bi in range(nb)]
    D = [sum(r["deciders"][bi] for r in runs) for bi in range(nb)]
    W = [sum(r["waits"][bi] for r in runs) for bi in range(nb)]
    R = [sum(r["refollows"][bi] for r in runs) for bi in range(nb)]
    for key, rkey, title in MEASURES:
        lines.append("### " + title + ", " + pooled_label)
        lines.append("")
        head = "| followers | authors | pairs | following |"
        sep = "|---|---:|---:|---:|"
        if key == "first":
            head += " waiting |"
            sep += "---:|"
        if key == "short":
            head += " re-followings |"
            sep += "---:|"
        head += " path mean | median | max | ratio mean | median | max |"
        sep += "---:|---:|---:|---:|---:|---:|"
        lines += [head, sep]
        for bi in range(nb):
            xs, rs = merged(key, bi), merged(rkey, bi)
            row = "| {} | {} | {} | {} |".format(BIN_LABELS[bi], A[bi], D[bi], len(xs))
            if key == "first":
                row += " {} |".format("{:.4f}".format(W[bi] / D[bi]) if D[bi] else "-")
            if key == "short":
                row += " {} |".format(R[bi])
            row += " {} | {} | {} |".format(*stats(xs))
            row += " {} | {} | {} |".format(*stats(rs, True))
            lines.append(row)
        xs = [x for bi in range(nb) for x in merged(key, bi)]
        rs = [x for bi in range(nb) for x in merged(rkey, bi)]
        row = "| all | {} | {} | {} |".format(sum(A), sum(D), len(xs))
        if key == "first":
            row += " {:.4f} |".format(sum(W) / sum(D))
        if key == "short":
            row += " {} |".format(sum(R))
        row += " {} | {} | {} |".format(*stats(xs))
        row += " {} | {} | {} |".format(*stats(rs, True))
        lines.append(row)
        lines.append("")
    lines.append("### Graph distance from follower to author (the shortest-path tree over the whole friendship graph; ratio 1), " + pooled_label)
    lines.append("")
    lines += ["| followers | following | mean | median | max |", "|---|---:|---:|---:|---:|"]
    for bi in range(nb):
        xs = merged("dist", bi)
        lines.append("| {} | {} | {} | {} | {} |".format(BIN_LABELS[bi], len(xs), *stats(xs)))
    lines.append("")
    return merged


def write_q(q, runs, outdir):
    runs = sorted(runs, key=lambda r: r["seed"])
    seeds = [r["seed"] for r in runs]
    lines = ["# GFF path lengths, q = {}".format(q), ""]
    lines.append("Kleinberg's {0} x {0} grid, {1} long-range friendship(s) per person; seeds {2}, pooled; "
                 "produced by programs/gff/paths/paths.py (see README.md).".format(N_SIDE, q, ", ".join(map(str, seeds))))
    lines.append("Path and ratio are over the deciders who follow; ratio is path length over graph distance; waiting is the fraction of deciders still waiting at the end.")
    lines.append("")
    merged = table(runs, lines, "seeds pooled")
    lines.append("### Distribution of path length, seeds pooled (length:count)")
    lines.append("")
    lines += ["| followers | measure | length:count |", "|---|---|---|"]
    for bi in range(len(BIN_LABELS)):
        for key, name in (("first", "first following"), ("short", "after shortening"),
                          ("sub", "tree, followers' subgraph"), ("dist", "graph distance")):
            xs = merged(key, bi)
            if xs:
                lines.append("| {} | {} | {} |".format(BIN_LABELS[bi], name, hist(xs)))
    lines.append("")
    lines.append("## Per seed")
    lines.append("")
    lines += ["| seed | followers | pairs | waiting | first: path mean / max, ratio mean / max | shortened: path mean / max, ratio mean / max | re-followings | tree over followers: path mean / max, ratio mean / max |",
              "|---:|---|---:|---:|---|---|---:|---|"]
    for r in runs:
        for bi in range(len(BIN_LABELS)):
            if not r["deciders"][bi]:
                continue
            cells = []
            for key, rkey in (("first", "rfirst"), ("short", "rshort"), ("sub", "rsub")):
                pm, _, px = stats(r["res"][key][bi])
                rm, _, rx = stats(r["res"][rkey][bi], True)
                cells.append("{} / {}, {} / {}".format(pm, px, rm, rx))
            lines.append("| {} | {} | {} | {:.4f} | {} | {} | {} | {} |".format(
                r["seed"], BIN_LABELS[bi], r["deciders"][bi], r["waits"][bi] / r["deciders"][bi],
                cells[0], cells[1], r["refollows"][bi], cells[2]))
    lines.append("")
    lines += ["| seed | author-follower pairs | mean friends per person | shortening passes | run time (s) |",
              "|---:|---:|---:|---:|---:|"]
    for r in runs:
        lines.append("| {} | {} | {:.3f} | {} | {:.0f} |".format(
            r["seed"], r["pairs"], r["mean_degree"], r["passes"], r["seconds"]))
    lines.append("")
    os.makedirs(outdir, exist_ok=True)
    with open(os.path.join(outdir, "q{}.md".format(q)), "w") as fh:
        fh.write("\n".join(lines))


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--q", type=int, nargs="+", default=[1, 2, 4])
    ap.add_argument("--seeds", type=int, nargs="+", default=[1, 2, 3, 4, 5])
    ap.add_argument("--n", type=int, default=N_SIDE)
    ap.add_argument("--jobs", type=int, default=1, help="runs in parallel (processes)")
    ap.add_argument("--out", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "tables"))
    a = ap.parse_args()
    set_side(a.n)
    t0 = time.time()
    tasks = [(q, s, a.n) for q in a.q for s in a.seeds]
    if a.jobs > 1:
        with Pool(a.jobs) as pool:
            results = pool.map(run, tasks)
    else:
        results = [run(t) for t in tasks]
    for r in results:
        print("q={} seed={} pairs={} passes={} {:.0f}s".format(
            r["q"], r["seed"], r["pairs"], r["passes"], r["seconds"]), flush=True)
    for q in a.q:
        write_q(q, [r for r in results if r["q"] == q], a.out)
    print("total {:.0f}s".format(time.time() - t0))


if __name__ == "__main__":
    main()
