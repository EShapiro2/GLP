#!/usr/bin/env python3
"""Path lengths under GFF-I and GFF-PS: following through friends on Kleinberg's grid.

Updated 2026-11-07 to the contracts in programs/gff at d37f58f7: GDO-L, GFF,
GFF-I and GFF-PS (their .txt clauses and .glp schemas).  The model of
2026-10-27 (GFF's Algorithms pragmas, discovery two and three hops deep through
relays, shortening by signatures and by re-asking, the earlier shortening rule)
is in git history, last at 095c9cca.  Python 3 standard library only; run with
/usr/bin/python3, Python 3.9.6.  Run:  python3 paths.py   (see README.md).

THE GRAPH.  Unchanged.  N = n*n persons, n = 100, on Kleinberg's n x n grid
(Kleinberg 2000), not a torus: person (i, j), 0 <= i, j < n, and lattice
distance d((i,j),(k,l)) = |i-k| + |j-l|.  Each person is a friend of their
grid neighbours (four inside the grid, three on an edge, two at a corner) and
makes q long-range friendships.  Person u, taken in index order u = i*n + j,
draws a long-range contact v with probability
        P(v) = d(u,v)^-2 / sum_{w != u} d(u,w)^-2      (Kleinberg's r = 2),
and redraws while v is already a friend of u, until u has made q new
friendships.  Friendship is symmetric.  The draw is exact, by rejection: d is
drawn with P(d) proportional to 1/d for d = 1 .. 2(n-1), a point uniformly
among the 4d lattice points at distance d, redrawn while outside the grid.

OTHER GRAPHS (added 2026-11-08; --graph, --r, --d).  The default above is
unchanged, draw for draw.  (a) Kleinberg's grid with another exponent r:
P(v) proportional to d(u,v)^-r, so P(d) proportional to d^(1-r); r = 0 draws
long-range contacts uniformly from the grid.  Kleinberg's grid at r = 2 is not
an expander (the cut between the grid's halves is crossed by about n edges,
so its conductance falls like 1/n); at r = 0 it is one with high probability.
(b) A random d-regular graph on N = n*n persons (default d = 6, the mean
number of friends of the grid at q = 1): the configuration model, each
person's d slots paired uniformly at random, with every loop or repeated pair
removed by a random double-edge swap.  An expander with high probability
(Friedman: its second eigenvalue is near 2*sqrt(d-1)/d).  It has no geometry:
the grid position of person i*n + j means nothing there, and "first" in the
friend list is a random friend.  graph_stats measures any graph: the spectral
gap of the random walk (an upper bound, by power iteration), the conductance
of the cut between the grid's halves, and the mean graph distance.

THE FOLLOWERS.  Unchanged.  Every person is an author.  An author's number of
deciders k (persons who come to want to follow the author) is drawn from the
discrete power law P(k) = k^-2 / zeta(2), capped at N - 1; the k deciders are
drawn uniformly without replacement from the other persons.

KNOWING.  Friends know each other: befriend (GSG:1) requires it.  Beyond
friendship, knowing comes only from introductions (GKK), modelled under
SHORTENING below; acquaintance is not modelled.

FRIEND POINTERS (GDO-L's preamble; befriend in GDO-L.glp).  Friends Alice and
Dave have a Dave-pointer from Alice to Dave, recorded by befriend.  So every
friend of an author follows the author's feed at path length 1 without
deciding, and the author is at 0.  unfollow excludes these pointers.

THE FIRST FOLLOWING (GDO-L:1, follow_friend, willed by the follower; kept by
GFF-I).  A decider Alice who wants to follow Dave adds a Dave-pointer to a
friend Bob who has a Dave-pointer (to anyone).  A decider who is Dave's friend
already follows.  The contract leaves open which Bob; the strategies:
  random    uniformly among Alice's friends who have a Dave-pointer;
  first     the first of them in Alice's friend list (grid neighbours first);
  shortest  the one with the shortest path to Dave (the old Algorithms
            pragma), ties to the first in the friend list.  An oracle: no
            agent's state holds path lengths and no clause lets Alice learn
            them.  Kept as a baseline; it reproduces the old tables' first
            following one hop deep exactly.
All author-decider pairs decide in one random order.  A decider none of whose
friends has a Dave-pointer waits (the volition stands until the act is
enabled).  When a person comes to have a Dave-pointer, every friend of theirs
waiting on Dave joins a FIFO queue and, taken from it, follows by the strategy
then; the queue runs out before the next decider decides.
There are no relays: following on someone else's behalf is no act of the
contracts (GDO-L:1 and GDO-L:2 are willed by the one who points), so the old
discovery two and three hops deep is gone.  Transitive pointing (GDO-L:2,
willed by Alice) is not modelled apart from GFF-PS's shorten.

SHORTENING (GFF-PS:1, shorten, no guarding role).  Alice, who has a
Dave-pointer to Bob and none to Carol, while Bob has a Dave-pointer to Carol,
replaces her pointer to Bob by one to Carol, if Alice knows Carol and Carol
knows Alice.  A friend of Dave never shortens: Dave has no Dave-pointer.
Unless Carol is Alice's friend, the knowing needs introductions (GKK): Bob
introduces Carol to Alice (Bob and Alice willing) and Alice to Carol (Bob and
Carol willing).  Bob can, since in this model a pointer from Alice to Bob
implies that each knows the other: friend pointers and follow_friend are
between friends, and shorten requires knowing both ways.  The model: each
introduction needed is taken with probability p (those who must will it do),
independently, and once refused (Carol to Alice, or Alice to Carol) is not
asked again.  p = 0: knowing is friendship.  p = 1: everyone is willing.
Followers are taken in a random order, a new order per pass, until a pass
changes nothing.  Counted: shortenings, introductions taken and refused.

Switching by arrival order (a follower trying a second friend and keeping
whichever pointer delivers first), which the version of 2026-11-09 had, is no
act of any contract and is gone; git holds it, last at 5fc93257.  GFF-FR and
GFF-FR-BC, which need time and faults, are in async_sim.py.

BASELINES.  (a) The shortest-path tree over the followers' subgraph: the
friendship graph restricted to the author and all who follow at the end of the
first following (the deciders who follow and the author's friends); the same
for every strategy.  (b) Graph distance in the friendship graph (ratio 1).
With introductions, a path can be shorter than the graph distance: ratio < 1.

MEASURES.  Over author-decider pairs, in bins by the author's number of
deciders k (1-9, 10-99, 100-999, 1000-9999, 10000; the last empty): the path
length of each decider who follows (count by length; mean, median, max), its
ratio to the graph distance, the fraction of deciders waiting, the holders of
a feed (all who follow it, the author's friends included) per decided
follower, and the author's fan-in, the persons with a Dave-pointer to Dave
(the author's friends included), mean and max per author.  After shortening
also: shortenings and introductions per decided follower who follows.

SEEDS.  A run (q, s) draws from random.Random(s), in this order: the graph, the
deciders, the order of deciding, then one integer from which each strategy's
generator (first following) and each (strategy, p)'s generator (shortening)
are seeded.  The graph, deciders and order are those of the old paths.py.
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


def build_graph(rng, n, q, r=2):
    """Kleinberg's grid with q long-range friendships per person, a contact at
    lattice distance d drawn with probability proportional to d^-r.  r = 2
    (the default) draws exactly as before, so its graphs and tables are
    unchanged; r = 0 draws contacts uniformly from the grid."""
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
    # 4d lattice points at distance d, each of weight d^-r: P(d) proportional to d^(1-r)
    if r == 2:
        dcum = cumulative([1.0 / d for d in range(1, dmax + 1)])
    else:
        dcum = cumulative([float(d) ** (1 - r) for d in range(1, dmax + 1)])
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


def build_regular(rng, N, d):
    """A random d-regular graph on N persons: each person's d friendship
    slots are paired uniformly at random (the configuration model), then each
    loop or repeated pair is removed by a random double-edge swap with another
    pair.  An expander with high probability for d >= 3 (Friedman 2008)."""
    if d >= N or (N * d) % 2:
        raise ValueError("a d-regular graph on N persons needs d < N and N*d even")
    stubs = [v for v in range(N) for _ in range(d)]
    rng.shuffle(stubs)
    edges = [[stubs[i], stubs[i + 1]] for i in range(0, len(stubs), 2)]
    key = lambda a, b: (a, b) if a < b else (b, a)
    count = {}
    for a, b in edges:
        count[key(a, b)] = count.get(key(a, b), 0) + 1
    bad = lambda e: e[0] == e[1] or count[key(*e)] > 1
    todo = [i for i, e in enumerate(edges) if bad(e)]
    while todo:
        i = todo.pop()
        if not bad(edges[i]):
            continue
        while True:
            j = rng.randrange(len(edges))
            if j == i:
                continue
            (a, b), (c, e) = edges[i], edges[j]
            if rng.random() < 0.5:
                c, e = e, c
            if a == c or b == e or count.get(key(a, c), 0) or count.get(key(b, e), 0) or key(a, c) == key(b, e):
                continue
            for x, y in ((a, b), (edges[j][0], edges[j][1])):
                count[key(x, y)] -= 1
            edges[i], edges[j] = [a, c], [b, e]
            count[key(a, c)] = count.get(key(a, c), 0) + 1
            count[key(b, e)] = count.get(key(b, e), 0) + 1
            break
    adj = [[] for _ in range(N)]
    for a, b in edges:
        adj[a].append(b)
        adj[b].append(a)
    return adj


GRAPHS = ("kleinberg", "regular")
D_REGULAR = 6   # the default degree: the mean number of friends of Kleinberg's grid at q = 1


def graph_spec(kind="kleinberg", q=1, r=2, d=D_REGULAR):
    return {"kind": "kleinberg", "q": q, "r": r} if kind == "kleinberg" else {"kind": "regular", "d": d}


def make_graph(rng, n, g):
    """The friendship graph for spec g, on N = n*n persons."""
    if g["kind"] == "kleinberg":
        return build_graph(rng, n, g["q"], g.get("r", 2))
    return build_regular(rng, n * n, g["d"])


def graph_label(g):
    """Short name for files and keys; Kleinberg at r = 2 keeps its old name."""
    if g["kind"] == "regular":
        return "regular-d{}".format(g["d"])
    r = g.get("r", 2)
    return "q{}".format(g["q"]) if r == 2 else "r{:g}-q{}".format(r, g["q"])


def graph_title(g):
    """The tables' title: q (and r, if not 2) for Kleinberg's grid, d for the regular graph."""
    if g["kind"] == "regular":
        return "random {}-regular graph".format(g["d"])
    r = g.get("r", 2)
    return "q = {}".format(g["q"]) if r == 2 else "q = {}, r = {:g}".format(g["q"], r)


def graph_text(g, n):
    if g["kind"] == "regular":
        return "a random {}-regular graph on {} persons (an expander)".format(g["d"], n * n)
    return "Kleinberg's {0} x {0} grid, {1} long-range friendship(s) per person, exponent r = {2:g}".format(
        n, g["q"], g.get("r", 2))


def spectral_gap(adj, rng, iters=400):
    """1 - lambda_2 of the random walk on the graph, lambda_2 its second largest
    eigenvalue, by power iteration on the lazy walk with the top eigenvector
    projected out.  The Rayleigh quotient approaches lambda_2 from below, so the
    estimate is an upper bound on the gap: close for an expander, loose for a
    graph whose gap is tiny (a grid), where it still shows the order."""
    N = len(adj)
    deg = [len(a) for a in adj]
    sq = [math.sqrt(x) for x in deg]
    norm = math.sqrt(sum(deg))
    u = [x / norm for x in sq]
    x = [rng.random() - 0.5 for _ in range(N)]

    def project(v):
        c = sum(a * b for a, b in zip(u, v))
        v = [a - c * b for a, b in zip(v, u)]
        m = math.sqrt(sum(a * a for a in v)) or 1.0
        return [a / m for a in v]

    def S(v):   # D^-1/2 A D^-1/2 v
        w = [v[i] / sq[i] if sq[i] else 0.0 for i in range(N)]
        return [sum(w[j] for j in adj[i]) / sq[i] if sq[i] else 0.0 for i in range(N)]

    x = project(x)
    lam = 0.0
    for _ in range(iters):
        sx = S(x)
        mx = [(a + b) / 2 for a, b in zip(x, sx)]   # the lazy walk
        lam = sum(a * b for a, b in zip(x, mx))
        x = project(mx)
    return 1 - (2 * lam - 1)


def graph_stats(adj, n, rng, sources=60, iters=400):
    """Expansion and distance measures of a graph: the spectral gap (an upper
    bound, see spectral_gap), the conductance of the cut between the left and
    right halves of the grid by index (an upper bound on the graph's
    conductance; by Cheeger, gap <= 2 * conductance), the mean number of
    friends, and the mean and largest graph distance from sampled persons."""
    N = len(adj)
    left = [(v % n) < n // 2 for v in range(N)]
    cross = sum(1 for v in range(N) for w in adj[v] if left[v] and not left[w])
    vol = sum(len(adj[v]) for v in range(N) if left[v])
    dist, far = [], 0
    for s in rng.sample(range(N), min(sources, N)):
        dd = bfs_targets(adj, s, range(N))
        dist += [x for v, x in dd.items() if v != s]
        far = max(far, max(dd.values()))
    return {"gap": spectral_gap(adj, rng, iters), "cut_conductance": cross / vol if vol else float("nan"),
            "mean_degree": sum(len(a) for a in adj) / N, "distance": sum(dist) / len(dist), "diameter_est": far}


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



STRATEGIES = ("random", "first", "shortest")
P_INTRO = (0.0, 0.25, 0.5, 1.0)


def first_following(adj, N, pairs, strategy, rng, log=None):
    """GDO-L:1 (follow_friend) for every decider, by the strategy.  Returns,
    per author, the depth map (path length of everyone following), the parent
    map (each follower -> the person their Dave-pointer points to; the author
    -> None) and the waiting deciders.  If log is given, it is called with
    (author, event) for each act, in order: ("f", a, via, woken) when a
    follows through via (woken: a waited), ("w", a) when a starts waiting
    (live.py's events; nothing else changes)."""
    depth = [None] * N
    parent = [None] * N
    for b in range(N):
        depth[b] = {b: 0}
        parent[b] = {b: None}
        for f in adj[b]:  # friend pointers
            depth[b][f] = 1
            parent[b][f] = b
    waiting = [set() for _ in range(N)]

    def choose(a, db):
        cands = [c for c in adj[a] if c in db]  # friends with a Dave-pointer
        if not cands:
            return None
        if strategy == "random":
            return rng.choice(cands)
        if strategy == "first":
            return cands[0]
        best = cands[0]  # shortest: the oracle baseline
        for c in cands[1:]:
            if db[c] < db[best]:
                best = c
        return best

    def come(a, via, b):
        db, pb, w = depth[b], parent[b], waiting[b]
        work = deque()
        woken = 0
        while True:
            db[a] = db[via] + 1
            pb[a] = via
            if log is not None:
                log(b, ("f", a, via, woken))
            woken = 1
            if w:
                for v in adj[a]:
                    if v in w:
                        w.discard(v)
                        work.append(v)
            if not work:
                return
            a = work.popleft()
            via = choose(a, db)  # a friend has just come to have a pointer

    for f, b in pairs:
        db = depth[b]
        if f in db:  # a friend of the author already follows
            continue
        via = choose(f, db)
        if via is None:
            waiting[b].add(f)
            if log is not None:
                log(b, ("w", f))
        else:
            come(f, via, b)
    return depth, parent, waiting


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


def shorten(friend_sets, b, par0, p, rng, log=None):
    """GFF-PS:1 (shorten) to a fixpoint for author b, introductions taken with
    probability p.  A worklist: the followers not pointing to the author, in a
    random order; one who shortens is tried again, and so is everyone pointing
    to them, whose Carol has changed.  Returns (parent map, shortenings,
    introductions taken, introductions refused).  If log is given, it is
    called with ("s", a, carol, intros, friend) for each shortening (intros:
    introductions taken for it; friend: Carol is a's friend) and ("r", a,
    carol, intros) for each refusal (intros: taken before it; live.py's events; nothing else changes)."""
    par = dict(par0)
    children = {}
    for a, x in par.items():
        if x is not None:
            children.setdefault(x, set()).add(a)
    movers = [a for a, x in par.items() if x is not None and x != b]
    rng.shuffle(movers)
    work = deque(movers)
    queued = set(movers)
    known = set()    # (x, y): x knows y by an introduction
    refused = set()  # (x, y): the introduction of y to x was refused
    shortens = intros = refusals = 0
    while work:
        a = work.popleft()
        queued.discard(a)
        bob = par[a]
        if bob == b:
            continue
        carol = par[bob]  # Bob's Dave-pointer
        ok = True
        took = 0
        for x, y in ((a, carol), (carol, a)):
            if y in friend_sets[x] or (x, y) in known:
                continue
            if p <= 0 or (x, y) in refused:
                ok = False
                break
            if p >= 1 or rng.random() < p:
                known.add((x, y))
                intros += 1
                took += 1
            else:
                refused.add((x, y))
                refusals += 1
                ok = False
                if log is not None:
                    log(("r", a, carol, took))
                break
        if not ok:
            continue
        children[bob].discard(a)
        children.setdefault(carol, set()).add(a)
        par[a] = carol
        shortens += 1
        if log is not None:
            log(("s", a, carol, took, int(carol in friend_sets[a])))
        # sorted: a set's order depends on the Python version, and with 0 < p < 1 the order
        # decides who draws which introduction, so iterating the set made the tables differ
        for x in [a] + sorted(children.get(a, ())):
            if x not in queued:
                work.append(x)
                queued.add(x)
    return par, shortens, intros, refusals


def key_first(s):
    return "first/" + s


def key_ps(s, p):
    return "ps/{}/{:g}".format(s, p)


COUNTERS = ("shortens", "intros", "refusals")
GLOBAL = ("authors", "deciders", "waits", "following", "holders")


def run(args):
    q, seed, n, strategies, ps = args[:5]
    g = args[5] if len(args) > 5 else graph_spec("kleinberg", q)
    t0 = time.time()
    rng = random.Random(seed)
    N = n * n
    adj = make_graph(rng, n, g)
    counts, followers = draw_followers(rng, N)
    pairs = [(f, b) for b in range(N) for f in followers[b]]
    rng.shuffle(pairs)
    base = rng.getrandbits(62)
    friend_sets = [set(a) for a in adj]
    nb = len(BIN_LABELS)
    keys = ["sub"] + [key_first(s) for s in strategies] + [key_ps(s, p) for s in strategies for p in ps]
    res = {k: [[] for _ in range(nb)] for k in keys}
    rat = {k: [[] for _ in range(nb)] for k in keys}
    fanin = {k: [[] for _ in range(nb)] for k in keys}
    cnt = {k: {c: [0] * nb for c in COUNTERS} for k in keys}
    glob = {c: [0] * nb for c in GLOBAL}
    dist = [[] for _ in range(nb)]
    seconds = {k: 0.0 for k in keys}

    F = {}
    for si, s in enumerate(strategies):
        ts = time.time()
        F[s] = first_following(adj, N, pairs, s, random.Random(base + si + 1))
        seconds[key_first(s)] += time.time() - ts
    s0 = strategies[0]
    for s in strategies[1:]:  # who follows does not depend on the strategy
        assert all(len(F[s][2][b]) == len(F[s0][2][b]) for b in range(N))
    R = {(s, p): random.Random(base + 1000 * (si + 1) + pi)
         for si, s in enumerate(strategies) for pi, p in enumerate(ps)}

    def add(key, bi, b, dep, par, fol, gd):
        for f in fol:
            res[key][bi].append(dep[f])
            rat[key][bi].append(dep[f] / gd[f])
        fanin[key][bi].append(sum(1 for x in par.values() if x == b))

    for b in range(N):
        bi = bin_of(counts[b])
        d0 = F[s0][0][b]
        fol = [f for f in followers[b] if f in d0]
        glob["authors"][bi] += 1
        glob["deciders"][bi] += counts[b]
        glob["waits"][bi] += len(F[s0][2][b])
        glob["following"][bi] += len(fol)
        glob["holders"][bi] += len(d0) - 1
        gd = bfs_targets(adj, b, fol) if fol else {}
        for f in fol:
            dist[bi].append(gd[f])
        if fol:
            sub = bfs_within(adj, b, d0)
            for f in fol:
                res["sub"][bi].append(sub[f])
                rat["sub"][bi].append(sub[f] / gd[f])
        for s in strategies:
            dep, par = F[s][0][b], F[s][1][b]
            add(key_first(s), bi, b, dep, par, fol, gd)
            for p in ps:
                k = key_ps(s, p)
                ts = time.time()
                par2, sh, it, rf = shorten(friend_sets, b, par, p, R[(s, p)])
                dep2 = depths_of(b, par2)
                seconds[k] += time.time() - ts
                add(k, bi, b, dep2, par2, fol, gd)
                cnt[k]["shortens"][bi] += sh
                cnt[k]["intros"][bi] += it
                cnt[k]["refusals"][bi] += rf
    return {"q": q, "seed": seed, "graph": g, "label": graph_label(g), "pairs": len(pairs), "mean_degree": sum(len(a) for a in adj) / N,
            "keys": keys, "res": res, "rat": rat, "fanin": fanin, "cnt": cnt, "glob": glob,
            "dist": dist, "times": seconds, "seconds": time.time() - t0}


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


def describe(key):
    if key == "sub":
        return "shortest-path tree over the followers' subgraph"
    parts = key.split("/")
    if parts[0] == "first":
        return "first following, {}".format(parts[1])
    return "after shortening, {}, p = {}".format(parts[1], parts[2])


def write_q(spec, runs, outdir, strategies, ps):
    runs = sorted(runs, key=lambda r: r["seed"])
    seeds = [r["seed"] for r in runs]
    nb = len(BIN_LABELS)
    bins = list(range(nb)) + [None]

    def pooled(field, key, bi):
        idx = range(nb) if bi is None else [bi]
        return [v for r in runs for i in idx for v in r[field][key][i]]

    def g(name, bi):
        idx = range(nb) if bi is None else [bi]
        return sum(r["glob"][name][i] for r in runs for i in idx)

    def c(key, name, bi):
        idx = range(nb) if bi is None else [bi]
        return sum(r["cnt"][key][name][i] for r in runs for i in idx)

    def label(bi):
        return "all" if bi is None else BIN_LABELS[bi]

    L = ["# GFF path lengths, first following and Short Feeds' shortening, {}".format(graph_title(spec)), ""]
    L.append("{}; seeds {}, pooled; "
             "produced by programs/gff/paths/paths.py, which reprints this file unchanged run after run, "
             "on Labelled Overlay:1 and Short Feeds:1 (programs/gff; see the docstring and README.md).".format(graph_text(spec, N_SIDE), ", ".join(map(str, seeds))))
    L.append("Path and ratio are over the deciders who follow; ratio is path length over graph distance "
             "in the friendship graph; waiting is the fraction of deciders still waiting at the end; holders "
             "per decided follower is all who follow the feed (the author's friends included) over the "
             "deciders who follow; fan-in is the persons with a pointer to the author, per author.  "
             "Strategies: random, first (first in the friend list) and shortest (an oracle baseline).  "
             "p is the probability that an introduction GFF-PS needs is taken.")
    L.append("")

    L.append("## First following (GDO-L:1, the strategy choosing the friend)")
    L.append("")
    head = ("| followers | authors | pairs | following | waiting | holders per decided follower "
            "| path mean | median | max | ratio mean | median | max | fan-in mean | max |")
    sep = "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
    for s in strategies:
        k = key_first(s)
        L += ["### Strategy {}".format(s), "", head, sep]
        for bi in bins:
            L.append(("| {} " * 14 + "|").format(
                label(bi), g("authors", bi), g("deciders", bi), g("following", bi),
                frac(g("waits", bi), g("deciders", bi)),
                frac(g("holders", bi), g("following", bi), "{:.3f}"),
                *stats(pooled("res", k, bi)), *stats(pooled("rat", k, bi), True),
                *stats(pooled("fanin", k, bi))[::2]))
        L.append("")

    L.append("## After shortening (GFF-PS:1), introductions taken with probability p")
    L.append("")
    head = ("| followers | following | shortenings per follower | introductions per follower "
            "| refused per follower | path mean | median | max | ratio mean | median | max | fan-in mean | max |")
    sep = "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
    for s in strategies:
        for p in ps:
            k = key_ps(s, p)
            L += ["### Strategy {}, p = {:g}".format(s, p), "", head, sep]
            for bi in bins:
                fo = g("following", bi)
                L.append("| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |".format(
                    label(bi), fo, frac(c(k, "shortens", bi), fo, "{:.3f}"),
                    frac(c(k, "intros", bi), fo, "{:.3f}"), frac(c(k, "refusals", bi), fo, "{:.3f}"),
                    *stats(pooled("res", k, bi)), *stats(pooled("rat", k, bi), True),
                    *stats(pooled("fanin", k, bi))[::2]))
            L.append("")

    L.append("## Baselines")
    L.append("")
    L.append("### Shortest-path tree over the followers' subgraph (the author, the author's friends and the deciders who follow)")
    L.append("")
    L += ["| followers | following | path mean | median | max | ratio mean | median | max |",
          "|---|---:|---:|---:|---:|---:|---:|---:|"]
    for bi in bins:
        L.append("| {} | {} | {} | {} | {} | {} | {} | {} |".format(
            label(bi), g("following", bi), *stats(pooled("res", "sub", bi)),
            *stats(pooled("rat", "sub", bi), True)))
    L.append("")
    L.append("### Graph distance from follower to author in the friendship graph (ratio 1)")
    L.append("")
    L += ["| followers | following | mean | median | max |", "|---|---:|---:|---:|---:|"]
    for bi in bins:
        idx = range(nb) if bi is None else [bi]
        xs = [v for r in runs for i in idx for v in r["dist"][i]]
        L.append("| {} | {} | {} | {} | {} |".format(label(bi), len(xs), *stats(xs)))
    L.append("")

    L.append("## Distribution of path length (length:count)")
    L.append("")
    L += ["| followers | measure | length:count |", "|---|---|---|"]
    keys = [key_first(s) for s in strategies] + [key_ps(s, p) for s in strategies for p in ps] + ["sub"]
    for bi in range(nb):
        for k in keys:
            xs = pooled("res", k, bi)
            if xs:
                L.append("| {} | {} | {} |".format(BIN_LABELS[bi], describe(k), hist(xs)))
        xs = [v for r in runs for v in r["dist"][bi]]
        if xs:
            L.append("| {} | graph distance | {} |".format(BIN_LABELS[bi], hist(xs)))
    L.append("")

    L.append("## Per seed, all bins")
    L.append("")
    L += ["| seed | measure | following | waiting | path mean / max | ratio mean / max | fan-in max "
          "| shortenings | introductions | refused |",
          "|---:|---|---:|---:|---|---|---:|---:|---:|---:|"]
    for r in runs:
        fo = sum(r["glob"]["following"])
        wa = frac(sum(r["glob"]["waits"]), sum(r["glob"]["deciders"]))
        for k in keys[:-1]:
            xs = [v for i in range(nb) for v in r["res"][k][i]]
            rs = [v for i in range(nb) for v in r["rat"][k][i]]
            fi = [v for i in range(nb) for v in r["fanin"][k][i]]
            pm, _, px = stats(xs)
            rm, _, rx = stats(rs, True)
            sh = sum(r["cnt"][k]["shortens"]) if k.startswith("ps/") else "-"
            it = sum(r["cnt"][k]["intros"]) if k.startswith("ps/") else "-"
            rf = sum(r["cnt"][k]["refusals"]) if k.startswith("ps/") else "-"
            L.append("| {} | {} | {} | {} | {} / {} | {} / {} | {} | {} | {} | {} |".format(
                r["seed"], describe(k), fo, wa, pm, px, rm, rx, max(fi) if fi else "-",
                sh, it, rf))
    L.append("")
    L += ["| seed | author-decider pairs | mean friends per person |", "|---:|---:|---:|"]
    for r in runs:
        L.append("| {} | {} | {:.3f} |".format(r["seed"], r["pairs"], r["mean_degree"]))
    L.append("")
    os.makedirs(outdir, exist_ok=True)
    with open(os.path.join(outdir, "gff-ps-{}.md".format(graph_label(spec))), "w") as fh:
        fh.write("\n".join(L))


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--graph", choices=GRAPHS, default="kleinberg",
                    help="kleinberg (the grid with long-range friends; default) or regular (a random d-regular graph)")
    ap.add_argument("--q", type=int, nargs="+", default=[1, 2, 4], help="kleinberg: long-range friends per person")
    ap.add_argument("--r", type=float, default=2.0,
                    help="kleinberg: the exponent of the long-range draw (default 2; 0 draws uniformly)")
    ap.add_argument("--d", type=int, nargs="+", default=[D_REGULAR],
                    help="regular: friends per person (default {})".format(D_REGULAR))
    ap.add_argument("--seeds", type=int, nargs="+", default=[1, 2, 3, 4, 5])
    ap.add_argument("--n", type=int, default=N_SIDE)
    ap.add_argument("--strategies", nargs="+", default=list(STRATEGIES), choices=STRATEGIES,
                    help="how a decider chooses among friends with a pointer (GDO-L:1)")
    ap.add_argument("--p", type=float, nargs="+", default=list(P_INTRO),
                    help="probabilities that an introduction GFF-PS needs is taken")
    ap.add_argument("--jobs", type=int, default=1, help="runs in parallel (processes)")
    ap.add_argument("--out", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "tables"))
    a = ap.parse_args()
    set_side(a.n)
    t0 = time.time()
    if a.graph == "kleinberg":
        specs = [graph_spec("kleinberg", q=q, r=a.r) for q in a.q]
    else:
        specs = [graph_spec("regular", d=d) for d in a.d]
    tasks = [(g.get("q", 0), s, a.n, a.strategies, a.p, g) for g in specs for s in a.seeds]
    if a.jobs > 1:
        with Pool(a.jobs) as pool:
            results = pool.map(run, tasks)
    else:
        results = []
        for t in tasks:
            results.append(run(t))
            r = results[-1]
            print("{} seed={} pairs={} total {:.0f}s".format(r["label"], r["seed"], r["pairs"], r["seconds"]),
                  flush=True)
    for g in specs:
        write_q(g, [r for r in results if r["label"] == graph_label(g)], a.out, a.strategies, a.p)
    print("total {:.0f}s".format(time.time() - t0))


if __name__ == "__main__":
    main()
