#!/usr/bin/env python3
"""Print a social graph for an sGLP run: a friendship graph of a chosen
family, and optionally a directed overlay on it (programs/sglp/graphs/README.md).

usage: gen.py <family> --n N [family parameters] --seed S [--points SPEC] > g.graph

Families and their parameters:
  complete                  every pair of agents
  ring     --k K            each agent and the K nearest on each side (default 1)
  grid     --rows R --cols C [--torus]
                            an R x C lattice, n = R * C
  er       --p P            Erdos-Renyi G(n, p): each pair with probability P
  regular  --d D            a uniformly random D-regular graph, an expander
                            with high probability for D >= 3 (n * D even)
  ws       --k K --beta B   Watts-Strogatz small world: the ring of K each
                            side, each edge rewired with probability B
  ba       --m M            Barabasi-Albert: each new agent attaches to M
                            agents by preferential attachment
  sbm      --sizes A,B,... --p-in P --p-out Q
                            a stochastic block model, n the sum of the sizes,
                            a pair within a block with probability P and
                            across blocks with Q
  file     --edges PATH     the undirected edges of PATH, one "a b" per line,
                            agents 1..n, # comments

The overlay, --points SPEC, directed edges along friendships:
  all                       each agent points to every friend
  out:D                     each agent points to D of its friends drawn
                            uniformly, all of them where it has fewer
  file:PATH                 the directed edges of PATH, one "a b" per line,
                            each of which must be a friendship

The output, read by compose.py and check.py:
  # comment lines: the family, its parameters, the seed, the graph's statistics
  n <N>
  friend <a> <b>            one per friendship, a < b
  points <a> <b>            one per pointer, a points to b

The draws are Python's random.Random(seed), so a graph is a function of its
family, its parameters and its seed; a run's record keeps the file itself.
"""
import argparse
import random
import sys
from collections import deque


def fail(msg):
    print("gen.py: " + msg, file=sys.stderr)
    sys.exit(2)


def add(edges, a, b):
    if a != b:
        edges.add((min(a, b), max(a, b)))


def complete(n, rng, a):
    return {(i, j) for i in range(1, n + 1) for j in range(i + 1, n + 1)}


def ring_lattice(n, k):
    edges = set()
    for i in range(1, n + 1):
        for d in range(1, k + 1):
            add(edges, i, (i - 1 + d) % n + 1)
    return edges


def ring(n, rng, a):
    if a.k < 1 or 2 * a.k >= n:
        fail("ring needs 1 <= k < n/2")
    return ring_lattice(n, a.k)


def grid(n, rng, a):
    edges = set()
    num = lambda r, c: r * a.cols + c + 1
    for r in range(a.rows):
        for c in range(a.cols):
            if c + 1 < a.cols or (a.torus and a.cols > 2):
                add(edges, num(r, c), num(r, (c + 1) % a.cols))
            if r + 1 < a.rows or (a.torus and a.rows > 2):
                add(edges, num(r, c), num((r + 1) % a.rows, c))
    return edges


def er(n, rng, a):
    if not 0 <= a.p <= 1:
        fail("er needs 0 <= p <= 1")
    return {(i, j) for i in range(1, n + 1) for j in range(i + 1, n + 1) if rng.random() < a.p}


def regular(n, rng, a):
    d = a.d
    if d < 0 or d >= n or (n * d) % 2:
        fail("regular needs 0 <= d < n and n * d even")
    for _ in range(10000):
        stubs = [i for i in range(1, n + 1) for _ in range(d)]
        rng.shuffle(stubs)
        edges = set()
        ok = True
        for x in range(0, len(stubs), 2):
            i, j = stubs[x], stubs[x + 1]
            e = (min(i, j), max(i, j))
            if i == j or e in edges:
                ok = False
                break
            edges.add(e)
        if ok:
            return edges
    fail("regular: no simple graph drawn in 10000 tries; try another seed or a smaller d")


def ws(n, rng, a):
    if a.k < 1 or 2 * a.k >= n or not 0 <= a.beta <= 1:
        fail("ws needs 1 <= k < n/2 and 0 <= beta <= 1")
    edges = ring_lattice(n, a.k)
    for d in range(1, a.k + 1):
        for i in range(1, n + 1):
            j = (i - 1 + d) % n + 1
            e = (min(i, j), max(i, j))
            if e in edges and rng.random() < a.beta:
                free = [t for t in range(1, n + 1)
                        if t != i and (min(i, t), max(i, t)) not in edges]
                if free:
                    edges.discard(e)
                    t = free[rng.randrange(len(free))]
                    edges.add((min(i, t), max(i, t)))
    return edges


def ba(n, rng, a):
    m = a.m
    if m < 1 or m >= n:
        fail("ba needs 1 <= m < n")
    edges = complete(m + 1, rng, a)
    ends = [x for e in sorted(edges) for x in e]
    for v in range(m + 2, n + 1):
        targets = set()
        while len(targets) < m:
            targets.add(ends[rng.randrange(len(ends))])
        for t in sorted(targets):
            edges.add((t, v))
            ends += [t, v]
    return edges


def sbm(n, rng, a):
    sizes = [int(s) for s in a.sizes.split(",")]
    block = [b for b, s in enumerate(sizes) for _ in range(s)]
    edges = set()
    for i in range(1, n + 1):
        for j in range(i + 1, n + 1):
            p = a.p_in if block[i - 1] == block[j - 1] else a.p_out
            if rng.random() < p:
                edges.add((i, j))
    return edges


def read_pairs(path, n, what):
    pairs = []
    try:
        lines = open(path).read().splitlines()
    except OSError as e:
        fail(f"cannot read {path}: {e}")
    for ln, line in enumerate(lines, 1):
        line = line.split("#", 1)[0].split()
        if not line:
            continue
        if len(line) != 2 or not all(t.isdigit() for t in line):
            fail(f"{path}:{ln}: not two agent numbers")
        x, y = int(line[0]), int(line[1])
        if not (1 <= x <= n and 1 <= y <= n) or x == y:
            fail(f"{path}:{ln}: {what} {x} {y} is not between two distinct agents of 1..{n}")
        pairs.append((x, y))
    return pairs


def from_file(n, rng, a):
    if not a.edges:
        fail("file needs --edges PATH")
    edges = set()
    for x, y in read_pairs(a.edges, n, "edge"):
        add(edges, x, y)
    return edges


FAMILIES = {"complete": complete, "ring": ring, "grid": grid, "er": er,
            "regular": regular, "ws": ws, "ba": ba, "sbm": sbm, "file": from_file}


def points(spec, n, edges, rng):
    nbrs = {i: [] for i in range(1, n + 1)}
    for i, j in sorted(edges):
        nbrs[i].append(j)
        nbrs[j].append(i)
    if spec == "all":
        return sorted((i, j) for i in nbrs for j in nbrs[i])
    if spec.startswith("out:") and spec[4:].isdigit():
        d = int(spec[4:])
        out = []
        for i in range(1, n + 1):
            fs = sorted(nbrs[i])
            out += [(i, j) for j in sorted(fs if len(fs) <= d else rng.sample(fs, d))]
        return out
    if spec.startswith("file:"):
        out = set()
        for x, y in read_pairs(spec[5:], n, "pointer"):
            if (min(x, y), max(x, y)) not in edges:
                fail(f"{spec[5:]}: pointer {x} {y} is not along a friendship")
            out.add((x, y))
        return sorted(out)
    fail(f"--points is all, out:D or file:PATH, not {spec}")


def stats(n, edges):
    nbrs = {i: set() for i in range(1, n + 1)}
    for i, j in edges:
        nbrs[i].add(j)
        nbrs[j].add(i)
    degs = [len(nbrs[i]) for i in nbrs]
    seen, comps, diam = set(), 0, 0
    for s in nbrs:
        if s not in seen:
            comps += 1
            q = deque([s])
            seen.add(s)
            while q:
                u = q.popleft()
                for w in nbrs[u]:
                    if w not in seen:
                        seen.add(w)
                        q.append(w)
    if comps == 1 and n <= 5000:
        for s in nbrs:
            dist = {s: 0}
            q = deque([s])
            while q:
                u = q.popleft()
                for w in nbrs[u]:
                    if w not in dist:
                        dist[w] = dist[u] + 1
                        q.append(w)
            diam = max(diam, max(dist.values()))
    cl = []
    for i in nbrs:
        k = len(nbrs[i])
        if k >= 2:
            links = sum(1 for u in nbrs[i] for w in nbrs[i] if u < w and w in nbrs[u])
            cl.append(2 * links / (k * (k - 1)))
    return (f"edges {len(edges)} degree min {min(degs)} mean {sum(degs) / n:.2f} max {max(degs)} "
            f"components {comps} diameter {diam if comps == 1 else 'none'} "
            f"clustering {sum(cl) / n:.3f}")


def main():
    ap = argparse.ArgumentParser(description="Print a social graph for an sGLP run.")
    ap.add_argument("family", choices=sorted(FAMILIES))
    ap.add_argument("--n", type=int)
    ap.add_argument("--seed", type=int, required=True)
    ap.add_argument("--k", type=int, default=1)
    ap.add_argument("--p", type=float)
    ap.add_argument("--d", type=int)
    ap.add_argument("--m", type=int)
    ap.add_argument("--beta", type=float)
    ap.add_argument("--rows", type=int)
    ap.add_argument("--cols", type=int)
    ap.add_argument("--torus", action="store_true")
    ap.add_argument("--sizes")
    ap.add_argument("--p-in", type=float)
    ap.add_argument("--p-out", type=float)
    ap.add_argument("--edges")
    ap.add_argument("--points")
    a = ap.parse_args()

    need = {"er": ["p"], "regular": ["d"], "ws": ["beta"], "ba": ["m"],
            "grid": ["rows", "cols"], "sbm": ["sizes", "p_in", "p_out"]}
    for p in need.get(a.family, []):
        if getattr(a, p) is None:
            fail(f"{a.family} needs --{p.replace('_', '-')}")
    if a.family == "grid":
        a.n = a.rows * a.cols
    elif a.family == "sbm":
        a.n = sum(int(s) for s in a.sizes.split(","))
    if a.n is None or a.n < 1:
        fail("--n is a positive number of agents")
    n = a.n
    params = {"complete": [], "ring": ["k"], "grid": ["rows", "cols", "torus"], "er": ["p"],
              "regular": ["d"], "ws": ["k", "beta"], "ba": ["m"],
              "sbm": ["sizes", "p_in", "p_out"], "file": ["edges"]}[a.family]

    rng = random.Random(a.seed)
    edges = FAMILIES[a.family](n, rng, a)
    ptrs = points(a.points, n, edges, random.Random(a.seed + 1)) if a.points else []

    desc = " ".join(p if getattr(a, p) is True else f"{p.replace('_', '-')} {getattr(a, p)}"
                    for p in params if getattr(a, p) is not False)
    out = [f"# sglp graph: {a.family} n {n}{' ' + desc if desc else ''} seed {a.seed}"]
    if a.points:
        out.append(f"# points {a.points}: {len(ptrs)} pointers")
    out.append(f"# {stats(n, edges)}")
    out.append(f"n {n}")
    out += [f"friend {i} {j}" for i, j in sorted(edges)]
    out += [f"points {i} {j}" for i, j in ptrs]
    print("\n".join(out))
    print("\n".join(line for line in out if line.startswith("#")), file=sys.stderr)


if __name__ == "__main__":
    main()
