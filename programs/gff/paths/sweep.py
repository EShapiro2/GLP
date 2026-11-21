#!/usr/bin/env python3
"""Parameter sweeps of paths.py's model: reach, path length, author load and
introductions as the graph, n, decider density, strategy and p vary.

Everything is run by paths.py's own functions (make_graph, draw_followers,
first_following, shorten, depths_of, bfs_targets, bfs_within, graph_stats);
nothing is re-implemented.  The graph for (spec, n, seed) is paths.py's:
random.Random(seed), make_graph first.

Graphs (updated 2026-11-08), in three families with matched mean numbers of
friends (4, 6, 8, 12):
  small world   Kleinberg's grid, exponent r = 2 (paths.py's default):
                q = 0 (the plain grid), 1, 2, 4.  Not an expander.
  uniform       Kleinberg's grid, r = 0 (long-range friends drawn uniformly):
                q = 1, 2, 4.  An expander that keeps the grid neighbours.
  regular       a random d-regular graph: d = 4, 6, 8, 12.  An expander.
The small-world runs draw exactly as the first sweep did (2026-11-08, before
the graph option), so their results are unchanged.

Experiments (each over --seeds; "authors" are drawn at random per point by a
generator of the sweep's own, and given exactly k = density * N deciders drawn
uniformly from everyone else, as live.py's exact k):
  graphs   n = 100: graph_stats of every graph (spectral gap, cut
           conductance, mean graph distance, mean friends).
  reach    n = 100, every graph, density 0.02 to 0.9: the first following
           under each strategy: the share of deciders who follow, path
           lengths, the graph distance and the best tree among followers.
  shorten  n = 100, mean friends 6 and 12 in each family, density 0.3 and
           0.6, p from 0 to 1, strategies random and shortest: after
           shortening, path lengths, author load, shortenings, introductions
           and refusals per follower.
  size     mean friends 6 in each family, density 0.5, n from 30 to 200: the
           first following (random, shortest), shortening at p = 0.5, and
           graph_stats of each graph.
  natural  n = 100, mean friends 6 in each family: paths.run itself, the drawn
           k of every author: the share of wanted follows that succeed.
The experiment switch of 2026-11-09 (switching by arrival order, no act of any
contract) is gone; git holds it, last at 5fc93257.

Writes tables/sweep.json (every author's results) and prints progress.
  python3 sweep.py [--seeds 1 2 3 4 5] [--authors 10] [--jobs 8] [--quick]
"""

import argparse
import json
import os
import random
import time
from multiprocessing import Pool

import paths

HERE = os.path.dirname(os.path.abspath(__file__))

K = lambda q, r=2: paths.graph_spec("kleinberg", q=q, r=r)
R = lambda d: paths.graph_spec("regular", d=d)
FAMILY = {"kleinberg2": "small world", "kleinberg0": "uniform", "regular": "regular"}
ALL_GRAPHS = [K(0), K(1), K(2), K(4), K(1, 0), K(2, 0), K(4, 0), R(4), R(6), R(8), R(12)]
SHORT_GRAPHS = [K(1), K(4), K(1, 0), K(4, 0), R(6), R(12)]
SIZE_GRAPHS = [K(1), K(1, 0), R(6)]
NATURAL_GRAPHS = [K(1), K(1, 0), R(6)]

REACH_RHO = [0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
SHORT_RHO = [0.3, 0.6]
SHORT_P = [0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0]
SHORT_STRATS = ["random", "shortest"]
SIZE_N = [30, 50, 70, 100, 140, 200]
SIZE_RHO = 0.5


def family(g):
    return "regular" if g["kind"] == "regular" else "kleinberg{:g}".format(g.get("r", 2))


def describe(g):
    """Fields every record carries about its graph."""
    return {"graph": paths.graph_label(g), "family": FAMILY[family(g)], "kind": g["kind"],
            "q": g.get("q"), "r": g.get("r"), "d": g.get("d")}


def world(g, n, seed):
    paths.set_side(n)
    adj = paths.make_graph(random.Random(seed), n, g)
    return adj, [set(x) for x in adj]


def pick_authors(g, n, seed, rho, m, tag):
    """m distinct authors, each with exactly round(rho * N) deciders.  The
    small world keys its generator by q alone, as the first sweep did."""
    N = n * n
    k = max(1, min(N - 1, round(rho * N)))
    gk = g["q"] if family(g) == "kleinberg2" else paths.graph_label(g)
    r = random.Random("sweep/{}/{}/{}/{}/{:g}".format(tag, n, gk, seed, rho))
    authors = r.sample(range(N), m)
    deciders, pairs = {}, []
    for b in authors:
        s = r.sample(range(N - 1), k)
        deciders[b] = [x + 1 if x >= b else x for x in s]
        pairs += [(f, b) for f in deciders[b]]
    r.shuffle(pairs)
    return k, authors, deciders, pairs, r.getrandbits(62)


def mean(xs):
    return sum(xs) / len(xs) if xs else float("nan")


def first(adj, N, pairs, strategies, base):
    return {s: paths.first_following(adj, N, pairs, s, random.Random(base + si + 1))
            for si, s in enumerate(strategies)}


def task_graphs(args):
    g, n, seed = args
    adj, _ = world(g, n, seed)
    st = paths.graph_stats(adj, n, random.Random("stats/{}/{}".format(n, seed)))
    return [dict(st, n=n)]


def task_reach(args):
    g, n, seed, rho, m = args
    N = n * n
    adj, _ = world(g, n, seed)
    k, authors, dec, pairs, base = pick_authors(g, n, seed, rho, m, "reach")
    F = first(adj, N, pairs, paths.STRATEGIES, base)
    out = []
    for b in authors:
        dep0 = F["random"][0][b]
        fol = [f for f in dec[b] if f in dep0]
        rec = {"n": n, "rho": rho, "k": k, "reach": len(fol) / k, "following": len(fol), "holders": len(dep0) - 1}
        if fol:
            gd = paths.bfs_targets(adj, b, fol)
            sub = paths.bfs_within(adj, b, set(dep0))
            rec["graph_distance"] = mean([gd[f] for f in fol])
            rec["sub"] = mean([sub[f] for f in fol])
            for s in paths.STRATEGIES:
                d = F[s][0][b]
                rec["path_" + s] = mean([d[f] for f in fol])
                rec["max_" + s] = max(d[f] for f in fol)
        out.append(rec)
    return out


def shorten_metrics(fs, b, dec, par, p, rng):
    par2, sh, it, rf = paths.shorten(fs, b, par, p, rng)
    dep = paths.depths_of(b, par2)
    fol = [f for f in dec if f in dep]
    nf = max(1, len(fol))
    return {"path": mean([dep[f] for f in fol]), "max": max((dep[f] for f in fol), default=0),
            "fanin": sum(1 for x in par2.values() if x == b),
            "shortens": sh / nf, "intros": it / nf, "refusals": rf / nf, "following": len(fol)}


def task_shorten(args):
    g, n, seed, rho, m = args
    N = n * n
    adj, fs = world(g, n, seed)
    k, authors, dec, pairs, base = pick_authors(g, n, seed, rho, m, "shorten")
    F = first(adj, N, pairs, SHORT_STRATS, base)
    out = []
    for b in authors:
        for s in SHORT_STRATS:
            for p in SHORT_P:
                rec = shorten_metrics(fs, b, dec[b], F[s][1][b], p,
                                      random.Random("{}/{}/{:g}/{}".format(base, s, p, b)))
                rec.update({"n": n, "rho": rho, "strategy": s, "p": p, "k": k, "friends": len(adj[b])})
                out.append(rec)
    return out


def task_size(args):
    g, n, seed, rho, m = args
    N = n * n
    adj, fs = world(g, n, seed)
    k, authors, dec, pairs, base = pick_authors(g, n, seed, rho, m, "size")
    F = first(adj, N, pairs, ["random", "shortest"], base)
    out = []
    for b in authors:
        dep0 = F["random"][0][b]
        fol = [f for f in dec[b] if f in dep0]
        rec = {"n": n, "rho": rho, "k": k, "reach": len(fol) / k}
        if fol:
            gd = paths.bfs_targets(adj, b, fol)
            rec["graph_distance"] = mean([gd[f] for f in fol])
            for s in ("random", "shortest"):
                rec["path_" + s] = mean([F[s][0][b][f] for f in fol])
            sm = shorten_metrics(fs, b, dec[b], F["random"][1][b], 0.5, random.Random("{}/size/{}".format(base, b)))
            rec["ps05_path"], rec["ps05_fanin"], rec["ps05_intros"] = sm["path"], sm["fanin"], sm["intros"]
        out.append(rec)
    return out


def task_natural(args):
    g, n, seed = args
    r = paths.run((g.get("q", 0), seed, n, list(paths.STRATEGIES), list(paths.P_INTRO), g))
    gl = r["glob"]
    pooled = lambda field, key: [x for b in r[field][key] for x in b]
    rec = {"n": n, "deciders": sum(gl["deciders"]), "waits": sum(gl["waits"]), "following": sum(gl["following"]),
           "mean_degree": r["mean_degree"],
           "by_bin": {lab: {"authors": gl["authors"][i], "deciders": gl["deciders"][i], "waits": gl["waits"][i]}
                      for i, lab in enumerate(paths.BIN_LABELS)}}
    for key in r["keys"]:
        rec[key] = {"path": mean(pooled("res", key)), "fanin_max": max(pooled("fanin", key), default=0),
                    "fanin_mean": mean(pooled("fanin", key))}
    rec["graph_distance"] = mean([x for b in r["dist"] for x in b])
    return [rec]


TASKS = {"graphs": task_graphs, "reach": task_reach, "shorten": task_shorten, "size": task_size,
         "natural": task_natural}


def run_task(t):
    kind, args = t
    t0 = time.time()
    recs = TASKS[kind](args)
    g, seed = args[0], args[2]
    for r in recs:
        r.update(describe(g))
        r["seed"] = seed
    return kind, recs, time.time() - t0


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--seeds", type=int, nargs="+", default=[1, 2, 3, 4, 5])
    ap.add_argument("--authors", type=int, default=10, help="authors per point (default 10)")
    ap.add_argument("--jobs", type=int, default=os.cpu_count())
    ap.add_argument("--quick", action="store_true", help="one seed, 2 authors per point (a smoke test)")
    ap.add_argument("--out", default=os.path.join(HERE, "tables", "sweep.json"))
    a = ap.parse_args()
    seeds, m = (a.seeds[:1], 2) if a.quick else (a.seeds, a.authors)

    tasks = [("size", (g, n, s, SIZE_RHO, m)) for n in sorted(SIZE_N, reverse=True) for g in SIZE_GRAPHS for s in seeds]
    tasks += [("graphs", (g, n, s)) for g in SIZE_GRAPHS for n in SIZE_N for s in seeds]
    tasks += [("graphs", (g, 100, s)) for g in ALL_GRAPHS if g not in SIZE_GRAPHS for s in seeds]
    tasks += [("natural", (g, 100, s)) for g in NATURAL_GRAPHS for s in seeds]
    tasks += [("shorten", (g, 100, s, rho, max(2, m // 2))) for g in SHORT_GRAPHS for rho in SHORT_RHO for s in seeds]
    tasks += [("reach", (g, 100, s, rho, m)) for g in ALL_GRAPHS for rho in REACH_RHO for s in seeds]
    t0 = time.time()
    results = {k: [] for k in TASKS}
    with Pool(a.jobs) as pool:
        for i, (kind, recs, sec) in enumerate(pool.imap_unordered(run_task, tasks), 1):
            results[kind] += recs
            if i % 50 == 0 or i == len(tasks):
                print("{}/{} tasks, {:.0f} s".format(i, len(tasks), time.time() - t0), flush=True)
    meta = {"date": time.strftime("%Y-%m-%d %H:%M"), "seeds": seeds, "authors_per_point": m,
            "seconds": round(time.time() - t0, 1),
            "graphs": [dict(describe(g), spec=g) for g in ALL_GRAPHS],
            "short_graphs": [paths.graph_label(g) for g in SHORT_GRAPHS],
            "size_graphs": [paths.graph_label(g) for g in SIZE_GRAPHS],
            "natural_graphs": [paths.graph_label(g) for g in NATURAL_GRAPHS],
            "reach_rho": REACH_RHO, "short_rho": SHORT_RHO, "short_p": SHORT_P, "size_n": SIZE_N,
            "size_rho": SIZE_RHO, "note": "per-author records; nan where no decider follows"}
    with open(a.out, "w") as fh:
        json.dump({"meta": meta, **results}, fh, separators=(",", ":"), default=lambda x: None)
    print("wrote {} in {:.0f} s".format(a.out, time.time() - t0))


if __name__ == "__main__":
    main()
