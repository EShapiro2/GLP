#!/usr/bin/env python3
"""Record the acts of GFF-PS, GFF-FR and GFF-FR-BC for a few authors, for live.html to play.

Every run is async_sim.py's (async_sim.simulate, which calls back with each
act through its log hook; nothing is re-implemented here): following,
delivery and the contract's own act at once in continuous time, one run per
author, contract and setting:
  ps   GFF-PS: shortening (GFF-PS:1), introductions taken with probability p;
  fr   GFF-FR: redundant following (GFF-FR:1), up to d pointers each;
  bc   GFF-FR-BC: redundant following and the back-channel (GFF-FR-BC:1);
and, served only (--serve), the prototype of combo_sim.py, which no contract
has yet:
  bcps   GFF-FR-BC with GFF-PS:1's shortening, up to d pointers each and
         introductions taken with probability p (combo_sim's combine);
  bcpsg  the same with the received-from guard (combo_sim's guard);
with a strategy for the first following (random or first; shortest is an
oracle the asynchronous model has no use for), a jitter (a hop takes
1 + jitter * U(0, 1)) and a share of machines down.  Switching by arrival
order, which earlier versions played, is no act of any contract and is gone
(git holds it, last at 5fc93257); the synchronous shortening to a fixpoint
is paths.py's, in its tables.

The world (graph, deciders) for (n, q, seed) is drawn as paths.run draws it,
and the graph distance and the best tree through the followers, the page's
baselines, are over the deciders who follow at the end of paths.py's first
following (who follows does not depend on the strategy).  The authors shown
are those whose drawn number of deciders k is nearest each target (--k), or
those named (--authors).  With --exact-k, each author picked by a target gets
exactly that many deciders instead, drawn uniformly from everyone else by a
generator of its own; the other authors and the graph are unchanged, but that
author is no longer the tables' draw.

Two ways to run it:
  python3 live.py --serve [options]   serves live.html at http://127.0.0.1:8765/,
                                      makes each run the page asks for on
                                      demand, and reruns the world whenever n,
                                      q, seed or k is changed.  Ctrl-C stops it.
  python3 live.py [options]           writes live-data.js (generated; not to be
                                      committed) holding the runs at --file-jitter
                                      and --file-down for every strategy, p and d;
                                      open live.html in a browser.
live.html?author=1&then=fr&d=3&down=0.1&t=3000&play=1 opens at a selection and act.

  python3 live.py --serve --open        # q=1, seed 1, authors with k near 9000, 3000, 2300
  python3 live.py --authors 1234 --n 30 --open
"""

import argparse
import json
import os
import random
import threading
import time
import webbrowser
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlparse

import async_sim
import combo_sim
import paths

HERE = os.path.dirname(os.path.abspath(__file__))
LIMITS = {"n": (5, 200), "q": (0, 12), "seed": (0, 2 ** 31 - 1), "targets": 4}


JITTERS = (0.0, 1.0, 4.0)
DOWNS = (0.0, 0.05, 0.1)
DS = (2, 3)
KINDS = ("ps", "fr", "bc")
PROTOTYPES = ("bcps", "bcpsg")   # combo_sim.py's, made only on demand by the server
ASYNC_SHORTEN_RATE = 1.0   # GFF-PS: each follower examined about once per item period


def key_of(kind, s, j, p, d, down):
    """The page's key of a run: p for GFF-PS and the prototypes, d for all but GFF-PS."""
    return "{}/{}/{:g}/{:g}/{}/{:g}".format(kind, s, j, p if kind in ("ps",) + PROTOTYPES else 0,
                                           d if kind != "ps" else 2, down)


def generate(n, q, seed, ks=None, authors=None, strategies=("random", "first"), ps=paths.P_INTRO,
             exact=False, jitters=JITTERS, file_runs=None, keep=None):
    """The world and the chosen authors, as live.html reads them.  file_runs:
    a list of (jitter, down) at which every run (strategy, contract, p or d) is
    made now and put in the data, for the page opened without the server; the
    server makes the others on demand, by run_async, from what is put in the
    dict keep."""
    paths.set_side(n)
    N = n * n
    rng = random.Random(seed)  # the order of paths.run: graph, deciders, order, base
    adj = paths.build_graph(rng, n, q)
    counts, followers = paths.draw_followers(rng, N)
    pairs = [(f, b) for b in range(N) for f in followers[b]]
    rng.shuffle(pairs)
    base = rng.getrandbits(62)
    friend_sets = [set(x) for x in adj]

    if authors:
        chosen = list(dict.fromkeys(authors))
        targets = [None] * len(chosen)
    else:
        chosen, targets = [], []
        for k in ks:
            b = min((x for x in range(N) if x not in chosen), key=lambda x: (abs(counts[x] - k), x))
            chosen.append(b)
            targets.append(k)
    exact_of = {}
    if exact:
        for b, k in zip(chosen, targets):
            if k is None:
                continue
            k = max(1, min(N - 1, k))
            sample = random.Random("{}/exact/{}/{}".format(seed, b, k)).sample(range(N - 1), k)
            new = [x + 1 if x >= b else x for x in sample]
            counts[b], followers[b] = k, new
            exact_of[b] = True
            order = [(f, b) for f in new]
            random.Random("{}/exact-order/{}/{}".format(seed, b, k)).shuffle(order)
            pairs = [pr for pr in pairs if pr[1] != b] + order   # decides after everyone else

    # who follows at the end of the first following, for the baselines (the same under every strategy)
    depth, _, _ = paths.first_following(adj, N, pairs, "random", random.Random(base + 1))
    out = {"n": n, "q": q, "seed": seed, "strategies": list(strategies), "ps": list(ps),
           "jitters": list(jitters), "downs": list(DOWNS), "ds": list(DS),
           "targets": [k for k in targets if k is not None], "exact": bool(exact),
           "mean_degree": sum(len(x) for x in adj) / N, "authors": []}
    for b in chosen:
        d0 = depth[b]
        fol = [f for f in followers[b] if f in d0]
        gd = paths.bfs_targets(adj, b, fol) if fol else {}
        sub = paths.bfs_within(adj, b, set(d0)) if fol else {}
        runs = {}
        for j, down in file_runs or ():
            for s in strategies:
                for kind in KINDS:
                    for x in (ps if kind == "ps" else DS):
                        p, d = (x, 2) if kind == "ps" else (0, x)
                        runs[key_of(kind, s, j, p, d, down)] = run_async(adj, friend_sets, b, followers[b], base,
                                                                         s, j, p, kind, d, down)
        out["authors"].append({
            "b": b, "k": counts[b], "exact": b in exact_of, "deciders": followers[b], "friends": adj[b],
            "graph_distance": [gd[f] for f in fol],  # of the deciders who come to follow
            "best_tree": [sub[f] for f in fol],      # the shortest-path tree over the followers' subgraph
            "async": runs,
        })
    if keep is not None:
        keep.update(adj=adj, friend_sets=friend_sets, base=base, authors=chosen, n=n,
                    deciders={b: followers[b] for b in chosen})
    out["on_demand"] = file_runs is None
    out["async_params"] = {k: async_sim.DEFAULTS[k] for k in ("period", "decide", "end", "down_mean")}
    out["async_params"]["shorten_rate"] = ASYNC_SHORTEN_RATE
    return out


def run_async(adj, friend_sets, b, deciders, base, s, j, p, kind, d=2, down=0.0):
    """The acts of one asynchronous run (async_sim.simulate) for one author:
    kind "ps" (GFF-PS, introductions taken with probability p), "fr" (GFF-FR,
    redundancy d) or "bc" (GFF-FR-BC); or combo_sim's prototype, "bcps"
    (GFF-FR-BC with shortening) or "bcpsg" (guarded); strategy s, jitter j, a
    share `down` of machines down."""
    ev = []
    rng = random.Random("{}/live/{}/{}".format(base, key_of(kind, s, j, p, d, down), b))
    if kind in PROTOTYPES:
        combo_sim.simulate(adj, friend_sets, b, deciders, rng, jitter=j, strategy=s, down_frac=down,
                           log=ev.append, redundancy=d, back_channel=True, shorten_rate=ASYNC_SHORTEN_RATE,
                           p=p, combine=True, guard=kind == "bcpsg")
        return ev
    kw = {"ps": {"shorten_rate": ASYNC_SHORTEN_RATE, "p": p},
          "fr": {"redundancy": d}, "bc": {"redundancy": d, "back_channel": True}}[kind]
    async_sim.simulate(adj, friend_sets, b, deciders, rng, jitter=j, strategy=s, down_frac=down,
                       log=ev.append, **kw)
    return ev


def summary(out):
    n = out["n"]
    return ["author {} at ({}, {}): {} deciders{}, {} follow, {} friends".format(
        A["b"], A["b"] // n, A["b"] % n, A["k"], " (set exactly)" if A["exact"] else "",
        len(A["graph_distance"]), len(A["friends"])) for A in out["authors"]]


def check(n, q, seed, ks):
    """Validate a request from the page; returns an error message or None."""
    for name, v in (("n", n), ("q", q), ("seed", seed)):
        lo, hi = LIMITS[name]
        if not lo <= v <= hi:
            return "{} must be between {} and {}".format(name, lo, hi)
    if q > n * n // 4:
        return "q is too large for this n"
    if not ks or len(ks) > LIMITS["targets"]:
        return "give 1 to {} values of k".format(LIMITS["targets"])
    if any(not 1 <= k <= n * n - 1 for k in ks):
        return "each k must be between 1 and n*n - 1 = {}".format(n * n - 1)
    return None


def serve(port, default, strategies, ps, jitters, do_open):
    lock = threading.Lock()   # paths.set_side is global state
    cache = {}
    worlds = {}

    def run_json(n, q, seed, ks, exact):
        key = (n, q, seed, tuple(ks), exact)
        if key not in cache:
            t0 = time.time()
            keep = {}
            with lock:
                out = generate(n, q, seed, ks=ks, strategies=strategies, ps=ps, exact=exact, jitters=jitters, keep=keep)
            if len(cache) >= 8:
                old = next(iter(cache))
                cache.pop(old)
                worlds.pop(old, None)
            cache[key] = json.dumps(out, separators=(",", ":")).encode()
            worlds[key] = keep
            print("ran n={} q={} seed={} k={}{} in {:.1f} s".format(
                n, q, seed, ks, " exact" if exact else "", time.time() - t0), flush=True)
            for line in summary(out):
                print("  " + line, flush=True)
        return cache[key]

    class Handler(BaseHTTPRequestHandler):
        def send(self, code, body, ctype):
            self.send_response(code)
            self.send_header("Content-Type", ctype)
            self.send_header("Content-Length", str(len(body)))
            self.send_header("Cache-Control", "no-store")
            self.end_headers()
            self.wfile.write(body)

        def do_GET(self):
            u = urlparse(self.path)
            if u.path in ("/", "/index.html"):
                self.send_response(302)
                self.send_header("Location", "/live.html")
                self.end_headers()
            elif u.path == "/live.html":
                with open(os.path.join(HERE, "live.html"), "rb") as fh:
                    self.send(200, fh.read(), "text/html; charset=utf-8")
            elif u.path == "/live-data.js":
                body = b"window.GFF_SERVER = true;\nwindow.GFF_LIVE = " + run_json(*default) + b";\n"
                self.send(200, body, "text/javascript; charset=utf-8")
            elif u.path == "/api/run":
                qs = parse_qs(u.query)
                try:
                    n, q, seed = int(qs["n"][0]), int(qs["q"][0]), int(qs["seed"][0])
                    ks = [int(x) for x in qs["k"][0].replace(",", " ").split()]
                    exact = qs.get("exact", ["0"])[0] == "1"
                except (KeyError, ValueError):
                    self.send(400, b"n, q, seed and k must be whole numbers", "text/plain; charset=utf-8")
                    return
                err = check(n, q, seed, ks)
                if err:
                    self.send(400, err.encode(), "text/plain; charset=utf-8")
                    return
                self.send(200, run_json(n, q, seed, ks, exact), "application/json")
            elif u.path == "/api/async":
                qs = parse_qs(u.query)
                try:
                    n, q, seed = int(qs["n"][0]), int(qs["q"][0]), int(qs["seed"][0])
                    ks = [int(x) for x in qs["k"][0].replace(",", " ").split()]
                    exact = qs.get("exact", ["0"])[0] == "1"
                    ai, s = int(qs["author"][0]), qs["strategy"][0]
                    j, p, kind = float(qs["jitter"][0]), float(qs["p"][0]), qs["kind"][0]
                    d, down = int(qs.get("d", ["2"])[0]), float(qs.get("down", ["0"])[0])
                    if kind not in KINDS + PROTOTYPES or not 2 <= d <= 6 or not 0 <= down < 0.5 or not 0 <= p <= 1:
                        raise ValueError
                except (KeyError, ValueError):
                    self.send(400, b"bad request", "text/plain; charset=utf-8")
                    return
                err = check(n, q, seed, ks)
                if err or s not in ("random", "first"):
                    self.send(400, (err or "the strategy is random or first").encode(),
                              "text/plain; charset=utf-8")
                    return
                run_json(n, q, seed, ks, exact)
                w = worlds[(n, q, seed, tuple(ks), exact)]
                if not 0 <= ai < len(w["authors"]):
                    self.send(400, b"no such author", "text/plain; charset=utf-8")
                    return
                b = w["authors"][ai]
                t0 = time.time()
                with lock:
                    paths.set_side(w["n"])
                    ev = run_async(w["adj"], w["friend_sets"], b, w["deciders"][b], w["base"], s, j, p, kind, d, down)
                print("run: author {} {}: {} acts in {:.1f} s".format(
                    b, key_of(kind, s, j, p, d, down), len(ev), time.time() - t0), flush=True)
                self.send(200, json.dumps(ev, separators=(",", ":")).encode(), "application/json")
            else:
                self.send(404, b"not found", "text/plain")

        def log_message(self, *args):
            pass

    httpd = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    url = "http://127.0.0.1:{}/live.html".format(port)
    print("serving " + url + "  (Ctrl-C stops)", flush=True)
    if do_open:
        webbrowser.open(url)
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print()


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--q", type=int, default=1, help="long-range friends per person (default 1)")
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--n", type=int, default=100, help="grid side (default 100)")
    ap.add_argument("--k", type=int, nargs="+", default=[9000, 3000, 2300],
                    help="pick, for each, the author whose number of deciders is nearest (default 9000 3000 2300:"
                         " at q=1, seed 1, everyone follows, about 70%% follow, about 15%% follow)")
    ap.add_argument("--exact-k", action="store_true",
                    help="give each author picked by --k exactly that many deciders (not the tables' draw)")
    ap.add_argument("--authors", type=int, nargs="+", help="author indices (i*n + j); overrides --k")
    ap.add_argument("--strategies", nargs="+", default=["random", "first"], choices=["random", "first"])
    ap.add_argument("--p", type=float, nargs="+", default=list(paths.P_INTRO),
                    help="GFF-PS: probabilities an introduction is taken (default 0 0.25 0.5 1)")
    ap.add_argument("--jitter", type=float, nargs="+", default=list(JITTERS),
                    help="the jitter values offered (a hop takes 1 + jitter * U(0,1); default 0 1 4)")
    ap.add_argument("--file-jitter", type=float, nargs="+", default=[1.0],
                    help="without --serve: the jitters whose runs go into live-data.js (default 1)")
    ap.add_argument("--file-down", type=float, nargs="+", default=[0.0],
                    help="without --serve: the shares of machines down whose runs go into live-data.js (default 0)")
    ap.add_argument("--out", default=os.path.join(HERE, "live-data.js"))
    ap.add_argument("--serve", action="store_true",
                    help="serve live.html, make the runs on demand and rerun the world from the page")
    ap.add_argument("--port", type=int, default=8765)
    ap.add_argument("--open", action="store_true", help="open the page in the browser")
    a = ap.parse_args()

    if a.serve:
        err = check(a.n, a.q, a.seed, a.k)
        if err:
            ap.error(err)
        serve(a.port, (a.n, a.q, a.seed, a.k, a.exact_k), a.strategies, a.p, a.jitter, a.open)
        return

    t0 = time.time()
    file_runs = [(j, d) for j in a.file_jitter for d in a.file_down]
    out = generate(a.n, a.q, a.seed, ks=a.k, authors=a.authors, strategies=a.strategies, ps=a.p,
                   exact=a.exact_k, jitters=a.file_jitter, file_runs=file_runs)
    out["downs"] = list(a.file_down)
    for line in summary(out):
        print(line)
    with open(a.out, "w") as fh:
        fh.write("// Generated by live.py ({}); not to be committed.\n".format(time.strftime("%Y-%m-%d %H:%M")))
        fh.write("window.GFF_LIVE = ")
        json.dump(out, fh, separators=(",", ":"))
        fh.write(";\n")
    print("wrote {} ({:.1f} MB) in {:.1f} s".format(a.out, os.path.getsize(a.out) / 1e6, time.time() - t0))
    page = os.path.join(HERE, "live.html")
    print("open " + page)
    if a.open:
        webbrowser.open("file://" + page)


if __name__ == "__main__":
    main()
