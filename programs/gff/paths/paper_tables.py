#!/usr/bin/env python3
"""The GFF paper's tables, in LaTeX, from the simulators' results.

  python3 paper_tables.py            # prints both tables to stdout
  python3 paper_tables.py --out tables/paper   # writes paths.tex and async.tex there

Table tab:paths (Section Path Shortening) is read from paths.py's tables,
tables/gff-ps-q1.md, -q2.md and -q4.md (python3 paths.py): Kleinberg's
100 x 100 grid, five seeds pooled, every person an author with a power-law
number of followers.  Its first part is by the author's number of followers at
q = 2; its second, the authors with thousands of followers at q = 1, 2 and 4.
The first following is Labelled Overlay:1 with the friend chosen at random;
shortening is Short Feeds:1, each introduction it needs taken with
probability p.

Table tab:async (Section Fault-Resilient Feeds) is read from async_sim.py's
paper experiment, tables/async-paper.json (python3 async_sim.py --paper):
the same grid at q = 1, 2 and 4, seeds 1-5, three authors a seed with 5,000
followers each, 80 items, every act at its own moment; Feeds (one pointer),
Short Feeds, Resilient Feeds and Backchannel Feeds, with 0, 1 and 10% of
machines down.  Means over seeds and authors: hops along all pointers at
item 80; over items 30-70, the time from posting to arrival (time units, an
item posted every 10, a hop taking 1 to 2), the share of followers reached
within 5 periods and by the end of the run, and the copies sent per follower
per item.
"""
import argparse
import json
import os
import re
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
TABLES = os.path.join(HERE, "tables")
BIG = "1000-9999"


def md_sections(path):
    """{heading: [row dicts]} for every markdown table under a ### or ## heading."""
    out, head, cols = {}, None, None
    for line in open(path):
        line = line.rstrip("\n")
        if line.startswith("#"):
            head, cols = line.lstrip("#").strip(), None
        elif line.startswith("|") and head:
            cells = [c.strip() for c in line.strip("|").split("|")]
            if cols is None:
                cols = cells
                # median and max after "path mean" / "ratio mean" / "fan-in mean" are named by it
                named, prefix = [], ""
                for c in cols:
                    if c.endswith(" mean"):
                        prefix = c[:-5] + " "
                    named.append(prefix + c if c in ("median", "max") else c)
                cols = named
            elif set(line) <= set("|-: "):
                continue
            else:
                out.setdefault(head, []).append(dict(zip(cols, cells)))
    return out


def row(sections, head, followers):
    for r in sections[head]:
        if r["followers"] == followers:
            return r
    raise KeyError((head, followers))


def num(s):
    return float(s.replace(",", ""))


def fmt_int(s):
    return "{:,}".format(int(num(s)))


def fmt(x, d=1):
    return "{:.{}f}".format(num(x) if isinstance(x, str) else x, d)


def paths_table():
    S = {q: md_sections(os.path.join(TABLES, "gff-ps-q{}.md".format(q))) for q in (1, 2, 4)}
    first = "Strategy random"
    short = "Strategy random, p = {}"
    tree = "Shortest-path tree over the followers' subgraph (the author, the author's friends and the deciders who follow)"
    dist = "Graph distance from follower to author in the friendship graph (ratio 1)"
    L = []
    L.append(r"\begin{table}[t]")
    L.append(r"\caption{Paths on Kleinberg's grid, five seeds pooled.  Above, $q = 2$, by the author's number of "
             r"followers: the followers who follow and the fraction waiting; the mean and the greatest path length "
             r"at the first following, each follower choosing at random a friend who follows (\cl{Labelled Overlay}{friend}), "
             r"after shortening by Short Feeds with each introduction taken with probability $p = 0.5$ (short.), and in the "
             r"shortest-path tree over the followers (tree); and the mean graph distance (dist.).  Below, the authors with thousands "
             r"of followers for each $q$: the mean path length at the first following, after shortening with "
             r"$p = 0.25$, $0.5$ and $1$, in the best tree, and the graph distance, and the author's mean fan-in, the "
             r"persons with a pointer to the author, at the first following and after shortening with $p = 0.5$ and $1$.}")
    L.append(r"\label{tab:paths}")
    L.append(r"\centering")
    L.append(r"\scriptsize")
    L.append(r"\setlength{\tabcolsep}{3pt}")
    L.append(r"\begin{tabular}{@{}lrrrrrrrrr@{}}")
    L.append(r"\hline")
    L.append(r"followers & following & waiting & first & max & short. & max & tree & max & dist. \\")
    L.append(r"\hline")
    s = S[2]
    for b, lab in (("1-9", "1--9"), ("10-99", "10--99"), ("100-999", "100--999"), (BIG, "1000--9999")):
        f, p5, t, d = row(s, first, b), row(s, short.format(0.5), b), row(s, tree, b), row(s, dist, b)
        L.append("{} & {} & {:.3f} & {} & {} & {} & {} & {} & {} & {} \\\\".format(
            lab, fmt_int(f["following"]), num(f["waiting"]), fmt(f["path mean"], 2 if num(f["path mean"]) < 10 else 1),
            f["path max"], fmt(p5["path mean"], 2 if num(p5["path mean"]) < 10 else 1), p5["path max"],
            fmt(t["path mean"], 2), t["path max"], fmt(d["mean"], 2)))
    L.append(r"\hline")
    L.append(r"\multicolumn{10}{@{}l}{authors with 1000--9999 followers} \\")
    L.append(r"$q$ & first & $p{=}0.25$ & $p{=}0.5$ & $p{=}1$ & tree & dist. & fan-in & $p{=}0.5$ & $p{=}1$ \\")
    L.append(r"\hline")
    for q in (1, 2, 4):
        s = S[q]
        f = row(s, first, BIG)
        p = {x: row(s, short.format(x), BIG) for x in (0.25, 0.5, 1)}
        t, d = row(s, tree, BIG), row(s, dist, BIG)
        L.append("{} & {} & {} & {} & {} & {} & {} & {} & {} & {} \\\\".format(
            q, fmt(f["path mean"]), fmt(p[0.25]["path mean"]), fmt(p[0.5]["path mean"]), fmt(p[1]["path mean"]),
            fmt(t["path mean"], 2), fmt(d["mean"], 2), fmt(f["fan-in mean"]), fmt(p[0.5]["fan-in mean"]),
            fmt_int(str(round(num(p[1]["fan-in mean"]))))))
    L.append(r"\hline")
    L.append(r"\end{tabular}")
    L.append(r"\end{table}")
    return "\n".join(L)


CONTRACT_ORDER = ["Feeds", "Short Feeds, $p = 0.25$", "Short Feeds, $p = 0.5$",
                  "Resilient Feeds, $d = 2$", "Backchannel Feeds, $d = 2$",
                  "Resilient Feeds, $d = 3$", "Backchannel Feeds, $d = 3$"]


def contract(r):
    if r["redundancy"] > 1:
        return "{} Feeds, $d = {}$".format("Backchannel" if r["back_channel"] else "Resilient", r["redundancy"])
    if r["shorten_rate"] > 0:
        return "Short Feeds, $p = {:g}$".format(r["p"])
    return "Feeds"


def mean(xs):
    xs = [x for x in xs if x is not None]
    return sum(xs) / len(xs) if xs else float("nan")


def async_stats(path):
    data = json.load(open(path))
    acc = defaultdict(lambda: defaultdict(list))
    for r in data["runs"]:
        q = int(re.search(r"q(\d+)", r["graph"]).group(1)) if re.search(r"q(\d+)", r["graph"]) else r["graph"]
        key = (r["down_frac"], contract(r), q)
        S = {s["item"]: s for s in r["samples"]}
        at80 = S.get(80) or r["samples"][-1]
        mid = [S[i] for i in range(30, 71) if i in S]
        a = acc[key]
        a["hops"].append(at80["hops"])
        a["best"].append(r["summary"].get("best_tree"))
        a["dist"].append(r["summary"].get("graph_distance"))
        a["cut"].append(max(s["cut_off"] for s in r["samples"]))
        a["fanin"].append(at80["fanin"])
        a["lat"].append(mean(s["latency"] for s in mid))
        a["ontime"].append(mean(s["on_time_share"] for s in mid))
        a["reach"].append(mean(s["reach_share"] for s in mid))
        fol = mean(s["following"] for s in mid)
        if fol and S.get(70) and S.get(30):
            a["copies"].append((S[70]["messages"] - S[30]["messages"]) / (40 * fol))
    return data["meta"], {k: {c: mean(v) for c, v in d.items()} for k, d in acc.items()}


def async_table(path):
    meta, st = async_stats(path)
    qs = sorted({k[2] for k in st})
    downs = sorted({k[0] for k in st})
    L = []
    L.append(r"\begin{table}[t]")
    L.append(r"\caption{The four contracts in time on Kleinberg's grid, five seeds and three authors a seed with "
             r"5{,}000 followers each, the author posting an item every 10 time units and a message taking 1 to 2.  "
             r"Without faults: the mean path length along all pointers after the 80th item, the mean time from "
             r"posting to arrival, and the copies sent per follower per item.  With one and ten machines in a hundred "
             r"down: the per cent of an item's followers it reached within 50 time units, and with ten down, by the "
             r"end of the run, at least 100 time units after posting.  The tree of shortest paths over the followers "
             r"has mean path length " + ", ".join("{:.1f}".format(st[(0.0, "Feeds", q)]["best"]) for q in qs) +
             r" and the graph distance is " + ", ".join("{:.1f}".format(st[(0.0, "Feeds", q)]["dist"]) for q in qs) +
             r" with $q = " + ", ".join(map(str, qs)) + r"$.}")
    L.append(r"\label{tab:async}")
    L.append(r"\centering")
    L.append(r"\scriptsize")
    L.append(r"\setlength{\tabcolsep}{3pt}")
    L.append(r"\begin{tabular}{@{}lrrrrrrr@{}}")
    L.append(r"\hline")
    L.append(r"& & \multicolumn{3}{c}{no faults} & \multicolumn{2}{c}{on time, down} & reached \\")
    L.append(r"contract & $q$ & path & time & copies & 1\% & 10\% & 10\% down \\")
    for c in CONTRACT_ORDER:
        if (0.0, c, qs[0]) not in st:
            continue
        L.append(r"\hline")
        for i, q in enumerate(qs):
            z = st[(0.0, c, q)]
            cells = [c if i == 0 else "", str(q), "{:.1f}".format(z["hops"]), "{:.1f}".format(z["lat"]),
                     "{:.2f}".format(z["copies"])]
            cells += ["{:.0f}".format(st[(f, c, q)]["ontime"] * 100) for f in downs[1:]]
            cells.append("{:.0f}".format(st[(downs[-1], c, q)]["reach"] * 100))
            L.append(" & ".join(cells) + r" \\")
    L.append(r"\hline")
    L.append(r"\end{tabular}")
    L.append(r"\end{table}")
    return "\n".join(L)


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--async-json", default=os.path.join(TABLES, "async-paper.json"))
    ap.add_argument("--out", default=None, help="a directory to write paths.tex and async.tex into")
    a = ap.parse_args()
    t1 = paths_table()
    t2 = async_table(a.async_json) if os.path.exists(a.async_json) else "% no " + a.async_json
    if a.out:
        os.makedirs(a.out, exist_ok=True)
        for name, t in (("paths.tex", t1), ("async.tex", t2)):
            with open(os.path.join(a.out, name), "w") as fh:
                fh.write(t + "\n")
    print(t1 + "\n\n" + t2)


if __name__ == "__main__":
    main()
