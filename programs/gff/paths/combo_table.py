"""The table of combo_sim.py's results (tables/combo.json, or a file given), as
async_sim.py's README reports its own: means over seeds and authors, per graph;
hops at item 80; on time, reached, copies and time to arrive over items 30-70;
followers cut off from the author, the most at any item and the share at the end."""
import json
import os
import sys
from collections import defaultdict

path = sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "tables", "combo.json")
runs = json.load(open(path))["runs"]


def label(r):
    s = []
    if r["redundancy"] > 1:
        s.append("FR-BC d={}".format(r["redundancy"]) if r["back_channel"] else "FR d={}".format(r["redundancy"]))
    if r["shorten_rate"] > 0:
        s.append("PS p={}{}".format(r["p"], " guarded" if r.get("guard") else ""))
    return " + ".join(s) or "one pointer"


def mean(xs):
    xs = [x for x in xs if x is not None]
    return sum(xs) / len(xs) if xs else float("nan")


acc = defaultdict(lambda: defaultdict(list))
graphs = []
for r in runs:
    g = r["graph"]
    if g not in graphs:
        graphs.append(g)
    key = (r["down_frac"], label(r), g)
    S = {s["item"]: s for s in r["samples"]}
    at80 = S.get(80) or r["samples"][-1]
    mid = [S[i] for i in range(30, 71) if i in S]
    a = acc[key]
    a["hops"].append(at80["hops"])
    a["max_hops"].append(at80["max_hops"])
    a["best"].append(r["summary"].get("best_tree"))
    a["cut"].append(max(s["cut_off"] for s in r["samples"]))
    a["fanin"].append(at80["fanin"])
    a["load"].append(at80["load"])
    a["ontime"].append(mean(s["on_time_share"] for s in mid))
    a["reach"].append(mean(s["reach_share"] for s in mid))
    a["cutend"].append(r["samples"][-1]["cut_off"] / max(1, r["samples"][-1]["following"]))
    a["lat"].append(mean(s["latency"] for s in mid))
    fol = mean(s["following"] for s in mid)
    if fol and S.get(70) and S.get(30):
        a["copies"].append((S[70]["messages"] - S[30]["messages"]) / (40 * fol))
    a["shortens"].append(r["summary"]["shortens"] / max(1, r["summary"]["followers_final"]))

order = ["one pointer", "PS p=0.25", "PS p=0.5", "FR-BC d=2"]
for q in ("0.25", "0.5", "1.0"):
    order += ["FR-BC d=2 + PS p=" + q, "FR-BC d=2 + PS p=" + q + " guarded"]
order += ["FR-BC d=3", "FR-BC d=3 + PS p=0.5", "FR-BC d=3 + PS p=0.5 guarded",
          "FR d=2", "FR d=2 + PS p=0.5", "FR d=2 + PS p=0.5 guarded"]
cols = [("hops", "hops@80"), ("best", "best tree"), ("max_hops", "max hops"), ("lat", "time to arrive"),
        ("ontime", "on time"), ("reach", "reached"), ("copies", "copies"), ("fanin", "author fan-in"),
        ("load", "max load"), ("cut", "cut off (max)"), ("cutend", "cut off at end"), ("shortens", "shortens/follower")]
for f in sorted({k[0] for k in acc}):
    print("\n## {:.0%} of machines down   (small world / uniform / 6-regular)\n".format(f))
    print("| setting | " + " | ".join(c[1] for c in cols) + " |")
    print("|---|" + "---:|" * len(cols))
    for lab in order:
        if (f, lab, graphs[0]) not in acc:
            continue
        cells = []
        for c, _ in cols:
            vals = [mean(acc[(f, lab, g)][c]) for g in graphs]
            fmt = "{:.2%}" if c == "cutend" else "{:.0%}" if c in ("ontime", "reach") else ("{:.0f}" if c in ("fanin", "load", "cut") else "{:.1f}")
            if c == "copies" or c == "shortens":
                fmt = "{:.2f}"
            cells.append(" / ".join(fmt.format(v) for v in vals))
        print("| {} | {} |".format(lab, " | ".join(cells)))
