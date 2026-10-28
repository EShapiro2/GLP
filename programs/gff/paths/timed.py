#!/usr/bin/env python3
"""GFF, GFF-PS and GFF-FR in time, with faults, on Kleinberg's grid.

GFF #1's task (Code_inbox.md, 2026-10-28 10:52 UTC, Udi approving): the study
of paths.py extended to compare GFF, GFF-PS and GFF-FR, same grid, seeds and
authors, now with time.  The contracts are programs/gff/GFF.txt, GFF-PS.txt
and GFF-FR.txt as they stand on the branch GFF-chooser (df374e36), clauses and
Algorithms lines.  Python 3 standard library only; written and run with
/usr/bin/python3, Python 3.9.6.  Run:  python3 timed.py   (see README.md).

THE GRAPH AND THE AUTHORS are those of paths.py, drawn by its build_graph and
draw_followers from random.Random(s) in the same order, so that for the same
n, q and s they are the same grid, friendships, authors and followers.

TIME.  Discrete steps.  The friendships of the grid and the long-range ones,
and the following requests (a decided follower coming to want to follow their
author, GFF:2), are put in one random order, interleaved, and one arrives at
each step, 1 .. M.  Each author posts an item every K steps, at an offset of
their own drawn uniformly in 0 .. K-1.  A message (an item, an update, a
request to follow through a friend or to cease) sent at step t is received at
step t + 1: one step per hand.  A friendship takes effect between the two at
once; what each machine does on it (updating, following by friendship) is
done by that machine when it is up.  The run goes on 2K steps after the last
arrival; items posted up to K steps after the last arrival are measured, each
having had at least K steps to arrive.

THE MACHINES (GFF and its Algorithms lines).
- Friends follow each other (GFF:2): the author sends every item to every
  friend (GFF:5).
- Updating (GFF:6, Algorithms): Carol updates every friend whenever Carol
  comes to follow an agent, by friendship or through a friend, and a new
  friend upon befriending with the list of all Carol follows.  An update is a
  message.  Alice's machine learns from updates which friends follow whom, in
  the order the updates arrive.
- Following via Friend (GFF:3, Algorithms): when Alice wants to follow Bob,
  Alice's machine follows Bob through the first friend known to follow Bob,
  and through no other while that following stands; with no friend known to
  follow, Alice waits, and follows when an update tells her of one.  Alice
  follows Bob through Carol by a message to Carol, after which Carol forwards
  Bob's items to Alice (GFF:5); Alice comes to follow Bob, and updates her
  friends, when she sends it.
- A follower who becomes a friend of the author ceases every following of the
  author through a friend, following the author by friendship (GFF:2 confines
  wanting to follow through a friend to a non-friend; the mail is silent).
- No one ceases to want to follow, so Unfollowing (GFF:4) is not exercised.
- Signing (GKK:4 on GFF-chooser): an item carries the signatures of its path,
  the author's first; hands are the number of signatures on the copy received.

DELIVERY (GFF:4 as GFF revised it, 2026-10-28 12:10 UTC): an item is never sent
to one who signed it: Carol forwards to Alice, who follows Bob through Carol,
every item of Bob's feed not signed by Alice, and redundant forwarding
likewise.

GFF-PS (GFF-PS:1 as revised, Algorithms: on arrival).  Alice, who received via
Carol, through whom she follows Bob, an item of Bob's feed signed by Alice's
friend Dave, follows Bob through Dave instead of via Carol (a message to each
of the two); with several such friends, the one nearest the author on the
item's path.  If Alice already follows through Dave (under GFF-FR), the
following via Carol ends, and GFF-FR's Algorithms line adds a second known
friend, other than Carol (else Carol is taken again and left again on every
item that comes via Carol signed by Dave).  An item received from a friend Alice does not follow through
(redundant forwarding) switches nothing.

GFF-FR (GFF-FR:1-2, Algorithms).  Alice's machine follows Bob through two
friends known to follow Bob when there are two, and through one when there is
one, adding a second when one becomes known.  Two variants of the choice:
'random', two chosen uniformly among the friends known to follow; 'far', the
two whose paths to Bob share the fewest agents, the author not counted, a
path being the chain of first followings from the friend to Bob at the time
(the simulator's knowledge: the contract gives Alice no means to learn paths
before items arrive).  Redundant forwarding: Alice, who follows Bob through
Carol and Dave and receives an item first from Carol, forwards it to Dave.

GFF-PS WITH GFF-FR (comparison 3): both rules together, random and far apart.

FAULTS.  Each agent, independently, goes down after a stretch of steps drawn
as ceil(Exp(mean 1/F)) up, and stays down for ceil(Exp(mean D)) steps, F the
fault rate per agent per step and D the mean down stretch.  A down agent takes
nothing: messages to it, and the arrivals concerning it, are kept and taken
when it comes back, in the order sent; a down author does not post.

MEASURES, per item, binned by the author's number of decided followers as in
paths.py.  The item's followers are the decided followers who want to follow
the author and follow them (by friendship or through a friend) when it is
posted; the waiting fraction is of those who want to follow and do not.
Over the item's followers: the share reached by the end of the run, the hands
the first copy took (signatures on it), the steps it took, and the messages
of the item sent, per follower reached.  For GFF and GFF-PS without faults:
the hands against the distance in the friendship graph as it stands at
posting (the tree of shortest paths over the whole graph), and against the
distance in the subgraph of the author and all following them at posting (the
tree over the followers), and the switches.

SEEDS.  Run (q, s): graph and authors from random.Random(s) as in paths.py;
from the same generator then the order of arrivals, the posting offsets, and
one integer from which the faults (one generator per fault setting) and each
scenario's own choices (one generator per scenario) are seeded.
"""

import argparse
import heapq
import math
import os
import random
import statistics
import sys
import time
from collections import Counter, defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from paths import build_graph, draw_followers, bin_of, BIN_LABELS  # noqa: E402

SCENARIOS = ("GFF", "GFF-PS", "GFF-FR-random", "GFF-FR-far", "GFF-PS-FR-random", "GFF-PS-FR-far")


def down_intervals(rng, N, F, D, horizon):
    """Per agent, the sorted list of (start, end) of its down stretches."""
    out = [[] for _ in range(N)]
    if F <= 0:
        return out
    for a in range(N):
        t = 0
        lst = out[a]
        while True:
            t += math.ceil(rng.expovariate(F))
            if t > horizon:
                break
            d = math.ceil(rng.expovariate(1.0 / D))
            lst.append((t, t + d))
            t += d
    return out


def simulate(n, N, adj_full, followers, counts, events, offsets, K, down,
             scenario, srng, measure_trees):
    ps = "PS" in scenario
    fr = "FR" in scenario
    far = scenario.endswith("far")
    target = 2 if fr else 1

    future = [set() for _ in range(N)]  # authors each agent will want
    for b in range(N):
        for f in followers[b]:
            future[f].add(b)
    deciders_of = [set(followers[b]) for b in range(N)]

    adj = [set() for _ in range(N)]          # friendships so far
    want = [set() for _ in range(N)]
    parents = [dict() for _ in range(N)]     # a -> {b: [friends, first first]}
    subs = [dict() for _ in range(N)]        # c -> {b: set of a}
    known = [dict() for _ in range(N)]       # a -> {b: [friends in order learnt]}
    through = [set() for _ in range(N)]      # b -> agents following b through a friend
    switches = [0] * N
    upd_msgs = 0
    ctl_msgs = 0

    M = len(events)
    last = M
    T_end = last + 2 * K
    T_measure = last + K

    # message queue: (time, seq, agent, msg)
    q = []
    seq = 0
    held = [None] * N
    ptr = [0] * N

    def is_down(a, t):
        iv = down[a]
        if not iv:
            return False
        i = ptr[a]
        while i < len(iv) and iv[i][1] <= t:
            i += 1
        ptr[a] = i
        return i < len(iv) and iv[i][0] <= t

    def send(t, a, msg):
        nonlocal seq
        seq += 1
        heapq.heappush(q, (t, seq, a, msg))

    # items
    items = []          # per item: [b, t_post, targets(set), reached{a:(hands, steps)}, msgs, measured]
    seen = {}           # item id -> set of agents who received it

    def follows(c, b):
        return b in adj[c] or b in parents[c]

    def path_of(c, b):
        out = []
        x = c
        guard = 0
        while x != b and guard < N:
            out.append(x)
            ps_ = parents[x].get(b)
            if ps_:
                x = ps_[0]
            elif b in adj[x]:
                x = b
            else:
                break
            guard += 1
        return out

    def come_to_follow(t, a, b):
        nonlocal upd_msgs
        for f in adj[a]:
            send(t + 1, f, ("U", a, b))
            upd_msgs += 1

    def follow_through(t, a, b, c):
        nonlocal ctl_msgs
        lst = parents[a].setdefault(b, [])
        first = not lst
        lst.append(c)
        send(t + 1, c, ("S", a, b))
        ctl_msgs += 1
        if first:
            through[b].add(a)
            come_to_follow(t, a, b)

    def act(t, a, b, exclude=None):
        """a wants b and is not b's friend: follow through known friends."""
        lst = parents[a].get(b, [])
        need = target - len(lst)
        if need <= 0:
            return
        cands = [c for c in known[a].get(b, ()) if c not in lst and c in adj[a] and c != exclude]
        if not cands:
            return
        if not fr:
            follow_through(t, a, b, cands[0])
            return
        if not far:
            pick = srng.sample(cands, min(need, len(cands)))
        else:
            if lst:
                base = set(path_of(lst[0], b))
                best = min(cands, key=lambda c: len(base & set(path_of(c, b))))
                pick = [best]
            elif len(cands) == 1:
                pick = cands
            else:
                paths = {c: set(path_of(c, b)) for c in cands}
                best, bv = None, None
                for i in range(len(cands)):
                    for j in range(i + 1, len(cands)):
                        v = len(paths[cands[i]] & paths[cands[j]])
                        if bv is None or v < bv:
                            bv, best = v, (cands[i], cands[j])
                pick = list(best)
        for c in pick:
            follow_through(t, a, b, c)

    def handle(t, a, msg):
        nonlocal upd_msgs, ctl_msgs
        kind = msg[0]
        if kind == "I":
            _, iid, sender, path = msg
            it = items[iid]
            b = it[0]
            s = seen[iid]
            first = a not in s
            if first:
                s.add(a)
                if a in it[2] and a not in it[3]:
                    it[3][a] = (len(path), t - it[1])
                newpath = path + (a,)
                signers = set(newpath)
                # an item is never sent to one who signed it (GFF:4, GFF-FR:2)
                for x in subs[a].get(b, ()):
                    if x not in signers:
                        send(t + 1, x, ("I", iid, a, newpath))
                        it[4] += 1
                if fr:
                    lst = parents[a].get(b)
                    if lst and len(lst) == 2 and sender in lst:
                        other = lst[1] if lst[0] == sender else lst[0]
                        if other not in signers:
                            send(t + 1, other, ("I", iid, a, newpath))
                            it[4] += 1
            if ps:
                # GFF-PS:1: received via Carol, a following of a, an item
                # signed by a's friend Dave: follow b through Dave instead of
                # via Carol; Dave the friend signer nearest the author
                lst = parents[a].get(b)
                if lst and sender in lst:
                    for d in path[1:]:
                        if d != sender and d in adj[a]:
                            i = lst.index(sender)
                            send(t + 1, sender, ("X", a, b))
                            ctl_msgs += 1
                            if d in lst:
                                del lst[i]
                            else:
                                lst[i] = d
                                send(t + 1, d, ("S", a, b))
                                ctl_msgs += 1
                            switches[b] += 1
                            if fr and len(lst) < target:
                                # the friend just left is not taken again
                                act(t, a, b, exclude=sender)
                            break
        elif kind == "U":
            _, c, b = msg
            if b in future[a] and c in adj[a]:
                kl = known[a].setdefault(b, [])
                if c not in kl:
                    kl.append(c)
                if b in want[a] and b not in adj[a]:
                    act(t, a, b)
        elif kind == "L":
            _, c, lst = msg
            if c in adj[a]:
                for b in lst:
                    if b in future[a]:
                        kl = known[a].setdefault(b, [])
                        if c not in kl:
                            kl.append(c)
                        if b in want[a] and b not in adj[a]:
                            act(t, a, b)
        elif kind == "S":
            _, x, b = msg
            subs[a].setdefault(b, set()).add(x)
        elif kind == "X":
            _, x, b = msg
            st = subs[a].get(b)
            if st:
                st.discard(x)
        elif kind == "B":  # befriending x, at a's machine
            _, x = msg
            # a now follows x by friendship
            come_to_follow(t, a, x)
            lst = list(adj[a] - {x}) + [b for b in parents[a]]
            send(t + 1, x, ("L", a, lst))
            upd_msgs += 1
            if x in parents[a]:  # following x through friends ends
                for c in parents[a].pop(x):
                    send(t + 1, c, ("X", a, x))
                    ctl_msgs += 1
                through[x].discard(a)
        elif kind == "W":
            _, b = msg
            want[a].add(b)
            if b not in adj[a]:
                act(t, a, b)
        elif kind == "P":
            b = a
            iid = len(items)
            tg = {f for f in deciders_of[b] if b in want[f] and follows(f, b)}
            wanting = sum(1 for f in deciders_of[b] if b in want[f])
            it = [b, t, tg, {}, 0, t <= T_measure, wanting]
            items.append(it)
            seen[iid] = {b}
            if measure_trees and tg:
                it.append(tree_distances(b, tg))
            for f in adj[b]:
                send(t + 1, f, ("I", iid, b, (b,)))
                it[4] += 1

    def tree_distances(b, tg):
        # distance in the friendship graph as it stands, and in the
        # subgraph of b and all following b
        def bfs(allowed):
            dist = {b: 0}
            left = set(tg)
            frontier = [b]
            d = 0
            while left and frontier:
                d += 1
                nxt = []
                for u in frontier:
                    for v in adj[u]:
                        if v not in dist and (allowed is None or v in allowed):
                            dist[v] = d
                            nxt.append(v)
                            left.discard(v)
                frontier = nxt
            return dist
        whole = bfs(None)
        allowed = adj[b] | through[b]
        sub = bfs(allowed)
        return {f: (whole.get(f), sub.get(f)) for f in tg}

    post_at = defaultdict(list)
    for b in range(N):
        post_at[offsets[b]].append(b)

    def deliver(t, a, msg):
        if is_down(a, t):
            if held[a] is None:
                held[a] = []
                end = down[a][ptr[a]][1]
                send(end, a, ("WAKE",))
            held[a].append(msg)
            return
        if msg[0] == "WAKE":
            pending, held[a] = held[a] or [], None
            for m in pending:
                deliver(t, a, m)
            return
        handle(t, a, msg)

    for t in range(1, T_end + 1):
        if t <= M:
            ev = events[t - 1]
            if ev[0] == "F":
                _, x, y = ev
                adj[x].add(y)
                adj[y].add(x)
                deliver(t, x, ("B", y))
                deliver(t, y, ("B", x))
            else:
                _, f, b = ev
                deliver(t, f, ("W", b))
        for b in post_at.get(t % K, ()):
            if not is_down(b, t):
                handle(t, b, ("P",))
        while q and q[0][0] <= t:
            _, _, a, msg = heapq.heappop(q)
            deliver(t, a, msg)

    return items, switches, upd_msgs, ctl_msgs, T_end


def run(args):
    n, q, seed, K, faults, scenarios = args
    t0 = time.time()
    rng = random.Random(seed)
    N = n * n
    adj_full = build_graph(rng, n, q)
    counts, followers = draw_followers(rng, N)
    edges = [(u, v) for u in range(N) for v in adj_full[u] if u < v]
    events = [("F", u, v) for u, v in edges] + [("W", f, b) for b in range(N) for f in followers[b]]
    rng.shuffle(events)
    offsets = [rng.randrange(K) for _ in range(N)]
    base = rng.getrandbits(62)
    M = len(events)
    out = {"n": n, "q": q, "seed": seed, "M": M, "edges": len(edges), "res": {}}
    for fi, (F, D) in enumerate(faults):
        down = down_intervals(random.Random(base + 1000 * (fi + 1)), N, F, D, M + 3 * K)
        frac_down = (sum(min(e, M + 2 * K) - s for iv in down for s, e in iv if s <= M + 2 * K)
                     / (N * (M + 2 * K)))
        for si, sc in enumerate(scenarios):
            ts = time.time()
            srng = random.Random(base + si + 1)
            items, switches, upd, ctl, T_end = simulate(
                n, N, adj_full, followers, counts, events, offsets, K, down, sc, srng,
                measure_trees=(F == 0 and sc in ("GFF", "GFF-PS")))
            out["res"][(F, D, sc)] = summarise(items, switches, counts, upd, ctl, M, T_end,
                                               frac_down, time.time() - ts)
    out["seconds"] = time.time() - t0
    return out


def summarise(items, switches, counts, upd, ctl, M, T_end, frac_down, seconds):
    nb = len(BIN_LABELS)
    S = {
        "items": [0] * nb, "targets": [0] * nb, "reached": [0] * nb, "wanting": [0] * nb,
        "msgs": [0] * nb, "hands": [Counter() for _ in range(nb)],
        "steps": [Counter() for _ in range(nb)], "rw": [Counter() for _ in range(nb)],
        "rs": [Counter() for _ in range(nb)], "switches": [0] * nb,
        "phase": [[Counter(), Counter(), 0, 0] for _ in range(5)],
        "upd": upd, "ctl": ctl, "frac_down": frac_down, "seconds": seconds,
    }
    for b, k in enumerate(counts):
        S["switches"][bin_of(k)] += switches[b]
    for it in items:
        if not it[5]:
            continue
        b, tp, tg, reached, msgs = it[0], it[1], it[2], it[3], it[4]
        bi = bin_of(counts[b])
        S["items"][bi] += 1
        S["targets"][bi] += len(tg)
        S["reached"][bi] += len(reached)
        S["wanting"][bi] += it[6]
        if reached:
            S["msgs"][bi] += msgs
        ph = min(4, (tp - 1) * 5 // M) if tp <= M else 4
        for f, (h, st) in reached.items():
            S["hands"][bi][h] += 1
            S["steps"][bi][st] += 1
            if len(it) > 7:
                dw, ds = it[7][f]
                if dw:
                    S["rw"][bi][(h, dw)] += 1
                    S["phase"][ph][0][(h, dw)] += 1
                if ds:
                    S["rs"][bi][(h, ds)] += 1
                    S["phase"][ph][1][(h, ds)] += 1
    return S


# ---- tables ----

def cstats(c, key=None):
    """mean, median, max of a Counter of values (or of ratios h/d)."""
    if not c:
        return None
    if key is None:
        vals = sorted(c.items())
    else:
        vals = sorted(((h / d), m) for (h, d), m in c.items())
        merged = {}
        for v, m in vals:
            merged[v] = merged.get(v, 0) + m
        vals = sorted(merged.items())
    n = sum(m for _, m in vals)
    mean = sum(v * m for v, m in vals) / n
    half = (n - 1) / 2
    lo = hi = None
    acc = 0
    for v, m in vals:
        if lo is None and acc + m > math.floor(half):
            lo = v
        if acc + m > math.ceil(half):
            hi = v
            break
        acc += m
    if hi is None:
        hi = vals[-1][0]
    return mean, (lo + hi) / 2, vals[-1][0]


def fmt3(s, ratio=False):
    if s is None:
        return "- | - | -"
    if ratio:
        return "{:.3f} | {:.3f} | {:.3f}".format(*s)
    med = s[1]
    return "{:.2f} | {} | {}".format(s[0], int(med) if med == int(med) else "{:.1f}".format(med), s[2])


def pool(results, key):
    nb = len(BIN_LABELS)
    P = None
    for r in results:
        S = r["res"][key]
        if P is None:
            P = {k: (v if not isinstance(v, list) else None) for k, v in S.items()}
            for k in ("items", "targets", "reached", "wanting", "msgs", "switches"):
                P[k] = [0] * nb
            for k in ("hands", "steps", "rw", "rs"):
                P[k] = [Counter() for _ in range(nb)]
            P["phase"] = [[Counter(), Counter()] for _ in range(5)]
            P["upd"] = P["ctl"] = 0
            P["seconds"] = 0.0
            P["frac_down"] = []
        for k in ("items", "targets", "reached", "wanting", "msgs", "switches"):
            for i in range(nb):
                P[k][i] += S[k][i]
        for k in ("hands", "steps", "rw", "rs"):
            for i in range(nb):
                P[k][i].update(S[k][i])
        for i in range(5):
            P["phase"][i][0].update(S["phase"][i][0])
            P["phase"][i][1].update(S["phase"][i][1])
        P["upd"] += S["upd"]
        P["ctl"] += S["ctl"]
        P["seconds"] += S["seconds"]
        P["frac_down"].append(S["frac_down"])
    return P


def write_tables(q, F, D, results, scenarios, outdir, K, n):
    results = sorted(results, key=lambda r: r["seed"])
    seeds = [r["seed"] for r in results]
    nb = len(BIN_LABELS)
    L = ["# GFF, GFF-PS and GFF-FR in time, q = {}, fault rate {}{}".format(
        q, F, "" if F == 0 else ", mean down stretch {} steps".format(D)), ""]
    L.append("Kleinberg's {0} x {0} grid, {1} long-range friendship(s) per person; seeds {2}, pooled; "
             "each author posts every {3} steps; produced by programs/gff/paths/timed.py (see README.md).".format(
                 n, q, ", ".join(map(str, seeds)), K))
    if F:
        fd = [x for r in results for x in [r["res"][(F, D, scenarios[0])]["frac_down"]]]
        L.append("Fraction of agent-steps down, per seed: " + ", ".join("{:.4f}".format(x) for x in fd) + ".")
    L.append("")
    L.append("Per item: its followers are the decided followers following the author at posting; share is of them "
             "reached by the end of the run; hands are the signatures on the first copy received; steps the steps "
             "it took; messages are the item's messages sent, over the followers reached (items reaching none "
             "left out); waiting is the share of decided followers wanting to follow at posting who do not follow.")
    L.append("")
    for sc in scenarios:
        P = pool(results, (F, D, sc))
        L.append("## {}".format(sc))
        L.append("")
        L.append("| followers | items | waiting | share reached | hands mean | median | max | steps mean | median | max | messages per follower reached | switches |")
        L.append("|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|")
        for bi in list(range(nb)) + [None]:
            if bi is None:
                idx = range(nb)
                label = "all"
            else:
                idx = [bi]
                label = BIN_LABELS[bi]
            it = sum(P["items"][i] for i in idx)
            tg = sum(P["targets"][i] for i in idx)
            rc = sum(P["reached"][i] for i in idx)
            wa = sum(P["wanting"][i] for i in idx)
            ms = sum(P["msgs"][i] for i in idx)
            hc = Counter()
            sc_ = Counter()
            for i in idx:
                hc.update(P["hands"][i])
                sc_.update(P["steps"][i])
            sw = sum(P["switches"][i] for i in idx)
            L.append("| {} | {} | {} | {} | {} | {} | {} | {} |".format(
                label, it, "{:.4f}".format(1 - tg / wa) if wa else "-",
                "{:.4f}".format(rc / tg) if tg else "-", fmt3(cstats(hc)), fmt3(cstats(sc_)),
                "{:.2f}".format(ms / rc) if rc else "-", sw))
        L.append("")
        if F == 0 and sc in ("GFF", "GFF-PS"):
            L.append("Hands against the tree of shortest paths at posting, over the whole friendship graph and over the followers' subgraph (ratio of hands to distance):")
            L.append("")
            L.append("| followers | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |")
            L.append("|---|---:|---:|---:|---:|---:|---:|")
            for bi in range(nb):
                L.append("| {} | {} | {} |".format(BIN_LABELS[bi], fmt3(cstats(P["rw"][bi], 1), True),
                                                   fmt3(cstats(P["rs"][bi], 1), True)))
            L.append("")
            L.append("The same by the fifth of the arrivals in which the item was posted (the fifth includes the tail after the last arrival):")
            L.append("")
            L.append("| fifth | over the whole graph: ratio mean | median | max | over the followers: ratio mean | median | max |")
            L.append("|---:|---:|---:|---:|---:|---:|---:|")
            for i in range(5):
                L.append("| {} | {} | {} |".format(i + 1, fmt3(cstats(P["phase"][i][0], 1), True),
                                                   fmt3(cstats(P["phase"][i][1], 1), True)))
            L.append("")
        L.append("Update messages {}, messages to follow through or cease to follow through a friend {}; run time {:.0f} s over {} seeds.".format(
            P["upd"], P["ctl"], P["seconds"], len(results)))
        L.append("")
    os.makedirs(outdir, exist_ok=True)
    name = "timed-q{}-f{}.md".format(q, F if F else 0)
    with open(os.path.join(outdir, name), "w") as fh:
        fh.write("\n".join(L))


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--q", type=int, nargs="+", default=[1, 2, 4])
    ap.add_argument("--seeds", type=int, nargs="+", default=[1, 2, 3, 4, 5])
    ap.add_argument("--n", type=int, default=100)
    ap.add_argument("--K", type=int, default=5000, help="steps between an author's posts")
    ap.add_argument("--faults", type=float, nargs="+", default=[0, 0.00001, 0.0001],
                    help="fault rates per agent per step (0 is no faults)")
    ap.add_argument("--down", type=float, default=1000, help="mean down stretch in steps")
    ap.add_argument("--scenarios", nargs="+", default=list(SCENARIOS))
    ap.add_argument("--out", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "tables"))
    a = ap.parse_args()
    faults = [(F, a.down if F else 0) for F in a.faults]
    t0 = time.time()
    results = []
    for qq in a.q:
        for s in a.seeds:
            r = run((a.n, qq, s, a.K, faults, a.scenarios))
            results.append(r)
            print("q={} seed={} arrivals={} {:.0f}s  ".format(qq, s, r["M"], r["seconds"]) +
                  " ".join("{}@{}={:.0f}s".format(sc, F, r["res"][(F, D, sc)]["seconds"])
                           for F, D in faults for sc in a.scenarios), flush=True)
    for qq in a.q:
        for F, D in faults:
            write_tables(qq, F, D, [r for r in results if r["q"] == qq], a.scenarios, a.out, a.K, a.n)
    print("total {:.0f}s".format(time.time() - t0))


if __name__ == "__main__":
    main()
