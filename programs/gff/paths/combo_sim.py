#!/usr/bin/env python3
"""A prototype: GFF-FR-BC with GFF-PS:1's shortening added, which no contract has yet.

A copy of async_sim.py (2026-11-14) for the question in Yuval's notes, "GFF:
uniting GFF-PS with GFF-FR-BC".  No contract combines redundancy with
shortening, so async_sim.py turns shortening off when d > 1; here two flags
turn it back on:
  combine  GFF-PS:1 read literally over several pointers: Alice has a
           Dave-pointer to Bob and none to Carol, Bob has one to Carol; one
           such pair (Bob, Carol) at random.  It can close a loop: Bob's
           redundant pointer may lead to someone who follows through Alice,
           and a group whose every pointer stays inside it has no pointer
           path to the author (cut_off in the samples).
  guard    GFF-PS:1 with a stronger body: Alice received item x from Bob by
           delivery, Bob received x from Carol by delivery, and Bob still
           points to Carol; x the latest item Alice received through a pointer
           she still has.  Far fewer loops, but not none.
A setting without these flags runs exactly as async_sim.py's, draw for draw
(the flags enter the run's seed only when set).  Everything below is
async_sim.py's description, which holds unchanged for the rest.

The acts of async_sim.py: follow_friend (GDO-L:1) and deliver (GFF-I:2), which
every run has, and the acts that improve the paths:
  GFF-PS     shorten (GFF-PS:1) with GKK's introductions, if shorten_rate > 0;
  GFF-FR     redundant_follow (GFF-FR:1), if redundancy = d > 1;
  GFF-FR-BC  redundant_follow and back_forward (GFF-FR-BC:1), if also
             back_channel.

TIME.  An item hop (one delivery) takes 1 + jitter * U(0, 1) time units, drawn
per message.  The author posts item 1, 2, ... every `period` units from time
`period` until `end`.

THE FIRST FOLLOWING (GDO-L:1, willed).  Each decider decides at a time drawn
uniformly from [0, decide].  A decider with a friend who has a Dave-pointer
follows through one (strategy random: uniformly; first: first in the friend
list); otherwise waits.  When a person comes to have a Dave-pointer, each
waiting friend learns of it one hop later and follows then.  The author's
friends have their friend pointers from the start.

DELIVERY (GFF-I:2, unwilled).  When Bob comes to hold an item, a copy is sent
to everyone with a Dave-pointer to Bob; on arrival it is taken only if that
pointer still exists (deliver needs it) and Alice does not hold the item yet
(deliver needs that too: so the first copy is the one taken, and
received(Alice, x, Dave, Bob) names its sender).  Bob sends a copy along every
pointer to him, not knowing who holds the item already; a copy that arrives
second is discarded.  A new pointer to Bob brings every item Bob holds that
Alice lacks (deliver: "every item Bob holds"), as does a machine coming back
up.  Every copy sent is counted as a message.

SHORTENING (GFF-PS:1, unwilled), if shorten_rate > 0.  Every follower who is
not the author's friend is examined at rate shorten_rate / period: with Bob
the one it points to and Carol the one Bob points to now, it repoints to
Carol as in paths.shorten: introductions taken with probability p, a refusal
not asked again, friends knowing each other.  Each follower has one pointer
and shortening moves it to the follower's grandparent, so the pointers stay a
tree rooted at the author.

REDUNDANCY (GFF-FR:1, redundant_follow, unwilled), if redundancy = d > 1.
Whoever has a Dave-pointer and fewer than d of them adds pointers to friends
who have one, at random, one hop after they become available (on following,
when a friend comes to follow, on coming back up).  The clause excludes no
one, so the author's friends add them too.

BACK-CHANNEL (GFF-FR-BC:1, back_forward, unwilled), if back_channel.  Alice,
who received an item by delivery first from Bob, sends it to each other person
she has a Dave-pointer to; one who lacks it takes it (received_back) and passes
it on by delivery, but not back again (back_forward needs a delivery's
received atom).  The act stays enabled while Alice points to both and Carol
lacks the item, so, as the paper's Proposition Back-channel has it, Carol comes
to hold it: Alice forwards again what Carol lacks when either comes back up
and when Alice adds a pointer to Carol.

CRASH FAULTS, if down_frac > 0.  Every agent but the author alternates up and
down: down for Exp(mean down_mean periods), up for Exp(mean chosen so that it
is down a fraction down_frac of the time), starting in that state with that
probability.  A machine that is down takes nothing (copies sent to it are
lost), sends nothing, and decides, wakes and shortens only once up; on coming
back up it receives what it missed from everyone it points to, and sends what
they missed to everyone pointing to it.

MEASURES, sampled as each item is posted: over the deciders who follow, the
mean and max hops (shortest path along all pointers), how many are cut off
(no pointer path to the author; none should be, the pointers staying
connected to the author), the author's fan-in, the most followers pointing to
one follower, and the counters (shortenings, introductions, refusals,
redundant pointers, back-forwards, messages); per item, by the end of the
run, the mean time from posting to arrival over the deciders it reached who
were following when it was posted, how many, the share of those following it
reached, and the share it reached within ON_TIME (5) periods.

  python3 combo_sim.py            # writes tables/combo.json, about 9 minutes:
                                  # three graphs, n = 100, density 0.5, seeds 1-3,
                                  # 3 authors; one pointer, GFF-PS, GFF-FR-BC and
                                  # GFF-FR (d = 2, 3) with and without shortening,
                                  # guarded or not, at 0 and 10% of machines down
  python3 combo_table.py          # the table of means from tables/combo.json
"""

import argparse
import json
import os
import random
import time
from collections import deque
from heapq import heappop, heappush
from multiprocessing import Pool

import paths

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULTS = {"period": 10.0, "decide": 200.0, "end": 800.0, "jitter": 1.0,
            "shorten_rate": 0.0, "p": 0.0, "strategy": "random",
            "redundancy": 1, "back_channel": False, "down_frac": 0.0, "down_mean": 5.0}
ON_TIME = 5.0   # an item is on time if it arrives within this many periods of posting


def simulate(adj, friend_sets, b, deciders, rng, period=10.0, decide=200.0, end=800.0, jitter=1.0,
             shorten_rate=0.0, p=0.0, strategy="random", log=None,
             redundancy=1, back_channel=False, down_frac=0.0, down_mean=5.0, final=None, combine=False, guard=False):
    """One author b and its deciders.  Returns (samples, summary, the set of
    persons with a Dave-pointer at the end).  log, if given, gets the acts in
    time order: ("i", x) item posted; ("f", a, via, woken); ("w", a);
    ("s", a, carol, intros, friend, bob) a shortening (GFF-PS), bob the
    pointer it replaces (live.html needs it once a follower has several);
    ("r", a, carol, intros) an introduction refused; ("a", a, x) a redundant
    pointer (GFF-FR); ("D", a) a machine goes down, ("U", a) comes back up.
    shorten_rate > 0 is GFF-PS; redundancy > 1 is GFF-FR's d (back_channel:
    GFF-FR-BC); no contract combines redundancy with shortening, so it turns
    shortening off unless combine (the prototype, with guard its guarded
    version; see the module docstring)."""
    if redundancy > 1 and not combine:
        shorten_rate = 0.0
    dec = set(deciders)
    afriends = set(adj[b])
    ptr = {f: [b] for f in afriends}            # each follower's Dave-pointers
    children = {b: set(afriends)}                # who points to whom
    held = {b: set()}                            # items held
    latest = {}                                  # the latest item each holds
    waiting = set()
    since = {f: 0 for f in afriends}             # the last item posted before each one followed
    known, refused = set(), set()
    c = dict(shortens=0, intros=0, refusals=0, messages=0,
             redundant=0, back_sent=0, back_taken=0, lost=0, downs=0)
    down = set()
    fol_at_post = {}                             # item -> deciders following when it was posted
    recv_from = {}                               # a -> {item: whom a received it from by delivery}
    delays = {}                                  # item -> delays of those it reached
    posted = 0
    post_time = {}
    arrivals = {}                                # item -> [sum of delays, count] over deciders
    samples = []
    q = []
    seq = [0]

    def push(t, kind, *data):
        seq[0] += 1
        heappush(q, (t, seq[0], kind, data))

    def hop():
        return 1.0 + jitter * rng.random()

    def emit(e):
        if log is not None:
            log(e)

    def catch_up(a, x, t):
        # deliver (GFF-I:2): a, pointing to x, receives every item x holds that a lacks
        if x in down:
            return
        for item in held.get(x, ()):
            if item not in held.get(a, ()):
                c["messages"] += 1
                push(t + hop(), "arrive", a, item, x)

    def back_catch_up(a, c_, t):
        # back_forward (GFF-FR-BC:1) stays enabled while a points to c_ and to the
        # one a received an item from, and c_ lacks it; the paper's Proposition
        # Back-channel: c_ comes to hold it.  Taken whenever it becomes enabled again.
        if not back_channel or a in down or c_ in down or c_ == b:
            return
        for item, frm in recv_from.get(a, {}).items():
            if frm != c_ and frm in ptr.get(a, ()) and item not in held.get(c_, ()):
                c["back_sent"] += 1
                c["messages"] += 1
                push(t + hop(), "back", c_, item, a)

    def add_pointer(a, x, t):
        ptr.setdefault(a, []).append(x)
        children.setdefault(x, set()).add(a)
        catch_up(a, x, t)
        back_catch_up(a, x, t)

    def remove_pointer(a, x):
        ptr[a].remove(x)
        children[x].discard(a)

    def start_clocks(a, t):
        if a in afriends:
            return
        if shorten_rate > 0:
            push(t + rng.expovariate(shorten_rate / period), "shorten", a)

    def follow(a, t, woken):
        cands = [x for x in adj[a] if x in ptr]
        if not cands:
            return False
        via = rng.choice(cands) if strategy == "random" else cands[0]
        since[a] = posted
        add_pointer(a, via, t)
        emit(("f", a, via, woken))
        start_clocks(a, t)
        for v in adj[a]:
            if v in waiting:
                waiting.discard(v)
                push(t + hop(), "wake", v)
            elif redundancy > 1 and v in ptr and len(ptr[v]) < redundancy:
                push(t + hop(), "redund", v)     # v may now add a's pointer (GFF-FR:1)
        if redundancy > 1:
            push(t + hop(), "redund", a)
        return True

    def send(x, item, t):
        if x in down:
            return
        for a in children.get(x, ()):   # a copy to everyone pointing to x; x can't know who holds it already
            c["messages"] += 1
            push(t + hop(), "arrive", a, item, x)

    def sample(t):
        # hops along all pointers, by BFS from the author over who points to whom
        dist = {b: 0}
        dq = deque([b])
        while dq:
            x = dq.popleft()
            for a in children.get(x, ()):
                if a not in dist:
                    dist[a] = dist[x] + 1
                    dq.append(a)
        fol = [a for a in dec if a in ptr]
        reach = [dist[a] for a in fol if a in dist]
        load = max((len(children[x]) for x in children if x != b and children[x]), default=0)
        samples.append({
            "item": posted, "time": t, "following": len(fol), "waiting": len(waiting),
            "hops": sum(reach) / len(reach) if reach else None, "max_hops": max(reach, default=0),
            "cut_off": len(fol) - len(reach), "fanin": len(children.get(b, ())), "load": load,
            "down": len(down), **c})

    for a in dec:
        if a not in afriends:
            push(rng.uniform(0, decide), "decide", a)
    if redundancy > 1:
        for f in afriends:
            push(hop(), "redund", f)
    up_mean = down_mean * period * (1 - down_frac) / down_frac if down_frac > 0 else None
    if down_frac > 0:   # crash faults: each agent but the author is down a fraction down_frac of the time
        for a in afriends | dec:
            if rng.random() < down_frac:
                down.add(a)
                emit(("D", a))
                push(rng.expovariate(1 / (down_mean * period)), "up", a)
            else:
                push(rng.expovariate(1 / up_mean), "down", a)
    t_post = period
    while t_post <= end + 1e-9:
        push(t_post, "post")
        t_post += period
    while q:
        t, _, kind, data = heappop(q)
        if t > end + 1e-9:
            break
        if kind == "arrive" or kind == "back":
            a, item, frm = data
            if a in down:
                c["lost"] += 1                   # sent to a machine that is down
                continue
            if kind == "arrive" and frm not in ptr.get(a, ()):
                continue                         # the pointer is gone: no delivery
            h = held.setdefault(a, set())
            if item in h:
                continue                         # a later copy: already held
            h.add(item)
            if latest.get(a, 0) < item:
                latest[a] = item
            if a in dec and since.get(a, item) < item:   # following when it was posted
                s = arrivals.setdefault(item, [0.0, 0])
                s[0] += t - post_time[item]
                s[1] += 1
                delays.setdefault(item, []).append(t - post_time[item])
            send(a, item, t)
            if kind == "arrive":
                recv_from.setdefault(a, {})[item] = frm
            if kind == "back":
                c["back_taken"] += 1             # received_back: passed on by delivery, not back again
                continue
            if back_channel:                     # GFF-FR-BC:1: forward to every other one a points to
                for y in ptr[a]:
                    if y != frm and y != b:
                        c["back_sent"] += 1
                        c["messages"] += 1
                        push(t + hop(), "back", y, item, a)
        elif kind == "redund":                    # GFF-FR:1, redundant following (no one wills it)
            a = data[0]
            if a in down or a not in ptr or len(ptr[a]) >= redundancy:
                continue
            cands = [x for x in adj[a] if x in ptr and x not in ptr[a]]
            rng.shuffle(cands)
            for x in cands[:redundancy - len(ptr[a])]:
                add_pointer(a, x, t)
                c["redundant"] += 1
                emit(("a", a, x))
        elif kind == "down":
            a = data[0]
            down.add(a)
            c["downs"] += 1
            emit(("D", a))
            push(t + rng.expovariate(1 / (down_mean * period)), "up", a)
        elif kind == "up":
            a = data[0]
            down.discard(a)
            emit(("U", a))
            push(t + rng.expovariate(1 / up_mean), "down", a)
            for x in ptr.get(a, ()):             # what a missed, from everyone it points to
                catch_up(a, x, t)
                back_catch_up(a, x, t)           # and back-forwards a owed while down
            for ch in children.get(a, ()):       # and what its followers missed from it
                if ch not in down:
                    catch_up(ch, a, t)
                    back_catch_up(ch, a, t)      # and back-forwards it missed while down
            if redundancy > 1 and a in ptr:
                push(t + hop(), "redund", a)
        elif kind == "post":
            posted += 1
            post_time[posted] = t
            fol_at_post[posted] = sum(1 for a in dec if a in ptr)
            sample(t)
            emit(("i", posted))
            held[b].add(posted)
            latest[b] = posted
            send(b, posted, t)
        elif kind in ("decide", "wake") and data[0] in down:
            push(t + period, kind, data[0])      # a machine that is down acts when it is up
        elif kind == "decide":
            a = data[0]
            if a in ptr:
                continue
            if not follow(a, t, 0):
                waiting.add(a)
                emit(("w", a))
        elif kind == "wake":
            a = data[0]
            if a in ptr:
                continue
            if not follow(a, t, 1):
                waiting.add(a)
        elif kind == "shorten":
            a = data[0]
            push(t + rng.expovariate(shorten_rate / period), "shorten", a)
            if a in down:
                continue
            if guard:
                # GFF-PS:1 with a stronger body: Alice received item x from Bob by delivery
                # (received(Alice, x, Dave, Bob)), Bob received x from Carol by delivery
                # (received(Bob, x, Dave, Carol)), and Bob still points to Carol; x the
                # latest item Alice received through a pointer she still has
                mine = recv_from.get(a, {})
                xs = [x for x, frm in mine.items() if frm in ptr[a] and frm != b]
                if not xs:
                    continue
                x = max(xs)
                bob = mine[x]
                carol = recv_from.get(bob, {}).get(x)
                if carol is None or carol == a or carol in ptr[a] or carol not in ptr.get(bob, ()):
                    continue
            elif combine:
                # GFF-PS:1 read literally over several pointers: Alice has a Dave-pointer
                # to Bob and none to Carol, Bob has one to Carol; one such pair at random
                bobs = [x for x in ptr[a] if x != b]
                if not bobs:
                    continue
                bob = rng.choice(bobs)
                carols = [y for y in ptr.get(bob, ()) if y != a and y not in ptr[a]]
                if not carols:
                    continue
                carol = rng.choice(carols)
            else:
                bob = ptr[a][0]
                if bob == b:
                    continue
                carol = ptr[bob][0]   # one pointer each: Bob's, to Carol (GFF-PS:1)
            ok, took = True, 0
            for x, y in ((a, carol), (carol, a)):
                if y in friend_sets[x] or (x, y) in known:
                    continue
                if p <= 0 or (x, y) in refused:
                    ok = False
                    break
                if p >= 1 or rng.random() < p:
                    known.add((x, y))
                    c["intros"] += 1
                    took += 1
                else:
                    refused.add((x, y))
                    c["refusals"] += 1
                    ok = False
                    emit(("r", a, carol, took))
                    break
            if not ok:
                continue
            remove_pointer(a, bob)
            add_pointer(a, carol, t)
            c["shortens"] += 1
            emit(("s", a, carol, took, int(carol in friend_sets[a]), bob))
    posted += 1
    sample(end)
    # each item's delivery: mean time from posting to arrival over the deciders
    # it reached, by the end of the run (the last items are still in flight)
    for smp in samples:
        x = smp["item"]
        a = arrivals.get(x)
        smp["latency"] = a[0] / a[1] if a and a[1] else None
        smp["reached"] = a[1] if a else 0
        f = fol_at_post.get(x, 0)
        smp["reach_share"] = smp["reached"] / f if f else None
        smp["on_time_share"] = sum(1 for d in delays.get(x, ()) if d <= ON_TIME * period) / f if f else None
    if final is not None:   # the end state, for checks
        final.update(ptr=ptr, held=held, recv_from=recv_from, down=down, post_time=post_time)
    final_s = samples[-1]
    fol = [a for a in dec if a in ptr]
    summary = dict(final_s, deciders=len(dec), followers_final=len(fol))
    return samples, summary, set(ptr) | {b}


# ---------- the experiment ----------

GRAPHS = [paths.graph_spec("kleinberg", q=1), paths.graph_spec("kleinberg", q=1, r=0), paths.graph_spec("regular", d=6)]
def cfg(jitter=1.0, shorten_rate=0.0, p=0.0, redundancy=1, back_channel=False, down_frac=0.0, combine=False, guard=False):
    d = dict(jitter=jitter, shorten_rate=shorten_rate, p=p, redundancy=redundancy,
             back_channel=back_channel, down_frac=down_frac)
    if combine:   # only the combined settings carry the key, so the others draw as async_sim.py's
        d["combine"] = True
    if guard:
        d["guard"] = True
    return d


CONTRACTS = (  # one pointer (GFF-I), GFF-PS at p = 0.25, GFF-FR and GFF-FR-BC at d = 2, 3
    dict(), dict(shorten_rate=1.0, p=0.25), dict(redundancy=2), dict(redundancy=2, back_channel=True),
    dict(redundancy=3), dict(redundancy=3, back_channel=True))
COMBO = (  # one pointer, GFF-PS, GFF-FR-BC, and GFF-FR-BC with GFF-PS:1 added
    dict(), dict(shorten_rate=1.0, p=0.25), dict(shorten_rate=1.0, p=0.5),
    dict(redundancy=2, back_channel=True), dict(redundancy=3, back_channel=True),
    dict(redundancy=2, back_channel=True, shorten_rate=1.0, p=0.25, combine=True),
    dict(redundancy=2, back_channel=True, shorten_rate=1.0, p=0.5, combine=True),
    dict(redundancy=2, back_channel=True, shorten_rate=1.0, p=1.0, combine=True),
    dict(redundancy=3, back_channel=True, shorten_rate=1.0, p=0.5, combine=True),
    dict(redundancy=2, back_channel=True, shorten_rate=1.0, p=0.25, combine=True, guard=True),
    dict(redundancy=2, back_channel=True, shorten_rate=1.0, p=0.5, combine=True, guard=True),
    dict(redundancy=2, back_channel=True, shorten_rate=1.0, p=1.0, combine=True, guard=True),
    dict(redundancy=3, back_channel=True, shorten_rate=1.0, p=0.5, combine=True, guard=True),
    dict(redundancy=2),   # without the back-channel: does delivery to the loops depend on it?
    dict(redundancy=2, shorten_rate=1.0, p=0.5, combine=True),
    dict(redundancy=2, shorten_rate=1.0, p=0.5, combine=True, guard=True))
GRID = [cfg(down_frac=f, **k) for f in (0.0, 0.1) for k in COMBO]


def task(args):
    import sweep
    g, n, seed, rho, m, conf = args
    adj, fs = sweep.world(g, n, seed)
    k, authors, dec, pairs, base = sweep.pick_authors(g, n, seed, rho, m, "async")
    out = []
    for b in authors:
        rng = random.Random("{}/async/{}/{}".format(base, b, json.dumps(conf, sort_keys=True)))
        samples, summ, holders = simulate(adj, fs, b, dec[b], rng, **conf)
        fol = [f for f in dec[b] if f in holders]
        if fol:  # the baselines over the deciders who follow at the end (as in paths.py)
            sub = paths.bfs_within(adj, b, holders)
            gd = paths.bfs_targets(adj, b, fol)
            summ["best_tree"] = sum(sub[f] for f in fol) / len(fol)
            summ["graph_distance"] = sum(gd[f] for f in fol) / len(fol)
        out.append({"graph": paths.graph_label(g), "n": n, "seed": seed, "rho": rho, "k": k, "author": b,
                    **conf, "samples": samples, "summary": summ})
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--seeds", type=int, nargs="+", default=[1, 2, 3])
    ap.add_argument("--authors", type=int, default=3)
    ap.add_argument("--n", type=int, default=100)
    ap.add_argument("--rho", type=float, default=0.5)
    ap.add_argument("--jobs", type=int, default=os.cpu_count())
    ap.add_argument("--out", default=os.path.join(HERE, "tables", "combo.json"))
    a = ap.parse_args()
    tasks = [(g, a.n, s, a.rho, a.authors, conf) for g in GRAPHS for s in a.seeds for conf in GRID]
    t0 = time.time()
    res = []
    with Pool(a.jobs) as pool:
        for i, r in enumerate(pool.imap_unordered(task, tasks), 1):
            res += r
            print("{}/{} tasks, {:.0f} s".format(i, len(tasks), time.time() - t0), flush=True)
    meta = {"date": time.strftime("%Y-%m-%d %H:%M"), "defaults": DEFAULTS, "grid": GRID, "n": a.n, "rho": a.rho,
            "seeds": a.seeds, "authors": a.authors, "seconds": round(time.time() - t0, 1)}
    with open(a.out, "w") as fh:
        json.dump({"meta": meta, "runs": res}, fh, separators=(",", ":"))
    print("wrote {} in {:.0f} s".format(a.out, time.time() - t0))


if __name__ == "__main__":
    main()
