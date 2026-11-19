#!/usr/bin/env python3
"""A Feeds run's summary from its graph and its log (programs/sglp/gff/Feeds_sglp.glp),
after programs/gff/paths/sglp/spread.py (branch yuval/feeds-sglp), with the
graph's distances.

usage: spread.py <graph> <log>

Prints the requests by act and how many were refused; the most copies of one
knows atom any agent holds; the agents that follow the author's feed (a
pointer labelled by the author, agent 1) at the end; for each item, when the
author posted it and how many agents came to hold it; and, by hop distance
from the author in the friendship graph, the agents at that distance, how many
follow and hold every item at the end, the median simulated hour at which they
came to follow, and their follow attempts (labeled_point_friend proposed).
"""
import re
import sys
from collections import Counter, deque


def main():
    if len(sys.argv) != 3:
        print("usage: spread.py <graph> <log>", file=sys.stderr)
        sys.exit(2)
    n, nbrs = 0, {}
    for line in open(sys.argv[1]):
        t = line.split("#", 1)[0].split()
        if t and t[0] == "n":
            n = int(t[1])
            nbrs = {i: set() for i in range(1, n + 1)}
        elif t and t[0] == "friend":
            a, b = int(t[1]), int(t[2])
            nbrs[a].add(b)
            nbrs[b].add(a)
    w = len(str(n))
    key = lambda i: f"p{i:0{w}d}"
    author = key(1)
    dist = {1: 0}
    q = deque([1])
    while q:
        u = q.popleft()
        for v in nbrs[u]:
            if v not in dist:
                dist[v] = dist[u] + 1
                q.append(v)

    reqs, refused, attempts = Counter(), Counter(), Counter()
    last, follows_at, holds, posted = {}, {}, {}, {}
    for line in open(sys.argv[2]):
        f = line.rstrip("\n").split("\t")
        if len(f) != 4:
            continue
        t, agent, ty, term = float(f[0]), f[1], f[2], f[3]
        if ty.startswith("Request"):
            act = re.match(r"propose\((\w+)", term).group(1)
            reqs[act] += 1
            if act == "labeled_point_friend":
                attempts[int(agent)] += 1
            if act == "post":
                it = re.search(r'utterance\("?(\w+)"?\)', term).group(1)
                posted.setdefault(it, t)
        elif ty == "output" and term.startswith("refused("):
            act = re.match(r'refused\(\s*(\w+)\(\s*"?(\w+)', term)
            if act and act.group(2) == key(int(agent)):
                refused[act.group(1)] += 1
        elif ty == "output" and term.startswith("view("):
            a = int(agent)
            st = term[term.index("[") + 1:]
            st = st[:st.index("], [")] if "], [" in st else st
            last[a] = st
            if a not in follows_at and re.search(r"pointer\(\w+, %s\)" % author, st):
                follows_at[a] = t
            for it in set(re.findall(r'item\(speech\(utterance\("?(\w+)"?\)', st)):
                holds.setdefault((it, a), t)

    items = sorted(posted)
    knows = max([0] + [max([0] + list(Counter(re.findall(r"knows\(\w+\)", s)).values())) for s in last.values()])
    fol = [a for a in range(2, n + 1) if a in follows_at]
    print(f"agents {n}, author {author}; requests {dict(reqs)}; refused {dict(refused)}; most copies of one knows atom {knows}")
    print(f"following the author's feed at the end: {len(fol)} of {n - 1}")
    for it in items:
        k = sum(1 for a in range(1, n + 1) if (it, a) in holds)
        print(f"{it}: posted at hour {posted[it] / 3600:.1f}, held by {k} of {n}")
    print("distance  agents  following  holding all  median hour followed  follow attempts")
    for d in sorted(set(dist.values()) - {0}):
        ag = [a for a in dist if dist[a] == d]
        fs = sorted(follows_at[a] / 3600 for a in ag if a in follows_at)
        full = sum(1 for a in ag if all((it, a) in holds for it in items)) if items else 0
        med = f"{fs[len(fs) // 2]:.1f}" if fs else "-"
        print(f"{d:8}  {len(ag):6}  {len(fs):9}  {full:11}  {med:>20}  {sum(attempts[a] for a in ag):15}")
    unreached = n - len(dist)
    if unreached:
        print(f"not connected to the author: {unreached} agents")


if __name__ == "__main__":
    main()
