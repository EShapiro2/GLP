#!/usr/bin/env python3
"""Check an sGLP run's log against the graph it was run on
(programs/sglp/graphs/README.md): whether the platform's state, as each
agent's last view shows it, holds the graph's friendships and pointers, and
when the graph was first complete.

usage: check.py <graph> <log>

<graph> is a graph gen.py prints; <log> a log programs/sglp/cva/run.sh writes,
"<time>\t<agent>\t<type>\t<term>", the views its agents' screens show being
lines of type output, view(Key, Atoms, ...).  A friendship counts once both
agents' views show it, friend(Other); a pointer once its agent's view shows
points_to(Other).  Prints:

  graph <graph>: n N, F friendships, P pointers
  friendships: F' of F at the end, first all at T s     (or: never all;
  pointers: P' of P at the end, first all at T s         or: none in the graph)
  beyond the graph: X friendships, Y pointers
  proposals: R, S of them the graph's steps, D of those repeated; E refused

the graph's steps being the proposals of establish, acquaint, befriend and
point_friend, and E the refusals of an agent's own proposals its screen shows
(the other's screen shows each too, and is not counted).

Exits 0 if every friendship and pointer of the graph is there at the end, 1
if not, and 2 on a bad argument.
"""
import re
import sys


STEP = re.compile(r"propose\((establish|acquaint|befriend|point_friend)\(")


def fail(msg):
    print("check.py: " + msg, file=sys.stderr)
    sys.exit(2)


def read_graph(path):
    n, friends, points = None, set(), set()
    try:
        lines = open(path).read().splitlines()
    except OSError as e:
        fail(f"cannot read {path}: {e}")
    for line in lines:
        t = line.split("#", 1)[0].split()
        if not t:
            continue
        if t[0] == "n" and len(t) == 2:
            n = int(t[1])
        elif t[0] == "friend" and len(t) == 3:
            a, b = int(t[1]), int(t[2])
            friends.add((min(a, b), max(a, b)))
        elif t[0] == "points" and len(t) == 3:
            points.add((int(t[1]), int(t[2])))
        else:
            fail(f"{path}: not a graph line: {line}")
    if not n:
        fail(f"{path} declares no number of agents, \"n N\"")
    return n, friends, points


def view_atoms(term):
    """The key and the atoms of view(Key, [Atoms], ...), or None."""
    m = re.match(r'view\(\s*"?([A-Za-z0-9_]+)"?\s*,\s*\[', term)
    if not m:
        return None
    i, depth = m.end(), 1
    while i < len(term) and depth:
        depth += {"[": 1, "]": -1}.get(term[i], 0)
        i += 1
    atoms = term[m.end():i - 1]
    friends = set(re.findall(r'(?<![a-z_])friend\(\s*"?([A-Za-z0-9_]+)"?\s*\)', atoms))
    points = set(re.findall(r'(?<![a-z_])points_to\(\s*"?([A-Za-z0-9_]+)"?\s*\)', atoms))
    return m.group(1), friends, points


def main():
    if len(sys.argv) != 3:
        fail("usage: check.py <graph> <log>")
    n, friends, points = read_graph(sys.argv[1])
    w = len(str(n))
    key = lambda i: f"p{i:0{w}d}"
    want_f = {(key(a), key(b)) for a, b in friends}
    want_p = {(key(a), key(b)) for a, b in points}

    state = {}            # key -> (friends, points) of its last view
    first_f = first_p = None
    proposals, refused = {}, 0
    try:
        lines = open(sys.argv[2]).read().splitlines()
    except OSError as e:
        fail(f"cannot read {sys.argv[2]}: {e}")
    for line in lines:
        f = line.split("\t")
        if len(f) != 4:
            continue
        t, agent, ty, term = f
        if ty.startswith("Request"):
            proposals[term] = proposals.get(term, 0) + 1
        elif ty == "output":
            if term.startswith("refused("):
                m = re.match(r'refused\(\s*[a-z_]+\(\s*"?([A-Za-z0-9_]+)"?', term)
                if m and agent.isdigit() and m.group(1) == key(int(agent)):
                    refused += 1
                continue
            v = view_atoms(term)
            if not v:
                continue
            state[v[0]] = (v[1], v[2])
            have_f = {(a, b) for a, b in want_f
                      if b in state.get(a, (set(),))[0] and a in state.get(b, (set(),))[0]}
            have_p = {(a, b) for a, b in want_p if b in state.get(a, (set(), set()))[1]}
            if first_f is None and have_f == want_f:
                first_f = float(t)
            if first_p is None and have_p == want_p:
                first_p = float(t)

    end_f = {(a, b) for a, b in want_f
             if b in state.get(a, (set(),))[0] and a in state.get(b, (set(),))[0]}
    end_p = {(a, b) for a, b in want_p if b in state.get(a, (set(), set()))[1]}
    all_f = {(min(a, b), max(a, b)) for a in state for b in state[a][0]}
    all_p = {(a, b) for a in state for b in state[a][1]}
    when = lambda x: f"first all at {x:.2f} s" if x is not None else "never all"
    print(f"graph {sys.argv[1]}: n {n}, {len(want_f)} friendships, {len(want_p)} pointers")
    for what, end, want, first in (("friendships", end_f, want_f, first_f),
                                   ("pointers", end_p, want_p, first_p)):
        if want:
            print(f"{what}: {len(end)} of {len(want)} at the end, {when(first)}")
        else:
            print(f"{what}: none in the graph")
    print(f"beyond the graph: {len(all_f - want_f)} friendships, {len(all_p - want_p)} pointers")
    steps = {t: c for t, c in proposals.items() if STEP.match(t)}
    print(f"proposals: {sum(proposals.values())}, {sum(steps.values())} of them the graph's steps, "
          f"{sum(c - 1 for c in steps.values())} of those repeated; {refused} refused")
    sys.exit(0 if end_f == want_f and end_p == want_p else 1)


if __name__ == "__main__":
    main()
