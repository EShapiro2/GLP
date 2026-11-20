#!/usr/bin/env python3
"""Compose the sGLP source of a run on a social graph of one's choosing
(programs/sglp/graphs/README.md): an sGLP source over a canonical CVA
platform, in the term form transform.sh --interface reads, with the setup
library (library.glp) and each person's script, graph_script/2, printed from
the graph.

usage: compose.py --graph G --interface I [--source S | --until 'C U' --seed N] > run_sglp.glp

  --graph G       a graph gen.py prints: "n N", "friend a b", "points a b";
  --interface I   the platform's interface, cascade/<NAME>/interface.glp;
  --source S      an owner's sGLP source over the platform, the declaration
                  exported procedure source(SglpTerms) and the unit clause
                  source(S), its profiles calling graph_next/3 and
                  graph_asked/3, its run declaration of N agents, N the
                  graph's, of graph agents, run(graph, ...), which compose.py
                  prints as the graph's number, or of a census whose keys
                  are the graph's;
  --until, --seed with no --source: a run whose persons only build the graph,
                  the profiles graph_builder and graph_approver of library.glp's
                  graph_profiles and a run declaration of a dimension of each.

The agents are keyed p1 ... pN, each number zero-padded to the width of N, as
programs/sglp/cva/run.sh keys them.  Agent a's script, graph_script(Key, Steps):
  establish                                    done: identity
  acquaint each friend b > a                   done: knows(b), needs identity
  befriend each friend b > a                   done: friend(b), needs knows(b)
  point_friend each b that a points to         done: points_to(b), needs friend(b)
  await each friend b < a                      done: friend(b), never proposed
The lower of a pair proposes its acquaintance and its friendship; the higher
approves them and awaits them, so that graph_next says done only once all of
a's friendships and pointers are there.  compose.py also prints graph_agents(Keys), the keys in order.  compose.py refuses a layer the platform has no act or atom
for, naming it, and prints nothing.
"""
import argparse
import os
import re
import sys

HERE_LIB = os.path.join(os.path.dirname(os.path.abspath(__file__)), "library.glp")

# What the library and each layer need of the platform's interface: a type,
# and the functor/arity of an alternative of it.
LIBRARY_NEEDS = [("Request", "propose", 1), ("Proposal", "establish", 1),
                 ("Proposal", "acquaint", 2), ("Proposal", "befriend", 2),
                 ("Atom", "identity", 0), ("Atom", "knows", 1), ("Atom", "friend", 1),
                 ("Output", "view", 5), ("Asking", "hello", 2), ("Attempt", "attempt", 5),
                 ("Class", "acquaint", 2), ("Class", "befriend", 2)]
LAYER_NEEDS = {"points": [("Proposal", "point_friend", 2), ("Atom", "points_to", 1)]}


def fail(msg):
    print("compose.py: " + msg, file=sys.stderr)
    sys.exit(2)


def strip_comments(text):
    """The text with % comments dropped, quoted atoms and strings kept."""
    out, i, n = [], 0, len(text)
    while i < n:
        c = text[i]
        if c in "'\"":
            j = i + 1
            while j < n and text[j] != c:
                j += 2 if text[j] == "\\" else 1
            out.append(text[i:j + 1])
            i = j + 1
        elif c == "%":
            while i < n and text[i] != "\n":
                i += 1
        else:
            out.append(c)
            i += 1
    return "".join(out)


def balanced(text, start):
    """The index just past the bracket closing the one at text[start]."""
    pairs = {"(": ")", "[": "]"}
    stack, i, n = [], start, len(text)
    while i < n:
        c = text[i]
        if c in "'\"":
            j = i + 1
            while j < n and text[j] != c:
                j += 2 if text[j] == "\\" else 1
            i = j + 1
            continue
        if c in pairs:
            stack.append(pairs[c])
        elif c in ")]":
            if not stack or stack.pop() != c:
                return -1
            if not stack:
                return i + 1
        i += 1
    return -1


def split_top(text):
    """The comma-separated items at the top level of text."""
    items, depth, cur, i, n = [], 0, [], 0, len(text)
    while i < n:
        c = text[i]
        if c in "'\"":
            j = i + 1
            while j < n and text[j] != c:
                j += 2 if text[j] == "\\" else 1
            cur.append(text[i:j + 1])
            i = j + 1
            continue
        if c in "([":
            depth += 1
        elif c in ")]":
            depth -= 1
        if c == "," and depth == 0:
            items.append("".join(cur).strip())
            cur = []
        else:
            cur.append(c)
        i += 1
    if "".join(cur).strip():
        items.append("".join(cur).strip())
    return items


def unit_list(path, name):
    """The items of the list argument of the unit clause name([...]) of path."""
    try:
        text = strip_comments(open(path).read())
    except OSError as e:
        fail(f"cannot read {path}: {e}")
    text = re.sub(r"(?:exported\s+)?procedure\s+%s\s*\(\s*SglpTerms\s*\)\s*\." % name, "", text, 1)
    m = re.search(r"(?<![A-Za-z0-9_])%s\s*\(\s*\[" % name, text)
    if not m:
        fail(f"{path} holds no unit clause {name}([...])")
    lb = m.end() - 1
    end = balanced(text, lb)
    rest = text[end:].strip() if end > 0 else ""
    if end < 0 or not re.match(r"\)\s*\.", rest):
        fail(f"{path}: {name}(...) is not one list and the clause's end")
    return split_top(text[lb + 1:end - 1])


def interface_types(path):
    """{type name: {(functor, arity)}} of the interface's type/2 items."""
    types = {}
    for item in unit_list(path, "interface"):
        m = re.match(r"type\(\s*'([A-Za-z0-9_]+)'\s*,\s*\[", item)
        if not m:
            continue
        lb = m.end() - 1
        alts = set()
        for alt in split_top(item[lb + 1:balanced(item, lb) - 1]):
            f = re.match(r"([a-z][A-Za-z0-9_]*)\s*(\(?)", alt)
            if f:
                arity = len(split_top(alt[f.end():alt.rfind(")")])) if f.group(2) else 0
                alts.add((f.group(1), arity))
        types[m.group(1)] = alts
    return types


def read_graph(path):
    n, friends, points = None, [], []
    try:
        lines = open(path).read().splitlines()
    except OSError as e:
        fail(f"cannot read {path}: {e}")
    for ln, line in enumerate(lines, 1):
        t = line.split("#", 1)[0].split()
        if not t:
            continue
        if t[0] == "n" and len(t) == 2 and t[1].isdigit() and n is None:
            n = int(t[1])
        elif t[0] in ("friend", "points") and len(t) == 3 and t[1].isdigit() and t[2].isdigit() and n:
            a, b = int(t[1]), int(t[2])
            if not (1 <= a <= n and 1 <= b <= n) or a == b:
                fail(f"{path}:{ln}: {t[0]} {a} {b} is not between two distinct agents of 1..{n}")
            (friends if t[0] == "friend" else points).append((a, b))
        else:
            fail(f"{path}:{ln}: neither \"n N\" first, nor \"friend a b\" or \"points a b\"")
    if not n:
        fail(f"{path} declares no number of agents, \"n N\"")
    fset = {(min(a, b), max(a, b)) for a, b in friends}
    for a, b in points:
        if (min(a, b), max(a, b)) not in fset:
            fail(f"{path}: points {a} {b} is not along a friendship")
    return n, sorted(fset), sorted(set(points))


def scripts(n, friends, points):
    w = len(str(n))
    key = lambda i: f"p{i:0{w}d}"
    up = {i: [] for i in range(1, n + 1)}
    down = {i: [] for i in range(1, n + 1)}
    for a, b in friends:
        up[a].append(b)
        down[b].append(a)
    out = {i: [] for i in range(1, n + 1)}
    for a, b in points:
        out[a].append(b)
    lines = ["   %% Each person's script, printed by compose.py from the graph.",
             "   decl(graph_script(dual('Key'), 'List'('GraphStep'))),"]
    for i in range(1, n + 1):
        me = key(i)
        steps = [f"step(establish({me}), identity, [])"]
        steps += [f"step(acquaint({me}, {key(b)}), knows({key(b)}), [identity])" for b in up[i]]
        steps += [f"step(befriend({me}, {key(b)}), friend({key(b)}), [knows({key(b)})])" for b in up[i]]
        steps += [f"step(point_friend({me}, {key(b)}), points_to({key(b)}), [friend({key(b)})])" for b in out[i]]
        steps += [f"await(friend({key(b)}))" for b in down[i]]
        lines.append(f"   clause(graph_script({me}, [{', '.join(steps)}]), [], []),")
    lines.append("   clause(graph_script(var('_'), []), [otherwise], [true]),")
    lines.append("   decl(graph_agents('GraphKeys')),")
    lines.append(f"   clause(graph_agents([{', '.join(key(i) for i in range(1, n + 1))}]), [], [])")
    return lines


UNITS = {"second", "seconds", "minute", "minutes", "hour", "hours", "day", "days",
         "week", "weeks", "year", "years"}


def main():
    ap = argparse.ArgumentParser(description="Compose an sGLP source for a run on a given social graph.")
    ap.add_argument("--graph", required=True)
    ap.add_argument("--interface", required=True)
    ap.add_argument("--source")
    ap.add_argument("--until")
    ap.add_argument("--seed", type=int)
    a = ap.parse_args()

    n, friends, points = read_graph(a.graph)
    types = interface_types(a.interface)
    needs = list(LIBRARY_NEEDS) + (LAYER_NEEDS["points"] if points else [])
    missing = [f"{t} has no {f}/{k}" for t, f, k in needs if (f, k) not in types.get(t, set())]
    if missing:
        fail(f"the platform of {a.interface} cannot carry this graph: " + "; ".join(missing))

    lib = unit_list(HERE_LIB, "graph_library")
    if a.source:
        if a.until or a.seed is not None:
            fail("--until and --seed are the source's run declaration's, given with no --source only")
        own = unit_list(a.source, "source")
        runs = [i for i in own if re.match(r"run\s*\(", i)]
        if len(runs) != 1:
            fail(f"{a.source} holds {len(runs)} run declarations, not one")
        who = split_top(runs[0][runs[0].index("(") + 1:runs[0].rindex(")")])[0]
        if who.isdigit() and int(who) != n:
            fail(f"{a.source}'s run declaration is of {who} agents, the graph of {n}")
        if who == "graph":
            own = [re.sub(r"^run\s*\(\s*graph\s*,", f"run({n},", i) if i is runs[0] else i for i in own]
        origin = f"the source {a.source}"
    else:
        if not a.until or a.seed is None:
            fail("with no --source, --until 'C U' and --seed N give the run declaration")
        c, u = (a.until.split() + [""])[:2]
        if not re.fullmatch(r"[0-9]+(\.[0-9]+)?", c) or u not in UNITS:
            fail(f"--until is '<count> <unit>', the unit second ... year, not '{a.until}'")
        own = unit_list(HERE_LIB, "graph_profiles")
        own.append(f"run({n}, [mix(requests, [share(graph_builder, 1.0)]), "
                   f"mix(approvals, [share(graph_approver, 1.0)])], {c}, {u}, {a.seed})")
        origin = "graph_builder and graph_approver"

    with open(a.graph) as f:
        head = [ln for ln in f.read().splitlines() if ln.startswith("#")]
    out = ["%% Composed by programs/sglp/graphs/compose.py: " + origin + ", on the graph " + a.graph + ",",
           "%% over the platform of " + a.interface + "; not edited.",
           *("%% " + h.lstrip("# ") for h in head),
           "exported procedure source(SglpTerms).",
           "source(["]
    body = ["   " + i for i in own] + ["   " + i for i in lib]
    out += [b + "," for b in body]
    out += scripts(n, friends, points)
    out.append("]).")
    print("\n".join(out))


if __name__ == "__main__":
    main()
