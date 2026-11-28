#!/usr/bin/env python3
"""concurrency_control.py --- controls of Disputes against concurrent cases (Grassroots-Audit #135), on the schemas as printed.

Reads sas(raise, ...) and sas(decide, ...) from Disputes.glp beside this file, as calculus/tools/adopt.py prints them,
and applies them as the Calculus defines (GCC, Definitions Ground instance, participants, and Induced transaction):
a ground instance whose body holds in the participants' local states adds and deletes the atoms of its head.  A run is
a sequence of such transactions; two acts that are both enabled in one configuration are two concurrent cases, and a
run takes one of them first.  The controls take every such pair, in both orders, and then every run over a small
village: one organisation, persons Alice, Bob, Carol and Dave, Carol and Dave arbiters, one dispute, two rulings.
Prints one line per control, "<name> ok" or "<name> WRONG", and exits 1 if any is wrong.  Standard library only.
"""
import itertools, os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = open(os.path.join(HERE, "Disputes.glp")).read()


# --- the schema terms, read as printed ---------------------------------------------------------------------------
def parse(text):
    toks = re.findall(r"'[^']*'|[A-Za-z_][A-Za-z0-9_]*|\d+|[()\[\],]", text)
    pos = [0]

    def term():
        t = toks[pos[0]]; pos[0] += 1
        if t == "[":
            items = []
            if toks[pos[0]] == "]":
                pos[0] += 1; return items
            while True:
                items.append(term())
                t = toks[pos[0]]; pos[0] += 1
                if t == "]": return items
        name = t[1:-1] if t.startswith("'") else t
        if pos[0] < len(toks) and toks[pos[0]] == "(":
            pos[0] += 1; args = []
            while True:
                args.append(term())
                t = toks[pos[0]]; pos[0] += 1
                if t == ")": return (name, args)
        return ("var", name) if t.startswith("'") else ("con", name)
    return term()


def schema(act):
    line = next(l for l in SRC.split("\n") if l.strip().startswith("sas(%s," % act))
    _, (n, roles, vs, head, body, cons, guards) = parse(line.strip().rstrip(",").rstrip(")") + ")")
    return {"head": head, "body": body, "guards": [g[1] for g in guards]}


RAISE, DECIDE = schema("raise"), schema("decide")


def ground(t, g):
    kind = t[0]
    if kind == "var": return g[t[1]]
    if kind == "const": return g[t[1][0][1]]        # a value variable of the act: const('Issue')
    if kind == "val": return t[1][0][1]             # a value written in the contract: val('Arbiter')
    if kind == "con": return t[1]
    sys.exit("concurrency_control.py: an argument this control does not read: %r" % (t,))


def literal(t, g):
    """(positive?, holder, atom)"""
    if t[0] == "not": t, pos = t[1][0], False
    else: pos = True
    p, args = t[1][0][1], [ground(a, g) for a in t[1][1]]
    return pos, args[0], (p,) + tuple(args)


def step(conf, sch, g):
    """the configuration after the act, or None if the act is not enabled in conf."""
    for b in sch["body"]:
        pos, h, a = literal(b, g)
        if (a in conf.get(h, frozenset())) != pos: return None
    new = {k: set(v) for k, v in conf.items()}
    for b in sch["head"]:
        pos, h, a = literal(b, g)
        (new.setdefault(h, set()).add if pos else new.setdefault(h, set()).discard)(a)
    new = {k: frozenset(v) for k, v in new.items()}
    return None if new == conf else new


def freeze(conf): return frozenset((k, v) for k, v in conf.items())
def thaw(f): return dict(f)


# --- the village ---------------------------------------------------------------------------------------------------
PERSONS = ["Alice", "Bob", "Carol", "Dave"]
ARBITERS = ["Carol", "Dave"]


def village(orgs=("Orga",)):
    c = {x: {("identity", x), ("person", x)} for x in PERSONS}
    for o in orgs:
        c[o] = {("identity", o)} | {("has_participant", o, x) for x in PERSONS} \
            | {("has_role", o, x, "Arbiter") for x in ARBITERS}
    return {k: frozenset(v) for k, v in c.items()}


def raise_(alice, bob, carol, issue, orga="Orga"):
    return (RAISE, {"Alice": alice, "Bob": bob, "Carol": carol, "Issue": issue, "Orga": orga})


def decide(carol, alice, bob, issue, ruling, orga="Orga"):
    return (DECIDE, {"Carol": carol, "Alice": alice, "Bob": bob, "Issue": issue, "Ruling": ruling, "Orga": orga})


def run(conf, acts):
    for sch, g in acts:
        conf = step(conf, sch, g)
        if conf is None: return None
    return conf


def concurrent(conf, a, b):
    """both enabled in conf, and in each order the second is refused."""
    return all(run(conf, [x]) is not None for x in (a, b)) and \
        run(conf, [a, b]) is None and run(conf, [b, a]) is None


bad = 0


def control(name, ok):
    global bad
    print("%s %s" % (name, "ok" if ok else "WRONG"))
    bad += not ok


V = village()
R = run(V, [raise_("Alice", "Bob", "Carol", "fence")])

# Two concurrent cases of one dispute: one is taken, the other refused.
control("two arbiters raised at once, one case", concurrent(V, raise_("Alice", "Bob", "Carol", "fence"),
                                                          raise_("Alice", "Bob", "Dave", "fence")))
control("both parties raise at once, one case", concurrent(V, raise_("Alice", "Bob", "Carol", "fence"),
                                                          raise_("Bob", "Alice", "Dave", "fence")))
control("a third participant raises it at once, one case", concurrent(V, raise_("Alice", "Bob", "Carol", "fence"),
                                                                     raise_("Dave", "Bob", "Carol", "fence")))
control("two rulings decided at once, one ruling", concurrent(R, decide("Carol", "Alice", "Bob", "fence", "Alice repairs"),
                                                             decide("Carol", "Alice", "Bob", "fence", "Bob repairs")))
control("an arbiter not assigned cannot decide",
        run(R, [decide("Dave", "Alice", "Bob", "fence", "Bob repairs")]) is None)
D = run(R, [decide("Carol", "Alice", "Bob", "fence", "Alice repairs")])
control("no case after the ruling", all(run(D, [raise_(a, b, c, "fence")]) is None
                                        for a, b, c in itertools.permutations(PERSONS, 3) if c in ARBITERS))
control("both parties hold the one ruling",
        ("ruling", "Alice", "Orga", "fence", "Alice repairs") in D["Alice"]
        and ("ruling", "Bob", "Orga", "fence", "Alice repairs") in D["Bob"])
# Different cases do not block each other.
control("two disputes at once, two cases", run(V, [raise_("Alice", "Bob", "Carol", "fence"),
                                                   raise_("Alice", "Bob", "Dave", "well")]) is not None)
V2 = village(("Orga", "Will"))
control("one dispute name in two organisations, two cases",
        run(V2, [raise_("Alice", "Bob", "Carol", "fence"), raise_("Alice", "Bob", "Carol", "fence", "Will")]) is not None)

# Every run of the village: every act of Raise and Decide on the dispute fence, by any persons in any order.
ACTS = [raise_(a, b, c, "fence") for a, b, c in itertools.permutations(PERSONS, 3) if c in ARBITERS] + \
       [decide(c, a, b, "fence", r) for a, b, c in itertools.permutations(PERSONS, 3) if c in ARBITERS
        for r in ("Alice repairs", "Bob repairs")]
seen, todo, worst = {freeze(V)}, [V], []
while todo and len(seen) <= 1000:                  # a bound: the contract as revised has 37
    c = todo.pop()
    o = c["Orga"]
    cases = [a for a in o if a[0] == "raised"]
    decisions = [a for a in o if a[0] == "decision"]
    rulings = {a[4] for x in PERSONS for a in c[x] if a[0] == "ruling"}
    arbiters = {a[1] for x in ARBITERS for a in c[x] if a[0] == "case"}
    if len(cases) > 1 or len(decisions) > 1 or len(cases) + len(decisions) > 1 or len(arbiters) > 1 or \
            (decisions and rulings != {decisions[0][6]}) or (not decisions and rulings):
        worst.append(c)
    for sch, g in ACTS:
        n = step(c, sch, g)
        if n is not None and freeze(n) not in seen:
            seen.add(freeze(n)); todo.append(n)
control("every run of the village: one case, one arbiter, at most one ruling (%d configurations)" % len(seen),
        not worst and len(seen) == 1 + 12 + 12 * 2)
sys.exit(1 if bad else 0)
