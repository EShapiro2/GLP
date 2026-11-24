#!/usr/bin/env python3
"""derive.py [NAME ...] [--root DIR] [--out FILE] [--first-lines FILE] --- derives each contract by the operations of the paper's Section 3
from the contracts its first line names, and compares the result with what the tools print for it.

The task: docs/task-2026-11-23-no-parameters.md of the paper's repository, point 8 (and docs/derive-task-2026-11-23.md
without its step 6).  The contracts have no parameter, no default and no binding.

ROOT is the tree of the tools (sas/ and sasx/), or, with --root DIR, a publication directory as tools/publish.py
writes it (NAME/schemas.txt and NAME/full-schemas.txt).  The first line is line 2 of the schemas; the result of a
contract is its expanded schemas.  For each contract N, from the expanded schemas of each contract P that its first
line names, in this order:

1. renaming: a predicate of P that N's schemas do not have takes another, one for one over the pair (an officer for a
   guardian, a coin for a note); the program prints the map;
2. lifting: a predicate of P that N has with one more argument, the same new variable in every atom of it in a
   clause (Denominated Coins from Coins); the program prints the predicates and the place;
3. where two contracts of the first line have one clause differently, the weakening or strengthening of the one, so
   that the two are one; the program prints the step;
4. the union;
5. N's own clauses: a clause of N is added, or weakens or strengthens an inherited clause that it cites (its label
   has N's clause after the clause it cites).

An act that the tools compile from an act of the contract, the act taken for a party ('Pay for Alice') or the vote on
it ('Vote of Alice for Pay'), is compiled: the program checks that the act it is compiled from is in the result and
that the compiled act has its conclusions and conditions.

It compares the result with N's expanded schemas up to the names of variables, the names of clauses and the order of
literals, and prints for each contract "derived", or every clause that differs, with both clauses in full.  The last
lines are the summary.  Standard library only.
"""
import os, re, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MARKS = "■◆○"
LINE = re.compile(r"^(\S.*?) ([%s]) ([^|:][^:]*): (.*)$" % MARKS)
NAMEW = re.compile(r"[A-Z][A-Za-z0-9]*")


def split_top(s):
    out, d, cur = [], 0, ""
    for ch in s:
        if ch in "([":
            d += 1
        if ch in ")]":
            d -= 1
        if ch == "," and d == 0:
            out.append(cur.strip())
            cur = ""
        else:
            cur += ch
    if cur.strip():
        out.append(cur.strip())
    return out


def lit(s):
    """'~p(A,b)' -> (neg, p, (args)); a guard 'Alice' -> (False, '', ('Alice',))."""
    neg = s.startswith("~")
    s = s[1:] if neg else s
    m = re.match(r"^([a-z_0-9]+)\((.*)\)$", s)
    if not m:
        return (neg, "", (s,))
    return (neg, m.group(1), tuple(split_top(m.group(2))))


def clause_of(l):
    m = LINE.match(l.rstrip("\n"))
    if not m or "<-" not in m.group(4):
        return None
    lab, mark, name, rest = m.group(1), m.group(2), m.group(3), m.group(4).rstrip(".")
    poss = ""
    if " possibly " in rest:
        rest, poss = rest.split(" possibly ", 1)
    taken = ""
    mt = re.search(r" taken for (.*)$", rest)
    if mt:
        taken, rest = mt.group(1), rest[:mt.start()]
    must = ""
    if " must " in rest:
        rest, must = rest.split(" must ", 1)
    h, b = rest.split("<-", 1)
    g = ""
    if " guarded by " in " " + b:
        b, g = (" " + b).split(" guarded by ", 1)
    return dict(lab=lab, first=lab.split("+")[0], name=name, mark=mark, raw=l.rstrip("\n"), taken=taken,
                H=[lit(x) for x in split_top(h)], B=list(dict.fromkeys(lit(x) for x in split_top(b))),      # a condition twice is once (an act and its weakening both say 'Alice is a person')
                G=[lit(x) for x in split_top(g)] + [(True, "must", (x,)) for x in split_top(must)])


def read(path):
    out = []
    for l in open(path, encoding="utf-8"):
        c = clause_of(l)
        if c:
            out.append(c)
    return out


def tmpl(a):
    """An argument as its template and its names: '>=VotesAlice' -> ('>=\\0', ['VotesAlice'])."""
    names = NAMEW.findall(a)
    return NAMEW.sub("\0", a), names


def compiled(c):
    return c["taken"] or c["name"].startswith("Vote of ")


class Pair:
    """The maps of one contract of the first line into N: renaming (pred -> pred) and lifting (pred -> place)."""
    def __init__(self, ren=None, lift=None):
        self.ren, self.lift = dict(ren or {}), dict(lift or {})


def embed(P, C, pm, exact=False):
    """Whether clause P, under pm (renaming and lifting), is in clause C with literals added: the literals C adds, by
    part ('H', 'B', 'G'), under one map of the names of P to those of C, one for one; or None."""
    parts = "HBG"
    src = []
    for k in parts:
        for (neg, p, a) in P[k]:
            q = pm.ren.get(p, p)
            src.append((k, neg, q, a, pm.lift.get(p)))
    tgt = {k: list(C[k]) for k in parts}
    if exact and any(len(P[k]) != len(C[k]) for k in parts):
        return None
    # the rarest first
    cnt = collections.Counter((k, n, q) for k, n, q, _, _ in src)
    src.sort(key=lambda x: (sum(1 for t in tgt[x[0]] if t[0] == x[1] and t[1] == x[2]), x[2]))
    used = {k: [False] * len(tgt[k]) for k in parts}
    best = [None]
    NEWV = "\1lift"

    def args_ok(a, b, place, mp, inv):
        if place is not None:
            if len(b) != len(a) + 1:
                return None
            extra = b[place]
            b = b[:place] + b[place + 1:]
            if not re.fullmatch(NAMEW, extra):
                return None
            if NEWV in mp and mp[NEWV] != extra:
                return None
            if NEWV not in mp and extra in inv:
                return None
            mp, inv = dict(mp), dict(inv)
            mp[NEWV] = extra
        if len(a) != len(b):
            return None
        mp, inv = dict(mp), dict(inv)
        for x, y in zip(a, b):
            tx, nx = tmpl(x)
            ty, ny = tmpl(y)
            if tx != ty or len(nx) != len(ny):
                return None
            for u, v in zip(nx, ny):
                if mp.get(u, v) != v or inv.get(v, u) != u:
                    return None
                mp[u], inv[v] = v, u
        return mp, inv

    def rec(i, mp, inv):
        if i == len(src):
            best[0] = mp
            return True
        k, neg, q, a, place = src[i]
        for j, (n2, p2, b2) in enumerate(tgt[k]):
            if used[k][j] or n2 != neg or p2 != q:
                continue
            r = args_ok(a, b2, place, mp, inv)
            if r is None:
                continue
            used[k][j] = True
            if rec(i + 1, *r):
                return True
            used[k][j] = False
        return False

    if not rec(0, {}, {}):
        return None
    if NEWV in best[0]:        # the lifted variable is new to the clause of P
        if best[0][NEWV] in {v for u, v in best[0].items() if u != NEWV}:
            return None
    return {k: [t for j, t in enumerate(tgt[k]) if not used[k][j]] for k in parts}


def text(ls):
    return ", ".join(("~" if n else "") + (p + "(" + ",".join(a) + ")" if p else a[0]) for n, p, a in ls)


class Deriver:
    def __init__(self, root):
        self.root = root
        pub = not os.path.isdir(os.path.join(root, "sasx"))
        self.pub = pub
        self.names = sorted(NM.names_in(os.path.join(root, "gr"))) if not pub else \
            sorted(NM.name_of(d) for d in os.listdir(root) if os.path.isfile(os.path.join(root, d, "full-schemas.txt")))
        self.full, self.first = {}, {}

    def path(self, n, which):
        f = NM.file_of(n)
        if self.pub:
            return os.path.join(self.root, f, {"sas": "schemas.txt", "sasx": "full-schemas.txt"}[which])
        return os.path.join(self.root, which, f + ".txt")

    def F(self, n):
        if n not in self.full:
            self.full[n] = read(self.path(n, "sasx"))
        return self.full[n]

    def first_line(self, n):
        if n not in self.first:
            ls = open(self.path(n, "sas"), encoding="utf-8").read().split("\n")
            self.first[n] = ls[1]
        return self.first[n]

    WITH = r" with (.*?)(?=, (?:and )?(?:restricts|strengthens|extends|composes|relaxes|lifts|renames|weakens|amends|unites)\b|$)"

    def words(self, n):
        """The renamings of words of the first line's 'instantiates B with ...': [(new, old)], 'a child for a ward'."""
        s = re.sub(r"\([^)]*\)", "", self.first_line(n)).rstrip(".")
        m = re.search(r"instantiates .*?" + self.WITH, s)
        its = re.split(r", | and |; ", m.group(1)) if m else []
        for l in open(self.path(n, "sas"), encoding="utf-8"):      # the pragma 'Renaming of Guardians: an organisation for a ward.'
            r = re.match(r"^Renaming of .+?: (.*)\.$", l.rstrip("\n"))
            if r:
                its += re.split(r", | and |; ", r.group(1))
        out = []
        for it in its:
            w = re.match(r"^(?:an? |the )?([a-z][a-z ]*?) for (?:an? |the )?([a-z][a-z ]*)$", it.strip())
            if w:
                out.append((w.group(1).replace(" ", "_"), w.group(2).replace(" ", "_")))
        return out

    def parents(self, n):
        s = re.sub(r"\([^)]*\)", "", self.first_line(n))[len(n):]
        s = re.sub(r"(instantiates (?:.*?))" + self.WITH, r"\1", s)      # the words of 'with ...' name no contract
        out = []
        for _, _, nm in NM.find(s, set(self.names)):
            if nm not in out and nm != n:
                out.append(nm)
        return out

    def learn(self, n, p):
        """The renaming and the lifting of p into n: a predicate of p that n does not have is renamed; one that n has
        with one more argument and never with its own number is lifted."""
        def arities(cs):
            a = collections.defaultdict(set)
            for c in cs:
                for k in "HBG":
                    for _, q, args in c[k]:
                        if q:
                            a[q].add(len(args))
            return a
        An, Ap = arities(self.F(n)), arities(self.F(p))
        lift, absent = {}, set()
        for q, ks in Ap.items():
            if q not in An:
                absent.add(q)
            elif {k + 1 for k in ks} == An[q] and ks != An[q] or {k + 1 for k in ks} < An[q]:      # lifted; n may have the predicate at more arities (a Dollar-denominated coin beside a share, items 39, 42)
                lift[q] = None
        ren = {}
        for new, old in self.words(n):          # the words the first line renames, in the names of predicates
            for q in list(absent):
                cand = re.sub(r"(?<![a-z])%s(?![a-z])" % re.escape(old), new, q)
                if cand != q and cand in An:
                    ren[q] = cand
                    absent.discard(q)
                    if {k + 1 for k in Ap[q]} <= An[cand]:      # renamed and lifted: a bond for a note, with its maturity date
                        lift[q] = None
        if absent or lift:
            # the renaming and the place of each lift, from the clauses of p and n of one first label and of one shape
            votes, lvotes = collections.Counter(), collections.Counter()
            byfirst = collections.defaultdict(list)
            for c in self.F(n):
                byfirst[c["first"]].append(c)
            for c in self.F(p):
                for d in byfirst.get(c["first"], []):
                    for k in "HB":
                        for (n1, q1, a1) in c[k]:
                            for (n2, q2, a2) in d[k]:
                                if n1 != n2:
                                    continue
                                if q1 in absent and q2 not in Ap and len(a1) == len(a2):
                                    votes[(q1, q2)] += 1
                                if q1 in lift and ren.get(q1, q1) == q2 and len(a2) == len(a1) + 1:
                                    for i in range(len(a2)):
                                        if a2[:i] + a2[i + 1:] == a1:
                                            lvotes[(q1, i)] += 1
            taken = set(ren.values())
            for v, q1, q2 in sorted(((v, q1, q2) for (q1, q2), v in votes.items() if q1 in absent), reverse=True):
                if q1 not in ren and q2 not in taken:      # one for one
                    ren[q1] = q2
                    taken.add(q2)
            for q in list(lift):
                cand = [(v, i) for (q1, i), v in lvotes.items() if q1 == q]
                if cand:
                    lift[q] = max(cand)[1]
                else:
                    del lift[q]
        return Pair(ren, lift)

    def derive(self, n):
        """(steps, differences): the steps printed and, for each clause that differs, (what, the source clause or
        None, the clause of n or None)."""
        steps, diffs = [], []
        ps = self.parents(n)
        self.rel = dict(pre={p: set() for p in ps}, own=set(), ps=ps, pairs=None)
        kinds = lambda x: ({"w"} if x["B"] or x["G"] else set()) | ({"s"} if x["H"] else set())
        pairs = {p: self.learn(n, p) for p in ps}
        self.rel["pairs"] = pairs
        for p in ps:
            pm = pairs[p]
            if pm.ren:
                steps.append("rename %s: %s" % (p, ", ".join("%s for %s" % (b, a) for a, b in sorted(pm.ren.items()))))
            if pm.lift:
                steps.append("lift %s at %s" % (p, ", ".join("%s (place %d)" % (a, i + 1) for a, i in sorted(pm.lift.items()))))
        mine = self.F(n)
        byfirst = collections.defaultdict(list)
        for c in mine:
            byfirst[c["first"]].append(c)
        own = lambda c: any(x.split(":")[0] == n for x in c["lab"].split("+"))
        # every clause of each contract of the first line, renamed and lifted, is in n: the same, or weakened or
        # strengthened by a clause of n that cites it, or by another contract of the first line that has it so
        hit = collections.defaultdict(list)       # id of n's clause -> [(p, source clause, what it adds)]
        for p in ps:
            for c in self.F(p):
                cands = byfirst.get(c["first"], [])
                exact = [d for d in cands if embed(c, d, pairs[p], exact=True) is not None and not any(embed(c, d, pairs[p], exact=True)[k] for k in "HBG")]
                if exact:
                    d = next((d for d in exact if d["name"] == c["name"]), exact[0])
                    hit[id(d)].append((p, c, None))
                    continue
                more = [(d, embed(c, d, pairs[p])) for d in cands]
                more = [(d, x) for d, x in more if x is not None]
                if not more:
                    diffs.append(("a clause of %s that is not in %s" % (p, n), c, None))
                    continue
                d, x = min(more, key=lambda dx: (dx[0]["name"] != c["name"], sum(len(v) for v in dx[1].values())))
                hit[id(d)].append((p, c, x))
        for d in mine:
            srcs = hit.get(id(d), [])
            if not srcs:
                if own(d) and not compiled(d):
                    self.rel["own"].add("e")
                    continue        # added by n
                if compiled(d):
                    self.rel["own"].add("e")
                    base = self.base_of(d, mine)
                    if base is None:
                        diffs.append(("an act compiled from no act of %s" % n, None, d))
                    continue
                diffs.append(("a clause of %s that no contract of the first line has and %s does not write" % (n, n), None, d))
                continue
            weak = [(p, c, x) for p, c, x in srcs if x is not None]
            if not weak:
                continue
            if own(d):          # weakened or strengthened by n's own clause, which cites it: what it adds is what that clause says
                said = self.said(n, d)
                for _, c2, _ in srcs:       # and what another contract of the first line has of it (the union)
                    said[1].update(q for k in "HB" for _, q, _ in c2[k])
                bad = [(p, c, x) for p, c, x in weak if any(not self.says(said, l) for k in "HBG" for l in x[k])]
                for p, c, x in bad:
                    diffs.append(("a clause of %s that %s has with more than its clause %s says" % (p, n, ", ".join(sorted(said[0]))), c, d))
                for p, c, x in weak:
                    self.rel["own"] |= kinds(x)
                continue
            if compiled(d):
                for p, c, x in weak:
                    self.rel["own"] |= kinds(x)
                continue        # compiled anew for n
            if len(srcs) > 1 and any(x is None for _, _, x in srcs):
                # two contracts of the first line have it differently: the one is weakened or strengthened first
                for p, c, x in weak:
                    self.rel["pre"][p] |= kinds(x)
                    steps.append("weaken or strengthen %s %s of %s by %s, as %s has it" % (c["lab"], c["name"], p,
                                 "; ".join("%s %s" % ({"H": "conclusions", "B": "conditions", "G": "guards"}[k], text(v)) for k, v in x.items() if v),
                                 next(q for q, _, y in srcs if y is None)))
                continue
            if len(srcs) > 1:
                ok = True
                for p, c, x in weak:
                    others = [y for q, _, y in srcs if q != p]
                    if not others:
                        ok = False
                if ok:
                    for p, c, x in weak:
                        self.rel["pre"][p] |= kinds(x)
                    steps.append("union of %s %s of %s, each weakened or strengthened by what the other has" % (d["lab"], d["name"], ", ".join(p for p, _, _ in srcs)))
                    continue
            for p, c, x in weak:
                diffs.append(("a clause of %s that %s has with more, and that no clause of %s cites" % (p, n, n), c, d))
        return steps, diffs

    def proposed(self, n):
        """The first line of n in the seven relations, as derive() found n: 'Ads unites Community Network and a weakening
        of Overlay.'"""
        r = self.rel
        ws = lambda k: {frozenset("w"): "weakens", frozenset("s"): "strengthens", frozenset("ws"): "amends"}.get(frozenset(k))
        def noun(p):
            x, pm = p, r["pairs"][p]
            if pm.ren:
                x = "a renaming of " + x
            if pm.lift:
                x = "a lift of " + x
            k = ws(r["pre"][p])
            if k:
                x = {"weakens": "a weakening of ", "strengthens": "a strengthening of ", "amends": "an amendment of "}[k] + x
            return x
        own = (["extends"] if "e" in r["own"] else []) + ([ws(r["own"] - {"e"})] if ws(r["own"] - {"e"}) else [])
        andl = lambda xs: xs[0] if len(xs) == 1 else ", ".join(xs[:-1]) + " and " + xs[-1]
        ps = r["ps"]
        if not ps:
            return "%s." % n
        if len(ps) == 1:
            p, pm = ps[0], r["pairs"][ps[0]]
            first = (["renames"] if pm.ren else []) + (["lifts"] if pm.lift else [])
            k = ws(r["pre"][p])
            verbs = first + own
            if not verbs:
                return "%s extends %s." % (n, p)      # the clauses of p and no other: 'extends' (Udi, 2026-11-23 23:36, item 28)
            if len(verbs) == 1:
                return "%s %s %s." % (n, verbs[0], p)
            return "%s %s %s, and %s it." % (n, verbs[0], p, andl(verbs[1:]))
        line = "%s unites %s" % (n, andl([noun(p) for p in ps]))
        return line + (", and %s the union." % andl(own) if own else ".")

    GEN = ("vote_", "voter_", "taker_", "decided_")      # the states of a vote, compiled for an act decided on votes

    def said(self, n, d):
        """The labels of n's clauses that d's label names, and the predicates and guards their schemas write."""
        labs = {x for x in d["lab"].split("+") if x.split(":")[0] == n}
        preds = set()
        for l in open(self.path(n, "sas"), encoding="utf-8"):
            m = re.match(r"^(\S.*?) [%s] " % MARKS, l)
            if m and m.group(1) in labs and "<-" in l:
                preds |= set(re.findall(r"([a-z_0-9]+)\(", l))
                g = l.split(" guarded by ", 1)[1] if " guarded by " in l else ""
                preds |= set(re.findall(r"[A-Z][A-Za-z0-9]*", g))
                preds |= set(re.findall(r"taken for ([A-Z][A-Za-z0-9]*) by ([A-Z][A-Za-z0-9]*)", l) and
                             [y for x in re.findall(r"taken for ([A-Z][A-Za-z0-9]*) by ([A-Z][A-Za-z0-9]*)", l) for y in x])
        return labs, preds

    def says(self, said, l):
        neg, p, a = l
        if p.startswith(self.GEN) or p in ("product", "knows"):
            return True
        if p in ("", "must"):
            return True     # a guard: the names are the clause's, under the map of names
        return p in said[1] or p.split("(")[0] in said[1]

    def base_of(self, d, mine):
        """The act d is compiled from: 'Pay for Alice' from Pay, 'Vote of Alice for Pay' from Pay; its conclusions
        and conditions are in d."""
        if d["name"].startswith("Vote of "):
            m = re.match(r"Vote of (.+?) for (.+)$", d["name"])
            bn = m.group(2) if m else None
            return next((c for c in mine if c["name"] in (bn, "%s for %s" % (bn, m.group(1)) if m else "")), None)
        bn = re.sub(r" for [A-Z][A-Za-z]*(?: and [A-Z][A-Za-z]*)*$", "", d["name"])
        for c in mine:
            if c is d or c["name"] != bn or c["first"] != d["first"]:
                continue
            if embed(c, dict(d, G=[]), Pair(), False) is not None or embed(dict(c, G=[]), dict(d, G=[]), Pair()) is not None:
                return c
        return None


def main(argv):
    root, out, names, fl = ROOT, None, [], None
    i = 0
    while i < len(argv):
        if argv[i] == "--root":
            root = argv[i + 1]
            i += 2
        elif argv[i] == "--first-lines":
            fl = argv[i + 1]
            i += 2
        elif argv[i] == "--out":
            out = argv[i + 1]
            i += 2
        else:
            names.append(NM.name_of(argv[i]) if NM.name_of(argv[i]) else argv[i])
            i += 1
    D = Deriver(root)
    names = names or D.names
    lines, derived, other, fls = [], [], [], []
    for n in names:
        steps, diffs = D.derive(n)
        present = open(os.path.join(root, "rev", NM.file_of(n) + ".txt") if not D.pub else os.path.join(root, NM.file_of(n), "contract.txt"),
                       encoding="utf-8").read().split("\n")[1]
        cite = "".join(" " + c for c in re.findall(r"\([^()]*(?:arxiv|in preparation)[^()]*\)", present))      # the papers it cites, kept
        prop = D.proposed(n)[:-1] + cite + "."
        fls.append("%s\n    present:  %s\n    proposed: %s%s" % (n, present, prop,
                   "" if not diffs else "\n    not derived: %d clauses differ (derive.txt)" % len(diffs)))
        lines.append("%s: %s" % (n, "derived" if not diffs else "%d clauses differ" % len(diffs)))
        lines.append("    first line: %s" % D.first_line(n))
        lines.append("    proposed:   %s" % D.proposed(n))
        for s in steps:
            lines.append("    step: %s" % s)
        for what, c, d in diffs:
            lines.append("    differs: %s" % what)
            lines.append("        source: %s" % (c["raw"] if c else "none"))
            lines.append("        %s: %s" % (n, d["raw"] if d else "none"))
        (other if diffs else derived).append((n, len(diffs)))
    lines.append("")
    lines.append("%d contracts, %d derived" % (len(names), len(derived)))
    for n, k in other:
        lines.append("not derived: %s, %d clauses differ" % (n, k))
    if fl:
        open(fl, "w", encoding="utf-8").write("\n".join(fls) + "\n")
    s = "\n".join(lines) + "\n"
    if out:
        open(out, "w", encoding="utf-8").write(s)
    sys.stdout.write(s if not out else "\n".join(lines[-(len(other) + 2):]) + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
