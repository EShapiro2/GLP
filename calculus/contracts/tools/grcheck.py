#!/usr/bin/env python3
"""grcheck.py --- compares the Logic pragmas of a contract, gr/NAME.txt, with its approved clauses, rev/NAME.txt.

    python3 tools/grcheck.py [NAME ...]      every contract of rev/ if none is named

gr/NAME.txt is written by hand from rev/NAME.txt (tools/SAS-WRITING.txt), and gcsas.py reads gr/ and never rev/.  This tool prints, for each contract, how the two files differ, of five kinds.

1. The title and the first line.  The titles are equal.  The first lines are equal once a reference in parentheses is taken out; if they are not, they have the same relations (extends, restricts, relaxes, strengthens, augments, instantiates) and name the same contracts, which is what SAS-WRITING.txt allows the pragmas to write otherwise ("T instantiates P with ITEMS").
2. The clauses.  The same numbers, in order, each with the same legal modality (■ ◆ ○) and the same title, or, if the title of rev/ is a list of act names ("Leave, Expel"), each of them an act name of gr/; a title of rev/ may be an act name of gr/, or begin one ("Count", "Count for"); a clause that one file has and the other lacks; an act name of the clause of rev/ ("Leave, Expel:") that the clause of gr/ lacks, unless an act name of gr/ begins with it ("Swap item for coin" for "Swap") or it is the title or an act name of a clause that the clause cites ("Escrow", Notes:4).  gr/ may name more acts, one sentence for each.
3. The citations.  The clause of gr/ cites each clause NAME:n that the clause of rev/ cites: itself, or through a clause it cites (Bonds:2 for Coins:2), or by writing its acts out, an act name of gr/ beginning with each ("Deposit item", Agora:6, for Notes:4); a citation of rev/ in parentheses, "(Guardians:1)", is a reference and gr/ may write the act out.  A clause that gr/ cites and rev/ does not is of a contract that the contract is written from; it is then a finding of kind 5, since a clause of rev/ may restrict acts it does not cite ("Every act of Alf requires this attestation").  An act name that gr/ writes after a citation ("Guardians:2 Make guardian") is the title or an act name of that clause, in rev/ or in gr/, or a form of an act of it that flat/ of a contract it is written from prints ("Notes:4 Deposit bond", flat/Bonds.txt).
4. The names.  Every name of the names line of rev/ is in the names line of gr/, and every such name that a clause of rev/ uses the clause of gr/ uses, or a clause of gr/ that it cites does.  gr/ may have more names.
5. Conditions and effects, a finding for a person to judge and not a fault.  A condition of gr/ is the text after ", provided" or ", only if", and an effect the text after ", and then", up to the next such phrase or the end of the sentence.  A condition of rev/ is marked by one of the words provided, only if, if, who, whom, whose, where, when, while, unless, until, once, new, not yet, and an effect by then or after.  The tool reports (a) a clause of gr/ with a condition, or an effect, if the clause of rev/ has no word that marks one, and the reverse; and (b) a condition or an effect of gr/ with a word that the clause of rev/ does not have, the names, the short common words and the endings -s, -es, -ed and -ing aside ("provided Alice and Bob know each other", Friendship:1, whose clause of rev/ does not say know).

What kind 5 does not judge: whether a sentence of gr/ says what the English of rev/ says.  It compares words and not meanings, it does not look into a clause that is cited, so the conditions a citation adds are all reported under (b), and it reads no line after "Logic pragmas.".  Reading sas/NAME.txt against rev/NAME.txt remains a person's judgement (SAS-WRITING.txt, RUN AND CHECK).

The last line counts the contracts, those with a difference of kinds 1 to 4 and those with a finding of kind 5.  Exit 1 if a contract has a difference of kinds 1 to 4.
"""
import os, re, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CLAUSE = re.compile(r"^(\d+)\. ([■◆○]) (.*)$")
RELATIONS = ("extends", "restricts", "relaxes", "strengthens", "augments", "instantiates", "weakens", "renames", "lifts", "unites")
ACTS = re.compile(r"(?:^|(?<=[.;] ))([A-Z][A-Za-z' -]*(?:, [A-Z][A-Za-z' -]*)*): ")
GR_PHRASE = re.compile(r", (provided|only if|and then) ")
GR_END = re.compile(r", (?:provided|only if|and then|with the consent of|without the consent of|decided by|on the votes of|taken for) |\.(?: |$)")
REV_CONDITION = re.compile(r"\b(provided|only if|if|who|whom|whose|where|when|while|unless|until|once|new|not yet)\b")
REV_EFFECT = re.compile(r"\b(then|after)\b")
COMMON = set("""a an the of to in on at by for with from as and or not no nor is are be been was it its he she they
his her him them their that which this those these each other who whom may will must can only if then also any every
all one has have had does do than more most so both either such there own into out up per""".split())


def read(d, name):
    """(title, first line, names line, {n: (mark, text)}, the numbers in order) of d/NAME.txt, or None."""
    p = os.path.join(ROOT, d, NM.file_of(name) + ".txt")
    if not os.path.exists(p):
        return None
    lines = [l.rstrip("\n") for l in open(p, encoding="utf-8")]
    head, clauses, order = [], {}, []
    for l in lines:
        if l.strip() == "Logic pragmas.":
            break
        m = CLAUSE.match(l)
        if m:
            clauses[int(m.group(1))] = (m.group(2), m.group(3))
            order.append(int(m.group(1)))
        elif not clauses and l.strip():
            head.append(l)
    title = head[0] if head else ""
    first = head[1] if len(head) > 1 else ""
    namesline = head[-1] if len(head) > 2 and order else ""
    return title, first, namesline, clauses, order


def title_of(text):
    """The title of a clause: its text up to the first ': ' or '. '."""
    m = re.search(r"[:.](?: |$)", text)
    return text[:m.start()] if m else text


def acts_of(text):
    """The act names of a clause: its title, and each name or list of names before ': ' at the start of a sentence."""
    out = {a.strip() for a in title_of(text).split(",")}
    for m in ACTS.finditer(text):
        out.update(a.strip() for a in m.group(1).split(","))
    return out


def cites(text, known):
    """The citations NAME:n of a text, each with the act name written after it, if any: [(NAME, n, act or None)]."""
    out = []
    for s, e, n in NM.find(text, known):
        m = re.match(r":(\d+)(?: ([A-Z][^,.:;]*))?", text[e:])
        if m:
            out.append((n, int(m.group(1)), m.group(2)))
    return out


def paren_cites(text, known):
    """The citations of a text that are in parentheses, '(Guardians:1)': references, which gr/ may cite or write out."""
    out = set()
    for m in re.finditer(r"\(([^()]*)\)", text):
        out |= {(c, k) for c, k, _ in cites(m.group(1), known)}
    return out


def ancestors(name, known, get, seen=None):
    """The contracts a contract is written from: those its first line names in rev/ or gr/, and theirs."""
    seen = set() if seen is None else seen
    for d in ("rev", "gr"):
        f = get(d, name)
        for _, _, n in (NM.find(f[1], known) if f else []):
            if n != name and n not in seen:
                seen.add(n)
                ancestors(n, known, get, seen)
    return seen


def reach(c, k, known, get, seen=None):
    """The clauses that the clause c:k of gr/ cites, and those they cite."""
    seen = set() if seen is None else seen
    f = get("gr", c)
    for c2, k2, _ in (cites(f[3][k][1], known) if f and k in f[3] else []):
        if (c2, k2) not in seen:
            seen.add((c2, k2))
            reach(c2, k2, known, get, seen)
    return seen


def clause_acts(c, k, get):
    """The title and act names of the clause c:k, in rev/ and in gr/."""
    have = set()
    for dd in ("rev", "gr"):
        f = get(dd, c)
        if f and k in f[3]:
            have |= acts_of(f[3][k][1])
    return have


def flat_acts(c, k, among):
    """The act names that flat/ of the contracts 'among' prints for the clause c:k: its acts and their forms ('Deposit bond')."""
    out = set()
    for n in among:
        p = os.path.join(ROOT, "flat", NM.file_of(n) + ".txt")
        if os.path.exists(p):
            for l in open(p, encoding="utf-8"):
                m = re.match(re.escape("%s:%d" % (c, k)) + r"(?:\+\S.*?)? [■◆○] ([^:]+): ", l)
                if m:
                    out.add(m.group(1).strip())
    return out


def begins(names, a):
    """A name of 'names' is a, or begins with a and a space ('Swap item for coin' for 'Swap')."""
    return any(x == a or x.startswith(a + " ") for x in names)


def names_of(line):
    """The names a names line declares: its capitalised words."""
    return set(re.findall(r"\b[A-Z][A-Za-z]*\b", line))


def uses(text, name):
    return re.search(r"\b" + re.escape(name) + r"\b", text) is not None


def stem(w):
    for s in ("ing", "ed", "es", "s"):
        if w.endswith(s) and len(w) - len(s) >= 3:
            return w[:-len(s)]
    return w


def words(text):
    return {stem(w) for w in re.findall(r"[a-z]+", text.lower())}


def phrases(text):
    """The conditions and effects of a clause of gr/: [(kind, the phrase with its first word)]."""
    out = []
    for m in GR_PHRASE.finditer(text):
        e = GR_END.search(text, m.end())
        end = e.start() if e else len(text)
        out.append(("effect" if m.group(1) == "and then" else "condition", text[m.start() + 2:end]))
    return out


def first_line_same(a, b, known):
    strip = lambda s: re.sub(r"\s*\([^()]*\)", "", s).strip()
    a, b = strip(a), strip(b)
    if a == b:
        return True
    rel = lambda s: sorted(r for r in RELATIONS if re.search(r"\b" + r + r"\b", s))
    con = lambda s: sorted({n for _, _, n in NM.find(s, known)})
    return rel(a) == rel(b) and con(a) == con(b)


def check(name, known, cache):
    """(the differences of kinds 1 to 4, the findings of kind 5) of a contract, each a line."""
    get = lambda d, n: cache.setdefault((d, n), read(d, n))
    rev, gr = get("rev", name), get("gr", name)
    if rev is None or gr is None:
        return ["%s: 2 no %s/%s.txt" % (name, "rev" if rev is None else "gr", NM.file_of(name))], []
    diffs, finds = [], []
    d = lambda n, kind, what: diffs.append("%s%s: %d %s" % (name, ":%d" % n if n else "", kind, what))
    if rev[0] != gr[0]:
        d(0, 1, "the title of gr/ is '%s'" % gr[0])
    if not first_line_same(rev[1], gr[1], known):
        d(0, 1, "the first lines differ in their relations or in the contracts they name")
    if rev[4] != sorted(rev[4]) or gr[4] != sorted(gr[4]):
        d(0, 2, "the clauses are not in the order of their numbers")
    for n in sorted(set(rev[3]) - set(gr[3])):
        d(n, 2, "gr/ lacks the clause")
    for n in sorted(set(gr[3]) - set(rev[3])):
        d(n, 2, "rev/ lacks the clause")
    lost = names_of(rev[2]) - names_of(gr[2]) - {name.split()[0]}
    if lost:
        d(0, 4, "the names line of gr/ lacks " + ", ".join(sorted(lost)))
    declared = names_of(rev[2]) | names_of(gr[2])
    anc = ancestors(name, known, get)
    for n in sorted(set(rev[3]) & set(gr[3])):
        (rm, rt), (gm, gt) = rev[3][n], gr[3][n]
        if rm != gm:
            d(n, 2, "the legal modality is %s in rev/ and %s in gr/" % (rm, gm))
        rc, gc = cites(rt, known), cites(gt, known)
        rs, gs = {(c, k) for c, k, _ in rc}, {(c, k) for c, k, _ in gc}
        far = set(gs)
        for c, k in gs:
            far |= reach(c, k, known, get)
        gacts = acts_of(gt)
        out = lambda c, k: any(begins(gacts, a) for a in clause_acts(c, k, get))     # written out, act by act
        met = {(c, k) for c, k in rs if (c, k) in far or (c, k) in paren_cites(rt, known) or out(c, k)}
        cited = set()
        for c, k in met | far:
            cited |= clause_acts(c, k, get)
        if title_of(rt) != title_of(gt) and "," not in title_of(rt) and not begins(gacts, title_of(rt)):
            d(n, 2, "the title is '%s' in rev/ and '%s' in gr/" % (title_of(rt), title_of(gt)))
        else:
            for a in sorted(acts_of(rt) - gacts):
                if not begins(gacts, a) and a not in cited:
                    d(n, 2, "gr/ lacks the act name '%s'" % a)
        for c, k in sorted(rs - met):
            d(n, 3, "gr/ does not cite %s:%d" % (c, k))
        for c, k in sorted(gs - rs):
            if c in anc:
                finds.append("%s:%d: 5 gr/ cites %s:%d, a clause of a contract %s is written from, which rev/ does not cite" % (name, n, c, k, name))
            else:
                d(n, 3, "gr/ cites %s:%d, which rev/ does not, and %s is not written from %s" % (c, k, name, c))
        for c, k, act in gc:
            if act is None:
                continue
            if act.strip() not in clause_acts(c, k, get) | flat_acts(c, k, anc):
                d(n, 3, "gr/ cites the act '%s' of %s:%d, which has no act of that name" % (act.strip(), c, k))
        below = " ".join(get("gr", c)[3][k][1] for c, k in far if get("gr", c) and k in get("gr", c)[3])
        for p in sorted(names_of(rev[2])):
            if uses(rt, p) and not uses(gt, p) and not uses(below, p):
                d(n, 4, "rev/ names %s and neither gr/ nor a clause it cites does" % p)
        ph = phrases(gt)
        for kind, rx in (("condition", REV_CONDITION), ("effect", REV_EFFECT)):
            g, r = [p for k, p in ph if k == kind], rx.search(rt)
            if g and not r:
                finds.append("%s:%d: 5 gr/ has %s %s ('%s') and rev/ has no word for one" % (name, n, "a" if kind == "condition" else "an", kind, g[0]))
            if r and not g and rt != gt:
                finds.append("%s:%d: 5 rev/ has the word '%s' and gr/ has no %s" % (name, n, r.group(1), kind))
        have = words(rt) | {stem(x.lower()) for x in declared}
        for kind, p in ph:
            new = sorted(w for w in words(p.split(" ", 2 if p.startswith(("only if", "and then")) else 1)[-1]) - have if w not in COMMON)
            if new:
                finds.append("%s:%d: 5 the %s '%s' has words that rev/ does not: %s" % (name, n, kind, p, ", ".join(new)))
    return diffs, finds


def main(argv):
    known = sorted(set(NM.names_in(os.path.join(ROOT, "rev"))) | set(NM.names_in(os.path.join(ROOT, "gr"))))
    todo = [NM.name_of(a[:-4] if a.endswith(".txt") else a) if a not in known else a for a in argv] or known
    cache, with_diff, with_find = {}, 0, 0
    for name in todo:
        diffs, finds = check(name, known, cache)
        for l in diffs + finds:
            print(l)
        with_diff += bool(diffs)
        with_find += bool(finds)
    print("%d contracts, %d with a difference of kinds 1 to 4, %d with a finding of kind 5." % (len(todo), with_diff, with_find))
    return 1 if with_diff else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
