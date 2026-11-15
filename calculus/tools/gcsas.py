#!/usr/bin/env python3
"""gcsas.py --- the four arrows of the diagram, as programs.

    python3 tools/gcsas.py [NAME ...]        all contracts of gr/, or those named with what they are written from

Reads gr/NAME.txt, the Logic pragmas of the contract rev/NAME.txt: its vocabulary, the meanings of its phrases and,
for each act of each clause, a sentence in the grammar of Section 1.1 (tools/SAS-WRITING.txt); and writes
    sas/NAME.txt    its schemas: the schemas of its own acts, its citations as restrictions, its first line
    flat/NAME.txt   its flat text: the operations of its first line and citations applied on the text
    sasx/NAME.txt   its expanded schemas: the same operations applied on the schemas of sas/
    gx/NAME.txt     its expanded sentences, in the grammar, written from sasx/NAME.txt
and checks, for each contract:
    recover   the contract recovered from sas/NAME.txt alone is gr/NAME.txt (same: character for character;
              equivalent: same schemas, and the recovered contract is then written to back/NAME.txt)
    commute   the schemas of flat/NAME.txt are sasx/NAME.txt
    return    the flat text recovered from sasx/NAME.txt is flat/NAME.txt
Standard library only.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
NAME = r"[A-Z][A-Za-z]*"
PH = re.compile(r"^([b-z])('s|-[A-Za-z]+)?$")
MARKS = "■◆○"
PRAGMAS = "Logic pragmas."
CLAUSE = re.compile(r"^(\d+)\. ([%s]) (.*)$" % MARKS)
ENTRY = re.compile(r"^((?:[A-Za-z-]+:\d+)(?:\+[A-Za-z-]+:\d+)*) ([%s-]) (.*)$" % MARKS)
REF = r"[A-Za-z-]+:\d+"
PRON = {"Alice": "her", "Alicia": "her", "Carol": "her", "Erin": "her", "Bob": "his", "Dave": "his"}
POSS = {"her": "hers", "his": "his", "its": "its", "their": "theirs"}
WILD = "_"        # in a place: someone; in a state that must not hold, or that ends: anyone
WILDW = ("someone", "anyone", "no one")
SIGNED = r"%s(?: signed by %s)+" % (NAME, NAME)   # a place holding a thing with its signatures: 'Item signed by Alice', in a schema Item@Alice
COUNTS = {"fewer than": "<", "more than": ">", "at most": "<=", "at least": ">="}   # 'fewer than D persons' in a place: <D
COUNTW = r"(?:%s) (?:[A-Z][A-Za-z]*|\d+) persons" % "|".join(COUNTS)
SIGNS = {v: k for k, v in COUNTS.items()}


def count_code(v):
    m = re.fullmatch(r"(%s) (\w+) persons" % "|".join(COUNTS), v)
    return COUNTS[m.group(1)] + m.group(2) if m else v
NOUNS = set()     # 'the role r' in a basic state: 'no role' and 'some role' are written for 'the role no one', 'the role someone'
MARK = "*#"   # the last argument of an atom: *n, n copies; #n, exactly n copies and no more
ARITH = "Vocabulary: n is at most m = le(n,m); n is at least m = ge(n,m); n is more than m = gt(n,m); n is less than m = lt(n,m); n is m plus k = sum(n,m,k); n is m times k = product(n,m,k)."   # of every contract
ACTOR = "Actor"   # in a citation: the first of those who take the act cited
IRREG = {"is": "be", "has": "have", "does": "do", "goes": "go"}


class Err(Exception):
    pass


def base(verb):
    if verb in IRREG:
        return IRREG[verb]
    if verb.endswith("ies"):
        return verb[:-3] + "y"
    if re.search(r"(ss|sh|ch|x|z)es$", verb):
        return verb[:-2]
    return verb[:-1] if verb.endswith("s") else verb


def cap(s):
    return s[:1].upper() + s[1:]


def low(s):
    return s if re.match(NAME + r"('s|-\w+)?(\W|$)", s) and not re.match(r"(Those|The|A|An|Every|Each|Its|No|Some)\b", s) else s[:1].lower() + s[1:]


# ---- templates: phrases with places ------------------------------------------------

def toks(t):
    return t.split(" ")


def is_place(tok):
    return bool(PH.match(tok.rstrip(",")))


def of_hers(tk, i):
    """The token i of a phrase is 'x's' after 'of' and ends its noun phrase: 'a new name of x's' -> 'of hers'."""
    return i > 0 and tk[i - 1] == "of" and (i == len(tk) - 1 or tk[i].endswith(","))


def tok_rx(tok, seen, fixed=None, subj=None, poss=False, article=False):
    """The regex of one token of a template."""
    if article:
        return "an?"
    comma = "," if tok.endswith(",") else ""
    core = tok[:-1] if comma else tok
    m = PH.match(core)
    if not m:
        return re.escape(tok)
    v, suf = m.group(1), m.group(2) or ""
    if fixed is not None and v in fixed:
        val = re.escape(fixed[v])
        if suf == "'s" and v == subj:
            return ("(?:hers|his|its|theirs|%s's)%s" if poss else "(?:her|his|its|their|%s's)%s") % (val, comma)
        return val + re.escape(suf) + comma
    if v in seen:
        return "(?P=%s)%s%s" % (v, re.escape(suf), comma)
    seen.add(v)
    return "(?P<%s>%s|%s|%s|%s|\\d+)%s%s" % (v, COUNTW, SIGNED, NAME, "|".join(WILDW), re.escape(suf), comma)


def wild(s, neg):
    """A phrase with the wildcard, in words: someone, or anyone where the state must not hold or ends."""
    s = re.sub(r"(?<![A-Za-z_<>=])(<=|>=|<|>)(\w+)", lambda m: "%s %s persons" % (SIGNS[m.group(1)], m.group(2)), s)
    return re.sub(r"(?<![A-Za-z_])_(?![A-Za-z_])", "anyone" if neg else "someone", s)


def tpl_rx(t, fixed=None, subj=None, first_any_case=True):
    seen, parts, tk = set(), [], toks(t)
    for i, tok in enumerate(tk):
        art = tok in ("a", "an") and i + 1 < len(tk) and is_place(tk[i + 1])
        r = tok_rx(tok, seen, fixed, subj, of_hers(tk, i), art)
        if i == 0 and first_any_case and tok[:1].islower() and not PH.match(tok.rstrip(",")):
            r = "[Aa]n?" if art else "[%s%s]%s" % (tok[0].upper(), tok[0], re.escape(tok[1:]))
        parts.append(r)
    return " ".join(parts)


def fill(t, b, subj=None, pron=False):
    out, tk, filled = [], toks(t), set()
    for i, tok in enumerate(tk):
        comma = "," if tok.endswith(",") else ""
        core = tok[:-1] if comma else tok
        m = PH.match(core)
        if m and m.group(1) in b:
            v, suf = m.group(1), m.group(2) or ""
            filled.add(i)
            if pron and suf == "'s" and v == subj:
                p = PRON.get(b[v], "its" if re.fullmatch(NAME, b[v]) else "their")
                out.append((POSS[p] if of_hers(tk, i) else p) + comma)
            else:
                out.append(b[v].replace("@", " signed by ") + suf + comma)
        else:
            out.append(tok)
    for i in filled:      # 'a y-coin' of Alice is 'an Alice-coin'
        if i > 0 and tk[i - 1] in ("a", "an") and re.match(NAME, out[i]):
            out[i - 1] = "an" if out[i][0] in "AEIOU" else "a"
    return " ".join(out)


def tvars(t):
    vs = []
    for tok in toks(t):
        m = PH.match(tok.rstrip(","))
        if m and m.group(1) not in vs:
            vs.append(m.group(1))
    return vs


def subject(t):
    """The subject places of an act phrase and the rest: 'x and y become friends' -> [x, y], 'become friends'."""
    tk, vs, i = toks(t), [], 0
    while i < len(tk):
        core = tk[i].rstrip(",")
        if PH.fullmatch(core) and PH.match(core).group(2) is None:
            vs.append(core)
            i += 1
            if i < len(tk) and tk[i] == "and":
                i += 1
            elif not tk[i - 1].endswith(","):
                break
        else:
            break
    return vs, " ".join(tk[i:])


PARTICIPLES = {"been", "given", "taken", "shown", "seen", "made", "paid", "sent", "held", "set", "put", "known", "written", "sold", "bought", "left", "met", "kept"}


def negative(t):
    """The English negation of a state phrase whose subject is its leading places: 'x knows y' -> 'x does not know y'."""
    sv, rest = subject(t)
    if not sv or not rest:
        w = t.split(" ")
        for i, x in enumerate(w[:-1]):
            if i and x in ("is", "are") or i and x in ("has", "have") and (w[i + 1].endswith("ed") or w[i + 1] in PARTICIPLES):
                return " ".join(w[:i + 1] + ["not"] + w[i + 1:])
        return None
    w = rest.split(" ")
    head = t[:len(t) - len(rest)]
    if w[0] in ("is", "are"):
        return head + " ".join([w[0], "not"] + w[1:])
    if w[0] in ("has", "have") and len(w) > 1 and (w[1].endswith("ed") or w[1] in PARTICIPLES):
        return head + " ".join([w[0], "not"] + w[1:])
    if len(sv) == 1:
        return head + " ".join(["does", "not", base(w[0])] + w[1:])
    return head + " ".join(["do", "not"] + w)


def not_text(t, b):
    n = negative(t)
    return fill(n, b) if n else "not " + fill(t, b)


# ---- atoms, literals, schemas --------------------------------------------------------

def atom_s(a):
    return "%s(%s)" % (a[0], ",".join(a[1]))


def lit_s(l):
    return ("~" if l[0] else "") + atom_s(l[1:])


def parse_atom(s):
    m = re.fullmatch(r"(~?)([a-z_0-9]+)\(([^()]*)\)", s.strip())
    if not m:
        raise Err("not an atom: " + s)
    return (bool(m.group(1)), m.group(2), tuple(x.strip() for x in m.group(3).split(",") if x.strip()))


def split_top(s):
    out, depth, cur = [], 0, ""
    for ch in s:
        if ch == "(":
            depth += 1
        if ch == ")":
            depth -= 1
        if ch == "," and depth == 0:
            out.append(cur.strip())
            cur = ""
        else:
            cur += ch
    if cur.strip():
        out.append(cur.strip())
    return out


def join(items):
    items = list(items)
    if len(items) < 2:
        return "".join(items)
    return ", ".join(items[:-1]) + " and " + items[-1]


# ---- the lexicon of a contract: its vocabulary, roles and definitions, with those it inherits ----

class Lex:
    def __init__(self):
        self.basic = []   # (template, pred, args)
        self.roles = []   # (template, pred, args)
        self.defs = []    # (template, right side, whether of an act: 'After x ...: effects.')
        self.req = {}     # act phrase -> the conditions it requires: 'After x ..., which requires c: effects.'
        self.rlist = {}   # role predicate -> the states that list its agents: 'those who decide for z = deciders(z), listed by z has y as a guardian and y is a guardian of z'
        self.short = {}   # basic state -> (the state as a sentence writes it, its holder's place): 'e is late, for x = late(x,e)'

    def copy(self):
        l = Lex()
        l.basic, l.roles, l.defs, l.req, l.short = list(self.basic), list(self.roles), list(self.defs), dict(self.req), dict(self.short)
        l.rlist = dict(self.rlist)
        return l

    def add(self, other):
        for attr in ("basic", "roles", "defs"):
            mine = getattr(self, attr)
            have = {e[0] for e in mine}
            mine.extend(e for e in getattr(other, attr) if e[0] not in have)
        for k, v in other.req.items():
            self.req.setdefault(k, v)
        for k, v in other.short.items():
            self.short.setdefault(k, v)
        for k, v in other.rlist.items():
            self.rlist.setdefault(k, v)

    def states(self):
        """Templates of states, longest first, each with its payload."""
        ts = [(t, ("basic", t, p, a)) for t, p, a in self.basic] + [(t, ("def", t, r)) for t, r, act in self.defs if not act]
        ts += [(self.short[t][0], ("short", t, p, a, self.short[t][1])) for t, p, a in self.basic if t in self.short]
        ts += [(negative(t), ("neg",) + pay) for t, pay in ts if negative(t)]
        ts.sort(key=lambda e: (-len(toks(e[0])), e[0]))
        return [(re.compile(tpl_rx(t) + r"(?=$|[ ,.])"), pay) for t, pay in ts]

    def persons(self):
        ts = sorted(self.roles, key=lambda e: -len(toks(e[0])))
        return [(re.compile(tpl_rx(t) + r"(?=$|[ ,.])"), ("role", t, p, a)) for t, p, a in ts] + \
               [(re.compile(NAME + r"(?=$|[ ,.])"), ("name",))]


def arith():
    l = Lex()
    for e in ARITH[len("Vocabulary: "):-1].split("; "):
        t, a = e.rsplit(" = ", 1)
        l.basic.append((t, parse_atom(a)[1], parse_atom(a)[2]))
    return l


def unwild(s, word="_"):
    return re.sub(r"(?<![A-Za-z_])_(?![A-Za-z_])", word, s)


def none_text(s):
    """A state of no one: its first unnamed place is 'no one', the others 'anyone'."""
    k = [0]

    def f(m):
        k[0] += 1
        return "no one" if k[0] == 1 else "anyone"
    return re.sub(r"(?<![A-Za-z_])_(?![A-Za-z_])", f, s)


def nouns_out(s):
    return re.sub(r"\bthe (%s) (no one|someone|anyone)\b" % "|".join(sorted(NOUNS)), lambda m: {"no one": "no ", "someone": "some ", "anyone": "any "}[m.group(2)] + m.group(1), s) if NOUNS else s


def parse_list(text, tpls, what="states"):
    text = text.strip()
    if NOUNS:
        text = re.sub(r"\b(no|some|any) (%s)\b" % "|".join(sorted(NOUNS)), lambda m: "the %s %s" % (m.group(2), {"no": "no one", "some": "someone", "any": "anyone"}[m.group(1)]), text)

    def rec(pos):
        neg, p = False, pos
        for pre in ("not ", "no longer "):
            if text.startswith(pre, p):
                neg, p = True, p + len(pre)
                break
        for rx, pay in tpls:
            m = rx.match(text, p)
            if not m:
                continue
            gd = m.groupdict()
            none = "no one" in gd.values()
            if none and (neg or pay[0] == "neg"):
                continue
            gd = {k: WILD if v in WILDW else count_code(v).replace(" signed by ", "@") for k, v in gd.items()}
            if (neg or none or pay[0] == "neg") and any(v[:1] in "<>" for v in gd.values()):
                continue
            item = (neg or none, pay, gd, text[p:m.end()])
            if pay[0] == "neg":
                if neg:
                    continue
                item = (True, pay[1:], gd, fill(pay[2], gd))
            if m.end() == len(text):
                return [item]
            for sep in (", and ", " and ", ", "):
                if text.startswith(sep, m.end()):
                    r = rec(m.end() + len(sep))
                    if r is not None:
                        return [item] + r
        return None
    if not text:
        return []
    r = rec(0)
    if r is None:
        raise Err("cannot read these %s: %s" % (what, text))
    return r


def unfold(text, lex, neg=False, actor=None):
    """A list of states, or of effects, as basic states: [(neg, template, binding, pred, args)].
    A state whose holder the sentence does not name is held by actor, the first of those who act."""
    out = []
    for n, pay, b, _ in parse_list(text, lex.states()):
        if pay[0] == "short":
            if not actor:
                raise Err("nobody acts, so nobody holds: " + text)
            b = dict(b)
            b[pay[4]] = actor
            pay = ("basic",) + pay[1:4]
        if pay[0] == "basic":
            out.append((n ^ neg, pay[1], b, pay[2], tuple(v[0] + b[v[1:]] if v[:1] in MARK else b.get(v, v) for v in pay[3])))   # a constant, as now; *n: n copies; #n: exactly n
        else:
            if n or neg:
                inner = unfold(unwild(fill(pay[2], b), "anyone"), lex, actor=actor)
                if any(i[0] for i in inner):
                    raise Err("'not' or 'no longer' on a phrase that has one: " + text)
                out.extend((True,) + i[1:] for i in inner)
            else:
                inner = unfold(unwild(fill(pay[2], b), "anyone"), lex, actor=actor)
                if any(v[:1] in '<>=' for v in b.values()) and len(inner) > 1:
                    raise Err("a count in a state defined by several states, which would not count one person: " + text)
                if WILD in b.values() and len(inner) > 1:
                    raise Err("'someone' in a state defined by several states, which would not be one person: " + text)
                out.extend(inner)
    return out


def persons_of(text, lex):
    """A list of persons: [(text, guard term)]."""
    out = []
    for _, pay, b, s in parse_list(text, lex.persons(), "persons"):
        if pay[0] == "name":
            out.append((s, s))
        else:
            out.append((low(s), "%s(%s)" % (pay[2], ",".join(b[v] for v in pay[3]))))
    return out


def lits_of(text, lex, actor=None):
    return [(n, p, a) for n, _, _, p, a in unfold(text, lex, actor=actor)]


def phrases_of(text, lex, actor=None):
    return [(n, wild(fill(t, b), n)) for n, t, b, _, _ in unfold(text, lex, actor=actor)]


# ---- reading a contract of gr/ -------------------------------------------------------

class Contract:
    pass


def strip_refs(s):
    return re.sub(r" \([^()]*\)", "", s)


def read_gr(name, d="gr"):
    c = Contract()
    c.name = name
    lines = [l.rstrip("\n") for l in open(os.path.join(ROOT, d, name + ".txt"), encoding="utf-8") if l.strip()]
    c.title = lines[0]
    c.first = None
    c.own = Lex()
    c.header = []      # (kind, line): vocabulary, roles, definitions, prose and names, in order
    c.clauses = []     # (number, mark, text)
    for i, l in enumerate(lines[1:]):
        m = CLAUSE.match(l)
        if m:
            c.clauses.append((int(m.group(1)), m.group(2), m.group(3)))
        elif l == PRAGMAS:
            continue
        elif i == 0 and re.match(re.escape(name) + r" (extends|restricts|strengthens|instantiates|composes)\b", l):
            c.first = l
        elif l.startswith("Vocabulary: ") or l.startswith("Roles: "):
            kind, rest = l.split(": ", 1)
            for e in rest.rstrip(".").split("; "):
                t, a = e.rsplit(" = ", 1)
                by = None
                if ", listed by " in a:
                    a, by = a.split(", listed by ", 1)
                _, p, args = parse_atom(a)
                if by:
                    c.own.rlist[p] = by
                NOUNS.update(re.findall(r"\bthe ([a-z]+) [b-z](?= |$)", t))
                hm = re.search(r", for ([b-z])$", t)
                if hm:
                    t = t[:hm.start()] + " for " + hm.group(1)
                    c.own.short[t] = (t[:-len(" for x")], hm.group(1))
                (c.own.basic if kind == "Vocabulary" else c.own.roles).append((t, p, args))
            c.header.append(("lex", l))
        elif ": " in l and "*" not in l and tvars(l.split(": ", 1)[0]) and l.endswith("."):
            t, r = l[:-1].split(": ", 1)
            act = t.startswith("After ")
            t = t[6:] if act else t
            if act and ", which requires " in t:
                t, c.own.req[t.split(", which requires ", 1)[0]] = t.split(", which requires ", 1)
            c.own.defs.append((t, r, act))
            c.header.append(("lex", l))
        else:
            c.header.append(("prose", l))
    return c


def first_line(c, known):
    """The contracts of the first line in order, and the substitutions of each instantiation."""
    if not c.first:
        return [], {}, []
    s = strip_refs(c.first)[:-1]
    subs, prose = {}, []
    m = re.search(r"instantiates ((?:[A-Za-z-]+)(?: and [A-Za-z-]+)*) with (.*?)(?=, (?:and )?(?:restricts|strengthens|extends|composes)\b|$)", s)
    if m:
        items = m.group(2).split("; ") if ";" in m.group(2) else re.split(r", | and ", m.group(2))
        for p in m.group(1).split(" and "):
            subs[p] = [it for it in items if " for " in it]
        prose = [it for it in items if " for " not in it]
    parents, operands = [], s[len(c.name):m.start(2)] + s[m.end(2):] if m else s[len(c.name):]
    for mm in re.finditer(r"[A-Za-z][A-Za-z-]*", operands):
        if mm.group(0) in known and mm.group(0) not in parents and mm.group(0) != c.name:
            parents.append(mm.group(0))
    return parents, subs, prose


# ---- substitutions of an instantiation ----------------------------------------------

def article(w):
    m = re.match(r"(an?|the) (.+)$", w)
    return (m.group(1), m.group(2)) if m else ("", w)


def plural(w):
    if w.endswith("child"):
        return w + "ren"
    if re.search(r"[^aeiou]y$", w):
        return w[:-1] + "ies"
    return w + ("es" if re.search(r"(s|sh|ch|x|z)$", w) else "s")


def word_sub(new, old):
    """An instantiation by a word: (text function, predicate function)."""
    (an, nw), (ao, ow) = article(new), article(old)

    def on_text(s):
        if ao and an:
            s = re.sub(r"\b%s %s\b" % (ao, re.escape(ow)), "%s %s" % (an, nw), s)
            s = re.sub(r"\b%s %s\b" % (cap(ao), re.escape(ow)), "%s %s" % (cap(an), nw), s)
        s = re.sub(r"\b%s\b" % re.escape(plural(ow)), plural(nw), s)
        s = re.sub(r"\b%s\b" % re.escape(ow), nw, s)
        return s

    def on_pred(p):
        ident = lambda x: x.replace(" ", "_").lower()
        p = re.sub(r"(?<![a-z])%s(?![a-z])" % re.escape(ident(plural(ow))), ident(plural(nw)), p)
        return re.sub(r"(?<![a-z])%s(?![a-z])" % re.escape(ident(ow)), ident(nw), p)
    return on_text, on_pred


def same_phrase(t, b):
    """b is the template t up to the names of its places: the map from b's places to t's, or None."""
    tk, bk, mp = toks(t), toks(b), {}
    if len(tk) != len(bk):
        return None
    for x, y in zip(tk, bk):
        mx, my = PH.match(x.rstrip(",")), PH.match(y.rstrip(","))
        if mx and my and (mx.group(2) or "") == (my.group(2) or "") and mp.setdefault(my.group(1), mx.group(1)) == mx.group(1):
            continue
        if x != y:
            return None
    return mp


def rename(text, mp):
    return " ".join((mp.get(m.group(1), m.group(1)) + (m.group(2) or "") + tok[len(tok.rstrip(",")):]) if (m := PH.match(tok.rstrip(","))) else tok
                    for tok in toks(text))


def split_for(item, plex):
    """'A for B' with B a state or a role of the contract instantiated: (kind, A in B's places, template, predicate, places); None for a word."""
    for m in re.finditer(r" for ", item):
        a, b = item[:m.start()], item[m.end():]
        for kind, entries in (("state", plex.basic), ("role", plex.roles)):
            for t, p, args in entries:
                mp = same_phrase(t, b)
                if mp is not None:
                    return kind, rename(a, mp), t, p, args
    return None


def extras(new):
    """The names an instantiation by a state adds: those of the state put, which the state replaced has no place for."""
    return re.findall(r"(?<![A-Za-z])%s(?![A-Za-z])" % NAME, new)


def say(neg, phrase, effect):
    if re.search(r"(?<![A-Za-z_])_(?![A-Za-z_])", phrase):
        return unwild(phrase, "no one" if neg else "someone")
    return (("no longer " if effect else "not ") if neg else "") + phrase


def subst_states(text, lex, tb, new, effect, put=()):
    """A definition of the contract instantiated, with the state put for the basic state tb.  A name the state put
    adds is anyone where the state must not hold, and where it ends for someone unnamed."""
    if not text:
        return text
    vs = sorted(set(re.findall(r"(?<![A-Za-z'-])([b-z])(?='s|-[A-Za-z]|[ ,.]|$)", text)))
    up = {v: "Zz" + v.upper() + "zz" for v in vs}
    filled = " ".join(fill(tok, up) for tok in toks(text))
    out = []
    def put_rx(t):
        r = tpl_rx(t)
        for x in extras(t):
            r = re.sub(r"(?<![A-Za-z])%s(?![A-Za-z])" % x, "(?:%s|%s)" % (x, "|".join(WILDW)), r)
        return re.compile(r + r"(?=$|[ ,.])")
    tpls = [(put_rx(t), ("put", t)) for t in put] + lex.states()
    for n, pay, b, was in parse_list(filled, tpls):
        if pay[0] in ("short", "neg"):
            return text
        if pay[0] == "put":
            n = n or "no one" in was
            ph = re.sub(r"\b(%s)\b" % "|".join(WILDW), WILD, was)
        elif pay[0] == "basic" and pay[1] == tb:
            ph = fill(new, b)
            if n and (not effect or WILD in b.values()):
                for x in extras(new):
                    ph = re.sub(r"(?<![A-Za-z])%s(?![A-Za-z])" % x, WILD, ph)
        else:
            ph = fill(pay[1], b)
        out.append(say(n, ph, effect))
    s2 = join(out)
    for v, u in up.items():
        s2 = s2.replace(u, v)
    return s2


def role_pattern(text, lex):
    vs = tvars(text)
    up = {v: "Zz" + v.upper() + "zz" for v in vs}
    g = persons_of(fill(text, up), lex)[0][1]
    for v, u in up.items():
        g = g.replace(u, v)
    return g


# ---- the compiler: gr -> sas, the flat text, and the lexicon -------------------------

class World:
    def __init__(self, names):
        self.known = set(names)
        self.c, self.lex, self.flat, self.sas, self.sasx = {}, {}, {}, {}, {}

    def contract(self, n):
        if n not in self.c:
            self.c[n] = read_gr(n)
        return self.c[n]

    def lexicon(self, n):
        """The lexicon of n: its own, and those of the contracts of its first line, instantiated."""
        if n in self.lex:
            return self.lex[n]
        c = self.contract(n)
        parents, subs, _ = first_line(c, self.known)
        lex = c.own.copy()
        for p in parents:
            pl = self.lexicon(p).copy()
            for item in subs.get(p, []):
                sf = split_for(item, self.lexicon(p))
                if sf and sf[0] == "state":
                    _, new, tb, _, _ = sf
                    put = getattr(pl, "put", []) + [new]
                    pl.put = put
                    pl.defs = [(t, subst_states(r, pl, tb, new, act, put), act) for t, r, act in pl.defs]
                    pl.req = {t: subst_states(v, pl, tb, new, False, put) for t, v in pl.req.items()}
                    pl.basic = [e for e in pl.basic if e[0] != tb]
                if sf is None:
                    new, old = item.split(" for ", 1)
                    ft, fp = word_sub(new, old)
                    pl.basic = [(ft(t), fp(q), a) for t, q, a in pl.basic]
                    pl.roles = [(ft(t), fp(q), a) for t, q, a in pl.roles]
                    pl.defs = [(ft(t), ft(r), act) for t, r, act in pl.defs]
                    pl.req = {ft(t): ft(v) for t, v in pl.req.items()}
                    pl.short = {ft(t): (ft(v[0]), v[1]) for t, v in pl.short.items()}
                    pl.rlist = {fp(k): ft(v) for k, v in pl.rlist.items()}
            lex.add(pl)
        lex.add(arith())
        self.lex[n] = lex
        self.binds(n)          # refuses two different states put for one state
        del self.lex[n]
        for p in parents:      # a parameter that one contract of the first line binds is bound in all of n
            for item, q in self.binds(p):
                self.bind_lex(lex, item, q)
        self.lex[n] = lex
        return lex

    def bind_lex(self, lex, item, q):
        sf = split_for(item, self.lexicon(q))
        if sf and sf[0] == "state" and any(e[0] == sf[2] for e in lex.basic):
            _, new, tb, _, _ = sf
            put = getattr(lex, "put", []) + [new]
            lex.put = put
            lex.defs = [(t, subst_states(r, lex, tb, new, act, put), act) for t, r, act in lex.defs]
            lex.req = {t: subst_states(v, lex, tb, new, False, put) for t, v in lex.req.items()}
            lex.basic = [e for e in lex.basic if e[0] != tb]

    def binds(self, n):
        """The states that n and the contracts it is written from put for states: [(item, the contract instantiated)]."""
        c = self.contract(n)
        parents, subs, _ = first_line(c, self.known)
        out = []
        for p in parents:
            for item in subs.get(p, []):
                sf = split_for(item, self.lexicon(p))
                if sf and sf[0] == "state":
                    out.append((item, p))
            out += [b for b in self.binds(p) if b not in out]
        seen = {}
        for item, q in out:
            sf = split_for(item, self.lexicon(q))
            if seen.setdefault((q, sf[2]), sf[1]) != sf[1]:
                raise Err("%s: two contracts it is written from put different states for '%s' of %s" % (n, sf[2], q))
        return out


def sentences(text):
    return re.split(r"(?<=\.) (?=[A-Z])", text)


def read_act(body, lex):
    """An act sentence without its name: persons, the act phrase and its definition, consents, conditions."""
    body = body[:-1] if body.endswith(".") else body
    conds = ""
    if ", provided " in body:
        body, conds = body.rsplit(", provided ", 1)
    must = ""
    mc = re.search(r", and (.+?) must consent$", body)
    if mc:
        body, must = body[:mc.start()], mc.group(1)
    consent = ""
    if ", with the consent of " in body:
        body, consent = body.rsplit(", with the consent of ", 1)
    m = re.match(r"(.*?) may (jointly )?(.*)$", body)
    persons = persons_of(m.group(1), lex) if m else []
    phrase = m.group(3) if m else body
    for t, r, isact in lex.defs:
        if not isact:
            continue
        if m:
            sv, rest = subject(t)
            if len(sv) != len(persons) or not rest:
                continue
            fixed = {v: p[0] for v, p in zip(sv, persons)}
            rk = toks(rest)
            if len(sv) == 1:
                rk[0] = base(rk[0])
            mm = re.fullmatch(tpl_rx(" ".join(rk), fixed, sv[0] if len(sv) == 1 else None, False), phrase)
            if mm:
                b = dict(fixed)
                b.update(mm.groupdict())
                return persons, t, b, r, consent, conds, must
        else:
            mm = re.fullmatch(tpl_rx(t), phrase)
            if mm:
                return [], t, mm.groupdict(), r, consent, conds, must
    raise Err("no definition of the act phrase: " + phrase)


def actor_of(persons, t, b):
    """The first of those who take an act; of an act nobody decides, the first subject of its act phrase."""
    sv, _ = subject(t)
    who = persons[0] if persons else b.get(sv[0]) if sv else None
    return who if who and re.fullmatch(NAME, who) else None


def act_text(persons, t, b, consent, conds, must=""):
    """The act sentence, from its parts."""
    if persons:
        sv, rest = subject(t)
        rk = toks(rest)
        if len(sv) == 1:
            rk[0] = base(rk[0])
        s = "%s may %s%s" % (cap(join(persons)), "jointly " if len(persons) > 1 else "",
                               fill(" ".join(rk), b, sv[0] if len(sv) == 1 else None, True))
    else:
        s = cap(fill(t, b))
    if consent:
        s += ", with the consent of " + consent
    if must:
        s += ", and %s must consent" % must
    if conds:
        s += ", provided " + conds
    return s + "."


def compile_contract(w, n):
    """gr/n -> the entries of sas/n and the entries of its own flat text."""
    c, lex = w.contract(n), w.lexicon(n)
    sas, own = [], []
    for kind, l in c.header:
        if kind == "prose":
            sas.append(dict(kind="kept", label="%s:0" % n, mark="-", text=l))
            own.append(dict(kind="kept", label="%s:0" % n, mark="-", text=l))
    for num, mark, text in c.clauses:
        label = "%s:%d" % (n, num)
        for s in sentences(text):
            m = re.match(r"([A-Z][A-Za-z' -]*?): (.+)$", s)
            cm = m and re.match(r"(%s)(?: ([A-Z][A-Za-z' -]*?))?((?:, only if .*?)?)((?:, with the consent of .*?)?)((?:, and then .*)?)\.$" % REF, m.group(2))
            try:
                if cm:
                    conds = cm.group(3)[len(", only if "):] if cm.group(3) else ""
                    cons = cm.group(4)[len(", with the consent of "):] if cm.group(4) else ""
                    then = cm.group(5)[len(", and then "):] if cm.group(5) else ""
                    e = dict(kind="cite", label=label, mark=mark, name=m.group(1), target=cm.group(1), tname=cm.group(2),
                             body=lits_of(conds, lex, ACTOR), guards=[g for _, g in persons_of(cons, lex)] if cons else [],
                             then=lits_of(then, lex, ACTOR))
                    theirs = {q for _, q, _ in w.lexicon(cm.group(1).split(":")[0]).basic}
                    if any(q in theirs for _, q, _ in e["then"]):
                        raise Err("a strengthening adds a state of the contract it strengthens")
                    sas.append(e)
                    own.append(dict(e, conds=phrases_of(conds, lex, ACTOR), persons=[p for p, _ in persons_of(cons, lex)] if cons else [],
                                    effects=phrases_of(then, lex, ACTOR)))
                elif m:
                    persons, t, b, r, consent, conds, must = read_act(m.group(2), lex)
                    musts = persons_of(must, lex) if must else []      # who must consent, and is not asked
                    cons = (persons_of(consent, lex) if consent else []) + musts
                    who = actor_of([p for p, _ in persons], t, b)
                    eff, cnd = unfold(fill(r, b), lex, actor=who), unfold(conds, lex, actor=who)
                    if t in lex.req:
                        cnd = cnd + unfold(fill(lex.req[t], b), lex, actor=who)
                    head = [(x[0], x[3], x[4]) for x in eff]
                    if any(WILD in x[4] for x in eff if not x[0]) or any(a[:1] in "<>" for x in eff for a in x[4]):
                        raise Err("an act cannot make a state hold of someone unnamed, or of a number of persons")
                    body = [(x[0], x[3], x[4]) for x in cnd] + [(False, x[3], x[4]) for x in eff if x[0] and WILD not in x[4]]
                    sas.append(dict(kind="act", label=label, mark=mark, name=m.group(1), head=head, body=body,
                                    guards=list(dict.fromkeys(g for _, g in persons + cons))))
                    same = same_of(c, num, names_of(sas[-1]))
                    sas[-1]["same"] = same
                    sas[-1]["must"] = [g for _, g in musts]
                    own.append(dict(kind="act", label=label, mark=mark, name=m.group(1), same=same, must=[p for p, _ in musts], persons=list(dict.fromkeys(p for p, _ in persons + cons)),
                                    effects=[(x[0], wild(fill(x[1], x[2]), x[0])) for x in eff], conds=[(x[0], wild(fill(x[1], x[2]), x[0])) for x in cnd]))
                else:
                    sas.append(dict(kind="kept", label=label, mark=mark, text=s))
                    own.append(dict(kind="kept", label=label, mark=mark, text=s))
            except Err as ex:
                raise Err("%s, %s: %s" % (label, s[:40], ex))
    return sas, own


# ---- printing and reading schemas ----------------------------------------------------

def schema_line(e):
    pre = "%s %s " % (e["label"], e["mark"])
    if e["kind"] == "kept":
        return pre + "| " + e["text"]
    head = ", ".join(lit_s(l) for l in e["head"]) if e["kind"] == "act" else e["target"] + (" " + e["tname"] if e.get("tname") else "") + \
        (" + " + ", ".join(lit_s(l) for l in e["then"]) if e.get("then") else "")
    s = "%s%s: %s <-" % (pre, e["name"], head)
    if e["body"]:
        s += " " + ", ".join(lit_s(l) for l in e["body"])
    if e["guards"]:
        s += " guarded by " + ", ".join(e["guards"])
    if e.get("must"):
        s += " must " + ", ".join(e["must"])
    if e.get("same"):
        s += " possibly " + ", ".join(e["same"])
    return s + "."


def read_schema_line(l):
    m = ENTRY.match(l)
    label, mark, rest = m.groups()
    if rest.startswith("| "):
        return dict(kind="kept", label=label, mark=mark, text=rest[2:])
    name, rest = rest.split(": ", 1)
    head, rest = rest[:-1].split(" <-", 1)
    same = []
    if " possibly " in " " + rest:
        rest, sm = (" " + rest).rsplit(" possibly ", 1)
        same = sm.split(", ")
    must = []
    if " must " in " " + rest:
        rest, mu = (" " + rest).rsplit(" must ", 1)
        must = mu.split(", ")
    guards = []
    if " guarded by " in " " + rest:
        rest, g = (" " + rest).rsplit(" guarded by ", 1)
        guards = split_top(g)
    body = [parse_atom(a) for a in split_top(rest)]
    cm = re.fullmatch(r"(%s)(?: ([^+]+?))?(?: \+ (.+))?" % REF, head.strip())
    if cm:
        return dict(kind="cite", label=label, mark=mark, name=name, target=cm.group(1), tname=cm.group(2), body=body, guards=guards,
                    then=[parse_atom(a) for a in split_top(cm.group(3))] if cm.group(3) else [])
    return dict(kind="act", label=label, mark=mark, name=name, head=[parse_atom(a) for a in split_top(head)], body=body, guards=guards, same=same, must=must)


def sas_first(w, n):
    """The first line of sas/n: that of gr/n, its states and roles as predicates."""
    c = w.contract(n)
    if not c.first:
        return None
    parents, subs, _ = first_line(c, w.known)
    s = c.first
    for p in parents:
        for item in subs.get(p, []):
            sf = split_for(item, w.lexicon(p))
            if sf:
                kind, a, t, pred, args = sf
                old = "%s(%s)" % (pred, ",".join(args))
                new = ", ".join(lit_s(l) for l in pattern(a, w.lexicon(n))) if kind == "state" else role_pattern(a, w.lexicon(n))
                s = s.replace(item, "%s for %s" % (new, old))
    return s


def write_sas(w, n, sas):
    c = w.contract(n)
    out = [c.title] + ([sas_first(w, n)] if c.first else []) + [l for k, l in c.header if k == "lex"] + [schema_line(e) for e in sas]
    put("sas", n, out)


def put(d, n, lines):
    os.makedirs(os.path.join(ROOT, d), exist_ok=True)
    with open(os.path.join(ROOT, d, n + ".txt"), "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")


def get(d, n):
    return [l.rstrip("\n") for l in open(os.path.join(ROOT, d, n + ".txt"), encoding="utf-8") if l.strip()]


# ---- the kinds of names; a citation in the names of its own contract -----------------

def kinds_line(c):
    """The kinds the names line of a contract declares: {name: {kind}}."""
    out = {}
    for kind, l in c.header:
        if kind != "prose" or "*" in l or not re.match(r"%s(?:, %s)*(?: and %s)? (?:is|are) " % (NAME, NAME, NAME), l):
            continue
        seg = re.split(r"; |\. ", l)[0].rstrip(".")
        for m in re.finditer(r"((?:%s, )*%s(?: and %s)?) (?:(?:is|are) )?((?:an? )?[a-z][A-Za-z' -]*?)(?=, | and [A-Z]|$)" % (NAME, NAME, NAME), seg):
            names, k = re.findall(NAME, m.group(1)), re.sub(r"^an? ", "", m.group(2))
            if k.startswith("possibly"):
                continue
            if len(names) > 1:
                k = "child" if k == "children" else k[:-3] + "y" if k.endswith("ies") else k[:-1] if k.endswith("s") else k
            for x in names:
                out.setdefault(x, set()).add(k)
    return out


def kinds_from(w, n, origin=None, memo=None):
    """The kinds of the names of the contract origin as n has them, with the words n's first lines put for words;
    with no origin, of the names of n and of every contract it is written from."""
    c = w.contract(n)
    out = {k: set(v) for k, v in kinds_line(c).items()} if origin in (None, n) else {}
    if origin == n:
        return out
    parents, subs, _ = first_line(c, w.known)
    for p in parents:
        if origin is not None and origin not in order([p], w):
            continue
        for x, ks in kinds_from(w, p, origin).items():
            for k in ks:
                for item in subs.get(p, []):
                    if split_for(item, w.lexicon(p)) is None:
                        new, old = item.split(" for ", 1)
                        k = word_sub(new, old)[0](k)
                out.setdefault(x, set()).add(k)
    return out


def possibly(c):
    """What the names line says of names that are possibly one person: [(clauses or None, name, name or '*')]."""
    nm = NAME + r"(?! (?:is |are )?possibly| an? | (?:is|are) )"
    item = r"(?:%s|one person|any of the others)" % nm
    rx = re.compile(r"(?:in clauses? (\d+(?:(?:, | and )\d+)*) )?(%s(?:(?:, | and | or )%s)*) (?:is |are )?possibly (%s(?:(?:, | or )%s)*)" % (NAME, NAME, item, item))
    out = []
    for kind, l in c.header:
        if kind != "prose" or "*" in l:
            continue
        for m in rx.finditer(l):
            cl = [int(x) for x in re.findall(r"\d+", m.group(1))] if m.group(1) else None
            lhs = re.findall(NAME, m.group(2))
            for r in re.split(r", | or ", m.group(3)):
                if r == "one person":
                    out += [(cl, a, b) for i, a in enumerate(lhs) for b in lhs[i + 1:]]
                else:
                    out += [(cl, a, "*" if r == "any of the others" else r) for a in lhs]
    return out


def same_of(c, num, names):
    """The pairs of names of an act of clause num that are possibly one person: ['Alice=Carol', ...]."""
    out = set()
    for cl, a, b in possibly(c):
        if cl is None or num in cl:
            for x in ({x for x in names - {a} if "person" in kinds_line(c).get(x, ())} if b == "*" else [b]):
                if a in names and x in names and a != x:
                    out.add("=".join(sorted((a, x))))
    return sorted(out)


def names_of(e, text=False):
    """The names of an entry: of its literals and guards, or of its phrases and persons."""
    if text:
        s = " ".join([p for _, p in e.get("effects", [])] + [p for _, p in e["conds"]] + e["persons"])
        return set(re.findall(r"(?<![A-Za-z])%s" % NAME, s))
    out = set()
    for l in e.get("head", []) + e["body"] + e.get("then", []):
        out.update(l[2])
    for g in e["guards"]:
        out.update(parse_atom(g)[2] if "(" in g else [g])
    return {x.lstrip(MARK) for x in out if x != WILD and x[:1] not in "<>" and not x.isdigit()}


def cite_map(w, n, cite, hit, text=False):
    """A citation is written in the names of its own contract: a name of it that the act cited does not have
    stands for the one name of its kind of that act which the citation does not write; else it is one more name.
    The holder that a condition does not name is the first of those who take the act."""
    cn, hn = names_of(cite, text) - {ACTOR}, names_of(hit, text)
    own, theirs = kinds_from(w, n), kinds_from(w, n, hit["label"].split("+")[0].split(":")[0])
    mp = {}
    for f in sorted(cn - hn):
        kf = own.get(f, set())
        if not kf or "person" in kf:
            continue
        cands = [x for x in sorted(hn - cn) if theirs.get(x, set()) & kf and "person" not in theirs.get(x, set())]
        if len(cands) == 1:
            mp[f] = cands[0]
    first = (hit["persons"] if text else hit["guards"])[:1]
    if first and not re.fullmatch(NAME, first[0]) and re.search(r"(?<![A-Za-z])%s" % NAME, first[0][1:]):
        first = [re.search(r"(?<![A-Za-z])%s" % NAME, first[0][1:]).group(0)]      # those who decide for Orga: Orga holds it
    if first and re.fullmatch(NAME, first[0]):
        mp[ACTOR] = first[0]
    elif ACTOR in names_of(cite, text):
        raise Err("%s: nobody takes %s, so nobody holds a state of the citation" % (cite["label"], hit["name"]))
    return mp


def map_text(s, mp):
    return re.sub(r"(?<![A-Za-z])(%s)(?![A-Za-z])" % "|".join(map(re.escape, mp)), lambda m: mp[m.group(1)], s) if mp else s


def map_guard(g, mp):
    if "(" not in g:
        return mp.get(g, g)
    _, p, a = parse_atom(g)
    return "%s(%s)" % (p, ",".join(mp.get(x, x) for x in a))


# ---- the operations, on the schemas --------------------------------------------------

def read_sas(w, n):
    """sas/n: title, first line with its substitutions as predicates, entries."""
    lines = get("sas", n)
    first = lines[1] if len(lines) > 1 and re.match(re.escape(n) + r" (extends|restricts|strengthens|instantiates|composes)\b", lines[1]) else None
    c = Contract()
    c.name, c.first = n, first
    parents, subs, _ = first_line(c, w.known)
    return lines[0], first, parents, subs, [read_schema_line(l) for l in lines if ENTRY.match(l)]


def sub_atoms(lits, rules, head=False):
    out = []
    for l in lits:
        for (pred, args), new in rules:
            copies = l[2][-1:] if l[2] and l[2][-1][:1] in MARK else ()
            if l[1] == pred and len(args) == len(l[2]) - len(copies):
                b = dict(zip(args, l[2]))
                more_any = l[0] and (not head or WILD in l[2])   # a name the state put adds: anyone, where it must not hold
                for nl in new:
                    if l[0] and nl[0]:
                        raise Err("a negated atom instantiated by a negated atom")
                    sub = lambda a: "@".join(b[x] if x in b else x for x in a.split("@")) if "@" in a else b[a] if a in b else WILD if more_any else a
                    out.append((l[0] or nl[0], nl[1], tuple(sub(a) for a in nl[2]) + copies))
                break
        else:
            out.append(l)
    return out


def instantiate_schemas(entries, items):
    """Each item 'atoms for atom', 'role for role' or 'a word for a word'."""
    for item in items:
        new, old = item.rsplit(" for ", 1)
        if "(" in old:
            _, pred, args = parse_atom(old)
            newl = [parse_atom(a) for a in split_top(new)]
            out = []
            for e in entries:
                e = dict(e)
                if e["kind"] == "act":
                    e["head"], e["body"] = sub_atoms(e["head"], [((pred, args), newl)], True), sub_atoms(e["body"], [((pred, args), newl)])
                    for x in {a for nl in newl for a in nl[2] if a not in args}:   # a name the act only requires: someone
                        if not any(x in l[2] for l in e["head"]):
                            e["body"] = [(n, q, tuple(WILD if a == x else a for a in aa)) for n, q, aa in e["body"]]
                    gs = []
                    for g in e["guards"]:
                        if "(" in g and parse_atom(g)[1] == pred:
                            b = dict(zip(args, parse_atom(g)[2]))
                            g = "%s(%s)" % (newl[0][1], ",".join(b.get(a, a) for a in newl[0][2]))
                        gs.append(g)
                    e["guards"] = gs
                out.append(e)
            entries = out
        else:
            ft, fp = word_sub(new, old)
            out = []
            for e in entries:
                e = dict(e)
                if e["kind"] == "kept":
                    e["text"] = ft(e["text"])
                else:
                    e["head"] = [(x, fp(p), a) for x, p, a in e["head"]]
                    e["body"] = [(x, fp(p), a) for x, p, a in e["body"]]
                    e["guards"] = [fp(g) if "(" in g else g for g in e["guards"]]
                out.append(e)
            entries = out
    return entries


def key(e):
    return (e["label"].split("+")[0], e.get("oname", e.get("name")), e.get("text"))


def more(a, b):
    """The list a with the items of b that it has fewer times."""
    out = list(a)
    for x in b:
        if out.count(x) < b.count(x):
            out.append(x)
    return out


def both(a, b, fields):
    """An act that a contract has by two paths: it has the restrictions of both."""
    o = dict(a)
    for f in fields:
        o[f] = more(a[f], b[f])
    if "body" in fields:        # of schemas: the conditions of both, and what the head of both deletes; never a condition
        o["body"] = more(conditions(a), conditions(b)) + [(False, q, args) for x, q, args in o["head"] if x and WILD not in args]      # merged with a deletion
    o["same"] = sorted(set(a.get("same", [])) | set(b.get("same", [])))
    o["must"] = more(a.get("must", []), b.get("must", []))
    o["label"] = "+".join(more(a["label"].split("+"), b["label"].split("+")))
    o["names"] = dict(a.get("names", {}), **b.get("names", {}))
    if "oname" in b and "oname" not in a:
        o["oname"], o["name"], o["mark"] = b["oname"], b["name"], b["mark"]
    return o


def inherit(entries, have, new, fields):
    for e in new:
        k = key(e)
        if k not in have:
            have[k] = len(entries)
            entries.append(e)
        elif e["kind"] == "act":
            entries[have[k]] = both(entries[have[k]], e, fields)


def cited(entries, target, tname, where):
    def named(e):       # the name the act has at the clause cited
        at = e.get("names", {}).get(target)
        return at == tname if at is not None else e["name"] == tname or e.get("oname") == tname
    hit = [i for i, e in enumerate(entries) if e["kind"] == "act" and target in e["label"].split("+") and (tname is None or named(e))]
    if not hit:
        raise Err("%s: no act of %s%s" % (where, target, " named " + tname if tname else ""))
    return hit


def binds_sas(w, n, memo={}):
    """The atoms that n and the contracts it is written from put for atoms, read from the first lines of sas/."""
    if n not in memo:
        _, first, parents, subs, _ = read_sas(w, n)
        out = []
        for p in parents:
            out += [i for i in subs.get(p, []) if "(" in i.rsplit(" for ", 1)[1] and re.match(r"[a-z_0-9]+\(", i) and i not in out]
            out += [i for i in binds_sas(w, p) if i not in out]
        memo[n] = out
    return memo[n]


def expand_schemas(w, n):
    if n in w.sasx:
        return w.sasx[n]
    _, first, parents, subs, own = read_sas(w, n)
    entries, have = [], {}
    for p in parents:
        inherit(entries, have, instantiate_schemas(expand_schemas(w, p), subs.get(p, [])), ("head", "body", "guards"))
    for p in parents:
        entries = instantiate_schemas(entries, binds_sas(w, p))
    for e in own:
        if e["kind"] == "cite":
            hit = cited(entries, e["target"], e["tname"], e["label"])
            for i in hit:
                o = dict(entries[i])
                mp = cite_map(w, n, e, o)
                then = [(x, p, tuple(mp.get(a, a) for a in args)) for x, p, args in e.get("then", [])]
                o["head"] = o["head"] + then
                o["body"] = o["body"] + [(x, p, tuple(mp.get(a, a) for a in args)) for x, p, args in e["body"]] + \
                    [(False, p, args) for x, p, args in then if x and WILD not in args]
                o["guards"] = o["guards"] + [g for g in (map_guard(g, mp) for g in e["guards"]) if g not in o["guards"]]
                o["label"], o["mark"] = o["label"] + "+" + e["label"], e["mark"]
                if len(hit) == 1:
                    o["oname"], o["name"] = o["name"], e["name"]
                o["names"] = dict(o.get("names", {}), **{e["label"]: o["name"]})
                entries[i] = o
        else:
            entries.append(dict(e, names={e["label"]: e["name"]}) if e["kind"] == "act" else e)
    w.sasx[n] = entries
    return entries


# ---- the operations, on the text ----------------------------------------------------

def flat_line(e):
    pre = "%s %s " % (e["label"], e["mark"])
    if e["kind"] == "kept":
        return pre + "| " + e["text"]
    eff = join(("no longer " if n else "") + p for n, p in e["effects"])
    s = cap(join(e["persons"])) + (" may jointly " if len(e["persons"]) > 1 else " may ") + eff if e["persons"] else cap(eff)
    if e["conds"]:
        s += ", provided " + join(("not " if n else "") + p for n, p in e["conds"])
    return "%s%s: %s.%s%s" % (pre, e["name"], s, " Must consent: %s." % ", ".join(e["must"]) if e.get("must") else "",
                                " Possibly one: %s." % ", ".join(e["same"]) if e.get("same") else "")


def sub_phrases(items, rules, lex, head=False):
    """Each basic state of the contract instantiated, replaced by the states put for it."""
    out = []
    for n, p in items:
        for rx, new in rules:
            m = rx.fullmatch(p)
            if m:
                ph = fill(new, m.groupdict())
                if n and (not head or any(v in WILDW for v in m.groupdict().values())):
                    for x in extras(new):
                        ph = re.sub(r"(?<![A-Za-z])%s(?![A-Za-z])" % x, "anyone", ph)
                for nn, np in phrases_of(ph, lex):
                    if n and nn:
                        raise Err("a negated state instantiated by a negated state")
                    out.append((n or nn, np.replace("someone", "anyone") if n else np))
                break
        else:
            out.append((n, p))
    return out


def instantiate_text(w, entries, items, p, n):
    plex, lex = w.lexicon(p), w.lexicon(n)
    for item in items:
        sf = split_for(item, plex)
        out = []
        if sf:
            kind, new, t, pred, args = sf
            rx = re.compile(tpl_rx(t))
            for e in entries:
                e = dict(e)
                if e["kind"] == "act":
                    if kind == "state":
                        e["effects"], e["conds"] = sub_phrases(e["effects"], [(rx, new)], lex, True), sub_phrases(e["conds"], [(rx, new)], lex)
                        for x in extras(new):
                            xr = r"(?<![A-Za-z])%s(?![A-Za-z])" % x
                            if not any(re.search(xr, ph) for _, ph in e["effects"]):
                                e["conds"] = [(c, re.sub(xr, "anyone" if c else "someone", ph)) for c, ph in e["conds"]]
                    else:
                        e["persons"] = [low(fill(new, rx.fullmatch(x).groupdict())) if rx.fullmatch(x) else x for x in e["persons"]]
                out.append(e)
        else:
            new, old = item.split(" for ", 1)
            ft, _ = word_sub(new, old)
            for e in entries:
                e = dict(e)
                if e["kind"] == "kept":
                    e["text"] = ft(e["text"])
                else:
                    e["effects"] = [(x, ft(s)) for x, s in e["effects"]]
                    e["conds"] = [(x, ft(s)) for x, s in e["conds"]]
                    e["persons"] = [ft(s) for s in e["persons"]]
                out.append(e)
        entries = out
    return entries


def expand_text(w, n, own):
    c = w.contract(n)
    parents, subs, _ = first_line(c, w.known)
    entries, have = [], {}
    for p in parents:
        inherit(entries, have, instantiate_text(w, w.flat[p], subs.get(p, []), p, n), ("effects", "conds", "persons"))
    for p in parents:
        for item, q in w.binds(p):
            entries = instantiate_text(w, entries, [item], q, n)
    for e in own:
        if e["kind"] == "cite":
            hit = cited(entries, e["target"], e["tname"], e["label"])
            for i in hit:
                o = dict(entries[i])
                mp = cite_map(w, n, e, o, True)
                o["effects"] = o["effects"] + [(x, map_text(s, mp)) for x, s in e.get("effects", [])]
                o["conds"] = o["conds"] + [(x, map_text(s, mp)) for x, s in e["conds"]]
                o["persons"] = o["persons"] + [g for g in (map_text(g, mp) for g in e["persons"]) if g not in o["persons"]]
                o["label"], o["mark"] = o["label"] + "+" + e["label"], e["mark"]
                if len(hit) == 1:
                    o["oname"], o["name"] = o["name"], e["name"]
                o["names"] = dict(o.get("names", {}), **{e["label"]: o["name"]})
                entries[i] = o
        else:
            entries.append(dict(e, names={e["label"]: e["name"]}) if e["kind"] == "act" else e)
    w.flat[n] = entries
    return entries


# ---- the two maps between a flat text and schemas ---------------------------------------

def phi(lines, lex):
    """The schemas of the lines of a flat text."""
    out = []
    for l in lines:
        m = ENTRY.match(l)
        if not m:
            continue
        label, mark, rest = m.groups()
        if rest.startswith("| "):
            out.append(dict(kind="kept", label=label, mark=mark, text=rest[2:]))
            continue
        same = []
        if " Possibly one: " in rest:
            rest, sm = rest.rsplit(" Possibly one: ", 1)
            same = sm[:-1].split(", ")
        must = []
        if " Must consent: " in rest:
            rest, mu = rest.rsplit(" Must consent: ", 1)
            must = mu[:-1].split(", ")
        name, body = rest[:-1].split(": ", 1)
        conds = ""
        if ", provided " in body:
            body, conds = body.rsplit(", provided ", 1)
        mm = re.match(r"(.*?) may (?:jointly )?(.*)$", body)
        persons = persons_of(mm.group(1), lex) if mm else []
        eff, cnd = lits_of(mm.group(2) if mm else low(body), lex), lits_of(conds, lex)
        out.append(dict(kind="act", label=label, mark=mark, name=name, head=eff, same=same, must=must,
                        body=cnd + [(False, p, a) for x, p, a in eff if x and WILD not in a], guards=[g for _, g in persons]))
    return out


def inv_basic(lex):
    """Predicates to phrases; a predicate with a number of copies has its own phrase, under 'pred*'."""
    return {p + (a[-1][0] if a and a[-1][:1] in MARK else ""): (t, a) for t, p, a in lex.basic}, {p: (t, a) for t, p, a in lex.roles}


def tb_of(l, ib):
    """The phrase of a literal and its binding."""
    t, args = ib[l[1] + (l[2][-1][0] if l[2] and l[2][-1][:1] in MARK else "")]
    return t, {a.lstrip(MARK): v.lstrip(MARK) if a[:1] in MARK else v for a, v in zip(args, l[2])}


def phrase_of(l, ib):
    t, b = tb_of(l, ib)
    return wild(fill(t, b), l[0])


def guard_text(g, ir):
    if "(" not in g:
        return g
    _, p, a = parse_atom(g)
    return fill(ir[p][0], dict(zip(ir[p][1], a)))


def conditions(e):
    """The body of a schema without the atoms it deletes."""
    body = list(e["body"])
    for l in e["head"]:
        if l[0]:
            for i in range(len(body) - 1, -1, -1):
                if body[i] == (False, l[1], l[2]):
                    del body[i]
                    break
    return body


def psi(entries, lex):
    """The flat text of schemas."""
    ib, ir = inv_basic(lex)
    out = []
    for e in entries:
        if e["kind"] == "kept":
            out.append(flat_line(e))
        else:
            out.append(flat_line(dict(e, persons=[guard_text(g, ir) for g in e["guards"]],
                                      effects=[(l[0], phrase_of(l, ib)) for l in e["head"]],
                                      conds=[(l[0], phrase_of(l, ib)) for l in conditions(e)])))
    return out


# ---- the decompiler: sas -> a contract ------------------------------------------------

def match_lits(pats, lits, b=None):
    """All bindings under which the literals pats, with places, are the literals lits, in any order."""
    b = b or {}
    if not pats:
        return [b] if not lits else []
    out, p = [], pats[0]
    for i, l in enumerate(lits):
        if l[0] != p[0] or l[1] != p[1] or len(l[2]) != len(p[2]):
            continue
        nb, ok = dict(b), True
        for a, v in zip(p[2], l[2]):
            if a[:1] in MARK and v[:1] == a[:1]:
                a, v = a[1:], v[1:]
            if "@" in a:                # t@x: a thing with its signatures, the last signer x, the rest t
                pa, va = a.split("@"), v.split("@")
                if len(va) < len(pa):
                    ok = False
                    continue
                va = ["@".join(va[:len(va) - len(pa) + 1])] + va[len(va) - len(pa) + 1:]
                for a1, v1 in zip(pa, va):
                    if PH.fullmatch(a1):
                        if nb.setdefault(a1, v1) != v1:
                            ok = False
                    elif a1 != v1:
                        ok = False
            elif PH.fullmatch(a):
                if nb.setdefault(a, v) != v:
                    ok = False
            elif a != v:
                ok = False
        if ok:
            out.extend(match_lits(pats[1:], lits[:i] + lits[i + 1:], nb))
    return out


def pattern(text, lex):
    """A phrase with places as literals with places."""
    vs = sorted(set(re.findall(r"(?<![A-Za-z'-])([b-z])(?='s|-[A-Za-z]|[ ,.]|$)", text)))
    up = {v: "Zz" + v.upper() + "zz" for v in vs}
    filled = " ".join(fill(tok, up) for tok in toks(text))
    back = {v: k for k, v in up.items()}
    bk = lambda a: "@".join(back.get(x, x) for x in a.split("@"))
    return [(n, p, tuple(a[0] + bk(a[1:]) if a[:1] in MARK else bk(a) for a in args)) for n, p, args in lits_of(filled, lex)]


def fold_states(lits, lex, actor=None):
    """Literals as a list of states, defined phrases in place of their definitions where they fit; a state whose
    holder is the first of those who act is written without it, where its vocabulary says so."""
    ib, _ = inv_basic(lex)

    def basic(l):
        t, b = tb_of(l, ib)
        if actor and t in lex.short and b.get(lex.short[t][1]) == actor:
            t = lex.short[t][0]
        if WILD in l[2]:
            return nouns_out(unwild(fill(t, b), "no one" if l[0] else "someone"))
        return not_text(t, b) if l[0] else wild(fill(t, b), False)
    items = [[l, None] for l in lits]
    defs = []
    for t, r, isact in lex.defs:
        if isact:
            continue
        pat = pattern(r, lex)
        if pat and not any(x[0] for x in pat):
            defs.append((t, pat))
    defs.sort(key=lambda d: -len(d[1]))
    for neg in (False, True):
        for t, pat in defs:
            while True:
                pool = [it[0] for it in items if it[1] is None and it[0][0] == neg]
                found = None
                for sub in subsets(pool, len(pat)):
                    bs = match_lits([(neg, p, a) for _, p, a in pat], list(sub))
                    if bs and len(set(bs[0].values())) == len(bs[0]) and WILD not in bs[0].values():   # two wildcards are not one place
                        found = (sub, bs[0])
                        break
                if not found:
                    break
                sub, b = found
                firsti = min(i for i, it in enumerate(items) if it[1] is None and it[0] in sub)
                todo = list(sub)
                keep = []
                for i, it in enumerate(items):
                    if i == firsti:
                        keep.append([None, not_text(t, b) if neg else fill(t, b)])
                        todo.remove(it[0])
                    elif it[1] is None and it[0] in todo:
                        todo.remove(it[0])
                    else:
                        keep.append(it)
                items = keep
    return join(it[1] if it[1] is not None else basic(it[0]) for it in items)


def effects_text(lits, lex):
    """Effects, as basic states: a state, 'no longer' a state, or a state of no one."""
    ib, _ = inv_basic(lex)
    out = []
    for l in lits:
        t, b = tb_of(l, ib)
        if WILD in l[2]:
            out.append(nouns_out(none_text(fill(t, b)) if l[0] else unwild(fill(t, b), "someone")))
        else:
            out.append(("no longer " if l[0] else "") + fill(t, b))
    return join(out)


def match_some(pats, lits, b=None):
    """As match_lits, the literals pats being some of lits: [(binding, the literals left)]."""
    b = b or {}
    if not pats:
        return [(b, lits)]
    out, p = [], pats[0]
    for i, l in enumerate(lits):
        for nb in match_lits([p], [l], b):
            out.extend(match_some(pats[1:], lits[:i] + lits[i + 1:], nb))
    return out


def subsets(pool, k):
    from itertools import combinations
    return combinations(pool, k) if k <= len(pool) else []


def subject_bindings(sv, b, gts):
    """The bindings of the subject places of an act phrase: each a guard; with no guards, as the effects bind them."""
    if not gts:
        return [dict(b)]
    from itertools import permutations
    free = [v for v in sv if v not in b]
    if any(b[v] not in gts for v in sv if v in b):
        return []
    rest = [g for g in gts if g not in [b[v] for v in sv if v in b]]
    out = []
    for perm in permutations(rest, len(free)):
        nb = dict(b)
        nb.update(zip(free, perm))
        out.append(nb)
    return out


def act_phrases(lex):
    return [(t, pattern(r, lex)) for t, r, isact in lex.defs if isact]


def act_sentence(e, lex, acts, ir):
    """The sentence of an act schema: the act phrase whose effects are its head, its conditions folded."""
    best = None
    gts = [guard_text(g, ir) for g in e["guards"]]
    for k, (t, pat) in enumerate(acts):
        sv, rest = subject(t)
        for b, rest_h in match_some(pat, e["head"]):
            for full in subject_bindings(sv, b, gts):
                if any(v not in full for v in tvars(t)) or (gts and not rest):
                    continue
                conds = conditions(e)
                if t in lex.req:
                    try:
                        for l in lits_of(fill(lex.req[t], full), lex):
                            conds.remove(l)
                    except (ValueError, Err):
                        continue
                for l in rest_h:         # an effect a strengthening adds, and the atom it requires if it deletes one
                    if l[0] and WILD not in l[2] and (False, l[1], l[2]) in conds:
                        conds.remove((False, l[1], l[2]))
                score = (len(rest_h), len([v for v in sv if v not in b]), k)
                if best is None or score < best[0]:
                    best = (score, t, full, [full[v] for v in sv] if gts else [], conds, rest_h)
    if not best:
        return None
    _, t, full, persons, conds, rest_h = best
    mu = [guard_text(g, ir) for g in e.get("must", [])]
    s = "%s: %s" % (e["name"], act_text(persons, t, full, join(g for g in gts if g not in persons and g not in mu),
                                        fold_states(conds, lex, actor_of(persons, t, full)), join(mu)))
    return s[:-1] + ", and then " + effects_text(rest_h, lex) + "." if rest_h else s


def expanded(w, n):
    """The expanded contract of n, in the grammar: the names lines and each clause of its expanded schemas, labelled
    by its origin, its acts written with their act phrases.  The phrases are defined in the contracts of origin."""
    lex = w.lexicon(n)
    ib, ir = inv_basic(lex)
    acts = act_phrases(lex)
    head = []
    prose, clauses, of = [], [], {}
    for e in w.sasx[n]:
        if e["label"].endswith(":0"):
            c0 = e["label"][:-2]
            if c0 not in of:
                of[c0] = len(prose)
                prose.append("Of %s: %s" % (c0, e["text"]))
            else:
                prose[of[c0]] += " " + e["text"]
            continue
        txt = e["text"] if e["kind"] == "kept" else act_sentence(e, lex, acts, ir)
        if txt is None:
            raise Err("%s %s: no act phrase has these effects" % (e["label"], e["name"]))
        if clauses and clauses[-1][0] == e["label"].split("+")[0] and e["kind"] == "kept":
            clauses[-1][3].append(txt)
        elif clauses and clauses[-1][1] == e["label"] and clauses[-1][2] == e["mark"]:
            clauses[-1][3].append(txt)
        else:
            clauses.append([e["label"].split("+")[0], e["label"], e["mark"], [txt]])
    return [w.contract(n).title] + head + prose + ["%s. %s %s" % (l.replace("+", ", "), m, " ".join(ts)) for _, l, m, ts in clauses]


def decompile(w, n):
    """sas/n -> the lines of a contract.  Uses the lexicon lines of sas/n and of the contracts it is written from."""
    lines = get("sas", n)
    title, first, parents, subs, entries = read_sas(w, n)
    lex = w.lexicon(n)
    ib, ir = inv_basic(lex)
    acts = act_phrases(lex)
    out = [title]
    if first:
        s = first
        for p in parents:
            pib, pir = inv_basic(w.lexicon(p))
            for item in subs.get(p, []):
                new, old = item.rsplit(" for ", 1)
                if "(" not in old:
                    continue
                _, pred, args = parse_atom(old)
                if pred in pib:
                    s = s.replace(item, "%s for %s" % (fold_states([parse_atom(a) for a in split_top(new)], lex), phrase_of((False, pred, args), pib)))
                else:
                    s = s.replace(item, "%s for %s" % (guard_text(new, ir), guard_text(old, pir)))
        out.append(s)
    head_lines = [l for l in lines[1:] if not ENTRY.match(l) and l != first]
    body, by = [], {}
    for e in entries:
        if e["label"].endswith(":0"):
            head_lines.append(("prose", e["text"]))
            continue
        if e["kind"] == "kept":
            s = e["text"]
        elif e["kind"] == "cite":
            s = "%s: %s%s" % (e["name"], e["target"], " " + e["tname"] if e["tname"] else "")
            if e["body"]:
                s += ", only if " + fold_states(e["body"], lex, ACTOR)
            if e["guards"]:
                s += ", with the consent of " + join(guard_text(g, ir) for g in e["guards"])
            if e.get("then"):
                s += ", and then " + effects_text(e["then"], lex)
            s += "."
        else:
            s = act_sentence(e, lex, acts, ir)
            if s is None:
                raise Err("%s %s: no act phrase has these effects" % (e["label"], e["name"]))
        num = int(e["label"].split(":")[1])
        if num not in by:
            by[num] = [e["mark"], []]
            body.append(num)
        by[num][1].append(s)
    # the contract: its prose and names lines, its clauses; then its Logic pragmas, the lexicon lines
    out += [l[1] for l in head_lines if not isinstance(l, str)]
    for num in body:
        out.append("%d. %s %s" % (num, by[num][0], " ".join(by[num][1])))
    lexl = [l for l in head_lines if isinstance(l, str) and l != PRAGMAS]
    if lexl:
        out += [PRAGMAS] + lexl
    return out


# ---- the checks ---------------------------------------------------------------------

def norm(entries):
    out = []
    for e in entries:
        if e["kind"] == "kept":
            out.append((e["label"], e["mark"], e["text"]))
        elif e["kind"] == "cite":
            out.append((e["label"], e["mark"], e["name"], e["target"], e["tname"], tuple(sorted(e["body"])), tuple(sorted(e["guards"])), tuple(sorted(e.get("then", [])))))
        else:
            out.append((e["label"], e["mark"], e["name"], tuple(sorted(e["head"])), tuple(sorted(e["body"])), tuple(sorted(e["guards"])), tuple(e.get("same") or ()), tuple(sorted(e.get("must") or ()))))
    return sorted(out, key=repr)


def order(names, w):
    done, out = set(), []

    def visit(n):
        if n in done:
            return
        done.add(n)
        for p in first_line(w.contract(n), w.known)[0]:
            visit(p)
        out.append(n)
    for n in names:
        visit(n)
    return out


def main():
    allnames = sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, "gr")) if f.endswith(".txt") and not f.startswith("NOTES"))
    w = World(allnames)
    names = order([a for a in sys.argv[1:] if not a.startswith("-")] or allnames, w)
    bad = 0
    for n in names:
        res = []
        try:
            sas, own = compile_contract(w, n)
            write_sas(w, n, sas)
            flat = expand_text(w, n, own)
            put("flat", n, [w.contract(n).title + " (flat text)"] + [flat_line(e) for e in flat])
            sx = expand_schemas(w, n)
            put("sasx", n, [w.contract(n).title + " (expanded schemas)"] + [schema_line(e) for e in sx])
            put("gx", n, expanded(w, n))
            lex = w.lexicon(n)
            res.append("commute " + ("yes" if norm(phi(get("flat", n), lex)) == norm(sx) else "NO"))
            f2 = [w.contract(n).title + " (flat text)"] + psi(sx, lex)
            res.append("return " + ("yes" if f2 == get("flat", n) else "equivalent" if norm(phi(f2, lex)) == norm(phi(get("flat", n), lex)) else "NO"))
            back = decompile(w, n)
            if back == get("gr", n):
                res.append("recover same")
            else:
                w2 = World(allnames)
                w2.c, w2.lex = dict(w.c), dict(w.lex)
                put("back-tmp", n, back)
                c2 = read_gr(n, "back-tmp")
                w2.c[n] = c2
                w2.lex.pop(n, None)
                s2, _ = compile_contract(w2, n)
                res.append("recover " + ("equivalent" if norm(s2) == norm(sas) and sas_first(w2, n) == sas_first(w, n) else "NO"))
                os.remove(os.path.join(ROOT, "back-tmp", n + ".txt"))
                os.rmdir(os.path.join(ROOT, "back-tmp"))
                put("back", n, back)
        except Err as ex:
            res.append("ERROR " + str(ex))
        if any("NO" in r or "ERROR" in r for r in res):
            bad += 1
        print("%-10s %s" % (n, "; ".join(res)))
    print("%d contracts, %d with a fault" % (len(names), bad))
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
