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
import itertools, os, re, sys

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
POSS_IN = re.compile(r"the (\w+?)s of ((?:%s) (?:[A-Z][A-Za-z]*|\d+) persons)" % "|".join(COUNTS))   # 'the votes of at least V persons' is read as 'at least V persons's vote'
POSS_OUT = re.compile(r"((?:%s) (?:[A-Z][A-Za-z]*|\d+) persons)'s (\w+)" % "|".join(COUNTS))      # and printed so (GO #6, Calculus #6, 2026-11-15 16:37 UTC)


def poss_in(l):
    return POSS_IN.sub(lambda m: "%s's %s" % (m.group(2), m.group(1)), l)


def poss_out(l):
    return POSS_OUT.sub(lambda m: "the %ss of %s" % (m.group(2), m.group(1)), l)


def count_code(v):
    m = re.fullmatch(r"(%s) (\w+) persons" % "|".join(COUNTS), v)
    return COUNTS[m.group(1)] + m.group(2) if m else v
NOUNS = set()     # 'the role r' in a basic state: 'no role' and 'some role' are written for 'the role no one', 'the role someone'
MARK = "*#"   # the last argument of an atom: *n, n copies; #n, exactly n copies and no more
PARL = "Parameters: "   # 'Parameters: x is entitled to propose m in z.': a state a later contract, the bylaws or the terms fill; a parameter of the term
ENVL = "Environment: "   # 'Environment: x is nearby y.': a state the platform gives, an environment predicate of the term; no act adds or ends it
REFL = "Reflexive: "   # 'Reflexive: x knows y.': every agent holding the state's atoms holds x knows x; no act adds or ends it
ARITH = "Vocabulary: n is at most m = le(n,m); n is at least m = ge(n,m); n is more than m = gt(n,m); n is less than m = lt(n,m); n is m plus k = sum(n,m,k); n is m times k = product(n,m,k); x is the author of t = author(x,t)."   # of every contract; the author of an item is its first signer (GFP), read by toglp.py as eq(author_of(t), x)
ACTOR = "Actor"   # in a citation: the first of those who take the act cited
IRREG = {"is": "be", "has": "have", "does": "do", "goes": "go"}
TAKEN = r", taken for %s by %s(?: and for %s by %s)*" % (NAME, NAME, NAME, NAME)   # a citation taking the act cited for a party: its guard NAME1 is NAME2
VOTES = r", on the votes of %s(?: and of %s)* for it" % (NAME, NAME)   # an act or a citation decided by the votes of organisations, GO's
GO_VOTES = {"vote": "z holds x's vote on m", "voter": "x is entitled to vote on m in z", "decided": "the votes decide m according to the bylaws of z"}   # GO's states of a motion m
VOTER = "Voter"   # the name of the one who votes, in the vote act generated
GO_TAKE = "x is entitled to take m for z"   # GO's state of the one who takes a motion m for z, derived for an act taken for a party on votes
GO_STATES = dict(GO_VOTES, taker=GO_TAKE)
WARD_ROLE = "those who decide for z"   # GnW's role: a contract whose lexicon has it has wards, and a ward takes an act of a party by those who decide for it
KNOWS = "x knows y"   # GKK's: the edge a ward's form adds from its officer to a role that reads, and from the organisation to each agent of a motion
OFFICER = "Officer"   # AliceOfficer: the one who takes an act for the party Alice, in the ward's form derived of the act


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
        self.refl = []    # basic states of two places declared reflexive: 'Reflexive: x knows y.', every agent holding x knows x
        self.env = []     # basic states declared environment predicates: 'Environment: x is nearby y.', given by the platform
        self.par = []     # basic states declared parameters: 'Parameters: x is entitled to propose m in z.'
        self.votes = []   # the acts decided 'on the votes of NAME for it': GO's states of a motion, derived for each (vote_family)
        self.voted = {}   # the name of a sentence decided on votes -> the ids of its vote families

    def copy(self):
        l = Lex()
        l.basic, l.roles, l.defs, l.req, l.short = list(self.basic), list(self.roles), list(self.defs), dict(self.req), dict(self.short)
        l.rlist = dict(self.rlist)
        l.refl = list(self.refl)
        l.env = list(self.env)
        l.par = list(self.par)
        l.votes = [dict(f) for f in self.votes]
        l.voted = dict(self.voted)
        return l

    def refl_preds(self):
        """The predicates of the states declared reflexive."""
        out = set()
        for t in self.refl:
            b = [(p, a) for tt, p, a in self.basic if tt == t]
            if not b or len(b[0][1]) != 2:
                raise Err("'Reflexive: %s.' names no basic state of two places" % t)
            out.add(b[0][0])
        return out

    def env_preds(self, attr="env"):
        """The predicates of the states declared environment predicates, or with attr 'par' parameters."""
        out = set()
        norm = lambda t: t.replace(",", "")
        for t in getattr(self, attr):
            b = [p for tt, p, a in self.basic if norm(tt) == norm(t)]
            if not b:
                raise Err("'%s%s.' names no basic state" % (ENVL if attr == "env" else PARL, t))
            out.add(b[0])
        return out

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
        self.refl.extend(t for t in other.refl if t not in self.refl)
        self.env.extend(t for t in other.env if t not in self.env)
        self.par.extend(t for t in other.par if t not in self.par)
        self.votes.extend(f for f in other.votes if f["id"] not in {g["id"] for g in self.votes})
        for k, v in other.voted.items():
            self.voted.setdefault(k, v)

    def states(self):
        """Templates of states, longest first, each with its payload; kept while the lexicon is the same."""
        k = (tuple(self.basic), tuple(self.defs), tuple(sorted(self.short.items())))
        if getattr(self, "_states", (None,))[0] != k:
            self._states = (k, self._states_of())
        return self._states[1]

    def _states_of(self):
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
    """A list of persons: [(text, guard term)].  A role may be followed by ', other than NAME', a person who is not of
    those who take the act for it, written as the argument -NAME of its guard term: deciders(Carol,-Bob)."""
    m = re.fullmatch(r"(.*), other than (%s),?" % NAME, text.strip())
    if m:
        out = persons_of(m.group(1), lex)
        if not out or "(" not in out[-1][1]:
            raise Err("'other than' after no role: " + text)
        out[-1] = (out[-1][0] + ", other than " + m.group(2) + ",", out[-1][1][:-1] + ",-" + m.group(2) + ")")
        return out
    out = []
    for _, pay, b, s in parse_list(text, lex.persons(), "persons"):
        if pay[0] == "name":
            out.append((s, s))
        else:
            out.append((low(s), "%s(%s)" % (pay[2], ",".join(b[v] for v in pay[3]))))
    return out


def lits_of(text, lex, actor=None):
    return [(n, p, a) for n, _, _, p, a in unfold(text, lex, actor=actor)]


def self_atom(pred, args, rp):
    """An atom of a reflexive predicate naming one agent twice, x knows x."""
    return pred in rp and len(args) == 2 and args[0] == args[1]


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
    lines = [poss_in(l.rstrip("\n")) for l in open(os.path.join(ROOT, d, name + ".txt"), encoding="utf-8") if l.strip()]
    c.title = lines[0]
    c.params = params_of(lines, name)
    c.first = None
    c.own = Lex()
    c.header = []      # (kind, line): vocabulary, roles, definitions, prose and names, in order
    c.clauses = []     # (number, mark, text)
    for i, l in enumerate(lines[1:]):
        m = CLAUSE.match(l)
        if m:
            c.clauses.append((int(m.group(1)), m.group(2), params_in(m.group(3), c.params)))
        elif l == PRAGMAS:
            continue
        elif i == 0 and re.match(re.escape(name) + r" (extends|restricts|strengthens|instantiates|composes)\b", l):
            c.first = l
        elif l.startswith(REFL):
            c.own.refl.extend(t for t in l[len(REFL):].rstrip(".").split("; ") if t not in c.own.refl)
            c.header.append(("prose", l))      # a kept declaration: once in the schemas, in the flat text and in every expansion
        elif l.startswith(ENVL):
            c.own.env.extend(t for t in params_in(l[len(ENVL):], c.params).rstrip(".").split("; ") if t not in c.own.env)
            c.header.append(("prose", l))      # a kept declaration: once in the schemas, in the flat text and in every expansion
        elif l.startswith(PARL):
            c.own.par.extend(t for t in params_in(l[len(PARL):], c.params).rstrip(".").split("; ") if t not in c.own.par)
            c.header.append(("prose", l))      # a kept declaration: once in the schemas, in the flat text and in every expansion
        elif l.startswith("Vocabulary: ") or l.startswith("Roles: "):
            kind, rest = l.split(": ", 1)
            for e in rest.rstrip(".").split("; "):
                t, a = e.rsplit(" = ", 1)
                by = None
                if ", listed by " in a:
                    a, by = a.split(", listed by ", 1)
                _, p, args = parse_atom(a)
                if any(x in args for x in c.params):
                    raise Err("the parameter %s is a place of %s" % ([x for x in c.params if x in args][0], a))
                t = params_in(t, c.params)
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
            t, r = params_in(l[:-1], c.params).split(": ", 1)
            act = t.startswith("After ")
            t = t[6:] if act else t
            if act and ", which requires " in t:
                t, c.own.req[t.split(", which requires ", 1)[0]] = t.split(", which requires ", 1)
            c.own.defs.append((t, r, act))
            c.header.append(("lex", l))
        else:
            c.header.append(("prose", l))
    return c


def params_of(lines, name):
    """The parameters a contract declares in lower case in a prose line, as GFF-FR's 'd > 1, an integer, is a
    parameter of GFF-FR.' and GDO-LB's 'd > 0, an integer, and e > 0 are parameters of GDO-LB.': {letter: its name}.
    A lower-case letter is a place of the grammar, so in the clauses the parameter is read as a name, its capital;
    the recovered clauses write the letter again (decompile)."""
    out = {}
    for l in lines:
        if re.search(r" (?:is a parameter|are parameters) of %s\.$" % re.escape(name), l):
            for x in re.findall(r"(?<![A-Za-z])([b-z]) (?:[<>]=?|=) ", l):
                out[x] = x.upper()
    names = set(re.findall(NAME, " ".join(l for l in lines if re.match(NAME + r"(?:, " + NAME + r")*(?: and " + NAME + r")? (?:is|are) ", l))))
    for x, y in out.items():
        if y in names:
            raise Err("the parameter %s would be read as %s, which the names line names" % (x, y))
    return out


def params_in(text, params):
    """A clause with each lower-case parameter read as its name."""
    for x, y in params.items():
        text = re.sub(r"(?<![A-Za-z'*-])%s(?![A-Za-z'-])" % x, y, text)
    return text


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


def rename_act(e, new, old):
    """An act of a contract instantiated by a word takes the new word in its name, as the full contract has it: GnW's
    Make guardian is CSSN's Make parent, Wards befriend its Children befriend.  The name its own contract gives it
    stays its original name, oname, by which it is joined and cited."""
    if e.get("kind") not in ("act", "cite") or "name" not in e:
        return
    (_, nw), (_, ow) = article(new), article(old)
    def one(m):
        w = m.group(0)
        to = plural(nw) if w.lower() == plural(ow).lower() else nw
        return cap(to) if w[:1].isupper() else to
    name = re.sub(r"(?i)\b(?:%s|%s)\b" % (re.escape(plural(ow)), re.escape(ow)), one, e["name"])
    if name != e["name"]:
        e.setdefault("oname", e["name"])
        e["name"] = name


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
        if x != y or mx and my:      # a place of b put for two places of t is not t
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


PUT_RX = {}


def put_rx(t):
    """The pattern of a state put for a state, a name it adds read also as someone, anyone or no one."""
    if t not in PUT_RX:
        r = tpl_rx(t)
        for x in extras(t):
            r = re.sub(r"(?<![A-Za-z])%s(?![A-Za-z])" % x, "(?:%s|%s)" % (x, "|".join(WILDW)), r)
        PUT_RX[t] = re.compile(r + r"(?=$|[ ,.])")
    return PUT_RX[t]


def subst_states(text, lex, tb, new, effect, put=()):
    """A definition of the contract instantiated, with the state put for the basic state tb.  A name the state put
    adds is anyone where the state must not hold, and where it ends for someone unnamed."""
    if not text:
        return text
    vs = sorted(set(re.findall(r"(?<![A-Za-z'-])([b-z])(?='s|-[A-Za-z]|[ ,.]|$)", text)))
    up = {v: "Zz" + v.upper() + "zz" for v in vs}
    filled = " ".join(fill(tok, up) for tok in toks(text))
    out = []
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
        self.party = {}    # n -> the ward's forms n derives of the acts of parties it has (party_specs)

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
            for item in derive_items(subs.get(p, []), self.lexicon(p)):
                sf = split_for(item, self.lexicon(p))
                if sf and sf[0] == "state":
                    _, new, tb, _, _ = sf
                    put = getattr(pl, "put", []) + [new]
                    pl.put = put
                    pl.defs = [(t, subst_states(r, pl, tb, new, act, put), act) for t, r, act in pl.defs]
                    pl.req = {t: subst_states(v, pl, tb, new, False, put) for t, v in pl.req.items()}
                    pl.basic = [e for e in pl.basic if e[0] != tb]
                    pl.par = [t for t in pl.par if t.replace(",", "") != tb.replace(",", "")]   # a parameter the first line binds is no longer one
                if sf is None:
                    new, old = item.split(" for ", 1)
                    ft, fp = word_sub(new, old)
                    pl.basic = [(ft(t), fp(q), a) for t, q, a in pl.basic]
                    pl.roles = [(ft(t), fp(q), a) for t, q, a in pl.roles]
                    pl.defs = [(ft(t), ft(r), act) for t, r, act in pl.defs]
                    pl.req = {ft(t): ft(v) for t, v in pl.req.items()}
                    pl.short = {ft(t): (ft(v[0]), v[1]) for t, v in pl.short.items()}
                    pl.rlist = {fp(k): ft(v) for k, v in pl.rlist.items()}
                    pl.refl = [ft(t) for t in pl.refl]
                    pl.env = [ft(t) for t in pl.env]
                    pl.par = [ft(t) for t in pl.par]
                    pl.votes = [dict(f, t=ft(f["t"]), P=ft(f["P"]), st={k: dict(v, go=fp(v["go"]), pred=fp(v["pred"]), tpl=ft(v["tpl"])) for k, v in f["st"].items()})
                                for f in pl.votes]
            lex.add(pl)
        lex.add(arith())
        for name, fams in self.vote_families(n, lex, parents, subs):
            lex.voted[name] = [add_family(lex, f) for f in fams]
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
            lex.par = [t for t in lex.par if t.replace(",", "") != tb.replace(",", "")]   # a parameter the first line binds is no longer one

    def binds(self, n):
        """The states that n and the contracts it is written from put for states: [(item, the contract instantiated)]."""
        c = self.contract(n)
        parents, subs, _ = first_line(c, self.known)
        out = []
        for p in parents:
            for item in derive_items(subs.get(p, []), self.lexicon(p)):
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

    def vote_families(self, n, lex, parents, subs):
        """The sentences of n decided 'on the votes of NAME for it': [(name, the vote family of each NAME)].  The family is
        the act's, as its contract names it, and the organisation's: two citations of one act on one's votes share it."""
        c, out = self.contract(n), []
        for num, mark, text in c.clauses:
            for s in sentences(text):
                m = re.match(r"([A-Z][A-Za-z' -]*?): (.+)$", s)
                vm = m and re.search(VOTES + r"\.$", s)
                if not vm:
                    continue
                orgs, body, where = votes_of(vm.group(0)), m.group(2)[:len(m.group(2)) - len(vm.group(0)) + 1], "%s:%d %s" % (n, num, m.group(1))
                cm = re.match(r"(%s)(?: ([A-Z][A-Za-z' -]*?))?(?=,|\.$)" % REF, body)
                try:
                    if cm:
                        entries = []
                        for p in parents:
                            words = [it for it in subs.get(p, []) if split_for(it, self.lexicon(p)) is None]
                            entries += instantiate_schemas(expand_schemas(self, p), words)
                        hit = own_acts(entries, cited(entries, cm.group(1), cm.group(2), where))
                        if len(hit) != 1:
                            raise Err("a citation on votes names one act")
                        e = entries[hit[0]]
                        actname = e["name"]
                        _, ir = inv_basic(lex)
                        best = best_phrase(e, lex, act_phrases(lex), [guard_text(g, ir) for g in e["guards"]])
                        if not best:
                            raise Err("no act phrase has the effects of the act cited")
                        t, b = best[1], best[2]
                    else:
                        _, t, b, _, _, _, _ = read_act(body, lex)
                        actname = m.group(1)
                    fams = []
                    for org in orgs:
                        zp = [v for v in tvars(t) if b.get(v) == org]
                        if not zp:
                            raise Err("%s is no place of the act decided on its votes" % org)
                        fams.append(pick_family(lex, actname, t, zp, org, out))
                    out.append((m.group(1), fams))
                except Err as ex:
                    raise Err("%s: %s" % (where, ex))
        self.party[n] = self.party_specs(n, lex, parents, subs)
        _, ir = inv_basic(lex)
        for sp in self.party[n]:
            if not sp["votes"]:
                continue
            best = best_phrase(sp["act"], lex, act_phrases(lex), [guard_text(g, ir) for g in sp["act"]["guards"]])
            if not best:
                raise Err("%s %s: no act phrase has the effects of the act" % (n, sp["name"]))
            t, b = best[1], best[2]
            fams = []
            for org in sp["votes"]:
                zp = [v for v in tvars(t) if b.get(v) == org]
                if not zp:
                    raise Err("%s %s: %s is no place of the act" % (n, sp["name"], org))
                fams.append(pick_family(lex, sp["act"]["name"], t, zp, org, out))
            out.append((sp["name"], fams))
        return out

    def party_specs(self, n, lex, parents, subs):
        """The ward's forms of the acts of parties, which a contract with wards (its lexicon has GnW's role 'those who
        decide for z') derives of the acts it has whose guards include names that the act's own contract declares
        parties, and that no contract it is written from has derived.  For each nonempty set S of those guards, one
        act '<Act> for <Role>[ and <Role>]', the act taken for each ward R of S by one who decides for it: written from
        GO, by its officer ROfficer, only if ROfficer is entitled to take it for R, on the votes of R for it (GO:2's Act);
        else by those who decide for R (GnW).  An act that names a ward as such, not as a party, has no such form."""
        if not any(r[0] == WARD_ROLE for r in lex.roles):
            return []
        entries = []
        for p in parents:
            words = [it for it in subs.get(p, []) if split_for(it, self.lexicon(p)) is None]
            entries += instantiate_schemas(expand_schemas(self, p), words)
        done = {e["party"] for e in entries if e.get("party")}
        go = any(e[0] == GO_TAKE for e in lex.basic)
        role = [r for r in lex.roles if r[0] == WARD_ROLE][0]
        out, seen = [], set()
        for e in entries:
            if e["kind"] != "act" or e.get("party") or e.get("voteact") or e.get("taken"):
                continue
            pid = e.get("pid") or key(e)
            if pid in done or pid in seen:
                continue
            seen.add(pid)
            origin = e["label"].split("+")[0].split(":")[0]
            kinds = kinds_line(self.contract(origin)) if origin in self.known else {}
            rs = [g for g in e["guards"] if "(" not in g and "party" in kinds.get(g, ())]
            if not rs:
                continue
            agents = {x for x, ks in kinds.items() if ks & {"person", "party", "organisation", "ward"}}
            kn = [p for t, p, a in lex.basic if t == KNOWS]
            kn = kn[0] if kn else None
            if not go and any(split_for(it, self.lexicon(q))[2] in GO_STATES.values() for it, q in self.binds(n)):
                raise Err("%s: the acts of parties have no ward's form in a contract that binds GO's states of a motion" % n)
            for k in range(1, len(rs) + 1):
                for S in itertools.combinations(rs, k):
                    sfx = " for " + join(S)
                    if go:
                        taken = {r: r + OFFICER for r in S}
                        ttext, votes = dict(taken), list(S)
                    else:
                        taken = {r: "%s(%s)" % (role[1], r) for r in S}
                        ttext, votes = {r: fill(WARD_ROLE, {"z": r}) for r in S}, None
                    out.append(dict(pid=pid, act=e, name=e["name"] + sfx, sfx=sfx, taken=taken, ttext=ttext, votes=votes,
                                    takers=dict(taken) if go else None, agents=agents if go else None,
                                    knows=reader_edges(e, taken, kn, agents) if go and kn else []))
        return out


def ident(name):
    return re.sub(r"[^a-z0-9]+", "_", name.lower()).strip("_")


def votes_of(s):
    """The organisations of ', on the votes of Alice and of Bob for it', or of a schema's 'Alice and of Bob'."""
    return re.findall(r"(?:^|of )(%s)(?= and of | for it|$)" % NAME, s.strip())


def votes_text(orgs):
    return " and of ".join(orgs)


def vote_family(lex, name, t, zp, org):
    """GO's states of a motion m derived for the act phrase t decided on the votes of the agent at its place zp: m is
    'whether' and the act phrase, that agent at GO's place z: 'z holds x's vote on whether z pays y n y-coins'."""
    tv = tvars(t)
    pool = [v for v in "bcdefghijklnopqrstuvwy" if v not in tv]
    ren = {v: "z" if v == zp else pool.pop(0) if v in "xzm" else v for v in tv}
    P = rename(t, ren)
    places = [ren[v] for v in tv if v != zp]
    st, fid = {}, ident(name + " " + org)
    for k, T in list(GO_VOTES.items()) + ([("taker", GO_TAKE)] if any(e[0] == GO_TAKE for e in lex.basic) else []):
        go = [e for e in lex.basic if e[0] == T]
        if not go:
            raise Err("on the votes of: no state '%s' of GO, unbound, in this contract" % T)
        _, gp, ga = go[0]
        st[k] = dict(go=gp, goargs=ga, pred=gp + "_" + fid, tpl=" ".join("whether " + P if tok == "m" else tok for tok in toks(T)),
                     args=tuple(a2 for a in ga for a2 in (places if a == "m" else [a])))
    return dict(name=name, vname="Vote of %s for %s" % (org, name), id=fid, org=org, t=t, zp=zp, ren=ren, P=P, places=places, st=st)


def pick_family(lex, name, t, zps, org, out):
    """The vote family of an act decided on the votes of org, which holds the places zps of its act phrase: of the
    place that gives a motion the contract has already, if one does (an act of one agent in two roles, as Helper #7's
    Deposit with the issuer, is decided as the act it specialises), else of the first."""
    have = {f["st"]["vote"]["tpl"] for f in lex.votes} | {f["st"]["vote"]["tpl"] for _, fs in out for f in fs}
    cands = [vote_family(lex, name, t, z, org) for z in zps]
    return next((f for f in cands if f["st"]["vote"]["tpl"] in have), cands[0])


def add_family(lex, f):
    """The derived states in the vocabulary, the entitlement and the decision parameters as GO's are, and the vote act;
    its id.  Two acts of one phrase on one organisation's votes (DGC's Deposit and Deposit more) are one motion, one family."""
    same = [g for g in lex.votes if g["st"]["vote"]["tpl"] == f["st"]["vote"]["tpl"]]
    if same:
        return same[0]["id"]
    if any(g["id"] == f["id"] for g in lex.votes):
        raise Err("two acts named %s, of different phrases, decided on votes" % f["name"])
    for k, v in f["st"].items():
        lex.basic.append((v["tpl"], v["pred"], v["args"]))
        if k != "vote":
            lex.par.append(v["tpl"])
    vt = f["st"]["vote"]["tpl"]
    act = "x votes on whether %s in z" % f["P"]
    lex.defs.append((act, vt, True))
    lex.req[act] = negative(vt)
    lex.votes.append(f)
    return f["id"]


def derive_items(items, plex):
    """The items of an instantiation, and for each that puts states for GO's state of a motion, the same for that
    state derived for each act decided on votes: m is 'whether' and the act phrase."""
    out = list(items)
    for item in items:
        sf = split_for(item, plex)
        if not sf or sf[0] != "state" or sf[2] not in GO_STATES.values():
            continue
        k = [k for k, T in GO_STATES.items() if T == sf[2]][0]
        for f in plex.votes:
            if k not in f["st"]:
                continue
            mine = set(tvars(sf[1])) - {"z", "x", "m"}
            pool = [v for v in "bcdefghijklnopqrstuvwy" if v not in mine and v not in f["places"]]
            mp = {v: pool.pop(0) if v in mine else v for v in f["places"]}
            P = rename(f["P"], mp)
            new = " ".join(("whether " + P + tok[1:]) if tok.rstrip(",") == "m" else tok for tok in toks(sf[1]))
            try:
                nums = count_names(pattern(sf[1], plex))
            except Err:
                nums = set()
            out.append(org_numbers(new, nums, f["org"]) + " for " + rename(f["st"][k]["tpl"], mp))
    return out


def count_names(lits):
    """The names a list of literals counts or computes with, numbers the bylaws set: Votes and More of 'z holds the
    votes of at least Votes persons on m, More is Votes plus 1 and z has fewer than More persons in the role Treasurer'."""
    out = set()
    for _, p, a in lits:
        for v in a:
            m = re.fullmatch(r"(<=|>=|<|>|=)?(%s)" % NAME, v.lstrip(MARK))
            if m and (m.group(1) or p in ("sum", "product", "le", "ge", "gt", "lt")):
                out.add(m.group(2))
    return out


def org_numbers(s, nums, org):
    """The numbers of a decision on the votes of org named for it, VotesAlice, so that an act on the votes of two
    organisations counts the votes of each against its own number."""
    return re.sub(r"(?<![A-Za-z])(%s)(?![A-Za-z])" % "|".join(sorted(nums)), lambda m: m.group(1) + org, s) if nums else s


def derive_atoms(items, fams):
    """derive_items on the schemas: 'vote(z,>=V,m), ... for decided(z,m)', for each act decided on votes."""
    out = list(items)
    for item in items:
        new, old = item.rsplit(" for ", 1)
        if "(" not in old:
            continue
        _, pred, args = parse_atom(old)
        for f in fams:
            k = [k for k, v in f["st"].items() if v["go"] == pred and len(v["goargs"]) == len(args)]
            if not k:
                continue
            ms = tuple("m%d" % (i + 1) for i in range(len(f["places"])))
            nums = count_names([parse_atom(l) for l in split_top(new)])
            ex = lambda a: [x for y in a for x in (ms if y == "m" else (y,))]
            nl = []
            for x, q, a in (parse_atom(l) for l in split_top(org_numbers(new, nums, f["org"]))):
                if "m" in a:
                    d = [v["pred"] for v in f["st"].values() if v["go"] == q]
                    if not d:
                        raise Err("the motion m in a state not derived for votes: %s" % item)
                    q = d[0]
                nl.append((x, q, tuple(ex(a))))
            out.append(", ".join(lit_s(l) for l in nl) + " for " + atom_s((f["st"][k[0]]["pred"], tuple(ex(args)))))
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
            cm = m and re.match(r"(%s)(?: ([A-Z][A-Za-z' -]*?))?((?:%s)?)((?:, only if .*?)?)((?:, with the consent of .*?)?)((?:, and then .*?)?)((?:%s)?)\.$"
                                % (REF, TAKEN, VOTES), m.group(2))
            vm = m and not cm and re.search(VOTES + r"\.$", m.group(2))
            try:
                if cm:
                    conds = cm.group(4)[len(", only if "):] if cm.group(4) else ""
                    cons = cm.group(5)[len(", with the consent of "):] if cm.group(5) else ""
                    then = cm.group(6)[len(", and then "):] if cm.group(6) else ""
                    e = dict(kind="cite", label=label, mark=mark, name=m.group(1), target=cm.group(1), tname=cm.group(2),
                             body=lits_of(conds, lex, ACTOR), guards=[g for _, g in persons_of(cons, lex)] if cons else [],
                             then=lits_of(then, lex, ACTOR), taken=taken_of(cm.group(3)), votes=votes_of(cm.group(7)) if cm.group(7) else None)
                    theirs = {q for _, q, _ in w.lexicon(cm.group(1).split(":")[0]).basic}
                    if any(q in theirs for _, q, _ in e["then"]):
                        raise Err("a strengthening adds a state of the contract it strengthens")
                    sas.append(e)
                    own.append(dict(e, conds=phrases_of(conds, lex, ACTOR), persons=[p for p, _ in persons_of(cons, lex)] if cons else [],
                                    effects=phrases_of(then, lex, ACTOR)))
                elif m:
                    persons, t, b, r, consent, conds, must = read_act(m.group(2)[:vm.start()] + "." if vm else m.group(2), lex)
                    musts = persons_of(must, lex) if must else []      # who must consent, and is not asked
                    cons = (persons_of(consent, lex) if consent else []) + musts
                    who = actor_of([p for p, _ in persons], t, b)
                    eff, cnd = unfold(fill(r, b), lex, actor=who), unfold(conds, lex, actor=who)
                    if t in lex.req:
                        cnd = cnd + unfold(fill(lex.req[t], b), lex, actor=who)
                    rp = lex.refl_preds()
                    if any(self_atom(x[3], x[4], rp) for x in eff):
                        raise Err("an act adds or ends a state its holder holds of itself, which the pragmas declare reflexive")
                    if any(x[0] and self_atom(x[3], x[4], rp) for x in cnd):
                        raise Err("a condition that a reflexive state not hold of its holder itself")
                    cnd = [x for x in cnd if not self_atom(x[3], x[4], rp)]      # it holds: taken as holding
                    head = [(x[0], x[3], x[4]) for x in eff]
                    if any(WILD in x[4] for x in eff if not x[0]) or any(a[:1] in "<>" for x in eff for a in x[4]):
                        raise Err("an act cannot make a state hold of someone unnamed, or of a number of persons")
                    body = [(x[0], x[3], x[4]) for x in cnd] + [(False, x[3], x[4]) for x in eff if x[0] and WILD not in x[4]]
                    sas.append(dict(kind="act", label=label, mark=mark, name=m.group(1), head=head, body=body,
                                    guards=list(dict.fromkeys(g for _, g in persons + cons))))
                    same = same_of(c, num, names_of(sas[-1]))
                    sas[-1]["same"] = same
                    sas[-1]["must"] = [g for _, g in musts]
                    sas[-1]["votes"] = votes_of(vm.group(0)) if vm else None
                    if not set(sas[-1]["must"]) <= set(sas[-1]["guards"]):      # one who must consent is one of those who take the act
                        raise Err("%s: one who must consent is not among its guards: %s" % (m.group(1), ", ".join(sorted(set(sas[-1]["must"]) - set(sas[-1]["guards"])))))
                    own.append(dict(kind="act", label=label, mark=mark, name=m.group(1), same=same, must=[p for p, _ in musts], persons=list(dict.fromkeys(p for p, _ in persons + cons)), votes=sas[-1]["votes"],
                                    effects=[(x[0], wild(fill(x[1], x[2]), x[0])) for x in eff], conds=[(x[0], wild(fill(x[1], x[2]), x[0])) for x in cnd]))
                else:
                    sas.append(dict(kind="kept", label=label, mark=mark, text=s))
                    own.append(dict(kind="kept", label=label, mark=mark, text=s))
            except Err as ex:
                raise Err("%s, %s: %s" % (label, s[:40], ex))
    return sas, own


# ---- printing and reading schemas ----------------------------------------------------

def taken_of(s):
    """{NAME1: NAME2} of ', taken for NAME1 by NAME2 and for NAME3 by NAME4'."""
    return dict(re.findall(r"for (%s) by (%s(?:\([^)]*\))?)" % (NAME, NAME), s)) if s else {}


def schema_line(e):
    pre = "%s %s " % (e["label"], e["mark"])
    if e["kind"] == "kept":
        return pre + "| " + e["text"]
    head = ", ".join(lit_s(l) for l in e["head"]) if e["kind"] == "act" else e["target"] + (" " + e["tname"] if e.get("tname") else "") + \
        taken_text(e.get("taken")) + (", on the votes of " + votes_text(e["votes"]) if e.get("votes") else "") + \
        (" + " + ", ".join(lit_s(l) for l in e["then"]) if e.get("then") else "")
    s = "%s%s: %s <-" % (pre, e["name"], head)
    if e["body"]:
        s += " " + ", ".join(lit_s(l) for l in e["body"])
    if e["guards"]:
        s += " guarded by " + ", ".join(e["guards"])
    if e["kind"] == "act" and e.get("taken"):
        s += " " + taken_text(e["taken"])[2:]
    if e["kind"] == "act" and e.get("votes"):
        s += " on the votes of " + votes_text(e["votes"])
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
    votes = None
    if " on the votes of " in " " + rest:
        rest, votes = (" " + rest).rsplit(" on the votes of ", 1)
        votes = votes_of(votes)
    taken = {}
    if " taken for " in " " + rest:
        rest, tk = (" " + rest).rsplit(" taken ", 1)
        taken = taken_of(tk)
    guards = []
    if " guarded by " in " " + rest:
        rest, g = (" " + rest).rsplit(" guarded by ", 1)
        guards = split_top(g)
    body = [parse_atom(a) for a in split_top(rest)]
    cm = re.fullmatch(r"(%s)(?: ([^+,]+?))?((?:%s)?)(?:, on the votes of (%s(?: and of %s)*))?(?: \+ (.+))?" % (REF, TAKEN, NAME, NAME), head.strip())
    if cm:
        return dict(kind="cite", label=label, mark=mark, name=name, target=cm.group(1), tname=cm.group(2), body=body, guards=guards,
                    then=[parse_atom(a) for a in split_top(cm.group(5))] if cm.group(5) else [], taken=taken_of(cm.group(3)), votes=votes_of(cm.group(4)) if cm.group(4) else None)
    return dict(kind="act", label=label, mark=mark, name=name, head=[parse_atom(a) for a in split_top(head)], body=body, guards=guards, same=same, must=must, votes=votes, taken=taken)


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
        f.write("\n".join(poss_out(l) for l in lines) + "\n")


def get(d, n):
    return [poss_in(l.rstrip("\n")) for l in open(os.path.join(ROOT, d, n + ".txt"), encoding="utf-8") if l.strip()]


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
        out.update(x.lstrip("-") for x in parse_atom(g)[2]) if "(" in g else out.add(g)
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
    back = {v: k for k, v in (hit.get("taken") or {}).items()}       # an act taken for a party: the party takes it
    first = [back.get(x, x) for x in (hit["persons"] if text else hit["guards"])[:1]]
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
    return "%s(%s)" % (p, ",".join("-" + mp.get(x[1:], x[1:]) if x.startswith("-") else mp.get(x, x) for x in a))


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
                    for x in {a for nl in newl for a in nl[2] if a not in args and re.fullmatch(r"[a-z]", a)}:   # a name the act only requires: someone; a constant (Treasurer) stays
                        if not any(x in l[2] for l in e["head"]):
                            e["body"] = [(n, q, tuple(WILD if a == x else a for a in aa)) for n, q, aa in e["body"]]
                    gs = []
                    for g in e["guards"]:
                        if "(" in g and parse_atom(g)[1] == pred:
                            ga = parse_atom(g)[2]
                            b = dict(zip(args, [x for x in ga if not x.startswith("-")]))
                            g = "%s(%s)" % (newl[0][1], ",".join([b.get(a, a) for a in newl[0][2]] + [x for x in ga if x.startswith("-")]))
                        gs.append(g)
                    e["guards"] = gs
                    if e.get("must"):
                        ms = []
                        for g in e["must"]:
                            if "(" in g and parse_atom(g)[1] == pred:
                                ga = parse_atom(g)[2]
                                b = dict(zip(args, [x for x in ga if not x.startswith("-")]))
                                g = "%s(%s)" % (newl[0][1], ",".join([b.get(a, a) for a in newl[0][2]] + [x for x in ga if x.startswith("-")]))
                            ms.append(g)
                        e["must"] = ms
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
                    rename_act(e, new, old)
                    e["head"] = [(x, fp(p), a) for x, p, a in e["head"]]
                    e["body"] = [(x, fp(p), a) for x, p, a in e["body"]]
                    e["guards"] = [fp(g) if "(" in g else g for g in e["guards"]]
                    e["must"] = [fp(g) if "(" in g else g for g in e.get("must", [])]
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


def unclash(entries):
    """Two acts of one name, one of them joined from two contracts that name it differently: the joined act takes
    the first of its other names, from the clauses it comes from, that no act of the contract has, so that every act
    has a name of its own; GSNW's GSG:2+GSN:3+GnW:3, Unfriend in GSN and Person unfriend in GnW, beside GnW:3's
    Unfriend, keeps GnW's Person unfriend."""
    acts = [e for e in entries if e["kind"] == "act"]
    for e in acts:
        if e.get("party") or "+" not in e["label"] or sum(1 for f in acts if f["name"] == e["name"]) < 2:
            continue
        used = {f["name"] for f in acts}
        alt = next((m for m in e.get("names", {}).values() if m not in used), None)
        if alt:
            e["name"] = alt
    base = {e["pid"]: e["name"] for e in acts if e.get("pid") and not e.get("party")}
    for e in acts:          # a ward's form is named for its act, as the act is named here: "Pay accepted coins for Alice"
        if e.get("party") in base:
            e["name"] = base[e["party"]] + e["sfx"]


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
    hit = [i for i, e in enumerate(entries) if e["kind"] == "act" and not e.get("voteact") and target in e["label"].split("+") and (tname is None or named(e))]
    if not hit:
        raise Err("%s: no act of %s%s" % (where, target, " named " + tname if tname else ""))
    pids = {entries[i].get("pid") for i in hit} - {None}     # an act cited by its name: its ward's forms with it
    return sorted(set(hit) | {i for i, e in enumerate(entries) if e.get("party") in pids})


def reader_edges(e, taken, kn, agents):
    """The edges a ward's form of the act e needs: the party, taken for by its officer, guards no longer, and a role
    that reads (holds a state the act requires, changes none, does not guard) had its edge to a guard from a state the
    party required; each officer then knows each agent the act names, so that it has an edge to each role that reads,
    and an act of one agent in two roles stays the act it specialises with the two names one (GSC's def:volition;
    Calculus #6, the client's decision, 2026-11-15)."""
    names = [a.lstrip(MARK) for l in e["head"] + e["body"] for a in l[2]] + [g for g in e["guards"] if "(" not in g]
    names = [x for x in dict.fromkeys(names) if x in agents]
    return [(False, kn, (o, r)) for o in taken.values() for r in names if r != o]


def own_acts(entries, hit):
    """The acts of hit that are no ward's form derived of another (party_specs)."""
    return [i for i in hit if not entries[i].get("party")]


def renamed(o, name):
    """An act a citation of one act renames; its ward's forms are renamed with it (unclash)."""
    if not o.get("party"):
        o["oname"], o["name"] = o["name"], name


def derive_party(w, n, lex, entries, text=False):
    """The ward's forms of the acts of parties that n derives (World.party_specs), each after its act, with its vote acts."""
    for sp in w.party.get(n, []):
        i = next(i for i, x in enumerate(entries) if x["kind"] == "act" and not x.get("party") and (x.get("pid") or key(x)) == sp["pid"])
        entries[i] = dict(entries[i], pid=sp["pid"])
        o = dict(entries[i])
        if text:
            gen = gen_votes_text(lex, o, sp["votes"], sp["name"], o["label"], o["mark"], sp["takers"], sp["agents"]) if sp["votes"] else None
            ib, _ = inv_basic(lex)
            o["conds"] = o["conds"] + [(False, phrase_of(l, ib)) for l in sp["knows"]]
            o["taken"] = dict(sp["ttext"])
            o["persons"] = [o["taken"].get(g, g) for g in o["persons"]]
            if gen:
                o["conds"], o["effects"] = o["conds"] + gen[0], o["effects"] + gen[1]
        else:
            gen = gen_votes(lex, o, sp["votes"], sp["name"], o["label"], o["mark"], sp["takers"], sp["agents"]) if sp["votes"] else None
            o["body"] = o["body"] + sp["knows"]
            o["taken"] = dict(sp["taken"])
            o["guards"] = [o["taken"].get(g, g) for g in o["guards"]]
            if gen:
                o["body"], o["head"] = o["body"] + gen[0], o["head"] + gen[1]
        o.update(name=sp["name"], oname=sp["name"], names={}, party=sp["pid"], sfx=sp["sfx"])
        o.pop("pid", None)
        j = after(entries, i)
        entries.insert(j, dict(o, of=key(entries[i])))
        if gen:
            entries[j + 1:j + 1] = [dict(v, of=key(entries[i])) for v in new_votes(entries, gen[2])]


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
    lex = w.lexicon(n)
    entries, have = [], {}
    for p in parents:
        inherit(entries, have, instantiate_schemas(expand_schemas(w, p), derive_atoms(subs.get(p, []), lex.votes)), ("head", "body", "guards"))
    for p in parents:
        entries = instantiate_schemas(entries, derive_atoms(binds_sas(w, p), lex.votes))
    for e in own:
        if e["kind"] == "cite":
            hit = cited(entries, e["target"], e["tname"], e["label"])
            if e.get("taken") or e.get("votes"):
                hit = own_acts(entries, hit)
                if len(hit) != 1:
                    raise Err("%s: a citation taken for a party or on votes names one act, and %s has %d" % (e["label"], e["target"], len(hit)))
            one = len(own_acts(entries, hit)) == 1
            for i in hit:
                o = dict(entries[i])
                mp = cite_map(w, n, e, o)
                then = [(x, p, tuple(mp.get(a, a) for a in args)) for x, p, args in e.get("then", [])]
                o["head"] = o["head"] + then
                o["body"] = o["body"] + [(x, p, tuple(mp.get(a, a) for a in args)) for x, p, args in e["body"]] + \
                    [(False, p, args) for x, p, args in then if x and WILD not in args]
                gen = gen_votes(lex, o, [mp.get(x, x) for x in e["votes"]], e["name"], e["label"], e["mark"]) if e.get("votes") else None
                if e.get("taken"):
                    o["taken"] = taken_map(e, mp, o["guards"])
                    o["guards"] = [o["taken"].get(g, g) for g in o["guards"]]
                o["guards"] = o["guards"] + [g for g in (map_guard(g, mp) for g in e["guards"]) if g not in o["guards"]]
                o["must"] = o.get("must", []) + [g for g in (map_guard(g, mp) for g in e.get("must", [])) if g not in o.get("must", [])]
                o["label"], o["mark"] = o["label"] + "+" + e["label"], e["mark"]
                if one:
                    renamed(o, e["name"])
                o["names"] = dict(o.get("names", {}), **{e["label"]: o["name"]})
                if gen:
                    o["body"], o["head"] = o["body"] + gen[0], o["head"] + gen[1]
                if e.get("taken"):          # the act taken for a party is one more act, cited by its own name; the act cited stays as it was
                    o["oname"], o["names"] = o["name"], {e["label"]: o["name"]}
                    j = after(entries, i)
                    entries.insert(j, dict(o, of=key(entries[i])))
                else:
                    j = i
                    entries[i] = o
                if gen:
                    entries[j + 1:j + 1] = [dict(v, of=key(entries[i])) for v in new_votes(entries, gen[2])]
        else:
            if e["kind"] == "act":
                e = dict(e, names={e["label"]: e["name"]})
                org = e.pop("votes", None)
                gen = gen_votes(lex, e, org, e["name"], e["label"], e["mark"]) if org else None
                if gen:
                    e["body"], e["head"] = e["body"] + gen[0], e["head"] + gen[1]
                entries += [e] + (new_votes(entries, gen[2]) if gen else [])
            else:
                entries.append(e)
    derive_party(w, n, lex, entries)
    unclash(entries)
    w.sasx[n] = entries
    return entries


def after(entries, i):
    """The place for one more act taken from entries[i]: after it and the acts taken from it before, in the order cited."""
    j = i + 1
    while j < len(entries) and entries[j].get("of") == key(entries[i]):
        j += 1
    return j


def new_votes(entries, votes):
    """The vote acts generated that the contract does not have yet: one act may be cited on one's votes twice."""
    have = {x["name"] for x in entries if x["kind"] == "act"}
    return [v for v in votes if v["name"] not in have]


def taken_map(e, mp, guards):
    """The guards a citation 'taken for NAME1 by NAME2' replaces, in the names of the act cited: {NAME1: NAME2}."""
    out = {}
    for a, b in e["taken"].items():
        a = mp.get(a, a)
        if a not in guards:
            raise Err("%s: taken for %s, who does not take %s" % (e["label"], a, e["target"]))
        out[a] = b
    return out


def gen_votes(lex, o, orgs, name, label, mark, takers=None, agents=None):
    """The act o decided on the votes of the organisations orgs: ([the conditions that the votes of each decide it],
    [the effect that clears them], [the vote act of each]).  With takers, {org: the one who takes it for org}, also
    the condition that each is entitled to take it for its organisation (GO:2's Act)."""
    out = ([], [], [])
    for k, org in enumerate(orgs):
        c, clear, vote = gen_vote(lex, o, org, k, name, label, mark, (takers or {}).get(org), agents)
        out[0].extend(c)
        out[1].append(clear)
        out[2].append(vote)
    return out


def gen_vote(lex, o, org, k, name, label, mark, taker=None, agents=None):
    """The act o decided on the votes of org: (the conditions that taker, if named, is entitled to take it for org and
    that the votes decide it, the effect that clears them, the vote act).
    The states are GO's of a motion, derived for the act (vote_family); the one who votes is Voter."""
    f = [f for f in lex.votes if f["id"] in lex.voted.get(name, [])[k:k + 1]]
    if not f:
        raise Err("%s %s: decided on votes, and no states of its votes" % (label, name))
    f = f[0]
    r = [r for t, r, act in lex.defs if act and t == f["t"]][0]
    _, ir = inv_basic(lex)
    sv, _ = subject(f["t"])
    full = None
    for b, _ in match_some(pattern(r, lex), o["head"]):
        for fb in subject_bindings(sv, b, [guard_text(g, ir) for g in o["guards"]]):
            if all(v in fb for v in tvars(f["t"])) and fb[f["zp"]] == org:
                full = fb
                break
        if full:
            break
    if not full:
        raise Err("%s %s: %s takes no place of its act phrase '%s'" % (label, name, org, f["t"]))
    if VOTER in names_of(o):
        raise Err("%s %s: the act names %s, the name of the one who votes" % (label, name, VOTER))
    vals = {f["ren"][v]: full[v] for v in tvars(f["t"])}
    at = lambda k, x: tuple(org if a == "z" else x if a == "x" else vals[a] for a in f["st"][k]["args"])
    pr = lambda k: f["st"][k]["pred"]
    # with agents (a ward's form), the organisation knows each agent its motion names, as the vote names them (GKK's
    # 'z knows y'; Calculus #6, the client's decision, 2026-11-15)
    kn = [p for t, p, a in lex.basic if t == KNOWS]
    knows = [(False, kn[0], (org, a)) for a in dict.fromkeys(at("vote", VOTER)) if agents and kn and a in agents and a != org]
    vote = dict(kind="act", label=label, mark=mark, name=f["vname"], head=[(False, pr("vote"), at("vote", VOTER))],
                body=[(False, pr("voter"), at("voter", VOTER))] + knows + [(True, pr("vote"), at("vote", VOTER))], guards=[VOTER], must=[],
                same=[p for p in o.get("same", []) if set(p.split("=")) <= set(at("vote", VOTER))],      # the names of the act possibly one are so in its motion
                names={label: f["vname"]}, voteact=True)
    if taker and "taker" not in f["st"]:
        raise Err("%s %s: taken for %s on its votes, and no state '%s' of GO in this contract" % (label, name, org, GO_TAKE))
    take = [(False, pr("taker"), at("taker", taker))] if taker else []
    return take + [(False, pr("decided"), at("decided", None))], (True, pr("vote"), at("vote", WILD)), vote


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
                            if re.fullmatch(NAME, x):    # a name the first line writes, a count or a constant, stays, as in instantiate_schemas
                                continue
                            xr = r"(?<![A-Za-z])%s(?![A-Za-z])" % x
                            if not any(re.search(xr, ph) for _, ph in e["effects"]):
                                e["conds"] = [(c, re.sub(xr, "anyone" if c else "someone", ph)) for c, ph in e["conds"]]
                    else:
                        def role_sub(x):
                            m = re.fullmatch(r"(.*?)((?:, other than %s,)*)" % NAME, x)      # a role and those other than whom
                            r = rx.fullmatch(m.group(1))
                            return low(fill(new, r.groupdict())) + m.group(2) if r else x
                        e["persons"] = [role_sub(x) for x in e["persons"]]
                        e["must"] = [role_sub(x) for x in e.get("must", [])]
                out.append(e)
        else:
            new, old = item.split(" for ", 1)
            ft, _ = word_sub(new, old)
            for e in entries:
                e = dict(e)
                if e["kind"] == "kept":
                    e["text"] = ft(e["text"])
                else:
                    rename_act(e, new, old)
                    e["effects"] = [(x, ft(s)) for x, s in e["effects"]]
                    e["conds"] = [(x, ft(s)) for x, s in e["conds"]]
                    e["persons"] = [ft(s) for s in e["persons"]]
                    e["must"] = [ft(s) for s in e.get("must", [])]
                out.append(e)
        entries = out
    return entries


def expand_text(w, n, own):
    c = w.contract(n)
    lex = w.lexicon(n)
    parents, subs, _ = first_line(c, w.known)
    entries, have = [], {}
    for p in parents:
        inherit(entries, have, instantiate_text(w, w.flat[p], derive_items(subs.get(p, []), w.lexicon(p)), p, n), ("effects", "conds", "persons"))
    for p in parents:
        for item, q in w.binds(p):
            entries = instantiate_text(w, entries, [item], q, n)
    for e in own:
        if e["kind"] == "cite":
            hit = cited(entries, e["target"], e["tname"], e["label"])
            if e.get("taken") or e.get("votes"):
                hit = own_acts(entries, hit)
                if len(hit) != 1:
                    raise Err("%s: a citation taken for a party or on votes names one act, and %s has %d" % (e["label"], e["target"], len(hit)))
            one = len(own_acts(entries, hit)) == 1
            for i in hit:
                o = dict(entries[i])
                mp = cite_map(w, n, e, o, True)
                o["effects"] = o["effects"] + [(x, map_text(s, mp)) for x, s in e.get("effects", [])]
                o["conds"] = o["conds"] + [(x, map_text(s, mp)) for x, s in e["conds"]]
                gen = gen_votes_text(lex, o, [map_text(x, mp) for x in e["votes"]], e["name"], e["label"], e["mark"]) if e.get("votes") else None
                if e.get("taken"):
                    o["taken"] = taken_map(e, {k: map_text(k, mp) for k in e["taken"]}, o["persons"])
                    o["persons"] = [o["taken"].get(g, g) for g in o["persons"]]
                o["persons"] = o["persons"] + [g for g in (map_text(g, mp) for g in e["persons"]) if g not in o["persons"]]
                o["label"], o["mark"] = o["label"] + "+" + e["label"], e["mark"]
                if one:
                    renamed(o, e["name"])
                o["names"] = dict(o.get("names", {}), **{e["label"]: o["name"]})
                if gen:
                    o["conds"], o["effects"] = o["conds"] + gen[0], o["effects"] + gen[1]
                if e.get("taken"):
                    o["oname"], o["names"] = o["name"], {e["label"]: o["name"]}
                    j = after(entries, i)
                    entries.insert(j, dict(o, of=key(entries[i])))
                else:
                    j = i
                    entries[i] = o
                if gen:
                    entries[j + 1:j + 1] = [dict(v, of=key(entries[i])) for v in new_votes(entries, gen[2])]
        else:
            if e["kind"] == "act":
                e = dict(e, names={e["label"]: e["name"]})
                org = e.pop("votes", None)
                gen = gen_votes_text(lex, e, org, e["name"], e["label"], e["mark"]) if org else None
                if gen:
                    e["conds"], e["effects"] = e["conds"] + gen[0], e["effects"] + gen[1]
                entries += [e] + (new_votes(entries, gen[2]) if gen else [])
            else:
                entries.append(e)
    derive_party(w, n, lex, entries, True)
    unclash(entries)
    w.flat[n] = entries
    return entries


def gen_votes_text(lex, o, org, name, label, mark, takers=None, agents=None):
    """gen_votes on the flat text: the act's effects read as atoms, and what it generates written as psi writes it."""
    head = []
    for x, ph in o["effects"]:
        head += [(x ^ l[0], l[1], l[2]) for l in lits_of(ph, lex)]
    guards = [g for _, g in persons_of(join(o["persons"]), lex)] if o["persons"] else []
    conds, clears, votes = gen_votes(lex, dict(o, head=head, body=[], guards=guards), org, name, label, mark, takers, agents)
    ib, ir = inv_basic(lex)
    return [(False, phrase_of(c, ib)) for c in conds], [(True, phrase_of(c, ib)) for c in clears], \
        [dict(v, persons=[guard_text(g, ir) for g in v["guards"]], effects=[(l[0], phrase_of(l, ib)) for l in v["head"]],
              conds=[(l[0], phrase_of(l, ib)) for l in conditions(v)]) for v in votes]


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
            must = [g for _, g in persons_of(mu[:-1], lex)]      # parsed as the guards are, a role by the role lexicon
        name, body = rest[:-1].split(": ", 1)
        conds = ""
        if ", provided " in body:
            body, conds = body.rsplit(", provided ", 1)
        mm = re.match(r"(.*?) may (?:jointly )?(.*)$", body)
        persons = persons_of(mm.group(1), lex) if mm else []
        eff, cnd = lits_of(mm.group(2) if mm else low(body), lex), lits_of(conds, lex)
        rp = lex.refl_preds()
        cnd = [x for x in cnd if x[0] or not self_atom(x[1], x[2], rp)]
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
    ex = [x[1:] for x in a if x.startswith("-")]
    a = [x for x in a if not x.startswith("-")]
    return fill(ir[p][0], dict(zip(ir[p][1], a))) + "".join(", other than %s," % x for x in ex)


def conditions(e):
    """The body of a schema without the atoms it deletes, each required by its deletion; a deletion with the wildcard,
    which ends every atom it matches, requires none, so an atom of the body that matches it is a condition stated."""
    body = list(e["body"])
    for l in e["head"]:
        if l[0] and WILD not in l[2]:
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
            out.append(flat_line(dict(e, persons=[guard_text(g, ir) for g in e["guards"]], must=[guard_text(g, ir) for g in e.get("must", [])],
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


def best_phrase(e, lex, acts, gts):
    """The act phrase whose effects are the head of the act schema e, its binding, persons, conditions and the effects left."""
    best = None
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
    return best


def taken_text(taken):
    """', taken for Alice by Erin and for Bob by Frank' of {Alice: Erin, Bob: Frank}."""
    return ", taken " + " and ".join("for %s by %s" % kv for kv in taken.items()) if taken else ""


def act_sentence(e, lex, acts, ir):
    """The sentence of an act schema: the act phrase whose effects are its head, its conditions folded.  An act a
    citation takes for a party is written with its own persons, 'taken for' the party 'by' the one who takes it."""
    tk = {k: guard_text(v, ir) for k, v in (e.get("taken") or {}).items()}
    back = {v: k for k, v in tk.items()}
    gts = [back.get(g, g) for g in (guard_text(g, ir) for g in e["guards"])]
    best = best_phrase(e, lex, acts, gts)
    if not best:
        return None
    _, t, full, persons, conds, rest_h = best
    mu = [guard_text(g, ir) for g in e.get("must", [])]
    whole = act_text(persons, t, full, join(g for g in gts if g not in persons and g not in mu),
                     fold_states(conds, lex, actor_of(persons, t, full)), join(mu))
    bare = act_text(persons, t, full, "", "")[:-1]
    s = "%s: %s" % (e["name"], bare + taken_text(tk) + whole[len(bare):])
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
            if e["label"] == n + ":0" and (e["text"].startswith(REFL) or e["text"].startswith(ENVL) or e["text"].startswith(PARL)):
                head_lines.append(e["text"])      # a declaration of the Logic pragmas, after the lexicon lines
            else:
                head_lines.append(("prose", e["text"]))
            continue
        if e["kind"] == "kept":
            s = e["text"]
        elif e["kind"] == "cite":
            s = "%s: %s%s%s" % (e["name"], e["target"], " " + e["tname"] if e["tname"] else "", taken_text(e.get("taken")))
            if e["body"]:
                s += ", only if " + fold_states(e["body"], lex, ACTOR)
            if e["guards"]:
                s += ", with the consent of " + join(guard_text(g, ir) for g in e["guards"])
            if e.get("then"):
                s += ", and then " + effects_text(e["then"], lex)
            if e.get("votes"):
                s += ", on the votes of %s for it" % votes_text(e["votes"])
            s += "."
        else:
            s = act_sentence(e, lex, acts, ir)
            if s is None:
                raise Err("%s %s: no act phrase has these effects" % (e["label"], e["name"]))
            if e.get("votes"):
                s = s[:-1] + ", on the votes of %s for it." % votes_text(e["votes"])
        num = int(e["label"].split(":")[1])
        if num not in by:
            by[num] = [e["mark"], []]
            body.append(num)
        by[num][1].append(s)
    # the contract: its prose and names lines, its clauses; then its Logic pragmas, the lexicon lines
    out += [l[1] for l in head_lines if not isinstance(l, str)]
    for num in body:
        line = " ".join(by[num][1])
        for x, y in w.contract(n).params.items():       # a lower-case parameter written as the contract writes it
            line = re.sub(r"(?<![A-Za-z'*-])%s(?![A-Za-z'-])" % y, x, line)
        out.append("%d. %s %s" % (num, by[num][0], line))
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
            out.append((e["label"], e["mark"], e["name"], e["target"], e["tname"], tuple(sorted(e["body"])), tuple(sorted(e["guards"])), tuple(sorted(e.get("then", [])))) +
                       ((tuple(sorted((e.get("taken") or {}).items())), tuple(e.get("votes") or ())) if e.get("taken") or e.get("votes") else ()))
        else:
            out.append((e["label"], e["mark"], e["name"], tuple(sorted(e["head"])), tuple(sorted(e["body"])), tuple(sorted(e["guards"])), tuple(e.get("same") or ()), tuple(sorted(e.get("must") or ()))) +
                       ((tuple(e["votes"]),) if e.get("votes") else ()))
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
                w2.c, w2.lex, w2.party = dict(w.c), dict(w.lex), dict(w.party)
                w2.sasx = {k: v for k, v in w.sasx.items() if k != n}
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
