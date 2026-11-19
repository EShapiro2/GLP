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
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NAMES_      # the names of the contracts and their files (tools/names.py, NAMES.txt)
import nameslines          # the one names line of a full contract (tools/nameslines.py)

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
NAME = r"[A-Z][A-Za-z]*"
PH = re.compile(r"^([b-z])('s|-[A-Za-z]+)?$")
MARKS = "■◆○"
PRAGMAS = "Logic pragmas."
CLAUSE = re.compile(r"^(\d+)\. ([%s]) (.*)$" % MARKS)
ENTRY = re.compile(r"^((?:%s)(?:\+%s)*) ([%s-]) (.*)$" % (NAMES_.REF, NAMES_.REF, MARKS))      # a label: names and numbers, "Notes:3+Coins:1", a name of one or two words
REF = NAMES_.REF
PRON = {"Alice": "her", "Alicia": "her", "Carol": "her", "Erin": "her", "Wendy": "her", "Bob": "his", "Dave": "his", "Frank": "his", "Will": "his"}
POSS = {"her": "hers", "his": "his", "its": "its", "their": "theirs"}
WILD = "_"        # in a place: someone; in a state that must not hold, or that ends: anyone
WILDW = ("someone", "anyone", "no one")
SIGNED = r"%s(?: signed by %s)+" % (NAME, NAME)   # a place holding a thing with its signatures: 'Item signed by Alice', in a schema Item@Alice
COUNTS = {"fewer than": "<", "more than": ">", "at most": "<=", "at least": ">="}   # 'fewer than D persons' in a place: <D
COUNTW = r"(?:%s) (?:[A-Z][A-Za-z]*|\d+) persons" % "|".join(COUNTS)
SIGNS = {v: k for k, v in COUNTS.items()}
POSS_IN = re.compile(r"the (\w+?)s of ((?:%s) (?:[A-Z][A-Za-z]*|\d+) persons)" % "|".join(COUNTS))   # 'the votes of at least V persons' is read as 'at least V persons's vote'
COUNT_ARE = re.compile(r"(%s) are " % COUNTW)   # 'fewer than Double persons are entitled to vote on m in z', a count as the holder, read with the template's 'is'
COUNT_IS = re.compile(r"(%s) is " % COUNTW)     # and printed so
POSS_OUT = re.compile(r"((?:%s) (?:[A-Z][A-Za-z]*|\d+) persons)'s (\w+)" % "|".join(COUNTS))      # and printed so (GO #6, Calculus #6, 2026-11-15 16:37 UTC)


def poss_in(l):
    return COUNT_ARE.sub(r"\1 is ", POSS_IN.sub(lambda m: "%s's %s" % (m.group(2), m.group(1)), l))


def poss_out(l):
    return COUNT_IS.sub(r"\1 are ", POSS_OUT.sub(lambda m: "the %ss of %s" % (m.group(2), m.group(1)), l))


def count_words(v):
    """A count as a place of a definition's right side reads it: <Double is 'fewer than Double persons', so that
    'fewer than Double persons is a director of z' reads as 'fewer than Double persons holds the role Director in z'."""
    m = re.fullmatch(r"(<=|>=|<|>)([A-Z][A-Za-z]*|\d+)", v)
    return "%s %s persons" % (SIGNS[m.group(1)], m.group(2)) if m else v


def count_code(v):
    m = re.fullmatch(r"(%s) (\w+) persons" % "|".join(COUNTS), v)
    return COUNTS[m.group(1)] + m.group(2) if m else v
NOUNS = set()     # 'the role r' in a basic state: 'no role' and 'some role' are written for 'the role no one', 'the role someone'
MARK = "*#"   # the last argument of an atom: *n, n copies; #n, exactly n copies and no more
DFLL = "Default: "   # 'Default: x is an officer of z for x is entitled to vote on m in z.': the state a parameter takes where no first line binds it (Udi, 2026-11-16, to Calculus #6)
PARL = "Parameters: "   # 'Parameters: x is entitled to propose m in z.': a state a later contract, the bylaws or the terms fill; a parameter of the term
ENVL = "Environment: "   # 'Environment: x is nearby y.': a state the platform gives, an environment predicate of the term; no act adds or ends it
KNDL = "Kind of note: "   # 'Kind of note: bond, y-bonds due Day for y-notes.': a kind of Notes's notes (Udi, 2026-11-16: special cases, not instances); see kind_forms
KINDL = re.compile(r"^Kind of note: ([a-z]+), (.+?) for ([b-z])-notes(; its acts keep their names)?\.$")
REFL = "Reflexive: "   # 'Reflexive: x knows y.': every agent holding the state's atoms holds x knows x; no act adds or ends it
ARITH = "Vocabulary: n is at most m = le(n,m); n is at least m = ge(n,m); n is more than m = gt(n,m); n is less than m = lt(n,m); n is m plus k = sum(n,m,k); n is m times k = product(n,m,k); n is the difference from m to k = difference(n,m,k); x is the author of t = author(x,t)."   # of every contract; the author of an item is its first signer (GFP), read by toglp.py as eq(author_of(t), x)
ACTOR = "Actor"   # in a citation: the first of those who take the act cited
IRREG = {"is": "be", "has": "have", "does": "do", "goes": "go"}
TAKEN = r", taken for %s by %s(?: and for %s by %s)*" % (NAME, NAME, NAME, NAME)   # a citation taking the act cited for a party: its guard NAME1 is NAME2
FORK = r", for [a-z]+(?:(?:, | and | or )[a-z]+)*"   # a citation for a special case: ', for bonds' restricts the form of the act for that kind of note
VOTES = r", on the votes of %s(?: and of %s)* for it" % (NAME, NAME)
# A relaxing citation (Udi, 2026-11-16, to Calculus #7): "N: C:n A, without the consent of P, without CONDITIONS, if
# CONDITIONS." adds the act N, the act A of C:n without the consents of P and the conditions named, with those after
# "if"; the act A stays.  At least one "without"; each part optional, in that order.
RELAX = r"(%s)(?: ([A-Z][A-Za-z' -]*?))?((?:, without the consent of .*?)?)((?:, without (?!the consent of ).*?)?)((?:, if .*?)?)\.$" % REF   # an act or a citation decided by the votes of organisations, GO's
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
        self.one = set()  # role predicates any one of whom decides: 'any guardian of z = any_guardian(z), any one listed by z has y as a guardian ...'
        self.dflt = {}    # a parameter -> the state it takes where no first line binds it: 'Default: x is an officer of z for x is entitled to vote on m in z.'
        self.votes = []   # the acts decided 'on the votes of NAME for it': GO's states of a motion, derived for each (vote_family)
        self.voted = {}   # the name of a sentence decided on votes -> the ids of its vote families

    def copy(self):
        l = Lex()
        l.basic, l.roles, l.defs, l.req, l.short = list(self.basic), list(self.roles), list(self.defs), dict(self.req), dict(self.short)
        l.rlist = dict(self.rlist)
        l.refl = list(self.refl)
        l.env = list(self.env)
        l.par = list(self.par)
        l.dflt = dict(self.dflt)
        l.one = set(self.one)
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
        for k, v in other.dflt.items():
            self.dflt.setdefault(k, v)
        self.one |= other.one
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
                inner = unfold(unwild(fill(pay[2], {k: count_words(v) for k, v in b.items()}), "anyone"), lex, actor=actor)
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
    lines = [poss_in(l.rstrip("\n")) for l in open(os.path.join(ROOT, d, NAMES_.file_of(name) + ".txt"), encoding="utf-8") if l.strip()]
    c.title = lines[0]
    c.params = params_of(lines, name)
    c.first = None
    c.own = Lex()
    c.header = []      # (kind, line): vocabulary, roles, definitions, prose and names, in order
    c.clauses = []     # (number, mark, text)
    c.kinds = []       # the kinds of note it declares: (word, noun, slot place, whether its acts keep their names)
    dfl = []           # the 'Default: ' lines, read once every 'Parameters: ' line is
    for i, l in enumerate(lines[1:]):
        m = CLAUSE.match(l)
        if m:
            c.clauses.append((int(m.group(1)), m.group(2), params_in(m.group(3), c.params)))
        elif l == PRAGMAS:
            continue
        elif i == 0 and re.match(re.escape(name) + r" (extends|restricts|strengthens|instantiates|composes|relaxes)\b", l):
            c.first = l
        elif KINDL.match(l):
            m = KINDL.match(l)
            c.kinds.append((m.group(1), m.group(2), m.group(3), bool(m.group(4))))
            c.header.append(("prose", l))      # a kept declaration, as 'Reflexive:'
        elif l.startswith(REFL):
            c.own.refl.extend(t for t in l[len(REFL):].rstrip(".").split("; ") if t not in c.own.refl)
            c.header.append(("prose", l))      # a kept declaration: once in the schemas, in the flat text and in every expansion
        elif l.startswith(ENVL):
            c.own.env.extend(t for t in params_in(l[len(ENVL):], c.params).rstrip(".").split("; ") if t not in c.own.env)
            c.header.append(("prose", l))      # a kept declaration: once in the schemas, in the flat text and in every expansion
        elif l.startswith(PARL):
            c.own.par.extend(t for t in params_in(l[len(PARL):], c.params).rstrip(".").split("; ") if t not in c.own.par)
            c.header.append(("prose", l))      # a kept declaration: once in the schemas, in the flat text and in every expansion
        elif l.startswith(DFLL):
            dfl.append(params_in(l[len(DFLL):], c.params).rstrip("."))
            c.header.append(("prose", l))      # a kept declaration, as 'Parameters:'
        elif l.startswith("Vocabulary: ") or l.startswith("Roles: "):
            kind, rest = l.split(": ", 1)
            for e in rest.rstrip(".").split("; "):
                t, a = e.rsplit(" = ", 1)
                by, one = None, False
                if ", any one listed by " in a:     # any one of the agents listed decides, and no other with them
                    a, by = a.split(", any one listed by ", 1)
                    one = True
                elif ", listed by " in a:
                    a, by = a.split(", listed by ", 1)
                _, p, args = parse_atom(a)
                if one:
                    c.own.one.add(p)
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
    c.dfl_later = []   # defaults of parameters a contract it is written from declares (CDC's of GO's), read with its lexicon
    for d in dfl:
        old = [t for t in c.own.par + [r[0] for r in c.own.roles] if d.endswith(" for " + t)]      # the parameter ends the line, which may itself say 'for' (take m for z); a role, as GnW's decision rule, is a parameter
        if not old:
            c.dfl_later.append(d)
            continue
        old = max(old, key=len)
        c.own.dflt[old] = d[:-len(" for " + old)]
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
    m = re.search(r"instantiates ((?:%s)(?: and (?:%s))*) with (.*?)(?=, (?:and )?(?:restricts|strengthens|extends|composes|relaxes)\b|$)" % (NAMES_.NAME_RX, NAMES_.NAME_RX), s)
    if m:
        items = m.group(2).split("; ") if ";" in m.group(2) else re.split(r", | and ", m.group(2))
        for p in m.group(1).split(" and "):
            subs[p] = [it for it in items if " for " in it]
        prose = [it for it in items if " for " not in it]
    parents, operands = [], s[len(c.name):m.start(2)] + s[m.end(2):] if m else s[len(c.name):]
    for _, _, nm in NAMES_.find(operands, known):          # the known names, longest first (tools/names.py)
        if nm not in parents and nm != c.name:
            parents.append(nm)
    cm = re.search(r"composes it with ((?:%s)(?: and (?:%s))*)" % (NAMES_.NAME_RX, NAMES_.NAME_RX), operands) if m else None
    if cm:          # the instance composed with a contract: its states put for states bind the contract composed too (SC's bylaws, Shares's issue of shares)
        states = [it for it in subs[m.group(1).split(" and ")[0]] if re.search(r"(?<![A-Za-z'_-])[b-z](?='s|-[A-Za-z]|[ ,.)]|$)", it)]
        for p in cm.group(1).split(" and "):
            if p in known and p not in subs:
                subs[p] = states
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
        b = {k: count_words(v) for k, v in b.items()}      # a count stays written in words: 'fewer than Double persons'
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


CASE_RX = re.compile(r", or (?=.* while )")


def role_cases(a):
    """A role put for a role by cases, 'R1 while STATES1, or R2 while STATES2' (SC's who decides the issue of shares):
    [(role, states)], or None for a role put as it is."""
    if " while " not in a:
        return None
    out = []
    for part in CASE_RX.split(a):
        r, _, st = part.partition(" while ")
        out.append((r.strip(), st.strip()))
    return out


def case_name(e, pred, k):
    """The name of the act of the k-th case of a role put by cases: the act's for the first, else the act's 'by' the case's
    role, 'Issue shares by shareholders'; a ward's form keeps its suffix last, 'Issue shares by shareholders for Alice'."""
    if k == 0:
        return {}
    sfx = " by " + pred.replace("_", " ")
    out = {}
    for f in ("name", "oname"):
        if f in e:
            v, ps = e[f], e.get("sfx", "")
            out[f] = (v[:-len(ps)] + sfx + ps) if ps and v.endswith(ps) else v + sfx
    if e.get("names"):
        out["names"] = {lb: x + sfx for lb, x in e["names"].items()}
    for f in ("pid", "party"):
        if e.get(f):
            out[f] = tuple(e[f]) + (("case", k),)
    return out


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
                    pl.dflt = {ft(k): ft(v) for k, v in pl.dflt.items()}
                    pl.one = {fp(r) for r in pl.one}
                    pl.votes = [dict(f, t=ft(f["t"]), P=ft(f["P"]), st={k: dict(v, go=fp(v["go"]), pred=fp(v["pred"]), tpl=ft(v["tpl"])) for k, v in f["st"].items()})
                                for f in pl.votes]
            lex.add(pl)
        lex.add(arith())
        for d in c.dfl_later:      # a default of n for a parameter of a contract n is written from: n's, in place of that contract's
            old = [t for t in lex.par + [r[0] for r in lex.roles] if d.endswith(" for " + t)]
            if not old:
                raise Err("'%s%s.' gives a default for no parameter this contract or one it is written from declares" % (DFLL, d))
            old = max(old, key=len)
            lex.dflt[old] = d[:-len(" for " + old)]
        kind_phrases(self, n, lex)
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
        entries = kind_forms(self, n, entries, lex=lex)          # the forms of the kinds n adds, which no contract it is written from has
        done = {e["party"] for e in entries if e.get("party")}
        go = any(e[0] == GO_TAKE for e in lex.basic)
        role = [r for r in lex.roles if r[0] == WARD_ROLE][0]
        out, seen = [], set()
        for e in entries:
            if e["kind"] != "act" or e.get("party") or e.get("voteact") or e.get("taken") or e.get("generic"):
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
            if not go and e.get("kinds") and any(split_for(it, self.lexicon(q))[2] in GO_STATES.values() for it, q in self.binds(n)):
                NO_WARD.setdefault(n, []).append(e["name"])      # a form of kinds first met here (SC's Swap coin for share): open, reported
                continue
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


# ---- pronouns and possessives, as a person writes them ------------------------------
# A pronoun stands for the one person the sentence has already named whom it can name: she, her, hers, herself for a name
# of PRON with "her"; he, him, his, himself for one with "his"; it, its, itself, they, them, their, theirs, themselves for
# any other name (an organisation, an AI agent, a service: GLP organisation/writing-style-guide.md, rule 28).  Two such
# persons named before it, or none, and the sentence is refused.  It is read in what follows the act phrase of an act or
# the cited act of a citation (", provided", ", only if", ", with the consent of", ", and then"): in the act phrase "her"
# is already the subject's own (SAS-WRITING.txt).  A possessive state is read by the vocabulary's own phrases:
# "Y is X's N" ("Y is her N") is "X has Y as a N" where the contract has that phrase, else "Y is a N of X" where it has
# that one, and "Y is not X's N" their negations; a phrase the vocabulary has as written ("y is x's delegate") stays.
PRONOUNS = {"she": "her", "her": "her", "hers": "her", "herself": "her", "he": "his", "him": "his", "his": "his", "himself": "his",
            "it": "its", "its": "its", "itself": "its", "they": "its", "them": "its", "their": "its", "theirs": "its", "themselves": "its"}
POSSESSIVE = {"her", "his", "its", "their"}
OBJECT_NEXT = {"and", "or", "as", "to", "in", "from", "for", "with", "on", "by", "of", "at", "is", "are", "has", "have", "does", "do", "may", "must", "will", "than", "but"}
TAIL = re.compile(r", (?:provided|only if|with the consent of|and then) ")


def pron_class(name):
    return PRON.get(name, "its")


def vocab_nouns(lex):
    """{N: (form, article)} of the vocabulary's possessive shapes: form 'has' for 'x has y as a N', 'of' for 'x is a N of y'."""
    out = {}
    for t, _, _ in lex.basic + lex.roles:
        m = re.fullmatch(r"([b-z]) has ([b-z]) as (an?) ([a-z][a-z ]*?)", t)
        if m:
            out.setdefault(m.group(4), ("has", m.group(3)))
    for t, _, _ in lex.basic + lex.roles:
        m = re.fullmatch(r"([b-z]) is (an?) ([a-z][a-z ]*?) of ([b-z])", t)
        if m and m.group(3) not in out:
            out[m.group(3)] = ("of", m.group(2))
    return out


AGENT_KIND = re.compile(r"\b(person|party|ward|child|AI agent|service|issuer|organisation|bank|cooperative|company|corporation|society|DAO|rider|driver)\b")


def natural(s, lex, agents=frozenset()):
    """The clause sentence s with its pronouns and possessive states in the plain form the grammar reads; agents, the
    names a pronoun may stand for: the contract's names of persons, wards, organisations and other agents."""
    m = re.match(r"([A-Z][A-Za-z' -]*?): (.+)$", s)
    t = m and TAIL.search(m.group(2))
    if not t:
        return s
    k = m.start(2) + t.start()
    head, tail = s[:k], s[k:]
    nouns = vocab_nouns(lex)
    out, pos = [], 0
    for mm in re.finditer(r"\b(%s)\b" % "|".join(sorted(PRONOUNS, key=len, reverse=True)), tail):
        w = mm.group(1)
        before = head + "".join(out) + tail[pos:mm.start()]
        named = list(dict.fromkeys(x for x in re.findall(NAME, before[len(m.group(1)) + 2:]) if x in agents))
        who = [x for x in named if pron_class(x) == PRONOUNS[w]]
        if len(who) != 1:
            raise Err("the pronoun '%s' names %s" % (w, "both " + " and ".join(who) if who else "no person the sentence has named"))
        rest = tail[mm.end():]
        nx = re.match(r" ([A-Za-z]+)", rest)
        if w in POSSESSIVE and nx and nx.group(1) not in OBJECT_NEXT:      # "her guardian", "her Delos": the possessive; "her as", "her and": the object
            rep = who[0] + "'s"
        elif w in ("hers", "theirs") or (w == "his" and not nx):
            rep = who[0]
        else:
            rep = who[0]
        out.append(tail[pos:mm.start()] + rep)
        pos = mm.end()
    tail = "".join(out) + tail[pos:]

    def poss(mm):
        y, neg, x, N = mm.group(1), mm.group(2), mm.group(3), mm.group(4)
        if N not in nouns:
            return mm.group(0)
        form, art = nouns[N]
        if form == "has":
            return "%s %s %s as %s %s" % (x, "does not have" if neg else "has", y, art, N)
        return "%s is %s%s %s of %s" % (y, "not " if neg else "", art, N, x)
    pat = r"\b(%s) is (not )?(%s)'s (%s)\b" % (NAME, NAME, "|".join(sorted(map(re.escape, nouns), key=len, reverse=True)) or "(?!)")
    tail = re.sub(pat, poss, tail)
    return head + tail


def compile_contract(w, n):
    """gr/n -> the entries of sas/n and the entries of its own flat text."""
    c, lex = w.contract(n), w.lexicon(n)
    agents = frozenset(x for x, ks in kinds_from(w, n, n).items() if any(AGENT_KIND.search(k) for k in ks)) | frozenset(PRON)
    sas, own = [], []
    for kind, l in c.header:
        if kind == "prose":
            sas.append(dict(kind="kept", label="%s:0" % n, mark="-", text=l))
            own.append(dict(kind="kept", label="%s:0" % n, mark="-", text=l))
    for num, mark, text in c.clauses:
        label = "%s:%d" % (n, num)
        def one(s):
            """The entries of sas/ and of the flat text of one sentence s, as written."""
            sas_n, own_n = [], []
            m = re.match(r"([A-Z][A-Za-z' -]*?): (.+)$", s)
            cm = m and re.match(r"(%s)(?: ([A-Z][A-Za-z' -]*?))?((?:%s)?)((?:%s)?)((?:, only if .*?)?)((?:, with the consent of .*?)?)((?:, decided by .*? in place of .*?)?)((?:, and then .*?)?)((?:%s)?)\.$"
                                % (REF, FORK, TAKEN, VOTES), m.group(2))
            vm = m and not cm and re.search(VOTES + r"\.$", m.group(2))
            rx = m and not cm and re.match(RELAX, m.group(2))
            rx = rx if rx and (rx.group(3) or rx.group(4)) else None
            try:
                if rx:
                    dcons = rx.group(3)[len(", without the consent of "):] if rx.group(3) else ""
                    dconds = rx.group(4)[len(", without "):] if rx.group(4) else ""
                    ifc = rx.group(5)[len(", if "):] if rx.group(5) else ""
                    parts = [x for x in re.split(r", | and ", ifc) if x] if ifc else []
                    eqs = [tuple(x.split(" is ")) for x in parts if re.fullmatch(r"%s is %s" % (NAME, NAME), x)]      # "if Carol is Bob": one agent in both
                    ifc = ", ".join(x for x in parts if not re.fullmatch(r"%s is %s" % (NAME, NAME), x))
                    e = dict(kind="cite", relax=True, eqs=eqs, label=label, mark=mark, name=m.group(1), target=rx.group(1), tname=rx.group(2),
                             body=lits_of(ifc, lex, ACTOR), guards=[], then=[], taken={}, votes=None,
                             drop_guards=[g for _, g in persons_of(dcons, lex)] if dcons else [], drop_body=lits_of(dconds, lex, ACTOR))
                    sas_n.append(e)
                    own_n.append(dict(e, conds=phrases_of(ifc, lex, ACTOR), persons=[], effects=[],
                                      drop_persons=[p for p, _ in persons_of(dcons, lex)] if dcons else [], drop_conds=phrases_of(dconds, lex, ACTOR)))
                elif cm:
                    conds = cm.group(5)[len(", only if "):] if cm.group(5) else ""
                    cons = cm.group(6)[len(", with the consent of "):] if cm.group(6) else ""
                    then = cm.group(8)[len(", and then "):] if cm.group(8) else ""
                    e = dict(kind="cite", label=label, mark=mark, name=m.group(1), target=cm.group(1), tname=cm.group(2),
                             body=lits_of(conds, lex, ACTOR), guards=[g for _, g in persons_of(cons, lex)] if cons else [],
                             then=lits_of(then, lex, ACTOR), taken=taken_of(cm.group(4)), votes=votes_of(cm.group(9)) if cm.group(9) else None)
                    if cm.group(7):          # ', decided by ROLE in place of ROLE2': the role that decides the act cited, put for the one it had
                        dn, _, dold = cm.group(7)[len(", decided by "):].partition(" in place of ")
                        e["decided"] = (persons_of(dn, lex)[0][1], persons_of(dold, lex)[0][1])
                    if cm.group(3):
                        e["kinds_for"] = re.split(r", | and | or ", cm.group(3)[len(", for "):])
                    theirs = {q for _, q, _ in w.lexicon(cm.group(1).split(":")[0]).basic}
                    if any(q in theirs for _, q, _ in e["then"]):
                        raise Err("a strengthening adds a state of the contract it strengthens")
                    sas_n.append(e)
                    own_n.append(dict(e, conds=phrases_of(conds, lex, ACTOR), persons=[p for p, _ in persons_of(cons, lex)] if cons else [],
                                    effects=phrases_of(then, lex, ACTOR)))
                    if cm.group(7):
                        own_n[-1]["decided"] = (persons_of(dn, lex)[0][0], persons_of(dold, lex)[0][0])
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
                    sas_n.append(dict(kind="act", label=label, mark=mark, name=m.group(1), head=head, body=body,
                                    guards=list(dict.fromkeys(g for _, g in persons + cons))))
                    same = same_of(c, num, names_of(sas_n[-1]))
                    sas_n[-1]["same"] = same
                    sas_n[-1]["must"] = [g for _, g in musts]
                    sas_n[-1]["votes"] = votes_of(vm.group(0)) if vm else None
                    if not set(sas_n[-1]["must"]) <= set(sas_n[-1]["guards"]):      # one who must consent is one of those who take the act
                        raise Err("%s: one who must consent is not among its guards: %s" % (m.group(1), ", ".join(sorted(set(sas_n[-1]["must"]) - set(sas_n[-1]["guards"])))))
                    own_n.append(dict(kind="act", label=label, mark=mark, name=m.group(1), same=same, must=[p for p, _ in musts], persons=list(dict.fromkeys(p for p, _ in persons + cons)), votes=sas_n[-1]["votes"],
                                    effects=[(x[0], wild(fill(x[1], x[2]), x[0])) for x in eff], conds=[(x[0], wild(fill(x[1], x[2]), x[0])) for x in cnd]))
                else:
                    sas_n.append(dict(kind="kept", label=label, mark=mark, text=s))
                    own_n.append(dict(kind="kept", label=label, mark=mark, text=s))
            except Err as ex:
                raise Err("%s, %s: %s" % (label, s[:40], ex))
            return sas_n, own_n
        for s in sentences(text):
            try:
                a, b = one(s)
            except Err:
                try:
                    s2 = natural(s, lex, agents)          # pronouns and possessives, as a person writes them (SAS-WRITING.txt)
                except Err as ex:
                    raise Err("%s, %s: %s" % (label, s[:40], ex))
                if s2 == s:
                    raise
                a, b = one(s2)
            sas.extend(a)
            own.extend(b)
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
        (", for " + join(e["kinds_for"]) if e.get("kinds_for") else "") + (", decided by %s in place of %s" % e["decided"] if e.get("decided") else "") + taken_text(e.get("taken")) + (", on the votes of " + votes_text(e["votes"]) if e.get("votes") else "") + \
        (" + " + ", ".join(lit_s(l) for l in e["then"]) if e.get("then") else "")
    if e.get("relax"):          # a relaxing citation: what it drops after "relaxed;", what it adds as its body
        head += " relaxed" + ("; one " + ", ".join("%s=%s" % q for q in e["eqs"]) if e.get("eqs") else "") + ("; without the consent of " + ", ".join(e["drop_guards"]) if e.get("drop_guards") else "") + \
            ("; without " + ", ".join(lit_s(l) for l in e["drop_body"]) if e.get("drop_body") else "")
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
    rx = re.fullmatch(r"(%s)(?: ([^+,;]+?))? relaxed(?:; one ([^;]+))?(?:; without the consent of ([^;]+))?(?:; without (.+))?" % REF, head.strip())
    if rx:
        return dict(kind="cite", relax=True, label=label, mark=mark, name=name, target=rx.group(1), tname=rx.group(2), body=body, guards=guards,
                    then=[], taken={}, votes=None, eqs=[tuple(q.split("=")) for q in rx.group(3).split(", ")] if rx.group(3) else [],
                    drop_guards=rx.group(4).split(", ") if rx.group(4) else [],
                    drop_body=[parse_atom(a) for a in split_top(rx.group(5))] if rx.group(5) else [])
    dm = re.search(r", decided by (\S+\)) in place of (\S+\))", head)
    if dm:
        head = head[:dm.start()] + head[dm.end():]
    cm = re.fullmatch(r"(%s)(?: ([^+,]+?))?((?:%s)?)((?:%s)?)(?:, on the votes of (%s(?: and of %s)*))?(?: \+ (.+))?" % (REF, FORK, TAKEN, NAME, NAME), head.strip())
    if cm and dm:
        e = read_schema_line(l.replace(dm.group(0), "", 1))
        e["decided"] = (dm.group(1), dm.group(2))
        return e
    if cm:
        e = dict(kind="cite", label=label, mark=mark, name=name, target=cm.group(1), tname=cm.group(2), body=body, guards=guards,
                 then=[parse_atom(a) for a in split_top(cm.group(6))] if cm.group(6) else [], taken=taken_of(cm.group(4)), votes=votes_of(cm.group(5)) if cm.group(5) else None)
        if cm.group(3):
            e["kinds_for"] = re.split(r", | and | or ", cm.group(3)[len(", for "):])
        return e
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
                cs = role_cases(a) if kind == "role" else None
                new = ", ".join(lit_s(l) for l in pattern(a, w.lexicon(n))) if kind == "state" else \
                    " or ".join("%s while %s" % (role_pattern(r, w.lexicon(n)), ", ".join(lit_s(l) for l in pattern(st, w.lexicon(n)))) for r, st in cs) if cs else role_pattern(a, w.lexicon(n))
                s = s.replace(item, "%s for %s" % (new, old))
    return s


def write_sas(w, n, sas):
    c = w.contract(n)
    out = [c.title] + ([sas_first(w, n)] if c.first else []) + [l for k, l in c.header if k == "lex"] + [schema_line(e) for e in sas]
    put("sas", n, out)


def put(d, n, lines):
    os.makedirs(os.path.join(ROOT, d), exist_ok=True)
    with open(os.path.join(ROOT, d, NAMES_.file_of(n) + ".txt"), "w", encoding="utf-8") as f:
        f.write("\n".join(poss_out(l) for l in lines) + "\n")


def get(d, n):
    return [poss_in(l.rstrip("\n")) for l in open(os.path.join(ROOT, d, NAMES_.file_of(n) + ".txt"), encoding="utf-8") if l.strip()]


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


def own_for(w, n, p):
    """The names n puts for names of p, its own: where n instantiates p with a kind for a kind ('an organisation for a
    ward'), the first name p's chain gives the kind replaced (Guardians' Wendy) is the name n's own names line gives the
    kind put, the last if it gives two (Organisation's Gov and Orga: Orga), and that no contract n is written from
    names (Children's Wendy and Will are Warded Groups', so Children keeps them) (Udi, 2026-11-17, to Calculus #8)."""
    memo = w.__dict__.setdefault("_own_for", {})
    if (n, p) in memo:
        return memo[(n, p)]
    c = w.contract(n)
    parents, subs, _ = first_line(c, w.known)
    mp = {}
    if p in parents:
        pk = kinds_from(w, p)
        own = kinds_line(c)
        for item in subs.get(p, []):
            if split_for(item, w.lexicon(p)) is not None or " for " not in item:
                continue
            new, old = (re.sub(r"^(?:an?|the) ", "", x.strip()) for x in item.split(" for ", 1))
            olds = [x for x, ks in pk.items() if old in ks and x not in mp]
            mine = [x for x, ks in own.items() if new in ks and x not in pk]
            if olds and mine:
                mp[olds[0]] = mine[-1]
    memo[(n, p)] = mp
    return mp


KEEP_KEYS = ("label", "name", "oname", "names", "tname", "target", "of", "kind", "mark", "kindof")


def rename_names(x, mp, key=None):
    """x with each name of mp put for, whole words; the labels and the names of acts as they are."""
    if not mp or key in KEEP_KEYS:
        return x
    if isinstance(x, str):
        return map_text(x, mp)
    if isinstance(x, dict):
        return {rename_names(k, mp): rename_names(v, mp, k) for k, v in x.items()}
    if isinstance(x, list):
        return [rename_names(v, mp) for v in x]
    if isinstance(x, tuple):
        return tuple(rename_names(v, mp) for v in x)
    return x


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
        mp = own_for(w, n, p)
        for x, ks in kinds_from(w, p, origin).items():
            x = mp.get(x, x)
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
    first = lines[1] if len(lines) > 1 and re.match(re.escape(n) + r" (extends|restricts|strengthens|instantiates|composes|relaxes)\b", lines[1]) else None
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
        if "(" in old and " while " in new:          # a role put by cases: each act it guards, one act for each case
            _, pred, args = parse_atom(old)
            cases = []
            for part in new.split(" or "):
                r, _, st = part.partition(" while ")
                cases.append((parse_atom(r.strip()), [parse_atom(a) for a in split_top(st)]))
            out = []
            for e in entries:
                hits = [g for g in e.get("guards", []) + e.get("must", []) if "(" in g and parse_atom(g)[1] == pred] if e["kind"] == "act" else []
                if not hits:
                    out.append(e)
                    continue
                ga = [x for x in parse_atom(hits[0])[2] if not x.startswith("-")]
                ex = [x for x in parse_atom(hits[0])[2] if x.startswith("-")]      # those other than whom: kept (GO:4's End officer)
                b = dict(zip(args, ga))
                for k, ((_, rp, ra), lits) in enumerate(cases):
                    rg = "%s(%s)" % (rp, ",".join([b.get(x, x) for x in ra] + ex))
                    sub = lambda gs: [rg if "(" in g and parse_atom(g)[1] == pred else g for g in gs]
                    f = dict(e, guards=sub(e["guards"]), must=sub(e.get("must", [])),
                             body=e["body"] + [(x, q, tuple(b.get(a, a) for a in aa)) for x, q, aa in lits])
                    if e.get("taken"):
                        f["taken"] = dict(zip(e["taken"], sub(list(e["taken"].values()))))
                    f.update(case_name(e, rp, k))
                    out.append(f)
            entries = out
        elif "(" in old:
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
                    if e.get("taken") and any("(" in v and parse_atom(v)[1] == pred for v in e["taken"].values()):      # an act taken for a ward by a role put for a role
                        tk = {}
                        for kk, v in e["taken"].items():
                            if "(" in v and parse_atom(v)[1] == pred:
                                va = parse_atom(v)[2]
                                bb = dict(zip(args, [x for x in va if not x.startswith("-")]))
                                v = "%s(%s)" % (newl[0][1], ",".join([bb.get(a, a) for a in newl[0][2]] + [x for x in va if x.startswith("-")]))
                            tk[kk] = v
                        e["taken"] = tk
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
    return (e["label"].split("+")[0], e.get("oname", e.get("name")), e.get("text"), e.get("kinds"))


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
    acts = [e for e in entries if e["kind"] == "act" and not e.get("generic")]
    for e in acts:
        if e.get("party") or "+" not in e["label"] or sum(1 for f in acts if f["name"] == e["name"]) < 2:
            continue
        used = {f["name"] for f in acts}
        alt = next((m for m in e.get("names", {}).values() if m not in used), None)
        if alt:
            e["name"] = alt
    for i, e in enumerate(entries):          # the form of an act of notes, of the kind that keeps the names, beside an act of that name: Joint's Deposit coin
        if e["kind"] == "act" and e.get("kinds") and not e.get("party") and any(f["name"] == e["name"] and not f.get("kinds") for f in acts):
            ks = list(e["kinds"])
            entries[i] = dict(e, name="%s %s" % (e["name"], ks[0] if len(ks) == 1 else plural(ks[0]) if len(set(ks)) == 1 else " for ".join(ks)))      # a copy: the entry is shared with the contracts written from the same one
    acts = [e for e in entries if e["kind"] == "act" and not e.get("generic")]
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
    hit = [i for i, e in enumerate(entries) if e["kind"] == "act" and not e.get("voteact") and target in e["label"].split("+") and (tname is None or named(e) and not e.get("generic"))]
    if not hit:
        gone = [v for (c, l, nm), v in REMOVED.items() if target in l.split("+") and (tname is None or nm == tname)]
        if gone:
            raise Err("%s: the act %s%s was removed as redundant; %s %s took its place" % (where, target, " " + tname if tname else "", gone[0][0], gone[0][1]))
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
    return [i for i in hit if not entries[i].get("party") and not entries[i].get("generic")]


def renamed(o, name):
    """An act a citation of one act renames; its ward's forms are renamed with it (unclash)."""
    if not o.get("party"):
        o["oname"], o["name"] = o["name"], name


def own_roles(e, rs, lex, text):
    """{R: the role that decides the act e for R} where each ward R has one among e's guards (persons), as Shares:1's those
    who decide the issue of shares of Alice; None where one has none."""
    out = {}
    for r in rs:
        if text:
            hit = []
            for x in e["persons"]:
                if re.fullmatch(NAME, x):
                    continue
                try:
                    g = role_pattern(x, lex)
                except (Err, IndexError):
                    continue
                if "(" in g and parse_atom(g)[2][:1] == (r,):
                    hit.append(x)
        else:
            hit = [g for g in e["guards"] if "(" in g and parse_atom(g)[2][:1] == (r,)]
        if not hit:
            return None
        out[r] = hit[0]
    return out


def derive_party(w, n, lex, entries, text=False):
    """The ward's forms of the acts of parties that n derives (World.party_specs), each after its act, with its vote acts."""
    for sp in w.party.get(n, []):
        i = next(i for i, x in enumerate(entries) if x["kind"] == "act" and not x.get("party") and (x.get("pid") or key(x)) == sp["pid"])
        entries[i] = dict(entries[i], pid=sp["pid"])
        o = dict(entries[i])
        own = own_roles(o, list(sp["taken"]), lex, text)
        if own and sp["votes"]:         # an act a role of the ward decides: taken for the ward by its officer, the role's consent beside it, with no vote (Calculus #8, 2026-11-16: Orga's issue of shares takes no vote of the directors, and is determined as the act is)
            off = [(t, q, a) for t, q, a in lex.basic if q == "officer" and len(a) == 2]      # GO's "y is an officer of x"
            for r, tk in sp["taken"].items():
                if off:
                    t, q, a = off[0]
                    if text:
                        o["conds"] = o["conds"] + [(False, fill(t, {a[0]: sp["ttext"][r], a[1]: r}))]
                    else:
                        o["body"] = o["body"] + [(False, q, (tk, r))]
            sp = dict(sp, votes=None)
        if text:
            gen = gen_votes_text(lex, o, sp["votes"], sp["name"], o["label"], o["mark"], sp["takers"], sp["agents"]) if sp["votes"] else None
            ib, _ = inv_basic(lex)
            o["conds"] = o["conds"] + [(False, phrase_of(l, ib)) for l in sp["knows"]]
            o["taken"] = dict(sp["ttext"])
            o["persons"] = list(dict.fromkeys(o["taken"].get(g, g) for g in o["persons"]))
            if gen:
                o["conds"], o["effects"] = o["conds"] + gen[0], o["effects"] + gen[1]
        else:
            gen = gen_votes(lex, o, sp["votes"], sp["name"], o["label"], o["mark"], sp["takers"], sp["agents"]) if sp["votes"] else None
            o["body"] = o["body"] + sp["knows"]
            o["taken"] = dict(sp["taken"])
            o["guards"] = list(dict.fromkeys(o["taken"].get(g, g) for g in o["guards"]))
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


def relax_hit(entries, e):
    """The one act a relaxing citation names (its ward's forms are not relaxed with it)."""
    hit = own_acts(entries, cited(entries, e["target"], e["tname"], e["label"]))
    if len(hit) != 1:
        raise Err("%s: a relaxing citation names one act, and %s has %d" % (e["label"], e["target"], len(hit)))
    return hit[0]


def relax_schemas(w, n, e, entries):
    """The act a relaxing citation adds, after the act it cites, which stays: the act cited without the consents and the
    conditions named, with the conditions after "if".  A condition dropped is a condition of the act, not an atom its
    head ends; a consent dropped is one of its persons."""
    i = relax_hit(entries, e)
    o = dict(entries[i])
    mp = cite_map(w, n, e, o)
    mapl = lambda l: (l[0], l[1], tuple(mp.get(a, a) for a in l[2]))
    ended = {(l[1], l[2]) for l in o["head"] if l[0]}
    for l in map(mapl, e.get("drop_body", [])):
        if l not in o["body"]:
            raise Err("%s: %s is no condition of %s" % (e["label"], lit_s(l), o["name"]))
        if not l[0] and (l[1], l[2]) in ended:
            raise Err("%s: %s is an atom %s ends, not a condition that may be dropped" % (e["label"], lit_s(l), o["name"]))
    drop = set(map(mapl, e.get("drop_body", [])))
    dg = [map_guard(g, mp) for g in e.get("drop_guards", [])]
    for g in dg:
        if g not in o["guards"] and g not in o.get("must", []):
            raise Err("%s: %s is no person of %s" % (e["label"], g, o["name"]))
    o["body"] = [l for l in o["body"] if l not in drop] + [l for l in map(mapl, e["body"]) if l not in o["body"]]
    o["guards"] = [g for g in o["guards"] if g not in dg]
    for a, b in e.get("eqs", []):       # one agent in both: the first name written as the second
        a, b = mp.get(a, a), mp.get(b, b)
        sub = lambda l: (l[0], l[1], tuple(re.sub(r"(?<![A-Za-z])%s(?![a-z])" % a, b, x) for x in l[2]))
        rp = w.lexicon(n).refl_preds()          # knows(Bob,Bob) of one agent: holds, and is no effect or condition
        o["head"] = [l for l in dict.fromkeys(map(sub, o["head"])) if not self_atom(l[1], l[2], rp)]
        o["body"] = [l for l in dict.fromkeys(map(sub, o["body"])) if not self_atom(l[1], l[2], rp)]
        o["guards"] = list(dict.fromkeys(b if g == a else g for g in o["guards"]))
        o["same"] = [p for p in o.get("same", []) if a not in p.split("=")]
    o["must"] = [g for g in o.get("must", []) if g not in dg]
    o["label"], o["mark"] = o["label"] + "+" + e["label"], e["mark"]
    o["name"] = o["oname"] = e["name"]
    o["names"] = {e["label"]: e["name"]}
    o.pop("pid", None)
    entries.insert(after(entries, i), dict(o, of=key(entries[i])))


def relax_text(w, n, e, entries):
    """relax_schemas on the flat text: the phrases of the conditions and the persons."""
    i = relax_hit(entries, e)
    o = dict(entries[i])
    mp = cite_map(w, n, e, o, True)
    drop = [(x, map_text(t, mp)) for x, t in e.get("drop_conds", [])]
    for c in drop:
        if c not in o["conds"]:
            raise Err("%s: '%s%s' is no condition of %s" % (e["label"], "not " if c[0] else "", c[1], o["name"]))
    dp = [map_text(g, mp) for g in e.get("drop_persons", [])]
    o["conds"] = [c for c in o["conds"] if c not in drop] + [c for c in ((x, map_text(t, mp)) for x, t in e["conds"]) if c not in o["conds"]]
    o["persons"] = [g for g in o["persons"] if g not in dp]
    for a, b in e.get("eqs", []):
        a, b = map_text(a, mp), map_text(b, mp)
        sub = lambda t: re.sub(r"(?<![A-Za-z])%s(?![a-z])" % a, b, t)
        refl = refl_phrase(w.lexicon(n))
        o["effects"] = [p for p in dict.fromkeys((x, sub(t)) for x, t in o["effects"]) if not refl(p[1])]
        o["conds"] = [p for p in dict.fromkeys((x, sub(t)) for x, t in o["conds"]) if not refl(p[1])]
        o["persons"] = list(dict.fromkeys(sub(g) for g in o["persons"]))
        o["same"] = [p for p in o.get("same", []) if a not in p.split("=")]
    o["must"] = [g for g in o.get("must", []) if g not in dp]
    o["label"], o["mark"] = o["label"] + "+" + e["label"], e["mark"]
    o["name"] = o["oname"] = e["name"]
    o["names"] = {e["label"]: e["name"]}
    o.pop("pid", None)
    entries.insert(after(entries, i), dict(o, of=key(entries[i])))


def refl_phrase(lex):
    """A test: the phrase is a reflexive state of one agent of itself, 'Bob knows Bob'."""
    rxs = [re.compile(tpl_rx(t)) for t in lex.refl]
    def test(ph):
        for r in rxs:
            m = r.fullmatch(ph)
            if m and len(set(m.groupdict().values())) == 1:
                return True
        return False
    return test


NO_WARD = {}        # contract -> the forms of kinds of note first met in it, which binds GO's states of a motion, and so have no ward's form (party_specs)


REMOVE = False      # the removal of redundant acts (Udi, 2026-11-16), on for the cases of tests/ whose directory has a file 'remove', and with --remove; off for the 62 until Calculus's word
REMOVED = {}        # (contract, label, name) of each act removed: the act that took its place, (label, name)


def remove_redundant(w, n, entries):
    """The acts of n's expanded schemas that another act of them makes redundant (tools/redundant.py), removed: of two
    acts equal both ways, the one of n's own clauses stays, else the first.  Each removal is kept in REMOVED, for the
    line naming it and for a later citation of the act removed."""
    import redundant
    acts = [e for e in entries if e["kind"] == "act"]
    red, both = redundant.redundant_pairs(n, w, acts)
    gone = {}
    own = lambda e: e["label"].split("+")[-1].split(":")[0] == n
    for x, y in both:
        drop, keep = (x, y) if own(y) and not own(x) else (y, x)
        gone.setdefault(id(drop), (drop, keep))
    for x, y, _, _ in red:
        gone.setdefault(id(x), (x, y))
    for d, k in gone.values():
        REMOVED[(n, d["label"], d["name"])] = (k["label"], k["name"])
    return [e for e in entries if id(e) not in gone]


def expand_schemas(w, n):
    if n in w.sasx:
        return w.sasx[n]
    _, first, parents, subs, own = read_sas(w, n)
    lex = w.lexicon(n)
    entries, have = [], {}
    for p in parents:
        inherit(entries, have, rename_names(instantiate_schemas(expand_schemas(w, p), derive_atoms(subs.get(p, []), lex.votes)), own_for(w, n, p)), ("head", "body", "guards"))
    for p in parents:
        entries = instantiate_schemas(entries, derive_atoms(binds_sas(w, p), lex.votes))
    entries = kind_forms(w, n, entries)
    for e in own:
        if e["kind"] == "cite" and e.get("relax"):
            relax_schemas(w, n, e, entries)
        elif e["kind"] == "cite":
            hit = for_kinds(w, n, entries, cited(entries, e["target"], e["tname"], e["label"]), e)
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
                gen = gen_votes(lex, o, [mp.get(x, x) for x in e["votes"]], e["name"], e["label"], e["mark"], agents=vote_agents(w, n)) if e.get("votes") else None
                if e.get("taken"):
                    o["taken"] = taken_map(e, mp, o["guards"])
                    o["guards"] = [o["taken"].get(g, g) for g in o["guards"]]
                o["guards"] = o["guards"] + [g for g in (map_guard(g, mp) for g in e["guards"]) if g not in o["guards"]]
                if e.get("decided"):
                    g2, m2 = decided_by(e, mp, o["guards"]), decided_by(e, mp, o.get("must", []))
                    if (g2, m2) == (o["guards"], o.get("must", [])):
                        raise Err("%s: %s is no role that decides %s" % (e["label"], e["decided"][1], o["name"]))
                    o["guards"], o["must"] = g2, m2
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
                gen = gen_votes(lex, e, org, e["name"], e["label"], e["mark"], agents=vote_agents(w, n)) if org else None
                if gen:
                    e["body"], e["head"] = e["body"] + gen[0], e["head"] + gen[1]
                entries += [e] + (new_votes(entries, gen[2]) if gen else [])
            else:
                entries.append(e)
    entries = kind_forms(w, n, entries)
    derive_party(w, n, lex, entries)
    unclash(entries)
    if REMOVE:
        entries = remove_redundant(w, n, entries)
    w.sasx[n] = entries
    return entries


# ---- kinds of note ------------------------------------------------------------------------

def kinds_of(w, n):
    """The kinds of note of n, in the order of its chain: [dict(word, noun, slot, plain, decl)].  A kind is declared in
    the Logic pragmas, 'Kind of note: bond, y-bonds due Day for y-notes.': its word, and its noun put for 'y-notes',
    the place y the issuer; '; its acts keep their names' for the one kind whose forms keep the act's name (Coins's coin)."""
    memo = w.__dict__.setdefault("_kinds", {})
    if n not in memo:
        c = w.contract(n)
        parents, _, _ = first_line(c, w.known)
        out = []
        for p in parents:
            out += [k for k in kinds_of(w, p) if k["word"] not in [x["word"] for x in out]]
        for word, noun, slot, plain in c.kinds:
            if word not in [x["word"] for x in out]:
                out.append(dict(word=word, noun=noun, slot=slot, plain=plain, decl=n))
        if sum(1 for k in out if k["plain"]) > 1:
            raise Err("%s: two kinds of note keep the names of the acts" % n)
        memo[n] = out
    return memo[n]


def kind_alts(w, k):
    """For each name the noun of kind k adds (Day of 'y-bonds due Day'), the names of its kind in the names line of the
    contract that declares it, from it on: the first note of the kind in an act takes Day, the second Term."""
    kl = kinds_line(w.contract(k["decl"]))
    out = {}
    for x in re.findall(NAME, k["noun"]):
        ks = kl.get(x)
        if not ks:
            raise Err("%s: the kind %s names %s, which its names line does not" % (k["decl"], k["word"], x))
        same = [y for y, kk in kl.items() if kk & ks]
        out[x] = same[same.index(x):]
    return out


def kind_noun(w, k, issuer, j, fresh=None):
    """The noun of kind k for the j-th note of its kind in an act, of the issuer: 'Carol-bonds due Term'; with fresh, a
    place for each name it adds instead (for an act phrase)."""
    s = fill(k["noun"], {k["slot"]: issuer})
    for x, alts in kind_alts(w, k).items():
        if fresh is not None:
            to = fresh[x]
        else:
            if j >= len(alts):
                raise Err("%s: no name for the %d-th %s of an act, after %s" % (k["decl"], j + 1, k["word"], ", ".join(alts)))
            to = alts[j]
        s = re.sub(r"(?<![A-Za-z])%s(?![A-Za-z])" % x, to, s)
    return s


def kind_name(name, ks, words, taken):
    """The name of the form of an act of notes for the kinds ks of its notes, in order: the act's name where every note
    is of the kind that keeps it; else the name and the kind, 'Deposit bond', 'Swap bonds' where its two notes are of
    one kind, 'Swap coin for bond' where not; where a sentence of another clause has that name, the plural, 'Issue bonds' beside Bonds:1's Issue bond, and
    the kind's word even for the kind that keeps the names, 'Deposit coin' beside Joint:2's Deposit."""
    if all(words[k]["plain"] for k in ks) and name not in taken:
        return name
    if len(ks) == 1:
        nm = "%s %s" % (name, ks[0])
        return nm if nm not in taken else "%s %s" % (name, plural(ks[0]))
    if len(set(ks)) == 1:
        return "%s %s" % (name, plural(ks[0]))
    return "%s %s" % (name, " for ".join(ks))


NOTES_RX = re.compile(r"(?<![A-Za-z])(%s|[b-z])-notes(?![A-Za-z])" % NAME)


def kind_phrases(w, n, lex):
    """The act phrases of the forms of each act phrase of notes, for the kinds of n: 'x and y swap n z-notes for m
    w-notes' gives 'x and y swap n z-coins for m w-bonds due e', a fresh place for each name a noun adds."""
    ks = kinds_of(w, n)
    if not ks:
        return
    have = {t for t, _, _ in lex.defs}
    for t, r, act in list(lex.defs):
        if not act:
            continue
        slots = list(dict.fromkeys(m.group(1) for m in NOTES_RX.finditer(t)))
        if not slots:
            continue
        for combo in itertools.product(ks, repeat=len(slots)):
            used = set(tvars(t)) | set(tvars(r))
            pool = [v for v in "efghijklmnopqrstuvwy" if v not in used]
            nouns = {}
            for s, k in zip(slots, combo):
                fresh = {x: pool.pop(0) for x in kind_alts(w, k)}
                nouns[s] = kind_noun(w, k, s, 0, fresh)
            sub = lambda txt: NOTES_RX.sub(lambda m: nouns.get(m.group(1), m.group(0)), txt)
            t2 = sub(t)
            if t2 in have:
                continue
            have.add(t2)
            lex.defs.append((t2, sub(r), True))
            if t in lex.req:
                lex.req[t2] = sub(lex.req[t])


def kind_atoms(w, n, lex, k):
    """{generic predicate: (its places, the slot's index, the predicate of kind k, [('v', index, mark) | ('x', name, mark)])}:
    each basic state of notes, as 'x holds n y-notes', and the state of kind k its noun makes of it, read in the lexicon."""
    memo = w.__dict__.setdefault("_katoms", {})
    mk = (n, k["word"])
    if mk in memo:
        return memo[mk]
    out = {}
    for t, p, args in lex.basic:
        tk = [x.rstrip(",") for x in toks(t)]
        sl = [m.group(1) for x in tk for m in [re.fullmatch(r"([b-z])-notes", x)] if m]
        if not sl:
            continue
        used = set(tvars(t))
        pool = [v for v in "efghijklmnopqrstuvwy" if v not in used]
        fresh = {x: pool.pop(0) for x in kind_alts(w, k)}
        t2 = NOTES_RX.sub(lambda m: kind_noun(w, k, m.group(1), 0, fresh), t)
        found = None
        for tb, pb, ab in lex.basic:
            mp = same_phrase(tb, t2)
            if mp is not None:
                found = (pb, ab, {v: u for u, v in mp.items()})
                break
        if not found:
            raise Err("%s: the kind %s has no state for '%s' ('%s')" % (n, k["word"], t, t2))
        pb, ab, inv = found
        strip = lambda a: a.lstrip(MARK)
        gpl = [strip(a) for a in args]
        spec = []
        for a in ab:
            mark = a[:len(a) - len(strip(a))]
            pl = inv.get(strip(a), strip(a))      # the place of t2
            back = {v: x for x, v in fresh.items()}
            if pl in back:
                spec.append(("x", back[pl], mark))
            else:
                spec.append(("v", gpl.index(pl), mark))
        out[p] = (gpl, gpl.index(sl[0]), pb, spec)
    memo[mk] = out
    return out


def kind_forms(w, n, entries, text=False, lex=None):
    """The acts of notes, each in one form for each kind of each note it moves (Udi, 2026-11-16, to Calculus #7: a coin,
    a bond and a share are notes, special cases and not instances; Issue, Swap, Pay and Escrow of Notes are each one act for
    any note).  An act whose atoms are of notes is generic: in a contract with kinds it is kept, marked, for the forms
    of a kind added later and for the citations of its clause, and is printed in its forms only.  Each form is the act
    with each note of its issuer's kind, the j-th note of a kind taking the j-th name of the kind's names (Day, Term),
    labelled as the act and named kind_name; a form a contract has, inherited, is not made again."""
    ks = kinds_of(w, n)
    if not ks:
        return entries
    words = {k["word"]: k for k in ks}
    lex = lex or w.lexicon(n)
    ka = {k["word"]: kind_atoms(w, n, lex, k) for k in ks}
    gmap = {}
    for v in ka.values():
        for q, (gpl, si, _, _) in v.items():
            gmap.setdefault(q, (gpl, si))
    gp = set(gmap)
    order_k = [k["word"] for k in ks]
    def slots_of(e):
        if text:
            return list(dict.fromkeys(m.group(1) for _, ph in e["effects"] + e["conds"] for m in NOTES_RX.finditer(ph)))
        return list(dict.fromkeys(l[2][gmap[l[1]][1]].lstrip(MARK) for l in e["head"] + e["body"] if l[1] in gp))
    acts = [e for e in entries if e["kind"] == "act"]
    chain = chain_names(w, n)
    have = {(e.get("kindof"), e.get("kinds")) for e in acts if e.get("kinds")}
    out, new = [], {}
    for e in entries:
        if e["kind"] != "act" or e.get("kinds") or e.get("party") or e.get("voteact"):
            out.append(e)
            continue
        slots = slots_of(e)
        if not slots:
            out.append(e)
            continue
        g = dict(e, generic=True)
        out.append(g)
        gk = key(e)
        perms = slot_symmetry(w, g, gmap)
        for combo in itertools.product([k["word"] for k in ks], repeat=len(slots)):
            if (gk, combo) in have:
                continue
            idx = tuple(order_k.index(c) for c in combo)
            if any(tuple(idx[s] for s in sg) < idx for sg in perms):
                continue            # the same act with its parties' names exchanged: Swap bond for coin is Swap coin for bond
            nth, js = {}, []
            for c in combo:
                js.append(nth.get(c, 0))
                nth[c] = nth.get(c, 0) + 1
            f = dict(g)
            f.pop("generic", None)
            f.pop("pid", None)
            if text:
                nouns = {s: kind_noun(w, words[c], s, j) for s, c, j in zip(slots, combo, js)}
                sub = lambda ph: NOTES_RX.sub(lambda m: nouns.get(m.group(1), m.group(0)), ph)
                f["effects"] = [(x, sub(ph)) for x, ph in g["effects"]]
                f["conds"] = [(x, sub(ph)) for x, ph in g["conds"]]
            else:
                at = {s: (c, j) for s, c, j in zip(slots, combo, js)}
                def sub(l):
                    if l[1] not in gp:
                        return l
                    gpl, si = gmap[l[1]]
                    c, j = at[l[2][si].lstrip(MARK)]
                    if l[1] not in ka[c]:
                        raise Err("%s: the kind %s has no state for %s" % (n, c, l[1]))
                    _, _, pb, spec = ka[c][l[1]]
                    alts = kind_alts(w, words[c])
                    vals = [x.lstrip(MARK) for x in l[2]]
                    return (l[0], pb, tuple(m + (vals[i] if t == "v" else alts[i][j]) for t, i, m in spec))
                f["head"] = [sub(l) for l in g["head"]]
                f["body"] = [sub(l) for l in g["body"]]
            f["kinds"], f["kindof"] = combo, gk
            mine = set(g.get("names", {}).items()) | {(g["label"].split("+")[0], g.get("oname", g["name"]))}
            taken = {x for lb, x in chain if (lb, x) not in mine}
            nm = lambda x: kind_name(x, list(combo), words, taken)
            f["name"] = nm(g["name"])
            if "oname" in g:
                f["oname"] = nm(g["oname"])
            f["names"] = {lb: nm(x) for lb, x in g.get("names", {}).items()}
            new.setdefault(gk, []).append(f)
    for gk, fs in new.items():          # after the forms the contract has of the act, or after the act
        at = max(i for i, e in enumerate(out) if e.get("kindof") == gk or (e.get("generic") and key(e) == gk))
        out[at + 1:at + 1] = fs
    return out


def slot_symmetry(w, g, gmap):
    """The permutations of the notes of an act of notes that a renaming of its names onto itself makes (Notes:2 Swap: Alice
    for Bob, Carol for Dave, Sum for Total), as the act's own contract has it in its schemas (sas/): [(the slot each
    slot goes to)].  Two forms one such permutation maps onto each other are one act, made once (kind_forms)."""
    memo = w.__dict__.setdefault("_symm", {})
    o = g["label"].split("+")[0]
    c = o.split(":")[0]
    nm = g.get("names", {}).get(o, g.get("oname", g["name"]))
    if (o, nm) in memo:
        return memo[(o, nm)]
    out = []
    try:
        es = [e for e in read_sas(w, c)[4] if e["kind"] == "act" and e["label"] == o and e["name"] == nm]
    except (OSError, Err):
        es = []
    if len(es) == 1:
        e = es[0]
        slots = list(dict.fromkeys(l[2][gmap[l[1]][1]].lstrip(MARK) for l in e["head"] + e["body"] if l[1] in gmap))
        names = sorted({a.lstrip(MARK) for l in e["head"] + e["body"] for a in l[2] if re.fullmatch(NAME, a.lstrip(MARK))} | {x for x in e["guards"] if re.fullmatch(NAME, x)})
        if len(slots) > 1 and len(names) <= 8:
            st = lambda ls, mp: frozenset((l[0], l[1], tuple(a[:len(a) - len(a.lstrip(MARK))] + mp.get(a.lstrip(MARK), a.lstrip(MARK)) for a in l[2])) for l in ls)
            h, b, gs = st(e["head"], {}), st(e["body"], {}), frozenset(e["guards"])
            for perm in itertools.permutations(names):
                mp = dict(zip(names, perm))
                if mp == {x: x for x in names} or not all(mp[s] in slots for s in slots):
                    continue
                if st(e["head"], mp) == h and st(e["body"], mp) == b and frozenset(mp.get(x, x) for x in e["guards"]) == gs:
                    sg = tuple(slots.index(mp[s]) for s in slots)
                    if sg not in out:
                        out.append(sg)
    memo[(o, nm)] = out
    return out


def decided_by(e, mp, gs):
    """The guards gs of an act a citation ', decided by ROLE in place of ROLE2' cites: ROLE2 put ROLE, those other than
    whom it was kept (GO:4's End officer, 'those who decide for Orga, other than Bob')."""
    new, old = (parse_atom(map_guard(x, mp)) for x in e["decided"])
    out = []
    for g in gs:
        if "(" in g:
            _, p, a = parse_atom(g)
            if p == old[1] and [x for x in a if not x.startswith("-")] == list(old[2]):
                g = "%s(%s)" % (new[1], ",".join(list(new[2]) + [x for x in a if x.startswith("-")]))
        out.append(g)
    return out


def for_kinds(w, n, entries, hit, e):
    """The acts a citation 'C:n, for bonds' restricts: of those it cites, the forms whose every note is of a kind it
    names, with their ward's forms; not the act of notes itself, nor its forms for the other kinds (Udi, 2026-11-16, to
    Calculus #8: a restriction may be for a special case of C that C' defines)."""
    if not e.get("kinds_for"):
        return hit
    words = {plural(k["word"]): k["word"] for k in kinds_of(w, n)}
    bad = [x for x in e["kinds_for"] if x not in words]
    if bad:
        raise Err("%s: %s is no kind of note of %s (%s)" % (e["label"], ", ".join(bad), n, ", ".join(sorted(words)) or "none"))
    want = {words[x] for x in e["kinds_for"]}
    out = [i for i in hit if entries[i].get("kinds") and set(entries[i]["kinds"]) <= want]
    if not out:
        raise Err("%s: %s has no form for %s" % (e["label"], e["target"], ", ".join(e["kinds_for"])))
    return out


def chain_names(w, n):
    """The names of the sentences of the clauses of n and of every contract it is written from ('Issue bond' of Bonds:1),
    which the form of an act of notes does not take (kind_name)."""
    memo = w.__dict__.setdefault("_cnames", {})
    if n not in memo:
        out = set()
        for c in order([n], w):
            for num, _, text in w.contract(c).clauses:
                for s in sentences(text):
                    m = re.match(r"([A-Z][A-Za-z' -]*?): ", s)
                    if m:
                        out.add(("%s:%d" % (c, num), m.group(1)))
        memo[n] = out
    return memo[n]


def default_items(w, n):
    """The defaults of the parameters n leaves open, 'STATE for PARAMETER': every parameter declared in n or a contract
    it is written from that no first line of its chain binds and whose declaration gives a default (DFLL).  A parameter
    with no default stays open, and the contract parametric in it."""
    lex = w.lexicon(n)
    norm = lambda t: t.replace(",", "")
    dd = {norm(k): v for k, v in lex.dflt.items()}
    out = []
    for t in lex.par:
        if norm(t) not in dd:
            continue
        v, put = dd[norm(t)], []
        for item, q in w.binds(n):      # a default names another parameter, which n's chain may bind: put its state there
            sf = split_for(item, w.lexicon(q))
            if sf and sf[0] == "state" and norm(sf[2]) != norm(t) and norm(t) in {norm(e[0]) for e in w.lexicon(q).basic}:      # of a contract the default is of
                v = subst_states(v, w.lexicon(q), sf[2], sf[1], False, put)
                put = put + [sf[1]]
        out.append("%s for %s" % (v, t))
    for t, _, _ in lex.roles:       # a role with a default, GnW's decision rule, that no first line of the chain gives
        if norm(t) in dd and not role_given(w, n, t):
            out.append("%s for %s" % (dd[norm(t)], t))
    return out


def role_given(w, n, t, seen=None):
    """Whether a first line of n's chain gives the role t: puts a role for it ('the guardians of z for those who decide
    for z', GSV), or instantiates the contract that declares it with the rule in words ('GO instantiates GnW with
    bylaws', the bylaws saying who decides)."""
    seen = set() if seen is None else seen
    if n in seen:
        return False
    seen.add(n)
    parents, subs, prose = first_line(w.contract(n), w.known)
    for p in parents:
        own = {r[0] for r in w.contract(p).own.roles}
        for item in subs.get(p, []):
            sf = split_for(item, w.lexicon(p))
            if sf and sf[0] == "role" and sf[2] == t:
                return True
        if prose and t in own and t in w.contract(p).own.dflt:
            return True
        if role_given(w, p, t, seen):
            return True
    return False


def default_atoms(w, n, items):
    """The items of default_items as atoms, as sas_first writes a first line's: 'officer(x,z) for voter(x,z,m)'."""
    lex, out = w.lexicon(n), []
    for item in items:
        kind, a, t, pred, args = split_for(item, lex)
        new = ", ".join(lit_s(l) for l in pattern(a, lex)) if kind == "state" else role_pattern(a, lex)
        out.append("%s for %s(%s)" % (new, pred, ",".join(args)))
    return out


def text_at_defaults(w, n, entries):
    """The flat text of n at its defaults: each open parameter put its default, as a first line would put a state for
    it, its defaults in turn and then the bindings of n's chain again, a default naming another parameter (GO's
    decision counts those entitled to vote, which a first line may bind)."""
    items = default_items(w, n)
    if not items:
        return entries
    lex = w.lexicon(n)
    for _ in items:
        for item in derive_items(items, lex):
            entries = instantiate_text(w, entries, [item], n, n)
    for item, q in w.binds(n):
        entries = instantiate_text(w, entries, [item], q, n)
    return entries


def schemas_at_defaults(w, n, entries):
    """The expanded schemas of n at its defaults, as text_at_defaults has the flat text."""
    items = default_items(w, n)
    if not items:
        return entries
    lex = w.lexicon(n)
    atoms = default_atoms(w, n, items)
    for _ in items:
        entries = instantiate_schemas(entries, derive_atoms(atoms, lex.votes))
    return instantiate_schemas(entries, derive_atoms(binds_sas(w, n), lex.votes))


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


def vote_agents(w, n):
    """The agents the names line of n declares, persons, parties, organisations and wards: an act of n decided on the
    votes of an organisation has it know each of them its motion names, as a ward's form has (gen_vote; CDC:1's Join,
    whose vote names Bob, Helper #8 for Calculus #7, 2026-11-16, as Calculus #6 decided for the ward's forms)."""
    return {x for x, ks in kinds_line(w.contract(n)).items() if ks & {"person", "party", "organisation", "ward"}}


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
    # a place a role holds, those who decide for the organisation (GO:3's Expel), is the organisation in its motion: the
    # motion is the organisation's act, "whether Orga ends Bob's participation in Orga" (CDC:2, on the votes of Orga)
    vals = {f["ren"][v]: full[v] if re.fullmatch(NAME, full[v]) or full[v] == WILD else org for v in tvars(f["t"])}
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
                    elif role_cases(new):          # a role put by cases: one act for each case, the case's states its conditions
                        split = lambda x: re.fullmatch(r"(.*?)((?:, other than %s,)*)" % NAME, x)      # a role and those other than whom
                        hit = [rx.fullmatch(split(x).group(1)) for x in e["persons"] + e.get("must", [])]
                        hit = [h for h in hit if h]
                        if hit:
                            gd = hit[0].groupdict()
                            for k, (r, st) in enumerate(role_cases(new)):
                                rr = low(fill(r, gd))
                                put = lambda x: rr + split(x).group(2) if rx.fullmatch(split(x).group(1)) else x
                                f = dict(e, persons=[put(x) for x in e["persons"]], must=[put(x) for x in e.get("must", [])],
                                         conds=e["conds"] + phrases_of(fill(st, gd), lex))
                                if e.get("taken"):
                                    f["taken"] = {kk: put(v) for kk, v in e["taken"].items()}
                                f.update(case_name(e, role_pattern(r, lex).split("(")[0], k))
                                out.append(f)
                            continue
                    else:
                        def role_sub(x):
                            m = re.fullmatch(r"(.*?)((?:, other than %s,)*)" % NAME, x)      # a role and those other than whom
                            r = rx.fullmatch(m.group(1))
                            return low(fill(new, r.groupdict())) + m.group(2) if r else x
                        e["persons"] = [role_sub(x) for x in e["persons"]]
                        e["must"] = [role_sub(x) for x in e.get("must", [])]
                        if e.get("taken"):
                            e["taken"] = {kk: role_sub(v) for kk, v in e["taken"].items()}
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
        inherit(entries, have, rename_names(instantiate_text(w, w.flat[p], derive_items(subs.get(p, []), w.lexicon(p)), p, n), own_for(w, n, p)), ("effects", "conds", "persons"))
    for p in parents:
        for item, q in w.binds(p):
            entries = instantiate_text(w, entries, [item], q, n)
    entries = kind_forms(w, n, entries, True)
    for e in own:
        if e["kind"] == "cite" and e.get("relax"):
            relax_text(w, n, e, entries)
        elif e["kind"] == "cite":
            hit = for_kinds(w, n, entries, cited(entries, e["target"], e["tname"], e["label"]), e)
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
                gen = gen_votes_text(lex, o, [map_text(x, mp) for x in e["votes"]], e["name"], e["label"], e["mark"], agents=vote_agents(w, n)) if e.get("votes") else None
                if e.get("taken"):
                    o["taken"] = taken_map(e, {k: map_text(k, mp) for k in e["taken"]}, o["persons"])
                    o["persons"] = [o["taken"].get(g, g) for g in o["persons"]]
                o["persons"] = o["persons"] + [g for g in (map_text(g, mp) for g in e["persons"]) if g not in o["persons"]]
                if e.get("decided"):
                    dn, dold = map_text(e["decided"][0], mp), map_text(e["decided"][1], mp)
                    o["persons"] = [dn + x[len(dold):] if x == dold or x.startswith(dold + ",") else x for x in o["persons"]]
                    o["must"] = [dn + x[len(dold):] if x == dold or x.startswith(dold + ",") else x for x in o.get("must", [])]
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
                gen = gen_votes_text(lex, e, org, e["name"], e["label"], e["mark"], agents=vote_agents(w, n)) if org else None
                if gen:
                    e["conds"], e["effects"] = e["conds"] + gen[0], e["effects"] + gen[1]
                entries += [e] + (new_votes(entries, gen[2]) if gen else [])
            else:
                entries.append(e)
    entries = kind_forms(w, n, entries, True)
    derive_party(w, n, lex, entries, True)
    unclash(entries)
    if REMOVE:          # the acts expand_schemas removes, removed from the text too, so that the two commute
        expand_schemas(w, n)
        entries = [e for e in entries if not (e["kind"] == "act" and (n, e["label"], e["name"]) in REMOVED)]
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


def best_phrase(e, lex, acts, gts, prefer=None):
    """The act phrase whose effects are the head of the act schema e, its binding, persons, conditions and the effects
    left; the phrase prefer first where it has these effects (the phrase of the act of the clause e strengthens)."""
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
                score = (prefer is not None and t != prefer, len(rest_h), len([v for v in sv if v not in b]), k)
                if best is None or score < best[0]:
                    best = (score, t, full, [full[v] for v in sv] if gts else [], conds, rest_h)
    return best


def taken_text(taken):
    """', taken for Alice by Erin and for Bob by Frank' of {Alice: Erin, Bob: Frank}."""
    return ", taken " + " and ".join("for %s by %s" % kv for kv in taken.items()) if taken else ""


def act_sentence(e, lex, acts, ir, prefer=None):
    """The sentence of an act schema: the act phrase whose effects are its head, its conditions folded.  An act a
    citation takes for a party is written with its own persons, 'taken for' the party 'by' the one who takes it."""
    tk = {k: guard_text(v, ir) for k, v in (e.get("taken") or {}).items()}
    back = {v: k for k, v in tk.items()}
    gts = [back.get(g, g) for g in (guard_text(g, ir) for g in e["guards"])]
    best = best_phrase(e, lex, acts, gts, prefer)
    if not best:
        return None
    _, t, full, persons, conds, rest_h = best
    mu = [guard_text(g, ir) for g in e.get("must", [])]
    whole = act_text(persons, t, full, join(g for g in gts if g not in persons and g not in mu),
                     fold_states(conds, lex, actor_of(persons, t, full)), join(mu))
    bare = act_text(persons, t, full, "", "")[:-1]
    s = "%s: %s" % (e["name"], bare + taken_text(tk) + whole[len(bare):])
    return s[:-1] + ", and then " + effects_text(rest_h, lex) + "." if rest_h else s


def origin_phrase(w, e):
    """The act phrase of the act a strengthened act schema e is (GSG:2's Unfriend, of GSN's GSG:2+GSN:3), as its own
    contract writes it; None for an act of one clause, or where its contract has no act of that clause and name."""
    if "+" not in e["label"] or e["kind"] != "act":
        return None
    o = e["label"].split("+")[0]
    c = o.split(":")[0]
    oe = [x for x in w.sasx.get(c, []) if x["label"] == o and x["kind"] == "act" and x["name"] == e["name"]]
    if len(oe) != 1:
        return None
    lex = w.lexicon(c)
    ib, ir = inv_basic(lex)
    tk = {k: guard_text(v, ir) for k, v in (oe[0].get("taken") or {}).items()}
    back = {v: k for k, v in tk.items()}
    best = best_phrase(oe[0], lex, act_phrases(lex), [back.get(g, g) for g in (guard_text(g, ir) for g in oe[0]["guards"])])
    return best[1] if best else None


KINDS2 = {}      # {contract: [(name, its kinds)]}: a name with two kinds neither of which is under the other


def unite_names(entries, n):
    """The names lines of n's chain, in the kept entries X:0 of its expanded entries, united into one names line,
    an entry n:0 at the place of the first (tools/nameslines.py); what else a names line said stays in its entry, each
    sentence once; an entry left with nothing is dropped."""
    head = [e for e in entries if e["kind"] == "kept" and e["label"].endswith(":0")]
    sents = [s for e in head for s in nameslines.split_sentences(e["text"])]
    named = [s for s in sents if nameslines.is_names(s)]
    if not named:
        return entries
    rest = repr([e for e in entries if not (e["kind"] == "kept" and e["label"].endswith(":0"))])
    line, findings, _ = nameslines.unite(named, sents, rest)
    KINDS2[n] = findings
    out, seen, at = [], set(), None
    for e in entries:
        if not (e["kind"] == "kept" and e["label"].endswith(":0")):
            out.append(e)
            continue
        keep = []
        for s in nameslines.split_sentences(e["text"]):
            p = nameslines.parse(s)
            if p is None:
                keep.append(s)
                continue
            if at is None:
                at = len(out)
            keep += [r for r in p[1] if r not in seen]
            seen.update(p[1])
        if keep:
            out.append(dict(e, text=" ".join(keep)))
    out.insert(at, dict(kind="kept", label="%s:0" % n, mark="-", text=line[0], united=True))
    return out


def expanded(w, n):
    """The expanded contract of n, in the grammar: the names lines and each clause of its expanded schemas, labelled
    by its origin, its acts written with their act phrases.  The phrases are defined in the contracts of origin."""
    lex = w.lexicon(n)
    ib, ir = inv_basic(lex)
    acts = act_phrases(lex)
    head = []
    prose, clauses, of = [], [], {}
    for e in w.sasx[n]:
        if e.get("united"):
            head.append(e["text"])
            continue
        if e["label"].endswith(":0"):
            c0 = e["label"][:-2]
            if c0 not in of:
                of[c0] = len(prose)
                prose.append("Of %s: %s" % (c0, e["text"]))
            else:
                prose[of[c0]] += " " + e["text"]
            continue
        txt = e["text"] if e["kind"] == "kept" else act_sentence(e, lex, acts, ir, origin_phrase(w, e))
        if txt is None:
            raise Err("%s %s: no act phrase has these effects" % (e["label"], e["name"]))
        if clauses and clauses[-1][0] == e["label"].split("+")[0] and e["kind"] == "kept":
            clauses[-1][3].append(txt)
        elif clauses and clauses[-1][1] == e["label"] and clauses[-1][2] == e["mark"]:
            clauses[-1][3].append(txt)
        else:
            clauses.append([e["label"].split("+")[0], e["label"], e["mark"], [txt]])
    return [w.contract(n).title] + prose + head + ["%s. %s %s" % (l.replace("+", ", "), m, " ".join(ts)) for _, l, m, ts in clauses]


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
                elif pred not in pir:
                    continue        # an item of the instance that binds the contract composed with it (first_line)
                elif " while " in new:          # a role put by cases
                    parts = []
                    for part in new.split(" or "):
                        r, _, st = part.partition(" while ")
                        parts.append("%s while %s" % (guard_text(r.strip(), ir), fold_states([parse_atom(a) for a in split_top(st)], lex)))
                    s = s.replace(item, "%s for %s" % (", or ".join(parts), guard_text(old, pir)))
                else:
                    s = s.replace(item, "%s for %s" % (guard_text(new, ir), guard_text(old, pir)))
        out.append(s)
    head_lines = [l for l in lines[1:] if not ENTRY.match(l) and l != first]
    body, by = [], {}
    for e in entries:
        if e["label"].endswith(":0"):
            if e["label"] == n + ":0" and (e["text"].startswith(REFL) or e["text"].startswith(KNDL) or e["text"].startswith(ENVL) or e["text"].startswith(PARL) or e["text"].startswith(DFLL)):
                head_lines.append(e["text"])      # a declaration of the Logic pragmas, after the lexicon lines
            else:
                head_lines.append(("prose", e["text"]))
            continue
        if e["kind"] == "kept":
            s = e["text"]
        elif e["kind"] == "cite" and e.get("relax"):
            s = "%s: %s%s" % (e["name"], e["target"], " " + e["tname"] if e["tname"] else "")
            if e.get("drop_guards"):
                s += ", without the consent of " + join(guard_text(g, ir) for g in e["drop_guards"])
            if e.get("drop_body"):
                s += ", without " + fold_states(e["drop_body"], lex, ACTOR)
            ifs = ["%s is %s" % q for q in e.get("eqs", [])] + ([fold_states(e["body"], lex, ACTOR)] if e["body"] else [])
            if ifs:
                s += ", if " + join(ifs)
            s += "."
        elif e["kind"] == "cite":
            s = "%s: %s%s%s%s" % (e["name"], e["target"], " " + e["tname"] if e["tname"] else "", ", for " + join(e["kinds_for"]) if e.get("kinds_for") else "", taken_text(e.get("taken")))
            dec = ", decided by %s in place of %s" % (guard_text(e["decided"][0], ir), guard_text(e["decided"][1], ir)) if e.get("decided") else ""
            if e["body"]:
                s += ", only if " + fold_states(e["body"], lex, ACTOR)
            if e["guards"]:
                s += ", with the consent of " + join(guard_text(g, ir) for g in e["guards"])
            s += dec
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
            out.append((e["label"], e["mark"], e["name"], e["target"], e["tname"], tuple(sorted(e["body"])), tuple(sorted(e["guards"])), tuple(sorted(e.get("then", []))), tuple(e.get("kinds_for") or ()), tuple(e.get("decided") or ())) +
                       ((tuple(sorted(e.get("drop_guards") or ())), tuple(sorted(e.get("drop_body") or ())), tuple(sorted(tuple(q) for q in e.get("eqs") or ()))) if e.get("relax") else ()) +
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


# ---- the acts no agent can take ----------------------------------------------------

def marks_of(acts):
    """The marks of a contract with wards and the list of the guardians of each: {ward: has_guardian}.  A mark is
    given with an identity by the act that creates its bearer, which also gives the bearer a guardian who holds
    the guardianship."""
    out = {}
    for e in acts:
        pos = [l for l in e['head'] if not l[0]]
        for l in pos:
            if l[1] == 'identity':
                x = l[2][0]
                for m in pos:
                    if len(m[2]) == 1 and m[2][0] == x and m[1] not in ('identity', 'person'):
                        us = [u for u in pos if len(u[2]) == 2 and u[2][0] == x and u[2][1] != x
                              and any(len(v[2]) == 2 and v[2] == (u[2][1], x) and v[1] in u[1] for v in pos)]
                        if us:
                            out.setdefault(m[1], us[0][1])
    return out


def untakable(w, n, acts):
    """{(label, act): why} of the acts of n, entries as read_schema_line reads them, that require of one name both
    that it is a person and that it bears a mark of a ward of the contract (child, ward, organisation, ai_agent:
    marks_of), which no agent is at once (GnW: a person or a ward).  A name is required a person where the act
    requires person(X), or where X decides it and the act's own contract declares X a person.  Such an act no agent
    can take: it is in none of sasx/, flat/, gx/ (main), and toglp.py has none to leave out of the term."""
    out = {}
    marks = marks_of(acts) if any(l[1] == 'person' for e in acts for l in e['head']) else {}
    if not marks:
        return out
    memo = {}
    for e in acts:
        o = re.split(r'[:+]', e['label'])[0]
        if o not in memo:
            memo[o] = frozenset(x for x, ks in kinds_from(w, n, o).items() if ks & {'person', 'party'} and not ks & {'organisation', 'ward'}) if o in w.known else frozenset()
        persons = memo[o] | frozenset((e.get('taken') or {}).values()) | {VOTER}
        has = lambda p, g: any(not l[0] and l[1] == p and l[2] and l[2][0] == g for l in e['body'])
        made = {l[2][0] for l in e['head'] if l[1] == 'identity' and not l[0]}
        names = {l[2][0] for l in e['body'] if l[2]} | {g for g in e['guards'] if '(' not in g}
        for x in sorted(names):
            person = has('person', x) or (x in e['guards'] and x not in made and x in persons
                                          and not any(has(m, x) and has(u, x) for m, u in marks.items()))
            ward = [m for m in marks if has(m, x)]
            if person and ward:
                out[(e['label'], e['name'])] = 'it requires person(%s) and %s(%s), and no agent is both' % (x, ward[0], x)
                break
    return out


UNTAKABLE = {}      # {(n, label, act): why}, the acts main left out of n's sasx/, flat/ and gx/


def main():
    allnames = NAMES_.names_in(os.path.join(ROOT, "gr"))
    w = World(allnames)
    names = order([a for a in sys.argv[1:] if not a.startswith("-")] or allnames, w)
    global REMOVE
    REMOVE = REMOVE or "--remove" in sys.argv
    bad = 0
    for n in names:
        res = []
        try:
            sas, own = compile_contract(w, n)
            write_sas(w, n, sas)
            flat = unite_names([e for e in text_at_defaults(w, n, expand_text(w, n, own)) if not e.get("generic")], n)      # printed at the defaults; inherited open (expand_schemas keeps w.sasx open); an act of notes in its kinds' forms only
            put("flat", n, [w.contract(n).title + " (flat text)"] + [flat_line(e) for e in flat])
            sx0 = expand_schemas(w, n)
            sx = unite_names([e for e in schemas_at_defaults(w, n, sx0) if not e.get("generic")], n)
            gone = untakable(w, n, [read_schema_line(schema_line(e)) for e in sx if e["kind"] == "act"])      # an act no agent can take is no act of the contract
            keep = os.path.join(ROOT, "sasx-all", NAMES_.file_of(n) + ".txt")      # sasx/ with the untakable acts kept, for relations.py alone: a restriction may make a base's act untakable
            if gone:
                put("sasx-all", n, [w.contract(n).title + " (expanded schemas, the acts no agent can take kept)"] + [schema_line(e) for e in sx])
            elif os.path.exists(keep):
                os.remove(keep)
            sx = [e for e in sx if (e["label"], e.get("name")) not in gone]
            flat = [e for e in flat if (e["label"], e.get("name")) not in gone]
            put("flat", n, [w.contract(n).title + " (flat text)"] + [flat_line(e) for e in flat])
            UNTAKABLE.update({(n,) + k: v for k, v in gone.items()})
            put("sasx", n, [w.contract(n).title + " (expanded schemas)"] + [schema_line(e) for e in sx])
            w.sasx[n] = sx
            put("gx", n, expanded(w, n))
            w.sasx[n] = sx0
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
                os.remove(os.path.join(ROOT, "back-tmp", NAMES_.file_of(n) + ".txt"))
                os.rmdir(os.path.join(ROOT, "back-tmp"))
                put("back", n, back)
        except Err as ex:
            res.append("ERROR " + str(ex))
        if any("NO" in r or "ERROR" in r for r in res):
            bad += 1
        if NO_WARD.get(n):
            print("%-10s no ward's form, its kinds first met in a contract that binds GO's states: %s" % (n, ", ".join(dict.fromkeys(NO_WARD[n]))))
        for (c, l, nm), (kl, kn) in sorted(REMOVED.items()):
            if c == n:
                print("%-10s removed %s %s, made redundant by %s %s" % (n, l, nm, kl, kn))
        for (c, l, nm), why in sorted(UNTAKABLE.items()):
            if c == n:
                print("%-10s untakable %s %s: %s" % (n, l, nm, why))
        print("%-10s %s" % (n, "; ".join(res)))
    two = [(n, x, ks) for n in names for x, ks in KINDS2.get(n, [])]
    print("names: %d names of two kinds neither of which is under the other" % len(two))
    for n, x, ks in two:
        print("kinds: %s: %s is %s" % (n, x, " and ".join("%s ('%s')" % k for k in ks)))
    print("%d contracts, %d with a fault" % (len(names), bad))
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
