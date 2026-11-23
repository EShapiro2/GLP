#!/usr/bin/env python3
"""expand.py --- the appendix of the contracts expanded, each readable alone.

    python3 tools/expand.py [--rev DIR] [--exp DIR] [--out FILE] [--check] [--notes]

Reads every rev/NAME.txt (not NOTES-*.txt) and computes, for each contract, its
expanded text: the text after the operations its first line and its citing
clauses state have been applied.  Writes exp/NAME.txt, in the format of rev/,
and sections/contracts-expanded.tex, one contractbox per contract in the order
of tools/revtex.py, whose reading of the files, tables, escaping and box style
it uses.

The rules.
1. extends, restricts, instantiates, composes: the expanded contract has the
   expanded definitions and clauses of each contract its first line names, in
   the order they are named, and then its own.  A clause that comes by two
   paths appears once.
2. A clause is labelled by its origin, NAME:n.
3. A citing sentence of a clause, `X:n[, Y:m and Z:k], REST.`, is an operation:
   REST is appended to the act sentences of each clause cited (those TARGETS
   names for the citing clause; else those headed by an act the citing
   sentence's heading names, or the cited clause's title; all its act
   sentences when the heading names none of them), after `, ` and, on a
   sentence already restricted, after `, and `; the label of the clause gains
   NAME:k.  The citing clause does not appear; its sentences that cite nothing
   are appended to the clause last cited.
4. A parenthetical mention, `(X:n)`, is no operation.
5. instantiates C with BINDING, W2 for W1, ...: W1 becomes W2 in what is
   inherited from C (singular, plural, capitalised, whole words, and the
   article before it), and `Here: BINDING.`, BINDING with the same words
   substituted, follows the sentence that declares the parameter bound: C's
   own, else the unbound one whose term BINDING uses, else the last unbound
   one of C's expanded definitions.
6. The names lines are united sentence by sentence, the definitions likewise,
   inherited first; a sentence appears once.
7. A relaxing citation, `X:n, without ...[, if ...]`, adds an act and keeps
   the act of X:n as it is (tools/SAS-WRITING.txt, RELAXATION): it is no
   operation, and its clause is kept as an own clause, as written, after the
   clause X:n it cites (Coins:1, Pay issuer: Notes:3, without the consent of Bob,
   if the coins are Bob-coins).

It checks and prints: every contract a first line names is in rev/ (after
revtex's RENAMED and DROPPED); every citation X:n is a clause of the expanded
contract; no contract depends on itself; each contract's count of inherited,
replaced and own clauses.  It exits 1 on any failure.  --check writes nothing,
and also fails where exp/ or the appendix is not what it would write.  --notes
prints each place where a rule did not decide and what was done.

Standard library only.
"""
import argparse
import copy
import os
import re
import sys

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import revtex  # noqa: E402
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM      # the names of the contracts and their files (tools/names.py, NAMES.txt)
import nameslines       # the one names line of a full contract (tools/nameslines.py)

HEADER = '% Written by tools/expand.py from rev/; do not edit.'
LEAD_IN = ('Each contract with the operations of its first line and of its citing clauses applied: '
           'the clauses of the contracts it is written from, each labelled by its origin, '
           'with their restrictions and substitutions, and then its own.')

NAME = NM.NAME_RX      # a name of the table, longest first, or one word (tools/names.py)
REF = NM.REF
VERBS = ('extends', 'restricts', 'strengthens', 'instantiates', 'composes', 'relaxes')
VERB = r'(?:%s)' % '|'.join(VERBS)
PRONOUNS = ('it', 'them', 'the composition')

# The act sentences a citing clause restricts, where its heading names none of the
# cited clause: (contract, clause number) -> the acts, as the cited contract heads
# them, before any substitution.  Consulted before the headings are matched.
# A decision of Calculus #2 of 2026-11-13: GO:4, `Officers. Appoint: GnW:2, only if
# Bob is a participant; the decision names his role.`, restricts Make guardian alone.
TARGETS = {('Organisation', 4): ['Make guardian']}

FAILURES = []     # what --check fails on
NOTES = []        # (contract, clause, what was done): where the rules did not decide


def fail(msg):
    if msg not in FAILURES:
        FAILURES.append(msg)


def note(contract, clause, what):
    n = (contract, clause, what)
    if n not in NOTES:
        NOTES.append(n)


# ---------------------------------------------------------------- sentences

def split_sentences(text):
    """The sentences of a paragraph: a break after . ? ! before a capital, an
    *italic* term or a one-letter name (d > 1 is an integer), as revtex splits."""
    out, start = [], 0
    for m in re.finditer(r'(?<=[.?!]) +', text):
        rest = text[m.end():]
        if not rest:
            break
        w = re.match(r'\w+', rest)
        if rest[0] == '*' or rest[0].isupper() or (w and len(w.group()) == 1 and rest[0].isalpha()):
            out.append(text[start:m.start()])
            start = m.end()
    out.append(text[start:])
    return [s for s in out if s]


ACT = r"[A-Z][\w'’-]*(?: [\w'’-]+){0,3}"
HEAD = re.compile(r"^(%s(?:, %s)*): (?=\S)" % (ACT, ACT))
CITE = re.compile(r'^(%s(?:, %s)*(?: and %s)?)(, | )(.*)\.$' % (REF, REF, REF), re.S)
CLAUSE = re.compile(r'^(\d+)\.\s+([■◆○])?\s*(.*)$')
PAREN = re.compile(r'\s*\(([^()]*)\)')
PARAM = re.compile(r'\bparameters? of (%s)' % NAME)
NAMES_LINE = re.compile(r'^[A-Z][a-z]+(?:, [A-Z][a-z]+)*(?: and [A-Z][a-z]+)? (?:is|are) ')


class Sent(object):
    """A sentence of a clause: the acts that head it, if any, and its text."""
    def __init__(self, heads, text):
        self.heads, self.text = heads, text
        self.restricted = 0       # the restrictions appended to it
        self.rests = []           # their texts, in the order appended

    def base(self):
        """The sentence before its restrictions were appended, or None where its text is not that and them."""
        if not self.rests:
            return self.text
        tail = ', ' + ', and '.join(self.rests) + '.'
        return self.text[:-len(tail)] + '.' if self.text.endswith(tail) else None

    def render(self):
        return (', '.join(self.heads) + ': ' if self.heads else '') + self.text


class Clause(object):
    def __init__(self, labels, mark, title, sents):
        self.labels, self.mark, self.title, self.sents = labels, mark, title, sents

    @property
    def origin(self):
        return self.labels[0]

    def label(self):
        return ', '.join(self.labels)

    def body(self):
        return ' '.join(([self.title] if self.title else []) + [s.render() for s in self.sents])

    def render(self):
        return '%s. %s%s' % (self.label(), self.mark + ' ' if self.mark else '', self.body())


def parse_clause(name, n, mark, text):
    """A clause's title, if its first sentence is headed by no act, and its sentences."""
    sents = []
    for s in split_sentences(text):
        m = HEAD.match(s)
        sents.append(Sent(m.group(1).split(', '), s[m.end():]) if m else Sent(None, s))
    title = None
    if len(sents) > 1 and not sents[0].heads:
        title = sents.pop(0).text
    c = Clause(['%s:%d' % (name, n)], mark, title, sents)
    if c.body() != text:
        fail('%s:%d is not read back as written' % (name, n))
    return c


class Def(object):
    """A sentence of the definitions: its text, the contract and line it is from,
    and what its parameter is bound to."""
    def __init__(self, text, origin, line=0):
        self.text, self.origin, self.line, self.here = text, origin, line, []


# ---------------------------------------------------------------- the first line

class First(object):
    def __init__(self):
        self.deps = []        # the contracts named, in order, once each
        self.inst = []        # the contracts instantiated
        self.binding = None
        self.pairs = []       # (W1, W2): W1 becomes W2
        self.papers = []      # the papers cited
        self.verbs = []


def parse_pairs(text):
    """`an organisation for a ward and an officer for a guardian` -> [(ward, organisation), ...]."""
    out = []
    for part in re.split(r', | and ', text):
        m = re.match(r'^an? (\S.*?) for an? (\S+(?: \S+)?)$', part)
        if not m:
            return None
        out.append((m.group(2), m.group(1)))
    return out


def parse_first(name, line, known):
    """The relation line of a contract.  Raises ValueError where it is not read."""
    f = First()
    s = line[len(name):].strip()
    if not s.endswith('.'):
        raise ValueError('no final period')
    s = s[:-1]
    for p in PAREN.findall(s):
        if not re.match(r'^%s, ' % NAME, p):
            raise ValueError('the parenthesis (%s) is no paper' % p)
        f.papers.append(p)
    s = PAREN.sub('', s)
    chunks = re.split(r'(?:,\s+and\s+|,\s+|\s+and\s+)(?=%s\b)' % VERB, s)
    last = []                 # the contracts of the chunk before, for `it`

    def names_of(text, chunk):
        if text in PRONOUNS:
            if not last:
                raise ValueError("'%s' refers to nothing" % text)
            return list(last)
        out = re.split(r', and |, | and ', text)
        for x in out:
            if not re.match(r'^%s$' % NAME, x):
                raise ValueError("'%s' in '%s' is no contract's name" % (x, chunk))
        return out

    for chunk in chunks:
        m = re.match(r'^(%s)\b\s*(.*)$' % VERB, chunk)
        if not m:
            raise ValueError("'%s' opens with no relation" % chunk)
        verb, rest = m.group(1), m.group(2)
        f.verbs.append(verb)
        if not rest:          # `extends and restricts X`: the names come with the next relation
            continue
        if verb == 'instantiates':
            m = re.match(r'^(.*?) with (.+)$', rest)
            if not m:
                raise ValueError('instantiates with no binding')
            got = names_of(m.group(1), chunk)
            f.inst = list(got)
            bind = m.group(2)
            f.binding = bind
            for k in [i.start() for i in re.finditer(', ', bind)]:
                pairs = parse_pairs(bind[k + 2:])
                if pairs:
                    f.binding, f.pairs = bind[:k], pairs
                    break
        elif verb == 'composes':
            m = re.match(r'^(.*?) with (.+)$', rest)
            if not m:
                raise ValueError('composes with nothing')
            got = names_of(m.group(1), chunk) + names_of(m.group(2), chunk)
        else:
            got = names_of(rest, chunk)
        for x in got:
            if x not in f.deps:
                f.deps.append(x)
        last = got
    if not f.deps:
        raise ValueError('names no contract')
    return f


# ---------------------------------------------------------------- contracts

class Contract(object):
    """A contract of rev/: its title, first line, definitions, names line and clauses."""
    def __init__(self, name, lines):
        lines = [l.rstrip() for l in lines if l.strip()]
        self.name = name
        self.title_line = lines[0]
        self.first_line = None
        self.def_lines, self.names_lines, self.clauses = [], [], []
        header = []
        for l in lines[1:]:
            m = CLAUSE.match(l)
            if m:
                self.clauses.append([int(m.group(1)), m.group(2) or '', m.group(3)])
            elif self.clauses:            # an unnumbered paragraph of the clause above
                self.clauses[-1][2] += ' ' + l
                note(name, '%s:%d' % (name, self.clauses[-1][0]), 'an unnumbered paragraph is joined to the clause above it')
            else:
                header.append(l)
        if header and re.match(r'^%s %s\b' % (re.escape(name), VERB), header[0]):
            self.first_line = header.pop(0)
        for l in header:
            (self.names_lines if NAMES_LINE.match(l) else self.def_lines).append(l)
        self.first = None                 # set by read_all


def read_all(rev_dir):
    contracts, raw = {}, {}
    for f in sorted(os.listdir(rev_dir)):
        if f.endswith('.txt') and not f.startswith('NOTES-'):
            name = NM.name_of(f[:-4])
            box = revtex.original_box(os.path.join(rev_dir, f))
            raw[name] = revtex.Contract(box, True, f)
            contracts[name] = Contract(name, box)
    return contracts, raw


def resolve(x, contracts):
    """A contract's name in rev/: itself, or the name revtex's RENAMED gives it."""
    if x in contracts:
        return x
    for new, old in revtex.RENAMED.items():
        if old == x and new in contracts:
            return new
    return None


def link(contracts):
    """Parse every first line; check that each contract it names is in rev/."""
    unparsed = []
    for name in sorted(contracts):
        c = contracts[name]
        if not (c.title_line.startswith(name + ':') or c.title_line == name):      # 'NAME: Title', or the name alone (Notes, Coins, Bonds, Shares, Udi, 2026-11-16)
            fail("%s: the title line does not open with '%s:' and is not the name alone" % (name, name))
        if c.first_line is None:
            c.first = First()
            continue
        try:
            c.first = parse_first(name, c.first_line, contracts)
        except ValueError as e:
            fail('%s: the first line is not read (%s): %s' % (name, e, c.first_line))
            unparsed.append(name)
            c.first = First()
            continue
        for attr in ('deps', 'inst'):
            got = []
            for x in getattr(c.first, attr):
                y = resolve(x, contracts)
                if y is None:
                    why = ('dropped in the revision' if re.search(r'\b%s\b' % re.escape(x), revtex.DROPPED)
                           else 'not in rev/')
                    fail('%s: the first line names %s, %s' % (name, x, why))
                elif y == name:
                    fail('%s: the first line names %s itself' % (name, name))
                elif y not in got:
                    got.append(y)
            setattr(c.first, attr, got)
    return unparsed


def cycles(contracts):
    """Names on a cycle of first lines."""
    state, bad = {}, set()

    def visit(n, path):
        if state.get(n) == 2:
            return
        if state.get(n) == 1:
            bad.update(path[path.index(n):])
            return
        state[n] = 1
        for d in contracts[n].first.deps:
            visit(d, path + [n])
        state[n] = 2

    for n in sorted(contracts):
        visit(n, [])
    return sorted(bad)


# ---------------------------------------------------------------- substitution

IRREGULAR = {'child': 'children', 'person': 'persons'}


def plural(w):
    if w in IRREGULAR:
        return IRREGULAR[w]
    if re.search(r'(?:s|x|ch|sh)$', w):
        return w + 'es'
    if re.search(r'[^aeiou]y$', w):
        return w[:-1] + 'ies'
    return w + 's'


def cap(w):
    return w[0].upper() + w[1:]


def article(word, capital):
    """a or an, by the letter the word opens with (an AI agent, an organisation)."""
    an = word[0].lower() in 'aeio' or re.match(r'^[AEFHILMNORSX][A-Z]', word)
    return ('An' if an else 'A') if capital else ('an' if an else 'a')


class Subst(object):
    """W1 -> W2 for each pair, in one pass: singular, plural and capitalised forms,
    whole words; an article before the word follows the new word."""
    def __init__(self, pairs, names=()):
        self.names = set(names)       # a contract's name where a name stands is no word: 'Guardians:2', 'a parameter of Guardians'
        self.forms = {}
        for w1, w2 in pairs:
            for a, b in ((w1, w2), (plural(w1), plural(w2))):
                self.forms[a] = b
                self.forms[cap(a)] = cap(b)
        alts = '|'.join(re.escape(x) for x in sorted(self.forms, key=len, reverse=True))
        self.rx = re.compile(r'(?:\b(an?|An?) (\*?))?(?<![\w-])(%s)(?![\w-])' % alts) if pairs else None

    def __call__(self, text):
        if not self.rx or text is None:
            return text

        def sub(m):
            if m.group(3) in self.names and (re.match(r':\d', text[m.end():]) or re.search(r'\bparameters? of $', text[:m.start(3)])):
                return m.group(0)
            new = self.forms[m.group(3)]
            if m.group(1):
                return '%s %s%s' % (article(new, m.group(1)[0] == 'A'), m.group(2), new)
            return new
        return self.rx.sub(sub, text)


# ---------------------------------------------------------------- expansion

class Expanded(object):
    def __init__(self):
        self.defs, self.names, self.clauses = [], [], []
        self.inherited = self.replaced = self.own = 0
        self.cited = []        # every X:n the contract's own text cites


def find(clauses, ref):
    return [c for c in clauses if ref in c.labels]


def refs_of(text, contracts):
    """The citations X:n of a text, X under its name in rev/ where revtex's RENAMED gives it one."""
    out = []
    for r in re.findall(REF, text):
        x, n = r.rsplit(':', 1)
        out.append('%s:%s' % (resolve(x, contracts) or x, n))
    return out


def bind(name, target, defs, binding, sub):
    """Add `Here: BINDING.` to the sentence that declares the parameter bound, the
    binding with the first line's own substitutions (the guardians -> the parents)."""
    params = [d for d in defs if PARAM.search(d.text)]
    own = [d for d in params if target in PARAM.findall(d.text)]
    free = [d for d in params if not d.here]
    how = None
    if own:
        d, how = own[-1], None
    else:
        named = [d for d in free
                 if any(re.search(r'(?<![\w-])%s(?![\w-])' % re.escape(t), binding)
                        for t in re.findall(r'\*([^*]+)\*', d.text))]
        words = lambda t: set(re.findall(r'[a-z]{4,}', t.lower()))
        said = [d for d in params if len(words(binding) & words(d.text)) > 1]
        if named:
            d, how = named[-1], 'the unbound parameter whose term the binding uses'
        elif said:
            d, how = said[-1], 'the parameter whose sentence has the words of the binding'
        elif free:
            d, how = free[-1], 'the last unbound parameter of its expanded definitions'
        else:
            fail('%s: instantiates %s with %s, and %s has no parameter to bind' % (name, target, binding, target))
            return
    if how:
        note(name, 'first line', "%s declares no parameter of its own; '%s' is bound to %s, %s's: %s"
             % (target, binding, how, d.origin, re.sub(r'\*', '', d.text)))
    here = sub(binding)
    if here not in d.here:
        if d.here:
            note(name, 'first line', "the parameter of '%s' is bound twice: %s; and now %s" % (d.text, '; '.join(d.here), here))
        d.here.append(here)


WEAK = '■◆○'    # the legal modalities, strongest first: enforced, attested, undertaken; an act had by two paths whose
                # texts differ only in the modality is one act, with the weaker of the two (Calculus #8, 2026-11-17)


def merge(a, b):
    """A clause that comes by two paths (rule 1): where the two are one clause, sentence by sentence, with restrictions
    appended, A takes every restriction of B it lacks, after its own, and B's labels, and the weaker of the two marks; the expanded text then carries
    every inherited restriction, as the merged schema has every condition (tools/gcsas.py).  False where they are not."""
    if len(a.sents) != len(b.sents) or a.title != b.title:
        return False
    if a.mark != b.mark and not (a.mark in WEAK and b.mark in WEAK):
        return False
    for x, y in zip(a.sents, b.sents):
        if x.heads != y.heads or x.base() is None or x.base() != y.base():
            return False
    for x, y in zip(a.sents, b.sents):
        rests = x.rests + [t for t in y.rests if t not in x.rests]
        base = x.base()
        x.text = base[:-1] + (', ' + ', and '.join(rests) if rests else '') + '.'
        x.rests, x.restricted = rests, len(rests)
    for l in b.labels:
        if l not in a.labels:
            a.labels.append(l)
    if a.mark != b.mark:                      # texts that differ only in the modality: one act, with the weaker
        a.mark = max(a.mark, b.mark, key=WEAK.index)
    return True


def restrict(name, k, mark, heads, title, refs, rest, exp, sub):
    """Apply one citing sentence of clause name:k to the clauses it cites; return them."""
    own = '%s:%d' % (name, k)
    targets = []
    for r in refs:
        got = find(exp.clauses, r)
        if not got:
            fail('%s cites %s, which is no clause of the expanded %s' % (name, r, name))
        for c in got:
            if c not in targets:
                targets.append(c)
    if not re.match(r'^(?:only if|only with the consent of|with the consent of|where) ', rest):
        note(name, own, "the restriction of %s opens with none of 'only if', 'only with the consent of', "
             "'with the consent of', 'where'; it is appended as written: '%s'" % (' and '.join(refs), rest))
    names = heads or ([title.rstrip('.')] if title else [])
    told = TARGETS.get((name, k))
    if told:                                  # the acts as the cited contract heads them, and as substituted here
        told = set(told) | set(sub(x) for x in told)
    for c in targets:
        acts = [s for s in c.sents if s.heads]
        if told:
            hit = [s for s in acts if set(s.heads) & told]
            if not hit:
                fail('%s: TARGETS names %s, and no sentence of %s is headed so' % (own, ', '.join(sorted(told)), c.label()))
        elif not acts:
            hit = list(c.sents)
            note(name, own, '%s has no sentence headed by an act; the restriction is appended to %s'
                 % (c.origin, 'its sentence' if len(hit) == 1 else 'each of its %d sentences' % len(hit)))
        else:
            hit = [s for s in acts if set(s.heads) & set(names)]
            if not hit and c.title and c.title.rstrip('.') in names:
                hit = acts                    # the heading names the clause by its title
            if not hit:
                hit = acts
                if heads and len(acts) > 1:
                    note(name, own, "the heading '%s' names no act of %s (%s); the restriction is appended to all %d of its act sentences"
                         % (', '.join(heads), c.label(), '; '.join(', '.join(s.heads) for s in acts), len(acts)))
                elif heads and set(heads) != set(acts[0].heads):
                    note(name, own, "the heading '%s' differs from the heading '%s' of %s, which is kept"
                         % (', '.join(heads), ', '.join(acts[0].heads), c.label()))
        for s in hit:
            if not s.text.endswith('.'):
                note(name, own, 'a sentence of %s ends with no period; the restriction follows it' % c.origin)
                s.text += '.'
            s.text = s.text[:-1] + (', and ' if s.restricted else ', ') + rest + '.'
            s.restricted += 1
            s.rests.append(rest)
        if c.mark != mark:
            note(name, own, 'its mark %s differs from the mark %s of %s; the replaced clause takes the mark %s of the citing clause'
                 % (mark, c.mark, c.label(), mark))
            c.mark = mark
        if own not in c.labels:
            c.labels.append(own)
    return targets


def own_for(c, e, pairs):
    """The names c puts for names of the contract e it instantiates, its own: where c puts a kind for a kind ('an
    organisation for a ward'), the first name e's names lines give the kind replaced (Guardians' Wendy) is the name c's
    own names line gives the kind put, the last if it gives two, and that e does not name (Children's Wendy and Will are
    Warded Groups', so Children keeps them) (Udi, 2026-11-17, to Calculus #8)."""
    theirs, mine = [], []
    for s in e.names:
        p = nameslines.parse(s)
        theirs += p[0] if p else []
    for line in c.names_lines:
        for s in split_sentences(line):
            p = nameslines.parse(s)
            mine += p[0] if p else []
    named = {x for x, _ in theirs}
    out = {}
    for old, new in pairs:
        olds = [x for x, k in theirs if k == old and x not in out]
        ours = [x for x, k in mine if k == new and x not in named]
        if olds and ours:
            out[olds[0]] = ours[-1]
    return out


def expand(name, contracts, cache):
    if name in cache:
        return cache[name]
    c = contracts[name]
    exp = Expanded()
    sub = Subst(c.first.pairs, contracts)
    for dep in c.first.deps:
        e = copy.deepcopy(expand(dep, contracts, cache))
        if dep in c.first.inst:
            own = own_for(c, e, c.first.pairs)
            if own:
                rx = re.compile(r'(?<![A-Za-z])(%s)(?![A-Za-z])' % '|'.join(map(re.escape, own)))
                ren = lambda s: rx.sub(lambda m: own[m.group(1)], s) if s else s
                for d in e.defs:
                    d.text = ren(d.text)
                    d.here = [ren(h) for h in d.here]
                e.names = [ren(s) for s in e.names]
                for cl in e.clauses:
                    cl.title = ren(cl.title)
                    for s in cl.sents:
                        s.text = ren(s.text)
                        s.rests = [ren(x) for x in s.rests]
            for d in e.defs:
                d.text = sub(d.text)
                d.here = [sub(h) for h in d.here]
            e.names = [sub(s) for s in e.names]
            for cl in e.clauses:
                cl.title = sub(cl.title)
                for s in cl.sents:
                    s.heads = [sub(h) for h in s.heads] if s.heads else s.heads
                    s.text = sub(s.text)
                    s.rests = [sub(x) for x in s.rests]
            bind(name, dep, e.defs, c.first.binding, sub)
        for d in e.defs:                      # definitions: a sentence once
            same = [x for x in exp.defs if x.text == d.text]
            if same:
                for h in d.here:
                    if h not in same[0].here:
                        if same[0].here:
                            note(name, 'definitions', "'%s' is bound twice: %s; %s" % (d.text, '; '.join(same[0].here), h))
                        same[0].here.append(h)
            else:
                exp.defs.append(d)
        for s in e.names:
            if s not in exp.names:
                exp.names.append(s)
        for cl in e.clauses:                  # a clause inherited by two paths appears once
            same = [x for x in exp.clauses if x.origin == cl.origin]
            if not same:
                exp.clauses.append(cl)
            elif any(x.render() == cl.render() for x in same):
                continue
            elif not any(merge(x, cl) for x in same):
                # plain union (the task of 2026-11-23, point 6): two texts of one clause that are not one clause with
                # restrictions are two clauses of the union, as Retire of Orga and Retire of Wendy in Rehearsal Rooms
                note(name, cl.origin, 'two clauses of the union: [%s] and [%s]' % (same[0].render(), cl.render()))
                exp.clauses.append(cl)
    exp.inherited = len(exp.clauses)

    for k, line in enumerate(c.def_lines):
        for s in split_sentences(line):
            if not any(x.text == s for x in exp.defs):
                exp.defs.append(Def(s, name, k))
    for line in c.names_lines:
        for s in split_sentences(line):
            if s not in exp.names:
                exp.names.append(s)

    replaced = []
    for n, mark, text in c.clauses:
        cl = parse_clause(name, n, mark, text)
        own = '%s:%d' % (name, n)
        cites = [CITE.match(s.text) for s in cl.sents]
        cites = [None if m and m.group(2) == ', ' and m.group(3).startswith('without ') else m for m in cites]     # rule 7: a relaxation
        if not any(m and m.group(2) == ', ' for m in cites):
            if any(cites):                    # `X:n and Y:m apply ...`: a citation, but no restriction to append
                note(name, own, "cites %s with no restriction after a comma; it is kept as an own clause, unchanged: '%s'"
                     % (' and '.join(re.findall(REF, [m for m in cites if m][0].group(1))), cl.body()))
            exp.clauses.append(cl)
            exp.own += 1
            continue
        last, pending = [], []
        for s, m in zip(cl.sents, cites):
            if m and m.group(2) == ', ':
                last = restrict(name, n, mark, s.heads, cl.title, refs_of(m.group(1), contracts), m.group(3), exp, sub)
                for t in last:
                    if t not in replaced:
                        replaced.append(t)
                for p in pending:             # a sentence before the first citation
                    if last:
                        last[-1].sents.append(p)
                pending = []
            elif last:                        # a sentence that cites nothing: appended to the replaced clause
                last[-1].sents.append(Sent(s.heads, s.text))
                if len(last) > 1 or s.heads:
                    note(name, own, "the sentence '%s' is appended to %s" % (s.render(), last[-1].label()))
            else:
                pending.append(Sent(s.heads, s.text))
        if pending:
            fail('%s: a sentence that cites nothing has no replaced clause to go to' % own)
    exp.replaced = len(replaced)

    for d in exp.defs:                        # every X:n of the contract's own text resolves
        if d.origin == name:
            exp.cited += refs_of(d.text, contracts)
    for n, mark, text in c.clauses:
        exp.cited += refs_of(text, contracts)
    for r in dict.fromkeys(exp.cited):
        x = r.rsplit(':', 1)[0]
        if x not in contracts:
            fail('%s cites %s, and %s is no contract of rev/' % (name, r, x))
        elif not find(exp.clauses, r):
            fail('%s cites %s, which is no clause of the expanded %s' % (name, r, name))
    cache[name] = exp
    return exp


# ---------------------------------------------------------------- printing

def def_paragraphs(exp):
    """The definitions, a paragraph for each contract and line they are from."""
    out, key = [], None
    for d in exp.defs:
        text = ' '.join([d.text] + ['Here: %s.' % h for h in d.here])
        if (d.origin, d.line) == key:
            out[-1] += ' ' + text
        else:
            out.append(text)
            key = (d.origin, d.line)
    return out


KINDS = {}      # {contract: [(name, its kinds)]}: a name with two kinds neither of which is under the other


def names_para(c, exp):
    """The names lines of the chain, united: one names line, each name once with its most special kind, and what else
    they said, each once (tools/nameslines.py)."""
    rest = ' '.join([d.text for d in exp.defs] + [cl.render() for cl in exp.clauses])
    out, findings, _ = nameslines.unite(exp.names, [d.text for d in exp.defs], rest)
    KINDS[c.name] = findings
    return ' '.join(out)


def text_of(c, exp):
    lines = [HEADER, c.title_line] + def_paragraphs(exp)
    if exp.names:
        lines.append(names_para(c, exp))
    return '\n'.join(lines + [cl.render() for cl in exp.clauses]) + '\n'


def tex(line):
    return revtex.emit_plain(revtex.tokenize(line, True)).strip()


def box_of(c, raw, exp):
    out = [r'\begin{contractbox}{%s}' % revtex.emit_plain(raw.title).strip()]
    paras = def_paragraphs(exp) + ([names_para(c, exp)] if exp.names else [])
    out += [tex(p) + r'\par' for p in paras]
    if exp.clauses:
        out.append(r'\begin{description}[style=unboxed,leftmargin=1.2em,labelindent=0pt,labelsep=0.45em,itemsep=1pt,font=\normalfont\bfseries]')
        for cl in exp.clauses:
            mark = revtex.MARKS[cl.mark] + '~' if cl.mark else ''
            out.append(r'\item[%s] %s%s' % (tex(cl.label()), mark, tex(cl.body())))
        out.append(r'\end{description}')
    elif out[-1].endswith(r'\par'):
        out[-1] = out[-1][:-4]
    return out + [r'\end{contractbox}']


def stale(path, text):
    try:
        with open(path, encoding='utf-8') as h:
            return h.read() != text
    except OSError:
        return True


def main():
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--rev', default=os.path.join(root, 'rev'), help='the revised texts (default rev/)')
    ap.add_argument('--exp', default=os.path.join(root, 'exp'), help='the expanded texts (default exp/)')
    ap.add_argument('--out', default=os.path.join(root, 'sections', 'contracts-expanded.tex'))
    ap.add_argument('--check', action='store_true', help='write nothing; exit 1 on any failure')
    ap.add_argument('--notes', action='store_true', help='print each place where a rule did not decide')
    args = ap.parse_args()

    contracts, raw = read_all(args.rev)
    if not contracts:
        sys.exit('expand: no contract in %s' % args.rev)
    unparsed = link(contracts)
    loop = cycles(contracts)
    if loop:
        fail('a cycle of first lines: %s' % ' '.join(loop))
        for n in loop:                        # cut the cycle, so that the rest is still expanded
            contracts[n].first = First()
    names, _ = revtex.order(raw)

    cache, files = {}, {}
    out = [HEADER, r'\section{The Contracts Expanded}\label{app:contracts-expanded}', '', LEAD_IN]
    print('%-10s %-28s %9s %8s %4s %6s' % ('contract', 'written from', 'inherited', 'replaced', 'own', 'in all'))
    total = [0, 0, 0, 0]
    for name in names:
        c = contracts[name]
        exp = expand(name, contracts, cache)
        files[NM.file_of(name) + '.txt'] = text_of(c, exp)
        out += [''] + box_of(c, raw[name], exp)
        row = (exp.inherited, exp.replaced, exp.own, len(exp.clauses))
        total = [a + b for a, b in zip(total, row)]
        print('%-10s %-28s %9d %8d %4d %6d' % ((name, ' '.join(c.first.deps) or '-') + row))
    print('%-10s %-28s %9d %8d %4d %6d' % (('%d contracts' % len(names), '') + tuple(total)))
    text = '\n'.join(out) + '\n'

    if args.check:
        if not os.path.isdir(args.exp):
            fail('%s is not there; run expand.py' % args.exp)
        else:
            for f in sorted(files):
                if stale(os.path.join(args.exp, f), files[f]):
                    fail('%s is not what expand.py prints' % os.path.join(args.exp, f))
            for f in sorted(os.listdir(args.exp)):
                if f.endswith('.txt') and f not in files:
                    fail('%s is of no contract in rev/' % os.path.join(args.exp, f))
        if stale(args.out, text):
            fail('%s is not what expand.py prints' % args.out)
    else:
        os.makedirs(args.exp, exist_ok=True)
        for f in sorted(os.listdir(args.exp)):
            if f.endswith('.txt') and f not in files:
                os.remove(os.path.join(args.exp, f))
        for f in sorted(files):
            with open(os.path.join(args.exp, f), 'w', encoding='utf-8') as h:
                h.write(files[f])
        with open(args.out, 'w', encoding='utf-8') as h:
            h.write(text)

    cited = sum(len(dict.fromkeys(e.cited)) for e in cache.values())
    print('checks: %d contracts; %d first lines read, %d not%s; %d citations X:n; cycles: %s; notes: %d; failures: %d'
          % (len(names), sum(1 for n in names if contracts[n].first_line) - len(unparsed), len(unparsed),
             ' (%s)' % ' '.join(unparsed) if unparsed else '', cited, ' '.join(loop) or 'none', len(NOTES), len(FAILURES)))
    two = [(n, x, ks) for n in names for x, ks in KINDS.get(n, [])]
    print('names: %d contracts, one names line each; %d names of two kinds neither of which is under the other' % (len(names), len(two)))
    for n, x, ks in two:
        print('kinds: %s: %s is %s' % (n, x, ' and '.join("%s ('%s')" % k for k in ks)))
    if args.notes:
        for n in NOTES:
            print('note: %s, %s: %s' % n)
    for w in revtex.WARNINGS:
        print('warning: ' + w, file=sys.stderr)
    for f in FAILURES:
        print('FAIL: ' + f, file=sys.stderr)
    if not args.check:
        print('%s and %s: %d contracts' % (args.exp, args.out, len(names)))
    if FAILURES:
        sys.exit(1)


if __name__ == '__main__':
    main()
