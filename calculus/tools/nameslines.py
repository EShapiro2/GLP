#!/usr/bin/env python3
"""nameslines.py --- the one names line of a full contract.

A full contract (exp/, flat/, gx/) inherits the names line of each contract of its chain.  They are united here into
one names line: each name once, with its most special kind (Udi, 2026-11-17, to Calculus #8: "if same name used for
the same role, should be stated once, and if different roles, a different name should be used").

A kind is more special than another by BASE and by the definitions of the full contract, "A *networked organisation*
is a backed community currency ...": the kind defined is under the first kind its defining phrase opens with, an
adjective or two skipped.  A name with two kinds neither of which is under the other is a finding.

What else stood on a names line keeps its place, as a sentence of its own: "Bob and Dave possibly one person" is
"Bob and Dave are possibly one person.", "; in clause 4 Alice and Bob are possibly one person" is "In clause 4 Alice
and Bob are possibly one person."; a sentence that is no list of names (the definitions of a party and a person, "A
person can be either a rider or a driver but not both.") stays as it is.  Each such sentence appears once.

Standard library only.
"""
import re

NAME = r"[A-Z][a-z]*"
NAMES_SENT = re.compile(r"^%s(?:, %s)*(?:,? (?:and|or) %s)? (?:is|are) " % (NAME, NAME, NAME))

# The kinds the contracts do not define by "A *K* is a ...": Udi's and Calculus #8's (2026-11-17): a person is a party,
# a rider or a driver a person, a child, an AI agent, an organisation and a service a ward.
BASE = {'person': 'party', 'ward': 'party', 'rider': 'person', 'driver': 'person',
        'child': 'ward', 'AI agent': 'ward', 'organisation': 'ward', 'service': 'ward'}

IRREGULAR = {'child': 'children', 'person': 'persons', 'party': 'parties'}


def plural(k):
    *pre, w = k.split(' ')
    if w in IRREGULAR:
        p = IRREGULAR[w]
    elif re.search(r'(?:s|x|ch|sh)$', w):
        p = w + 'es'
    elif re.search(r'[^aeiou]y$', w):
        p = w[:-1] + 'ies'
    else:
        p = w + 's'
    return ' '.join(pre + [p])


def singular(k):
    *pre, w = k.split(' ')
    inv = {v: s for s, v in IRREGULAR.items()}
    if w in inv:
        s = inv[w]
    elif w.endswith('ies'):
        s = w[:-3] + 'y'
    elif re.search(r'(?:s|x|ch|sh)es$', w):
        s = w[:-2]
    elif w.endswith('s') and not w.endswith('ss'):
        s = w[:-1]
    else:
        s = w
    return ' '.join(pre + [s])


def article(k):
    an = k[0].lower() in 'aeio' or re.match(r'^[AEFHILMNORSX][A-Z]', k)
    return 'an' if an else 'a'


def split_sentences(text):
    """The sentences of a paragraph: a break after . ? ! before a capital or an *italic* term (expand.py's)."""
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


def is_names(s):
    return '*' not in s and bool(NAMES_SENT.match(s))


AGREE = re.compile(r"\b(?:%s, )*%s and %s agree\b" % (NAME, NAME, NAME))
POSS = re.compile(r"^(?:%s|one person|any of the others)(?: or (?:%s|one person|any of the others))*$" % (NAME, NAME))


def sentence_of(part):
    """'Bob and Dave possibly one person' -> 'Bob and Dave are possibly one person.'"""
    part = part.strip()
    m = re.match(r"^((?:%s, )*%s(?: (?:and|or) %s)?) possibly " % (NAME, NAME, NAME), part)
    if m:
        verb = 'are' if ' and ' in m.group(1) or ', ' in m.group(1) else 'is'
        part = '%s %s %s' % (m.group(1), verb, part[len(m.group(1)) + 1:])
    part = part[0].upper() + part[1:]
    return part if part.endswith('.') else part + '.'


def parse(s):
    """A names sentence -> ([(name, kind)], [the sentences of what else it says]), or None if s is no names sentence."""
    if not is_names(s):
        return None
    s = s.rstrip('.')
    seg, _, tail = s.partition('; ')
    seg = AGREE.sub(lambda m: m.group(0).replace(', ', '\x00').replace(' and ', '\x01'), seg)
    parts = re.split(r"(, (?=[A-Z])| and (?=[A-Z]))", seg)
    items, seps = parts[0::2], [''] + parts[1::2]
    back = lambda x: x.replace('\x00', ', ').replace('\x01', ' and ')
    pairs, extras, pending, poss = [], [], [], None
    for item, sep in zip(items, seps):
        item = back(item)
        if poss is not None and POSS.match(item):
            poss += sep + item
            continue
        if poss is not None:
            extras.append(poss)
            poss = None
        if re.fullmatch(NAME, item):
            pending.append((sep, item))
            continue
        m = re.match(r"^(%s)(?: (is|are))?(?: (an?))? (.+)$" % NAME, item)
        if m and re.match(r"(?:possibly|or) ", m.group(4)) is None and not m.group(4).startswith('possibly'):
            names = [x for _, x in pending] + [m.group(1)]
            k = m.group(4)
            if not m.group(3) and (len(names) > 1 or m.group(2) == 'are'):
                k = ' or '.join(singular(x) for x in k.split(' or '))
            pairs += [(x, k) for x in names]
            pending = []
        else:
            poss = ''.join((sp if i else '') + x for i, (sp, x) in enumerate(pending)) + (sep if pending else '') + item
            pending = []
    if poss is not None:
        extras.append(poss)
    if pending:
        extras.append(''.join((sp if i else '') + x for i, (sp, x) in enumerate(pending)))
    out = [sentence_of(x) for x in extras]
    if tail:
        out.append(sentence_of(tail))
    return pairs, out


def order_of(defs):
    """{kind: [the kinds it is under]}: BASE and the definitions 'A *K* is a [adjective] K2 ...[ that is a K3]' of the
    sentences defs."""
    up = {k: [v] for k, v in BASE.items()}
    found = []
    for s in defs:
        for m in re.finditer(r"\b[Aa]n? \*([^*]+)\* is an? ([^,;.]+)", s):
            for i, phrase in enumerate(re.split(r" that is an? ", m.group(2))):
                found.append((m.group(1), phrase, i))
    known = set(up) | set(BASE.values()) | {k for k, _, _ in found}
    for k, phrase, i in found:
        if k in BASE:
            continue
        ws = phrase.split(' ')
        best = None
        for a in range(3 if i == 0 else 1):
            for b in range(len(ws), a, -1):
                cand = ' '.join(ws[a:b])
                if cand in known and cand != k:
                    best = cand
                    break
            if best:
                break
        if best and best not in up.setdefault(k, []):
            up[k].append(best)
    return up


def under(a, b, up, seen=None):
    """a is b or under it."""
    if a == b:
        return True
    seen = seen or set()
    seen.add(a)
    return any(under(x, b, up, seen) for x in up.get(a, []) if x not in seen)


def unite(sentences, defs=(), text=None):
    """The sentences of the names lines of a chain -> (the sentences that replace them, the findings).  The first is
    the one names line; then what else they said, each once, in their order.  defs: the definitions, for the order of
    kinds; text: the rest of the full contract, if given a name it does not mention is left out."""
    up = order_of(list(defs) + list(sentences))
    kinds, first, other, findings, src = {}, [], [], [], {}
    for s in sentences:
        p = parse(s)
        if p is None:
            if s not in other:
                other.append(s)
            continue
        pairs, rest = p
        for x, k in pairs:
            if x not in kinds:
                kinds[x] = []
                first.append(x)
            if k not in kinds[x]:
                kinds[x].append(k)
                src[(x, k)] = s
        for r in rest:
            if r not in other:
                other.append(r)
    best = {}
    for x in first:
        ks = kinds[x]
        low = [k for k in ks if all(under(k, j, up) for j in ks)]
        if low:
            best[x] = low[0]
        else:
            mins = [k for k in ks if not any(j != k and under(j, k, up) for j in ks)]
            findings.append((x, [(k, src[(x, k)]) for k in mins]))
            best[x] = mins[-1]      # the kind of the last names line that names it, the contract's own
    names = [x for x in first if text is None or re.search(r'(?<![A-Za-z])%s(?![a-z])' % x, text)]
    left = [x for x in first if x not in names]
    groups = []
    for x in names:
        g = [gr for gr in groups if gr[0] == best[x]]
        if g:
            g[0][1].append(x)
        else:
            groups.append((best[x], [x]))
    out = []
    for i, (k, xs) in enumerate(groups):
        who = xs[0] if len(xs) == 1 else ', '.join(xs[:-1]) + ' and ' + xs[-1]
        if len(xs) == 1:
            kind = k if re.match(r'^(?:something|a number of)\b', k) else '%s %s' % (article(k), k)
        else:
            kind = ' or '.join(plural(x) for x in k.split(' or '))
        out.append(('%s %s %s' % (who, 'is' if len(xs) == 1 else 'are', kind)) if i == 0 else '%s %s' % (who, kind))
    line = [', '.join(out) + '.'] if out else []
    return line + other, findings, left
