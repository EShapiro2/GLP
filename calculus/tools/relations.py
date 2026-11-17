#!/usr/bin/env python3
"""relations.py [NAME ...] [--json] [--root DIR] [--schemas DIR] --- a verdict for each relation a contract's first line states, on the schemas.

ROOT is the tree of the tools (gr/ and sasx/), or, with --root DIR, DIR: a tree so, or a publication directory, as
tools/publish.py writes it, where the first line is read from NAME/logic-pragmas.txt (or NAME/contract.txt) and the
schemas from NAME/full-schemas.txt.  tools/publish.py writes its lines for each contract into NAME/relations.txt,
and tools/adopt.py into the owner's record, cascade/NAME/relations.txt.

For each contract NAME of gr/ (or those named), it reads the relations of its first line (gr/NAME.txt, line 2), in
their order, and checks them on the expanded schemas: sasx/NAME.txt against sasx/B.txt of each contract B the line
names.  It reads the schemas as they are written; it does not run tools/gcsas.py and does not use how gcsas.py made
them.  One line a relation:
    NAME extends Bonds: holds
    NAME restricts Bonds: FAILS: <act>: <what breaks it>
and, with --json, a list of {"contract", "relation", "base", "holds", "failures"} for the site.  Exit 1 if any fails.

The paper is the Calculus (branch main, sections/01-contracts-schemas.tex): Definition def:relations, its items
1 to 5; a first line with several relations "applies each to the result of the one before"; an act of a strengthened
contract is, the states the strengthening adds left out, an act of the contract cited.  A schema bears the label of
its clause; an act of the base is found in NAME's schemas by the first label of its own label and by its name.  The
base of the relations after the first is the result of those before it: for 'B instantiates C with s, and restricts
and extends it', C's schemas instantiated; for 'composes it with D', the composition.

What is checked, for each act b of the base, against the act s of NAME with b's first label and b's name (or, where a
citation names it anew, the one act of that label no act of the base names), and for each act of NAME no act of the
base has:

extends B (def:relations 1, the schemas S u phi(A)):  every act of B is in NAME, as it is or as the other relations of
    the first line change it; an act of NAME that is not of B is allowed.  FAILS: an act of B is not in NAME, or is
    changed in a way no relation of the first line allows.
restricts B (def:relations 2, H <- B, i(c+), ~i(c-) guarded by G u i(P)):  an act of B in NAME has B's head, B's body
    with literals added, and B's guards with guards added; nothing of B's act is taken away.  FAILS: an act whose head
    differs from B's (and the line does not strengthen B), or that lacks a literal or a guard of B's act; or, where
    the line does not extend B, an act of NAME that is not of B.
strengthens B (def:relations 3, H, i(e+), ~i(e-) <- B, i(e-) guarded by G, e of basic states B does not have):  an
    act of B in NAME has B's head with literals added, each of a predicate that no schema of B has, and B's body with
    the atoms with no wildcard that the added literals delete; any other literal added to the body is a restriction.
    FAILS: an added effect of a predicate of B, or a literal or a guard of B's act taken away.
instantiates B with s (def:relations 4):  s is read from the first line's 'X for Y': a word for a word (an
    organisation for a ward) is put for it in the names of acts and in the predicates and roles, as has_officer for
    has_guardian; a predicate of B that NAME's schemas no longer have is one s defines, and its atoms are taken out of
    B's act and those of predicates that B does not have allowed in its place; a role that NAME no longer has is
    likewise one s puts, and NAME's act may have any guard in its place.  Where B's act had atoms of a predicate s
    defines, the literals NAME's act adds in their place may be of any predicate (s may define a state by states B
    has, as 'x is a member of z' for 'x is entitled to propose m in z'), so such an act is checked only for what it
    keeps of B.  Then each act of B is in NAME as the other relations of the line allow.  FAILS: an act of B
    with no act in NAME, or a literal of B's act, not one s defines, that NAME's act does not have.
composes B with C (def:relations 5):  the base is the union of the schemas of B and C, two acts that both have from
    one act of a contract both are written from (the same first label and name) being one: the head literals and
    guards of both, the conditions of both and the atoms its head deletes.  FAILS: an act of the union that NAME does
    not have, as the line's other relations allow.  'extends B and C', two contracts, is their union so too.

Defaults (gr/: 'Default: STATE for PARAMETER.'; NOTES-GRAMMAR.md, 'Defaults of parameters'): a base is printed at
    its defaults, and an instance whose first line binds a parameter with a default replaces the default.  So for an
    instantiation the conditions of the base act of the predicates of such a default (gcsas.py's default_items and
    default_atoms, as 'officer(x,z) for proposer(x,z,m)') are, where the instance's act lacks them, taken out as a
    predicate s defines is, and the instance's own states allowed in their place; an instance that takes the default
    keeps them, and is checked for them.  So too for a contract that gives a parameter of its base a Default of its own
    (gcsas.py: a Default for a parameter declared up the chain overrides the parent's), whatever its relation: the
    base's default conditions it lacks are taken out, and the conditions of its own default allowed in their place.

Not checked: derived acts ('<Act> for <Role>', 'Vote of ...'), which no item of def:relations names, are matched by
label and name only; kept sentences; that a restriction's added literals are what its clause says (the clauses are
not read); the pairs of names possibly one, and 'taken for'.
Standard library only.
"""
import json, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM      # the names of the contracts and their files (tools/names.py, NAMES.txt)
from collections import Counter

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
VERBS = ('extends', 'restricts', 'strengthens', 'instantiates', 'composes', 'relaxes')
DERIVED = re.compile(r'^(?:.+ for .+|Vote of .+)$')


# ---- reading schemas ----------------------------------------------------------------------------------------------

def split_top(s):
    """'a(x,y), b(z)' -> ['a(x,y)', 'b(z)']: commas outside parentheses."""
    out, depth, cur = [], 0, ''
    for ch in s:
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
        if ch == ',' and depth == 0:
            out.append(cur.strip())
            cur = ''
        else:
            cur += ch
    if cur.strip():
        out.append(cur.strip())
    return out


LINE = re.compile(r'^((?:%s)(?:\+%s)*) ([■◆○-]) (.*)$' % (NM.REF, NM.REF))      # a label: names of one or two words (tools/names.py)


_cache = {}


SCHEMAS = 'sasx'


def schemas(n, d=None):
    d = d or SCHEMAS
    if (n, d) not in _cache:
        _cache[(n, d)] = _schemas(n, d)
    s = _cache[(n, d)]
    return None if s is None else [dict(a, head=Counter(a['head']), body=Counter(a['body']), guards=set(a['guards']), src=list(a['src'])) for a in s]


def published():
    """True where ROOT is a publication directory (tools/publish.py), NAME/ for each contract, and not a tree with gr/."""
    return not os.path.isdir(os.path.join(ROOT, 'gr'))


def path(n, what):
    """The file of contract n: 'first' its Logic pragmas (gr/NAME.txt, or NAME/logic-pragmas.txt, or else
    NAME/contract.txt), whose second line is the first line; 'schemas' its expanded schemas (SCHEMAS/NAME.txt, or
    NAME/full-schemas.txt)."""
    if not published():
        keep = os.path.join(ROOT, 'sasx-all', NM.file_of(n) + '.txt')      # the untakable acts kept (gcsas.py): a first line is judged on the acts before they are left out
        if what == 'schemas' and SCHEMAS == 'sasx' and os.path.exists(keep):
            return keep
        return os.path.join(ROOT, 'gr' if what == 'first' else SCHEMAS, NM.file_of(n) + '.txt')
    if what == 'schemas':
        return os.path.join(ROOT, NM.file_of(n), 'full-schemas.txt')
    p = os.path.join(ROOT, NM.file_of(n), 'logic-pragmas.txt')
    return p if os.path.exists(p) else os.path.join(ROOT, NM.file_of(n), 'contract.txt')


def contracts():
    if published():
        return sorted(NM.name_of(d) for d in os.listdir(ROOT) if os.path.exists(os.path.join(ROOT, d, 'full-schemas.txt')))
    return NM.names_in(os.path.join(ROOT, 'gr'))


def _schemas(n, d):
    """The acts of the expanded schemas of n, in order: dicts with label, labels, first, mark, name, head, body
    (Counters of literals), guards (a set)."""
    p = path(n, 'schemas')
    out = []
    if not os.path.exists(p):
        return None
    for l in open(p, encoding='utf-8'):
        m = LINE.match(l.rstrip('\n'))
        if not m or m.group(3).startswith('| '):
            continue
        name, _, rest = m.group(3).partition(': ')
        rest = re.split(r' possibly | taken for | votes ', rest.rstrip('.'))[0]
        head, _, rest = rest.partition(' <- ')
        if rest.startswith('guarded by '):
            body, g = '', rest[len('guarded by '):]
        else:
            body, _, g = rest.partition(' guarded by ')
        g = re.split(r' possibly | taken | votes ', g)[0]
        out.append({'label': m.group(1), 'labels': m.group(1).split('+'), 'first': m.group(1).split('+')[0],
                    'mark': m.group(2), 'name': name, 'head': Counter(split_top(head)),
                    'body': Counter(split_top(body)), 'guards': set(split_top(g)), 'src': [n]})
    return out


def pred(lit):
    return re.match(r'~?([A-Za-z_][A-Za-z0-9_]*)', lit).group(1)


def preds(acts):
    return {pred(x) for a in acts for x in list(a['head']) + list(a['body']) if re.match(r'~?[a-z_]', x)}


def roles(acts):
    return {re.match(r'([a-z_]+)\(', g).group(1) for a in acts for g in a['guards'] if '(' in g}


# ---- reading the first line ---------------------------------------------------------------------------------------

def first_line(n):
    """[(verb, [bases], sigma text)] of gr/n.txt's first line, in order; 'it', 'them' and 'the composition' are
    None (the result of the relations before)."""
    if not os.path.exists(path(n, 'first')):
        return []
    lines = [l.rstrip('\n') for l in open(path(n, 'first'), encoding='utf-8')]
    fl = lines[1] if len(lines) > 1 else ''
    if not fl.startswith(n + ' ') or not any(' %s ' % v in ' ' + fl + ' ' for v in VERBS):
        return []
    fl = re.sub(r'\s*\([^()]*\)', '', fl[len(n) + 1:]).rstrip('.').strip()
    toks = re.split(r'\b(%s)\b' % '|'.join(VERBS), fl)
    steps, pend = [], []
    for i in range(1, len(toks), 2):
        verb, obj = toks[i], toks[i + 1].strip()
        obj = re.sub(r'^(and|,)\s*', '', obj).strip()
        obj = re.sub(r',?\s*(and)?\s*,?$', '', obj).strip()
        if not obj:
            pend.append(verb)
            continue
        sigma = ''
        if verb == 'instantiates' and ' with ' in obj:
            obj, sigma = obj.split(' with ', 1)
        with_ = None
        if verb == 'composes' and ' with ' in obj:
            obj, with_ = obj.split(' with ', 1)
        bases = None if obj in ('it', 'them', 'the composition') else [b.strip() for b in re.split(r',\s*|\s+and\s+', obj) if b.strip()]
        for v in pend:
            steps.append((v, bases, ''))
        pend = []
        if verb == 'composes':
            steps.append((verb, bases, with_.strip().rstrip(',')))
        else:
            steps.append((verb, bases, sigma))
    return steps


def words(sigma):
    """The words s puts for words: [('organisation', 'ward'), ...] from 'an organisation for a ward', 'a parent for a
    guardian'; a phrase longer than two words (a state or a role) is not a word."""
    out = []
    for part in re.split(r',\s*|;\s*|\s+and\s+', sigma):
        m = re.fullmatch(r'(?:an?|the)\s+([A-Za-z][A-Za-z ]*?)\s+for\s+(?:an?|the)\s+([A-Za-z][A-Za-z ]*?)', part.strip())
        if m and len(m.group(1).split()) <= 2 and len(m.group(2).split()) <= 2:
            out.append((m.group(1), m.group(2)))
    return out


# ---- the operations on the base -----------------------------------------------------------------------------------

def merge(a, b):
    """The union of two sets of schemas, an act both have from one act (the same first label and name) being one."""
    out = [dict(x, head=Counter(x['head']), body=Counter(x['body']), guards=set(x['guards']), src=list(x['src'])) for x in a]
    pair = {}
    for k, y in enumerate(b):
        x = next((i for i, x in enumerate(out) if x['first'] == y['first'] and x['name'] == y['name']), None)
        if x is not None:
            pair[k] = x
    for lab in {y['first'] for y in b}:          # an act one side names anew by a citation: the one act left of its label on each side
        ys = [k for k, y in enumerate(b) if y['first'] == lab and k not in pair and not DERIVED.match(y['name'])]
        xs = [i for i, x in enumerate(out) if x['first'] == lab and i not in pair.values() and not DERIVED.match(x['name'])
              and not any(y['name'] == x['name'] and y['first'] == lab for y in b)]
        sib = lambda x, y: len(x['labels']) > 1 and len(y['labels']) > 1 and x['labels'][1] != y['labels'][1] and (covered(x, y) or covered(y, x))
        if len(ys) == 1 and len(xs) == 1 and not sib(out[xs[0]], b[ys[0]]):
            pair[ys[0]] = xs[0]         # but two citations of one act by two clauses, one making the other redundant, are two acts, the one removed (Udi, 2026-11-16)
    for k, y in enumerate(b):
        x = out[pair[k]] if k in pair else None
        if x is None:
            out.append(dict(y, src=list(y['src'])))
            continue
        if len(y['labels']) > len(x['labels']):          # the name a citation gives
            x['name'], x['label'], x['labels'] = y['name'], y['label'], y['labels']
        x['src'] = x['src'] + [c for c in y['src'] if c not in x['src']]
        deleted = lambda s: Counter({l[1:]: c for l, c in s['head'].items() if l.startswith('~') and '_' not in split_args(l)})
        cx, cy = x['body'] - deleted(x), y['body'] - deleted(y)
        x['head'] = x['head'] | y['head']
        x['guards'] = x['guards'] | y['guards']
        x['body'] = (cx | cy) | deleted(x)
    return out


def split_args(lit):
    m = re.search(r'\((.*)\)', lit)
    return split_top(m.group(1)) if m else []


def substitute(acts, ws):
    """The base's acts with the words of s put for its words, in names, predicates and roles."""
    if not ws:
        return acts

    def w_name(s):
        for new, old in ws:
            s = re.sub(r'\b%s\b' % re.escape(old.capitalize()), new.capitalize(), s)
            s = re.sub(r'\b%s\b' % re.escape(old), new, s)
            s = re.sub(r'\b%ss\b' % re.escape(old.capitalize()), new.capitalize() + 's', s)
        return s

    def w_pred(lit):
        neg = lit.startswith('~')
        p = pred(lit)
        q = p
        for new, old in ws:
            q = re.sub(r'(^|_)%s($|_)' % re.escape(old.replace(' ', '_')), r'\g<1>%s\g<2>' % new.replace(' ', '_').lower(), q)
        return ('~' if neg else '') + q + lit[len(p) + neg:]

    out = []
    for a in acts:
        out.append(dict(a, name=w_name(a['name']),
                        head=Counter({w_pred(l): c for l, c in a['head'].items()}),
                        body=Counter({w_pred(l): c for l, c in a['body'].items()}),
                        guards={w_pred(g) if '(' in g else g for g in a['guards']}))
    return out


# ---- matching and the verdicts ------------------------------------------------------------------------------------

def match(base, mine):
    """{index of a base act: index of NAME's act}: by first label and name; then, within a label, the acts left in
    order where as many are left on each side."""
    got, used = {}, set()
    for i, b in enumerate(base):
        j = next((j for j, a in enumerate(mine) if j not in used and a['first'] == b['first'] and a['name'] == b['name']), None)
        if j is not None:
            got[i] = j
            used.add(j)
    for lab in {b['first'] for b in base}:
        bi = [i for i, b in enumerate(base) if b['first'] == lab and i not in got]
        mj = [j for j, a in enumerate(mine) if a['first'] == lab and j not in used and not any(x['name'] == a['name'] for x in base)]
        if bi and len(bi) != len(mj):          # the act a citation renames beside the ward's forms and vote acts this contract derives of it (Shares's Issue shares)
            bi2 = [i for i in bi if not DERIVED.match(base[i]['name'])]
            mj2 = [j for j in mj if not DERIVED.match(mine[j]['name'])]
            if bi2 and len(bi2) == len(mj2):
                bi, mj = bi2, mj2
        if bi and len(bi) == len(mj):
            for i, j in zip(bi, mj):
                got[i] = j
                used.add(j)
    return got


def relaxed_of(b, a):
    """Whether NAME's act a is the base act b relaxed (Udi, 2026-11-16, to Calculus #7): under a renaming of b's names
    (two may become one, "if Carol is Bob"), b's head is a's, and a has fewer conditions or fewer consents than b, its
    other conditions (those after "if") allowed.  The literals a lacks, or None."""
    nm = lambda x: re.findall(r'[A-Z][A-Za-z]*', x)
    shape = lambda x: re.sub(r'[A-Z][A-Za-z]*', 'N', x)
    bh, ah = sorted(b['head']), sorted(a['head'])

    def ext(ls, targets, s):
        if not ls:
            yield s
            return
        l, rest = ls[0], ls[1:]
        for t in targets:
            if shape(l) != shape(t):
                continue
            u, ok = dict(s), True
            for x, y in zip(nm(l), nm(t)):
                if u.setdefault(x, y) != y:
                    ok = False
                    break
            if ok:
                yield from ext(rest, targets, u)
    ren = lambda x, s: re.sub(r'[A-Z][A-Za-z]*', lambda m: s.get(m.group(0), m.group(0)), x)
    for s in ext(list(set(bh)), list(set(ah)), {}):
        if {ren(l, s) for l in bh} != set(ah):
            continue
        lost = sorted({ren(l, s) for l in b['body']} - set(a['body'])) + sorted({ren(g, s) for g in b['guards']} - set(a['guards']))
        if lost:
            return lost
    return None


def covered(b, a):
    """Whether NAME's act a makes the base act b redundant (Udi, 2026-11-16: a redundant act is removed): under a one to
    one renaming of b's names b's head is a's, and a's conditions and guards are among b's."""
    nm = lambda x: re.findall(r'[A-Z][A-Za-z]*', x)
    shape = lambda x: re.sub(r'[A-Z][A-Za-z]*', 'N', x)
    ren = lambda x, s: re.sub(r'[A-Z][A-Za-z]*', lambda m: s.get(m.group(0), m.group(0)), x)

    def ext(ls, targets, s):
        if not ls:
            yield s
            return
        l, rest = ls[0], ls[1:]
        for t in targets:
            if shape(l) != shape(t):
                continue
            u, ok = dict(s), True
            for x, y in zip(nm(l), nm(t)):
                if x in u and u[x] != y or x not in u and y in u.values():
                    ok = False
                    break
                u[x] = y
            if ok:
                yield from ext(rest, targets, u)
    for s in ext(sorted(set(b['head'])), sorted(set(a['head'])), {}):
        if {ren(l, s) for l in b['head']} == set(a['head']) and set(a['body']) <= {ren(l, s) for l in b['body']} and \
                set(a['guards']) <= {ren(g, s) for g in b['guards']}:
            return True
    return False


def wild(lit):
    return '_' in split_args(lit)


_world = {}


def gworld(n):
    """gcsas.py's World of this tree (overridden builds it), and its root."""
    if 'w' not in _world:
        overridden([], n)
    return _world['w'], _world['root']


def kind_forms(base, n):
    """The base with each act of notes (Notes's Issue, Swap, Pay, Escrow, whose atoms are of notes) put as its forms for
    the kinds of note n has (gcsas.py's kind_forms, 'Kind of note: ...' of the Logic pragmas): a coin, a bond and a share
    are special cases of a note, and an act of notes is in n as one act for each kind of each note it moves."""
    import gcsas
    try:
        w, root = gworld(n)
    except Exception:
        return base
    saved = gcsas.ROOT
    gcsas.ROOT = root
    try:
        if n not in w.known or not gcsas.kinds_of(w, n):
            return base
        lit = lambda l: gcsas.parse_atom(l[1:]) if l.startswith('~') else gcsas.parse_atom(l)
        neg = lambda l: (True,) + lit(l)[1:] if l.startswith('~') else (False,) + lit(l)[1:]
        es = [dict(kind='act', label=b['label'], name=b['name'], head=[neg(l) for l in b['head'].elements()],
                   body=[neg(l) for l in b['body'].elements()], guards=sorted(b['guards']), rel=b) for b in base]
        out = []
        for e in gcsas.kind_forms(w, n, es):
            if e.get('generic'):
                continue
            b = e['rel']
            if not e.get('kinds'):
                out.append(b)
                continue
            out.append(dict(b, name=e['name'], head=Counter(gcsas.lit_s(l) for l in e['head']),
                            body=Counter(gcsas.lit_s(l) for l in e['body'])))
        return out
    except gcsas.Err:
        return base
    finally:
        gcsas.ROOT = saved


def overridden(bases, n, both=False):
    """The predicates of the defaults of the bases' parameters that n's first line binds otherwise (tools/gcsas.py's
    default_items and default_atoms: 'officer(x,z) for proposer(x,z,m)'): the base's schemas are printed at its
    defaults, and an instance that binds such a parameter puts its own states where the default's are."""
    import gcsas
    if 'w' not in _world:
        tmp = None
        if published():         # a publication directory: its Logic pragmas as gr/ in a temporary tree
            import tempfile, shutil
            tmp = tempfile.mkdtemp(prefix='relations-')
            os.makedirs(os.path.join(tmp, 'gr'))
            for c in contracts():
                p = path(c, 'first')            # NAME/logic-pragmas.txt, or NAME/contract.txt where there is none
                if os.path.exists(p):
                    shutil.copyfile(p, os.path.join(tmp, 'gr', NM.file_of(c) + '.txt'))
        saved = gcsas.ROOT
        gcsas.ROOT = tmp or ROOT
        try:
            _world['w'] = gcsas.World(NM.names_in(os.path.join(gcsas.ROOT, 'gr')))
            _world['root'] = gcsas.ROOT
        finally:
            gcsas.ROOT = saved
    w = _world['w']
    saved = gcsas.ROOT
    gcsas.ROOT = _world['root']
    try:
        def params(c):
            out = {}
            items = gcsas.default_items(w, c)
            for a in gcsas.default_atoms(w, c, items) if items else []:
                new, _, par = a.rpartition(' for ')
                out[pred(par)] = {pred(x) for x in split_top(new) if re.match(r'~?[a-z_]', x)}
            return out
        mine = params(n)
        out, own = set(), set()
        for b in bases:
            for par, ps in params(b).items():
                if mine.get(par) != ps:         # bound by n's first line, or given another default by n's own Default line
                    out |= ps
                    own |= mine.get(par, set())
        return (out, own) if both else out
    except gcsas.Err:          # a tree whose contracts gcsas.py cannot read, as a case of schemas written by hand: no defaults
        return (set(), set()) if both else set()
    finally:
        gcsas.ROOT = saved


def verdicts(n):
    steps = first_line(n)
    mine = schemas(n)
    if not steps or mine is None:
        return []
    stated = {}
    for v, bs, _ in steps:
        for b in bs or [None]:
            stated.setdefault(v, set()).add(b)
    # the base, step by step
    base, cur, inst = None, [], {}
    names = []          # the contracts the base is made of
    resolved = []       # the bases of each step, 'it' read as the contracts before
    for v, bs, extra in steps:
        resolved.append(list(bs) if bs else list(names))
        if v == 'instantiates':
            b = bs[0]
            s = schemas(b)
            if len(bs) > 1:
                for c in bs[1:]:
                    s = merge(s, schemas(c))
            ws = words(extra)
            base = substitute(s, ws)
            inst = {'from': bs, 'words': ws, 'gone': preds(base) - preds(mine), 'roles_gone': roles(base) - roles(mine),
                    'base_preds': preds(base), 'defaulted': overridden(bs, n)}
            names = bs
        elif v == 'composes':
            b = bs if bs else None
            first = base if b is None else merge_all(b)
            withs = [x for x in re.split(r',\s*|\s+and\s+', extra) if x]      # 'GA composes GPE with GAS and GDS': the composition with each (Helper #5, 2026-11-16)
            base = merge(first, merge_all(withs))
            names = (names if b is None else b) + withs
            if inst:            # the states the instance puts for states bind the contract composed with it (SC's bylaws, Shares's issue of shares)
                ex = preds(merge_all(withs) or [])
                inst['gone'] |= ex - preds(mine)
                inst['base_preds'] |= ex
                inst['defaulted'] |= overridden(withs, n)
        elif bs:
            add = merge_all([x for x in bs if x not in names])
            if add:
                base = add if base is None else merge(base, add)
                names = names + [x for x in bs if x not in names]
    base = kind_forms(base or [], n)
    owndef = overridden(names, n, both=True) if names and not inst else (set(), set())
    bpreds = preds(base)
    m = match(base, mine)
    fails = {}

    def fail(rel, msg):
        fails.setdefault(rel, []).append(msg)

    rel_ext = 'extends' in stated
    rel_res = 'restricts' in stated
    rel_str = 'strengthens' in stated
    rel_rlx = 'relaxes' in stated
    # The acts of NAME a relaxation added: of a base act's first label, matched to no base act, and the base act relaxed
    # (relaxed_of).  On a line that says "relaxes" they hold, and a base act they make redundant may be gone (removed,
    # Udi, 2026-11-16); on any other line an act with fewer conditions or consents than the act of its label fails.
    firsts = {b['first'] for b in base}
    relaxes = {}            # index of NAME's act: (index of the base act it relaxes, what it lacks)
    dflt0 = inst['defaulted'] if inst else owndef[0]
    rput = lambda x: '(' in x and re.match(r'~?([a-z_]+)\(', x) and re.match(r'~?([a-z_]+)\(', x).group(1) in dflt0 | inst.get('roles_gone', set())
    for j, a in enumerate(mine):
        if j in m.values() or a['first'] not in firsts or DERIVED.match(a['name']):
            continue
        for i, b in enumerate(base):
            if b['first'] == a['first'] and not DERIVED.match(b['name']):      # a ward's form or a vote act is not what an act relaxes
                lost = [x for x in (relaxed_of(b, a) or []) if not rput(x)]      # a role the line puts by cases is no consent lost (SC's issue of shares)
                if lost:
                    relaxes[j] = (i, lost)
                    break
    relaxed_base = {i for i, _ in relaxes.values()}
    gone = inst.get('gone', set())
    rgone = inst.get('roles_gone', set())
    who = lambda v: v                       # the relation a failure is told under
    for i, b in enumerate(base):
        if i not in m:
            if DERIVED.match(b['name']) or (rel_rlx and i in relaxed_base) or any(covered(b, a) for a in mine):
                continue            # an act of the base removed, made redundant by an act of NAME
            rel = 'instantiates' if inst else ('composes' if 'composes' in stated else ('extends' if rel_ext else 'restricts'))
            fail(rel, '%s %s: not in %s' % (b['label'], b['name'], n))
            continue
        a = mine[m[i]]
        if DERIVED.match(b['name']):
            continue
        if a['name'] != b['name'] and (rel_rlx and relaxed_of(b, a) or covered(b, a)):
            continue            # the act left of the label is a relaxation of b, b removed; or b is made redundant by it
        keep = lambda c: Counter({l: k for l, k in c.items() if pred(l) not in gone})
        bh, bb = keep(b['head']), keep(b['body'])
        dflt = inst['defaulted'] if inst else owndef[0]       # a parameter's default the contract replaces, by its binding or its own Default
        bg = {g for g in b['guards'] if not ('(' in g and (re.match(r'([a-z_]+)\(', g).group(1) in rgone or re.match(r'([a-z_]+)\(', g).group(1) in dflt and g not in a['guards']))}
        had_def = any(pred(l) in dflt and l not in a['body'] for l in bb)
        if dflt:
            bb = Counter({l: k for l, k in bb.items() if pred(l) not in dflt or l in a['body']})
        had_gone = inst and any(pred(l) in gone or (pred(l) in dflt and l not in a['body']) for l in list(b['head']) + list(b['body']))
        had_role = inst and len(bg) < len(b['guards'])
        inst_new = lambda l: inst and (pred(l) not in inst['base_preds'] or had_gone)
        own_preds = inst['base_preds'] if inst else set().union(*[preds(schemas(c)) for c in b['src']])
        h_add = a['head'] - bh
        h_lost = bh - a['head']
        b_add = a['body'] - bb
        b_lost = bb - a['body']
        g_add = (a['guards'] - bg) if not (inst and len(bg) < len(b['guards'])) else {g for g in a['guards'] - bg if '(' not in g}
        g_lost = bg - a['guards']
        act = '%s %s' % (a['label'], a['name'])
        if h_lost:
            fail('instantiates' if inst else ('strengthens' if rel_str else ('restricts' if rel_res else 'extends')),
                 '%s: an effect of the base act is gone: %s' % (act, ', '.join(sorted(h_lost))))
        if b_lost:
            fail('instantiates' if inst else ('restricts' if rel_res else ('strengthens' if rel_str else 'extends')),
                 '%s: a condition of the base act is gone: %s' % (act, ', '.join(sorted(b_lost))))
        if g_lost:
            fail('instantiates' if inst else ('restricts' if rel_res else 'extends'),
                 '%s: a guard of the base act is gone: %s' % (act, ', '.join(sorted(g_lost))))
        h_add = Counter({l: k for l, k in h_add.items() if not inst_new(l)})
        if h_add:
            old = sorted(l for l in h_add if pred(l) in own_preds)
            if not rel_str:
                fail('extends' if rel_ext else ('restricts' if rel_res else 'instantiates' if inst else 'composes'),
                     '%s: effects added and the line does not strengthen: %s' % (act, ', '.join(sorted(h_add))))
            elif old:
                fail('strengthens', '%s: an added effect of a state of the contract strengthened: %s' % (act, ', '.join(old)))
        deleted = {l[1:] for l in h_add if l.startswith('~') and not wild(l)}
        cond = sorted(l for l in b_add if l not in deleted and not inst_new(l) and not (had_def and pred(l) in owndef[1]))
        if had_role:
            cond = []           # a role the line puts, by cases with their conditions (SC's issue of shares): checked for what it keeps of B
        if (cond or g_add) and not rel_res:
            what = ', '.join(cond + sorted(g_add))
            fail('extends' if rel_ext else ('strengthens' if rel_str else 'instantiates' if inst else 'composes'),
                 '%s: conditions or consents added and the line does not restrict: %s' % (act, what))
    for j, (i, lost) in relaxes.items():
        if not rel_rlx:
            fail('instantiates' if inst else ('restricts' if rel_res else ('extends' if rel_ext else 'composes')),
                 '%s %s: %s %s with fewer conditions or consents, and the line does not relax: %s' % (mine[j]['label'], mine[j]['name'], base[i]['label'], base[i]['name'], ', '.join(lost)))
    own = [a for j, a in enumerate(mine) if j not in m.values() and a['first'].split(':')[0] not in {x['first'].split(':')[0] for x in base}]
    if own and not rel_ext:
        rel = 'restricts' if rel_res else ('strengthens' if rel_str else ('instantiates' if inst else 'composes'))
        for a in own:
            if not DERIVED.match(a['name']):
                fail(rel, '%s %s: an act not of the base, and the line does not extend' % (a['label'], a['name']))
    out = []
    for (v, bs, extra), rb in zip(steps, resolved):
        bl = ' and '.join(bs) if bs else 'it (%s)' % ' and '.join(rb)
        label = '%s %s %s' % (n, v, bl) + (' with %s' % extra if v == 'composes' else '')
        f = fails.pop(v, [])
        out.append((label, v, rb, f))
    # a failure told under a relation the line does not state goes with the first relation
    for v, f in fails.items():
        out[0] = (out[0][0], out[0][1], out[0][2], out[0][3] + ['(%s) %s' % (v, x) for x in f])
    return out


def merge_all(bs):
    out = None
    for b in bs:
        s = schemas(b)
        if s is None:
            continue
        out = s if out is None else merge(out, s)
    return out


def run(names=None, root=None, schemas_dir=None):
    """[(line, relation, bases, failures)] for the contracts named, or all of gr/; for tools/tests.py."""
    global ROOT, SCHEMAS
    saved = ROOT, SCHEMAS
    ROOT, SCHEMAS = root or ROOT, schemas_dir or SCHEMAS
    _cache.clear()
    _world.clear()
    try:
        names = names or contracts()
        return [(n,) + v for n in names for v in verdicts(n)]
    finally:
        ROOT, SCHEMAS = saved
        _cache.clear()
        _world.clear()


def text(n, root=None, schemas_dir=None):
    """NAME/relations.txt: a line for each relation of n's first line, as the tool prints it; empty where the line
    states none.  Written by tools/publish.py and tools/adopt.py."""
    return ''.join('%s: %s\n' % (line, 'holds' if not f else 'FAILS: ' + '; '.join(f)) for _, line, _, _, f in run([n], root, schemas_dir))


def main():
    global ROOT, SCHEMAS
    argv = sys.argv[1:]
    for opt in ('--root', '--schemas'):
        if opt in argv:
            k = argv.index(opt)
            if opt == '--root':
                ROOT = os.path.abspath(argv[k + 1])
            else:
                SCHEMAS = argv[k + 1]
            del argv[k:k + 2]
    args = [a for a in argv if not a.startswith('--')]
    names = args or contracts()
    rows, bad = [], 0
    for n in names:
        for label, v, bs, f in verdicts(n):
            rows.append({'contract': n, 'relation': v, 'base': bs, 'line': label, 'holds': not f, 'failures': f})
            bad += bool(f)
    if '--json' in sys.argv:
        print(json.dumps(rows, ensure_ascii=False, indent=1))
    else:
        for r in rows:
            print('%s: %s' % (r['line'], 'holds' if r['holds'] else 'FAILS: ' + '; '.join(r['failures'])))
        print('%d relations, %d hold, %d fail' % (len(rows), len(rows) - bad, bad))
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
