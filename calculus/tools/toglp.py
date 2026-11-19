#!/usr/bin/env python3
"""toglp.py --- the expanded schemas of a contract in the term form that GLP's checker of speech-act schemas reads,
and the checker's verdict on them.

    python3 tools/toglp.py [NAME ...]          writes glp/NAME.glp.txt, the term, for the contracts named or all
    python3 tools/toglp.py --check [NAME ...]  also runs check(Term, V) of GLP's programs/gsc and writes glp/VERDICTS.txt

Reads sasx/NAME.txt, written by tools/gcsas.py.  The term is sas(Name, Names, Vars, Head, Body, Constraint, Guards)
of GLP's programs/gsc/README.md, "Writing a contract".  --check needs the Dart SDK on the PATH and a clone of GLP
beside this directory, or $GLP_DIR.  A schema guarded by a role, as deciders(Carol), has no term: the checker
writes those who decide as a listed role with its anchor, which the vocabulary of a contract does not say; such a
contract is listed as not translated.
"""
import os, re, subprocess, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM      # the names of the contracts and their files (tools/names.py, NAMES.txt)
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import gcsas

ARITH = {'le': 'le', 'ge': 'ge', 'gt': 'gt', 'lt': 'lt'}
# The current date, today, is an environment predicate wherever an act reads it (GLP programs/currencies/bonds/Bonds.glp;
# vGLP compiler README, 10); every other is declared in the Logic pragmas, 'Environment: ...'.
ENV = ['today']
CMP = {'<': 'lt', '>': 'gt', '<=': 'le', '>=': 'ge', '=': 'eq'}
NOT = {'lt': 'ge', 'ge': 'lt', 'gt': 'le', 'le': 'gt', 'eq': 'ne', 'ne': 'eq'}   # the complement of a comparison, for a condition under 'not' (F07)


def q(x):
    return x if re.fullmatch(r'[a-z][a-z_0-9]*|\d+', x) else "'%s'" % x


def ident(s):
    """A schema's name: its act's name in lower case, each space an underscore, as vGLP's reader of pragmas takes a
    button (GLP programs/vglp/cva/compiler/README.md, 2), so that [No-show] is the schema no-show."""
    return re.sub(r' +', '_', s.lower().strip().replace("'", ''))   # no apostrophe in a name: Redeem member's coin


def schemas(n):
    """The entries of the contract's expanded schemas (sasx/), but for the acts no binding enables (unenabled): an act
    whose requirements no binding can meet is no act of the contract, and every tool that reads the contract's acts
    through this leaves it out alike (the term, cascade.py's Interface pragmas, ifadopt.py, possibly.py, supplied.py)."""
    es = [gcsas.read_schema_line(l) for l in gcsas.get('sasx', n) if gcsas.ENTRY.match(l)]
    out = unenabled(n, [e for e in es if e['kind'] == 'act'])
    return [e for e in es if not (e['kind'] == 'act' and (e['label'], e['name']) in out)]


_UNENABLED = {}


def unenabled(n, acts):
    """{(label, act): why} of the acts of n that no agent can take (gcsas.untakable, the one test): gcsas.py leaves
    each out of sasx/, so none is ever found here, and main says so if one is."""
    key = (ROOT, n)
    if key not in _UNENABLED:
        marks = marks_of(acts) if any(l[1] == 'person' for e in acts for l in e['head']) else {}
        w = gcsas.World(NM.names_in(os.path.join(ROOT, 'gr'))) if marks else None
        _UNENABLED[key] = gcsas.untakable(w, n, acts) if marks else {}
    return _UNENABLED[key]


ONE = set()     # the roles any one of whom decides, 'any one listed by' (gcsas.Lex.one), of the contract values_of read last


def values_of(n, refl=None, env=None, par=None):
    """The predicates of values and of no person: those whose vocabulary has the current date, now; and, in refl,
    the predicates the Logic pragmas declare reflexive, 'Reflexive: x knows y.', in n or a contract n is written from."""
    w = gcsas.World(NM.names_in(os.path.join(ROOT, 'gr')))
    lex = w.lexicon(n)
    rl = {p: gcsas.pattern(by, lex) for p, by in lex.rlist.items()}
    if refl is not None:
        refl.update(lex.refl_preds())
    if env is not None:
        env.update(lex.env_preds())
    if par is not None:
        par.update(lex.env_preds('par'))
    ONE.clear()
    ONE.update(lex.one)
    return {p for _, p, a in lex.basic if 'now' in a}, rl, gcsas.kinds_from(w, n)


def agents_of(acts, values=()):
    """The names that hold an atom or guard an act, in some act of the contract."""
    out = set()
    for e in acts:
        for l in e['head'] + e['body']:
            if l[1] not in values and l[1] not in ARITH and l[1] not in ('sum', 'product', 'difference') and l[2] and re.fullmatch(gcsas.NAME, l[2][0]):
                out.add(l[2][0])
        out.update(g for g in e['guards'] if '(' not in g)
    return out


marks_of = gcsas.marks_of      # the marks of a contract with wards (gcsas.marks_of)


def schema_nums(lits, values, places=frozenset()):
    """The names a schema writes as numbers: those its arithmetic, values and counts use, and those at a place of a
    predicate, (predicate, arity, position), that holds a number in some schema of the contract (num_places)."""
    nums = set()
    for l in lits:
        if l[1] in values or l[1] in ARITH or l[1] in ('sum', 'product', 'difference'):
            nums.update(a for a in l[2] if re.fullmatch(gcsas.NAME, a))
        for i, a in enumerate(l[2]):
            if a[:1] in '*#':
                nums.add(a[1:])
            m = re.fullmatch(r'(<=|>=|<|>|=)(\w+)', a)
            if m and not m.group(2).isdigit():
                nums.add(m.group(2))
            if (l[1], len(l[2]), i) in places and re.fullmatch(gcsas.NAME, a):
                nums.add(a)
    return nums


def num_places(acts, values):
    """The places of predicates that hold a number in some schema of the contract, so that a place has one type in
    the whole term: Lib's borrowed(y,t,x,e) holds the day e as a number in Lend, which compares it, and so in Return."""
    places, n = set(), -1
    while n != len(places):
        n = len(places)
        for e in acts:
            lits = e['head'] + e['body']
            nums = schema_nums(lits, values, places)
            places |= {(l[1], len(l[2]), i) for l in lits for i, a in enumerate(l[2]) if a in nums}
    return frozenset(places)


def const_places(acts, consts):
    """The places of predicates, (predicate, arity, position), at which some schema of the contract writes a constant
    the contract names, val(C): GBCC's and GNO's role(Alice, Orga, val('Treasurer')).  A name of no agent at such a
    place in another schema is a value variable over the constants, const(X) (GSC #17, Calculus_inbox.md, 2026-11-15
    15:09 UTC: const(X) beside num(X) and real(X)), as GO's Role in Appoint and Found."""
    return frozenset((l[1], len(l[2]), i) for e in acts for l in e['head'] + e['body'] for i, a in enumerate(l[2]) if a.lstrip('*#') in consts)


def extent(l, cleared, acts, dels=(), same=()):
    """The extent of a deletion with the wildcard as its holder, l = P(_, a1, ...): a cleared atom c of the act, the
    place k of c that names each holder, and, for each other wildcard place j of c, the wildcard place i of l that it
    names, so that the act deletes P(x, ..., y at i, ...) at every agent x with c[k] = x and c[j] = y; that map is empty
    today, the anchor keeping its other wildcards, as vGLP carries them (Calculus #6, 2026-11-15 21:44 UTC).  It is read off the acts that add P.
    The names of an adding act are its variables: an atom m = P(h, ...) it adds is matched to l, h the holder; one that
    does not match l is not deleted by it.  A matched m is covered when the same act adds an atom n of c's predicate
    that matches c under the same binding, with n[k] the holder h, or when its holder is one at which the deleting act
    deletes P itself (dels), or at a name the deleting act says is possibly that holder (same).  A name of the adding act the match leaves unbound stands for the deleting act's name that
    is spelt the same, the contracts' common names.  A cleared atom that names P comes first, then one whose name contains it,
    then the rest.  None where no cleared atom covers every matched P atom of every act that adds P: the extent
    cannot be derived, and the deletion is refused."""
    def match(m):
        s = {}
        for i in range(1, len(l[2])):
            a, v = m[2][i], ('_', i) if l[2][i] == '_' else l[2][i]
            if a not in s or s[a] == v:
                s[a] = v
            elif isinstance(s[a], tuple) and not isinstance(v, tuple):
                s[a] = v
            elif not (isinstance(v, tuple) and not isinstance(s[a], tuple)):
                return None
        return s
    val = lambda s, a: s.get(a, a)          # an unbound name stands for the deleting act's name spelt the same
    pairs_same = [set(x.split('=')) for x in same]
    adds = []
    for e in acts:
        for m in e['head']:
            if not m[0] and m[1] == l[1] and len(m[2]) == len(l[2]):
                s = match(m)
                if s is not None and not any(d[1] == m[1] and (val(s, m[2][0]) == d[2][0] or {val(s, m[2][0]), d[2][0]} in pairs_same) for d in dels):
                    s.setdefault(m[2][0], ('h',))
                    adds.append((e, m, s))
    order = sorted(cleared, key=lambda c: (c[1] != l[1], l[1] not in c[1]))
    for c in order:
        for k in [j for j, a in enumerate(c[2]) if j and a == '_']:
            def cover(e, m, s):
                for n in e['head']:
                    if n[0] or n[1] != c[1] or len(n[2]) != len(c[2]) or val(s, n[2][k]) != val(s, m[2][0]):
                        continue
                    if all(j == k or a == '_' or val(s, n[2][j]) == a for j, a in enumerate(c[2])):
                        return n
                return None
            pairs = [(s, cover(e, m, s)) for e, m, s in adds]
            if not adds or any(n is None for s, n in pairs):
                continue
            # The anchor's other wildcard places stay wildcards: vGLP carries the wildcard in the anchor, and not a quantifier
            # binding another variable (Calculus #6, 2026-11-15 21:44 UTC); binds is kept empty.
            binds = {}
            return c, k, binds
    return None


def binding_holds(e, x, y):
    """Whether GSC's Definition Binding lets one agent take the roles x and y of the act e: both are roles of it, the
    holder of an atom it requires or changes or one who decides it, and neither adds or deletes an atom of its effect
    that names the other (as vGLP's compiler has it, programs/vglp/cva/compiler/local.glp, pair_ok)."""
    rs = {l[2][0] for l in e['head'] + e['body'] if l[2]} | {g for g in e['guards'] if '(' not in g}
    names = lambda a, b: any(l[2] and l[2][0] == a and b in [z.lstrip('*#') for z in l[2][1:]] for l in e['head'])
    return x in rs and y in rs and not names(x, y) and not names(y, x)


def term(e, agents, used, values=(), rlist={}, marks={}, consts=(), places=frozenset(), cplaces=frozenset(), persons=frozenset(), acts=(), anyof=frozenset()):
    # A name of the act that holds, in what the act requires, the state that lists a role of those who decide, of the
    # same agent, is the one of them who decides, Decider: "Bob, a friend of a guardian of Carol" (GnW:2) is a friend
    # of the guardian who decides, as CSSN's make_guardian had it (Alice, a guardian, deciding).
    k = 0
    for g in e['guards']:
        if '(' in g:
            k += 1
            _, p, a = gcsas.parse_atom(g)
            exn = {x[1:] for x in a if x.startswith('-')}
            a = [x for x in a if not x.startswith('-')]
            d = 'Decider' + ('' if k == 1 else 'ABCDEFG'[k - 1])
            deleted = {(l[1], l[2]) for l in e['head'] if l[0]}
            for _, pp, aa in rlist.get(p, []):
                if aa[:1] == ('z',) and 'y' in aa:
                    iy = aa.index('y')
                    hit = next((l for l in e['body'] if not l[0] and l[1] == pp and len(l[2]) == len(aa) and l[2][0] == a[0]
                                and re.fullmatch(gcsas.NAME, l[2][iy]) and l[2][iy] not in e['guards'] and l[2][iy] != a[0]
                                and l[2][iy] not in exn and (l[1], l[2]) not in deleted), None)
                    if hit:
                        n = hit[2][iy]
                        sub = lambda ls: [(l[0], l[1], tuple(d if x == n else x for x in l[2])) for l in ls]
                        e = dict(e, head=sub(e['head']), body=[l for l in sub(e['body']) if l != (False, pp, tuple(d if x == n else x for x in hit[2]))])
                    break
    lits = e['head'] + e['body']
    nums = schema_nums(lits, values, places)
    cvars = {a for l in lits for i, a in enumerate(l[2]) if (l[1], len(l[2]), i) in cplaces and re.fullmatch(gcsas.NAME, a)
             and a not in consts and a not in nums and a not in agents}
    holderless = lambda l: l[1] in values or l[1] in ARITH or l[1] in ('sum', 'product', 'difference', 'author') or not l[2] or l[2][0] not in agents
    roles = []
    for l in lits:
        if not holderless(l) and l[2][0] not in roles:
            roles.append(l[2][0])
    roles += [g for g in e['guards'] if '(' not in g and g not in roles]
    if not e['guards']:             # an act nobody decides: its first role opens it, and is the role whose own required states name every other role and every name its effects name (vGLP, Section 5.4, the paragraph on act schemas)
        need = set(roles) | {a for l in e['head'] for a in l[2] if re.fullmatch(gcsas.NAME, a)}
        own = lambda r: {r} | {a for l in e['body'] if not l[0] and not holderless(l) and l[2][0] == r for a in l[2]}
        r = next((r for r in roles if need <= own(r)), None)
        if r and r != roles[0]:
            roles.remove(r)
            roles.insert(0, r)
    others, bound, listed, lists, deciders, onecons = [], [], [], [], [], []
    for g in e['guards']:           # those who decide: one of them, named, and every other the first agent of the role lists for it
        if '(' in g:
            _, p, a = gcsas.parse_atom(g)
            ex = [x[1:] for x in a if x.startswith('-')]      # those who decide, other than these (GnW:2, End guardian)
            a = [x for x in a if not x.startswith('-')]
            if a[0] not in roles:
                roles.append(a[0])
            k = len(deciders) + 1
            d, x = 'Decider' + ('' if k == 1 else 'ABCDEFG'[k - 1]), 'Codecider' + ('' if k == 1 else 'ABCDEFG'[k - 1])
            if p not in ONE:
                listed.append(x)
            deciders.append(d)
            anchor = lambda v: 'atom(%s, [%s])' % (p, ', '.join([q(a[0]), q(v)] + [q(y) for y in a[1:]]))
            holds = lambda v: []
            if p in rlist:          # the vocabulary says which states list the agents of the role
                ls = [(pp, tuple({'z': a[0], 'y': '%s'}.get(x, x) for x in aa)) for _, pp, aa in rlist[p]]
                fm = lambda l, v: 'atom(%s, [%s])' % (l[0], ', '.join(('val(%s)' % q(x)) if x in consts else q(x if x != '%s' else v) for x in l[1]))
                named = {x for l in e['head'] + e['body'] for x in l[2]}
                # a state that lists the role and names a name the act does not name is left out of the act's list:
                # GAI's record of an attestation, which GSC's def:relay, clause 2, asks of the acts that add a guardian
                # (Make guardian names the attestation), and not of End guardian or Retire
                ls = [l for l in ls if l[1][0] == a[0] or all(x in named or not re.fullmatch(gcsas.NAME, x) for x in l[1])]
                at_z = [l for l in ls if l[1][0] == a[0]] or [l for l in ls if a[0] in l[1]]      # a state of z that lists them; else one of each that names z (SC's shareholders: y holds a share of z)
                anchor = lambda v, l=at_z[0]: fm(l, v)
                holds = lambda v, ls=ls, l0=at_z[0]: [fm(l, v) for l in ls if l is not l0]
            person = lambda v: ['atom(person, [%s])' % q(v)] if marks else []      # those who decide for a ward are persons
            lists.append(anchor(d))
            lists.extend(person(d) + holds(d))
            if p in ONE:            # a role any one of whom decides ('any one listed by'): the one, and no other (GnW's default, Udi, 2026-11-16: 'any guardian')
                onecons.extend('ne(%s, %s)' % (q(d), q(y)) for y in ex)
                continue
            conj = ['atom(identity, [%s])' % q(x)] + person(x) + holds(x)
            f = conj[-1]
            for c in reversed(conj[:-1]):
                f = 'and(%s, %s)' % (c, f)
            phi = 'true'
            for y in reversed(ex):
                c = 'ne(%s, %s)' % (q(x), q(y))
                phi = c if phi == 'true' else 'and(%s, %s)' % (c, phi)
            lists.append('forall([%s], %s, %s, %s)' % (q(x), anchor(x), phi, f if marks else '[%s]' % ', '.join(conj)))

    def arg(a):
        if a == '_':
            return "'_'"
        if a.isdigit():
            return 'val(%s)' % a
        if '@' in a:                 # a thing with its signatures, Item@Alice@Bob: sgn(sgn(Item, Alice), Bob) (GSC, Section 3.2, signed terms)
            *t, s = a.split('@')
            return 'sgn(%s, %s)' % (arg('@'.join(t)), arg(s))
        if a in consts:              # a name of no kind that no one holds, as the role Arbiter: a constant the contract names
            return 'val(%s)' % q(a)
        if a in nums:
            if a not in others:
                others.append(a)
            return 'num(%s)' % q(a)
        if a in cvars:               # a value variable over the constants, const(X)
            if a not in others:
                others.append(a)
            return 'const(%s)' % q(a)
        if re.fullmatch(gcsas.NAME, a):
            if a not in roles and a not in others:
                others.append(a)
            return q(a)
        return 'val(%s)' % q(a)      # a constant, as now

    def atom(l):
        """An atom, and the constraints its counts and exact amounts add; under 'not' a count or an exact amount is
        the complement of its comparison (F07), and the atom itself is then not written."""
        cmp = (lambda c: NOT[c]) if l[0] else (lambda c: c)
        args, cons, mult = list(l[2]), [], None
        if args and args[-1][:1] == '*':
            mult, args = args[-1][1:], args[:-1]
        exact = None
        if args and args[-1][:1] == '#':
            exact, args = args[-1][1:], args[:-1]
        out = []
        for a in args:
            m = re.fullmatch(r'(<=|>=|<|>|=)(\w+)', a)
            if m:
                x = 'X%d' % (len(bound) + 1)
                bound.append(x)
                out.append((q(x), m))
            else:
                out.append((arg(a), None))
        at = 'atom(%s, [%s])' % (l[1], ', '.join(o for o, _ in out))
        for o, m in out:
            if m:
                cons.append('%s(cardinality([%s], %s, true), %s)' % (cmp(CMP[m.group(1)]), o, at, arg(m.group(2)) if not m.group(2).isdigit() else 'val(%s)' % m.group(2)))
                return None, cons
        if exact is not None:
            return None, ['%s(cardinality([], %s, true), %s)' % (cmp('eq'), at, arg(exact))]
        if mult is not None:
            at = 'times(%s, %s)' % (arg(mult), at)
        return at, cons

    head, body, cons = [], [], list(onecons)
    cleared = [l for l in e['head'] if l[0] and l[2] and l[2][0] in roles and '_' in l[2][1:]]
    for where, ls in ((head, e['head']), (body, e['body'])):
        for l in ls:
            if where is head and l[0] and l[2] and l[2][0] == '_':
                if not cleared:
                    return None
                ck = extent(l, cleared, acts, [d for d in e['head'] if d[0] and d[2] and d[2][0] != '_'], e.get('same', ()))
                if ck is None:
                    raise ValueError('%s: the deletion %s(%s) has no extent the acts that add %s give' % (e['name'], l[1], ','.join(l[2]), l[1]))
                c, k, binds = ck
                x = 'E%d' % (len(bound) + 1)
                bound.append(x)
                ys = {j: '%s_%d' % (x, j) for j in binds}      # the other wildcards of c, each the variable of the place of l it names
                # written as the place's type has it: a value variable over the constants at a place of constants, const(X),
                # a number at a place of numbers, num(X) (typecheck_places.py: a place of one type)
                # (either place of the two: the variable is one, as const(Role) is at both role/3 and has_role/3)
                def bv(j):
                    ps = {(c[1], len(c[2]), j), (l[1], len(l[2]), binds[j])}
                    return 'const(%s)' % q(ys[j]) if ps & cplaces else 'num(%s)' % q(ys[j]) if ps & places else q(ys[j])
                anchor = 'atom(%s, [%s])' % (c[1], ', '.join(q(x) if i == k else bv(i) if i in ys else arg(a) for i, a in enumerate(c[2])))
                lj = {i: j for j, i in binds.items()}
                at = 'atom(%s, [%s])' % (l[1], ', '.join([q(x)] + [bv(lj[i]) if i in lj else arg(a) for i, a in enumerate(l[2]) if i]))
                vs = ', '.join([q(x)] + [q(ys[j]) for j in sorted(ys)])
                head.append('forall([%s], %s, true, [not(%s)])' % (vs, anchor, at))
                # what a listed person loses, they hold; but an atom cleared with the wildcard is not required, a required
                # atom having no wildcard (vGLP, Section 5.4: the wildcard only in a negated atom)
                lists.append('forall([%s], %s, true, [atom(identity, [%s])%s])' % (vs, anchor, q(x), '' if "'_'" in at else ', ' + at))
                continue
            if l[1] == 'author':      # x is the author of t: the first signer of t, GSC's eq(author_of(t), x) (Section 3.2; GFP's author)
                cons.append('%s(author_of(%s), %s)' % ('ne' if l[0] else 'eq', arg(l[2][1]), arg(l[2][0])))
            elif l[1] in ARITH:
                cons.append('%s(%s, %s)' % (NOT[ARITH[l[1]]] if l[0] else ARITH[l[1]], arg(l[2][0]), arg(l[2][1])))
            elif l[1] == 'difference':     # n is the difference from m to k, |m - k|: two linear cases, joined by or (def:numbers' connectives; linear.glp decides or)
                n, m, k = (arg(a) for a in l[2])
                c = 'or(and(eq(%s, plus(%s, %s)), ge(%s, %s)), and(eq(%s, plus(%s, %s)), ge(%s, %s)))' % (m, n, k, m, k, k, n, m, k, m)
                cons.append('not(%s)' % c if l[0] else c)
            elif l[1] in ('sum', 'product'):
                cons.append('%s(%s, %s(%s, %s))' % ('ne' if l[0] else 'eq', arg(l[2][0]), 'plus' if l[1] == 'sum' else 'mul', arg(l[2][1]), arg(l[2][2])))
            elif holderless(l) and not re.fullmatch(r'(<=|>=|<|>|=)\w+', l[2][0] if l[2] else ''):      # a counted holder is a count, cardinality(...) (SC's 'fewer than 1 persons holds a share of z')
                c = 'rel(%s, [%s])' % (l[1], ', '.join(arg(a) for a in l[2]))
                cons.append('not(%s)' % c if l[0] else c)
            else:
                at, cs = atom(l)
                cons += cs
                if at:
                    where.append('not(%s)' % at if l[0] else at)
    made = {l[2][0] for l in e['head'] if l[1] == 'identity' and not l[0]}
    if marks:                       # who acts is a person, or a ward with a guardian (GSC, Contracts with Wards)
        has = lambda p, g: any(not l[0] and l[1] == p and l[2] and l[2][0] == g for l in e['body'])
        k = 0
        for g in [g for g in e['guards'] if '(' not in g]:
            if g in made or has('person', g) or any(has(m, g) and has(u, g) for m, u in marks.items()):
                continue
            if g in persons:        # a guard the act's own contract declares a person or a party is a person, not a ward (Bonds:4's escrow agent, Calculus #6, 2026-11-15 17:23 UTC; a party, Udi, 2026-11-15)
                lists.append('atom(person, [%s])' % q(g))
                continue
            f = 'atom(person, [%s])' % q(g)
            for m, u in marks.items():
                k += 1
                f = 'or(%s, and(atom(%s, [%s]), exists([%s], atom(%s, [%s, %s]), true)))' % (f, m, q(g), q('G%d' % k), u, q(g), q('G%d' % k))
            lists.append(f)
        lists[:] = [x.replace('[atom(identity, [%s])' % q(d), '[atom(identity, [%s]), atom(person, [%s])' % (q(d), q(d))) for x in lists for d in [None]] if False else lists
    idn = ['atom(identity, [%s])' % q(r) for r in roles + deciders if r not in made and 'not(atom(identity, [%s]))' % q(r) not in body]
    # Two roles the contract says are possibly one agent are written one(Name, Other), Name the later of the two
    # (GLP programs/gsc/self.glp: "the schema's head writes the second name as one('Bob', 'Alice')"), where GSC's
    # Definition Binding lets one agent take both: neither adds or deletes an atom naming the other (binding_holds).
    # A pair of which one is an agent variable that is no role is marked as the declaration says it, Binding
    # restricting roles alone; and a name the clause admits as any of the others, one(Name, '_') (GSC's README, GLP
    # GSC-one 5cf7633b; Udi's approval to vGLP #30, 2026-11-17: two names are two persons unless the clause says
    # possibly one).
    order = roles + deciders + [o for o in others if o in agents and o not in roles]
    rs = set(roles + deciders)
    same = [p.split('=') for p in e.get('same', []) if all(z in order for z in p.split('='))
            and (not (p.split('=')[0] in rs and p.split('=')[1] in rs) or binding_holds(e, *p.split('=')))]
    # one(Name, Others), Name the later of each pair in Names and Others the earlier names it may be, a list, or one
    # name, or '_' where the clause declares Name possibly any of the others and every other name is so marked
    # (GSC #19, Calculus_inbox.md, 2026-11-17 11:31 UTC: Swap writes one('Carol', ['Alice', 'Bob'])).
    anyone = [x for x in order if x in anyof and len(order) > 2 and {y for a, b in same for y in (a, b) if x in (a, b)} == set(order)]
    same = [(a, b) for a, b in same if a not in anyone and b not in anyone]
    names, plain = [], []
    for x in order:
        ys = [y for y in plain if (x, y) in same or (y, x) in same]
        names.append('one(%s, %s)' % (q(x), q('_')) if x in anyone else 'one(%s, %s)' % (q(x), q(ys[0]) if len(ys) == 1 else '[%s]' % ', '.join(q(y) for y in ys)) if ys else q(x))
        plain.append(x)
    name = ident(e['name'])
    k = used.get(name, 0)
    used[name] = k + 1
    if k:
        name += '_%d' % (k + 1)
    constraint = 'true'
    for c in reversed(cons):
        constraint = c if constraint == 'true' else 'and(%s, %s)' % (c, constraint)
    return 'sas(%s, [%s], [%s], [%s], [%s], %s, [%s])' % (
        q(name), ', '.join(names + [q(x) for x in listed]), ', '.join(q(o) for o in others if o not in roles and o not in deciders), ', '.join(head), ', '.join(idn + body + lists), constraint,
        ', '.join([q(g) for g in e['guards'] if '(' not in g] + [q(x) for x in deciders + listed]))


def contract_term(n):
    acts = [e for e in schemas(n) if e['kind'] == 'act']
    declared, envd, pard = set(), set(), set()
    values, rlist, kinds = values_of(n, declared, envd, pard)
    agents, used, out, roles = agents_of(acts, values), {}, [], 0
    ak = {'person'} | {k for x in agents for k in kinds.get(x, ())}      # a name of the kind of one who holds a state is an agent
    agents = agents | {x for x, ks in kinds.items() if ks & ak}
    own = gcsas.kinds_line(gcsas.read_gr(n))
    agents = agents - {x for x, ks in own.items() if 'issuer' in ks}   # a name the contract's own names line makes an issuer is a value of no role: the coins an organisation moves (GO #5; Calculus #6, 2026-11-15 16:13 UTC)
    marks = marks_of(acts) if any(l[1] == 'person' for e in acts for l in e['head']) else {}
    places = num_places(acts, values)
    consts = {a.lstrip('*#') for e in acts for l in e['head'] + e['body'] for a in l[2]
              if re.fullmatch(gcsas.NAME, a.lstrip('*#')) and a.lstrip('*#') not in kinds and a.lstrip('*#') not in agents}
    # a name some schema counts or computes with is a number, though no names line declares it: the numbers of a
    # decision on the votes of an organisation, named for it, VotesAlice (gcsas.org_numbers)
    consts -= {x for e in acts for x in schema_nums(e['head'] + e['body'], values, places)
               if any(x.startswith(k) and x != k and 'number' in ks for k, ks in kinds.items())}
    cplaces = const_places(acts, consts)
    # A predicate the Logic pragmas declare reflexive, 'Reflexive: x knows y.' (GKK's, and so every contract's
    # written from GKK), is declared reflexive in the term, as GSC's GnW declares knows (GSC's Section "Contracts with
    # Wards", "Reflexive predicates"; GLP programs/gsc/GnW.glp), with or without wards; gcsas.py writes no atom p(x,x).
    refl = sorted(declared)
    if refl:
        drop = lambda ls: [l for l in ls if not (l[1] in refl and len(l[2]) == 2 and l[2][0] == l[2][1])]
        acts = [dict(e, head=drop(e['head']), body=drop(e['body'])) for e in acts]
    w = gcsas.World(NM.names_in(os.path.join(ROOT, 'gr')))
    pmemo = {}
    def persons_of(e):      # the names the contract that wrote the act declares persons or parties, and only those
        o = re.split(r'[:+]', e['label'])[0]
        if o not in pmemo:
            pmemo[o] = frozenset(x for x, ks in gcsas.kinds_from(w, n, o).items() if ks & {'person', 'party'} and not ks & {'organisation', 'ward'}) if o in w.known else frozenset()
        # a party guards the act in its person's form as a person, its ward's form being the act derived 'for' it
        # (gcsas.party_specs); who takes an act for a party, and who votes on it, is a person (GO:2's Act and Vote)
        return pmemo[o] | frozenset((e.get('taken') or {}).values()) | {gcsas.VOTER}
    amemo = {}
    def anyof(e):           # the names the clause that wrote the act declares possibly any of the others (gcsas.possibly)
        o, num = re.split(r'[:+]', e['label'])[0], int(re.split(r'[:+]', e['label'])[1])
        if (o, num) not in amemo:
            amemo[(o, num)] = frozenset(a for cl, a, b in gcsas.possibly(w.contract(o)) if b == '*' and (cl is None or num in cl)) if o in w.known else frozenset()
        return amemo[(o, num)]
    for e in acts:
        t = term(e, agents, used, values, rlist, marks, consts, places, cplaces, persons_of(e), acts, anyof(e))
        if t is None:
            roles += 1
        else:
            out.append(t)
    body = '[' + ',\n '.join(out) + ']'
    body = 'reflexive([%s], %s)' % (', '.join(refl), body) if refl else body
    # The environment predicates: today, and those the Logic pragmas declare, 'Environment: x is nearby y.' (GSC's
    # appendix "Values in Speech-Act Schemas"; vGLP compiler README, 6 and 10), where an act of the contract has them.
    env = [p for p in ENV + sorted(envd - set(ENV)) if any(l[1] == p for e in acts for l in e['head'] + e['body'])]
    if env:
        body = 'environment([%s], %s)' % (', '.join(env), body)
    # The parameters the Logic pragmas declare, 'Parameters: ...', that an act of the contract requires and none adds:
    # the contract is parametric in them, outermost (GSC's Definition Parametric contract; GLP programs/gsc/README.md,
    # "Parametric contracts and their instances"); a contract whose acts add one has filled it.
    added = {l[1] for e in acts for l in e['head'] if not l[0]}
    par = sorted(p for p in pard if p not in added and any(l[1] == p for e in acts for l in e['body']))
    if par:
        body = 'parametric([%s], %s)' % (', '.join(par), body)
    return body, len(acts), roles


def check(t, glp):
    env = dict(os.environ)
    goal = '%s/programs/gsc/\n:limit 1000000000\ncheck(%s, V).\n:quit\n' % (glp, t.replace('\n', ''))
    r = subprocess.run(['bin/glpc'], input=goal, capture_output=True, text=True, cwd=os.path.join(glp, 'glp_runtime'), env=env, timeout=3600)
    m = re.search(r'V = (.*)', r.stdout)
    return m.group(1).strip() if m else 'NO ANSWER: ' + (r.stdout + r.stderr).strip().replace('\n', ' ')[-300:]


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('-')]
    allnames = NM.names_in(os.path.join(ROOT, 'sasx'))
    glp = os.environ.get('GLP_DIR') or next((d for d in (os.path.join(os.path.dirname(ROOT), 'GLP'), '/home/user/GLP') if os.path.isdir(d)), None)
    os.makedirs(os.path.join(ROOT, 'glp'), exist_ok=True)
    lines, bad = [], 0
    for n in [NM.name_of(a) for a in args] or allnames:
        t, acts, roles = contract_term(n)
        for (lb, a), why in sorted(unenabled(n, []).items()):      # filled by schemas(n), which contract_term read
            print('%-10s left out of the term: %s (%s), %s' % (NM.file_of(n), a, lb, why), flush=True)
        p = os.path.join(ROOT, 'glp', NM.file_of(n) + '.glp.txt')
        if roles:                   # no term is written, and none of an earlier run is left in its place
            if os.path.exists(p):
                os.remove(p)
            bad += 1
            v = 'not translated: %d of its %d schemas end a state for everyone with no list of them' % (roles, acts)
        elif '--check' in sys.argv:
            open(p, 'w', encoding='utf-8').write(t + '\n')
            v = check(t, glp)
            bad += v.startswith('NO ANSWER')
        else:
            open(p, 'w', encoding='utf-8').write(t + '\n')
            v = 'translated: %d schemas' % acts
        lines.append('%-10s %s' % (NM.file_of(n), v))      # a line by the file name, one word
        print(lines[-1], flush=True)
    if '--check' in sys.argv and not args:
        open(os.path.join(ROOT, 'glp', 'VERDICTS.txt'), 'w', encoding='utf-8').write('\n'.join(lines) + '\n')
    return 1 if bad else 0          # a contract not translated, or no answer of the checker; a verdict is no failure


if __name__ == '__main__':
    sys.exit(main())
