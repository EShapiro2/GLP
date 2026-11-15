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
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import gcsas

ARITH = {'le': 'le', 'ge': 'ge', 'gt': 'gt', 'lt': 'lt'}
# The current date, today, is an environment predicate wherever an act reads it (GLP programs/currencies/bonds/GB.glp;
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
    return [gcsas.read_schema_line(l) for l in gcsas.get('sasx', n) if gcsas.ENTRY.match(l)]


def values_of(n, refl=None, env=None, par=None):
    """The predicates of values and of no person: those whose vocabulary has the current date, now; and, in refl,
    the predicates the Logic pragmas declare reflexive, 'Reflexive: x knows y.', in n or a contract n is written from."""
    w = gcsas.World(sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'gr')) if f.endswith('.txt') and not f.startswith('NOTES')))
    lex = w.lexicon(n)
    rl = {p: gcsas.pattern(by, lex) for p, by in lex.rlist.items()}
    if refl is not None:
        refl.update(lex.refl_preds())
    if env is not None:
        env.update(lex.env_preds())
    if par is not None:
        par.update(lex.env_preds('par'))
    return {p for _, p, a in lex.basic if 'now' in a}, rl, gcsas.kinds_from(w, n)


def agents_of(acts, values=()):
    """The names that hold an atom or guard an act, in some act of the contract."""
    out = set()
    for e in acts:
        for l in e['head'] + e['body']:
            if l[1] not in values and l[1] not in ARITH and l[1] not in ('sum', 'product') and l[2] and re.fullmatch(gcsas.NAME, l[2][0]):
                out.add(l[2][0])
        out.update(g for g in e['guards'] if '(' not in g)
    return out


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


def schema_nums(lits, values, places=frozenset()):
    """The names a schema writes as numbers: those its arithmetic, values and counts use, and those at a place of a
    predicate, (predicate, arity, position), that holds a number in some schema of the contract (num_places)."""
    nums = set()
    for l in lits:
        if l[1] in values or l[1] in ARITH or l[1] in ('sum', 'product'):
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
    the whole term: LIB's borrowed(y,t,x,e) holds the day e as a number in Lend, which compares it, and so in Return."""
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


def binding_holds(e, x, y):
    """Whether GSC's Definition Binding lets one agent take the roles x and y of the act e: both are roles of it, the
    holder of an atom it requires or changes or one who decides it, and neither adds or deletes an atom of its effect
    that names the other (as vGLP's compiler has it, programs/vglp/cva/compiler/local.glp, pair_ok)."""
    rs = {l[2][0] for l in e['head'] + e['body'] if l[2]} | {g for g in e['guards'] if '(' not in g}
    names = lambda a, b: any(l[2] and l[2][0] == a and b in [z.lstrip('*#') for z in l[2][1:]] for l in e['head'])
    return x in rs and y in rs and not names(x, y) and not names(y, x)


def term(e, agents, used, values=(), rlist={}, marks={}, consts=(), places=frozenset(), cplaces=frozenset(), persons=frozenset()):
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
    holderless = lambda l: l[1] in values or l[1] in ARITH or l[1] in ('sum', 'product', 'author') or not l[2] or l[2][0] not in agents
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
    others, bound, listed, lists, deciders = [], [], [], [], []
    for g in e['guards']:           # those who decide: one of them, named, and every other the first agent of the role lists for it
        if '(' in g:
            _, p, a = gcsas.parse_atom(g)
            ex = [x[1:] for x in a if x.startswith('-')]      # those who decide, other than these (GnW:2, End guardian)
            a = [x for x in a if not x.startswith('-')]
            if a[0] not in roles:
                roles.append(a[0])
            k = len(listed) + 1
            d, x = 'Decider' + ('' if k == 1 else 'ABCDEFG'[k - 1]), 'Codecider' + ('' if k == 1 else 'ABCDEFG'[k - 1])
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
                at_z = [l for l in ls if l[1][0] == a[0]]
                anchor = lambda v, l=at_z[0]: fm(l, v)
                holds = lambda v, ls=ls, l0=at_z[0]: [fm(l, v) for l in ls if l is not l0]
            person = lambda v: ['atom(person, [%s])' % q(v)] if marks else []      # those who decide for a ward are persons
            lists.append(anchor(d))
            lists.extend(person(d) + holds(d))
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

    head, body, cons = [], [], []
    cleared = [l for l in e['head'] if l[0] and l[2] and l[2][0] in roles and '_' in l[2][1:]]
    for where, ls in ((head, e['head']), (body, e['body'])):
        for l in ls:
            if where is head and l[0] and l[2] and l[2][0] == '_':
                if not cleared:
                    return None
                c = next((c for c in cleared if c[1] == l[1]), None) or next((c for c in cleared if l[1] in c[1] or c[1] in l[1]), None) or cleared[0]
                x = 'E%d' % (len(bound) + 1)
                bound.append(x)
                k = list(c[2]).index('_', 1)
                anchor = 'atom(%s, [%s])' % (c[1], ', '.join(q(x) if i == k else arg(a) for i, a in enumerate(c[2])))
                at = 'atom(%s, [%s])' % (l[1], ', '.join([q(x)] + [arg(a) for a in l[2][1:]]))
                head.append('forall([%s], %s, true, [not(%s)])' % (q(x), anchor, at))
                # what a listed person loses, they hold; but an atom cleared with the wildcard is not required, a required
                # atom having no wildcard (vGLP, Section 5.4: the wildcard only in a negated atom)
                lists.append('forall([%s], %s, true, [atom(identity, [%s])%s])' % (q(x), anchor, q(x), '' if "'_'" in at else ', ' + at))
                continue
            if l[1] == 'author':      # x is the author of t: the first signer of t, GSC's eq(author_of(t), x) (Section 3.2; GFP's author)
                cons.append('%s(author_of(%s), %s)' % ('ne' if l[0] else 'eq', arg(l[2][1]), arg(l[2][0])))
            elif l[1] in ARITH:
                cons.append('%s(%s, %s)' % (NOT[ARITH[l[1]]] if l[0] else ARITH[l[1]], arg(l[2][0]), arg(l[2][1])))
            elif l[1] in ('sum', 'product'):
                cons.append('%s(%s, %s(%s, %s))' % ('ne' if l[0] else 'eq', arg(l[2][0]), 'plus' if l[1] == 'sum' else 'mul', arg(l[2][1]), arg(l[2][2])))
            elif holderless(l):
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
            if g in persons:        # a guard the act's own contract declares a person is a person, not a ward (GB:4's escrow agent, Calculus #6, 2026-11-15 17:23 UTC)
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
    same = [p.split('=') for p in e.get('same', []) if binding_holds(e, *p.split('='))]
    names, plain = [], []
    for x in roles + deciders + [o for o in others if o in agents and o not in roles]:
        ys = [b for a, b in same if a == x and b in plain] + [a for a, b in same if b == x and a in plain]
        if len(ys) > 1:       # one(Name, Other) carries one Other a name (GLP programs/gsc/self.glp, RName): a pair is never dropped
            raise ValueError('%s: %s is possibly one agent with %s, and the term writes one Other for a name, one(%s, Other)' % (e['name'], x, ' and '.join(ys), x))
        names.append('one(%s, %s)' % (q(x), q(ys[0])) if ys else q(x))
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
    cplaces = const_places(acts, consts)
    # A predicate the Logic pragmas declare reflexive, 'Reflexive: x knows y.' (GKK's, and so every contract's
    # written from GKK), is declared reflexive in the term, as GSC's GnW declares knows (GSC's Section "Contracts with
    # Wards", "Reflexive predicates"; GLP programs/gsc/GnW.glp), with or without wards; gcsas.py writes no atom p(x,x).
    refl = sorted(declared)
    if refl:
        drop = lambda ls: [l for l in ls if not (l[1] in refl and len(l[2]) == 2 and l[2][0] == l[2][1])]
        acts = [dict(e, head=drop(e['head']), body=drop(e['body'])) for e in acts]
    w = gcsas.World(sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'gr')) if f.endswith('.txt') and not f.startswith('NOTES')))
    pmemo = {}
    def persons_of(e):      # the names the contract that wrote the act declares persons, and only persons
        o = re.split(r'[:+]', e['label'])[0]
        if o not in pmemo:
            pmemo[o] = frozenset(x for x, ks in gcsas.kinds_from(w, n, o).items() if 'person' in ks and not ks & {'organisation', 'ward'}) if o in w.known else frozenset()
        return pmemo[o]
    for e in acts:
        t = term(e, agents, used, values, rlist, marks, consts, places, cplaces, persons_of(e))
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
    allnames = sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'sasx')) if f.endswith('.txt'))
    glp = os.environ.get('GLP_DIR') or next((d for d in (os.path.join(os.path.dirname(ROOT), 'GLP'), '/home/user/GLP') if os.path.isdir(d)), None)
    os.makedirs(os.path.join(ROOT, 'glp'), exist_ok=True)
    lines = []
    for n in args or allnames:
        t, acts, roles = contract_term(n)
        open(os.path.join(ROOT, 'glp', n + '.glp.txt'), 'w', encoding='utf-8').write(t + '\n')
        if roles:
            v = 'not translated: %d of its %d schemas end a state for everyone with no list of them' % (roles, acts)
        elif '--check' in sys.argv:
            v = check(t, glp)
        else:
            v = 'translated: %d schemas' % acts
        lines.append('%-10s %s' % (n, v))
        print(lines[-1], flush=True)
    if '--check' in sys.argv and not args:
        open(os.path.join(ROOT, 'glp', 'VERDICTS.txt'), 'w', encoding='utf-8').write('\n'.join(lines) + '\n')


if __name__ == '__main__':
    main()
