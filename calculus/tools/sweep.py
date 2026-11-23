#!/usr/bin/env python3
"""sweep.py [--out DIR] --- the sweep of the contracts against the operations of the paper (Section 3, sections/01c-algebra.tex).

The task: docs/sweep-task-2026-11-23.md of the Calculus (Calculus #5, 2026-11-23; Udi, 2026-11-23 12:13 and 12:15 UTC).
The program reads every contract of calculus/contracts and changes nothing there: it writes five files into
calculus/sweep/ (or DIR).  Run:  python3 -I calculus/tools/sweep.py

For each contract G' and each contract G its first line names, it takes the expanded clauses of both, as
full-schemas.txt has them.  The source of a clause C' of G' is the clause of G from which C' is written, found as
tools/relations.py finds the act of the base (its match, imported: by the first label and the name, then, within a
label, the acts left in order), G's names of acts first put through the words of the first line ('an organisation for
a ward'), as relations.py does.  Each clause C' gets one line: contract | label | name | source | differences.

A clause is H <- B guarded by Q.  C' is compared with its source under a map of the variables of the source to those
of C', sought so that most literals agree, one-to-one if that loses no literal, and under the contract's map of
predicates.  The differences the paper has:
    1 Same               C' is its source, up to the names of variables and the order of literals
    2 Added              C' has no source in a contract of the first line
    3 Weakened by literals         the body of C' has more: the literals
    4 Weakened by guarding agents  the guards of C' are more: the guards
    5 Strengthened by literals     the head of C' has more: the literals; of each, whether its predicate occurs in G,
                                   and of a negated one whether the body of C' has its atom
    6 Renamed            literals of C' are those of the source under a map of predicates (sweep-maps.txt)
The residue, each line with both clauses in full in sweep-residue.txt:
    R1 a body literal of the source is absent          R2 a guard of the source is absent, or another in its place
    R3 a head literal of the source is absent          R4 a literal with the predicate of a source literal and other arguments
    R5 a parameter of G (a line "Parameters:" of G or of a contract G is written from) has other literals in its place
    R6 two variables of the source are one in C', or the pairs possibly one differ
    R7 a clause of G has no clause of G' of which it is the source
    R8 C' has two sources, in two contracts of the first line
    R9 two clauses of G' have one label and one name
    R10 any other difference, described

How cases the task does not decide are classified (each is told in the lines it touches):
 a. The map of predicates of G' from G: a predicate p of G that no clause of G' has, and a predicate q such that a
    literal of p in a source and a literal of q in its clause agree in sign, place (head or body) and arguments; of
    several q, the one most clauses give.  A parameter is not renamed: it is R5.  Guards are agents: a role in the place of a role is R2, never a renaming.
 b. R5.  full-schemas.txt prints a contract at the defaults of its parameters, so the source seldom has the parameter
    itself.  The parameter is read where it is declared: the clause of schemas.txt of the contract of a part of the
    label, with the name of C' (or, failing that, the one its source was read against).  Where that clause has a
    literal of a predicate of a line "Parameters:" and C' has no such literal, C' gets R5, and what is put is: the
    binding 'literals for p(x,..)' of the first line of G', if it has one for p; else what the source has put, if C'
    has it all; else the literals of that place that C' has more than the declared clause, each given to the
    parameter with which it shares most variables.  A literal of the source that was put for a parameter and is
    absent from C' is R5 too, not R1, and the literals put for a parameter are not listed again as a weakening
    (but a second copy is).
 c. R8 only where the two sources differ as clauses; one clause that two contracts of the first line both have is one
    source, named with both.
 d. A capitalised argument of a binding of a first line that no clause of the bases has (Governor, Director) is a
    constant, not a variable, in every contract.
 e. A derived act with no source ('<Act> for <Role>', 'Vote of ...', or a clause 'taken for' a ward) is R10, a derived
    act, not Added: no operation of the paper gives it.  A difference of 'taken for' or 'must' between a clause and
    its source is R10.
 h. A clause with no declared clause of its own (a derived act) in a contract whose first line binds parameters: body
    literals of the source, of predicates put for parameters up the chain, that are absent while the clause has
    literals of the predicates the first line binds, are R5, and those literals are no weakening.
 f. A clause of a contract whose first line names no contract has no line of difference ('-').
 g. Where a guard of the source is absent and C' has guards the source lacks, as many of these as are absent are 'in
    its place' (roles first) and are not listed as a weakening.
Standard library only.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import relations as R
from collections import Counter, defaultdict

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
R.ROOT = os.path.join(HERE, 'contracts')
OUT = os.path.join(HERE, 'sweep')
VAR = re.compile(r'[A-Z][A-Za-z0-9]*')
CONST = set()
NODES = 30000


# ---- reading ------------------------------------------------------------------------------------------------------

def raw_lines(n):
    """The schema lines of n's full-schemas.txt, in the order of relations.schemas(n): (text, suffix)."""
    out = []
    for l in open(R.path(n, 'schemas'), encoding='utf-8'):
        l = l.rstrip('\n')
        m = R.LINE.match(l)
        if not m or m.group(3).startswith('| '):
            continue
        _, _, rest = m.group(3).partition(': ')
        g = rest.rstrip('.').rpartition(' guarded by ')[2] if ' guarded by ' in rest or rest.startswith('guarded by ') else ''
        if rest.startswith('guarded by '):
            g = rest.rstrip('.')[len('guarded by '):]
        sm = re.search(r' (possibly|taken for|must) ', g)
        out.append((l, g[sm.start() + 1:] if sm else ''))
    return out


def lit(s, side):
    neg = s.startswith('~')
    t = s[1:] if neg else s
    m = re.match(r'([A-Za-z_][A-Za-z0-9_]*)\((.*)\)$', t)
    if m:
        return (side, neg, m.group(1), tuple(R.split_top(m.group(2))), s)
    if side == 'g':
        return (side, False, '', (t,), s)
    return (side, neg, t, (), s)


def items(a):
    out = []
    for side, c in (('h', a['head']), ('b', a['body'])):
        for s in sorted(c.elements()):
            if s:
                out.append(lit(s, side))
    for s in sorted(a['guards']):
        if s:
            out.append(lit(s, 'g'))
    return out


def vars_of(arg):
    return [v for v in VAR.findall(arg) if v not in CONST]


def shape(arg):
    return VAR.sub(lambda m: m.group(0) if m.group(0) in CONST else '\0', arg)


_clauses = {}


def clauses(n):
    if n not in _clauses:
        s = R.schemas(n)
        if s is None:
            _clauses[n] = []
        else:
            raws = raw_lines(n)
            assert len(raws) == len(s), n
            for a, (t, suf) in zip(s, raws):
                a['text'], a['suffix'], a['contract'] = t, suf, n
            _clauses[n] = s
    return _clauses[n]


def bases_of(n):
    out = []
    for v, bs, extra in R.first_line(n):
        for b in (bs or []) + ([x for x in re.split(r',\s*|\s+and\s+', extra) if x] if v == 'composes' else []):
            if b not in out and os.path.exists(R.path(b, 'schemas')):
                out.append(b)
    return out


def the_first_line(n):
    p = R.path(n, 'first')
    ls = open(p, encoding='utf-8').read().split('\n') if os.path.exists(p) else []
    return ls[1] if len(ls) > 1 else ''


def bindings(n):
    """{parameter predicate: (its arguments, [literals put])} of the bindings 'lits for p(x,..)' of n's first line."""
    out = {}
    p = os.path.join(R.ROOT, R.NM.file_of(n), 'schemas.txt')          # its second line has the bindings as atoms
    ls = open(p, encoding='utf-8').read().split('\n') if os.path.exists(p) else []
    fl = ls[1] if len(ls) > 1 else ''
    for part in re.split(r';\s*|, (?:and )?(?=restricts|extends|strengthens|composes|relaxes)', fl):
        part = re.sub(r'^.*? with (?=~?[a-z_]+\()', '', part)
        m = re.search(r'((?:~?[a-z_]+\([^()]*\)(?:,\s*)?)+) for ([a-z_]+)\(([^()]*)\)', part)
        if m:
            out[m.group(2)] = (tuple(R.split_top(m.group(3))), R.split_top(m.group(1)))
    return out


def vocabulary(n):
    out = {}
    p = os.path.join(R.ROOT, R.NM.file_of(n), 'schemas.txt')
    if not os.path.exists(p):
        return out
    for l in open(p, encoding='utf-8'):
        if l.startswith('Vocabulary: ') or l.startswith('Roles: '):
            for part in l.rstrip('\n').rstrip('.').split(': ', 1)[1].split('; '):
                if ' = ' in part:
                    ph, at = part.rsplit(' = ', 1)
                    out[ph.strip()] = at.strip()
    return out


def parameters(n):
    """The predicates of n's line 'Parameters:', by its Vocabulary."""
    p = os.path.join(R.ROOT, R.NM.file_of(n), 'schemas.txt')
    out = []
    if not os.path.exists(p):
        return out
    voc = vocabulary(n)
    for l in open(p, encoding='utf-8'):
        m = re.match(r'.*\| Parameters: (.*)$', l.rstrip('\n'))
        if m:
            for ph in m.group(1).rstrip('.').split('; '):
                at = voc.get(ph.strip())
                out.append(R.pred(at) if at else '?' + ph)
    return out


def declared(n):
    """The clauses of n's schemas.txt: {(label, name): {'head': [...], 'body': [...], 'guards': [...], 'text'}}; of a
    citation, the effects it adds (after ' + ') and its conditions."""
    p = os.path.join(R.ROOT, R.NM.file_of(n), 'schemas.txt')
    out, cites = {}, []
    if not os.path.exists(p):
        return out, cites
    for l in open(p, encoding='utf-8'):
        l = l.rstrip('\n')
        m = R.LINE.match(l)
        if not m or m.group(3).startswith('| ') or ' <-' not in m.group(3):
            continue
        name, _, rest = m.group(3).partition(': ')
        head, _, rest = rest.rstrip('.').partition(' <-')
        rest = rest.strip()
        cite = bool(re.match(R.NM.REF, head))
        if cite:
            if ' + ' in head:
                cites.append(l)
            head = head.split(' + ', 1)[1] if ' + ' in head else ''
        body, _, g = (('', '', rest[len('guarded by '):]) if rest.startswith('guarded by ') else rest.partition(' guarded by '))
        g = re.split(r' possibly | taken | must ', g)[0]
        ok = lambda xs: [x for x in xs if re.match(r'~?[a-z_]+(\(.*\))?$', x)]
        out[(m.group(1), name)] = {'head': ok(R.split_top(head)), 'body': ok(R.split_top(body)),
                                   'guards': R.split_top(g), 'text': l, 'cite': cite}
    return out, cites


# ---- comparing two clauses ----------------------------------------------------------------------------------------

def align(S, C, ren, injective=True):
    """The best map of the variables of S to those of C: (score, {index in S: index in C}, theta).  ren(p, q): whether a
    literal of predicate p may be one of q."""
    cands = []
    for s in S:
        cs = []
        for j, c in enumerate(C):
            if c[0] != s[0] or c[1] != s[1] or len(c[3]) != len(s[3]):
                continue
            if c[2] != s[2] and not (s[0] != 'g' and ren(s[2], c[2])):
                continue
            if any(shape(x) != shape(y) for x, y in zip(s[3], c[3])):
                continue
            cs.append((0 if c[4] == s[4] else 1 if c[2] == s[2] else 2, j))
        cands.append([j for _, j in sorted(cs)])
    order = sorted(range(len(S)), key=lambda i: (len(cands[i]) == 0, len(cands[i])))
    order = [i for i in order if cands[i]]
    best = [-1.0, {}, {}]
    count = [0]

    def go(k, th, inv, used, got, score):
        count[0] += 1
        if score + (len(order) - k) <= best[0] or count[0] > NODES:
            return
        if k == len(order):
            best[0], best[1], best[2] = score, dict(got), dict(th)
            return
        i = order[k]
        s = S[i]
        for j in cands[i]:
            if j in used:
                continue
            c = C[j]
            new, ok = [], True
            for x, y in zip(s[3], c[3]):
                for u, v in zip(vars_of(x), vars_of(y)):
                    if u in th:
                        if th[u] != v:
                            ok = False
                    elif injective and inv.get(v, u) != u:
                        ok = False
                    else:
                        th[u] = v
                        new.append(u)
                        inv.setdefault(v, u)
                    if not ok:
                        break
                if not ok:
                    break
            if ok:
                used.add(j)
                got[i] = j
                go(k + 1, th, inv, used, got, score + (1.0 if c[2] == s[2] else 0.9) + (0.001 if c[4] == s[4] else 0))
                used.discard(j)
                del got[i]
            for u in new:
                if inv.get(th[u]) == u:
                    del inv[th[u]]
                del th[u]
        go(k + 1, th, inv, used, got, score)

    go(0, {}, {}, set(), {}, 0.0)
    return best[0], best[1], best[2]


def thr(text, th):
    return VAR.sub(lambda m: th.get(m.group(0), m.group(0)), text)


def compare(S, C, ren):
    """S, C: clauses (dicts).  The alignment and what is lost and added."""
    si, ci = items(S), items(C)
    sc, got, th = align(si, ci, ren, True)
    merged = []
    if len(got) < len(si):
        sc2, got2, th2 = align(si, ci, ren, False)
        if len(got2) > len(got):
            got, th = got2, th2
            inv = defaultdict(list)
            for u, v in th.items():
                inv[v].append(u)
            merged = sorted('%s and %s of the source are %s' % (', '.join(sorted(us[:-1])), sorted(us)[-1], v) for v, us in inv.items() if len(us) > 1)
            merged = sorted('%s of the source are %s' % (' and '.join(sorted(us)), v) for v, us in inv.items() if len(us) > 1)
    lost = [si[i] for i in range(len(si)) if i not in got]
    added = [ci[j] for j in range(len(ci)) if j not in got.values()]
    renamed = sorted({(si[i][2], ci[j][2]) for i, j in got.items() if si[i][2] != ci[j][2]})
    return {'theta': th, 'lost': lost, 'added': added, 'renamed': renamed, 'merged': merged, 'got': got, 'si': si, 'ci': ci}


def pairs_of(suffix):
    m = re.search(r'possibly ((?:[A-Z][A-Za-z]*=[A-Z][A-Za-z]*(?:, )?)+)', suffix)
    return {frozenset(p.split('=')) for p in m.group(1).split(', ')} if m else set()


def rest_of(suffix):
    return re.sub(r'\s*possibly (?:[A-Z][A-Za-z]*=[A-Z][A-Za-z]*(?:, )?)+', '', suffix).strip()


def kind_put(P, put):
    """What is put for the parameter literal P (text): the task's words."""
    if not put:
        return 'no literal'
    if len(put) > 1:
        return 'several literals (%d)' % len(put)
    pa = lit(P, 'b')[3]
    qa = lit(put[0], 'b')[3]
    pv, qv = set(pa), set(qa)
    const = [x for x in qa if x not in pv and not vars_of(x) and x != '_'] + [x for x in qa if x not in pv and x in CONST]
    if qv == pv:
        return 'one literal with the same arguments'
    if qv < pv:
        return 'one literal with fewer arguments'
    if const and qv - set(const) <= pv:
        return 'one literal with a constant more' if qv - set(const) == pv else 'one literal with fewer arguments and a constant'
    return 'one literal with other arguments'


# ---- the sweep ----------------------------------------------------------------------------------------------------

def main():
    global OUT
    if '--out' in sys.argv:
        OUT = os.path.abspath(sys.argv[sys.argv.index('--out') + 1])
    os.makedirs(OUT, exist_ok=True)
    names = R.contracts()
    preds = {n: R.preds(clauses(n)) for n in names}
    bases = {n: bases_of(n) for n in names}
    binds = {n: bindings(n) for n in names}
    for n in names:                                  # constants (d)
        seen = set()
        for b in bases[n]:
            for a in clauses(b):
                for x in list(a['head']) + list(a['body']):
                    seen.update(VAR.findall(x))
        for p, (args, lits) in binds[n].items():
            for l in lits:
                CONST.update(v for v in VAR.findall(l) if v not in seen)
    params = {n: parameters(n) for n in names}
    allparams = {p for n in names for p in params[n] if not p.startswith('?')}
    decl, cites = {}, {}
    for n in names:
        decl[n], cites[n] = declared(n)

    order, done = [], set()

    def visit(n):
        if n in done:
            return
        done.add(n)
        for b in bases[n]:
            if b in preds:
                visit(b)
        order.append(n)
    for n in names:
        visit(n)

    # the sources and the maps of predicates
    src = {}         # (n, j): [(G, i)]
    unsourced = {}   # (n, G): [i]
    maps = {}        # (n, G): {p: q}
    votes = {}
    for n in order:
        mine = clauses(n)
        ws = [w for v, bs, extra in R.first_line(n) if v == 'instantiates' for w in R.words(extra)]
        for g in bases[n]:
            base = clauses(g)
            m = R.match(R.substitute(base, ws), mine)
            for i, j in m.items():
                src.setdefault((n, j), []).append((g, i))
            unsourced[(n, g)] = [i for i in range(len(base)) if i not in m]
            gone = preds[g] - preds[n]
            vt = defaultdict(Counter)
            for i, j in m.items():
                c = compare(base[i], mine[j], lambda p, q: p in gone and q != p and p not in allparams)
                for p, q in c['renamed']:
                    vt[p][q] += 1
            maps[(n, g)] = {p: sorted(qs.items(), key=lambda kv: (-kv[1], kv[0]))[0][0] for p, qs in vt.items()}
            votes[(n, g)] = vt

    lines, residue, relations = [], [], []
    count = defaultdict(lambda: defaultdict(int))          # difference: contract: clauses
    putpreds = set()
    put_of = {}       # (n, j): {parameter literal (in C' names): [literals put]}
    decl_of = {}      # (n, j): (declaring contract, key)
    tests = defaultdict(list)

    for n in order:
        mine = clauses(n)
        dup = Counter((a['label'], a['name']) for a in mine)
        gen = set()
        nres = 0
        for j, a in enumerate(mine):
            diffs, res = [], []
            ss = src.get((n, j), [])
            # (c) one clause that two contracts both have is one source
            uniq = []
            for g, i in ss:
                b = clauses(g)[i]
                if not any(Counter(x['head']) == Counter(b['head']) and Counter(x['body']) == Counter(b['body']) and x['guards'] == b['guards'] and x['suffix'] == b['suffix']
                           for _, _, x in uniq):
                    uniq.append((g, i, b))
            stext = '-'
            if ss:
                stext = '; '.join('%s %s %s' % (' and '.join(g2 for g2, i2 in ss if clauses(g2)[i2]['text'] == b['text'] or (g2, i2) == (g, i)), b['label'], b['name']) for g, i, b in uniq)
            if len(uniq) > 1:
                res.append('R8 two sources, in %s' % ' and '.join(g for g, _, _ in uniq))
            if dup[(a['label'], a['name'])] > 1:
                res.append('R9 %d clauses of %s have the label %s and the name %s' % (dup[(a['label'], a['name'])], n, a['label'], a['name']))
            # the declared clause with a parameter (b)
            dk = None
            for part in a['labels']:
                c = R.NM.find(part, names)
                dn = part.rsplit(':', 1)[0]
                if dn in decl and (part, a['name']) in decl[dn]:
                    d = decl[dn][(part, a['name'])]
                    if any(R.pred(x) in allparams for x in d['head'] + d['body']):
                        dk = (dn, (part, a['name']))
            if dk is None:
                for g, i, b in uniq:
                    if (g, i) in decl_of:
                        dk = decl_of[(g, i)]
                        break
            if dk:
                decl_of[(n, j)] = dk
            comps = []
            for g, i, b in uniq:
                mp = maps[(n, g)]
                comps.append((g, i, b, compare(b, a, lambda p, q: mp.get(p) == q)))
            # R5: the parameters of the declared clause
            putlits = Counter()
            if dk:
                d = decl[dk[0]][dk[1]]
                D = {'head': Counter(d['head']), 'body': Counter(d['body']), 'guards': set()}
                c = compare(D, a, lambda p, q: False)
                absent = [x for x in c['lost'] if x[2] in allparams]
                if absent:
                    put = {}
                    more = [x for x in c['added'] if x[0] != 'g']
                    for P in absent:
                        Pt = thr(P[4], c['theta'])
                        how = None
                        if P[2] in binds[n]:
                            args, ls = binds[n][P[2]]
                            sub = dict(zip(args, lit(Pt, P[0])[3]))
                            put[Pt] = [re.sub(r'\b[a-z]\b', lambda m: sub.get(m.group(0), m.group(0)), x) for x in ls]
                            how = 'by the first line of %s' % n
                        else:
                            for g, i, b, cc in comps:
                                sp = put_of.get((g, i))
                                if sp:
                                    for k, v in sp.items():
                                        if R.pred(k) == P[2]:
                                            img = [thr(x, cc['theta']) for x in v]
                                            have = Counter(list(a['head']) + list(a['body']))
                                            if not (Counter(img) - have):
                                                put[Pt] = img
                                                how = 'as in the source'
                        if how is None:
                            put[Pt] = None
                        P_how = how
                        put.setdefault('how', {})[Pt] = P_how
                    hows = put.pop('how')
                    left = [P for P in put if put[P] is None]
                    if left:
                        taken = Counter(x for P in put if put[P] for x in put[P])
                        free = []
                        for x in more:
                            if taken[x[4]] > 0:
                                taken[x[4]] -= 1
                            elif x[0] == ('h' if any(lit(P, 'h')[3] and P in [thr(y[4], c['theta']) for y in absent if y[0] == 'h'] for P in left) else 'b'):
                                free.append(x[4])
                        for P in left:
                            put[P] = []
                        given = {}
                        pending = list(free)
                        for rnd in range(3):
                            for x in list(pending):
                                xv = set(v for arg in lit(x, 'b')[3] for v in vars_of(arg))
                                best, bk = None, None
                                for P in left:
                                    pv = set(v for arg in lit(P, 'b')[3] for v in vars_of(arg))
                                    for y in put[P]:
                                        pv |= set(v for arg in lit(y, 'b')[3] for v in vars_of(arg)) if rnd else set()
                                    k = (len(xv & pv), -len(pv - xv))
                                    if k[0] > 0 and (bk is None or k > bk):
                                        best, bk = P, k
                                if best is not None:
                                    put[best].append(x)
                                    pending.remove(x)
                        for P in left:
                            hows[P] = 'the literals %s has more than the declared clause %s %s' % (n, dk[1][0], dk[1][1])
                    for P in put:
                        for x in put[P]:
                            putlits[x] += 1
                        res.append('R5 the parameter %s of %s (%s %s) is absent; in its place: %s: %s (%s)'
                                   % (P, dk[0], dk[1][0], dk[1][1], ', '.join(put[P]) or 'nothing', kind_put(P, put[P]), hows[P]))
                    put_of[(n, j)] = put
                    putpreds.update(R.pred(x) for v in put.values() for x in v)
            derived = bool(R.DERIVED.match(a['name'])) or 'taken for' in a['suffix']
            if not bases[n]:
                diffs.append('-')
            elif not ss:
                if derived:
                    res.append('R10 a derived act with no source in %s: no operation of the paper gives it' % ' or '.join(bases[n]))
                else:
                    diffs.append('2 Added')
                    gen.add('extends')
            for g, i, b, c in comps:
                tag = ' [against %s]' % g if len(uniq) > 1 else ''
                lost, added = list(c['lost']), list(c['added'])
                d0 = len(diffs)
                # R5 for a literal of the source put for a parameter, or a parameter itself
                sput = Counter(x for v in (put_of.get((g, i)) or {}).values() for x in v)
                for x in list(lost):
                    if x[0] != 'g' and (x[2] in allparams or sput[x[4]] > 0):
                        lost.remove(x)
                        if x[2] in allparams:
                            if not any(r.startswith('R5') and R.pred(r.split('parameter ', 1)[1]) == x[2] for r in res):
                                res.append('R5 the parameter %s of the source is absent%s' % (x[4], tag))
                        else:
                            sput[x[4]] -= 1
                            res.append('R5 %s, put in the source for a parameter, is absent%s' % (x[4], tag))
                # (h) a clause with no declared clause of its own (a derived act) in a contract whose first line binds parameters
                bp = {R.pred(y) for _, ls in binds[n].values() for y in ls}
                hl = [x for x in lost if x[0] == 'b' and x[2] in putpreds and x[2] not in bp]
                ha = [x for x in added if x[0] == 'b' and x[2] in bp]
                if not dk and hl and ha:
                    for x in hl:
                        lost.remove(x)
                    for x in ha:
                        added.remove(x)
                    res.append('R5 %s, put in the source for parameters, absent; in their place, as the first line of %s binds: %s%s'
                               % (', '.join(x[4] for x in hl), n, ', '.join(x[4] for x in ha), tag))
                # the literals put for a parameter are no weakening
                pl = Counter(putlits)
                for x in list(added):
                    if x[0] != 'g' and pl[x[4]] > 0 and not any(thr(y[4], c['theta']) == x[4] for y in c['si']):
                        pl[x[4]] -= 1
                        added.remove(x)
                # R4: the predicate of a source literal with other arguments
                mp = maps[(n, g)]
                for x in list(lost):
                    for y in added:
                        if y[0] == x[0] and y[1] == x[1] and x[0] != 'g' and y[2] == mp.get(x[2], x[2]):
                            la, lb = len(y[3]), len(x[3])
                            res.append('R4 %s of the source is %s%s: %s arguments%s' % (x[4], y[4], ' (renamed)' if y[2] != x[2] else '',
                                                                                 'other' if la == lb else 'fewer' if la < lb else 'more', tag))
                            lost.remove(x)
                            added.remove(y)
                            break
                lb = [x[4] for x in lost if x[0] == 'b']
                lh = [x[4] for x in lost if x[0] == 'h']
                lg = [x[4] for x in lost if x[0] == 'g']
                ab = [x[4] for x in added if x[0] == 'b']
                ah = [x[4] for x in added if x[0] == 'h']
                ag = sorted([x[4] for x in added if x[0] == 'g'], key=lambda s: ('(' not in s, s))
                if lb:
                    res.append('R1 absent from the body: %s%s' % (', '.join(lb), tag))
                if lg:
                    place, ag = ag[:len(lg)], ag[len(lg):]
                    res.append('R2 absent from the guards: %s%s%s' % (', '.join(lg), '; in its place: %s' % ', '.join(place) if place else '', tag))
                if lh:
                    res.append('R3 absent from the head: %s%s' % (', '.join(lh), tag))
                if c['merged']:
                    res.append('R6 %s%s' % ('; '.join(c['merged']), tag))
                sp = {frozenset(c['theta'].get(v, v) for v in p) for p in pairs_of(b['suffix'])}
                sp = {p for p in sp if len(p) == 2}
                if sp != pairs_of(a['suffix']):
                    res.append('R6 possibly one: the source has %s, the clause %s%s' % (
                        ', '.join(sorted('='.join(sorted(p)) for p in sp)) or 'none', ', '.join(sorted('='.join(sorted(p)) for p in pairs_of(a['suffix']))) or 'none', tag))
                if thr(rest_of(b['suffix']), c['theta']) != rest_of(a['suffix']):
                    res.append("R10 the source has '%s' after its guards, the clause '%s'%s" % (rest_of(b['suffix']), rest_of(a['suffix']), tag))
                if ab:
                    twice = [x for x in ab if (Counter(a['body'])[x] > 1)]
                    diffs.append('3 Weakened by literals: %s%s%s' % (', '.join(ab), ' (the body has %s twice)' % ', '.join(sorted(set(twice))) if twice else '', tag))
                    gen.add('weakens')
                if ag:
                    diffs.append('4 Weakened by guarding agents: %s%s' % (', '.join(ag), tag))
                    gen.add('weakens')
                if ah:
                    notes = []
                    for x in ah:
                        s = '%s (its predicate %s in %s' % (x, 'occurs' if R.pred(x) in preds[g] else 'does not occur', g)
                        if x.startswith('~'):
                            s += '; the body %s' % ('has its atom' if x[1:] in a['body'] else 'has a wildcard, no one atom' if '_' in R.split_args(x) else 'does not have its atom')
                        notes.append(s + ')')
                    diffs.append('5 Strengthened by literals: %s%s' % (', '.join(notes), tag))
                    gen.add('strengthens')
                if c['renamed']:
                    diffs.append('6 Renamed: %s%s' % (', '.join('%s to %s' % pq for pq in c['renamed']), tag))
                    gen.add('renames')
                if len(diffs) == d0 and not any(r.endswith(tag) and tag or not tag for r in res[-1:] if False):
                    if not (lb or lg or lh or c['merged']) and not any(r.startswith(('R4', 'R1', 'R2', 'R3', 'R6', 'R10', 'R5')) for r in res):
                        diffs.append('1 Same' + tag)
            if not diffs and not res:
                diffs.append('1 Same')
            line = '%s | %s | %s | %s | %s' % (n, a['label'], a['name'], stext, '; '.join(diffs + res))
            lines.append(line)
            for d in diffs:
                if d != '-':
                    count[d.split(':')[0].split(' [')[0]][n] += 1
            for r in sorted({r.split(' ')[0] for r in res}):
                count[r][n] += 1
            if res:
                nres += 1
                residue.append(line)
                residue.append('    clause: ' + a['text'])
                for g, i, b in uniq:
                    residue.append('    source (%s): %s' % (g, b['text']))
                if dk and any(r.startswith('R5') for r in res):
                    residue.append('    declared (%s/schemas.txt): %s' % (R.NM.file_of(dk[0]), decl[dk[0]][dk[1]]['text']))
                residue.append('')
        # R7
        for g in bases[n]:
            for i in unsourced[(n, g)]:
                b = clauses(g)[i]
                line = '%s | - | - | %s %s %s | R7 this clause of %s has no clause of %s of which it is the source' % (n, g, b['label'], b['name'], g, n)
                lines.append(line)
                residue.append(line)
                residue.append('    source (%s): %s' % (g, b['text']))
                residue.append('')
                count['R7'][n] += 1
                nres += 1
        if len(bases[n]) >= 2:
            gen.add('unites')
        if 'weakens' in gen and 'strengthens' in gen:
            gen.add('amends')
        rel = [x for x in ('extends', 'unites', 'weakens', 'strengthens', 'amends', 'renames') if x in gen]
        relations.append('%s\n    first line: %s\n    contracts named: %s\n    its lines generate: %s\n    lines of residue: %d\n'
                         % (n, the_first_line(n) or '(none)', ', '.join(bases[n]) or 'none', ', '.join(rel) or ('nothing: every clause is its source' if bases[n] else '-'), nres))

    key = lambda l: (l.split(' | ')[0])
    w = lambda f, ls: open(os.path.join(OUT, f), 'w', encoding='utf-8').write('\n'.join(ls) + '\n')
    head = ['contract | label | name | source | differences   (sweep.py; %d contracts, %d clauses, %d lines)' % (
        len(names), sum(len(clauses(n)) for n in names), len(lines))]
    lines.sort(key=key)
    w('sweep-lines.txt', head + lines)
    w('sweep-residue.txt', ['The residue: every line with a difference the paper does not have, with both clauses in full (sweep.py).', ''] + residue)
    relations.sort()
    w('sweep-relations.txt', ['Each contract: its first line as it is, and the relations of the paper that its lines generate (sweep.py).',
                              'extends: a clause Added; unites: two contracts or more named; weakens: a clause 3 or 4; strengthens: a clause 5; amends: both; renames: a clause 6.', ''] + relations)
    ml = ['The map of predicates of each contract from each contract its first line names (sweep.py, a).', '']
    for (n, g) in sorted(maps):
        mp = maps[(n, g)]
        if not mp:
            continue
        one = len(set(mp.values())) == len(mp)
        ml.append('%s from %s: %s' % (n, g, ', '.join('%s to %s' % (p, q) for p, q in sorted(mp.items()))))
        ml.append('    one-to-one: %s' % ('yes' if one else 'no'))
        has = sorted(q for q in mp.values() if q in preds[g])
        ml.append('    maps a predicate to one %s has: %s' % (g, ', '.join(has) if has else 'no'))
        other = ['%s also to %s (%d clauses)' % (p, q, k) for p, qs in sorted(votes[(n, g)].items()) for q, k in sorted(qs.items()) if q != mp[p]]
        if other:
            ml.append('    not one map: %s' % '; '.join(other))
        ml.append('')
    ml.append('Contracts with no map (no predicate renamed): %s' % ', '.join(sorted({n for n in names} - {n for (n, g) in maps if maps[(n, g)]})))
    w('sweep-maps.txt', ml)
    sm = ['The summary of the sweep (sweep.py): %d contracts, %d clauses.' % (len(names), sum(len(clauses(n)) for n in names)), '',
          'For each difference, the number of clauses and the contracts that have it (a clause with several is counted under each).', '']
    title = {'1 Same': '1 Same', '2 Added': '2 Added', '3 Weakened by literals': '3 Weakened by literals', '4 Weakened by guarding agents': '4 Weakened by guarding agents',
             '5 Strengthened by literals': '5 Strengthened by literals', '6 Renamed': '6 Renamed',
             'R1': 'R1 a body literal of the source is absent', 'R2': 'R2 a guard of the source is absent, or another in its place',
             'R3': 'R3 a head literal of the source is absent', 'R4': 'R4 the predicate of a source literal with other arguments',
             'R5': 'R5 a parameter has other literals in its place', 'R6': 'R6 two variables are one, or the pairs possibly one differ',
             'R7': 'R7 a clause of G with no clause of G\' (counted by clauses of G)', 'R8': 'R8 two sources', 'R9': 'R9 one label and one name twice', 'R10': 'R10 other'}
    for k in title:
        cs = count.get(k, {})
        sm.append('%s: %d clauses, %d contracts' % (title[k], sum(cs.values()), len(cs)))
        if cs:
            sm.append('    ' + ', '.join('%s %d' % (c, v) for c, v in sorted(cs.items())))
    none = [n for n in names if not bases[n]]
    sm += ['', 'Contracts whose first line names no contract (their clauses have no line of difference): %s' % ', '.join(none),
           'Clauses with a residue: %d; lines R7: %d.' % (sum(1 for l in lines if re.search(r'[|;] R\d', l) and ' | R7 ' not in l), sum(count['R7'].values())), '',
           'The lines "Parameters:": ' + '; '.join('%s: %s' % (n, ', '.join(params[n])) for n in names if params[n]),
           'Constants read from the bindings of first lines: %s' % (', '.join(sorted(CONST)) or 'none'), '',
           'Citations of schemas.txt that add effects (" + " before "<-"): %d' % sum(len(v) for v in cites.values())]
    for n in names:
        for l in cites[n]:
            m = R.LINE.match(l)
            name, _, rest = m.group(3).partition(': ')
            ref = re.match(r'(%s)' % R.NM.REF, rest).group(1)
            cn = ref.rsplit(':', 1)[0]
            eff = [x for x in R.split_top(rest.split(' <-')[0].split(' + ', 1)[1]) if re.match(r'~?[a-z_]', x)]
            inb = sorted({R.pred(x) for x in eff if cn in preds and R.pred(x) in preds[cn]})
            sm.append('    %s %s %s: cites %s; adds %s; of a predicate %s has: %s' % (n, m.group(1), name, ref, ', '.join(eff), cn, ', '.join(inb) or 'none'))
    sm += ['', 'Every line of the residue:', ''] + [l for l in lines if re.search(r'[|;] R\d', l)]
    w('sweep-summary.txt', sm)
    print('%d contracts, %d clauses, %d lines; %d with a residue' % (len(names), sum(len(clauses(n)) for n in names), len(lines), sum(1 for l in lines if re.search(r'[|;] R\d', l))))
    for k in title:
        print('  %s: %d' % (k, sum(count.get(k, {}).values())))


if __name__ == '__main__':
    main()
