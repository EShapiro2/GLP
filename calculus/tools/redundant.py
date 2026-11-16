#!/usr/bin/env python3
"""redundant.py --- the acts of each contract that another act of it makes redundant (Udi, 2026-11-16, to Calculus #7:
"require that a contract has no logically-redundant clauses ... such a clause is removed automatically").

    python3 tools/redundant.py [NAME ...]     the contracts named, or all; a line for each redundant act, then the counts

An act X of a full contract is redundant when another act Y of it, not X, has the same effects up to a renaming of names,
and Y's conditions and Y's persons are among X's under that renaming: whatever X lets be done, Y lets be done with no more.
The acts are those of the expanded schemas (sasx/) as toglp.schemas reads them, an act no binding enables left out.
- The renaming is one to one, maps a name to a name of the same kind (the names lines of the contracts that wrote the
  two acts: a person to a person, a ward to a ward, a number to a number).
- The effects are the head, the atoms added and ended; the conditions the body, the atoms required and forbidden (an atom
  the head ends is required, and is in the body), with person(X) for a guard X its contract declares a person in a
  contract with wards, as the term has it (toglp.term); the persons those who decide the act and those who must consent.
- The possibly-one pairs: every pair of X renamed is a pair of Y, so Y allows each binding of one agent that X allows.
- Of two acts that make each other redundant ("equal both ways") one is listed, as a pair; an act had by two paths is one
  act in sasx/ and is not counted.

A line: NAME, the redundant act (its label and name), "by" the act that makes it redundant, and what X has more:
conditions, persons.  Pairs equal both ways are listed apart.  Standard library only.
"""
import os, re, sys
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import gcsas, toglp

NAME = re.compile(r'^[A-Z][A-Za-z]*$')


def bare(a):
    return a.lstrip('*#')


def names_of(e):
    out = []
    for l in e['head'] + e['body']:
        for a in l[2]:
            for t in re.findall(r'[A-Z][A-Za-z]*', a):
                if t not in out:
                    out.append(t)
    for g in e['guards'] + e.get('must', []):
        for t in re.findall(r'[A-Z][A-Za-z]*', g):
            if t not in out:
                out.append(t)
    return out


def ren(a, s):
    """The argument a with every name of s renamed (inside 'Alice-coin', '*Sum', 'Item@Alice' too)."""
    return re.sub(r'[A-Z][A-Za-z]*', lambda m: s.get(m.group(0), m.group(0)), a)


def lit(l, s):
    return (l[0], l[1], tuple(ren(a, s) for a in l[2]))


def match_lits(ys, xs, s, kind_ok, used):
    """Extensions of the renaming s mapping every literal of ys onto a literal of xs, one to one on names."""
    if not ys:
        yield s
        return
    y, rest = ys[0], ys[1:]
    for x in xs:
        if x[0] != y[0] or x[1] != y[1] or len(x[2]) != len(y[2]):
            continue
        t, ok = dict(s), True
        for ay, ax in zip(y[2], x[2]):
            ny, nx = re.findall(r'[A-Z][A-Za-z]*', ay), re.findall(r'[A-Z][A-Za-z]*', ax)
            if len(ny) != len(nx) or re.sub(r'[A-Z][A-Za-z]*', 'N', ay) != re.sub(r'[A-Z][A-Za-z]*', 'N', ax):
                ok = False
                break
            for a, b in zip(ny, nx):
                if a in t:
                    if t[a] != b:
                        ok = False
                        break
                elif b in t.values() or not kind_ok(a, b):
                    ok = False
                    break
                else:
                    t[a] = b
            if not ok:
                break
        if ok:
            yield from match_lits(rest, xs, t, kind_ok, used)


def covers(y, x, ky, kx):
    """A renaming under which y makes x redundant, or None: same head, y's body among x's, those y asks among those x asks, y's persons among x's, same possibly-one pairs."""
    hy, hx = sorted(y['head'], key=lambda l: (l[0], l[1], len(l[2]))), x['head']
    if len(set(hy)) != len(set(hx)) or sorted((l[0], l[1], len(l[2])) for l in set(hy)) != sorted((l[0], l[1], len(l[2])) for l in set(hx)):
        return None
    kind_ok = lambda a, b: (ky.get(a, frozenset()) & kx.get(b, frozenset())) or (not ky.get(a) and not kx.get(b)) or a == b
    for s in match_lits(list(set(hy)), list(set(hx)), {}, kind_ok, None):
        if {lit(l, s) for l in hy} != set(hx):
            continue
        for t in match_lits(list(set(y['body'])), list(set(x['body'])), s, kind_ok, None):
            gy = [g for g in y['guards'] if '(' not in g]
            gx = set(g for g in x['guards'] if '(' not in g)
            free = [g for g in gy if g not in t]
            if free:
                continue
            my, mx = set(y.get('must', [])), set(x.get('must', []))
            # the will of one who is asked is a condition of the act; one who must consent is not asked (Udi, 2026-11-16: logical implication)
            if not {t[g] for g in gy if g not in my} <= (gx - mx):
                continue
            if not {t[g] for g in gy} <= gx or any('(' in g for g in y['guards']) and sorted(ren(g, t) for g in y['guards'] if '(' in g) != sorted(g for g in x['guards'] if '(' in g):
                continue
            if not {t.get(g, g) for g in y.get('must', [])} <= gx | mx:
                continue
            sy = {frozenset(ren(a, t) for a in p.split('=')) for p in y.get('same', [])}
            sx = {frozenset(p.split('=')) for p in x.get('same', [])}
            if not sx <= sy:          # every one-agent binding X allows, Y allows
                continue
            return t
    return None


def survey(n, w):
    return redundant_pairs(n, w, [e for e in toglp.schemas(n) if e['kind'] == 'act'])


def redundant_pairs(n, w, acts):
    """([(X, Y, conditions X has more, persons X has more)], [(X, Y) equal both ways]) of the acts of n; gcsas.py's
    removal (remove_redundant) takes its decisions from this."""
    acts = [e for e in acts if e['kind'] == 'act']
    memo = {}
    # The conditions as the term has them: a guard the act's own contract declares a person is required a person, in a
    # contract with wards (toglp.term, 'a guard the act's own contract declares a person or a party is a person').
    marks = toglp.marks_of(acts) if any(l[1] == 'person' for e in acts for l in e['head']) else {}
    if marks:
        pm = {}
        for i, e in enumerate(acts):
            o = re.split(r'[:+]', e['label'])[0]
            if o not in pm:
                pm[o] = frozenset(x for x, ks in gcsas.kinds_from(w, n, o).items() if ks & {'person', 'party'} and not ks & {'organisation', 'ward'}) if o in w.known else frozenset()
            has = lambda p, g: any(not l[0] and l[1] == p and l[2] and l[2][0] == g for l in e['body'])
            made = {l[2][0] for l in e['head'] if l[1] == 'identity' and not l[0]}
            add = [(False, 'person', (g,)) for g in e['guards'] if '(' not in g and g in pm[o] | frozenset((e.get('taken') or {}).values()) | {gcsas.VOTER}
                   and g not in made and not has('person', g) and not any(has(m, g) and has(u, g) for m, u in marks.items())]
            if add:
                acts[i] = dict(e, body=list(e['body']) + add)

    def kinds(e):
        o = re.split(r'[:+]', e['label'])[0]
        if o not in memo:
            memo[o] = {x: frozenset(ks) for x, ks in gcsas.kinds_from(w, n, o).items()} if o in w.known else {}
        return memo[o]
    sig = lambda e: tuple(sorted((l[0], l[1], len(l[2])) for l in set(e['head'])))
    by = {}
    for e in acts:
        by.setdefault(sig(e), []).append(e)
    red, both = [], []
    for group in by.values():
        for i, x in enumerate(group):
            for j, y in enumerate(group):
                if i == j:
                    continue
                t = covers(y, x, kinds(y), kinds(x))
                if t is None:
                    continue
                back = covers(x, y, kinds(x), kinds(y))
                if back is not None:
                    if i < j:
                        both.append((x, y))
                    continue
                more_c = sorted(set(x['body']) - {lit(l, t) for l in y['body']})
                more_p = sorted(set(g for g in x['guards'] if '(' not in g) - {t[g] for g in y['guards'] if '(' not in g}) + \
                    ['must ' + g for g in set(x.get('must', [])) - {t.get(g, g) for g in y.get('must', [])}]
                red.append((x, y, more_c, more_p))
    return red, both


def main():
    w = gcsas.World(sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'gr')) if f.endswith('.txt') and not f.startswith('NOTES-')))
    names = sys.argv[1:] or sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'sasx')) if f.endswith('.txt'))
    tot, totb, counts = 0, 0, []
    for n in names:
        red, both = survey(n, w)
        for x, y, c, p in red:
            print('%-10s %s %s  by  %s %s;  more: %s' % (n, x['label'], x['name'], y['label'], y['name'],
                  '; '.join([gcsas.lit_s(l) for l in c] + p) or 'nothing'))
        for x, y in both:
            print('%-10s equal both ways: %s %s  and  %s %s' % (n, x['label'], x['name'], y['label'], y['name']))
        if red or both:
            counts.append('%s %d%s' % (n, len(red), ' (+%d both ways)' % len(both) if both else ''))
        tot += len(red)
        totb += len(both)
    print('counts: ' + ', '.join(counts))
    print('%d contracts, %d acts redundant, %d pairs equal both ways' % (len(names), tot, totb))
    return 0


if __name__ == '__main__':
    sys.exit(main())
