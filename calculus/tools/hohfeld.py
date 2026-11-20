#!/usr/bin/env python3
"""hohfeld.py [NAME ...] [--all] [--json] [--root DIR] --- for each act of a contract, who holds the power to take it and who
is liable to it, in Hohfeld's sense.

Hohfeld (1913, "Some Fundamental Legal Conceptions as Applied in Judicial Reasoning", Yale LJ 23(1):16-59):
a power is the ability to change another's legal relations by one's own act, and its correlative is a liability, the
position of the one whose relations are so changed; the opposite of a liability is an immunity.  On a schema: the
power is held by the act's guards, those who take it ('Alice', 'those who decide for Orga' for deciders(Orga),
'AliceOfficer for Alice' for 'AliceOfficer taken for Alice by AliceOfficer'); a person is liable to the act where
the act changes a state that person holds (the first place of a literal of its head) and that person is no guard.
A ward its guards act for is liable as a principal: "The creation of an agency relation involves, inter alia, the
grant of legal powers to the so-called agent, and the creation of correlative liabilities in the principal"
(Hohfeld 1913), and it is listed apart.  Every other person is immune to the act, which GSC's closure
condition already guarantees: an act changes only the states of its participants.

ROOT is the tree of the tools (gr/ and sasx/) or, with --root DIR, a publication directory (NAME/full-schemas.txt),
read as tools/relations.py reads them.  The acts are those of the contract's own clauses (the last part of the
act's label is the contract's); with --all, every act of its expanded schemas, those of its bases too.  An act no
one guards is taken by its machine, not by a person, and holds no power.  One line an act:
    DAO:3 Propose: power: Carol; liable: Alice (proposal)
    Organisation:3 Expel: power: those who decide for Orga; liable: Bob (participant); principal: Orga (has_participant)
and a last line counting the acts and those that make someone liable.  With --json, a list of
{"contract", "label", "name", "power", "liable": {person: [predicates]}, "principal": {ward: [predicates]}}.  Standard library only.
"""
import json, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import relations as R

VAR = re.compile(r'^[A-Z][A-Za-z0-9_]*$')


def powers(guards, takes):
    """The guards of an act, in words, the persons who hold the power, and the wards they act for (the ward a guard
    is taken for, the ward those who decide for it act for); takes: {guard: [wards it acts for]}."""
    words, holders, wards = [], set(), set()
    for g in sorted(guards):
        g = g.split(' must ')[0]          # 'Orga must Orga': Orga a guard, who must consent
        m = re.match(r'deciders\((\w+)', g)
        if m:
            words.append('those who decide for ' + m.group(1))
            wards.add(m.group(1))
        elif VAR.match(g):
            ws = takes.get(g, [])
            words.append(g + (' for ' + ' and '.join(ws) if ws else ''))
            holders.add(g)
            wards.update(ws)
        else:
            words.append(g)
    return words, holders, wards - holders


def act_positions(a, takes):
    """(power words, {liable person: [predicates]}, {principal ward: [predicates]}) of an act a."""
    words, holders, wards = powers(a['guards'], takes)
    liable, principal = {}, {}
    for lit in a['head']:
        args = R.split_args(lit)
        if not args:
            continue
        holder = args[0]
        if VAR.match(holder) and holder not in holders:
            d = principal if holder in wards else liable
            p = R.pred(lit)
            d.setdefault(holder, [])
            if p not in d[holder]:
                d[holder].append(p)
    return words, liable, principal


def taken_for(n):
    """{label+name: {guard: [wards]}} from the 'taken for W by G' of n's schemas, which relations.py leaves out."""
    out = {}
    p = R.path(n, 'schemas')
    if not os.path.exists(p):
        return out
    for l in open(p, encoding='utf-8'):
        m = R.LINE.match(l.rstrip('\n'))
        if not m or m.group(3).startswith('| '):
            continue
        name = m.group(3).partition(': ')[0]
        t = re.search(r' taken (for .*?)(?: possibly .*)?\.?$', m.group(3))
        if not t:
            continue
        d = {}
        for w, g in re.findall(r'for (\w+) by (\w+)', t.group(1)):
            d.setdefault(g, []).append(w)
        out[(m.group(1), name)] = d
    return out


def own(a, n):
    """True where act a is of a clause of n itself: the last part of its label is n's."""
    return a['labels'][-1].rsplit(':', 1)[0] == n


def run(names=None, every=False):
    rows = []
    for n in names or R.contracts():
        acts = R.schemas(n)
        if acts is None:
            continue
        tf = taken_for(n)
        for a in acts:
            if not every and not own(a, n):
                continue
            words, liable, principal = act_positions(a, tf.get((a['label'], a['name']), {}))
            rows.append({'contract': n, 'label': a['label'], 'name': a['name'], 'power': words, 'liable': liable,
                         'principal': principal})
    return rows


def main():
    argv = sys.argv[1:]
    if '--root' in argv:
        k = argv.index('--root')
        R.ROOT = os.path.abspath(argv[k + 1])
        del argv[k:k + 2]
    as_json, every = '--json' in argv, '--all' in argv
    names = [a for a in argv if a not in ('--json', '--all')]
    rows = run(names or None, every)
    if as_json:
        print(json.dumps(rows, ensure_ascii=False, indent=1))
        return
    for r in rows:
        li = '; '.join('%s (%s)' % (p, ', '.join(ps)) for p, ps in r['liable'].items()) or 'none'
        pr = '; '.join('%s (%s)' % (p, ', '.join(ps)) for p, ps in r['principal'].items())
        print('%s %s: power: %s; liable: %s%s' % (r['label'], r['name'], ', '.join(r['power']) or 'none (unguarded, taken by its machine)',
                                                li, '; principal: ' + pr if pr else ''))
    g = [r for r in rows if r['power']]
    print('%d acts, %d of them guarded; of these, %d make a person liable and %d a principal'
          % (len(rows), len(g), sum(1 for r in g if r['liable']), sum(1 for r in g if r['principal'])))


if __name__ == '__main__':
    main()
