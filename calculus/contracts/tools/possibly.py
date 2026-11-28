#!/usr/bin/env python3
"""possibly.py --- the pairs of roles a contract says are possibly one agent, and whether GSC's Definition Binding lets
one agent take both.

    python3 tools/possibly.py [NAME ...]     the contracts named, or all; a line for each pair, then the count

For each act of each contract's expanded schemas (sasx/) that writes "possibly X=Y", and each such pair: GSC's
Definition Binding sends distinct agent roles to distinct agents, "but for two the schema marks as possibly one, which
it may send to one agent if neither of them adds or deletes an atom naming the other" (as vGLP's compiler has it,
programs/vglp/cva/compiler/local.glp, rule_ok and pair_ok).  A role adds or deletes an atom when the atom is in the
act's effect, added or deleted (cleared included), and held by the role, its first place; it names the other when the
other is one of its places.  A pair of which one is no role of the act (it holds no atom the act requires or adds and
does not decide it) is bound by no such rule and is printed "not a role".

Where it does not hold, the pair is "covered by" the act of one role of the contract whose schema is the act's with the
two names one (either name kept), as GSN's Post own is Post with Bob written Alice; or "exempt", where EXEMPT below gives
the sentence of the contract's text by which one agent's case does not arise or an act that already is that case.

A line: NAME, act, the pair, "holds", "covered by ACT", "exempt: REASON", "does not hold: X adds p(...)" or "not a role:
X".  The last line counts each; the exit code is 1 where a pair does not hold and nothing covers or exempts it.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM      # the names of the contracts and their files (tools/names.py, NAMES.txt)
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import toglp


def roles_of(e):
    """The agent roles of an act as toglp.py takes them: the holders of its atoms, and those who decide it."""
    rs = []
    for l in e['head'] + e['body']:
        if l[2] and re.fullmatch(r'[A-Z][A-Za-z]*', l[2][0]) and l[2][0] not in rs:
            rs.append(l[2][0])
    return rs + [g for g in e['guards'] if '(' not in g and g not in rs]


def names_other(e, x, y):
    """An atom of the act's effect held by x that names y, or None."""
    for neg, p, args in e['head']:
        if args and args[0] == x and y in [a.lstrip('*#') for a in args[1:]]:
            return '%s%s(%s)' % ('~' if neg else '', p, ','.join(args))
    return None


def merged(e, y, x):
    """The act e with the name y written x: its required and changed atoms and those who decide it, as sets, the
    reflexive atoms knows(x,x) left out (gcsas.py writes none)."""
    sub = lambda l: (l[0], l[1], tuple(x if a.lstrip('*#') == y else a for a in l[2]))
    keep = lambda ls: frozenset(sub(l) for l in ls if not (sub(l)[1] == 'knows' and len(set(sub(l)[2])) == 1))
    return keep(e['head']), keep(e['body']), frozenset(x if g == y else g for g in e['guards'])


def covering(e, x, y, acts):
    """The act of one role among acts that is e with x and y one agent, either name kept; None where there is none."""
    want = {merged(e, y, x), merged(e, x, y)}
    for f in acts:
        if f is not e and (frozenset(f['head']), frozenset(l for l in f['body'] if not (l[1] == 'knows' and len(set(l[2])) == 1)), frozenset(f['guards'])) in want:
            return f['name']
    return None


# A pair whose one agent's case the contract's own text does not allow, with the sentence that says so, or whose case an
# act already covers that is not the act with the two names one: (the clause the act comes from, the act, the pair).
EXEMPT = {
    ('Groups:1', 'Message', 'Alice=Bob'): 'Friendship:1: "Alice and Bob may jointly befriend each other": a friend is another person, and Groups:1 is "Alice may send a message to her friend Bob."',
    ('Groups:3', 'Join', 'Alice=Bob'): 'Groups:2 Create: "Alice may create a group, of which she is the leader and first member."',
    ('Groups:3', 'Remove', 'Alice=Bob'): 'Groups:2 Close: "Alice may close the group by leaving it."',
    ('Groups:3', 'Leave', 'Alice=Bob'): 'Groups:2 Close: "Alice may close the group by leaving it."',
    ('Groups:4', 'Deliver', 'Alice=Carol'): 'Groups:4 Deliver leader, the leader\'s own receipt of each post (Groups:4: "Its leader delivers it to every current member").',
    ('Liquid DAO:1', 'Delegate own', 'Alice=Dave'): 'Liquid DAO\'s names line, "Carol or Dave possibly Alice": in Delegate own Alice is also Carol, and Carol and Dave are not possibly one person.',
    ('Liquid DAO:1', 'Delegate to the steward', 'Alice=Carol'): 'Liquid DAO\'s names line, "Carol or Dave possibly Alice": in Delegate to the steward Alice is also Dave, and Carol and Dave are not possibly one person.',
    ('Agora:4', 'Deliver to poster', 'Alice=Carol'): 'Agora:4: "The keeper delivers it to every member but the keeper, the poster included."',
}


def exempt(e, pr):
    return EXEMPT.get((re.split(r'[+]', e['label'])[0], e['name'], pr))


def term_names(n):
    """The Names of each schema of n's term, in the order of the acts of toglp.schemas(n): [(names, {(x, y)})], the
    second the pairs its marks say are possibly one, one(Name, Other), one(Name, [Others]) and one(Name, '_')."""
    t, _, roles = toglp.contract_term(n)
    if roles:
        return None
    out = []
    for m in re.finditer(r"sas\('?[A-Za-z0-9_-]+'?, \[((?:[^\[\]]|\[[^\[\]]*\])*)\]", t):
        items = re.findall(r"one\('([A-Za-z0-9_]+)', (?:'([^']*)'|\[([^\]]*)\])\)|'([A-Za-z0-9_]+)'", m.group(1))
        names = [a or d for a, b, c, d in items]
        pairs = set()
        for a, b, c, d in items:
            if a:
                ys = [y for y in names if y != a] if b == '_' else [b] if b else re.findall(r"'([^']+)'", c)
                pairs |= {tuple(sorted((a, y))) for y in ys}
        out.append((names, pairs))
    return out


def survey(n):
    out = []
    acts = [e for e in toglp.schemas(n) if e['kind'] == 'act']
    tn = term_names(n)
    tn = dict(zip(range(len(acts)), tn)) if tn and len(tn) == len(acts) else {}
    for i, e in enumerate(acts):
        rs = roles_of(e)
        names, marked = tn.get(i, ([], set()))
        declared = {tuple(sorted(p.split('='))) for p in e.get('same', [])}
        for x, y in sorted(marked - declared):      # no pair the declaration does not call possibly one has a mark
            out.append((n, e['label'] + ' ' + e['name'], '%s=%s' % (x, y), 'does not hold: marked one in the term, and not declared possibly one'))
        if not e.get('same'):
            continue
        for pr in e['same']:
            x, y = pr.split('=')
            miss = [z for z in (x, y) if z not in rs]
            if miss:      # a pair of which one is no role: the term marks it, where both are agent variables of its Names
                gone = [z for z in (x, y) if z not in names]
                st = ('not a role, not an agent: ' + ', '.join(gone)) if gone else ('not a role, marked: ' + ', '.join(miss)) if tuple(sorted((x, y))) in marked else 'does not hold: not a role, and not marked in the term: ' + ', '.join(miss)
                out.append((n, e['label'] + ' ' + e['name'], pr, st))
                continue
            a = names_other(e, x, y)
            b = names_other(e, y, x)
            bad = ['%s adds %s' % (x, a)] if a else []
            bad += ['%s adds %s' % (y, b)] if b else []
            assert toglp.binding_holds(e, x, y) == (not bad)      # toglp.py prints the mark by the same condition
            why = '; '.join(bad).replace('adds ~', 'deletes ')
            if not bad:
                st = 'holds'
            elif covering(e, x, y, acts):
                st = 'covered by %s (%s)' % (covering(e, x, y, acts), why)
            elif exempt(e, pr):
                st = 'exempt: %s (%s)' % (exempt(e, pr), why)
            else:
                st = 'does not hold: ' + why
            out.append((n, e['label'] + ' ' + e['name'], pr, st))
    return out


def main():
    names = sys.argv[1:] or NM.names_in(os.path.join(ROOT, 'sasx'))
    rows = [r for n in names for r in survey(n)]
    for r in rows:
        print('%-10s %-36s %-12s %s' % r)
    bad = sum(1 for r in rows if r[3].startswith('does not hold'))
    cnt = lambda w: sum(1 for r in rows if r[3].startswith(w))
    print('%d contracts, %d pairs possibly one agent: %d hold, %d covered by an act of one role, %d exempt, %d not roles (%d marked in the term, %d not agents); %d for which Binding does not hold or a mark is wrong' % (len(names), len(rows), cnt('holds'), cnt('covered'), cnt('exempt'), cnt('not a role'), cnt('not a role, marked'), cnt('not a role, not an agent'), bad))
    sys.exit(1 if bad else 0)


if __name__ == '__main__':
    main()
