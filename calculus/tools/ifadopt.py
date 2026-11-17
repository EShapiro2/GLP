#!/usr/bin/env python3
"""ifadopt.py --- a contract's Interface pragmas in the form an owner's adopted file takes them, proposed to the owner.

    python3 tools/ifadopt.py [NAME ...]     the contracts named, or every one with cascade/NAME/NAME.txt

cascade/NAME/NAME.txt, which tools/cascade.py writes and tools/publish.py publishes as interface.txt, is the file the
cascade compiles: one clause for each act of the full schemas, titled by the act, and its Interface pragmas, a line for
each act, keyed by the act.  An owner's adopted file (tools/adopt.py) has the contract's own clauses, and vGLP's reader
of pragmas (GLP, programs/vglp/cva/compiler/pragmas.sh and README.md, 1 and 2) keys a line by its clause's title, the
words before the first full stop or colon of the clause, as "Proposing" of "3. ■ Proposing. Propose: ...".  For each
contract this writes cascade/NAME/interface-adopted.txt: the line "Interface pragmas" and the same lines, keyed so.
Each clause of the contract's own with an act that has a line has one line: its title, a colon and the line of its
first such act; the line of each other act of the clause follows, indented two spaces, as "  Alice [Close]", each
with its questions after it, so that a question belongs to its own act's button line.  Who must consent to an act
is the act's schema's, the "must" of the Logic pragmas, as in the cascade's file: a person who must consent is not
asked, though the clause's words do not say it, as GnW's Enter, whose Carol does not yet exist.  Two acts of one clause with the
same proposer and no question share a button line, as GSN's "Group: Alice [Create] <name> [Close]".  An act of a clause of
a contract it is written from that a clause of its own changes, labelled so in the expanded schemas, as GSG:2+GnW:3,
is written under that clause of its own; one changed by a definition, as Befriend by GDO-L's, has no clause of its
own and keeps the inherited line.  The other acts of the contracts it is written from are not written: the reader takes their lines from those contracts' own files.  A
contract with no act of its own that has a line gets no file.

The ward's forms of the acts of parties and their vote acts ("Swap for Alice", "Vote of Alice for Swap") are derived
in GBank, GCorp and GCoop (gcsas.py, World.party_specs), parametric, which the cascade compiles to no platform and so
to no NAME.txt of their own; every contract written from one of them has them, labelled by the inherited clauses they
come from.  Their lines stand in the file of the contract that derives them, each under the title of the clause its
label names last (as "Swap" of DGC:2), a clause of a contract it extends, which vGLP's reader finds by the contract's
"% Extends:" line; every contract written from it inherits them by its own "% Extends:" line, the reader taking a
base's lines and keeping each button while the contract has the act it names.  The fields of a line are those the
reader asks of an instance, the variables its proposer's role does not bind, so the lines are taken from the cascade's
NAME.txt of the contracts written from it, which must agree; a contract none of whose instances has been through the
cascade gets none of them.

The file is a proposal to the owner, who may change its lines (README-PUBLIC.md).
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM      # the names of the contracts and their files (tools/names.py, NAMES.txt)
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import toglp

CLAUSE = re.compile(r'^(\d+)\.\s+(?:[■◆○]\s+)?([^.:]+)[.:]\s*(.*)$')      # the reader's form of a clause line (pragmas.sh)
FILE = 'interface-adopted.txt'


def titles(n):
    """{clause number: (title, the rest of the clause)} of the contract's own clauses, rev/NAME.txt, as the reader
    takes them; the reader skips the Logic pragmas of an adopted file, so these are the clauses it reads."""
    out = {}
    for l in open(os.path.join(ROOT, 'rev', NM.file_of(n) + '.txt'), encoding='utf-8'):
        m = CLAUSE.match(l.rstrip('\n'))
        if m:
            out[int(m.group(1))] = (m.group(2), m.group(3))
    return out




def kind_preds(n):
    """The predicates of the states of the kinds of note n itself declares ('Kind of note: bond, ...' of its Logic
    pragmas): bond and escrow_bond for Bonds; empty for a contract that declares none."""
    import gcsas
    w = gcsas.World(NM.names_in(os.path.join(ROOT, 'gr')))
    if n not in w.known:
        return set()
    ks = [k for k in gcsas.kinds_of(w, n) if k['decl'] == n]
    if not ks:
        return set()
    lex = w.lexicon(n)
    return {v[2] for k in ks for v in gcsas.kind_atoms(w, n, lex, k).values()}


_PARENT_ACTS = {}


def renamed_here(n, e):
    """Whether the act e, of a contract n is written from, is printed in n under another name than in every contract of
    n's first line that has it (Joint's "Deposit coin", Notes:4's Deposit of coins named so beside Joint:2's Deposit):
    the line of that contract's file names its act by the other name, which in n is n's own act, so e's line is n's
    (Calculus #8, 2026-11-16, on Code #33's "the schema deposit_coin ... is fitted by no line")."""
    import gcsas
    w = gcsas.World(NM.names_in(os.path.join(ROOT, 'gr')))
    if n not in w.known:
        return False
    parents, _, _ = gcsas.first_line(w.contract(n), w.known)
    head = sorted(e['head'])
    names = set()
    for p in parents:
        if p not in _PARENT_ACTS:
            try:
                _PARENT_ACTS[p] = [x for x in toglp.schemas(p) if x['kind'] == 'act']
            except (OSError, gcsas.Err):
                _PARENT_ACTS[p] = []
        names |= {x['name'] for x in _PARENT_ACTS[p] if x['label'].split('+')[0] == e['label'].split('+')[0] and sorted(x['head']) == head}
    return bool(names) and e['name'] not in names


def adopted(n, txt):
    """The text of interface-adopted.txt from the cascade's NAME.txt, txt; None where the contract has no line of its own."""
    ls = txt.split('\n')
    k = ls.index('Pragmas')
    clauses, sent = {}, {}      # the act's title in the cascade's file: its place among the acts of the full schemas, and its sentence
    for l in ls[1:k]:
        m = re.match(r'^(\d+)\. [■◆○] (.+?)\.  (.*)$', l)
        if m:
            clauses[m.group(2)] = int(m.group(1))
            sent[m.group(2)] = m.group(3)
    acts = [e for e in toglp.schemas(n) if e['kind'] == 'act']
    lines = []          # (act's title, its button line, its questions)
    for l in ls[k + 1:]:
        if not l.strip():
            continue
        if l.startswith('  '):
            lines[-1][2].append(l)
        else:
            t, rest = l.split(': ', 1)
            lines.append((t, rest, []))
    tl = {j: v for j, v in titles(n).items()}
    kp = kind_preds(n)          # the predicates of the kinds of note n declares: a form of an act of notes with one is n's act
    own = {}           # clause number, or (contract, clause number) of the clause of Notes a form comes from: [(button line, questions)]
    for t, rest, qs in lines:
        e = acts[clauses[t] - 1]
        if t != e['name'] and not re.fullmatch(re.escape(e['name']) + r' \d+', t):
            raise ValueError('%s: the act %s of cascade/%s/%s.txt is not %s of the full schemas' % (n, t, n, n, e['name']))
        mine = [l.rpartition(':')[2] for l in e['label'].split('+') if l.rpartition(':')[0] == n]
        if not mine and (kp and any(l[1] in kp for l in e['head']) or renamed_here(n, e)):
            # a form of an act of notes for a kind n declares (Bonds' Swap coin for bond, Pay bond, Deposit bond): n's act,
            # proposed in n's file under the title of the clause of Notes it comes from, which GLP's reader takes there
            # (programs/vglp/cva/compiler/pragmas.sh: a pragma line may name a clause of a contract its contract extends)
            o, _, oj = e['label'].split('+')[0].rpartition(':')
            ot = titles(o) if os.path.exists(os.path.join(ROOT, 'rev', NM.file_of(o) + '.txt')) else {}
            if int(oj) in ot:
                tl[(o, int(oj))] = ot[int(oj)]
                own.setdefault((o, int(oj)), []).append((rest, [q for q in qs if q.strip().split(':', 1)[0] not in set(e.get('must', ()))]))
            continue
        if not mine:
            continue          # an act of a contract this one is written from: its line is in that contract's file
        j = mine[-1]          # an act of an inherited clause that a clause of this one changes, as GSG:2+GnW:3, is that clause's
        if int(j) not in tl:
            continue          # changed by a definition of the contract, not a clause, as GDO-L's Befriend: the inherited line names it
        # Who must consent is the act's schema's, the 'must' of the Logic pragmas (sasx/NAME.txt), as in the cascade's
        # file: a party who must consent is a guard who is not asked (vGLP, 5.4), whatever the clause's words say, as
        # GnW's Enter, whose Carol does not yet exist (Calculus #6, 2026-11-15 21:30 UTC, on the review's F11).
        must = set(e.get('must', ()))
        qs = [q for q in qs if q.strip().split(':', 1)[0] not in must]
        own.setdefault(int(j), []).append((rest, qs))
    if not own:
        return None
    out = ['Interface pragmas']
    for j in sorted(own, key=lambda j: (1, j[0], j[1]) if isinstance(j, tuple) else (0, '', j)):      # n's own clauses, then those of Notes its forms come from
        first = True
        prev = None          # the proposer of the last button line, where it has no question
        for rest, qs in own[j]:
            who = rest.split(' ', 1)[0]
            if prev == who and not qs:
                out[-1] += ' ' + rest.split(' ', 1)[1]
                continue
            out.append(('%s: %s' if first else '  %s%s') % ((tl[j][0], rest) if first else ('', rest)))
            out.extend(qs)
            first = False
            prev = who if not qs else None
    return '\n'.join(out) + '\n'


def pragma_lines(n):
    """{act's title: (its button line, its questions)} of the cascade's cascade/NAME/NAME.txt, or None."""
    p = os.path.join(ROOT, 'cascade', NM.file_of(n), NM.file_of(n) + '.txt')
    if not os.path.exists(p):
        return None
    ls = open(p, encoding='utf-8').read().split('\n')
    out, cur = {}, None
    for l in ls[ls.index('Pragmas') + 1:]:
        if not l.strip():
            continue
        if l.startswith('  '):
            out[cur][1].append(l)
        else:
            cur, rest = l.split(': ', 1)
            out[cur] = (rest, [])
    return out


_world = []


def world():
    """The contracts of gr/, compiled and expanded by gcsas.py, once."""
    if not _world:
        import gcsas
        names = NM.names_in(os.path.join(ROOT, 'gr'))
        w = gcsas.World(names)
        for m in gcsas.order(names, w):
            sas, own = gcsas.compile_contract(w, m)
            gcsas.expand_text(w, m, own)
            gcsas.expand_schemas(w, m)
        _world.append((gcsas, w, names))
    return _world[0]


def derived(n):
    """The lines of the acts derived in n (the ward's forms and their vote acts), in the order of its expanded schemas,
    each under the title of the clause its label names last; [] where n derives none."""
    gcsas, w, names = world()
    if not w.party.get(n):
        return []
    acts = [e for e in w.sasx[n] if e['kind'] == 'act' and (e.get('party') or e.get('voteact'))]
    insts = [m for m in names if m != n and n in gcsas.order([m], w) and pragma_lines(m)]
    ref = None
    for m in insts:
        L = pragma_lines(m)
        mine = {e['name']: L.get(e['name']) for e in acts}
        if any(v is None for v in mine.values()):
            continue
        if ref is not None and mine != ref[1]:
            bad = [a for a in mine if mine[a] != ref[1][a]]
            raise ValueError('%s: the lines of its derived acts differ between %s and %s: %s' % (n, ref[0], m, ', '.join(bad[:5])))
        ref = ref or (m, mine)
    if ref is None:
        return []
    out = []
    for e in acts:
        c, num = e['label'].split('+')[-1].split(':')
        rest, qs = ref[1][e['name']]
        must = set(e.get('must', ()))
        out.append((titles(c)[int(num)][0], rest, [q for q in qs if q.strip().split(':', 1)[0] not in must]))
    return out


def lines_text(groups):
    """The lines of [(title, [(button line, questions)])], each title's first line 'Title: line', the others indented."""
    out = []
    for title, ls in groups:
        first, prev = True, None
        for rest, qs in ls:
            who = rest.split(' ', 1)[0]
            if prev == who and not qs:
                out[-1] += ' ' + rest.split(' ', 1)[1]
                continue
            out.append(('%s: %s' % (title, rest)) if first else '  ' + rest)
            out.extend(qs)
            first = False
            prev = who if not qs else None
    return out


def write(n):
    d = os.path.join(ROOT, 'cascade', NM.file_of(n))
    p, f = os.path.join(d, NM.file_of(n) + '.txt'), os.path.join(d, FILE)
    txt = adopted(n, open(p, encoding='utf-8').read()) if os.path.exists(p) else None
    der = derived(n)
    if der:             # the lines of the acts derived in n, after its own, grouped by the title of their clause
        groups = []
        for t, rest, qs in der:
            if t not in [g[0] for g in groups]:
                groups.append((t, []))
            next(g for g in groups if g[0] == t)[1].append((rest, qs))
        txt = (txt or 'Interface pragmas\n') + '\n'.join(lines_text(groups)) + '\n'
    if txt is None:
        if os.path.exists(f):
            os.remove(f)
        return 'no Interface pragmas of its own'
    open(f, 'w', encoding='utf-8').write(txt)
    return '%d lines' % (txt.count('\n') - 1)


def main():
    names = [NM.name_of(a) for a in sys.argv[1:]] or sorted(NM.name_of(n) for n in os.listdir(os.path.join(ROOT, 'cascade')) if os.path.isdir(os.path.join(ROOT, 'cascade', n)))
    for n in names:
        print('%-10s %s' % (NM.file_of(n), write(n)))


if __name__ == '__main__':
    main()
