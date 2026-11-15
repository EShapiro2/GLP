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
with its questions after it, so that a question belongs to its own act's button line.  The reader reads who must
consent to an act from the clause of the adopted file, not from its Logic pragmas: a person the act's sentence says
must consent and the clause does not is asked, "Carol: Alice asks for your consent to enter.", and a person the clause
says must consent is not.  Two acts of one clause with the
same proposer and no question share a button line, as GSN's "Group: Alice [Create] <name> [Close]".  An act of a clause of
a contract it is written from that a clause of its own changes, labelled so in the expanded schemas, as GSG:2+GnW:3,
is written under that clause of its own; one changed by a definition, as Befriend by GDO-L's, has no clause of its
own and keeps the inherited line.  The other acts of the contracts it is written from are not written: the reader takes their lines from those contracts' own files.  A
contract with no act of its own that has a line, and a parametric one, get no file.

The file is a proposal to the owner, who may change its lines (README-PUBLIC.md).
"""
import os, re, sys
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import toglp

CLAUSE = re.compile(r'^(\d+)\.\s+(?:[■◆○]\s+)?([^.:]+)[.:]\s*(.*)$')      # the reader's form of a clause line (pragmas.sh)
FILE = 'interface-adopted.txt'


def titles(n):
    """{clause number: (title, the rest of the clause)} of the contract's own clauses, rev/NAME.txt, as the reader
    takes them; the reader skips the Logic pragmas of an adopted file, so these are the clauses it reads."""
    out = {}
    for l in open(os.path.join(ROOT, 'rev', n + '.txt'), encoding='utf-8'):
        m = CLAUSE.match(l.rstrip('\n'))
        if m:
            out[int(m.group(1))] = (m.group(2), m.group(3))
    return out


MUST = re.compile(r'\b([A-Z][a-z]+) must consent\b')      # the reader's form (pragmas.sh)


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
    tl = titles(n)
    own = {}           # clause number: [(button line, questions)]
    for t, rest, qs in lines:
        e = acts[clauses[t] - 1]
        if t != e['name'] and not re.fullmatch(re.escape(e['name']) + r' \d+', t):
            raise ValueError('%s: the act %s of cascade/%s/%s.txt is not %s of the full schemas' % (n, t, n, n, e['name']))
        mine = [l.rpartition(':')[2] for l in e['label'].split('+') if l.rpartition(':')[0] == n]
        if not mine:
            continue          # an act of a contract this one is written from: its line is in that contract's file
        j = mine[-1]          # an act of an inherited clause that a clause of this one changes, as GSG:2+GnW:3, is that clause's
        if int(j) not in tl:
            continue          # changed by a definition of the contract, not a clause, as GDO-L's Befriend: the inherited line names it
        # Who must consent: the cascade's file takes it from the act's sentence of the Logic pragmas and asks them not;
        # the reader of an adopted file takes it from the contract's clause (pragmas.sh, "must consent"), so a person the
        # sentence says must consent and the clause does not is asked, and one the clause says must consent is not.
        who = rest.split(' ', 1)[0]
        mrev = set(MUST.findall(tl[int(j)][1]))
        qs = [q for q in qs if q.strip().split(':', 1)[0] not in mrev]
        asked = {q.strip().split(':', 1)[0] for q in qs}
        for g in MUST.findall(sent.get(t, '')):
            if g not in mrev and g != who and g not in asked:
                qs.append('  %s: %s asks for your consent to %s.' % (g, who, t.lower()))
                asked.add(g)
        own.setdefault(int(j), []).append((rest, qs))
    if not own:
        return None
    out = ['Interface pragmas']
    for j in sorted(own):
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


def write(n):
    d = os.path.join(ROOT, 'cascade', n)
    p, f = os.path.join(d, n + '.txt'), os.path.join(d, FILE)
    txt = adopted(n, open(p, encoding='utf-8').read()) if os.path.exists(p) else None
    if txt is None:
        if os.path.exists(f):
            os.remove(f)
        return 'no Interface pragmas of its own'
    open(f, 'w', encoding='utf-8').write(txt)
    return '%d lines' % (txt.count('\n') - 1)


def main():
    names = sys.argv[1:] or sorted(n for n in os.listdir(os.path.join(ROOT, 'cascade')) if os.path.isdir(os.path.join(ROOT, 'cascade', n)))
    for n in names:
        print('%-10s %s' % (n, write(n)))


if __name__ == '__main__':
    main()
