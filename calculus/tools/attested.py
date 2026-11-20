#!/usr/bin/env python3
"""attested.py [NAME ...] [--root DIR] --- for each attested act of a contract, the acts that can end its record.

An attested clause (◆) lets a person record a fact no act can check: the platform enforces that only the person may
make the record, not that the record is true.  In Crawford and Ostrom's grammar of institutions (Ostrom 2005,
Understanding Institutional Diversity, ch. 5) a statement is a rule only where breaking it has a consequence that
another rule backs; a statement without one is a norm.  So for each attested act this tool lists the acts of the
same contract that can end its record: those that end a state the attested act writes (a literal ~p of their head,
p a predicate of the attested act's head), as Withdraw ends a recommendation.  Whether such an act is an answer to a
false record or only the record's ordinary course (Lend takes a thing off the shelf Shelve put it on) is for the
reader to judge; an attested act no act can end is a recorded norm: a false record is answered, if at all, off the
platform.

ROOT is the tree of the tools (gr/ and sasx/) or, with --root DIR, a publication directory (NAME/full-schemas.txt),
read as tools/relations.py reads them.  Only the contract's own attested acts are listed (the last part of the act's
label is the contract's); the acts that answer them are any of its expanded schemas.  One line an attested act:
    Recommendations:1 Recommend: ended by Withdraw (Recommendations:3)
    Stays:1 List place: no act ends it (a recorded norm)
and a last line counting them.  Standard library only.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import relations as R


def own(a, n):
    return a['labels'][-1].rsplit(':', 1)[0] == n


def written(a):
    return {R.pred(l) for l in a['head'] if not l.startswith('~')}


def ended(a):
    return {R.pred(l) for l in a['head'] if l.startswith('~')}


def run(names=None):
    rows = []
    for n in names or R.contracts():
        acts = R.schemas(n)
        if acts is None:
            continue
        for a in acts:
            if a['mark'] != '◆' or not own(a, n):
                continue
            w = written(a)
            ans = [(b['name'], b['label']) for b in acts if b is not a and ended(b) & w]
            rows.append((n, a['label'], a['name'], ans))
    return rows


def main():
    argv = sys.argv[1:]
    if '--root' in argv:
        k = argv.index('--root')
        R.ROOT = os.path.abspath(argv[k + 1])
        del argv[k:k + 2]
    rows = run(argv or None)
    for n, lab, name, ans in rows:
        if ans:
            print('%s %s: ended by %s' % (lab, name, ', '.join('%s (%s)' % x for x in ans)))
        else:
            print('%s %s: no act ends it (a recorded norm)' % (lab, name))
    print('%d attested acts, %d whose record an act of their contract can end' % (len(rows), sum(1 for r in rows if r[3])))


if __name__ == '__main__':
    main()
