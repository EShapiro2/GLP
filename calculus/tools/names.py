#!/usr/bin/env python3
"""names.py --- the names of the contracts and their files, in one place for every tool.

NAMES.txt (a copy of GLP's calculus/NAMES.txt, Udi, 2026-11-16, to Calculus #8) has a row for each contract: its name on
GLP's main, its name, its file name and the row and atom of cascade.sh and contract_named.  A name is one plain word or
two; the file name is the name with a hyphen for each space and no apostrophe ("Community Bank", Community-Bank.txt;
"Workers' Cooperative", Workers-Cooperative.txt).  A contract not in the table (a case of tests/) is named by its file.

    file_of(name)        the file name, without .txt
    name_of(file)        the name of the contract of a file name
    names_in(dir)        the names of the contracts of a directory of NAME.txt, the notes NOTES-*.txt left out
    atom_of(name)        the row and atom: the file name in lower case, a hyphen an underscore
    atom_on_main(name)   the row and atom on main today, before the renaming (the fifth column)
    glp_name(name)       its name on GLP's main, where its owner's file still has it
    of_glp(f)            the name of a file or directory in GLP, under its file name or its name on main
    add(path)            a test's made-up contracts, rows after the table's
    find(text, known)    the known names in text, longest first, each whole: (start, end, name)
NAME_RX, REF          a name, of the table or one word, and a citation or a label's part, NAME:n
"""
import os, re

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TABLE = []          # (name on main, name, file name, atom, atom on main)


def _rows(path):
    out = []
    for l in open(path, encoding='utf-8'):
        p = [x.strip() for x in l.rstrip('\n').split(' | ')]
        if len(p) in (4, 5) and p[0] != 'name on main':
            out.append(tuple(p) if len(p) == 5 else tuple(p) + (re.sub(r'[^a-z0-9]', '_', p[0].lower()),))
    return out


def _index():
    global _FILE, _NAME, _MAIN, _OLD, _ATOM0, NAME_RX, REF
    _FILE = {r[1]: r[2] for r in TABLE}
    _NAME = {r[2]: r[1] for r in TABLE}
    _MAIN = {r[1]: r[0] for r in TABLE}
    _OLD = {r[0]: r[1] for r in TABLE}
    _ATOM0 = {r[1]: r[4] for r in TABLE}
    # A name: one of the table's, longest first, so that 'Community Bank' is read whole and not as 'Bank'; else one
    # word, a contract of a case of tests/ in no table.  A word of a clause before a name is no part of it: 'Unlike
    # Bank:1' cites Bank.
    NAME_RX = r"(?:%s|%s)" % ("|".join(re.escape(r[1]) for r in sorted(TABLE, key=lambda r: -len(r[1]))), WORD)
    REF = r"(?<![A-Za-z0-9'-])%s:\d+" % NAME_RX      # a citation, or a label's part: NAME:n


def add(path):
    """A test's made-up contracts (NAMES.txt: 'A test may add made-up contracts: rows of the same form after the
    table's, in a table of its own that it builds from this one'): the rows of path, after the table's.  The tools of
    a test run in a process of their own read it from $CALCULUS_NAMES (tools/tests.py)."""
    TABLE.extend(r for r in _rows(path) if r not in TABLE)
    _index()


WORD = r"[A-Z][A-Za-z0-9'-]*"
TABLE.extend(_rows(os.path.join(ROOT, 'NAMES.txt')))
_index()
if os.environ.get('CALCULUS_NAMES') and os.path.exists(os.environ['CALCULUS_NAMES']):
    add(os.environ['CALCULUS_NAMES'])



def file_of(name):
    return _FILE.get(name) or name.replace(' ', '-').replace("'", '')


def name_of(f):
    return _NAME.get(f, f)


def names_in(d):
    return sorted(name_of(f[:-4]) for f in os.listdir(d) if f.endswith('.txt') and not f.startswith('NOTES-'))


def of_glp(f):
    return _NAME.get(f) or _OLD.get(f) or f


def atom_of(name):
    return file_of(name).lower().replace('-', '_')


def atom_on_main(name):
    return _ATOM0.get(name, atom_of(name))


def glp_name(name):
    return _MAIN.get(name, name)


def find(text, known):
    """The names of known in text, longest first, each whole (not inside a word): [(start, end, name)]."""
    ks = sorted(known, key=len, reverse=True)
    if not ks:
        return []
    rx = re.compile(r"(?<![A-Za-z'-])(%s)(?![A-Za-z'-])" % "|".join(re.escape(k) for k in ks))
    return [(m.start(), m.end(), m.group(1)) for m in rx.finditer(text)]
