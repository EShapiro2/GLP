#!/usr/bin/env python3
"""names_left.py --- what still names a contract by its name before the renaming (calculus/NAMES.txt).

    python3 tools/names_left.py            a count for each project, by servers/peoplesweb/tools/directories.py
    python3 tools/names_left.py PROJECT    each line of that project's files, as file:line: text
    python3 tools/names_left.py --all      every line

It finds, for each contract whose file name changed: its old file, OLD.txt and OLD.glp; its old record, cascade/OLD;
its old row, "cascade.sh <old atom>"; and its old atom, contract_named(<old atom>.  It does not find a contract's old
name in prose, in a title or in a citation, which a reader mends by eye.  The records (cascade/), the Calculus's
texts (calculus/contracts/) and the simulations' records are left out: they are printed, not written.
"""
import os, re, subprocess, sys, collections
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'servers', 'peoplesweb', 'tools'))
import directories as D

rows = [[x.strip() for x in l.split(' | ')] for l in open(os.path.join(ROOT, 'calculus', 'NAMES.txt'), encoding='utf-8')
        if l.count(' | ') >= 3 and not l.startswith('name on main')]
pats = []
for r in rows:
    if r[0] == r[2]:
        continue
    o, a = re.escape(r[0]), re.escape(r[4] if len(r) > 4 else re.sub(r'[^a-z0-9]', '_', r[0].lower()))
    pats += [r'(?<![A-Za-z0-9_-])%s\.(?:txt|glp)\b' % o, r'cascade/%s(?![A-Za-z0-9+_-])' % o,
             r'cascade\.sh %s\b' % a, r'contract_named\(%s\b' % a]
RX = re.compile('|'.join(pats))
SKIP = ('cascade/', 'calculus/contracts/', 'simulation/', 'calculus/NAMES.txt', 'tools/names_left.py')
found = collections.defaultdict(list)
for f in subprocess.run(['git', '-C', ROOT, 'ls-files'], capture_output=True, text=True).stdout.split('\n'):
    if not f or f.startswith(SKIP) or f.endswith(('.png', '.pdf', '.jpg', '.ttf')):
        continue
    try:
        ls = open(os.path.join(ROOT, f), encoding='utf-8').read().split('\n')
    except Exception:
        continue
    for i, l in enumerate(ls):
        if RX.search(l):
            found[D.answering(f)[1] if D.answering(f) else '?'].append('%s:%d: %s' % (f, i + 1, l.strip()[:160]))
arg = sys.argv[1] if len(sys.argv) > 1 else None
if arg is None:
    for p, ls in sorted(found.items(), key=lambda x: -len(x[1])):
        print('%-12s %4d lines in %3d files' % (p, len(ls), len({l.split(':')[0] for l in ls})))
    print('%-12s %4d' % ('all', sum(map(len, found.values()))))
else:
    for p, ls in sorted(found.items()):
        if arg in ('--all', p):
            print('\n'.join(ls))
sys.exit(1 if found else 0)
