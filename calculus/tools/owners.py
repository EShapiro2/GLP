#!/usr/bin/env python3
"""owners.py --- each contract of the Calculus against its owner's text in GLP, written to OWNERS.md.

    python3 tools/owners.py              writes OWNERS.md, here, for the contracts of cascade/STATUS.txt, in its order,
                                         and then those of rev/ it has not, by name
    python3 tools/owners.py --ref REF    reads GLP at REF (default: origin/main of the clone, else its HEAD)

The owner's text is GLP's programs/<dir>/NAME.txt that has a '% Owner' line (GLP docs/writing-a-contract.md).  Its
contract is its lines after the '%' lines and before its first heading of pragmas ('Pragmas', 'Interface pragmas',
'Implementation pragmas', 'Logic pragmas'): the title, the first line, the definitions, the names line and the
clauses.  Ours is rev/NAME.txt, the same lines.  Two lines are the same where they are equal once every run of
white space is one space and the line is stripped; blank lines are left out.  Needs a clone of GLP beside this
directory, or $GLP_DIR, and git; Python's standard library and nothing else.
"""
import difflib, os, re, subprocess, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GLP = os.environ.get('GLP_DIR') or next((d for d in (os.path.join(os.path.dirname(ROOT), 'GLP'), '/home/user/GLP') if os.path.isdir(d)), None)
# The contracts renamed (Udi, 2026-11-16, to Code #33 and Calculus #8: "contract names that are not acronyms capitalised
# not all caps"): their owners' files in GLP keep the old name until Code renames them at adoption.
GLP_NAME = {'Agora': 'AGORA', 'Joint': 'JOINT', 'Stay': 'STAY', 'Lib': 'LIB', 'Rec': 'REC'}
PRAGMAS = re.compile(r'^(Interface |Implementation |Logic )?[Pp]ragmas\b')


def git(*args):
    return subprocess.run(['git', '-C', GLP] + list(args), capture_output=True, text=True)


def ref_of(args):
    if '--ref' in args:
        return args[args.index('--ref') + 1]
    return 'origin/main' if git('rev-parse', '--verify', '-q', 'origin/main').returncode == 0 else 'HEAD'


def owner_files(ref):
    """{NAME: (path, owner)} for every programs/**/NAME.txt at ref with a '% Owner' line."""
    out = {}
    r = git('grep', '-l', '^% Owner', ref, '--', 'programs')
    for l in r.stdout.split('\n'):
        if not l.strip():
            continue
        path = l.split(':', 1)[1]
        name = os.path.basename(path)[:-4]
        text = git('show', '%s:%s' % (ref, path)).stdout
        m = re.search(r'^% Owner:\s*([^\s(.,;]+)', text, re.M)
        out[name] = (path, m.group(1) if m else '?', text)
    return out


def norm(lines):
    return [re.sub(r'\s+', ' ', l).strip() for l in lines if l.strip()]


def contract_lines(text, owner):
    lines = text.split('\n')
    if owner:
        lines = [l for l in lines if not l.startswith('%')]
        for i, l in enumerate(lines):
            if PRAGMAS.match(l.strip()):
                lines = lines[:i]
                break
    return norm(lines)


def main():
    args = sys.argv[1:]
    if not GLP:
        sys.exit('owners.py: no clone of GLP beside this directory, and no $GLP_DIR')
    ref = ref_of(args)
    commit = git('rev-parse', '--short', ref).stdout.strip()
    names = [l.split()[0] for l in open(os.path.join(ROOT, 'cascade', 'STATUS.txt'), encoding='utf-8') if l.strip()]
    names += sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'rev')) if f.endswith('.txt') and not f.startswith('NOTES') and f[:-4] not in names)      # a contract the cascade has not run yet (GN, GS)
    owners = owner_files(ref)
    owners.update({n: owners[o] for n, o in GLP_NAME.items() if o in owners and n not in owners})      # an owner's file under the old name
    rows, details, same, differ, none, starred = [], [], 0, 0, 0, 0
    for n in names:
        ours = contract_lines(open(os.path.join(ROOT, 'rev', n + '.txt'), encoding='utf-8').read(), False)
        if n not in owners:
            none += 1
            rows.append('| %s | none in GLP | | | |' % n)
            continue
        path, owner, text = owners[n]
        theirs = contract_lines(text, True)
        sm = difflib.SequenceMatcher(None, theirs, ours, autojunk=False)
        hunks = [op for op in sm.get_opcodes() if op[0] != 'equal']
        if not hunks:
            same += 1
            rows.append('| %s | %s | `%s` | same | same |' % (n, owner, path))
            continue
        differ += 1
        a = sum(i2 - i1 for _, i1, i2, _, _ in hunks)
        b = sum(j2 - j1 for _, _, _, j1, j2 in hunks)
        sm2 = difflib.SequenceMatcher(None, [l.replace('*', '') for l in theirs], [l.replace('*', '') for l in ours], autojunk=False)
        h2 = [op for op in sm2.get_opcodes() if op[0] != 'equal']
        c2 = sum(i2 - i1 + j2 - j1 for _, i1, i2, j1, j2 in h2)
        starred += 0 if h2 else 1
        rows.append('| %s | %s | `%s` | %d lines differ (owner %d, ours %d) | %s |' % (n, owner, path, a + b, a, b, '%d' % c2 if h2 else 'same'))
        d = ['### %s' % n, '']
        for _, i1, i2, j1, j2 in hunks:
            for l in theirs[i1:i2]:
                d.append('- owner: ' + l)
            for l in ours[j1:j2]:
                d.append('- ours: ' + l)
            d.append('')
        details.append('\n'.join(d))
    out = ['# The contracts and their owners', '',
           'Printed by `python3 tools/owners.py` from `rev/` and GLP at %s (`%s`), for the contracts of `cascade/STATUS.txt`, in its order.  '
           'The owner text is GLP\'s `programs/<dir>/NAME.txt` with a `%% Owner` line; the project is the first word of that line.  '
           'Compared: the title, the first line, the definitions, the names line and the clauses, that is the lines before the first heading of pragmas and after the `%%` lines; '
           'two lines are the same where they are equal once every run of white space is one space.  '
           'A count is of the lines in the owner text and in ours that are in no common run of lines.  '
           'The last column counts the same way with the marks of a defined term, `*term*`, taken out, which rev/ writes and the owner texts mostly do not; the lines below are in full, marks included.' % (ref, commit), '',
           '%d contracts: %d the same, %d differ, %d with no owner text in GLP; of those that differ, %d differ only in the marks `*term*`.' % (len(names), same, differ, none, starred), '',
           '| Contract | Owner | Owner text in GLP at `%s` | Calculus text against it | The same, the marks `*` taken out |' % commit,
           '|---|---|---|---|---|'] + rows + ['', '## The lines that differ, the owner first and ours second', ''] + details
    open(os.path.join(ROOT, 'OWNERS.md'), 'w', encoding='utf-8').write('\n'.join(out).rstrip('\n') + '\n')
    print('%d contracts: %d the same, %d differ (%d only in the marks *term*), %d with no owner text in GLP (GLP %s)' % (len(names), same, differ, starred, none, commit))


if __name__ == '__main__':
    main()
