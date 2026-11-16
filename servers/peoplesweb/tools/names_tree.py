"""A stand-in for the tree the contracts will have under their new names: python3 tools/names_tree.py <GLP's root> <out>.

calculus/NAMES.txt has the names of the 66 contracts for the publication after that of 2026-11-16 (Udi approved them, 2026-11-16), each with its name on main, its name and its file name; the names are not yet made on main.  This copies programs/, cascade/ and calculus/ and gives every contract its new name: its files and its record's directory by the file name, its title line the name, and every old name in its text, its full contract, its schemas and its relations the new one, the longest matched first.  The site built over it, at peoplesweb.org/next (deploy/next.sh), shows the names as they will be, for Udi to confirm the globe's layout before they are live (Udi, 2026-11-16, to Website #2).  It is a preview and no adoption: the platforms inside keep the old names, and nothing here is run."""
import os, re, shutil, subprocess, sys
src, out = sys.argv[1], sys.argv[2]
rows = [[x.strip() for x in l.split('|')] for l in open(os.path.join(src, 'calculus', 'NAMES.txt')) if l.count('|') == 3 and not l.startswith('name on main')]
old2 = {r[0]: r for r in rows}
pat = re.compile(r"(?<![A-Za-z-])(" + '|'.join(re.escape(o) for o in sorted(old2, key=len, reverse=True)) + r")(?![A-Za-z-])")
shutil.rmtree(out, ignore_errors=True)
for d in ('programs', 'cascade', 'calculus'):
    shutil.copytree(os.path.join(src, d), os.path.join(out, d), symlinks=True, ignore=shutil.ignore_patterns('web_worlds', '*.glpw'))
shutil.copy(os.path.join(src, 'cascade.sh'), out)
def fix(path, title=False):
    try: t = open(path, encoding='utf-8').read()
    except Exception: return
    lines = t.split('\n')
    for i, l in enumerate(lines):
        if l.startswith('%'): continue
        if title and l.strip() and not l.startswith('%'):
            head = l.split(': ', 1)[0].strip()
            if head in old2: lines[i] = old2[head][1]; title = False; continue
        lines[i] = pat.sub(lambda m: old2[m.group(1)][1], l)
    open(path, 'w', encoding='utf-8').write('\n'.join(lines))
for dp, dn, fn in os.walk(os.path.join(out, 'programs')):
    for f in fn:
        if f.endswith('.txt') and f[:-4] in old2:
            fix(os.path.join(dp, f), title=True)
            os.rename(os.path.join(dp, f), os.path.join(dp, old2[f[:-4]][2] + '.txt'))
for o, r in old2.items():
    d = os.path.join(out, 'cascade', o)
    if os.path.isdir(d):
        for f in ('full-contract.txt', 'full-schemas.txt', 'schemas.txt', 'relations.txt'):
            if os.path.exists(os.path.join(d, f)): fix(os.path.join(d, f), title=(f == 'full-contract.txt'))
        if r[2] != o: os.rename(d, os.path.join(out, 'cascade', r[2]))
for a in (['init', '-q'], ['add', '-A'], ['-c', 'user.name=x', '-c', 'user.email=x@x', 'commit', '-qm', 'renamed']):
    subprocess.run(['git', '-C', out, *a], check=True, stdout=subprocess.DEVNULL)
