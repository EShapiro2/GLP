"""A stand-in tree for the site's build, from a publication of the Calculus that main does not have yet: python3 tools/next_tree.py <calculus/next> <GLP's root> <out>.

The Calculus pushes the contracts of its next publication, before any owner adopts them, as calculus/next/NAME/ on a branch of GLP (Calculus #8, 2026-11-16: calculus-8/test-site), with OWNERS.md and relations.txt.  This writes a tree in which each is as if adopted, programs/next/NAME.txt and cascade/NAME/, with what the build reads beside them (programs/apps, cascade.sh), so that the site of those contracts can be built and seen, at peoplesweb.org/next (deploy/next.sh), before they reach main (Udi, 2026-11-16, to Code #33: 'i want website to prepare test site based on contracts being tested by calculus').  It shows no platform and no running app, the stand-in tree having none, and every contract in the directory programs/next."""
import os, re, shutil, subprocess, sys
src, glp, out = sys.argv[1], sys.argv[2], sys.argv[3]
shutil.rmtree(out, ignore_errors=True)
os.makedirs(os.path.join(out, 'programs', 'next')); os.makedirs(os.path.join(out, 'cascade'))
shutil.copytree(os.path.join(glp, 'programs', 'apps'), os.path.join(out, 'programs', 'apps'))
shutil.copy(os.path.join(glp, 'cascade.sh'), out)
owners = {}
for l in open(os.path.join(src, 'OWNERS.md')):
    m = re.match(r'\| ([A-Za-z-]+) \| ([A-Za-z-]+)', l)
    if m: owners[m.group(1)] = m.group(2)
rel = open(os.path.join(src, 'relations.txt')).read().split('\n')
for n in sorted(os.listdir(src)):
    d = os.path.join(src, n)
    if not os.path.isfile(os.path.join(d, 'contract.txt')): continue
    with open(os.path.join(out, 'programs', 'next', n + '.txt'), 'w') as f:
        f.write(f'% Owner: {owners.get(n, "")}.\n' + open(os.path.join(d, 'contract.txt')).read())
    c = os.path.join(out, 'cascade', n); os.makedirs(c)
    for x in ('full-contract.txt', 'full-schemas.txt', 'schemas.txt'):
        if os.path.exists(os.path.join(d, x)): shutil.copy(os.path.join(d, x), c)
    open(os.path.join(c, 'relations.txt'), 'w').write('\n'.join(l for l in rel if l.startswith(n + ' ')) + '\n')
for a in (['init', '-q'], ['add', '-A'], ['-c', 'user.name=x', '-c', 'user.email=x@x', 'commit', '-qm', 'next']):
    subprocess.run(['git', '-C', out, *a], check=True)
