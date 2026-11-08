"""Build GAS, the Grassroots App Store, at peoplesweb.org, from this repository: build/www and the live worlds.

Usage (from servers/peoplesweb):  python3 -I tools/build.py

Reads the repository only: every contract with a compiled platform,
cascade/<NAME>/platform.vglp as cascade.sh records it, gets

  programs/vglp/cva/web_worlds/live_<name>/  its live harness, four persons in one
                              heap (gen_harness.py), in the scope of the canonical
                              platforms, as vGLP's plays are
  build/www/specs/<name>.json the menu the app page draws, read off the platform (menu_spec.py)
  build/www/stack/<NAME>/     the cascade's layers, which the site loads on demand

and build/www/data.js holds every contract's text, owner, the contracts it
extends, its schemas and verdict, from cascade/<NAME>/ and the NAME.txt files
under programs/, with each contract's owner and paper (tools/papers.py).  The pages are web/*.html, copied as they are; web/about.json
holds the site's own line, icon and family for each contract.  build/ and
programs/vglp/cva/web_worlds/ are generated, gitignored, and this script rebuilds them whole.
"""
import json, os, re, shutil, subprocess, sys

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))   # servers/peoplesweb
GLP = os.path.dirname(os.path.dirname(HERE))                          # the repository
BUILD = os.path.join(HERE, 'build')
WORLDS, WWW = os.path.join(GLP, 'programs', 'vglp', 'cva', 'web_worlds'), os.path.join(BUILD, 'www')
sys.dont_write_bytecode = True
sys.path.insert(0, os.path.join(HERE, 'tools'))
from menu_spec import spec   # noqa: E402
from papers import load_state, paper_of  # noqa: E402

LAYERS = [('schemas', 'schemas.txt'), ('verdict', 'verdict.txt'), ('vmat', 'vmat.tex'),
          ('platform', 'platform.vglp'), ('glp', 'platform.glp')]


def read(p):
    with open(p, encoding='utf-8') as f:
        return f.read()


def split_top(s):
    out, depth, buf = [], 0, ''
    for ch in s:
        if ch in '([': depth += 1
        if ch in ')]': depth -= 1
        if ch == ',' and depth == 0:
            out.append(buf.strip()); buf = ''; continue
        buf += ch
    if buf.strip(): out.append(buf.strip())
    return out


def parse_schemas(path):
    """schemas.txt, as cascade.sh prints it, into heads and roles."""
    if not os.path.exists(path): return [], []
    header, out, cur = [], [], None
    for line in read(path).split('\n'):
        if not line.strip(): continue
        if line.startswith('    at '):
            m = re.match(r'    at (.+?): (.*)$', line)
            r = {'role': m.group(1), 'need': [], 'not': [], 'add': [], 'del': []}
            for it in split_top(m.group(2)):
                if it.startswith('not '): r['not'].append(it[4:])
                elif it.startswith('+'): r['add'].append(it[1:])
                elif it.startswith('-'): r['del'].append(it[1:])
                else: r['need'].append(it)
            cur['roles'].append(r)
        elif re.match(r'^[a-z_0-9]+\(', line):
            m = re.match(r'^([a-z_0-9]+)\((.*?)\)(?: \| (.*?))? :$', line)
            roles, _, vars_ = m.group(2).partition(';')
            cur = {'name': m.group(1), 'head': line[:-2], 'constraint': m.group(3) or '',
                   'parties': split_top(roles), 'vars': [x.strip() for x in vars_.split(',') if x.strip()],
                   'roles': []}
            out.append(cur)
        else:
            header.append(line.strip())
    return header, out


def contract_text(path):
    """A NAME.txt: its % lines, title, the declaration of its names, clauses, pragmas."""
    lines = read(path).split('\n')
    meta = [l for l in lines if l.startswith('%')]
    body = [l for l in lines if l.strip() and not l.startswith('%')]
    name, title = body[0].split(': ', 1)
    end = body.index('Pragmas') if 'Pragmas' in body else len(body)
    clauses = []
    for l in body[2:end]:
        m = re.match(r'(\d+)\. (\S) (.+?)\.\s+(.*)$', l)
        if m: clauses.append({'n': int(m.group(1)), 'mod': m.group(2), 'name': m.group(3), 'text': m.group(4)})
    return {'id': name, 'title': title, 'preamble': body[1], 'clauses': clauses,
            'pragmas': body[end + 1:], 'meta': meta}


# Every contract's text, wherever its owner keeps it: programs/**/NAME.txt.
texts = {}
for d, _, fs in os.walk(os.path.join(GLP, 'programs')):
    for f in fs:
        if f.endswith('.txt') and f[:-4] == f[:-4].strip() and re.fullmatch(r'[A-Z][A-Za-z]*(?:-[A-Z][A-Za-z]*)*', f[:-4]):
            p = os.path.join(d, f)
            first = next((l for l in read(p).split('\n') if l.strip() and not l.startswith('%')), '')
            if first.startswith(f[:-4] + ': '):
                texts[f[:-4]] = p

# The owners, as cascade.sh's table names them; else the first word of the % Owner line.
owners = {}
for m in re.finditer(r'^#\s+[a-z]+\s+([A-Z][A-Za-z]*)\s+(\S+)\s+programs/', read(os.path.join(GLP, 'cascade.sh')), re.M):
    owners[m.group(1)] = m.group(2)


RELATIONS = ('extends', 'restricts', 'amends', 'implements', 'instantiates')


def extends_of(c, lines):
    """The relation a contract states and the contracts it relates to: the box's own first line, 'NAME amends GSN and GFP (cite).' (style guide, 36; GSC, Section 8), wherever it stands among the text's lines, or failing it the same sentence in a % line (DGC's); failing both, the % Extends or % Restricts line, its first sentence, less the contracts named only 'through' another."""
    rel = re.compile(r'\b' + re.escape(c['id']) + r' (' + '|'.join(RELATIONS) + r') ([^.(]+)')
    for l in [l for l in lines if not l.startswith('%')] + [l for l in lines if l.startswith('%')]:
        m = rel.search(l)
        if m:
            c['relation'] = m.group(1)
            names = re.findall(r"\b([A-Z][A-Za-z]+(?:-[A-Z][A-Za-z]*)*)\b", m.group(2))
            return [t for t in dict.fromkeys(names) if t in texts and t != c['id']]
    line = next((l for l in c['meta'] if l.startswith('% Extends:') or l.startswith('% Restricts:')), '% Extends:')
    c['relation'] = 'restricts' if line.startswith('% Restricts:') else 'extends'
    first = re.split(r'\.\s{2}', line.split(':', 1)[1].strip())[0]
    named = []
    for part in first.split(';'):   # each part names some contracts, then perhaps 'through it' their own
        # a name followed by 's is an owner (GAI's), not a contract extended
        named += re.findall(r"\b([A-Z][A-Za-z]+(?:-[A-Z][A-Za-z]*)*)\b(?!['\u2019]s)", re.split(r'\bthrough\b|\bfactored\b', part)[0])
    return [t for t in dict.fromkeys(named) if t in texts and t != c['id']]


cat = {n: contract_text(p) for n, p in texts.items()}
for c in cat.values():
    c['extends'] = extends_of(c, read(texts[c['id']]).split('\n'))
    own = next((l for l in c['meta'] if l.startswith('% Owner:')), '% Owner: ')
    c['owner'] = re.sub(r'[^A-Za-z].*$', '', own[len('% Owner:'):].strip()) or owners.get(c['id'], '')
    c['source'] = 'example' if c['meta'] and re.search(r'an (example|experimental) contract', c['meta'][0]) else 'paper'

def has_platform(n):
    p = os.path.join(GLP, 'cascade', n, 'platform.vglp')
    # Four records hold the compiler's refusal, '%% not compiled(...)', where a
    # pragma is missing: no platform, no live world.
    return os.path.exists(p) and 'exported procedure agent(' in read(p)


compiled = sorted(n for n in os.listdir(os.path.join(GLP, 'cascade')) if has_platform(n) and n in cat)

shutil.rmtree(BUILD, ignore_errors=True); shutil.rmtree(WORLDS, ignore_errors=True)
os.makedirs(WORLDS); os.makedirs(os.path.join(WWW, 'specs'))
with open(os.path.join(WORLDS, '.gitignore'), 'w') as f:
    f.write('# Generated by servers/peoplesweb/tools/build.py, and rebuilt whole by it.\n*\n')

for n in compiled:
    c = n.lower()
    subprocess.run([sys.executable, '-I', os.path.join(HERE, 'tools', 'gen_harness.py'),
                    os.path.join(GLP, 'cascade', n, 'platform.vglp'), os.path.join(WORLDS, f'live_{c}'), n],
                   check=True, stdout=subprocess.DEVNULL)
    s = spec(os.path.join(WORLDS, f'live_{c}', 'platform.vglp'))
    s['id'], s['title'] = n, cat[n]['title']
    s['calendar'] = 'cal(alice, N)' in read(os.path.join(WORLDS, f'live_{c}', 'self.glp'))
    s.pop('raw_items', None)
    with open(os.path.join(WWW, 'specs', f'{c}.json'), 'w', encoding='utf-8') as f:
        json.dump(s, f)


about = json.loads(read(os.path.join(HERE, 'web', 'about.json')))
# The papers' PDFs: those tools/papers_sync.py keeps current in PAPERS_DIR on
# the server, with its state.json, or else the ones built by hand in
# web/papers.
pd = os.environ.get('PAPERS_DIR') or os.path.join(HERE, 'web', 'papers')
if not os.path.isdir(pd):
    pd = os.path.join(HERE, 'web', 'papers')
served = {f[:-4] for f in os.listdir(pd) if f.endswith('.pdf')} if os.path.isdir(pd) else set()
paper_state = load_state(pd)
byid = dict(cat)


def ancestors(cid):
    out = []
    for p in byid[cid]['extends']: out += [p] + ancestors(p)
    return out


shown = set(cat)


def nearest(cid):
    out = []
    for p in byid[cid]['extends']:
        out += [p] if p in shown else nearest(p)
    return list(dict.fromkeys(out))


contracts = []
for n in sorted(set(compiled) | set(cat)):
    c = dict(cat[n])
    d = os.path.join(GLP, 'cascade', n)
    _a = about.get(n, ['📄', 'other', ''])
    c['icon'], c['family'], c['about'] = _a[0], _a[1], _a[2] or c['title'] + '.'
    # The shown name starts with the letters, and a contract built on another
    # names the base by its letters, never by its full title: "GDO-L: GDO with
    # Labels" (Udi, 2026-11-07).
    t = c['title']
    for p in c['extends']:
        if p in cat:
            t = re.sub(re.escape(cat[p]['title']), p, t, flags=re.I)
    c['label'] = f"{n}: {t}" 
    c['cascade'] = n in compiled
    c['paper'] = paper_of(next((l for l in c['meta'] if l.startswith('% Owner:')), ''), served, paper_state)
    c['schemaHeader'], c['schemas'] = parse_schemas(os.path.join(d, 'schemas.txt'))
    c['verdict'] = read(os.path.join(d, 'verdict.txt')).strip() if os.path.exists(os.path.join(d, 'verdict.txt')) else ''
    c['live'], c['noteBefore'], c['layers'] = n in compiled, '', {}
    for k, f in LAYERS:
        if os.path.exists(os.path.join(d, f)):
            os.makedirs(os.path.join(WWW, 'stack', n), exist_ok=True)
            shutil.copy(os.path.join(d, f), os.path.join(WWW, 'stack', n, f))
            c['layers'][k] = {'file': f'stack/{n}/{f}', 'lines': read(os.path.join(d, f)).count('\n')}
    # A contract that builds on one with no platform is drawn building on the nearest shown ones.
    ps = nearest(n)
    c['extends'] = [p for p in ps if not any(p in set(ancestors(q)) for q in ps if q != p)]
    c.pop('meta')
    contracts.append(c)

for c in contracts:
    for s in c['schemas']:
        s['from'] = c['id']
        for a in ancestors(c['id']):
            if a in shown and any(x['name'] == s['name'] for x in next(k for k in contracts if k['id'] == a)['schemas']):
                s['from'] = a

commit = subprocess.run(['git', '-C', GLP, 'log', '-1', '--format=%H %cs'], capture_output=True, text=True).stdout.split()
for f in os.listdir(os.path.join(HERE, 'web')):
    if f.endswith(('.html', '.png')):
        shutil.copy(os.path.join(HERE, 'web', f), os.path.join(WWW, f))
if served:
    os.makedirs(os.path.join(WWW, 'papers'), exist_ok=True)
    for f in os.listdir(pd):
        if f.endswith('.pdf'):
            shutil.copy(os.path.join(pd, f), os.path.join(WWW, 'papers', f))
with open(os.path.join(WWW, 'data.js'), 'w', encoding='utf-8') as f:
    f.write('window.DATA = ' + json.dumps({'contracts': contracts, 'glpCommit': commit}, ensure_ascii=False) + ';\n')
print(len(contracts), 'contracts:', ' '.join(c['id'] for c in contracts))
