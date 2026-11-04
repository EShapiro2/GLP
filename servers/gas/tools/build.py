"""Build GAS, the GLP app store, from this repository: build/worlds and build/www.

Usage (from servers/gas):  python3 -I tools/build.py

Reads the repository only: every contract with a compiled platform,
cascade/<NAME>/platform.vglp as cascade.sh records it, gets

  programs/vglp/cva/gas_worlds/live_<name>/  its live harness, four persons in one
                              heap (gen_harness.py), in the scope of the canonical
                              platforms, as vGLP's plays are
  build/www/specs/<name>.json the menu the app page draws, read off the platform (menu_spec.py)
  build/www/stack/<NAME>/     the cascade's layers, which the site loads on demand

and build/www/data.js holds every contract's text, owner, the contracts it
extends, its schemas and verdict, from cascade/<NAME>/ and the NAME.txt files
under programs/.  The pages are web/*.html, copied as they are; web/about.json
holds the site's own line, icon and family for each contract.  build/ and
programs/vglp/cva/gas_worlds/ are generated, gitignored, and this script rebuilds them whole.
"""
import json, os, re, shutil, subprocess, sys

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))   # servers/gas
GLP = os.path.dirname(os.path.dirname(HERE))                          # the repository
BUILD = os.path.join(HERE, 'build')
WORLDS, WWW = os.path.join(GLP, 'programs', 'vglp', 'cva', 'gas_worlds'), os.path.join(BUILD, 'www')
sys.dont_write_bytecode = True
sys.path.insert(0, os.path.join(HERE, 'tools'))
from menu_spec import spec   # noqa: E402

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
        if f.endswith('.txt') and f[:-4] == f[:-4].strip() and re.fullmatch(r'[A-Z][A-Za-z]*', f[:-4]):
            p = os.path.join(d, f)
            first = next((l for l in read(p).split('\n') if l.strip() and not l.startswith('%')), '')
            if first.startswith(f[:-4] + ': '):
                texts[f[:-4]] = p

# The owners, as cascade.sh's table names them; else the first word of the % Owner line.
owners = {}
for m in re.finditer(r'^#\s+[a-z]+\s+([A-Z][A-Za-z]*)\s+(\S+)\s+programs/', read(os.path.join(GLP, 'cascade.sh')), re.M):
    owners[m.group(1)] = m.group(2)


def extends_of(c):
    """The contracts a contract extends directly: those named in the first sentence of its
    % Extends line, less those it extends only 'through' another."""
    line = next((l for l in c['meta'] if l.startswith('% Extends:')), '')
    first = re.split(r'\.\s{2}', line[len('% Extends:'):].strip())[0]
    named = []
    for part in first.split(';'):   # each part names some contracts, then perhaps 'through it' their own
        # a name followed by 's is an owner (GAI's), not a contract extended
        named += re.findall(r"\b([A-Z][A-Za-z]+)\b(?!['\u2019]s)", re.split(r'\bthrough\b', part)[0])
    return [t for t in dict.fromkeys(named) if t in texts and t != c['id']]


cat = {n: contract_text(p) for n, p in texts.items()}
for c in cat.values():
    c['extends'] = extends_of(c)
    own = next((l for l in c['meta'] if l.startswith('% Owner:')), '% Owner: ')
    c['owner'] = owners.get(c['id']) or re.sub(r'[^A-Za-z].*$', '', own[len('% Owner:'):].strip())
    c['source'] = 'example' if c['meta'] and re.search(r'an (example|experimental) contract', c['meta'][0]) else 'paper'

compiled = sorted(n for n in os.listdir(os.path.join(GLP, 'cascade'))
                  if os.path.exists(os.path.join(GLP, 'cascade', n, 'platform.vglp')) and n in cat)

shutil.rmtree(BUILD, ignore_errors=True); shutil.rmtree(WORLDS, ignore_errors=True)
os.makedirs(WORLDS); os.makedirs(os.path.join(WWW, 'specs'))
with open(os.path.join(WORLDS, '.gitignore'), 'w') as f:
    f.write('# Generated by servers/gas/tools/build.py, and rebuilt whole by it.\n*\n')

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
byid = dict(cat)


def ancestors(cid):
    out = []
    for p in byid[cid]['extends']: out += [p] + ancestors(p)
    return out


shown = set(compiled)


def nearest(cid):
    out = []
    for p in byid[cid]['extends']:
        out += [p] if p in shown else nearest(p)
    return list(dict.fromkeys(out))


contracts = []
for n in compiled:
    c = dict(cat[n])
    d = os.path.join(GLP, 'cascade', n)
    c['icon'], c['family'], c['about'] = about.get(n, ['📄', 'other', c['title'] + '.'])
    c['cascade'] = True
    c['schemaHeader'], c['schemas'] = parse_schemas(os.path.join(d, 'schemas.txt'))
    c['verdict'] = read(os.path.join(d, 'verdict.txt')).strip() if os.path.exists(os.path.join(d, 'verdict.txt')) else ''
    c['live'], c['noteBefore'], c['layers'] = True, '', {}
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
with open(os.path.join(WWW, 'data.js'), 'w', encoding='utf-8') as f:
    f.write('window.DATA = ' + json.dumps({'contracts': contracts, 'glpCommit': commit}, ensure_ascii=False) + ';\n')
print(len(contracts), 'contracts:', ' '.join(c['id'] for c in contracts))
