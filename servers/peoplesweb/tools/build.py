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
under programs/, with each contract's owner and paper (tools/papers.py), and
the families' names and colours.  The pages are web/*.html, copied as they
are.  A contract's icon and family are the line its interface pragmas open
with, 'Look: <icon>, <family>' (GSC's Section 2), and web/about.json's until
its owner writes that line; web/about.json holds the site's own line for each
contract; and programs/apps/look.json each family's name and colours, which
the app's asset sync reads too.  build/ and
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
from papers import FOUNDATIONAL, load_state, paper_of  # noqa: E402
import certify  # noqa: E402

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


def not_compiled(text):
    """The compiler's reasons for printing no platform, as cascade.sh writes
    them into platform.vglp: each reason once, with the speech acts it names
    on the '%% the schema...(name...)' line after it."""
    reasons, last = {}, None
    for l in text.split('\n'):
        if not l.startswith('%% ') or l.startswith('%% not compiled'):
            continue
        l = l[3:].strip()
        m = re.match(r'^the schema[^(]*\(([a-z_0-9]+)', l)
        if m and last is not None:
            if m.group(1) not in reasons[last]:
                reasons[last].append(m.group(1))
            continue
        m = re.match(r'^(.*?):\s*\(([a-z_0-9]+)\)\s*$', l)
        last = (m.group(1) if m else l).rstrip(':').strip()
        reasons.setdefault(last, [])
        if m and m.group(2) not in reasons[last]:
            reasons[last].append(m.group(2))
    return [r + (f" ({', '.join(ns)})" if ns else '') for r, ns in reasons.items()]


def parse_rules(text):
    """schemas.txt in the form cascade.sh prints since 2026-11: each speech act
    'name: heads <-' and, indented, its body, ending 'guarded by Alice, Bob'
    where it is guarded.  A head 'not A' removes A; every other head adds."""
    header, out, cur = [], [], None
    for line in text.split('\n'):
        if not line.strip():
            cur = None
            continue
        m = re.match(r'^([a-z_0-9]+): (.*?)\s*<-\s*$', line)
        if m:
            heads = split_top(m.group(2))
            cur = {'name': m.group(1), 'head': f"{m.group(1)}: {m.group(2)}", 'form': 'rule',
                   'adds': [h for h in heads if not h.startswith('not ')],
                   'removes': [h[4:] for h in heads if h.startswith('not ')],
                   'body': '', 'needs': [], 'guarded': []}
            out.append(cur)
        elif cur is not None and line.startswith(' '):
            cur['body'] += ' ' + line.strip()
            body, _, g = cur['body'].strip().rpartition(' guarded by ')
            if not body:
                body, g = cur['body'].strip(), ''
            cur['needs'] = split_top(body)
            cur['guarded'] = [x.strip() for x in g.split(',') if x.strip()]
        else:
            header.append(line.strip())
    return header, out


def parse_schemas(path):
    """schemas.txt, as cascade.sh prints it, into heads and roles: the form
    by roles of the records before 2026-11, or the form by rules since."""
    if not os.path.exists(path): return [], []
    if re.search(r'^[a-z_0-9]+: .*<-\s*$', read(path), re.M):
        return parse_rules(read(path))
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
            cur = {'name': m.group(1), 'head': line[:-2], 'constraint': m.group(3) or '', 'form': 'roles',
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
    # The pragmas follow the clauses under 'Pragmas', or under 'Interface
    # pragmas' and 'Implementation pragmas', their two kinds (GSC's Section 2;
    # Coordination, to_all 2026-11-07 22:13 UTC), the headings kept among them.
    heads = ('Pragmas', 'Interface pragmas', 'Implementation pragmas')
    end = next((i for i, l in enumerate(body) if l.strip() in heads), len(body))
    clauses = []
    for l in body[2:end]:
        m = re.match(r'(\d+)\. (\S) (.+?)\.\s+(.*)$', l)
        if m: clauses.append({'n': int(m.group(1)), 'mod': m.group(2), 'name': m.group(3), 'text': m.group(4)})
    return {'id': name, 'title': title, 'preamble': body[1], 'clauses': clauses,
            'pragmas': [l for l in body[end:] if l.strip() != 'Pragmas'], 'meta': meta}


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
    """The relation a contract states and the contracts it relates to: the box's own first line, 'NAME amends GSN and GFP (cite).' (style guide, 36; GSC, Relations among Contracts, sec:relations), wherever it stands among the text's lines, or failing it the same sentence in a % line (DGC's); failing both, the % Extends or % Restricts line, its first sentence, less the contracts named only 'through' another."""
    rel = re.compile(r'\b' + re.escape(c['id']) + r' (' + '|'.join(RELATIONS) + r') ([^.(]+)')
    for l in [l for l in lines if not l.startswith('%')] + [l for l in lines if l.startswith('%')]:
        m = rel.search(l)
        if m:
            # Each contract named takes the relation written before it: 'GAI
            # extends GnW and amends GFP' is two relations, one to each.
            c['relation'], c['rels'], cur = m.group(1), {}, m.group(1)
            for part in re.split(r'\b(' + '|'.join(RELATIONS) + r')\b', m.group(2)):
                if part in RELATIONS:
                    cur = part
                    continue
                for t in re.findall(r"\b([A-Z][A-Za-z]+(?:-[A-Z][A-Za-z]*)*)\b", part):
                    if t in texts and t != c['id']:
                        c['rels'].setdefault(t, cur)
            return list(c['rels'])
    line = next((l for l in c['meta'] if l.startswith('% Extends:') or l.startswith('% Restricts:')), '% Extends:')
    c['relation'] = 'restricts' if line.startswith('% Restricts:') else 'extends'
    first = re.split(r'\.\s{2}', line.split(':', 1)[1].strip())[0]
    named = []
    for part in first.split(';'):   # each part names some contracts, then perhaps 'through it' their own
        # a name followed by 's is an owner (GAI's), not a contract extended
        named += re.findall(r"\b([A-Z][A-Za-z]+(?:-[A-Z][A-Za-z]*)*)\b(?!['\u2019]s)", re.split(r'\bthrough\b|\bfactored\b', part)[0])
    found = [t for t in dict.fromkeys(named) if t in texts and t != c['id']]
    c['rels'] = {t: c['relation'] for t in found}
    return found


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
look = json.loads(read(os.path.join(GLP, 'programs', 'apps', 'look.json')))
# The papers' PDFs: those tools/papers_sync.py keeps current in PAPERS_DIR on
# the server, with its state.json, or else the ones built by hand in
# web/papers.
# web/papers, each served from where it is newest: PAPERS_DIR where the sync
# has built it, web/papers otherwise, so a paper not yet built there keeps
# its link.
hand = os.path.join(HERE, 'web', 'papers')
synced = os.environ.get('PAPERS_DIR') or ''
pdfs = {}
for d in (hand, synced):
    if d and os.path.isdir(d):
        for f in os.listdir(d):
            if f.endswith('.pdf'):
                pdfs[f] = os.path.join(d, f)
served = {f[:-4] for f in pdfs}
paper_state = load_state(synced) if synced else {}
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


def look_line(pragmas):
    """The look a contract's interface pragmas open with, 'Look: <icon>,
    <family>' (GSC's Section 2, 34bc713), as (icon, family); None where the
    first line is not it."""
    for l in pragmas:
        if l.strip() in ('Interface pragmas', 'Implementation pragmas'):
            continue
        m = re.match(r'Look:\s*(.+?),\s*([\w-]+)\s*$', l.strip())
        return (m.group(1), m.group(2)) if m else None
    return None


contracts = []
for n in sorted(set(compiled) | set(cat)):
    c = dict(cat[n])
    d = os.path.join(GLP, 'cascade', n)
    # The icon and family the contract's Look line names, where its owner
    # wrote one, and else about.json's, which holds them until then.
    _a = about.get(n, ['📄', 'other', ''])
    _icon, _family = look_line(c.get('pragmas', [])) or (_a[0], _a[1])
    c['icon'], c['family'], c['about'] = _icon, _family, _a[2] or c['title'] + '.'
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
    # Why a contract with a record has no app: the compiler's reasons, as
    # cascade.sh writes them into platform.vglp in place of a platform; a
    # contract with no record has not been through the cascade.
    c['record'] = os.path.isdir(d)
    pv = os.path.join(d, 'platform.vglp')
    c['notCompiled'] = [] if c['live'] or not os.path.exists(pv) else not_compiled(read(pv))
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

# The relations each contract states, checked against the schemas (GSC,
# Relations among Contracts, sec:relations), and each app of programs/apps as a certified artefact, both
# through the REPL and cached (tools/certify.py); CERTIFY=0 skips both, for a
# quick build of the pages alone.
if os.environ.get('CERTIFY', '1') != '0':
    checks = certify.relations([{'id': c['id'], 'path': texts[c['id']], 'rels': c.get('rels', {})} for c in contracts])
    arts = certify.artefacts(os.path.join(WWW, 'artefacts'))
else:
    checks, arts = {}, {}
for c in contracts:
    c['checks'] = checks.get(c['id'], [])
    a = arts.get(c['id'].lower().replace('-', '_'))
    c['artefact'] = ({'file': 'artefacts/' + a['file'], 'key': a['key']} if a and 'file' in a else {'why': a['why']} if a else None)
with open(os.path.join(WWW, 'relations.json'), 'w', encoding='utf-8') as f:
    json.dump({c['id']: c['checks'] for c in contracts if c['checks']}, f, ensure_ascii=False, indent=1)

commit = subprocess.run(['git', '-C', GLP, 'log', '-1', '--format=%H %cs'], capture_output=True, text=True).stdout.split()
for f in os.listdir(os.path.join(HERE, 'web')):
    if f.endswith(('.html', '.png', '.ico', '.jpg')):
        shutil.copy(os.path.join(HERE, 'web', f), os.path.join(WWW, f))
if served:
    os.makedirs(os.path.join(WWW, 'papers'), exist_ok=True)
    for f, path in pdfs.items():
        shutil.copy(path, os.path.join(WWW, 'papers', f))
with open(os.path.join(WWW, 'data.js'), 'w', encoding='utf-8') as f:
    f.write('window.DATA = ' + json.dumps({'contracts': contracts, 'families': look['families'], 'glpCommit': commit, 'foundational': FOUNDATIONAL}, ensure_ascii=False) + ';\n')
print(len(contracts), 'contracts:', ' '.join(c['id'] for c in contracts))
