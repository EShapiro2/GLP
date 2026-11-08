"""Build GAS2 from a clone of the GLP repository: every grassroots contract in it, how
the contracts relate, and each contract taken down the cascade to a certified mini-app
of the Grassroots Super-App, with a run of it on two (or three) super-apps.

Usage:  python3 -I tools/build.py <GLP clone> <output directory>

The clone is a working copy GAS2 may write into: the mini-apps it makes go under
programs/apps/gas2_<id>/, their phones under programs/social/graph/core/, and
core/self.glp gets three lines for each phone (miniapp.py).  The output directory gets
data.json, which the site reads, artefacts/<app>.glpw, and platforms/<id>.vglp."""
import json
import os
import re
import shutil
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from glp import Glp  # noqa: E402
import miniapp as M  # noqa: E402

KINDS = ('extends', 'amends', 'restricts', 'instantiates', 'implements')


def read(p):
    with open(p, encoding='utf-8') as f:
        return f.read()


# --- the contracts' texts ----------------------------------------------------------

def contract_text(path):
    """A NAME.txt: its % lines, title, preamble, clauses, pragmas."""
    lines = read(path).split('\n')
    meta = [l for l in lines if l.startswith('%')]
    body = [l for l in lines if l.strip() and not l.startswith('%')]
    name, title = body[0].split(': ', 1)
    end = body.index('Pragmas') if 'Pragmas' in body else len(body)
    clauses = []
    for l in body[2:end]:
        m = re.match(r'(\d+)\. (\S) (.+?)\.\s+(.*)$', l)
        if m:
            clauses.append({'n': int(m.group(1)), 'mod': m.group(2), 'name': m.group(3), 'text': m.group(4)})
    return {'id': name, 'title': title.strip(), 'preamble': body[1] if len(body) > 1 else '',
            'clauses': clauses, 'meta': meta, 'path': path, 'pragmas': body[end + 1:]}


def discover(root):
    """Every contract's text, wherever its owner keeps it: programs/**/NAME.txt whose first
    line is 'NAME: title'."""
    texts = {}
    for d, dirs, fs in os.walk(os.path.join(root, 'programs')):
        dirs[:] = [x for x in dirs if not x.startswith('gas2_')]
        for f in sorted(fs):
            n = f[:-4]
            if f.endswith('.txt') and re.fullmatch(r'[A-Z][A-Za-z0-9]*(-[A-Z0-9]+)*', n):
                p = os.path.join(d, f)
                first = next((l for l in read(p).split('\n') if l.strip() and not l.startswith('%')), '')
                if first.startswith(n + ': ') and n not in texts:
                    texts[n] = p
    return texts


def meta_line(c, key):
    l = next((l for l in c['meta'] if l.startswith(f'% {key}:')), '')
    return l[len(key) + 3:].strip()


def built_on(c, ids):
    """The contracts the '% Extends:' line names directly: those of its first sentence,
    less those it extends only 'through' another, and less owners (GAI's)."""
    first = re.split(r'\.\s{2}', meta_line(c, 'Extends'))[0]
    named = []
    for part in first.split(';'):
        named += re.findall(r"\b([A-Z][A-Za-z0-9]*(?:-[A-Z0-9]+)*)\b(?!['’]s)", re.split(r'\bthrough\b', part)[0])
    return [t for t in dict.fromkeys(named) if t in ids and t != c['id']]


def stated(c, ids):
    """The relations the contract's own text states of itself: 'X restricts Y' and the like."""
    out = {}
    txt = read(c['path'])
    for m in re.finditer(r'\b([A-Z][A-Za-z0-9]*(?:-[A-Z0-9]+)*) (extends|amends|restricts|instantiates|implements) '
                         r'([A-Z][A-Za-z0-9]*(?:-[A-Z0-9]+)*)\b', txt):
        if m.group(1) == c['id'] and m.group(3) in ids and m.group(3) != c['id']:
            out.setdefault(m.group(3), m.group(2))
    return out


def owner_file(c):
    """The owner's act schemas beside the text, NAME.glp, and the name its contract_named/2 gives."""
    p = c['path'][:-4] + '.glp'
    if not os.path.exists(p):
        return None, None
    m = re.search(r'^contract_named\(([a-z_0-9]+),', read(p), re.M)
    return (p, m.group(1)) if m else (p, None)


def schemas_of(term):
    """The schemas of a contract term as the REPL prints it, name -> text: the clause syntax's
    sas/7 or the checker's schema/3 and lschema/4."""
    out = {}
    i = 0
    while True:
        m = re.compile(r'(?<![A-Za-z0-9_])(sas|schema|lschema)\(').search(term, i)
        if not m:
            break
        i = m.start()
        d, j = 0, m.end() - 1
        while j < len(term):
            if term[j] in '([':
                d += 1
            elif term[j] in ')]':
                d -= 1
                if d == 0:
                    break
            j += 1
        s = term[i:j + 1]
        name = re.match(r'[a-z]+\(([a-z_0-9]+)', s)
        out[name.group(1) if name else s[:20]] = normal(s)
        i = j + 1
    return out


# --- the cascade -------------------------------------------------------------------

def normal(schema):
    """A schema with its variables and its roles renamed in order of first occurrence, so
    that two contracts that write one schema with other names compare equal."""
    names = {}

    def ren(m):
        k = m.group(2).strip("'")
        names.setdefault((m.group(1), k), len(names))
        return f"{m.group(1)}(v{names[(m.group(1), k)]})"
    s = re.sub(r"\b(pvar|svar|own_var|nvar|var)\(('[^']*'|[A-Za-z_0-9]+)\)", ren, schema)
    # the roles' names, the schema's second argument, by their number
    m = re.match(r"^([a-z]+\([a-z_0-9]+, )\[([^\]]*)\]", s)
    if m:
        n = len([x for x in m.group(2).split(',') if x.strip()])
        s = m.group(1) + '[' + ', '.join(f'r{i}' for i in range(n)) + ']' + s[m.end():]
    return s


def cascade(g, c):
    """The steps programs/apps/compile.sh takes, for one contract; what each gave."""
    r = {'term': None, 'verdict': None, 'platform': None, 'why': None, 'step': None}
    f, name = owner_file(c)
    if not f:
        r['why'], r['step'] = 'no act schemas beside the contract (no NAME.glp)', 'schemas'
        return r
    if not name:
        r['why'], r['step'] = f'{os.path.basename(f)} names no contract (no contract_named/2)', 'schemas'
        return r
    r['name'] = name
    t0 = time.time()
    C = g.contract(f, name)
    if not C:
        tail = [l for l in getattr(g, 'last', []) if l.strip()][-6:]
        r['why'], r['step'] = 'the contract term was not read: ' + ' | '.join(tail)[-600:], 'schemas'
        return r
    r['term'] = C
    R = g.as_read(C)
    r['read'] = R
    V = g.verdict(C)
    r['verdict'] = V
    if not R or not V:
        r['why'], r['step'] = 'the checker gave no verdict', 'check'
        return r
    if V != 'syntactically_grassroots':
        r['why'], r['step'] = f'the checker: {V}', 'check'
        return r
    I = g.introduction(f, name)
    P, why = g.pragmas(c['path'], R)
    if not P:
        r['why'], r['step'] = f'the pragmas: {why}', 'pragmas'
        return r
    plat, why = g.compile_platform(name, R, V, I, P)
    if not plat:
        r['why'], r['step'] = f'the compiler: {why}', 'compile'
        return r
    r['platform'], r['step'], r['seconds'] = plat, 'compiled', round(time.time() - t0, 1)
    return r


VIEW = re.compile(r'^\[([a-z]+)\]\s+(.*)$')


def parse_run(lines):
    """A boot's output: each person's lines, in order, and whether the boot settled."""
    order, settled = [], False
    for l in lines:
        m = VIEW.match(l.strip())
        if m:
            order.append([m.group(1), m.group(2)])
        if 'Boot settled' in l:
            settled = True
    errs = [l for l in lines if re.search(r'\bError\b|not well-typed|\[timeout\]', l)]
    return {'lines': order, 'settled': settled, 'errors': errs[:5]}


def main(root, out):
    g = Glp(root)
    os.makedirs(out, exist_ok=True)
    art_dir = os.path.join(out, 'artefacts')
    shutil.rmtree(art_dir, ignore_errors=True)
    os.makedirs(art_dir)
    os.makedirs(os.path.join(out, 'platforms'), exist_ok=True)
    commit = subprocess.run(['git', '-C', root, 'log', '-1', '--format=%H%n%cI%n%s'],
                            capture_output=True, text=True).stdout.split('\n')
    texts = discover(root)
    ids = set(texts)
    cs = {n: contract_text(p) for n, p in texts.items()}

    # the cascade, two contracts at a time
    t0 = time.time()
    with ThreadPoolExecutor(int(os.environ.get('GAS2_WORKERS', '2'))) as ex:
        res = dict(zip(cs, ex.map(lambda n: cascade(g, cs[n]), cs)))
    print(f'cascade: {len(cs)} contracts in {time.time() - t0:.0f}s', flush=True)

    core = g.path('programs', 'social', 'graph', 'core')
    contracts = []
    gas2_apps = []
    for n in sorted(cs):
        c, r = cs[n], res[n]
        # the schemas as GSC's reader reads them, one form for every contract, to compare
        sch = schemas_of(r.get('read') or r['term'] or '')
        e = {'id': n, 'title': c['title'], 'preamble': c['preamble'], 'clauses': c['clauses'],
             'owner': re.sub(r'[ ,(.].*$', '', meta_line(c, 'Owner')),
             'paper': meta_line(c, 'Paper'),
             'file': os.path.relpath(c['path'], root).replace('\\', '/'),
             'extendsLine': meta_line(c, 'Extends'),
             'schemas': list(sch), 'verdict': r['verdict'], 'step': r['step'], 'why': r['why'],
             'compileSeconds': r.get('seconds')}
        c['sch'] = sch
        # its app: the hand-made one in programs/apps, if any, and GAS2's own where the cascade compiled
        low = n.lower().replace('-', '_')
        udi = g.path('programs', 'apps', low)
        e['udiApp'] = low if os.path.exists(os.path.join(udi, 'platform.vglp')) else None
        e['udiBoot'] = f'{low}_boot.glp' if os.path.exists(g.path('programs', 'apps', f'{low}_boot.glp')) else None
        e['app'] = None
        if r['platform']:
            with open(os.path.join(out, 'platforms', f'{n}.vglp'), 'w', encoding='utf-8', newline='\n') as f:
                f.write(r['platform'])
            app = f'gas2_{low}'
            d = M.write_app(g, app, n, c['title'], r['platform'], r.get('read'))
            e['app'] = app
            e['acts'] = M.proposals(r['platform'])
            gas2_apps.append((n, app, d, e))
        contracts.append(e)

    # relations: what each contract's text names it built on, of which kind
    edges = []
    for n in sorted(cs):
        c = cs[n]
        st = stated(c, ids)
        for p in dict.fromkeys(built_on(c, ids) + list(st)):
            if p in st:
                kind, how = st[p], 'stated'
            elif c['sch'] and cs[p]['sch']:
                kept = all(c['sch'].get(k) == v for k, v in cs[p]['sch'].items())
                kind, how = ('extends' if kept else 'amends'), 'computed'
                if not kept:
                    gone = [k for k in cs[p]['sch'] if k not in c['sch']]
                    changed = [k for k in cs[p]['sch'] if k in c['sch'] and c['sch'][k] != cs[p]['sch'][k]]
                    how = 'computed: ' + '; '.join(x for x in [
                        ('deletes ' + ', '.join(gone)) if gone else '',
                        ('replaces ' + ', '.join(changed)) if changed else ''] if x)
            else:
                kind, how = 'extends', 'as its Extends line says (no schemas to compare)'
            edges.append({'from': n, 'to': p, 'kind': kind, 'how': how})

    # the artefacts: GAS2's mini-apps, and the hand-made ones whose boots GAS2 runs
    for n, app, d, e in gas2_apps:
        a, why = g.artefact(d, core)
        if a:
            shutil.copy(a, art_dir)
            e['artefact'] = f'artefacts/{app}.glpw'
            M.write_phone(g, app, n, e['acts'], e['verdict'])
        else:
            e['artefactWhy'] = why
    M.patch_core(g, [app for _, app, _, e in gas2_apps if e.get('artefact')])
    for e in contracts:
        if e['udiApp'] and e['udiBoot']:
            a, why = g.artefact(g.path('programs', 'apps', e['udiApp']), core)
            e['udiArtefact'] = bool(a)
            if not a:
                e['udiArtefactWhy'] = why
            if a:
                shutil.copy(a, os.path.join(art_dir, os.path.basename(a)))

    # one run of each, to say whether it runs: GAS2's own run, and the hand-made boot
    def run(e):
        out_ = {}
        if e.get('artefact'):
            t = time.time()
            out_['gas2'] = parse_run(g.boot(g.path('programs', 'apps', f"{e['app']}_boot.glp"), core))
            out_['gas2']['seconds'] = round(time.time() - t, 1)
        if e.get('udiArtefact'):
            t = time.time()
            out_['udi'] = parse_run(g.boot(g.path('programs', 'apps', e['udiBoot']), core))
            out_['udi']['seconds'] = round(time.time() - t, 1)
        return out_
    with ThreadPoolExecutor(int(os.environ.get('GAS2_WORKERS', '2'))) as ex:
        for e, r in zip(contracts, ex.map(run, contracts)):
            e['checked'] = {k: {'settled': v['settled'], 'lines': len(v['lines']), 'errors': v['errors'],
                                'seconds': v['seconds']} for k, v in r.items()}

    data = {'commit': {'sha': commit[0], 'date': commit[1] if len(commit) > 1 else '',
                       'subject': commit[2] if len(commit) > 2 else ''},
            'built': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
            'tools': os.environ.get('GAS2_TOOLS'),
            'contracts': contracts, 'relations': edges}
    tmp = os.path.join(out, 'data.json.tmp')
    with open(tmp, 'w', encoding='utf-8') as f:
        json.dump(data, f, ensure_ascii=False, indent=1)
    os.replace(tmp, os.path.join(out, 'data.json'))
    ok = sum(1 for e in contracts if e.get('artefact'))
    print(f'{len(contracts)} contracts, {len(edges)} relations, {ok} mini-apps made, '
          f'{time.time() - t0:.0f}s', flush=True)


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
