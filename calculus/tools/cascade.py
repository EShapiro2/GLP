#!/usr/bin/env python3
"""cascade.py --- the bottom row of Figure 1: the expanded schemas of a contract compiled to a program by the tools
of the abstraction cascade in the GLP repository, as its cascade.sh runs them on an owner's contract.

    python3 tools/cascade.py [NAME ...]     the contracts named, or all

For each it writes cascade/NAME/: verdict.txt, the verdict of GLP's checker (programs/gsc, check/2); vmat.tex, the
VMAT form (programs/gsc, compile_contract/2); platform.vglp, the canonical platform (programs/vglp/cva/compiler,
compile/5, with no introduction named and no Interface pragmas); platform.glp, the platform compiled to GLP (the
REPL's :emit), counted compiled only if it then loads in GLP, the load's lines in run.log; and prints a line.  A
parametric contract, a term parametric(...), gets its verdict and its VMAT form and no platform.  cascade/STATUS.txt has the lines of the last run of all.  Needs the Dart SDK on
the PATH and a clone of GLP beside this directory, or $GLP_DIR; it writes a scratch directory under the clone's
programs/tests/cascade_emit/ and removes it.
"""
import os, re, shutil, subprocess, sys
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import toglp

GLP = os.environ.get('GLP_DIR') or next((d for d in (os.path.join(os.path.dirname(ROOT), 'GLP'), '/home/user/GLP') if os.path.isdir(d)), None)


PARAMETRIC = 'no platform: a parametric contract has none until it is instantiated'


def repl(text, timeout=int(os.environ.get('CASCADE_TIMEOUT', 600))):
    try:
        r = subprocess.run(['bin/glpc'], input=text, capture_output=True, text=True, cwd=os.path.join(GLP, 'glp_runtime'), timeout=timeout)
    except subprocess.TimeoutExpired:
        return ['Error: no answer in %d seconds' % timeout]
    return [re.sub(r'^(GLP> )*', '', l) for l in (r.stdout + r.stderr).split('\n')]


def answer(lines, var):
    for l in lines:
        if l.startswith(var + ' = '):
            return l[len(var) + 3:]
    return None


def contract_txt(n):
    """The contract as the cascade's reader of pragmas takes it: one clause for each act, titled by the act, with
    the sentence of the act; and its Interface pragmas, printed from the act: the first of those who decide it
    proposes it by a button of its name, with a field for each name its own state does not give, and each other who
    decides is asked for their consent to the act, by its name."""
    import gcsas
    acts = [e for e in toglp.schemas(n) if e['kind'] == 'act']
    flat = {}
    for l in gcsas.get('gx', n)[1:]:          # the expanded sentences of the pragmas, by the names of their acts
        m = re.match(r'[^.]*\. [%s] (.*)$' % gcsas.MARKS, l)
        for snt in gcsas.sentences(m.group(1)) if m else []:
            mm = re.match(r"([A-Z][A-Za-z' -]*?): (.+)$", snt)
            if mm:
                flat.setdefault(mm.group(1), []).append(mm.group(2))
    w = gcsas.World(sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'gr')) if f.endswith('.txt') and not f.startswith('NOTES')))
    kinds = gcsas.kinds_from(w, n)
    title = gcsas.get('gr', n)[0]
    out, prag, used = [title], ['Pragmas'], {}
    for k, e in enumerate(acts, 1):
        nm = toglp.ident(e['name'])
        i = used.get(nm, 0)
        used[nm] = i + 1
        t = e['name'] if not i else '%s %d' % (e['name'], i + 1)
        sent = flat.get(e['name'], [e['name'] + '.'])[min(i, len(flat.get(e['name'], [1])) - 1)]
        out.append('%d. %s %s.  %s' % (k, e['mark'], t, sent))
        named = [g for g in e['guards'] if '(' not in g]
        roles = sum(1 for g in e['guards'] if '(' in g)
        deciders = ['Decider' + ('' if j == 0 else 'ABCDEFG'[j]) for j in range(roles)]
        listed = ['Codecider' + ('' if j == 0 else 'ABCDEFG'[j]) for j in range(roles)]
        who = named + deciders
        if not who:
            continue
        mine = {a for l in e['body'] if not l[0] and l[2] and l[2][0] == who[0] for a in l[2]}        # what the proposer's own state names
        names = []
        for l in e['head'] + e['body']:
            for a in l[2]:
                a = a.lstrip('*#')
                if re.fullmatch(gcsas.NAME, a) and a not in names:
                    names.append(a)
        fields = ['<%s>' % sorted(kinds.get(a, {a.lower()}))[0] for a in names if a != who[0] and a not in mine]
        prag.append('%s: %s [%s]' % (t, who[0], t))        # the fields are added as the reader of the pragmas asks for them
        must = set(re.findall(r'\b([A-Z][a-z]+) must consent\b', sent))       # a person who must consent is a guard who is not asked (vGLP, 5.4)
        for g in who[1:] + listed:
            if g in must:
                continue
            prag.append('  %s: %s asks for your consent to %s.' % (g, who[0], t.lower()))     # a question names persons of the act only
    return '\n'.join(out + prag) + '\n'


def run(n):
    name = re.sub(r'[^a-z0-9]', '_', n.lower())
    out = os.path.join(ROOT, 'cascade', n)
    os.makedirs(out, exist_ok=True)
    put = lambda f, s: open(os.path.join(out, f), 'w', encoding='utf-8').write(s if s.endswith('\n') else s + '\n')
    ct = toglp.contract_term(n)[0].replace('\n', '')
    gsc = GLP + '/programs/gsc/'
    rd = answer(repl('%s\n:limit 10000000\nas_read(%s, R).\n:quit\n' % (gsc, ct)), 'R')
    if not rd:
        return 'no reading by as_read/2'
    vt = answer(repl('%s\n:limit 1000000000\ncheck(%s, V).\n:quit\n' % (gsc, ct)), 'V')
    if not vt:
        return 'no verdict'
    put('verdict.txt', vt)
    if not vt.startswith('syntactically_grassroots') and not vt.startswith('grassroots_with_wards'):
        return 'not certified'
    lines = repl('%s\n:limit 1000000000\ncompile_contract(%s, %s).\n:quit\n' % (gsc, name, ct))
    vm, on = [], False
    for l in lines:
        if l.startswith('Goal reduction limit set to'):
            on = True
        elif l.startswith('→ '):
            on = False
        elif on and l:
            vm.append(l)
    if not vm:
        return 'certified; no VMAT form printed'
    put('vmat.tex', '\n'.join(vm))
    res = 'certified; VMAT %d displays' % sum(1 for l in vm if l == '\\begin{align*}')
    if any(l.startswith('% not compiled') for l in vm):
        res = 'certified; VMAT not compiled: ' + next(l for l in vm if l.startswith('% not compiled'))[:80]
    if ct.startswith('parametric('):      # a parametric contract is certified and has no platform until it is instantiated (vGLP, 5.4; vGLP #28, 2026-11-15)
        for f in ('platform.vglp', 'platform.glp', 'run.log', n + '.txt'):
            if os.path.exists(os.path.join(out, f)):
                os.remove(os.path.join(out, f))
        return res + '; ' + PARAMETRIC
    txt = contract_txt(n)
    for _ in range(60):          # the reader of the pragmas says which names a line leaves unnamed: a field is added for each
        put(n + '.txt', txt)
        pr = subprocess.run(['bash', GLP + '/programs/vglp/cva/compiler/pragmas.sh', os.path.join(out, n + '.txt'), rd], capture_output=True, text=True)
        m = re.search(r'the pragma "([^"]+)" leaves unnamed the variable (.*?) of ', pr.stderr)
        if pr.returncode == 0 or not m:
            break
        add = ''.join(' <%s>' % v.strip().lower() for v in m.group(2).split(','))
        txt, k = re.subn(r'(?m)^(%s: \w+ \[[^\]]*\](?: <[^>]*>)*)$' % re.escape(m.group(1)), lambda mm: mm.group(1) + add, txt)
        if not k:
            break
    if pr.returncode != 0:
        return res + '; its Interface pragmas were not read: ' + pr.stderr.strip().split('\n')[0][:200]
    lines = repl('%s\n:limit 1000000000\ncompile(%s, %s, %s, no_introduction, %s).\n:quit\n' % (GLP + '/programs/vglp/cva/compiler/', name, rd, vt, pr.stdout.strip()))
    if '%% compile begin' not in lines or '%% compile end' not in lines:
        return res + '; no platform printed: ' + ' '.join(l for l in lines if 'rror' in l or '✗' in l)[:120]
    mod = lines[lines.index('%% compile begin') + 1:lines.index('%% compile end')]
    put('platform.vglp', '\n'.join(mod))
    if any(l.startswith('%% not compiled') for l in mod):
        return res + '; platform not compiled: ' + next(l for l in mod if l.startswith('%% not compiled'))[:100]
    res += '; platform.vglp %d lines' % len(mod)
    emit = os.path.join(GLP, 'programs', 'tests', 'cascade_emit', 'calculus_' + name)
    shutil.rmtree(emit, ignore_errors=True)
    os.makedirs(emit)
    try:
        shutil.copy(os.path.join(out, 'platform.vglp'), emit)
        lines = repl(':emit %s\n:quit\n' % emit)
        g = os.path.join(emit, 'platform.glp')
        if not os.path.exists(g):
            return res + '; not compiled to GLP: ' + ' '.join(l for l in lines if 'rror' in l or '✗' in l)[:160]
        shutil.copy(g, out)
        res += '; platform.glp %d lines' % sum(1 for _ in open(g, encoding='utf-8'))
        lines = repl('%s\n:quit\n' % g)          # compiled only if platform.glp loads in GLP
        bad = [l for l in lines if 'rror' in l or '✗' in l or 'Duplicate' in l]
        put('run.log', '\n'.join(['load %s' % os.path.relpath(g, GLP)] + (bad or [l for l in lines if l.startswith('✓')])))
        if bad or not any(l.startswith('✓') for l in lines):
            return res + '; platform.glp does not load: ' + ' '.join(bad)[:160]
        return res + '; platform.glp loads'
    finally:
        shutil.rmtree(emit, ignore_errors=True)


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('-')]
    names = args
    if not names:
        names = sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'sasx')) if f.endswith('.txt'))
    from concurrent.futures import ThreadPoolExecutor      # each contract is run by processes of its own, four at a time

    def one(n):
        try:
            r = run(n)
        except Exception as ex:
            r = 'EXCEPTION %s: %s' % (type(ex).__name__, str(ex)[:120])
        line = '%-10s %s' % (n, r)
        print(line, flush=True)
        return line
    with ThreadPoolExecutor(max_workers=int(os.environ.get('JOBS', os.cpu_count() or 1))) as ex:
        lines = list(ex.map(one, names))
    if not args:
        open(os.path.join(ROOT, 'cascade', 'STATUS.txt'), 'w', encoding='utf-8').write('\n'.join(lines) + '\n')


if __name__ == '__main__':
    main()
