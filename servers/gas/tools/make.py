#!/usr/bin/env python3
"""GAS's app maker: a contract, written as its owner writes it, made into a
package of the GAS app (phone/tool/packages.py's form), by the tools of the
cascade (cascade.sh's steps 1, 3 and 5) and GAS's harness and menu.

The text is the contract's NAME.txt --- its % lines, its title line
"ID: Title", its clauses and its Pragmas --- then a line "Schemas" and its
speech-act schemas in GSC's notation (notation.py reads them into the term
form).  The steps:

  1. the schemas read into the term (notation.py);
  2. the checker's verdict (programs/gsc, check/2);
  3. the pragmas (programs/vglp/cva/compiler/pragmas.sh);
  4. the canonical platform (programs/vglp/cva/compiler, compile/5);
  5. the live harness (gen_harness.py) and the menu (menu_spec.py);
  6. the package, named by its SHA-256.

Usage: python3 -I make.py TEXT OUTDIR
  writes OUTDIR/result.json --- ok, step, verdict, notes, errors, and for a
  package made, id, title, hash --- and OUTDIR/pkg-<hash>.json.
  GLPC: the command running the GLP REPL (default glp_runtime/bin/glpc);
  BASH: the bash running pragmas.sh (default bash).
"""
import hashlib
import json
import os
import re
import shlex
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
GAS = os.path.dirname(HERE)
REPO = os.path.normpath(os.path.join(GAS, '..', '..'))
sys.path.insert(0, HERE)
import notation          # noqa: E402
from menu_spec import spec  # noqa: E402

LIMIT_CHECK, LIMIT_COMPILE = 10000000, 1000000000


class Refused(Exception):
    def __init__(self, step, errors, **extra):
        super().__init__(step)
        self.step, self.errors, self.extra = step, errors, extra


def repl(lines, timeout=900):
    """The REPL's output for the input lines, each "GLP> " prompt removed."""
    cmd = shlex.split(os.environ.get('GLPC', os.path.join(REPO, 'glp_runtime', 'bin', 'glpc')), posix=os.name != 'nt')
    p = subprocess.run(cmd, input='\n'.join(lines + [':quit']) + '\n', capture_output=True, text=True,
                       encoding='utf-8', cwd=os.path.join(REPO, 'glp_runtime'), timeout=timeout)
    return [re.sub(r'^(GLP> )+', '', l) for l in (p.stdout + p.stderr).splitlines()]


def bash():
    """The bash running pragmas.sh: BASH, or bash on the path, or Git for
    Windows' own."""
    import shutil
    if os.environ.get('BASH'):
        return os.environ['BASH']
    if os.name == 'nt':
        for p in (r'C:\Program Files\Git\bin\bash.exe', r'C:\Program Files\Git\usr\bin\bash.exe'):
            if os.path.exists(p):
                return p
    return shutil.which('bash') or 'bash'


def fwd(p):
    """A path as the REPL and bash read it on any system: with forward slashes."""
    return p.replace(os.sep, '/')


def answer(out, var):
    for l in out:
        if l.startswith(var + ' = '):
            return l[len(var) + 3:]
    return None


def errors_of(out):
    return [l for l in out if re.search(r'Error|error|✗|failed', l)][:10]


def make(text, outdir):
    os.makedirs(outdir, exist_ok=True)
    notes = []
    parts = re.split(r'^Schemas\s*$', text, maxsplit=1, flags=re.M)
    if len(parts) != 2:
        raise Refused('text', ['the schemas follow a line "Schemas", after the contract\'s text and its Pragmas'])
    contract_txt, schemas = parts
    head = next((l for l in contract_txt.splitlines() if l.strip() and not l.startswith('%')), '')
    m = re.match(r'([A-Z][A-Za-z0-9]{0,30}):\s*(.+)$', head.strip())
    if not m:
        raise Refused('text', ['the contract\'s first line (after its % lines) is "ID: Title", as "AGORA: Grassroots Barter"'])
    cid, title = m.group(1), m.group(2).strip()
    name = cid.lower()

    # 1. The term.
    try:
        term = notation.read(schemas, {}, notes)
    except notation.NotationError as e:
        raise Refused('schemas', [str(e)])

    # 2. The verdict.
    out = repl([fwd(os.path.join(REPO, 'programs', 'gsc')) + '/', f':limit {LIMIT_CHECK}', f'check({term}, V).'])
    verdict = answer(out, 'V')
    if verdict is None:
        raise Refused('check', ['the checker gave no verdict'] + errors_of(out), notes=notes)
    if not verdict.startswith('syntactically_grassroots'):
        raise Refused('check', [f'not grassroots: {verdict}'], verdict=verdict, notes=notes)

    with tempfile.TemporaryDirectory() as work:
        # 3. The pragmas, read beside the contracts it extends.
        txt = os.path.join(work, f'{cid}.txt')
        with open(txt, 'w', encoding='utf-8') as f:
            f.write(contract_txt)
        p = subprocess.run([bash(),
                            fwd(os.path.join(REPO, 'programs', 'vglp', 'cva', 'compiler', 'pragmas.sh')), fwd(txt), term],
                           capture_output=True, text=True, encoding='utf-8')
        if p.returncode != 0:
            raise Refused('pragmas', (p.stderr.strip() or 'the pragmas were not read').splitlines()[:5],
                          verdict=verdict, notes=notes)
        pragmas = p.stdout.strip()

        # 4. The platform.
        out = repl([fwd(os.path.join(REPO, 'programs', 'vglp', 'cva', 'compiler')) + '/', f':limit {LIMIT_COMPILE}',
                    f'compile({name}, {term}, {verdict}, no_introduction, {pragmas}).'])
        body, on, done = [], False, False
        for l in out:
            if l == '%% compile begin':
                on = True
            elif l == '%% compile end' and on:
                on, done = False, True
            elif on:
                body.append(l)
        if not done:
            raise Refused('compile', ['the compiler printed no platform'] + errors_of(out), verdict=verdict, notes=notes)
        if body and body[0].startswith('%% not compiled'):
            raise Refused('compile', [re.sub(r'^%%\s*', '', l) for l in body][:12], verdict=verdict, notes=notes)
        plat = os.path.join(work, 'platform.vglp')
        with open(plat, 'w', encoding='utf-8') as f:
            f.write('\n'.join(body) + '\n')

        # 5. The harness and the menu.
        world = os.path.join(work, f'live_{name}')
        p = subprocess.run([sys.executable, '-I', os.path.join(HERE, 'gen_harness.py'), plat, world, cid],
                           capture_output=True, text=True, encoding='utf-8')
        if p.returncode != 0:
            raise Refused('harness', p.stderr.strip().splitlines()[-5:], verdict=verdict, notes=notes)
        s = spec(os.path.join(world, 'platform.vglp'))
        s['id'], s['title'] = cid, title
        s['calendar'] = 'cal(alice, N)' in open(os.path.join(world, 'self.glp'), encoding='utf-8').read()
        s.pop('raw_items', None)

        # 6. The package (phone/tool/packages.py's form), unlisted.
        pkg = {
            'gas': 1, 'id': cid, 'title': title,
            'meta': {'id': cid, 'title': title, 'icon': '🧩', 'family': 'other', 'about': 'Made with GAS\'s app maker.'},
            'spec': s,
            'files': {f: open(os.path.join(world, f), encoding='utf-8').read() for f in ('self.glp', 'platform.vglp')},
        }
    data = json.dumps(pkg, sort_keys=True, separators=(',', ':'), ensure_ascii=False).encode('utf-8')
    h = hashlib.sha256(data).hexdigest()
    with open(os.path.join(outdir, f'pkg-{h}.json'), 'wb') as f:
        f.write(data)
    return {'ok': True, 'id': cid, 'title': title, 'hash': h, 'verdict': verdict, 'notes': notes, 'bytes': len(data)}


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    text, outdir = open(sys.argv[1], encoding='utf-8').read(), sys.argv[2]
    try:
        r = make(text, outdir)
    except Refused as e:
        r = {'ok': False, 'step': e.step, 'errors': e.errors, **e.extra}
    except subprocess.TimeoutExpired:
        r = {'ok': False, 'step': 'time', 'errors': ['the tools took too long']}
    os.makedirs(outdir, exist_ok=True)
    with open(os.path.join(outdir, 'result.json'), 'w', encoding='utf-8') as f:
        json.dump(r, f, ensure_ascii=False, indent=1)
    print(json.dumps(r, ensure_ascii=False)[:2000])


if __name__ == '__main__':
    main()
