"""GAS2's server: the site, the build of every contract of GLP's main branch, once an
hour, and live runs of the mini-apps on super-apps.

    python3 -I server.py [port]

Environment: PORT (default 5101); DATA, where the two working copies of GLP and their
builds live (default ./data); UPSTREAM, the GLP repository followed (default
https://github.com/EShapiro2/GLP.git) and BRANCH (main); INTERVAL, seconds between
checks (3600); DART, the dart executable (dart); BASH, a bash with perl (found).

The two working copies, slots a and b, take turns: the site serves the build of one
while the other is brought to the branch's head and built, and is switched to only
when its build is complete."""
import json
import mimetypes
import os
import re
import shutil
import subprocess
import sys
import threading
import time
import traceback
from http.server import ThreadingHTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs

sys.dont_write_bytecode = True
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, 'tools'))
from glp import Glp, IS_WIN  # noqa: E402
from build import parse_run  # noqa: E402

PORT = int(os.environ.get('PORT') or (sys.argv[1] if len(sys.argv) > 1 else 5101))
DATA = os.path.abspath(os.environ.get('DATA') or os.path.join(HERE, 'data'))
UPSTREAM = os.environ.get('UPSTREAM', 'https://github.com/EShapiro2/GLP.git')
BRANCH = os.environ.get('BRANCH', 'main')
INTERVAL = int(os.environ.get('INTERVAL', '3600'))
DART = os.environ.get('DART', 'dart')
WEB = os.path.join(HERE, 'web')

state = {'live': None, 'building': False, 'buildStarted': None, 'lastCheck': None, 'lastError': None,
         'nextCheck': None, 'lastBuildSeconds': None, 'log': []}
run_lock = threading.Lock()
run_cache = {}


def log(msg):
    line = time.strftime('%Y-%m-%d %H:%M:%S ') + msg
    print(line, flush=True)
    state['log'] = (state['log'] + [line])[-40:]


def slot(name):
    return os.path.join(DATA, name)


def live_slot():
    p = os.path.join(DATA, 'live.txt')
    if os.path.exists(p):
        s = open(p).read().strip()
        if s in ('a', 'b') and os.path.exists(os.path.join(slot(s), 'out', 'data.json')):
            return s
    return None


def git(args, cwd=None, check=True):
    p = subprocess.run(['git'] + args, cwd=cwd, capture_output=True, text=True)
    if check and p.returncode != 0:
        raise RuntimeError(f"git {' '.join(args)}: {p.stderr.strip()[:300]}")
    return p.stdout.strip()


def head_of(clone):
    return git(['-C', clone, 'rev-parse', 'HEAD'], check=False) if os.path.exists(os.path.join(clone, '.git')) else None


def remote_head():
    out = git(['ls-remote', UPSTREAM, f'refs/heads/{BRANCH}'])
    return out.split()[0] if out else None


def prepare(clone):
    """The working copy at the branch's head, GAS2's files of an earlier build removed."""
    if not os.path.exists(os.path.join(clone, '.git')):
        os.makedirs(os.path.dirname(clone), exist_ok=True)
        # LF endings as committed: pragmas.sh and the GLP reader read the files as written
        git(['-c', 'core.autocrlf=false', 'clone', '--depth', '1', '-b', BRANCH, UPSTREAM, clone])
        git(['-C', clone, 'config', 'core.autocrlf', 'false'])
    else:
        git(['-C', clone, 'fetch', '--depth', '1', 'origin', BRANCH])
        git(['-C', clone, 'reset', '--hard', 'FETCH_HEAD'])
    git(['-C', clone, 'clean', '-fdq', '--', 'programs'])
    g = Glp(clone)
    # the REPL, compiled ahead of time where its Dart sources are newer than it
    exe = g.repl_exe
    newest = 0
    for sub in ('lib', 'bin'):
        for d, _, fs in os.walk(os.path.join(g.rt, sub)):
            for f in fs:
                if f.endswith('.dart'):
                    newest = max(newest, os.path.getmtime(os.path.join(d, f)))
    if not os.path.exists(exe) or os.path.getmtime(exe) < newest:
        log('compiling the REPL')
        subprocess.run([DART, 'pub', 'get'], cwd=g.rt, check=True, capture_output=True)
        p = subprocess.run([DART, 'compile', 'exe', os.path.join('bin', 'glp_repl.dart'), '-o', exe],
                           cwd=g.rt, capture_output=True, text=True)
        if p.returncode != 0:
            raise RuntimeError('compiling the REPL: ' + (p.stderr or p.stdout)[-400:])
    return g


def build_once(force=False):
    live = live_slot()
    target = 'b' if live == 'a' else 'a'
    state['lastCheck'] = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    try:
        head = remote_head()
    except Exception as e:
        state['lastError'] = f'checking {UPSTREAM}: {e}'
        log(state['lastError'])
        return
    if live and not force and head and head == head_of(os.path.join(slot(live), 'glp')):
        log(f'{BRANCH} is still {head[:8]}; nothing to build')
        return
    state['building'], state['buildStarted'] = True, time.time()
    try:
        log(f'building {head[:8] if head else "?"} in slot {target}')
        g = prepare(os.path.join(slot(target), 'glp'))
        out = os.path.join(slot(target), 'out.new')
        shutil.rmtree(out, ignore_errors=True)
        env = dict(os.environ, GLPC=g.repl_exe, BASH=g.bash)
        p = subprocess.run([sys.executable, '-I', os.path.join(HERE, 'tools', 'build.py'), g.root, out],
                           env=env, capture_output=True, text=True, encoding='utf-8', errors='replace')
        if p.returncode != 0 or not os.path.exists(os.path.join(out, 'data.json')):
            raise RuntimeError('build.py: ' + (p.stderr or p.stdout)[-800:])
        log(p.stdout.strip().split('\n')[-1])
        final = os.path.join(slot(target), 'out')
        shutil.rmtree(final, ignore_errors=True)
        os.replace(out, final)
        with open(os.path.join(DATA, 'live.txt'), 'w') as f:
            f.write(target)
        state['live'] = target
        state['lastError'] = None
        run_cache.clear()
        log(f'slot {target} is live')
    except Exception as e:
        state['lastError'] = str(e)[:1000]
        log('build failed: ' + state['lastError'])
        traceback.print_exc()
    finally:
        state['lastBuildSeconds'] = round(time.time() - state['buildStarted'])
        state['building'] = False


def updater():
    os.makedirs(DATA, exist_ok=True)
    state['live'] = live_slot()
    while True:
        build_once()
        state['nextCheck'] = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(time.time() + INTERVAL))
        time.sleep(INTERVAL)


def live_data():
    s = state['live'] or live_slot()
    if not s:
        return None, None
    p = os.path.join(slot(s), 'out', 'data.json')
    return s, json.load(open(p, encoding='utf-8'))


def run_app(cid, which):
    """A live run of a contract's mini-app on its super-apps, in the live working copy."""
    s, data = live_data()
    if not data:
        return {'error': 'no build yet'}
    c = next((x for x in data['contracts'] if x['id'] == cid), None)
    if not c:
        return {'error': f'no contract {cid}'}
    g = Glp(os.path.join(slot(s), 'glp'))
    core = g.path('programs', 'social', 'graph', 'core')
    if which == 'udi' and c.get('udiArtefact'):
        boot = g.path('programs', 'apps', c['udiBoot'])
    elif which == 'gas2' and c.get('artefact'):
        boot = g.path('programs', 'apps', f"{c['app']}_boot.glp")
    else:
        return {'error': 'no such run'}
    key = (s, cid, which)
    with run_lock:
        if key in run_cache and time.time() - run_cache[key]['at'] < 30:
            return run_cache[key]
        t = time.time()
        r = parse_run(g.boot(boot, core))
        r['seconds'] = round(time.time() - t, 1)
        r['at'] = time.time()
        r['commit'] = data['commit']['sha']
        run_cache[key] = r
        return r


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        pass

    def send(self, code, body, ctype='application/json; charset=utf-8', cache='no-cache'):
        if isinstance(body, (dict, list)):
            body = json.dumps(body, ensure_ascii=False).encode('utf-8')
        elif isinstance(body, str):
            body = body.encode('utf-8')
        self.send_response(code)
        self.send_header('Content-Type', ctype)
        self.send_header('Content-Length', str(len(body)))
        self.send_header('Cache-Control', cache)
        self.end_headers()
        self.wfile.write(body)

    def file(self, path, cache='no-cache'):
        if not os.path.isfile(path):
            return self.send(404, {'error': 'not found'})
        ctype = mimetypes.guess_type(path)[0] or 'application/octet-stream'
        if path.endswith(('.vglp', '.glp', '.txt')):
            ctype = 'text/plain; charset=utf-8'
        if ctype.startswith('text/') and 'charset' not in ctype:
            ctype += '; charset=utf-8'
        with open(path, 'rb') as f:
            self.send(200, f.read(), ctype, cache)

    def do_GET(self):
        u = urlparse(self.path)
        p = u.path
        if p in ('', '/'):
            return self.file(os.path.join(WEB, 'index.html'))
        if p == '/data.json':
            s = state['live'] or live_slot()
            if not s:
                return self.send(503, {'error': 'the first build is under way', 'state': status()})
            return self.file(os.path.join(slot(s), 'out', 'data.json'))
        if p == '/api/status':
            return self.send(200, status())
        m = re.fullmatch(r'/(artefacts|platforms)/([A-Za-z0-9_.-]+)', p)
        if m:
            s = state['live'] or live_slot()
            return self.file(os.path.join(slot(s), 'out', m.group(1), m.group(2))) if s else self.send(404, {})
        m = re.fullmatch(r'/([a-z0-9_-]+\.(html|css|js|svg|png|ico))', p)
        if m:
            return self.file(os.path.join(WEB, m.group(1)))
        self.send(404, {'error': 'not found'})

    def do_POST(self):
        u = urlparse(self.path)
        m = re.fullmatch(r'/api/run/([A-Za-z0-9-]+)', u.path)
        if m:
            which = parse_qs(u.query).get('which', ['gas2'])[0]
            try:
                return self.send(200, run_app(m.group(1), which))
            except Exception as e:
                return self.send(500, {'error': str(e)[:500]})
        self.send(404, {'error': 'not found'})


def status():
    s = dict(state)
    s['upstream'], s['branch'], s['interval'] = UPSTREAM, BRANCH, INTERVAL
    if s['building'] and s['buildStarted']:
        s['buildingFor'] = round(time.time() - s['buildStarted'])
    s.pop('buildStarted', None)
    return s


if __name__ == '__main__':
    threading.Thread(target=updater, daemon=True).start()
    srv = ThreadingHTTPServer(('127.0.0.1' if os.environ.get('HOST') is None else os.environ['HOST'], PORT), Handler)
    log(f'GAS2 on port {PORT}, following {UPSTREAM} {BRANCH}, data in {DATA}')
    srv.serve_forever()
