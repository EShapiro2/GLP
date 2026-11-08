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
# the app maker: jobs one at a time in the live working copy (tools/make.py); a build
# takes the lock to bring its slot to the head, so no job runs in a copy being reset
MADE = os.path.join(DATA, 'made')
make_lock = threading.Lock()
make_queue = []
make_cv = threading.Condition()
make_running = {'key': None}


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


def tools_hash():
    """GAS2's own build code: a build made by other code is rebuilt."""
    import hashlib
    h = hashlib.sha256()
    for f in sorted(os.listdir(os.path.join(HERE, 'tools'))):
        if f.endswith('.py'):
            h.update(open(os.path.join(HERE, 'tools', f), 'rb').read())
    h.update(b'device-tree')  # the build writes the device tree since 2026-11-07
    return h.hexdigest()[:16]


def built_with(s):
    try:
        return json.load(open(os.path.join(slot(s), 'out', 'data.json'), encoding='utf-8')).get('tools')
    except Exception:
        return None


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
    if live and not force and head and head == head_of(os.path.join(slot(live), 'glp')) \
            and built_with(live) == tools_hash():
        log(f'{BRANCH} is still {head[:8]}; nothing to build')
        return
    state['building'], state['buildStarted'] = True, time.time()
    try:
        log(f'building {head[:8] if head else "?"} in slot {target}')
        with make_lock:
            g = prepare(os.path.join(slot(target), 'glp'))
        out = os.path.join(slot(target), 'out.new')
        shutil.rmtree(out, ignore_errors=True)
        env = dict(os.environ, GLPC=g.repl_exe, BASH=g.bash, GAS2_TOOLS=tools_hash())
        p = subprocess.run([sys.executable, '-I', os.path.join(HERE, 'tools', 'build.py'), g.root, out],
                           env=env, capture_output=True, text=True, encoding='utf-8', errors='replace')
        if p.returncode != 0 or not os.path.exists(os.path.join(out, 'data.json')):
            raise RuntimeError('build.py: ' + (p.stderr or p.stdout)[-800:])
        log(p.stdout.strip().split('\n')[-1])
        # the part of GLP a device runs, for GAS2 on the web (device/bin/web.dart)
        d = subprocess.run([sys.executable, '-I', os.path.join(HERE, 'tools', 'device_tree.py'), g.root,
                            os.path.join(out, 'device'), os.path.join(out, 'data.json')],
                           capture_output=True, text=True, encoding='utf-8', errors='replace')
        if d.returncode != 0:
            log('device tree: ' + (d.stderr or d.stdout)[-400:])
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


def maker():
    while True:
        with make_cv:
            while not make_queue:
                make_cv.wait()
            key = make_queue.pop(0)
        job = os.path.join(MADE, key)
        make_running['key'] = key
        try:
            with make_lock:
                s = state['live'] or live_slot()
                g = Glp(os.path.join(slot(s), 'glp'))
                p = subprocess.run([sys.executable, '-I', os.path.join(HERE, 'tools', 'make.py'), g.root, job],
                                   env=dict(os.environ, GLPC=g.repl_exe, BASH=g.bash), capture_output=True,
                                   text=True, encoding='utf-8', errors='replace', timeout=1800)
            if not os.path.exists(os.path.join(job, 'result.json')):
                with open(os.path.join(job, 'result.json'), 'w', encoding='utf-8') as f:
                    json.dump({'key': key, 'step': 'maker', 'why': (p.stderr or p.stdout)[-800:]}, f)
            log(f'made {key}: {p.stdout.strip()[-200:]}')
        except Exception as e:
            with open(os.path.join(job, 'result.json'), 'w', encoding='utf-8') as f:
                json.dump({'key': key, 'step': 'maker', 'why': str(e)[:800]}, f)
        finally:
            make_running['key'] = None


def make_job(body):
    s, data = live_data()
    if not data:
        return 503, {'error': 'the first build is under way'}
    txt, glp_ = body.get('txt'), body.get('glp')
    if not isinstance(txt, str) or not isinstance(glp_, str) or len(txt) + len(glp_) > 200000:
        return 400, {'error': 'a contract is its text and its schemas, under 200 KB'}
    import hashlib
    key = hashlib.sha256('\0'.join([txt, glp_, data['commit']['sha']]).encode('utf-8')).hexdigest()[:16]
    job = os.path.join(MADE, key)
    if os.path.exists(os.path.join(job, 'result.json')) or key in make_queue or make_running['key'] == key:
        return 200, {'key': key}
    if len(make_queue) >= 10:
        return 503, {'error': 'the maker is busy; try again in a few minutes'}
    os.makedirs(job, exist_ok=True)
    with open(os.path.join(job, 'job.json'), 'w', encoding='utf-8') as f:
        json.dump({'txt': txt, 'glp': glp_, 'base': body.get('base'), 'commit': data['commit']['sha']}, f)
    with make_cv:
        make_queue.append(key)
        make_cv.notify()
    return 200, {'key': key}


def made(key):
    job = os.path.join(MADE, key)
    r = os.path.join(job, 'result.json')
    if os.path.exists(r):
        return json.load(open(r, encoding='utf-8'))
    if make_running['key'] == key:
        return {'key': key, 'step': 'running'}
    if key in make_queue:
        return {'key': key, 'step': 'queued', 'ahead': make_queue.index(key) + (1 if make_running['key'] else 0)}
    return None


def live_app(app):
    """A mini-app of the live build as a device installs it: so that two devices
    installing it run one source, whichever build their app came with."""
    s = state['live'] or live_slot()
    if not s:
        return None
    root = os.path.join(slot(s), 'out', 'device', 'programs')
    try:
        idx = json.load(open(os.path.join(root, 'gas2_apps.json'), encoding='utf-8'))
    except OSError:
        return None
    e = next((x for x in idx if x['app'] == app), None)
    d = os.path.join(root, 'apps', app)
    if not e or not os.path.isdir(d):
        return None
    files = {}
    for n in sorted(os.listdir(d)):
        if re.fullmatch(r'[a-z0-9_]+\.(glp|vglp|json)', n):
            with open(os.path.join(d, n), encoding='utf-8') as f:
                files[n] = f.read()
    return dict(e, files=files)


def source(cid):
    s, data = live_data()
    c = next((x for x in (data or {}).get('contracts', []) if x['id'] == cid), None)
    if not c:
        return None
    root = os.path.join(slot(s), 'glp')
    t = os.path.join(root, *c['file'].split('/'))
    rd = lambda p: open(p, encoding='utf-8').read() if os.path.exists(p) else ''
    return {'id': cid, 'txt': rd(t), 'glp': rd(t[:-4] + '.glp')}


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
        m = re.fullmatch(r'/api/source/([A-Za-z0-9-]+)', p)
        if m:
            r = source(m.group(1))
            return self.send(200, r) if r else self.send(404, {'error': 'no such contract'})
        m = re.fullmatch(r'/api/made/([0-9a-f]{16})', p)
        if m:
            r = made(m.group(1))
            return self.send(200, r) if r else self.send(404, {'error': 'no such app'})
        m = re.fullmatch(r'/apps/(gas2_[a-z0-9_]+)\.json', p)
        if m:
            r = live_app(m.group(1))
            return self.send(200, r) if r else self.send(404, {'error': 'no such mini-app'})
        m = re.fullmatch(r'/made/([0-9a-f]{16})\.json', p)
        if m:
            return self.file(os.path.join(MADE, m.group(1), 'app.json'))
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
        if u.path == '/api/make':
            try:
                n = int(self.headers.get('Content-Length') or 0)
                code, r = make_job(json.loads(self.rfile.read(min(n, 300000)).decode('utf-8')))
                return self.send(code, r)
            except Exception as e:
                return self.send(400, {'error': str(e)[:300]})
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
    os.makedirs(MADE, exist_ok=True)
    threading.Thread(target=maker, daemon=True).start()
    srv = ThreadingHTTPServer(('127.0.0.1' if os.environ.get('HOST') is None else os.environ['HOST'], PORT), Handler)
    log(f'GAS on port {PORT}, following {UPSTREAM} {BRANCH}, data in {DATA}')
    srv.serve_forever()
