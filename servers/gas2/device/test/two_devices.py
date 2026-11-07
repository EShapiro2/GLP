# Two GAS2 devices, driven through their pages' API: trust, install, befriend over the relay,
# invite and accept, then the mini-app's acts.  Start a relay (talmonlab's server/gasrelay) and two
# devices (bin/gas2.dart --port 8301 / 8302 --relay <it>) first; see README.md.
#
#   python3 test/two_devices.py [gas2_gkk | gas2_gc | gas2_gsn]
import json, time, urllib.request, sys
APP = sys.argv[1] if len(sys.argv) > 1 else 'gas2_gc'
# A device is a page's base URL: GAS2_A and GAS2_B (default the two devices on the
# loopback); a base on GAS2 on the web (bin/web.dart) is a person made there at first call.
import os, urllib.error
BASE = {'A': os.environ.get('GAS2_A', 'http://127.0.0.1:8301'), 'B': os.environ.get('GAS2_B', 'http://127.0.0.1:8302')}
TOKEN = {}
def call(p, path, body=None):
    for _ in range(20):
        h = {'X-GAS2-Token': TOKEN[p]} if p in TOKEN else {}
        r = urllib.request.Request(BASE[p] + path, data=None if body is None else json.dumps(body).encode(), headers=h, method='GET' if body is None else 'POST')
        try:
            return json.load(urllib.request.urlopen(r, timeout=60))
        except urllib.error.HTTPError as e:
            if e.code != 401: raise
        except (urllib.error.URLError, ConnectionError, TimeoutError):
            time.sleep(3)  # a server restarting, a network's blip
            continue
            TOKEN[p] = json.load(urllib.request.urlopen(urllib.request.Request(BASE[p] + '/api/start', data=b'', method='POST')))['token']
if any(b.startswith('https:') for b in BASE.values()):
    # through a proxy curl takes better than urllib does
    import subprocess
    def call(p, path, body=None):
        for _ in range(20):
            h = ['-H', 'X-GAS2-Token: ' + TOKEN[p]] if p in TOKEN else []
            d = ['-X', 'POST', '--data-binary', json.dumps(body)] if body is not None else []
            r = subprocess.run(['curl', '-s', '-m', '60', '-w', '\n%{http_code}', *h, *d, BASE[p] + path], capture_output=True, text=True)
            out, _, code = r.stdout.rpartition('\n')
            if code == '401':
                TOKEN[p] = json.loads(subprocess.run(['curl', '-s', '-X', 'POST', BASE[p] + '/api/start'], capture_output=True, text=True).stdout)['token']
                continue
            if code == '200':
                return json.loads(out)
            time.sleep(3)
def st(p): return call(p, '/api/state')
def act(p, j): call(p, '/api/act', j)
def add(p, k): call(p, '/api/add', {'key': k})
def app(p): return next(a for a in st(p)['apps'] if a['app'] == APP)
def wait(cond, t=60, what=''):
    for _ in range(t*2):
        r = cond()
        if r: return r
        time.sleep(0.5)
    raise SystemExit('timeout: ' + what)
def req(p):
    for k, d in app(p)['draws'].items():
        if d['v'].get('f') == 'request_r': return int(k), d['v']['a'][0]['a'][0]
def asks(p):
    return [(int(k), d['v']['a'][0]['a'][2]['a'][0], d['v']) for k, d in app(p)['draws'].items() if d['v'].get('f') == 'approval_w']
def view(p):
    d = app(p)['draws'].get('0')
    if not d: return None
    vs = [x for x in d['v']['a'][0] if isinstance(x, dict) and x.get('f') == 'view']
    return vs[-1]['a'] if vs else None
def propose(p, f, args):
    i, path = wait(lambda: req(p), what='request question')
    act(p, {'op': 'grant', 'app': APP, 'id': i, 'path': path, 'value': {'propose': {'f': f, 'a': args}}})
A, B = 'A', 'B'
wait(lambda: st(A)['status'] == 'ready' and st(B)['status'] == 'ready', 120, 'ready')
ka, kb = st(A)['me'], st(B)['me']
# GAS2_ADD: a link of the app maker, added on both first (APP is then the made app's name)
if os.environ.get('GAS2_ADD'):
    for p in (A, B): act(p, {'op': 'add', 'link': os.environ['GAS2_ADD']})
    wait(lambda: all(any(a['app'] == APP for a in st(p)['apps']) for p in (A, B)), 300, 'added from the maker')
print('apps', [a['app'] for a in st(A)['apps']])
for p in (A, B):
    act(p, {'op': 'trust'}); time.sleep(0.5); act(p, {'op': 'install', 'app': APP})
wait(lambda: app(A)['installed'] and app(B)['installed'], what='install')
act(B, {'op': 'accept', 'app': APP, 'key': ka}); act(B, {'op': 'friend', 'key': ka, 'yes': True})
add(A, kb); act(A, {'op': 'invite', 'app': APP, 'key': kb})
wait(lambda: view(A) and view(B), 90, 'mini-app running')
print('running')
propose(A, 'establish', [ka]); propose(B, 'establish', [kb])
wait(lambda: 'identity' in (view(A) or [None, []])[1] and 'identity' in (view(B) or [None, []])[1], what='identity')
propose(A, 'acquaint', [ka, kb])
a = wait(lambda: asks(B), what='acquaint asked')[0]
act(B, {'op': 'grant', 'app': APP, 'id': a[0], 'path': a[1], 'value': 'yes'})
wait(lambda: any(isinstance(x, dict) and x.get('f') == 'knows' for x in view(A)[1]), what='knows')
print('acquainted')
if APP == 'gas2_gc':
    propose(A, 'mint', [ka, 5])
    wait(lambda: any(isinstance(x, dict) and x.get('f') == 'coin' for x in view(A)[1]), what='minted')
    print('A holds', [x for x in view(A)[1] if isinstance(x, dict) and x.get('f') == 'coin'][:3])
    propose(A, 'pay', [ka, kb, 2])
    def bcoins():
        return [x for x in view(B)[1] if isinstance(x, dict) and x.get('f') == 'coin']
    ok = None
    for _ in range(60):
        if asks(B):
            a = asks(B)[0]; act(B, {'op': 'grant', 'app': APP, 'id': a[0], 'path': a[1], 'value': 'yes'})
        if bcoins(): ok = bcoins(); break
        time.sleep(1)
    print('B holds', ok)
if APP == 'gas2_gsn':
    propose(A, 'befriend', [ka, kb])
    a = wait(lambda: asks(B), what='befriend asked')[0]
    act(B, {'op': 'grant', 'app': APP, 'id': a[0], 'path': a[1], 'value': 'yes'})
    wait(lambda: any(isinstance(x, dict) and x.get('f') == 'friend' for x in view(A)[1]), what='friend')
    propose(A, 'message', [ka, kb, {'f': 'speech', 'a': [{'f': 'utterance', 'a': ['Hi Bob, over GAS2']}, ka]}])
    r = wait(lambda: [x for x in view(B)[1] if isinstance(x, dict) and x.get('f') == 'received'], what='received')
    print('B received', json.dumps(r)[:300])
