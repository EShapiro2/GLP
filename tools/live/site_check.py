#!/usr/bin/env python3
"""Part 4 of the live suite (GSA #2's task for GNA, Code_inbox.md 2026-11-08
12:51 UTC): the store site at peoplesweb.org, servers/peoplesweb.

Three modes, each printing one line per check,
    RESULT <id> PASS|FAIL|PENDING|INFO <what>

  static   every file the site serves loads: the pages, data.js, and for
           every contract each layer of its cascade (the listed programs:
           schemas, verdict, VMAT form, vGLP and GLP platform), downloaded
           whole and its line count held to the count data.js lists; each
           live contract's menu, specs/<id>.json; each paper's PDF or arXiv
           page.
  render   every contract's page, index.html#<ID>, and every live
           demonstration's page, app.html?c=<id>, rendered by headless
           Chrome, which runs the page's JavaScript: no uncaught error and
           no resource that fails to load in the console, and the page
           draws the contract's title.
  live     the live demonstrations named, or all: each world started through
           the API the app page calls, /api/<id>/state, until its status is
           ready and every person has drawn a menu, at most three at a time
           (the server's MAX_WORLDS) and four minutes each.

Usage: site_check.py <base-url> static|render|live [contract ...]
"""
import concurrent.futures as cf
import json
import os
import subprocess
import sys
import time
import urllib.error
import urllib.request

BASE = sys.argv[1].rstrip('/')
MODE = sys.argv[2]
ONLY = [a.upper() for a in sys.argv[3:]]
CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
UA = {'User-Agent': 'glp-live-suite/1 (GSA #2 for GNA)'}


def result(i, v, what):
    print(f'RESULT {i} {v} {what}', flush=True)


def get(url, timeout=60):
    req = urllib.request.Request(url, headers=UA)
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return r.status, r.read()
    except urllib.error.HTTPError as e:
        return e.code, b''
    except Exception as e:  # noqa: BLE001 - reported, not raised
        return f'{type(e).__name__}: {e}', b''


def data():
    s, body = get(f'{BASE}/data.js')
    if s != 200:
        result('4.0-data', 'FAIL', f'data.js: {s}')
        sys.exit(1)
    text = body.decode('utf-8')
    d = json.loads(text[text.index('=') + 1:].strip().rstrip(';'))
    return d


def static():
    for p in ['', 'index.html', 'how.html', 'app.html', 'data.js']:
        s, body = get(f'{BASE}/{p}')
        result(f'4.0-page-{p or "root"}', 'PASS' if s == 200 and body else 'FAIL',
               f'{BASE}/{p}: {s}, {len(body)} bytes')
    s, _ = get(f'{BASE}/favicon.ico')
    result('4.0-favicon', 'PASS' if s == 200 else 'FAIL',
           f'{BASE}/favicon.ico: {s}; a browser asks for it on every page, and the '
           f'console of every page reports the 404')
    d = data()
    cs = d['contracts']
    live = [c for c in cs if c.get('live')]
    result('4.0-data', 'INFO',
           f'{len(cs)} contracts, {len(live)} with a live demonstration, '
           f'built from GLP {d["glpCommit"][0][:8]} ({d["glpCommit"][1]})')

    def layer(c, k, l):
        url = f'{BASE}/{l["file"]}'
        s, body = get(url)
        n = body.decode('utf-8', 'replace').count('\n') if body else 0
        want = l.get('lines')
        # data.js counts lines as the text's lines; a final line with no
        # newline is a line too.
        if body and not body.endswith(b'\n'):
            n += 1
        ok = s == 200 and len(body) > 0 and (want is None or n == want)
        return (f'4.1-download-{c["id"]}-{k}', 'PASS' if ok else 'FAIL',
                f'{l["file"]}: {s}, {len(body)} bytes, {n} lines'
                f'{"" if want is None else f" (listed {want})"}')

    jobs = []
    with cf.ThreadPoolExecutor(8) as ex:
        for c in cs:
            if not c.get('layers'):
                result(f'4.1-download-{c["id"]}', 'INFO',
                       'no layers listed: the contract has no recorded cascade run')
            for k, l in (c.get('layers') or {}).items():
                jobs.append(ex.submit(layer, c, k, l))
        for c in live:
            jobs.append(ex.submit(lambda c=c: spec(c)))
        for j in jobs:
            result(*j.result())

    papers = {}
    for c in cs:
        p = c.get('paper') or {}
        if p.get('pdf'):
            papers[f'{BASE}/{p["pdf"]}'] = p['title']
        if p.get('arxiv'):
            papers[f'https://arxiv.org/abs/{p["arxiv"]}'] = p['title']
    for url, title in sorted(papers.items()):
        s, body = get(url)
        pdf = url.endswith('.pdf')
        ok = s == 200 and (not pdf or body[:5] == b'%PDF-')
        result(f'4.2-paper-{url.rsplit("/", 1)[-1]}', 'PASS' if ok else 'FAIL',
               f'{title}: {url}: {s}, {len(body)} bytes')


def spec(c):
    s, body = get(f'{BASE}/specs/{c["id"].lower()}.json')
    try:
        ok = s == 200 and isinstance(json.loads(body), (dict, list))
    except ValueError:
        ok = False
    return (f'4.1-menu-{c["id"]}', 'PASS' if ok else 'FAIL',
            f'specs/{c["id"].lower()}.json: {s}, {len(body)} bytes')


HERE = os.path.dirname(os.path.abspath(__file__))
# The site has no favicon: every browser asks for one and gets 404, which the
# console reports on every page.  It is reported once, by static (4.0-favicon),
# and not again on each page.
FAVICON = 'favicon.ico'


def chrome(url, secs, wait_for):
    """The page rendered by headless Chrome over the DevTools protocol
    (render.mjs): its text, its uncaught exceptions, its console errors and
    its failed requests, the favicon's left out.  A load cut by
    ERR_NETWORK_CHANGED, this machine's network changing under Chrome, is
    rendered once more: it says nothing of the site."""
    r = _chrome(url, secs, wait_for)
    if any('ERR_NETWORK_CHANGED' in e for e in r['errors']):
        r = _chrome(url, secs, wait_for)
    return r


def _chrome(url, secs, wait_for):
    try:
        p = subprocess.run(['node', f'{HERE}/render.mjs', url, str(secs), wait_for],
                           capture_output=True, text=True, timeout=secs + 60)
        r = json.loads(p.stdout.strip().splitlines()[-1])
    except Exception as e:  # noqa: BLE001 - reported as the check's failure
        return {'text': '', 'errors': [f'render.mjs: {type(e).__name__}: {e}']}
    errs = r['exceptions'] + [e for e in r['consoleErrors'] if FAVICON not in e] + \
        [e for e in r['failedRequests'] if FAVICON not in e]
    return {'text': r['text'], 'errors': errs}


def render():
    """Every contract's page, index.html#<ID>: rendered, its JavaScript run,
    no error, and the contract drawn: its title and its rules."""
    if not os.path.exists(CHROME):
        result('4.3-render', 'PENDING', f'no headless Chrome at {CHROME}')
        return
    d = data()
    cs = [c for c in d['contracts'] if not ONLY or c['id'] in ONLY]

    def one(c):
        url = f'{BASE}/#{c["id"]}'
        r = chrome(url, 20, f'{c["id"]}: ')
        drawn = f'{c["id"]}: ' in r['text']
        rules = all(cl['name'] in r['text'] for cl in c.get('clauses', []))
        ok = drawn and rules and not r['errors']
        return (f'4.3-page-{c["id"]}', 'PASS' if ok else 'FAIL',
                f'{url}: title {"drawn" if drawn else "NOT drawn"}, '
                f'{len(c.get("clauses", []))} rules {"drawn" if rules else "NOT all drawn"}'
                + (f'; errors: {" | ".join(r["errors"])[:600]}' if r['errors'] else ''))

    with cf.ThreadPoolExecutor(4) as ex:
        for r in ex.map(one, cs):
            result(*r)


def live():
    """Each live demonstration named, or all: its world started through the
    API the app page calls, /api/<id>/state, until its status is ready and
    every person has drawn a menu (4.5); then its page, app.html?c=<id>,
    rendered until it shows Ready, with no error, each of the four persons
    drawn with the actions of the menu (4.4).  At most three at a time, the
    server's MAX_WORLDS, four minutes each.  No act is made."""
    d = data()
    cs = [c for c in d['contracts'] if c.get('live') and (not ONLY or c['id'] in ONLY)]

    def one(c):
        url = f'{BASE}/api/{c["id"].lower()}/state'
        t0 = time.time()
        last = None
        while time.time() - t0 < 240:
            s, body = get(url, timeout=40)
            if s == 200:
                try:
                    last = json.loads(body)
                except ValueError:
                    last = None
                st = (last or {}).get('status', '')
                if st.startswith('failed'):
                    break
                if st in ('ready', 'working') and last['persons'] and all(
                        len(v) > 0 for v in last['persons'].values()):
                    break
            time.sleep(3)
        secs = int(time.time() - t0)
        out = []
        if not last:
            out.append((f'4.5-live-{c["id"]}', 'FAIL', f'{url}: no state in {secs}s'))
        else:
            st = last.get('status')
            drawn = {p: len(v) for p, v in last.get('persons', {}).items()}
            ok = st in ('ready', 'working') and drawn and all(n > 0 for n in drawn.values())
            out.append((f'4.5-live-{c["id"]}', 'PASS' if ok else 'FAIL',
                        f'world status {st} after {secs}s, menu items drawn per person {drawn}'
                        f'{", calendar day " + str(last["day"]) if "day" in last else ""}'))
        app = f'{BASE}/app.html?c={c["id"].lower()}'
        r = chrome(app, 60, 'Ready')
        ready = 'Ready' in r['text']
        persons = all(p in r['text'] for p in ('Alice', 'Bob', 'Carol', 'Dave'))
        actions = r['text'].count('ACTIONS')
        ok = ready and persons and actions == 4 and not r['errors']
        out.append((f'4.4-app-{c["id"]}', 'PASS' if ok else 'FAIL',
                    f'{app}: {"Ready" if ready else "NOT ready"}, the four persons '
                    f'{"drawn" if persons else "NOT drawn"}, {actions} menus of actions'
                    + (f'; errors: {" | ".join(r["errors"])[:600]}' if r['errors'] else '')))
        return out

    with cf.ThreadPoolExecutor(3) as ex:
        for rs in ex.map(one, cs):
            for r in rs:
                result(*r)


{'static': static, 'render': render, 'live': live}[MODE]()
