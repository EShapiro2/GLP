"""GAS2's app maker: a contract a person writes, its text (NAME.txt: title, clauses,
Pragmas) and its act schemas (NAME.glp: contract_named/2), taken down the cascade
GAS2 takes Udi's contracts down (build.py's cascade), to a mini-app certified here and
packed for a device to install.

Usage:  python3 -I tools/make.py <GLP working copy> <job directory>

The job directory holds job.json, {"txt": ..., "glp": ..., "base": ...}, and gets
result.json (what each step gave, and why it stopped) and, where the mini-app was
made, app.json: {app, id, title, shapes, files}, the mini-app's directory as a device
writes it under programs/apps/ and certifies under its person's key (device/lib/host.dart).
The contract's files go under programs/gas2made/ of the working copy, the mini-app under
programs/apps/gas2m_<id>_<job>; server.py's next build of that copy cleans both away."""
import json
import os
import re
import sys
import tempfile
import time

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from glp import Glp  # noqa: E402
import build as B  # noqa: E402
import miniapp as M  # noqa: E402

ID = re.compile(r'[A-Z][A-Za-z0-9]*(-[A-Z0-9]+)*')


def make(root, job):
    j = json.load(open(os.path.join(job, 'job.json'), encoding='utf-8'))
    key = os.path.basename(os.path.normpath(job))
    res = {'key': key, 'base': j.get('base'), 'started': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())}

    def done(**kw):
        res.update(kw)
        res['finished'] = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
        with open(os.path.join(job, 'result.json'), 'w', encoding='utf-8') as f:
            json.dump(res, f, ensure_ascii=False, indent=1)
        return res

    txt, glp = j['txt'].replace('\r\n', '\n'), j['glp'].replace('\r\n', '\n')
    first = next((l for l in txt.split('\n') if l.strip() and not l.startswith('%')), '')
    m = re.match(r'(\S+): (.+)$', first)
    if not m or not ID.fullmatch(m.group(1)):
        return done(step='text', why='the text\'s first line is not "NAME: title", NAME as GLP\'s contracts name theirs (GC, GKK, GDO-LB)')
    cid, title = m.group(1), m.group(2).strip()
    res.update(id=cid, title=title)
    if not re.search(r'^contract_named\(([a-z_0-9]+),', glp, re.M):
        return done(step='schemas', why='the schemas name no contract: contract_named(<name>, [schema(...), ...]) is missing')
    g = Glp(root)
    d = g.path('programs', 'gas2made', f'{cid}_{key[:6]}')
    os.makedirs(d, exist_ok=True)
    for n, t in ((f'{cid}.txt', txt), (f'{cid}.glp', glp)):
        with open(os.path.join(d, n), 'w', encoding='utf-8', newline='\n') as f:
            f.write(t)
    c = B.contract_text(os.path.join(d, f'{cid}.txt'))
    res['clauses'] = c['clauses']
    r = B.cascade(g, c)
    res.update(step=r['step'], why=r['why'], verdict=r['verdict'], seconds=r.get('seconds'))
    if not r['platform']:
        return done()
    res['schemas'] = list(B.schemas_of(r.get('read') or r['term'] or ''))
    low = cid.lower().replace('-', '_')
    app = f'gas2m_{low}_{key[:6]}'
    ad = M.write_app(g, app, cid, title, r['platform'])
    res['acts'] = M.proposals(r['platform'])
    a, why = g.artefact(ad, tempfile.mkdtemp(prefix='gas2make-'))
    if not a:
        return done(step='certify', why=f'the mini-app was not certified: {why}')
    files = {}
    for n in sorted(os.listdir(ad)):
        with open(os.path.join(ad, n), encoding='utf-8') as f:
            files[n] = f.read()
    shapes = json.loads(files['shapes.json'])
    with open(os.path.join(job, 'app.json'), 'w', encoding='utf-8') as f:
        json.dump({'app': app, 'id': cid, 'title': title, 'shapes': shapes, 'files': files}, f, ensure_ascii=False)
    return done(step='made', app=app)


if __name__ == '__main__':
    r = make(sys.argv[1], sys.argv[2])
    print(r['step'], r.get('why') or '')
