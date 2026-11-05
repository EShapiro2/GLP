#!/usr/bin/env python3
"""The contracts as packages for the GAS app: one file per contract, named by
the SHA-256 of its bytes, and a catalog of them.

A package holds what the app needs to run the contract on top of what every
GAS app carries: the live harness and its contract unit, the canonical
platform and its person (programs/vglp/cva/gas_worlds/live_<c>/ and its
contract/, written by ../tools/build.py), the spec the app page reads
(build/www/specs/<c>.json), the site's line for it (title, icon, family,
about), and the unit's warranties (../tools/warrant.py): the compiler's and
the creator's, with the contract's text, schemas and verdict, from its
cascade record.  Two people's agents work together only when
they run the same package, so the hash is the contract's name: the app checks
it on every download, and invites carry it.

Usage: python3 -I tool/packages.py OUT [CONTRACT ...]
  writes OUT/pkg-<hash>.json for each contract (all with a live world when
  none is named) and OUT/catalog.json listing them.
"""
import hashlib
import json
import os
import subprocess
import sys

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'tools'))
from warrant import FILES, warranties  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
GAS = os.path.normpath(os.path.join(HERE, '..', '..'))       # servers/gas
REPO = os.path.normpath(os.path.join(GAS, '..', '..'))
WORLDS = os.path.join(REPO, 'programs', 'vglp', 'cva', 'gas_worlds')
WWW = os.path.join(GAS, 'build', 'www')


def site_data():
    s = open(os.path.join(WWW, 'data.js'), encoding='utf-8').read()
    return json.loads(s[s.index('=') + 1:].strip().rstrip(';'))


def read(p):
    with open(p, encoding='utf-8') as f:
        return f.read()


def unsigned(cid, meta):
    world = os.path.join(WORLDS, 'live_' + cid)
    spec = json.load(open(os.path.join(WWW, 'specs', cid + '.json'), encoding='utf-8'))
    return {
        'gas': 2,
        'id': spec['id'],
        'title': spec['title'],
        'meta': {k: meta[k] for k in ('id', 'title', 'icon', 'family', 'about') if k in meta},
        'spec': spec,
        'files': {f: read(os.path.join(world, f)) for f in FILES},
    }


def request(pkg):
    rec = os.path.join(REPO, 'cascade', pkg['id'])
    return {'id': pkg['id'], 'files': pkg['files'], 'text': read(os.path.join(rec, pkg['id'] + '.txt')),
            'schemas': read(os.path.join(rec, 'schemas.txt')), 'verdict': read(os.path.join(rec, 'verdict.txt')).strip()}


def package(pkg):
    data = json.dumps(pkg, sort_keys=True, separators=(',', ':'), ensure_ascii=False).encode('utf-8')
    return hashlib.sha256(data).hexdigest(), data, pkg


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    out = sys.argv[1]
    os.makedirs(out, exist_ok=True)
    metas = {c['id'].lower(): c for c in site_data()['contracts']}
    names = [a.lower() for a in sys.argv[2:]] or sorted(
        d[len('live_'):] for d in os.listdir(WORLDS) if d.startswith('live_'))
    commit = subprocess.run(['git', '-C', REPO, 'log', '-1', '--format=%h'],
                            capture_output=True, text=True).stdout.strip()
    pkgs = []
    for cid in names:
        if not os.path.exists(os.path.join(WWW, 'specs', cid + '.json')):
            print(f'packages: {cid}: no spec, skipped', file=sys.stderr)
            continue
        pkgs.append(unsigned(cid, metas.get(cid, {})))
    catalog = []
    for pkg, w in zip(pkgs, warranties([request(p) for p in pkgs])):
        if isinstance(w, Exception):
            print(f"packages: {pkg['id']}: {w}, skipped", file=sys.stderr)
            continue
        pkg['warranty'] = w
        h, data, pkg = package(pkg)
        with open(os.path.join(out, f'pkg-{h}.json'), 'wb') as f:
            f.write(data)
        catalog.append({'id': pkg['id'], 'title': pkg['title'], 'hash': h, 'bytes': len(data),
                        **{k: pkg['meta'][k] for k in ('icon', 'family', 'about') if k in pkg['meta']},
                        'built': commit})
    with open(os.path.join(out, 'catalog.json'), 'w', encoding='utf-8') as f:
        json.dump({'gas': 2, 'contracts': catalog}, f, ensure_ascii=False, indent=1)
    print(f'packages: {len(catalog)} into {out}')


if __name__ == '__main__':
    main()
