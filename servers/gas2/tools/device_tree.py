"""The GLP tree a GAS2 device runs: the part of a built working copy of GLP that the
device's super-app loads, as glp_multiagent/tool/sync_glp_assets.sh stages it for
GLP's own app, with GAS2's mini-apps and its device harness.

    python3 -I tools/device_tree.py <built GLP working copy> <output directory> [<data.json>]

The output holds programs/: the root self.glp, every self.glp between the root and
programs/social/graph, every .glp of that tree and every .vglp no .glp stands for,
vglp/dispatcher.glp, each of GAS2's mini-apps (programs/apps/gas2_*), and
gas2_apps.json, which names them.  core/self.glp gets the three lines that start the
device harness (miniapp.patch_device)."""
import json
import os
import shutil
import sys

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from glp import Glp  # noqa: E402
import miniapp as M  # noqa: E402

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TREES = ['social/graph']
# and, below, the tree of each directory the root exposes a module of (-expose)


def stage(src, dst, rel):
    os.makedirs(os.path.dirname(os.path.join(dst, rel)), exist_ok=True)
    shutil.copy(os.path.join(src, rel), os.path.join(dst, rel))


def device_tree(root, out, apps=None):
    """apps: [(app, id, title)]; by default every programs/apps/gas2_* with a
    shapes.json, its id and title read from data.json beside, if given."""
    src = os.path.join(root, 'programs')
    dst = os.path.join(out, 'programs')
    shutil.rmtree(out, ignore_errors=True)
    os.makedirs(dst)
    stage(src, dst, 'self.glp')
    import re
    trees = list(TREES)
    for m in re.findall(r'^-expose\(([a-z_0-9#]+)\)\.', open(os.path.join(src, 'self.glp'), encoding='utf-8').read(), re.M):
        top = m.split('#')[0]
        if not any(t == top or t.startswith(top + '/') for t in trees) and os.path.isdir(os.path.join(src, top)):
            trees.append(top)
    for t in trees:
        d = os.path.dirname(t)
        while d:
            if os.path.exists(os.path.join(src, d, 'self.glp')):
                stage(src, dst, os.path.join(d, 'self.glp'))
            d = os.path.dirname(d)
        for dp, _, fs in os.walk(os.path.join(src, t)):
            for f in sorted(fs):
                rel = os.path.relpath(os.path.join(dp, f), src)
                if f.endswith('.glp') or (f.endswith('.vglp') and not os.path.exists(os.path.join(dp, f[:-5] + '.glp'))):
                    stage(src, dst, rel)
    stage(src, dst, os.path.join('vglp', 'dispatcher.glp'))
    if apps is None:
        apps = [(a, a[5:].upper(), a[5:].upper()) for a in sorted(os.listdir(os.path.join(src, 'apps')))
                if a.startswith('gas2_') and os.path.isdir(os.path.join(src, 'apps', a))]
    index = []
    for app, cid, title in apps:
        a = os.path.join(src, 'apps', app)
        if not os.path.exists(os.path.join(a, 'shapes.json')):
            continue
        shutil.copytree(a, os.path.join(dst, 'apps', app))
        shapes = json.load(open(os.path.join(a, 'shapes.json'), encoding='utf-8'))
        index.append({'app': app, 'id': cid, 'title': title, 'shapes': shapes})
    with open(os.path.join(dst, 'gas2_apps.json'), 'w', encoding='utf-8') as f:
        json.dump(index, f, indent=1)
    M.patch_device(Glp(out), os.path.join(HERE, 'glp', 'gas2_device.glp'))
    return dst


def apps_of(data_json):
    """The mini-apps a GAS2 build made, [(app, id, title)], from its data.json."""
    d = json.load(open(data_json, encoding='utf-8'))
    return [(c['app'], c['id'], c['title']) for c in d['contracts'] if c.get('artefact')]


if __name__ == '__main__':
    # device_tree.py <built GLP working copy> <output> [<the build's data.json>]
    print(device_tree(sys.argv[1], sys.argv[2], apps_of(sys.argv[3]) if len(sys.argv) > 3 else None))
