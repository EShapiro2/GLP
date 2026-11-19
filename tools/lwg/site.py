"""Write site/index.html: one page that shows, for each example, the record, the bylaws, the contract, and lets a visitor try a decision."""
import json, os
from lwg import build, rules, map_
HERE = os.path.dirname(os.path.abspath(__file__))
EXAMPLES = [('amutot', 'Israeli association'), ('ens', 'ENS DAO'), ('unicorn', 'Worker co-op (excerpt)'), ('moloch', 'Moloch DAO')]

def load(ex, label):
    d = os.path.join(HERE, 'examples', ex)
    j = lambda f: json.load(open(os.path.join(d, f), encoding='utf8'))
    m, meta, rows = map_.compose(j('map.json')), j('meta.json'), j('record.json')
    rl = {}
    for k, r in m['rules'].items(): rl[k] = dict(r, english=rules.describe(r, m['roles']), body_name='share' if r.get('weight') == 'shares' else m['roles'][str(r['policy'][1] if 'policy' in r else r['body'])], by=map_.by(m, r))
    acts = [{'act': 'every other act' if a == '*' else m.get('act_names', {}).get(a, a), 'rule': r} for a, r in m['acts'].items()]
    acts += [{'act': s['clause'].split(':')[0].lower(), 'rule': s['rule']} for s in m.get('also', {}).values()]
    v = open(os.path.join(d, 'verdict.txt')).read()
    return {'key': ex, 'label': label, 'title': m['title'], 'source': m['source'], 'link': meta['link'], 'lang': meta['language'], 'base': m['base'],
            'rows': rows, 'rules': rl, 'acts': acts, 'contract': build.clauses(m), 'passed': 'syntactically_grassroots' in v, 'name': m['name']}

data = [load(e, l) for e, l in EXAMPLES]
html = open(os.path.join(HERE, 'site_template.html'), encoding='utf8').read().replace('/*DATA*/', json.dumps(data, ensure_ascii=False)).replace('/*PROMPT*/', json.dumps(open(os.path.join(HERE, 'prompt.md'), encoding='utf8').read(), ensure_ascii=False))
os.makedirs(os.path.join(HERE, 'site'), exist_ok=True)
open(os.path.join(HERE, 'site', 'index.html'), 'w', encoding='utf8').write(html)
print('site/index.html', len(html))
