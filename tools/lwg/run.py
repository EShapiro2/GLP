"""Run the four modules: python3 run.py examples/amutot [--check], or on a bundle from the web page: python3 run.py --bundle my.json [--check]"""
import os, sys
from lwg import read, map_, review, build, record
ex = sys.argv[1]
if ex == '--bundle':
    from lwg import bundle
    ex = bundle.unpack(sys.argv[2], os.path.join(os.path.dirname(os.path.abspath(__file__)), 'examples', '_bundle'))
glp_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
import json
meta = json.load(open(os.path.join(ex, 'meta.json'), encoding='utf8'))
rows = read.run(os.path.join(ex, 'source.txt'), os.path.join(ex, 'kinds.json'), meta['format'])                # Read
m = map_.run(rows, os.path.join(ex, 'map.json'))                                               # Map
qs = review.run(rows, os.path.join(ex, 'answers.json'))                                        # Review
n = build.write(m, glp_root)                                                                   # Build
cl = build.clauses(m)
for r in rows: r['clause'] = {'built': m['name']}.get(r['status'], '')
print('%d provisions; %d questions; %d schemas written' % (len(rows), len(qs), n))
verdict = 'not checked'
if '--check' in sys.argv:
    verdict, lines = build.check(m, glp_root); print('\n'.join(lines[-6:]))
record.save(rows, os.path.join(ex, 'record.json'))
if '--check' in sys.argv: open(os.path.join(ex, 'verdict.txt'), 'w').write(verdict + '\n')
head = '\n'.join(['', '## Bylaws', '', map_.bylaws_table(m), '', '## Contract', ''] + ['    ' + l for l in cl] + ['', 'Check: `%s`' % verdict, ''])
open(os.path.join(ex, 'record.md'), 'w', encoding='utf8').write(record.markdown(rows, m['title']) + head)
open(os.path.join(ex, 'questions.md'), 'w', encoding='utf8').write(review.markdown(qs, m['title']))
