"""The record: one row for each provision of the document; each module fills its own columns."""
import json
COLUMNS = ['id', 'text', 'kind', 'note', 'mapped_to', 'question', 'answer', 'clause', 'status']
def load(path): return json.load(open(path, encoding='utf8'))
def save(rows, path): json.dump(rows, open(path, 'w', encoding='utf8'), ensure_ascii=False, indent=1)
def markdown(rows, title):
    def cell(v): return (v or '').replace('\n', ' ').replace('|', '/')
    out = ['# Record: %s' % title, '', '| # | Provision | Kind | Became | Question | Answer | Clause | Status |', '|---|---|---|---|---|---|---|---|']
    for r in rows:
        t = r['text'] if len(r['text']) < 140 else r['text'][:137] + '...'
        out.append('| %s | %s | %s | %s | %s | %s | %s | %s |' % tuple(cell(v) for v in (r['id'], t, r.get('kind'), r.get('mapped_to'), r.get('question'), r.get('answer'), r.get('clause'), r.get('status'))))
    return '\n'.join(out) + '\n'
