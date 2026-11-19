"""Read: split the document into provisions, and attach to each the kind an LLM gave it (kinds.json).

Kinds: Ostrom's seven types of rules (position, boundary, choice, aggregation, information, payoff, scope),
and none (no rule), outside (a reference outside the document), judgment (left to a person's judgment)."""
import json, re
KINDS = ['position', 'boundary', 'choice', 'aggregation', 'information', 'payoff', 'scope', 'none', 'outside', 'judgment']
HEB = {'א': 'a', 'ב': 'b', 'ג': 'c', 'ד': 'd', 'ה': 'e'}

def clean(s):
    s = re.sub(r'\{\{ח:פנימי\|[^|}]*\|([^}]*)\}\}', r'\1', s)
    s = re.sub(r'\{\{ח:חיצוני\|[^|}]*\|([^}]*)\}\}', r'\1', s)
    s = re.sub(r'\{\{[^}]*\}\}', ' ', s)
    return re.sub(r'\s+', ' ', s).strip()

def wikisource_statute(text):
    """Provisions of a statute in the Hebrew Wikisource markup: one for each section, or for each sub-section where a section has them."""
    rows, sec, title = [], None, ''
    for line in text.split('\n'):
        m = re.match(r'\{\{ח:סעיף\*?\|(\d+)\|([^|}]*)', line)
        if m: sec, title = m.group(1), m.group(2); continue
        if sec is None: continue
        m = re.match(r'\{\{ח:תת\|\((.)\)\}\}\s*(.*)', line)
        if m: rows.append({'id': '%s(%s)' % (sec, HEB.get(m.group(1), m.group(1))), 'text': '%s: %s' % (title, clean(m.group(2)))}); continue
        m = re.match(r'\{\{ח:ת\}\}\s*(.*)', line)
        if m: rows.append({'id': sec, 'text': '%s: %s' % (title, clean(m.group(1)))}); continue
        m = re.match(r'\{\{ח:תתת(?:\|\((\d+)\))?\}\}\s*(.*)', line)
        if m and rows: rows[-1]['text'] += ' (%s) %s' % (m.group(1) or '', clean(m.group(2)))
    return rows

def headed_articles(text):
    """Provisions of a document whose articles are headed by Roman numerals: the preamble, each article, and each article's examples."""
    rows, cur = [], {'id': '0', 'text': ''}
    for line in text.split('\n'):
        m = re.match(r'^([IVX]+)\. (.*)', line)
        if m:
            rows.append(cur); cur = {'id': m.group(1), 'text': m.group(2) + ':'}; continue
        if line.strip() == 'Examples' and cur['id'] != '0':
            rows.append(cur); cur = {'id': cur['id'] + ' ex', 'text': 'Examples:'}; continue
        if line.strip(): cur['text'] += ' ' + line.strip()
    rows.append(cur)
    for r in rows: r['text'] = r['text'].strip()
    return rows

def provisions(text):
    """Provisions already split (a bundle from the web page): a JSON list of {id, text}."""
    return [{'id': str(r['id']), 'text': r['text']} for r in json.loads(text)]

FORMATS = {'provisions': provisions, 'wikisource_statute': wikisource_statute, 'headed_articles': headed_articles}

def run(source_path, kinds_path, fmt='wikisource_statute'):
    rows = FORMATS[fmt](open(source_path, encoding='utf8').read())
    kinds = json.load(open(kinds_path, encoding='utf8'))
    missing = [r['id'] for r in rows if r['id'] not in kinds]
    assert not missing, 'no kind for provisions %s' % missing
    assert not set(kinds) - {r['id'] for r in rows}, 'kinds for unknown provisions'
    for r in rows:
        k = kinds[r['id']]
        assert k['kind'] in KINDS, k
        r['kind'], r['note'] = k['kind'], k.get('note', '')
    return rows
