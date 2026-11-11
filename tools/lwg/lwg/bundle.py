"""A bundle: one JSON file from the web page, holding a document's provisions, kinds, bylaws and answers. Unpack it into the files the modules read."""
import json, os
NAME = 'LWGX'      # every bundle builds the one trial contract LWGX

def unpack(path, out, name=NAME):
    b = json.load(open(path, encoding='utf8'))
    os.makedirs(out, exist_ok=True)
    w = lambda f, x: json.dump(x, open(os.path.join(out, f), 'w', encoding='utf8'), ensure_ascii=False, indent=1)
    ps = b['provisions']
    open(os.path.join(out, 'source.txt'), 'w', encoding='utf8').write(json.dumps([{'id': p['id'], 'text': p['text']} for p in ps], ensure_ascii=False, indent=1))
    w('meta.json', {'format': 'provisions', 'language': b.get('language', 'en'), 'link': b.get('link', '')})
    w('kinds.json', {str(p['id']): {'kind': p['kind'], 'note': p.get('note', '')} for p in ps})
    m = {k: b[k] for k in ('title', 'source', 'base', 'roles', 'rules', 'acts') }
    m.update(name=name, shares=b.get('shares', {}), undertakings=b.get('undertakings', []),
             rows={str(p['id']): {'mapped_to': p['mapped_to'], 'status': p['status'], 'question': p.get('question', '')} for p in ps})
    w('map.json', m); w('answers.json', b.get('answers', {}))
    return out
