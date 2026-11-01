"""Read a platform.vglp's Proposal type and its social_menu widget declaration into JSON."""
import re, json, sys

class P:
    def __init__(s, t): s.t, s.i = t, 0
    def ws(s):
        while s.i < len(s.t) and s.t[s.i] in ' \n\t': s.i += 1
    def term(s):
        s.ws(); c = s.t[s.i]
        if c == '[': return s.lst()
        if c == '"':
            j = s.t.index('"', s.i + 1); a = s.t[s.i + 1:j]; s.i = j + 1; return {'s': a}
        if c == "'":
            j = s.t.index("'", s.i + 1); a = s.t[s.i + 1:j]; s.i = j + 1
        else:
            m = re.compile(r"-?[A-Za-z0-9_]+").match(s.t, s.i); a = m.group(0); s.i = m.end()
            if re.fullmatch(r'-?\d+', a): a = int(a)
        s.ws()
        if s.i < len(s.t) and s.t[s.i] == '(':
            s.i += 1; args = []
            while True:
                args.append(s.term()); s.ws()
                if s.t[s.i] == ',': s.i += 1; continue
                if s.t[s.i] == ')': s.i += 1; break
            return {'f': a, 'a': args}
        return a
    def lst(s):
        s.i += 1; out = []; s.ws()
        if s.t[s.i] == ']': s.i += 1; return out
        while True:
            out.append(s.term()); s.ws()
            if s.t[s.i] == ',': s.i += 1; continue
            if s.t[s.i] == ']': s.i += 1; return out

def spec(path):
    src = open(path, encoding='utf-8').read()
    m = re.search(r'^Proposal ::=(.*?)\.\s*$', src, re.M | re.S)
    props = {}
    for alt in m.group(1).split(';'):
        alt = alt.strip()
        mm = re.match(r'(\w+)(?:\((.*)\))?$', alt, re.S)
        args = []
        if mm.group(2):
            depth, buf = 0, ''
            for ch in mm.group(2):
                if ch == '(': depth += 1
                if ch == ')': depth -= 1
                if ch == ',' and depth == 0: args.append(buf.strip()); buf = ''; continue
                buf += ch
            args.append(buf.strip())
        props[mm.group(1)] = args
    mm = re.search(r'^Menu =::= social_menu\((.*)\)\s*\.\s*$', src, re.M | re.S)
    items = P(mm.group(1)).term() if mm else []
    labels, fields, acts, meeting, texts = {}, {}, {}, {}, {}
    for it in items:
        f, a = it['f'], it['a']
        if f == 'label' and len(a) == 2: labels[a[0]] = a[1]['s']
        elif f == 'label' and len(a) == 3: fields.setdefault(a[0], {})[a[1]] = a[2]['s']
        elif f == 'act':
            name, tab, me, ps, gs, its = a
            acts[name] = {'tab': tab['s'], 'me': me, 'persons': ps,
                          'groups': [[g['a'][0], g['a'][1]] for g in gs if isinstance(g, dict) and g['f'] == 'group'],
                          'items': [[x['a'][0], x['a'][1]] for x in its if isinstance(x, dict) and x['f'] == 'item'],
                          'picks': [[x['a'][0], x['a'][1]] for x in its if isinstance(x, dict) and x['f'] == 'pick'],
                          'req': [[x['a'][0], x['a'][1]] for x in its if isinstance(x, dict) and x['f'] in ('req',)],
                          'forb': [[x['a'][0], x['a'][1]] for x in its if isinstance(x, dict) and x['f'] in ('forb',)]}
        elif f == 'meeting': meeting[a[0]] = a[1]
        elif f == 'text': texts[a[0]] = a[1]
    out = []
    for name, args in props.items():
        a = acts.get(name)
        out.append({'name': name, 'args': args, 'label': labels.get(name, name.replace('_', ' ').capitalize()),
                    'fields': {str(k): v for k, v in fields.get(name, {}).items()},
                    'act': a, 'meeting': meeting.get(name)})
    return {'proposals': out, 'raw_items': [i['f'] for i in items]}

if __name__ == '__main__':
    s = spec(sys.argv[1])
    for p in s['proposals']: print(p['name'], p['args'], p['label'], p['fields'], json.dumps(p['act'])[:200], p['meeting'])
    print(set(s['raw_items']))
