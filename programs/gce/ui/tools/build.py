"""The prototype's input for one contract, read from the Grassroots Contracts Calculus.

    python3 build.py CALC NAME OUT.json [GLP] [--changed=NAME:N ...]

CALC is a scratch copy of the Calculus's directory (calculus/ of GLP) with gr/ and rev/ made from
contracts/*/logic-pragmas.txt and contract.txt, as tools/SAS-WRITING.txt says, and tools/toglp.py run
for NAME, so that glp/NAME.glp.txt holds the contract's schemas.  GLP is the GLP checkout, for the
Interface pragmas of the contracts in programs/.

The output holds, for the page: the contract's text and the texts it is written from; the
vocabulary; and each schema of the Calculus: its persons, values, effects, preconditions and who must
agree to it, with the button, the question and the fields of the Interface pragmas.
"""
import json, os, re, sys

# ---------- a reader of the Calculus's terms ----------
TOK = re.compile(r"\s*(?:('(?:[^'\\]|\\.)*')|([A-Za-z_][\w]*)|(-?\d+)|([\[\](),]))")


def parse(s):
    pos = 0
    toks = []
    while pos < len(s):
        m = TOK.match(s, pos)
        if not m or m.end() == pos:
            if s[pos:].strip() == '':
                break
            raise ValueError('cannot read at ' + s[pos:pos + 40])
        pos = m.end()
        q, w, n, p = m.groups()
        toks.append(('q', q[1:-1]) if q else ('w', w) if w else ('n', int(n)) if n else ('p', p))
    i = 0

    def term():
        nonlocal i
        k, v = toks[i]
        i += 1
        if k == 'p' and v == '[':
            xs = []
            if toks[i] == ('p', ']'):
                i += 1
                return xs
            while True:
                xs.append(term())
                k2, v2 = toks[i]
                i += 1
                if v2 == ']':
                    return xs
        if k in ('w', 'q') and i < len(toks) and toks[i] == ('p', '('):
            i += 1
            args = []
            while True:
                args.append(term())
                k2, v2 = toks[i]
                i += 1
                if v2 == ')':
                    return {'f': v, 'a': args}
        if k == 'q':
            return v
        return {'w': v} if k == 'w' else v

    return term()


def atom(t):
    """atom(P, Args) -> {p, args, neg}; not(c) -> neg; forall(Vars, A, true, Body); or(...); exists(Vars, Body);
    and(...) -> a list of conditions, flattened by conds()."""
    f = t.get('f') if isinstance(t, dict) else None
    if f == 'not':
        a = atom(t['a'][0])
        if isinstance(a, dict) and 'p' in a:
            a['neg'] = True
            return a
        return {'not': a}
    if f == 'forall':
        vs, a, _, body = t['a']
        return {'forall': vs, 'over': atom(a), 'then': conds(body)}
    if f == 'exists':
        return {'exists': t['a'][0], 'body': conds(t['a'][1])}
    if f == 'or':
        return {'or': [conds(x) for x in t['a']]}
    if f == 'and':
        return {'and': conds(t)}
    p, args = t['a']
    return {'p': p['w'] if isinstance(p, dict) else p, 'args': [arg(x) for x in args], 'neg': False}


TYPES = {}


def arg(x):
    """A typed argument, const(V), num(V), set(V), text(V) or val(V), is its variable; the type is kept in TYPES.
    val(w) of a word is that word, as a value; val(f(...)) is the term f(...), {f, a}."""
    if isinstance(x, dict) and x.get('f') in ('const', 'num', 'set', 'text', 'val') and len(x['a']) == 1 and isinstance(x['a'][0], str):
        TYPES.setdefault(x['a'][0], x['f'])
        return x['a'][0]
    if isinstance(x, dict) and x.get('f') == 'val' and len(x['a']) == 1:
        v = x['a'][0]
        if isinstance(v, dict) and 'w' in v:
            return {'lit': v['w']}
        if isinstance(v, dict) and 'f' in v:
            return {'f': v['f'], 'a': [arg(y) for y in v['a']]}
    if isinstance(x, dict) and 'w' in x:
        return {'lit': x['w']}
    return x


def sizes(g, out):
    """The sizes the guard gives the sets: eq(size(S), num(N)) or eq(size(S), K)."""
    if isinstance(g, dict) and g.get('f') == 'and':
        for y in g['a']:
            sizes(y, out)
    elif isinstance(g, dict) and g.get('f') == 'eq':
        l, r = g['a']
        if isinstance(l, dict) and l.get('f') == 'size':
            v = r['a'][0] if isinstance(r, dict) and r.get('f') == 'num' else r
            out[l['a'][0]] = v
    return out


def conds(t):
    if isinstance(t, list):
        return [c for x in t for c in conds(x)]
    if isinstance(t, dict) and t.get('f') == 'and':
        return [c for x in t['a'] for c in conds(x)]
    if isinstance(t, dict) and t.get('w') == 'true':
        return []
    return [atom(t)]


def schemas(text):
    t = parse(text.strip())
    refl, env, param = [], [], []
    while isinstance(t, dict) and t.get('f') in ('environment', 'reflexive', 'parametric'):
        {'environment': env, 'reflexive': refl, 'parametric': param}[t['f']].extend(t['a'][0])
        t = t['a'][1]
    ss = t
    out = []
    for s in ss:
        name, persons, values, eff, pre, guard, by = s['a']
        name = name['w'] if isinstance(name, dict) else name
        ps, one = [], {}
        for p in persons:
            if isinstance(p, dict) and p.get('f') == 'one':
                one[p['a'][0]] = p['a'][1] if isinstance(p['a'][1], list) else [p['a'][1]]
            else:
                ps.append(p)
        TYPES.clear()
        eff_, pre_ = conds(eff), conds(pre)
        szs = sizes(guard, {})
        inatoms = set()
        def walk(cs):
            for c in cs:
                if 'p' in c:
                    for x in c['args']:
                        if isinstance(x, str): inatoms.add(x)
                        elif isinstance(x, dict) and 'a' in x: inatoms.update(y for y in x['a'] if isinstance(y, str))
                for k in ('then', 'body', 'and'):
                    if k in c: walk(c[k])
                if 'over' in c: walk([c['over']])
                if 'or' in c:
                    for y in c['or']: walk(y)
                if 'not' in c: walk([c['not']])
        walk(eff_); walk(pre_)
        derived = [v for v in values if isinstance(v, str) and v not in inatoms and v not in szs.values()]
        # a value the constraint defines by a declared function, Next by "Next is the position after Move on Board", is the machine's to compute
        def fdefs(g, out):
            if isinstance(g, dict):
                if g.get('f') == 'eq' and len(g.get('a', [])) == 2:
                    x, e = g['a']
                    if isinstance(e, dict) and e.get('f') == 'fn' and isinstance(x, dict) and x.get('f') in ('num', 'const') and isinstance(x['a'][0], str): out.add(x['a'][0])
                for y in g.get('a', []): fdefs(y, out) if isinstance(y, dict) else [fdefs(z, out) for z in y] if isinstance(y, list) else None
            return out
        derived += [v for v in sorted(fdefs(guard, set())) if v in values and v not in derived]
        out.append({'name': name, 'persons': ps, 'one': one, 'types': dict(TYPES), 'sizes': szs, 'guard': guard, 'derived': derived, 'values': [v for v in values if v not in one],
                    'effects': eff_, 'pre': pre_, 'by': by})
    w = lambda xs: [r['w'] if isinstance(r, dict) else r for r in xs]
    return w(refl), w(env), w(param), out


# ---------- the texts ----------
def parents(calc, name, names):
    l = open(os.path.join(calc, 'gr', name + '.txt')).read().split('\n')[1]
    l = re.sub(r'\(.*?\)', '', l)
    rest = l.split(' ', 1)[1] if ' ' in l else ''
    out = []
    for d, n in sorted(names.items(), key=lambda x: -len(x[0])):
        if n != name and re.search(r'\b' + re.escape(d) + r'\b', rest):
            out.append(n)
            rest = re.sub(r'\b' + re.escape(d) + r'\b', '', rest)
    return out


def chain(calc, name, names):
    seen, order = set(), []

    def go(n):
        if n in seen:
            return
        seen.add(n)
        for p in parents(calc, n, names):
            go(p)
        order.append(n)
    go(name)
    return order


def vocab(gr):
    v = {}
    for l in gr:
        if l.startswith('Vocabulary:'):
            for part in l[len('Vocabulary:'):].strip().rstrip('.').split(';'):
                if '=' in part:
                    ph, at = [x.strip() for x in part.rsplit('=', 1)]
                    m = re.match(r'(\w+)\((.*)\)$', at)
                    if m and m.group(1) not in v:
                        # arguments split at the commas outside parentheses; a set's *n is named n
                        args, depth, cur = [], 0, ''
                        for ch in m.group(2):
                            if ch == ',' and depth == 0:
                                args.append(cur); cur = ''
                                continue
                            depth += (ch == '(') - (ch == ')')
                            cur += ch
                        args.append(cur)
                        v[m.group(1)] = {'phrase': re.sub(r', for \w+$', '', ph), 'vars': [x.strip().lstrip('*') for x in args]}
    return v


def clauses(lines):
    """Act title -> its sentence, from the numbered clauses."""
    out = {}
    for l in lines:
        m = re.match(r'^\d+\. [■◆○] (.*)$', l)
        if not m:
            continue
        body = m.group(1)
        parts = re.split(r'(?:^|(?<=\. ))([A-Z][A-Za-z ]*?)[:.] ', body)
        for k in range(1, len(parts) - 1, 2):
            out[parts[k].strip()] = parts[k + 1].strip()
    return out


def interface(path):
    """Button -> act line facts, questions; tab lines."""
    if not path or not os.path.exists(path):
        return {}, {}
    lines = open(path).read().split('\n')
    try:
        i = lines.index('Interface pragmas')
    except ValueError:
        return {}, {}
    acts, tabs, last = {}, {}, None
    for l in lines[i + 1:]:
        if not l.strip() or l.startswith('%'):
            continue
        q = re.match(r'^\s+([A-Z]\w*): (.*)$', l)
        if q and last and not re.search(r'\[', q.group(2)):
            for b in last:
                acts[b].setdefault('questions', {})[q.group(1)] = q.group(2)
            continue
        body = l.split(': ', 1)[1] if ': ' in l else l
        bs = re.findall(r'\[([^\]]+)\]', body)
        if bs:
            fields = re.findall(r'<([^>]+)>', body)
            last = bs
            for b in bs:
                after = body.split('[' + b + ']', 1)[1]
                f = re.match(r'\s*<([^>]+)>', after)
                acts[b] = {'field': f.group(1) if f else None}
        elif ':' in l:
            tabs[l.split(':')[0].strip()] = l.split(':', 1)[1].strip().rstrip('.')
    return acts, tabs


def main():
    calc, name, out = sys.argv[1:4]
    glp = sys.argv[4] if len(sys.argv) > 4 and not sys.argv[4].startswith('--') else '/home/claude/GLP'
    changed = [a.split('=', 1)[1] for a in sys.argv if a.startswith('--changed=')]
    names = {}
    for l in open(os.path.join(calc, 'NAMES.txt')):
        cols = [c.strip() for c in l.split('|')]
        if len(cols) == 5 and cols[0] and cols[0] != 'name on main':
            names[cols[1]] = cols[2]
    order = chain(calc, name, names)
    refl, envs, param, ss = schemas(open(os.path.join(calc, 'glp', name + '.glp.txt')).read())
    voc, sent, texts, iface, tabs, fns = {}, {}, [], {}, {}, []
    for n in order:
        gr = open(os.path.join(calc, 'gr', n + '.txt')).read().split('\n')
        rev = open(os.path.join(calc, 'rev', n + '.txt')).read().rstrip('\n').split('\n')
        # a predicate's first phrase in the chain is its most general; a later contract may phrase a special case
        for k, x in vocab(gr).items():
            voc.setdefault(k, x)
        # a declared function, 'Functions: v is the legality of m on b.', holds of values and of no person: of the environment
        for l in gr:
            if l.startswith('Functions: '):
                for f in l[len('Functions: '):].rstrip('.').split('; '):
                    for k, x in vocab(gr).items():
                        if x['phrase'] == f.strip() and k not in fns: fns.append(k)
        for t, s in clauses(gr).items():
            m = re.match(r'^(\w[\w ]*):(\d+)', s)
            s = re.split(r', (?:provided|only if|with the consent of)', s)[0].rstrip('.')
            sent[t] = sent.get(t, s) if m else s
        texts.append({'contract': n, 'text': rev, 'changed': [c.split(':', 1)[1] for c in changed if c.split(':', 1)[0] == n]})
        prog = None
        for root, _, fs in os.walk(os.path.join(glp, 'programs')):
            if n + '.txt' in fs and '/tests' not in root:
                prog = os.path.join(root, n + '.txt')
                break
        a, t = interface(prog)
        iface.update(a)
        tabs.update(t)
    for s in ss:
        title = s['name'].replace('_', ' ').capitalize()
        s['button'] = title
        s['sentence'] = sent.get(title, title)
        s['auto'] = not s['by']
        i = iface.get(title, {})
        s['questions'] = i.get('questions', {})
        s['field'] = i.get('field')
    json.dump({'contract': name, 'chain': texts, 'vocab': voc, 'reflexive': refl, 'schemas': ss, 'tabs': tabs,
               'env': (envs or ['today']) + fns, 'parametric': param}, open(out, 'w'), indent=1, ensure_ascii=False)
    print(name, len(ss), 'schemas;', 'chain', order)


if __name__ == '__main__':
    main()
