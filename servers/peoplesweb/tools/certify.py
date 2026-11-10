"""What the site's build asks of GLP itself, through the REPL (bin/glpc):

- relations(): each contract's stated relation to the contracts it names,
  checked against their schemas by the definitions of GSC's Relations among
  Contracts, sec:relations (Amendment, extension, instantiation;
  Implementation, restriction): an extension keeps
  every schema of what it extends; an amendment deletes and adds; a
  restriction is decided by GSC's checker, restricts/3 of programs/gsc, by the
  syntactic conditions of Proposition Syntactic restrictions; an
  instantiation needs a parametric contract, and no contract declares
  parameters yet; an implementation is a relation between runs, which no
  check of the text decides.
- artefacts(): each app of programs/apps as a certified artefact, the REPL's
  ':artefact <app> <dir>', the .glpw the Grassroots App installs, certified
  under the key of the machine that builds the site.

Both are cached in servers/peoplesweb/cache (CERTIFY_CACHE), gitignored, by the git hashes of what they read, so a
sweep that changes nothing asks the REPL nothing.  The schemas are compared
as the REPL prints the contract's term, contract_named/2 of its NAME.glp,
each schema with its variables and roles renamed in order of first
occurrence, so that two contracts writing one schema with other names compare
equal (after Nimrod Talmon's normal/1, GLP pull request 9, servers/gas2).
"""
import hashlib
import json
import os
import re
import subprocess

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GLP = os.environ.get('GLP_ROOT') or os.path.abspath(os.path.join(HERE, '..', '..'))
CACHE = os.environ.get('CERTIFY_CACHE') or os.path.join(HERE, 'cache')


def _git(*a):
    return subprocess.run(['git', '-C', GLP, *a], capture_output=True, text=True).stdout.strip()


def _hash(*paths):
    """The git hashes of files and directories as they stand in the work tree."""
    h = hashlib.sha256()
    for p in paths:
        r = os.path.relpath(p, GLP)
        h.update((_git('hash-object', p) if os.path.isfile(p) else _git('ls-files', '-s', r)).encode())
    return h.hexdigest()[:20]


def repl(lines, timeout=900):
    try:
        p = subprocess.run([os.path.join(GLP, 'glp_runtime', 'bin', 'glpc')], input='\n'.join(lines + [':quit']) + '\n',
                           capture_output=True, text=True, errors='replace', cwd=os.path.join(GLP, 'glp_runtime'), timeout=timeout)
        return p.stdout
    except Exception as e:  # no Dart, a timeout: nothing is checked, the site still builds
        return f'[certify] {e}'


def answer(out, var):
    m = re.search(r'^(?:GLP> )?' + re.escape(var) + r' = (.*)$', out, re.M)
    return m.group(1).strip() if m else None


def _load(name):
    try:
        with open(os.path.join(CACHE, name)) as f:
            return json.load(f)
    except (OSError, ValueError):
        return {}


def _save(name, d):
    os.makedirs(CACHE, exist_ok=True)
    with open(os.path.join(CACHE, name + '.tmp'), 'w') as f:
        json.dump(d, f)
    os.replace(os.path.join(CACHE, name + '.tmp'), os.path.join(CACHE, name))


# --- the contracts' terms and their schemas ---------------------------------

def glp_of(txt):
    """NAME.glp beside NAME.txt, and the name its contract_named/2 gives."""
    p = txt[:-4] + '.glp'
    if not os.path.exists(p):
        return None, None
    with open(p, encoding='utf-8') as f:
        m = re.search(r'^contract_named\(([a-z_0-9]+),', f.read(), re.M)
    return (p, m.group(1)) if m else (p, None)


def split_top(s):
    out, d, q, buf = [], 0, False, ''
    for ch in s:
        if ch == "'":
            q = not q
        if not q:
            if ch in '([':
                d += 1
            elif ch in ')]':
                d -= 1
            elif ch == ',' and d == 0:
                out.append(buf.strip())
                buf = ''
                continue
        buf += ch
    if buf.strip():
        out.append(buf.strip())
    return out


def normal(schema):
    """A schema with its variables and its roles renamed in order of first occurrence."""
    names = {}

    def ren(m):
        k = (m.group(1), m.group(2).strip("'"))
        names.setdefault(k, len(names))
        return f"{m.group(1)}(v{names[k]})"
    s = re.sub(r"\b(pvar|svar|own_var|nvar|var)\(('[^']*'|[A-Za-z_0-9]+)\)", ren, schema)
    m = re.match(r"^([a-z]+\([a-z_0-9]+, )\[([^\]]*)\]", s)
    if m:
        n = len(split_top(m.group(2)))
        s = m.group(1) + '[' + ', '.join(f'r{i}' for i in range(n)) + ']' + s[m.end():]
    return s


def schemas_of(term):
    """The schemas of a contract's term as GSC reads it, name -> normal form:
    the list of schemas, or a structure, environment(Ps, C) or reflexive(Ps,
    C), holding it as its last argument."""
    body = term.strip()
    while re.match(r'^[a-z_]+\(', body) and body.endswith(')'):
        body = split_top(body[body.index('(') + 1:-1])[-1]
    if not (body.startswith('[') and body.endswith(']')):
        return {}
    out = {}
    for s in split_top(body[1:-1]):
        n = re.match(r"[a-z_]+\(([a-z_0-9]+)", s)
        if n:
            out[n.group(1)] = normal(s)
    return out


def terms(contracts):
    """Each contract's term, by its id, and the same as GSC's checker reads
    it, as_read/2, one form for the clause syntax's sas/7 and the checker's
    schema/4, to compare; cached by the hashes of its NAME.glp and of
    programs/gsc."""
    cache, out, read, changed = _load('terms.json'), {}, {}, False
    gsc = _hash(os.path.join(GLP, 'programs', 'gsc'))
    for c in contracts:
        p, name = glp_of(c['path'])
        if not p or not name:
            continue
        h = _hash(p) + gsc
        e = cache.get(c['id'], {})
        if e.get('hash') != h or (not e.get('term') and 'error' not in e):
            o = repl([f'load {os.path.relpath(p, os.path.join(GLP, "glp_runtime"))}', f'contract_named({name}, C).'])
            t = answer(o, 'C')
            r = answer(repl(['../programs/gsc', ':limit 10000000', f'as_read({t}, R).']), 'R') if t else None
            err = None if t else next((l.split(': ', 1)[-1].strip() for l in o.splitlines() if 'Error' in l), 'no term')
            cache[c['id']] = {'hash': h, 'term': t, 'read': r, 'error': err}
            changed = True
        if cache[c['id']]['term']:
            out[c['id']] = cache[c['id']]['term']
        if cache[c['id']].get('read'):
            read[c['id']] = cache[c['id']]['read']
    if changed:
        _save('terms.json', cache)
    errors = {c['id']: cache[c['id']]['error'] for c in contracts if c['id'] in cache and cache[c['id']].get('error')}
    return out, read, errors


# --- the relations ---------------------------------------------------------

# The checker's reasons, RFault of programs/gsc/self.glp, in words.
FAULTS = {
    'not_sg': lambda a: f'{a[0]} is not syntactically grassroots ({a[1]})',
    'preds_not_in_base': lambda a: f'it uses predicates its base has not: {a[0].strip("[]")}',
    'body_not_stronger': lambda a: f'the body of {a[0]} is not shown stronger than its base\'s',
    'guards_differ': lambda a: f'{a[0]} is guarded otherwise than in its base',
    'roles_differ': lambda a: f'{a[0]} has other roles than in its base',
    'added_guarded': lambda a: f'it adds {a[0]}, which a person must will, where only acts no person wills may be added',
    'no_schema_of': lambda a: f'it adds {a[0]}, which has the head and roles of no schema of its base',
    'deleted_unguarded': lambda a: f'it deletes {a[0]}, which no person wills, and an act owed may go with it',
    'step_not_sg': lambda a: f'a step, {a[0]}, leaves a contract that is not syntactically grassroots',
    'no_simple_unguarded': lambda a: f'{a[0]} does not have simple unguarded classes',
}


def faults_in_words(v):
    """not_shown([F, ...]) in words, one per fault, each in the checker's
    term where it has no words here."""
    inner = v[v.index('(') + 1:-1].strip()
    out = []
    for f in split_top(inner[1:-1] if inner.startswith('[') else inner):
        m = re.match(r'^([a-z_]+)\((.*)\)$', f)
        if m and m.group(1) in FAULTS:
            try:
                out.append(FAULTS[m.group(1)](split_top(m.group(2))))
                continue
            except Exception:
                pass
        out.append(f)
    return '; '.join(out)


def compare(sub, sup):
    """What sub does to sup's schemas: those it deletes and those it writes otherwise."""
    gone = [k for k in sup if k not in sub]
    changed = [k for k in sup if k in sub and sub[k] != sup[k]]
    return gone, changed


def relations(contracts):
    """For each contract, each relation its box states, checked: a list of
    {base, rel, result, why}, result one of agrees, given, disagrees, open."""
    T, R, E = terms(contracts)
    P = {c['id']: c['path'] for c in contracts}

    def missing(i):
        if i in E:
            return f'{os.path.basename(P[i])[:-4]}.glp does not load: {E[i][:200]}'
        return f'{i} has no speech-act schemas ({i}.glp)'
    S = {k: schemas_of(v) for k, v in R.items()}
    gsc = _hash(os.path.join(GLP, 'programs', 'gsc'))
    cache, changed = _load('restricts.json'), False
    out = {}
    for c in contracts:
        rs = []
        for base, rel in c.get('rels', {}).items():
            r = {'base': base, 'rel': rel}
            if rel in ('extends', 'amends'):
                if c['id'] not in S or base not in S:
                    r['result'], r['why'] = 'open', 'not checked: ' + missing(c['id'] if c['id'] not in S else base)
                else:
                    gone, chg = compare(S[c['id']], S[base])
                    if rel == 'extends':
                        if gone or chg:
                            r['result'] = 'disagrees'
                            r['why'] = 'it does not keep every schema of ' + base + ': ' + '; '.join(x for x in [
                                ('deletes ' + ', '.join(gone)) if gone else '', ('writes otherwise ' + ', '.join(chg)) if chg else ''] if x) + \
                                ' (GSC, Definition Amendment, extension: an extension deletes nothing)'
                        else:
                            r['result'], r['why'] = 'agrees', 'it keeps every schema of ' + base
                    else:
                        added = [k for k in S[c['id']] if k not in S[base]]
                        r['result'] = 'agrees'
                        r['why'] = '; '.join(x for x in [('deletes ' + ', '.join(gone)) if gone else '',
                                                         ('writes otherwise ' + ', '.join(chg)) if chg else '',
                                                         ('adds ' + ', '.join(added)) if added else ''] if x)
                        if not gone and not chg:
                            r['why'] = f'it deletes nothing of {base} and writes none of its schemas otherwise, so it extends it too'
            elif rel == 'restricts':
                if c['id'] not in T or base not in T:
                    r['result'], r['why'] = 'open', 'not checked: ' + missing(c['id'] if c['id'] not in T else base)
                else:
                    key = f"{c['id']}|{base}|{_hash(glp_of(c['path'])[0])}|{gsc}"
                    if key not in cache:
                        v = answer(repl(['../programs/gsc', ':limit 10000000', f"restricts({T[c['id']]}, {T[base]}, V)."]), 'V')
                        cache = {k: x for k, x in cache.items() if not k.startswith(f"{c['id']}|{base}|")}
                        cache[key] = v
                        changed = True
                    v = cache[key] or ''
                    if v.startswith('restriction_given_invariant'):
                        last = split_top(v[v.index('(') + 1:-1])[-1]
                        r['result'], r['why'] = 'given', f"GSC's checker: a restriction given an invariant of the contract for {last.strip('[]')} (Proposition Syntactic restrictions, (c))"
                    elif v.startswith('restriction'):
                        r['result'], r['why'] = 'agrees', "GSC's checker: a restriction by Proposition Syntactic restrictions"
                    elif v:
                        r['result'], r['why'] = 'disagrees', "GSC's checker shows no restriction: " + (faults_in_words(v) if v.startswith('not_shown(') else v)[:700] + ' (Proposition Syntactic restrictions)'
                    else:
                        r['result'], r['why'] = 'open', "GSC's checker gave no verdict"
            elif rel == 'instantiates':
                r['result'], r['why'] = 'open', 'no contract declares parameters yet, so no instantiation can be checked (GSC, Definition Parametric contract)'
            else:
                r['result'], r['why'] = 'open', 'an implementation is a relation between runs; no check of the text decides it (GSC, Definition Implementation)'
            rs.append(r)
        if rs:
            out[c['id']] = rs
    if changed:
        _save('restricts.json', cache)
    return out


# --- the apps as certified artefacts ------------------------------------------

def artefacts(dest):
    """Each app of programs/apps certified into dest: app -> {file, key} or {why}."""
    apps = os.path.join(GLP, 'programs', 'apps')
    os.makedirs(dest, exist_ok=True)
    store = os.path.join(CACHE, 'artefacts')
    os.makedirs(store, exist_ok=True)
    cache, changed, out = _load('artefacts.json'), False, {}
    for a in sorted(os.listdir(apps)) if os.path.isdir(apps) else []:
        d = os.path.join(apps, a)
        if not os.path.isdir(d) or not os.path.exists(os.path.join(d, 'self.glp')):
            continue
        h = _hash(d)
        e = cache.get(a, {})
        if e.get('hash') != h or ('file' in e and not os.path.exists(os.path.join(store, a + '.glpw'))):
            o = repl([f':artefact {os.path.relpath(d, os.path.join(GLP, "glp_runtime"))} {store}'])
            m = re.search(r'Wrote .*?' + re.escape(a) + r'\.glpw --- certified under ([0-9a-f]+)', o)
            e = {'hash': h, 'file': f'{a}.glpw', 'key': m.group(1)} if m else \
                {'hash': h, 'why': '\n'.join(l for l in o.splitlines() if l.strip() and not l.startswith(('GLP> Goodbye', '╔', '║', '╚', 'Build', 'Working', 'Input', 'Commands', 'Loaded root')))[-800:]}
            cache[a] = e
            changed = True
        if 'file' in e:
            import shutil
            shutil.copy(os.path.join(store, e['file']), os.path.join(dest, e['file']))
        out[a] = {k: v for k, v in e.items() if k != 'hash'}
    if changed:
        _save('artefacts.json', cache)
    return out
