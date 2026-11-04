#!/usr/bin/env python3
"""Read speech-act schemas written in GSC's notation, as programs/gsc's
notation_contract/1 prints them (programs/gsc/README.md, "The notation"),
into the term form programs/gsc reads ("Writing a contract"), for GAS's app
maker.

The notation names a schema's variables without their kind; the term form
gives each one: pvar (an agent), svar (a speech act) or num (a value).  The
kind is read off the variable's use where the use decides it (a value in a
constraint or a multiplicity, a speech act in a name term or a signed term)
and off the argument places it fills, an argument place taking its kind from
every contract GAS knows (kinds.json, tool/kinds.py) and from the roles,
numbers and variables of known kind filling it in the text itself.  A variable
still undecided is named in the error, and a line

    % kinds: c person, x act, d number

beside the schemas decides it.

Usage: python3 -I notation.py FILE [KINDS.json]   prints the term
"""
import json
import os
import re
import sys

KIND_WORDS = {'person': 'pvar', 'agent': 'pvar', 'act': 'svar', 'speech act': 'svar',
              'number': 'num', 'value': 'num'}


class NotationError(Exception):
    pass


# ---------- the text ----------

def split_top(s, sep=','):
    out, d, cur = [], 0, ''
    for ch in s:
        if ch in '([<':
            d += 1
        elif ch in ')]>':
            d -= 1
        if ch == sep and d == 0:
            out.append(cur.strip())
            cur = ''
        else:
            cur += ch
    if cur.strip():
        out.append(cur.strip())
    return out


IDENT = r'[a-z][a-zA-Z0-9_]*'
NAME = r'[A-Z][a-zA-Z0-9_]*'
# A role's name: a name, or a term of names, as CSSN's one(Bob, Alice).
ROLE = NAME + r'|' + IDENT + r'\(\s*' + NAME + r'(?:\s*,\s*' + NAME + r')*\s*\)'


def name_term(n):
    """A role's name as the term form writes it in a schema's Names."""
    m = re.fullmatch(r'(' + IDENT + r')\((.*)\)', n)
    if m:
        return m.group(1) + '(' + ', '.join(q(x.strip()) for x in m.group(2).split(',')) + ')'
    return q(n)


def norm_name(n):
    return re.sub(r'\s+', '', n)


def q(name):
    return "'" + name.replace("'", "\\'") + "'"


class Schema:
    def __init__(self, line_no, name, roles, vars_, constraint):
        self.line_no, self.name, self.roles, self.vars, self.constraint = line_no, name, roles, vars_, constraint
        self.lines = {}          # role label -> items text


def parse_text(text):
    """Headers, schemas and kind hints of the text."""
    env, refl, hints, schemas = None, None, {}, []
    cur = None
    for no, raw in enumerate(text.splitlines(), 1):
        line = raw.rstrip()
        s = line.strip()
        if not s:
            continue
        m = re.match(r'%\s*kinds:\s*(.*)$', s)
        if m:
            for part in m.group(1).split(','):
                w = part.strip().split(None, 1)
                if len(w) != 2 or w[1].strip() not in KIND_WORDS or not re.fullmatch(IDENT, w[0]):
                    raise NotationError(f'line {no}: a kind is "<variable> person", "act" or "number": {part.strip()}')
                hints[w[0]] = KIND_WORDS[w[1].strip()]
            continue
        if s.startswith('%'):
            continue
        m = re.match(r'Environment predicates:\s*(.*)$', s)
        if m:
            env = [p.strip() for p in m.group(1).split(',') if p.strip()]
            continue
        m = re.match(r'Reflexive predicates:\s*(.*)$', s)
        if m:
            refl = [p.strip() for p in m.group(1).split(',') if p.strip()]
            continue
        m = re.match(r'at\s+(.+?):\s*(.*)$', s)
        if m and cur is not None and not re.match(IDENT + r'\(', s):
            label = m.group(1).strip()
            if label in cur.lines:
                raise NotationError(f'line {no}: a second line for {label} in {cur.name}')
            cur.lines[label] = (no, m.group(2))
            continue
        m = re.match(r'(' + IDENT + r')\(', s)
        if m and s.endswith(':'):
            # The head's parentheses, matched: a constraint after the bar may
            # hold parentheses of its own, as unsigned(Alice, y).
            d, j = 0, m.end() - 1
            for j in range(m.end() - 1, len(s)):
                d += {'(': 1, ')': -1}.get(s[j], 0)
                if d == 0:
                    break
            inner, rest = s[m.end():j], s[j + 1:-1].strip()
            if rest and not rest.startswith('|'):
                raise NotationError(f'line {no}: after a head\'s roles and variables comes "|" and its constraint, or ":"')
            m = (m.group(1), rest[1:].strip() or None)
            parts = split_top(inner, ';')
            if len(parts) > 2:
                raise NotationError(f'line {no}: a head has its roles, then at most one ";" and its variables')
            roles = split_top(parts[0])
            vars_ = split_top(parts[1]) if len(parts) == 2 else []
            for v in vars_:
                if not re.fullmatch(IDENT, v):
                    raise NotationError(f'line {no}: {v} is not a variable (a lower-case name)')
            cur = Schema(no, m[0], roles, vars_, m[1])
            schemas.append(cur)
            continue
        raise NotationError(f'line {no}: neither a schema\'s head, nor an "at <role>:" line: {s}')
    if not schemas:
        raise NotationError('no schemas')
    return env, refl, hints, schemas


# ---------- atoms ----------

class Ctx:
    """One schema's roles and variables, while it is read."""

    def __init__(self, sch):
        self.sch = sch
        self.agent = {}          # role name -> index of its agent role
        self.names = []
        self.listed = {}         # index -> (base index, predicate, args text)
        for i, r in enumerate(sch.roles, 1):
            m = re.fullmatch(r'(' + ROLE + r')(\?)?', r)
            if m:
                n = norm_name(m.group(1))
                if n in self.agent:
                    raise NotationError(f'line {sch.line_no}: role {m.group(1)} twice in {sch.name}')
                self.agent[n] = i
                self.names.append(m.group(1))
                continue
            m = re.fullmatch(r'(' + NAME + r')\[(' + IDENT + r')\(\*\s*(?:,(.*))?\)\]', r)
            if m:
                self.listed[i] = (m.group(1), m.group(2), m.group(3) or '')
                self.names.append(m.group(1))
                continue
            raise NotationError(f'line {sch.line_no}: {r} is not a role (a name, with ? when it guards, or Name[pred(*, ...)])')
        for i, (base, p, a) in self.listed.items():
            if base not in self.agent:
                raise NotationError(f'line {sch.line_no}: the listed role {sch.roles[i - 1]} names no agent role {base}')

    def role(self, name):
        return self.agent.get(norm_name(name))


def parse_arg(a, ctx, uses, where):
    """An argument, as a tree: ('role', i), ('var', v), ('num', n), ('any',),
    ('nterm', tree), ('sgn', v, i)."""
    a = a.strip()
    if a == '*':
        return ('any',)
    if re.fullmatch(r'-?\d+', a):
        return ('num', a)
    if re.fullmatch(ROLE, a):
        i = ctx.role(a)
        if i is None:
            raise NotationError(f'{where}: {a} is no agent role of {ctx.sch.name}')
        return ('role', i)
    if re.fullmatch(IDENT, a):
        return ('var', a)
    m = re.fullmatch(r'sgn\((' + IDENT + r'),\s*(' + NAME + r')\)', a)
    if m:
        i = ctx.role(m.group(2))
        if i is None:
            raise NotationError(f'{where}: {m.group(2)} is no agent role of {ctx.sch.name}')
        uses.setdefault(m.group(1), set()).add('svar')
        return ('sgn', m.group(1), i)
    m = re.fullmatch(r'<(' + NAME + r'|' + IDENT + r')>((?:\.(?:' + IDENT + r'|\*))+)', a)
    if m:
        base = m.group(1)
        if not re.fullmatch(NAME, base) or ctx.role(base) is None:
            raise NotationError(f'{where}: a name term is <Role>.n for an agent role of the schema (def:role): {a}')
        t = ('own', ctx.role(base))
        for step in m.group(2).split('.')[1:]:
            if step == '*':
                t = ('ext_any', t)
            else:
                uses.setdefault(step, set()).add('svar')
                t = ('ext', t, step)
        return ('nterm', t)
    raise NotationError(f'{where}: cannot read the argument {a}')


def parse_atom(s, ctx, uses, where):
    m = re.fullmatch(r'(' + IDENT + r')(?:\((.*)\))?', s.strip())
    if not m:
        raise NotationError(f'{where}: cannot read the atom {s.strip()}')
    args = [parse_arg(a, ctx, uses, where) for a in split_top(m.group(2))] if m.group(2) is not None else []
    return (m.group(1), args)


def parse_items(text, ctx, uses, where):
    add, dele, keep, forb = [], [], [], []
    for it in split_top(text):
        it = it.strip()
        if it.startswith('not '):
            forb.append(('atom', parse_atom(it[4:], ctx, uses, where)))
            continue
        mark = None
        if it[:1] in '+-':
            mark, it = it[0], it[1:].strip()
        mult = None
        m = re.match(r'(\d+|' + IDENT + r')\.(' + IDENT + r'(?:\(.*\))?)$', it)
        if m and mark:
            mult, it = m.group(1), m.group(2)
            if not mult.isdigit():
                uses.setdefault(mult, set()).add('num')
        atom = ('atom', parse_atom(it, ctx, uses, where))
        if mult:
            atom = ('times', mult, atom)
        {'+': add, '-': dele, None: keep}[mark].append(atom)
    return add, dele, keep, forb


# ---------- constraints ----------

OPS = [('<=', 'le'), ('>=', 'ge'), ('=', 'eq'), ('<', 'lt'), ('>', 'gt')]


def parse_expr(s, uses, where):
    s = s.strip()
    parts = re.split(r'\s*([+-])\s*', s)
    if len(parts) > 1 and parts[0] != '':
        t = parse_term(parts[0], uses, where)
        for i in range(1, len(parts), 2):
            t = ('plus' if parts[i] == '+' else 'minus', t, parse_term(parts[i + 1], uses, where))
        return t
    return parse_term(s, uses, where)


def parse_term(s, uses, where):
    fs = [f.strip() for f in s.split('*')]
    t = parse_factor(fs[0], uses, where)
    for f in fs[1:]:
        t = ('mul', t, parse_factor(f, uses, where))
    return t


def parse_factor(s, uses, where):
    if re.fullmatch(r'-?\d+', s):
        return ('num', s)
    if re.fullmatch(IDENT, s):
        uses.setdefault(s, set()).add('num')
        return ('var', s)
    raise NotationError(f'{where}: cannot read {s} in the constraint')


def parse_constraint(c, ctx, uses, where):
    cs = []
    for part in split_top(c):
        m = re.fullmatch(r'(signed|unsigned)\((.*)\)', part)
        if m:
            a = split_top(m.group(2))
            if len(a) != 2:
                raise NotationError(f'{where}: {part}')
            u = parse_arg(a[0], ctx, uses, where)
            uses.setdefault(a[1], set()).add('svar')
            cs.append((m.group(1), u, ('var', a[1])))
            continue
        if ' or ' in part:
            raise NotationError(f'{where}: the checker refuses "or" in a constraint')
        for sym, op in OPS:
            if sym in part:
                l, r = part.split(sym, 1)
                if sym == '=' and re.fullmatch(r'\s*' + IDENT + r'\s*', l) and re.fullmatch(r'\s*' + IDENT + r'\s*\+\s*1\s*', r):
                    y, x = l.strip(), r.split('+')[0].strip()
                    uses.setdefault(x, set()).add('num')
                    uses.setdefault(y, set()).add('num')
                    cs.append(('succ', ('var', x), ('var', y)))
                else:
                    cs.append((op, parse_expr(l, uses, where), parse_expr(r, uses, where)))
                break
        else:
            raise NotationError(f'{where}: cannot read the constraint {part}')
    t = cs[-1]
    for c1 in reversed(cs[:-1]):
        t = ('and', c1, t)
    return t


# ---------- kinds ----------

def places(tree, out, pred=None):
    """(predicate, arity, place) of each variable and fixed argument of the
    atoms in the tree."""
    if tree[0] == 'times':
        places(tree[2], out)
    elif tree[0] == 'atom':
        p, args = tree[1]
        for k, a in enumerate(args):
            out.append(((p, len(args), k), a))


def decide_kinds(schemas_read, base, hints):
    """Each schema's variables' kinds, and the places' kinds: from the text
    alone first, then with the places of the contracts GAS knows."""
    known = _fix(schemas_read, {}, hints)
    for k, v in base.items():
        known.setdefault(k, v)
    return _fix(schemas_read, known, hints)


def _fix(schemas_read, known, hints):
    known = dict(known)              # 'p/arity/k' -> kind
    while True:
        changed = False
        for sch, ctx, uses, roles in schemas_read:
            occ = []
            for add, dele, keep, forb, _ in roles:
                for t in add + dele + keep + forb:
                    places(t, occ)
            for (p, n, k), a in occ:
                key = f'{p}/{n}/{k}'
                kind = {'role': 'pvar', 'num': 'num', 'sgn': 'svar'}.get(a[0])
                if a[0] == 'var':
                    vs = uses.get(a[1], set()) | ({hints[a[1]]} if a[1] in hints else set())
                    if len(vs) == 1:
                        kind = next(iter(vs))
                    elif key in known:
                        uses.setdefault(a[1], set()).add(known[key])
                        changed = True
                if kind and key not in known:
                    known[key] = kind
                    changed = True
        if not changed:
            return known


WORD = {'pvar': 'person', 'svar': 'act', 'num': 'number'}


def kinds_of(sch, uses, hints, where, notes):
    """The kinds of the schema's variables; a variable no use shows to be a
    person or a number is a speech act, and noted."""
    out = {}
    for v in sch.vars:
        ks = set(uses.get(v, set()))
        if v in hints:
            ks = {hints[v]}
        if len(ks) > 1:
            raise NotationError(f'{where}: {v} is used as {" and as ".join(WORD[k] for k in sorted(ks))}; add a line "% kinds: {v} person" (or act, or number)')
        if not ks:
            notes.append(f'{sch.name}: {v} read as a speech act (no use shows it to be a person or a number); "% kinds: {v} person" says otherwise')
            ks = {'svar'}
        out[v] = next(iter(ks))
    return out


# ---------- the term ----------

def arg_term(a, kinds):
    if a[0] == 'role':
        return f'role({a[1]})'
    if a[0] == 'num':
        return f'num({a[1]})'
    if a[0] == 'any':
        return 'any'
    if a[0] == 'var':
        return f'{kinds[a[1]]}({a[1]})'
    if a[0] == 'sgn':
        return f'sgn({a[1]}, {a[2]})'
    if a[0] == 'nterm':
        return f'nterm({nt_term(a[1])})'
    raise ValueError(a)


def nt_term(t):
    if t[0] == 'own':
        return f'own({t[1]})'
    if t[0] == 'ext':
        return f'ext({nt_term(t[1])}, {t[2]})'
    return f'ext_any({nt_term(t[1])})'


def atom_term(t, kinds):
    if t[0] == 'times':
        m = t[1] if t[1].isdigit() else t[1]
        return f'times(num({m}), {atom_term(t[2], kinds)})'
    p, args = t[1]
    return f'atom({p}, [{", ".join(arg_term(a, kinds) for a in args)}])'


def expr_term(e, kinds):
    if e[0] in ('num',):
        return f'num({e[1]})'
    if e[0] == 'var':
        return f'{kinds.get(e[1], "num")}({e[1]})'
    if e[0] in ('role', 'any', 'sgn', 'nterm'):
        return arg_term(e, kinds)
    return f'{e[0]}({expr_term(e[1], kinds)}, {expr_term(e[2], kinds)})'


def lst(ts, kinds):
    return '[' + ', '.join(atom_term(t, kinds) for t in ts) + ']'


def read(text, base=None, notes=None):
    """The contract term of the text, as a string; [notes] gets what was
    assumed."""
    notes = [] if notes is None else notes
    env, refl, hints, schemas = parse_text(text)
    names = [s.name for s in schemas]
    dup = {n for n in names if names.count(n) > 1}
    if dup:
        raise NotationError(f'two schemas named {", ".join(sorted(dup))}')
    read_ = []
    for sch in schemas:
        where = f'line {sch.line_no} ({sch.name})'
        ctx = Ctx(sch)
        uses = {}
        cons = parse_constraint(sch.constraint, ctx, uses, where) if sch.constraint else None
        roles = []
        labels = dict(sch.lines)
        for i, r in enumerate(sch.roles, 1):
            label = r.rstrip('?')
            key = next((k for k in labels if norm_name(k) == norm_name(label)), label)
            no, items = labels.pop(key, (sch.line_no, ''))
            add, dele, keep, forb = parse_items(items, ctx, uses, f'line {no} ({sch.name})')
            roles.append((add, dele, keep, forb, (i, r)))
        if labels:
            raise NotationError(f'{where}: a line for {", ".join(labels)}, which is no role of the schema')
        lroles = {}
        for i, (base_name, p, a) in ctx.listed.items():
            lroles[i] = (ctx.agent[base_name], p, [parse_arg(x, ctx, uses, where) for x in split_top(a)])
        read_.append((sch, ctx, uses, roles, cons, lroles, where))
    decide_kinds([(s, c, u, r) for s, c, u, r, *_ in read_], base or {}, hints)
    out = []
    for sch, ctx, uses, roles, cons, lroles, where in read_:
        kinds = kinds_of(sch, uses, hints, where, notes)
        vars_t = '[' + ', '.join(f'{kinds[v]}({v})' for v in sch.vars) + ']'
        rs = []
        for add, dele, keep, forb, (i, r) in roles:
            body = f'{lst(add, kinds)}, {lst(dele, kinds)}, {lst(keep, kinds)}, {lst(forb, kinds)}'
            if i in lroles:
                bi, p, la = lroles[i]
                rs.append(f'crs(listed({bi}, {p}, [{", ".join(arg_term(x, kinds) for x in la)}]), unguarded, {body})')
            else:
                rs.append(f'rs({"guarded" if r.endswith("?") else "unguarded"}, {body})')
        nm = '[' + ', '.join(name_term(n) for n in ctx.names) + ']'
        rl = '[' + ', '.join(rs) + ']'
        if cons:
            out.append(f'nschema({sch.name}, {nm}, {vars_t}, {cons_term(cons, kinds)}, {rl})')
        elif lroles:
            out.append(f'cschema({sch.name}, {nm}, {vars_t}, [], {rl})')
        else:
            out.append(f'schema({sch.name}, {nm}, {vars_t}, {rl})')
    body = '[' + ', '.join(out) + ']'
    if refl:
        body = f'reflexive([{", ".join(refl)}], {body})'
    if env:
        body = f'environment([{", ".join(env)}], {body})'
    return body


def cons_term(c, kinds):
    if c[0] == 'and':
        return f'and({cons_term(c[1], kinds)}, {cons_term(c[2], kinds)})'
    if c[0] in ('signed', 'unsigned'):
        return f'{c[0]}({arg_term(c[1], kinds)}, svar({c[2][1]}))'
    if c[0] == 'succ':
        return f'succ(num({c[1][1]}), num({c[2][1]}))'
    return f'{c[0]}({expr_term(c[1], kinds)}, {expr_term(c[2], kinds)})'


def load_kinds(path):
    if path and os.path.exists(path):
        return json.load(open(path, encoding='utf-8'))
    return {}


if __name__ == '__main__':
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    try:
        print(read(open(sys.argv[1], encoding='utf-8').read(),
                   load_kinds(sys.argv[2] if len(sys.argv) > 2 else os.path.join(os.path.dirname(__file__), 'kinds.json'))))
    except NotationError as e:
        sys.exit(f'notation: {e}')
