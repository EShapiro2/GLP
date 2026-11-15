#!/usr/bin/env python3
"""typecheck_places.py --- for every contract, the places of a predicate, (predicate, arity, position), that its term
writes with two types: a number num(X), a speech-act variable or a constant val(C).  A platform gives each such
place two alternatives of one functor and does not load.  Run from anywhere: python3 tools/typecheck_places.py
"""
import sys, re, os
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import toglp
def args_split(s):
    out, d, cur = [], 0, ''
    for ch in s:
        if ch == ',' and d == 0: out.append(cur.strip()); cur = ''; continue
        if ch in '([': d += 1
        if ch in ')]': d -= 1
        cur += ch
    if cur.strip(): out.append(cur.strip())
    return out
def kind(a):
    if a.startswith('const('): return 'val'      # a value variable over the constants is of the constants' type
    for k in ('num', 'svar', 'val', 'sgn', 'item_of'):
        if a.startswith(k + '('): return k
    if a == "'_'": return 'any'
    if a.startswith("'") or re.fullmatch(r'[A-Z]\w*', a): return 'name'
    return 'other'
bad = {}
names = sorted(f[:-4] for f in os.listdir(ROOT + '/sasx') if f.endswith('.txt'))
for n in names:
    t = toglp.contract_term(n)[0]
    types = {}
    starts = [(m.start(), m.end()) for m in re.finditer(r"sas\(", t)]
    def names_at(pos):
        st = [b for a, b in starts if a <= pos]
        if not st: return set()
        b = st[-1]; d = 1; k = b
        while d:
            if t[k] in '([': d += 1
            elif t[k] in ')]': d -= 1
            k += 1
        parts = args_split(t[b:k-1])
        return set(x.strip("'") for x in re.findall(r"'?[A-Za-z]\w*'?", parts[1]))
    for m in re.finditer(r'atom\((\w+), \[', t):
        roles = names_at(m.start())
        j = m.end(); d = 1; k = j
        while d:
            if t[k] in '([': d += 1
            elif t[k] in ')]': d -= 1
            k += 1
        al = args_split(t[j:k-1])
        for pos, a in enumerate(al):
            kd = kind(a)
            if kd == 'name':
                kd = 'role' if a.strip("'") in roles else 'svar'
            if kd in ('num', 'svar', 'val'):
                types.setdefault((m.group(1), len(al), pos), set()).add(kd)
    two = {p: ks for p, ks in types.items() if len(ks) > 1}
    if two: bad[n] = two
print(len(names), 'contracts;', len(bad), 'with a place of two types')
for n, two in bad.items(): print(n, sorted((p, sorted(ks)) for p, ks in two.items()))
