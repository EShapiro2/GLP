#!/usr/bin/env python3
"""wildcards.py --- every wildcard in an atom a schema requires, in the terms glp/NAME.glp.txt that tools/toglp.py writes:
vGLP, Section 5.4, admits the wildcard only in a negated atom.  Its last line is the count, 0 when there is none."""
# Each sas(...) term: report a wildcard in a required atom of the body: an atom(...) with '_' not under not(...).
import os, re
def split_top(s):
    out, d, cur = [], 0, ''
    for ch in s:
        if ch in '([': d += 1
        elif ch in ')]': d -= 1
        if ch == ',' and d == 0: out.append(cur.strip()); cur = ''
        else: cur += ch
    if cur.strip(): out.append(cur.strip())
    return out
def walk(t, neg, hits, name):
    t = t.strip()
    m = re.match(r'(\w+)\((.*)\)$', t, re.S)
    if t.startswith('['): 
        for x in split_top(t[1:-1]): walk(x, neg, hits, name)
        return
    if not m: return
    f, inner = m.group(1), m.group(2)
    if f == 'atom':
        if "'_'" in inner and not neg: hits.append(name + ': ' + t[:120])
        return
    if f == 'not': walk(inner, True, hits, name); return
    for x in split_top(inner): walk(x, neg, hits, name)
tot = 0
for f in sorted(os.listdir('glp')):
    if not f.endswith('.glp.txt'): continue
    txt = open('glp/' + f).read().replace('\n', '')
    for m in re.finditer(r"sas\((\w+), ", txt):
        start = m.start(); d = 0
        for j in range(start, len(txt)):
            if txt[j] in '([': d += 1
            elif txt[j] in ')]':
                d -= 1
                if d == 0: break
        args = split_top(txt[start+4:j])
        hits = []
        walk(args[4], False, hits, f[:-8] + ' ' + m.group(1))
        for h in hits: print(h); tot += 1
print(tot, 'wildcards in required atoms')
