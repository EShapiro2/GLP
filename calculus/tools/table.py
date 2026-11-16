#!/usr/bin/env python3
"""Prints sections/contracts-table.tex, the table of the contracts, from gr/ and exp/.
Run tools/expand.py first, and tools/gcsas.py for the sizes of the schemas (sas/ and sasx/).  Calculus #2, 2026-11-13; the schemas, Calculus #3, 2026-11-14."""
import os, re, sys
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REL = re.compile(r'\b(extends|restricts|strengthens|instantiates|composes|relaxes)\b')
def lines(path):
    return [l.rstrip('\n') for l in open(path, encoding='utf-8') if l.strip() and not l.startswith('%')]
def words(ls):
    return sum(len(re.sub(r'[*■◆○]', ' ', l).split()) for l in ls)
def esc(s):
    s = s.replace('\\', '').replace('*', '')
    for c in '&%$#_{}':
        s = s.replace(c, '\\' + c)
    return s
def names():
    return sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'rev')) if f.endswith('.txt') and not f.startswith('NOTES'))
def first_line(name):
    body = lines(os.path.join(ROOT, 'rev', name + '.txt'))[1:]
    first = next((l for l in body if not re.match(r'\d+\.', l) and l.startswith(name + ' ') and REL.search(l)), '')
    return re.sub(r'\s*\([^()]*(https?:|in preparation)[^()]*\)', '', first)
def deps(name, all_names):
    return [w for w in dict.fromkeys(re.findall(r'[A-Za-z][A-Za-z-]*', first_line(name))) if w in all_names and w != name]
def layers():
    """The canonical order: by layer (GKK is 0; else one more than the highest layer among
    the contracts of the first line); within a layer by the list of those contracts, each
    list sorted in this order and compared entry by entry; ties by name."""
    ns = names(); d = {n: deps(n, set(ns)) for n in ns}; lay = {}
    def L(n, seen=()):
        if n not in lay:
            assert n not in seen, 'cycle at ' + n
            lay[n] = 1 + max(L(m, seen + (n,)) for m in d[n]) if d[n] else 0
        return lay[n]
    for n in ns: L(n)
    pos = {}; out = []
    for k in range(max(lay.values()) + 1):
        layer = sorted((n for n in ns if lay[n] == k), key=lambda n: (sorted(pos[m] for m in d[n]), n))
        for n in layer: pos[n] = len(pos)
        out.append(layer)
    return out
def order():
    return [n for layer in layers() for n in layer]
def row(name):
    rv = lines(os.path.join(ROOT, 'rev', name + '.txt'))
    ex = lines(os.path.join(ROOT, 'exp', name + '.txt'))
    body = rv[1:]
    first = next((l for l in body if not re.match(r'\d+\.', l) and l.startswith(name + ' ') and REL.search(l)), '')
    how = re.sub(r'\s*\([^()]*(https?:|in preparation)[^()]*\)', '', first)
    how = re.sub(r'^' + re.escape(name) + r'\s+', '', how).rstrip('.')
    own = sum(1 for l in body if re.match(r'\d+\.\s', l))
    exc = sum(1 for l in ex[1:] if re.match(r'[A-Za-z-]+:\d+(, [A-Za-z-]+:\d+)*\.\s', l))
    return (name, how, own, words(body), exc, words(ex[1:])) + schemas('sas', name) + schemas('sasx', name)
def schemas(d, name):
    """The schemas of d/name and their literals: lines 'LABEL MARK Name: head <- body guarded by G.'; (None, None) if not written."""
    path = os.path.join(ROOT, d, name + '.txt')
    if not os.path.exists(path): return (None, None)
    ls = [l for l in lines(path) if re.match(r'[A-Za-z-]+:\d+(\+[A-Za-z-]+:\d+)* [■◆○] [^|]', l)]
    return len(ls), sum(len(re.findall(r'[a-z_0-9]+\(', l.split(' guarded by ')[0])) for l in ls)
def outcome():
    """For each contract, the verdict of GLP's checker (glp/VERDICTS.txt) and the lines of its program in GLP (cascade/STATUS.txt)."""
    out = {}
    for l in lines(os.path.join(ROOT, 'glp', 'VERDICTS.txt')):
        n, v = l.split(None, 1)
        out[n] = ['wards' if v.startswith('syntactically_grassroots_with_wards') else 'yes' if v.startswith('syntactically_grassroots') else 'no', None]
    for l in lines(os.path.join(ROOT, 'cascade', 'STATUS.txt')):
        n = l.split()[0]; m = re.search(r'platform\.glp (\d+) lines', l)
        if n in out and m: out[n][1] = int(m.group(1))
        elif n in out and 'a parametric contract' in l: out[n][1] = 'parametric'      # certified, no platform until instantiated
    return out
def num(x):
    return '---' if x is None else '\\emph{param.}' if x == 'parametric' else '%d' % x
def tot(rows, i):
    return num(None if any(r[i] is None for r in rows) else sum(r[i] for r in rows))
def main():
    lays = layers(); rows = [row(n) for n in order()]; byname = {r[0]: r for r in rows}; oc = outcome()
    out = ['% Written by tools/table.py from rev/, exp/, sas/ and sasx/; do not edit.  The table has twelve columns; the last two are the outcome of the abstraction cascade.',
           '\\subsection{The Contracts}\\label{sec:table}', '',
           'Table~\\ref{tab:contracts} lists the contracts of Appendix~\\ref{app:contracts-revised}: what each is written from and how, its clauses and words, those of its expanded text (Appendix~\\ref{app:contracts-expanded}), its schemas and their literals, its own and expanded, and its outcome in the abstraction cascade: whether its checker found the contract syntactically grassroots, as it is or as a contract with wards, and the lines of the program in GLP compiled from it.  The clauses and the words are counted in the text of a contract, without its Logic pragmas; the schemas and their literals are those of its Logic pragmas.  The contracts are in layers: GKK is layer 0, and the layer of any other contract is one more than the highest layer among the contracts it is written from.  Within a layer the contracts are ordered by the contracts they are written from, and then by name.', '',
           '{\\footnotesize\\setlength\\tabcolsep{3pt}', '\\begin{longtable}{@{}lp{0.2\\linewidth}rrrrrrrrlr@{}}',
           '\\caption{The contracts.}\\label{tab:contracts}\\\\', '\\hline',
           ' & & \\multicolumn{2}{c}{\\emph{contract}} & \\multicolumn{2}{c}{\\emph{expanded}} & \\multicolumn{2}{c}{\\emph{schemas}} & \\multicolumn{2}{c}{\\emph{expanded}} & \\multicolumn{2}{c}{\\emph{cascade}}\\\\',
           '\\emph{contract} & \\emph{written from} & \\emph{clauses} & \\emph{words} & \\emph{clauses} & \\emph{words} & \\emph{schemas} & \\emph{literals} & \\emph{schemas} & \\emph{literals} & \\emph{grassroots} & \\emph{GLP}\\\\', '\\hline', '\\endhead']
    for k, layer in enumerate(lays):
        out.append('\\multicolumn{12}{@{}l}{\\textbf{Layer %d}}\\\\' % k)
        for name in layer:
            n, how, own, w, exc, ew = byname[name][:6]
            out.append('%s & %s & %d & %d & %d & %d & %s\\\\' % (esc(n), esc(how) if how else '---', own, w, exc, ew, ' & '.join(num(x) for x in byname[name][6:]) + ' & %s & %s' % (oc[name][0], num(oc[name][1]))))
    out += ['\\hline', '%d contracts & & %d & %d & %d & %d & %s\\\\' % (len(rows), sum(r[2] for r in rows), sum(r[3] for r in rows), sum(r[4] for r in rows), sum(r[5] for r in rows), ' & '.join(tot(rows, i) for i in (6, 7, 8, 9)) + ' & %d & %d' % (sum(1 for r in rows if oc[r[0]][0] != 'no'), sum(1 for r in rows if isinstance(oc[r[0]][1], int)))),
            '\\hline', '\\end{longtable}', '}', '']
    open(os.path.join(ROOT, 'sections', 'contracts-table.tex'), 'w', encoding='utf-8').write('\n'.join(out))
    print('%d contracts; own clauses %d, words %d; expanded clauses %d, words %d' % (len(rows), sum(r[2] for r in rows), sum(r[3] for r in rows), sum(r[4] for r in rows), sum(r[5] for r in rows)))
    open(os.path.join(ROOT, 'LAYERS.txt'), 'w', encoding='utf-8').write(''.join('%d %s\n' % (k, ' '.join(l)) for k, l in enumerate(lays)))      # the order of adoption: a line for each layer
    bad = [r[0] for r in rows if r[0] != 'GKK' and not r[1]]
    if bad: print('no first line found:', bad); sys.exit(1)
if __name__ == '__main__':
    main()
