#!/usr/bin/env python3
"""pragtex.py --- the appendix of the Logic pragmas: gr/NAME.txt of each contract, as the compilers read it.

    python3 tools/pragtex.py        writes sections/contracts-pragmas.tex, the contracts in the order of Table 1
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM      # the names of the contracts and their files (tools/names.py, NAMES.txt)
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import table
MARKS = {'■': r'\enf', '◆': r'\att', '○': r'\und'}
UNI = {'τ': r'$\tau$', '−': r'$-$', '₀': r'$_0$', '₂': r'$_2$', '≥': r'$\geq$', '≤': r'$\leq$', 'λ': r'$\lambda$', 'δ': r'$\delta$'}


def tex(s):
    s = re.sub(r'([&%$#_{}])', r'\\\1', s)
    s = s.replace('~', r'\textasciitilde{}').replace('^', r'\textasciicircum{}').replace('<', r'$<$').replace('>', r'$>$')
    s = re.sub(r'(?<![(,])\*([^*(),=;]+)\*', r'\\emph{\1}', s)       # a term *defined*; not the *n of a predicate, n copies
    s = s.replace('*', r'{\ast}') if False else re.sub(r'(?<=[(,])\*(?=\w)', r'$\\ast$', s)
    s = re.sub(r'https?://\S+?(?=[).,]*(\s|$))', lambda m: r'\url{%s}' % m.group(0).replace('\\', ''), s)
    return ''.join(UNI.get(ch, ch) for ch in s)


def box(name):
    lines = [l.rstrip('\n') for l in open(os.path.join(ROOT, 'gr', NM.file_of(name) + '.txt'), encoding='utf-8') if l.strip()]
    out = [r'\begin{contractbox}{%s}' % tex(lines[0]), r'\raggedright']
    items = False
    for l in lines[1:]:
        m = re.match(r'(\d+)\. ([■◆○]) (.*)$', l)
        if m:
            if not items:
                out.append(r'\begin{enumerate}[label=\arabic*.]')
                items = True
            out.append(r'\setcounter{enumi}{%d}\item %s~%s' % (int(m.group(1)) - 1, MARKS[m.group(2)], tex(m.group(3))))
            continue
        if items:
            out.append(r'\end{enumerate}')
            items = False
        if l == 'Logic pragmas.':
            continue
        out.append(tex(l) + r'\par')
    if items:
        out.append(r'\end{enumerate}')
    out.append(r'\end{contractbox}')
    return out


def main():
    names = table.order()
    out = ['% Printed by tools/pragtex.py from gr/NAME.txt.  Do not edit.',
           r'\section{The Logic Pragmas}\label{app:contracts-pragmas}', '',
           'The Logic pragmas of the %d contracts, as the compilers read them, in the order of Table~\\ref{tab:contracts}: the first line, the names and the definitions of the contract, a sentence for each act and each citation of each clause, with the sentences kept as written, and then the vocabulary, the roles and the meanings of the phrases.  A place is a single small letter.' % len(names), '']
    for n in names:
        out += box(n) + ['']
    os.makedirs(os.path.join(ROOT, 'sections'), exist_ok=True)
    with open(os.path.join(ROOT, 'sections', 'contracts-pragmas.tex'), 'w', encoding='utf-8') as f:
        f.write('\n'.join(out))
    print('contracts-pragmas.tex: %d contracts' % len(names))


if __name__ == '__main__':
    main()
