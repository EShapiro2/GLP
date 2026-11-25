#!/usr/bin/env python3
"""assemble.py --- cascade/STATUS.txt and glp/VERDICTS.txt from the directories cascade/NAME/, as a run of all would
write them: for a run made in shares.  A contract with no verdict.txt, or whose verdict is no certificate, is
'not certified'; one with no platform.glp has no program; a parametric contract, its term in glp/NAME.glp.txt
parametric(...), is certified and has no platform until it is instantiated, and is counted apart."""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM      # the names of the contracts and their files (tools/names.py, NAMES.txt)
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def parametric(n):
    """Whether the term of NAME, glp/NAME.glp.txt, is a parametric contract, parametric(...)."""
    p = os.path.join(ROOT, 'glp', NM.file_of(n) + '.glp.txt')
    return os.path.exists(p) and open(p, encoding='utf-8').read().lstrip().startswith('parametric(')


def main():
    names = NM.names_in(os.path.join(ROOT, 'rev'))
    st, vd = [], []
    for n in names:
        d = os.path.join(ROOT, 'cascade', NM.file_of(n))
        rd = lambda f: open(os.path.join(d, f), encoding='utf-8').read() if os.path.exists(os.path.join(d, f)) else ''
        v = rd('verdict.txt').strip() or 'no verdict'
        vd.append('%-10s %s' % (NM.file_of(n), v))
        if not v.startswith('syntactically_grassroots'):
            st.append('%-10s %s' % (NM.file_of(n), 'no verdict' if v == 'no verdict' else 'not certified')); continue
        s = 'certified; VMAT %d displays' % len(re.findall(r'\\\[|\\begin\{(?:align|equation|gather|displaymath)', rd('vmat.tex')))
        pv, pg = rd('platform.vglp'), rd('platform.glp')
        if parametric(n):
            s += '; no platform: a parametric contract has none until it is instantiated'
        elif pg and pv and '%% not compiled' not in pv:
            s += '; platform.vglp %d lines; platform.glp %d lines' % (pv.count('\n'), pg.count('\n'))
        elif pv:
            s += '; platform not compiled'
        else:
            s += '; no platform'
        st.append('%-10s %s' % (NM.file_of(n), s))
    open(os.path.join(ROOT, 'cascade', 'STATUS.txt'), 'w', encoding='utf-8').write('\n'.join(st) + '\n')
    open(os.path.join(ROOT, 'glp', 'VERDICTS.txt'), 'w', encoding='utf-8').write('\n'.join(vd) + '\n')
    c = sum(1 for l in st if ' certified;' in l); g = sum(1 for l in st if 'platform.glp' in l)
    p = sum(1 for l in st if 'a parametric contract' in l)
    print('%d contracts: %d certified, %d with a program in GLP, %d parametric with no platform until instantiated' % (len(names), c, g, p))


if __name__ == '__main__':
    main()
