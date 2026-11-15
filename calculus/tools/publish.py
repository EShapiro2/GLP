#!/usr/bin/env python3
"""publish.py DIR [HELD ...] --- writes into DIR what is published: one directory for each contract, with every artefact of
Figure 1 of the paper, and the programs that print them.

    DIR/NAME/contract.txt           the contract, as approved (rev/)
    DIR/NAME/logic-pragmas.txt      its Logic pragmas (gr/)
    DIR/NAME/schemas.txt            its speech-act schemas (sas/)
    DIR/NAME/full-contract.txt      the full contract: the operations of its first line and citations applied (exp/)
    DIR/NAME/full-pragmas.txt       the expanded sentences of its Logic pragmas (gx/)
    DIR/NAME/flat.txt               its flat text (flat/)
    DIR/NAME/full-schemas.txt       its expanded schemas (sasx/)
    DIR/NAME/term.glp.txt           the expanded schemas as the term GLP's checker reads (glp/)
    DIR/NAME/verdict.txt, vmat.tex, interface.txt, platform.vglp, platform.glp    the abstraction cascade (cascade/NAME/)
    DIR/README.md, OWNERS.md, MIGRATION.md, STATE.txt (a line for each contract: name, owner, certified, compiled, adopted), STATUS.txt, VERDICTS.txt, tools/, tests/
"""
import os, shutil, sys
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MAP = [('rev', '%s.txt', 'contract.txt'), ('gr', '%s.txt', 'logic-pragmas.txt'), ('sas', '%s.txt', 'schemas.txt'),
       ('exp', '%s.txt', 'full-contract.txt'), ('gx', '%s.txt', 'full-pragmas.txt'), ('flat', '%s.txt', 'flat.txt'),
       ('sasx', '%s.txt', 'full-schemas.txt'), ('glp', '%s.glp.txt', 'term.glp.txt')]


def state(out, names):
    """STATE.txt, a line for each contract: NAME owner certified compiled adopted, each of the last three yes or no.
    The owner is read from OWNERS.md; adopted is yes when OWNERS.md says the owner's text is the same."""
    import re
    own, same, cert, comp = {}, {}, {}, {}
    for l in open(os.path.join(ROOT, 'OWNERS.md'), encoding='utf-8'):
        m = re.match(r'\| (\S+) \| ([^|]+) \|[^|]*\| ([^|]*) \|', l)
        if m and m.group(1) != 'Contract': own[m.group(1)] = m.group(2).strip().replace(' ', '_'); same[m.group(1)] = m.group(3).strip() == 'same'
    for l in open(os.path.join(ROOT, 'glp', 'VERDICTS.txt'), encoding='utf-8'):
        if l.strip(): cert[l.split()[0]] = l.split(None, 1)[1].startswith('syntactically_grassroots')
    for l in open(os.path.join(ROOT, 'cascade', 'STATUS.txt'), encoding='utf-8'):
        if l.strip(): comp[l.split()[0]] = 'platform.glp' in l
    own['AGORA'] = 'Currencies'      # Udi, 2026-11-15; its owner line in GLP names Talmon, for Currencies to correct
    yn = lambda b: 'yes' if b else 'no'
    with open(os.path.join(out, 'STATE.txt'), 'w', encoding='utf-8') as f:
        for n in names:
            f.write('%s %s %s %s %s\n' % (n, own.get(n, 'none'), yn(cert.get(n)), yn(comp.get(n)), yn(same.get(n))))


def main(out, hold=()):
    names = sorted(f[:-4] for f in os.listdir(os.path.join(ROOT, 'rev')) if f.endswith('.txt') and not f.startswith('NOTES'))
    names = [n for n in names if n not in hold]      # a contract with no certificate is held out
    missing = []
    for n in names:
        d = os.path.join(out, n); os.makedirs(d, exist_ok=True)
        for src, pat, dst in MAP:
            p = os.path.join(ROOT, src, pat % n)
            if os.path.exists(p): shutil.copyfile(p, os.path.join(d, dst))
            else: missing.append('%s: %s' % (n, dst))
        c = os.path.join(ROOT, 'cascade', n)
        for f in sorted(os.listdir(c)) if os.path.isdir(c) else []:
            shutil.copyfile(os.path.join(c, f), os.path.join(d, 'interface.txt' if f == n + '.txt' else f))
        for f in ('verdict.txt', 'platform.glp'):
            if not os.path.exists(os.path.join(d, f)): missing.append('%s: %s' % (n, f))
    for f, g in (('README-PUBLIC.md', 'README.md'), ('OWNERS.md', 'OWNERS.md'), ('MIGRATION.md', 'MIGRATION.md'), ('cascade/STATUS.txt', 'STATUS.txt'), ('glp/VERDICTS.txt', 'VERDICTS.txt')):
        if os.path.exists(os.path.join(ROOT, f)): shutil.copyfile(os.path.join(ROOT, f), os.path.join(out, g))
    state(out, names)
    for d in ('tools', 'tests'):
        shutil.copytree(os.path.join(ROOT, d), os.path.join(out, d), dirs_exist_ok=True, ignore=shutil.ignore_patterns('__pycache__'))
    print('%d contracts in %s; missing: %s' % (len(names), out, '; '.join(missing) or 'nothing'))


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2:])
