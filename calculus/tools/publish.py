#!/usr/bin/env python3
"""publish.py DIR [HELD ...] [--since OLD] --- writes into DIR what is published: one directory for each contract, with every artefact of
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
    DIR/NAME/interface-adopted.txt  the Interface pragmas of interface.txt keyed by the titles of the contract's own
                                    clauses, for its owner's adopted file, proposed to the owner (tools/ifadopt.py)
    DIR/CHANGES.txt                 with --since OLD, a publication directory: what DIR asks of an owner who
                                    adopted OLD (tools/changes.py)
    DIR/README.md, OWNERS.md, MIGRATION.md, STATE.txt (a line for each contract: name, owner, certified, compiled (yes, no or parametric), adopted), STATUS.txt, VERDICTS.txt, tools/, tests/
"""
import os, shutil, sys, tempfile
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
        if l.strip(): comp[l.split()[0]] = 'parametric' if 'a parametric contract' in l else 'platform.glp' in l
    own['SGSG'] = 'SPM'      # it has no text in GLP yet; SPM's paper has it
    own['AGORA'] = 'Currencies'      # Udi, 2026-11-15; its owner line in GLP names Talmon, for Currencies to correct
    yn = lambda b: b if isinstance(b, str) else 'yes' if b else 'no'      # compiled: yes, no, or parametric, certified with no platform until instantiated
    with open(os.path.join(out, 'STATE.txt'), 'w', encoding='utf-8') as f:
        for n in names:
            f.write('%s %s %s %s %s\n' % (n, own.get(n, 'none'), yn(cert.get(n)), yn(comp.get(n)), yn(same.get(n))))


def main(out, hold=()):
    """Built in a fresh directory beside OUT and moved into OUT only when nothing is missing; else OUT is left as it
    was and the exit is 1."""
    final = os.path.abspath(out)
    out = tempfile.mkdtemp(prefix='.publish-', dir=os.path.dirname(final) or '.')
    try:
        missing, names = build(out, hold)
        if missing:
            print('%d contracts; missing: %s; nothing published in %s' % (len(names), '; '.join(missing), final), file=sys.stderr)
            return 1
        os.makedirs(final, exist_ok=True)
        for e in sorted(os.listdir(out)):          # each entry replaces the one of the same name, whole
            dst = os.path.join(final, e)
            if os.path.isdir(dst) and not os.path.islink(dst):
                shutil.rmtree(dst)
            elif os.path.exists(dst):
                os.remove(dst)
            shutil.move(os.path.join(out, e), dst)
        print('%d contracts in %s; missing: nothing' % (len(names), final))
        return 0
    finally:
        shutil.rmtree(out, ignore_errors=True)


def build(out, hold=()):
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
        param = any(l.split()[:1] == [n] and 'a parametric contract' in l for l in open(os.path.join(ROOT, 'cascade', 'STATUS.txt'), encoding='utf-8'))
        for f in ('verdict.txt',) if param else ('verdict.txt', 'platform.glp'):      # a parametric contract has no platform until instantiated
            if not os.path.exists(os.path.join(d, f)): missing.append('%s: %s' % (n, f))
    for f, g in (('README-PUBLIC.md', 'README.md'), ('OWNERS.md', 'OWNERS.md'), ('LAYERS.txt', 'LAYERS.txt'), ('MIGRATION.md', 'MIGRATION.md'), ('cascade/STATUS.txt', 'STATUS.txt'), ('glp/VERDICTS.txt', 'VERDICTS.txt')):
        if os.path.exists(os.path.join(ROOT, f)): shutil.copyfile(os.path.join(ROOT, f), os.path.join(out, g))
    state(out, names)
    for d in ('tools', 'tests'):
        shutil.copytree(os.path.join(ROOT, d), os.path.join(out, d), dirs_exist_ok=True, ignore=shutil.ignore_patterns('__pycache__'))
    return missing, names


if __name__ == '__main__':
    since = sys.argv[sys.argv.index('--since') + 1] if '--since' in sys.argv else None
    args = [a for i, a in enumerate(sys.argv[1:], 1) if a != '--since' and sys.argv[i - 1] != '--since']
    r = main(args[0], args[1:])
    if r == 0 and since:       # CHANGES.txt: what this publication asks of an owner who adopted the one in SINCE
        sys.path.insert(0, os.path.join(ROOT, 'tools'))
        import changes
        open(os.path.join(args[0], 'CHANGES.txt'), 'w', encoding='utf-8').write(changes.changes(since, args[0]))
        print('CHANGES.txt written in %s, since %s' % (args[0], since))
    sys.exit(r)
