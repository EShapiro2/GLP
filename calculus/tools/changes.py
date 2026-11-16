#!/usr/bin/env python3
"""changes.py OLD NEW [--out FILE] --- what a publication asks of an owner who adopted the one before it.

OLD and NEW are two publication directories as tools/publish.py writes them, one directory NAME/ for each contract,
with NAME/full-schemas.txt (sasx/), NAME/contract.txt (rev/) and NAME/full-contract.txt (exp/).  It prints, or writes
to FILE, CHANGES.txt: an entry for each contract, and a summary at its end.

An act of a contract is its own where the label of its line in full-schemas.txt names the contract (GSN:4, or
GSG:2+GSN:3, an act GSN strengthens), and inherited otherwise, from the contract its label names first.  For each
contract the entry gives
    its own acts added and gone, and renamed: an act of NEW whose schema is that of an act of OLD by another name,
        both names said;
    its inherited acts added, gone and renamed, apart, each with the contract it comes from;
    its own acts whose schema changed, the old line and the new;
    derived acts ('<Act> for <Role>', 'Vote of ...') counted in one line, not listed;
    'text only' where no act changed and only contract.txt or full-contract.txt differ, and 'unchanged' where
    nothing does.
Standard library only.
"""
import os, re, sys

LINE = re.compile(r"^((?:[A-Za-z-]+:\d+)(?:\+[A-Za-z-]+:\d+)*) ([■◆○-]) (.*)$")
DERIVED = re.compile(r"^(?:.+ for .+|Vote of .+)$")


def acts(path):
    """[(name, schema, labels)] of the act lines of a full-schemas.txt, in order; [] where there is none."""
    out = []
    if not os.path.exists(path):
        return out
    tp = os.path.join(os.path.dirname(path), 'term.glp.txt')      # an act the term leaves out, no binding enabling it, is no act (toglp.unenabled)
    term = open(tp, encoding='utf-8').read() if os.path.exists(tp) else None
    for l in open(path, encoding='utf-8'):
        m = LINE.match(l.rstrip('\n'))
        if not m or m.group(3).startswith('| '):
            continue
        name, _, schema = m.group(3).partition(': ')
        f = re.sub(r' +', '_', name.lower().strip().replace("'", ''))      # the schema's name, as toglp.ident
        if term is not None and not re.search(r"sas\('?%s(?:_\d+)?'?, " % re.escape(f), term):
            continue
        out.append((name, schema, m.group(1).split('+')))
    return out


def text(path):
    return open(path, encoding='utf-8').read() if os.path.exists(path) else None


def own_of(n, labels):
    return any(x.split(':')[0] == n for x in labels)


def compare(n, old, new):
    """The changes of contract n's acts of one kind (own or inherited) from old to new, each a list of acts."""
    on, nn = {}, {}
    for a in old: on.setdefault(a[0], []).append(a)
    for a in new: nn.setdefault(a[0], []).append(a)
    added = [a for a in new if a[0] not in on]
    gone = [a for a in old if a[0] not in nn]
    renamed = []
    for a in list(added):
        b = next((b for b in gone if b[1] == a[1]), None)       # the same schema under another name
        if b:
            renamed.append((b, a)); added.remove(a); gone.remove(b)
    changed = []
    for k in nn:
        if k in on:
            for b, a in zip(on[k], nn[k]):
                if b[1] != a[1]:
                    changed.append((b, a))
    return added, gone, renamed, changed


def entry(n, od, nd):
    """The lines of contract n's entry, and its case for the summary."""
    po, pn = os.path.join(od, n), os.path.join(nd, n)
    if not os.path.isdir(po):
        return ['%s: new in this publication' % n], 'new'
    if not os.path.isdir(pn):
        return ['%s: no longer published' % n], 'gone'
    ao, an = acts(os.path.join(po, 'full-schemas.txt')), acts(os.path.join(pn, 'full-schemas.txt'))
    ao = [a for a in ao if not DERIVED.match(a[0])]
    an = [a for a in an if not DERIVED.match(a[0])]
    own_o, own_n = [a for a in ao if own_of(n, a[2])], [a for a in an if own_of(n, a[2])]
    inh_o, inh_n = [a for a in ao if not own_of(n, a[2])], [a for a in an if not own_of(n, a[2])]
    out = []
    o_add, o_gone, o_ren, o_chg = compare(n, own_o, own_n)
    i_add, i_gone, i_ren, i_chg = compare(n, inh_o, inh_n)
    src = lambda a: a[2][0].split(':')[0]
    for a in o_add: out.append('  own act added: %s' % a[0])
    for a in o_gone: out.append('  own act gone: %s' % a[0])
    for b, a in o_ren: out.append('  own act renamed: %s, now %s' % (b[0], a[0]))
    for b, a in o_chg:
        out.append('  own act changed: %s' % a[0])
        out.append('    was: %s' % b[1])
        out.append('    now: %s' % a[1])
    for a in i_add: out.append('  inherited act added: %s, from %s' % (a[0], src(a)))
    for a in i_gone: out.append('  inherited act gone: %s, from %s' % (a[0], src(a)))
    for b, a in i_ren: out.append('  inherited act renamed: %s, now %s, from %s' % (b[0], a[0], src(a)))
    if i_chg:
        out.append('  inherited acts changed: %d (%s)' % (len(i_chg), ', '.join('%s from %s' % (a[0], src(a)) for _, a in i_chg)))
    dn = [a for a in acts(os.path.join(pn, 'full-schemas.txt')) if DERIVED.match(a[0])]
    do = [a for a in acts(os.path.join(po, 'full-schemas.txt')) if DERIVED.match(a[0])]
    same = sorted((a[0], a[1]) for a in do) == sorted((a[0], a[1]) for a in dn)
    if (do or dn) and (out or not same):        # counted where the acts changed, or the derived acts themselves did
        out.append('  derived acts: %d, before %d%s' % (len(dn), len(do), '' if same else ', changed'))
    own = bool(o_add or o_gone or o_ren or o_chg)
    if out:
        return ['%s:' % n] + out, 'own' if own else 'inherited'
    if text(os.path.join(po, 'contract.txt')) != text(os.path.join(pn, 'contract.txt')) or \
       text(os.path.join(po, 'full-contract.txt')) != text(os.path.join(pn, 'full-contract.txt')):
        return ['%s: text only' % n], 'text'
    return ['%s: unchanged' % n], 'unchanged'


def changes(od, nd):
    names = sorted({d for d in os.listdir(od) if os.path.isdir(os.path.join(od, d, '')) and os.path.exists(os.path.join(od, d, 'full-schemas.txt'))} |
                   {d for d in os.listdir(nd) if os.path.isdir(os.path.join(nd, d, '')) and os.path.exists(os.path.join(nd, d, 'full-schemas.txt'))})
    lines, cases, owned = ['Changes from %s to %s, for an owner who adopted the first.' % (od, nd), ''], {}, []
    for n in names:
        e, c = entry(n, od, nd)
        lines.extend(e)
        cases.setdefault(c, []).append(n)
        if c == 'own':
            owned.append(n)
    words = [('own', 'own acts changed'), ('inherited', 'inherited acts changed only'), ('text', 'text only'),
             ('unchanged', 'unchanged'), ('new', 'new'), ('gone', 'no longer published')]
    lines += ['', 'Summary: %d contracts; ' % len(names) + '; '.join('%s %d' % (w, len(cases.get(c, []))) for c, w in words) + '.',
              'With an own act added, gone, renamed or changed: %s.' % (', '.join(owned) if owned else 'none')]
    return '\n'.join(lines) + '\n'


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    out = sys.argv[sys.argv.index('--out') + 1] if '--out' in sys.argv else None
    if out in args:
        args.remove(out)
    if len(args) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    s = changes(args[0], args[1])
    if out:
        open(out, 'w', encoding='utf-8').write(s)
    else:
        sys.stdout.write(s)
    return 0


if __name__ == '__main__':
    sys.exit(main())
