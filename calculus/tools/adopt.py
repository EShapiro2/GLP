#!/usr/bin/env python3
"""adopt.py --- a contract adopted by its owner: its one file in GLP, and what the Calculus's compilers print from it.

    python3 adopt.py --write NAME DIR   writes DIR/NAME.txt, the adopted file, from the Calculus's text of NAME and the
                                        owner's NAME.txt in GLP ($GLP_DIR), keeping its % lines and its Interface pragmas
    python3 adopt.py --paths            writes PATHS.txt beside this directory: a line "NAME path" for each contract, the
                                        path in GLP of its adopted file, or calculus/NAME/ where GLP has none
    python3 adopt.py NAME               writes NAME.glp beside NAME's adopted file, and schemas.txt, full-contract.txt,
                                        full-pragmas.txt, flat.txt, full-schemas.txt and term.glp.txt into cascade/NAME/
                                        of GLP, compiled from the contracts of PATHS.txt
    python3 adopt.py --check NAME       prints nothing and exits 0 where those files are what it would write; else
                                        names each that differs, and exits 1
    python3 adopt.py --split-check      writes every contract's adopted file into a temporary directory and checks
                                        that it splits back into its contract and its Logic pragmas, byte for byte;
                                        prints the count and the contracts that do not, and exits 1 if there is one

THE ADOPTED FILE, programs/<dir>/NAME.txt, line by line:
1. the owner's lines that begin with %, as they are;
2. the contract, the lines of the Calculus's contract.txt (rev/NAME.txt): title, first line, definitions, names line,
   clauses;
3. the line "Logic pragmas";
4. the Logic pragmas, the lines of logic-pragmas.txt (gr/NAME.txt) after its first line, the title, which is the
   contract's; its own line "Logic pragmas.", where it has one, stays where it is, between the contract written in
   the grammar and the vocabulary, so that the file splits back into the two, byte for byte;
5. where the owner has Interface pragmas, the line "Interface pragmas" and the owner's lines after its heading of
   pragmas ("Pragmas" or "Interface pragmas"), Implementation pragmas included, as they are.
It splits back: the contract is the lines from the first that does not begin with % up to "Logic pragmas"; the Logic
pragmas are the title and the lines after "Logic pragmas" up to "Interface pragmas" or the end.  No line of a
contract or of its Logic pragmas is either heading.

NAME.glp is the full schemas, the term of term.glp.txt, complete in itself, in the form of programs/gsc/README.md,
"Writing a contract": procedure contract_named(Constant?, SDeclared) and one clause, contract_named(name, Term), the
name in lower case, every other character an underscore.  A bare term would not load: programs/gsc, the checker,
is one project, and a NAME.glp there is one of its modules.

It reads GLP at $GLP_DIR, or a clone named GLP beside the directory above this one.  It runs from tools/ on the branch
code and from calculus/tools/ in GLP: it uses the gcsas.py, toglp.py, expand.py and revtex.py beside it, and builds
their rev/ and gr/ in a temporary directory from every contract of PATHS.txt, so that each compiles as it does in the
Calculus.  Python's standard library and nothing else.
"""
import os, re, shutil, subprocess, sys, tempfile

TOOLS = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(TOOLS)
GLP = os.environ.get('GLP_DIR') or next((d for d in (os.path.join(os.path.dirname(ROOT), 'GLP'), os.path.join(os.path.dirname(os.path.dirname(ROOT)), 'GLP'), '/home/user/GLP') if os.path.isdir(os.path.join(d, 'programs'))), None)
PATHS = os.path.join(ROOT, 'PATHS.txt')
LP, IP = 'Logic pragmas', 'Interface pragmas'
IFACE = ('Pragmas', 'Interface pragmas')
OUTS = [('sas', '%s.txt', 'schemas.txt'), ('exp', '%s.txt', 'full-contract.txt'), ('gx', '%s.txt', 'full-pragmas.txt'),
        ('flat', '%s.txt', 'flat.txt'), ('sasx', '%s.txt', 'full-schemas.txt'), ('glp', '%s.glp.txt', 'term.glp.txt')]


def die(msg):
    sys.stderr.write('adopt.py: %s\n' % msg)
    sys.exit(2)


def read(p):
    with open(p, encoding='utf-8', newline='') as f:
        return f.read()


def lines(text):
    return text.split('\n')[:-1] if text.endswith('\n') else text.split('\n')


def owner_file(name):
    """The owner's NAME.txt in GLP: programs/**/NAME.txt with a '% Owner' line, or None."""
    for d, _, fs in os.walk(os.path.join(GLP, 'programs')):
        if name + '.txt' in fs:
            p = os.path.join(d, name + '.txt')
            if re.search(r'^% Owner', read(p), re.M):
                return p
    return None


def adopted(text):
    return LP in lines(text)


def split(text):
    """An adopted file as (percent lines, contract text, logic pragmas text, interface lines)."""
    ls = lines(text)
    i = 0
    while i < len(ls) and ls[i].startswith('%'):
        i += 1
    k = ls.index(LP)
    j = ls.index(IP) if IP in ls[k:] else len(ls)
    rev = ls[i:k]
    gr = rev[:1] + ls[k + 1:j]
    return ls[:i], '\n'.join(rev) + '\n', '\n'.join(gr) + '\n', ls[j + 1:] if j < len(ls) else None


def interface_of(text):
    """The % lines and the interface pragmas (the lines after the heading) of an owner's NAME.txt, adopted or not."""
    ls = lines(text)
    pct = [l for l in ls[:next((i for i, l in enumerate(ls) if not l.startswith('%')), len(ls))]]
    if adopted(text):
        return split(text)[0], split(text)[3]
    h = next((i for i, l in enumerate(ls) if l in IFACE), None)
    return pct, (ls[h + 1:] if h is not None else None)


def compose(pct, rev, gr, iface):
    gl = lines(gr)
    out = list(pct) + lines(rev) + [LP] + gl[1:]
    if iface is not None:
        out += [IP] + list(iface)
    return '\n'.join(out) + '\n'


def calculus_text(name):
    """The Calculus's contract.txt and logic-pragmas.txt of NAME: from rev/ and gr/ beside the tools, on the branch
    code, or from calculus/NAME/ in GLP."""
    for c, g in ((os.path.join(ROOT, 'rev', name + '.txt'), os.path.join(ROOT, 'gr', name + '.txt')),
                 (os.path.join(ROOT, name, 'contract.txt'), os.path.join(ROOT, name, 'logic-pragmas.txt')),
                 (os.path.join(GLP, 'calculus', name, 'contract.txt'), os.path.join(GLP, 'calculus', name, 'logic-pragmas.txt'))):
        if os.path.exists(c) and os.path.exists(g):
            return read(c), read(g)
    die('no text of %s in rev/ and gr/, in calculus/%s/, or in %s/calculus/%s/' % (name, name, GLP, name))


def write_adopted(name, out):
    rev, gr = calculus_text(name)
    o = owner_file(name)
    pct, iface = interface_of(read(o)) if o else ([], None)
    text = compose(pct, rev, gr, iface)
    s = split(text)
    if s[1] != rev or s[2] != gr:
        die('%s: the adopted file does not split back into its contract and its Logic pragmas' % name)
    os.makedirs(out, exist_ok=True)
    with open(os.path.join(out, name + '.txt'), 'w', encoding='utf-8', newline='') as f:
        f.write(text)


def all_names():
    names = set()
    for base in (os.path.join(ROOT, 'rev'), ROOT, os.path.join(GLP, 'calculus')):
        if os.path.isdir(base):
            for f in os.listdir(base):
                if base.endswith('rev'):
                    if f.endswith('.txt') and not f.startswith('NOTES'):
                        names.add(f[:-4])
                elif os.path.exists(os.path.join(base, f, 'contract.txt')):
                    names.add(f)
    return sorted(names)


def write_paths():
    out = []
    for n in all_names():
        o = owner_file(n)
        out.append('%s %s' % (n, os.path.relpath(o, GLP) if o and adopted(read(o)) else 'calculus/%s/' % n))
    with open(PATHS, 'w', encoding='utf-8') as f:
        f.write('\n'.join(out) + '\n')


def paths():
    if not os.path.exists(PATHS):
        die('no %s: run adopt.py --paths first' % PATHS)
    return dict(l.split(None, 1) for l in read(PATHS).split('\n') if l.strip())


def texts(name, path):
    """The contract and Logic pragmas of a line of PATHS.txt: an adopted file in GLP, or else the Calculus's text
    (calculus_text: rev/ and gr/ on the branch code, calculus/NAME/ in GLP)."""
    p = os.path.join(GLP, path.strip())
    if p.endswith('.txt'):
        _, rev, gr, _ = split(read(p))
        return rev, gr
    return calculus_text(name)


def compiled(name):
    """{file: text} of what NAME compiles to, from a temporary rev/ and gr/ of every contract of PATHS.txt."""
    ps = paths()
    if name not in ps:
        die('%s is not in %s' % (name, PATHS))
    tmp = tempfile.mkdtemp(prefix='adopt-')
    try:
        for d in ('rev', 'gr', 'tools'):
            os.makedirs(os.path.join(tmp, d))
        for n, p in ps.items():
            rev, gr = texts(n, p)
            for d, t in (('rev', rev), ('gr', gr)):
                with open(os.path.join(tmp, d, n + '.txt'), 'w', encoding='utf-8', newline='') as f:
                    f.write(t)
        for t in ('gcsas.py', 'toglp.py', 'expand.py', 'revtex.py'):
            shutil.copy(os.path.join(TOOLS, t), os.path.join(tmp, 'tools', t))
        run = lambda *a: subprocess.run([sys.executable] + list(a), cwd=tmp, capture_output=True, text=True)
        steps = [run('tools/gcsas.py', name), run('tools/expand.py', '--out', os.path.join(tmp, 'expanded.tex')), run('tools/toglp.py', name)]
        out = {}
        for d, pat, f in OUTS:
            p = os.path.join(tmp, d, pat % name)
            if not os.path.exists(p):
                die('%s: no %s printed; %s' % (name, f, ' | '.join((r.stdout + r.stderr).strip()[-300:] for r in steps)))
            out[f] = read(p)
        return out, ps[name].strip()
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def targets(name):
    out, path = compiled(name)
    files = {os.path.join(GLP, 'cascade', name, f): t for f, t in out.items()}
    if path.endswith('.txt'):
        files[os.path.join(GLP, os.path.dirname(path), name + '.glp')] = module(name, out['term.glp.txt'])
    return files


def module(name, term):
    """NAME.glp: the full schemas, the term of term.glp.txt, in the form of programs/gsc/README.md, "Writing a
    contract": a file of its own, procedure contract_named(Constant?, SDeclared), one clause returning the contract
    under its name, the name in lower case with every other character an underscore, as cascade.py names it."""
    ident = re.sub(r'[^a-z0-9]', '_', name.lower())
    return ('%%%% %s.glp --- printed by adopt.py from %s.txt beside this file: the contract\'s full schemas, the term of\n'
            '%%%% term.glp.txt in cascade/%s/, complete in itself.  Not to be edited: edit %s.txt and run adopt.py %s.\n'
            '%% contract_named(%s, C): the contract, %s.\n'
            'procedure contract_named(Constant?, SDeclared).\n'
            'contract_named(%s, %s).\n') % (name, name, name, name, name, ident, name, ident, term.rstrip('\n'))


def main():
    a = sys.argv[1:]
    if not GLP:
        die('no clone of GLP: set GLP_DIR')
    if a[:1] == ['--write'] and len(a) == 3:
        write_adopted(a[1], a[2])
    elif a == ['--paths']:
        write_paths()
    elif a == ['--split-check']:
        bad, names = [], all_names()
        for n in names:
            rev, gr = calculus_text(n)
            o = owner_file(n)
            pct, iface = interface_of(read(o)) if o else ([], None)
            s = split(compose(pct, rev, gr, iface))
            if s[1] != rev or s[2] != gr:
                bad.append(n)
        print('%d contracts, %d that do not split back%s' % (len(names), len(bad), (': ' + ' '.join(bad)) if bad else ''))
        sys.exit(1 if bad else 0)
    elif a[:1] == ['--check'] and len(a) == 2:
        bad = [p for p, t in targets(a[1]).items() if not os.path.exists(p) or read(p) != t]
        for p in bad:
            print(os.path.relpath(p, GLP))
        sys.exit(1 if bad else 0)
    elif len(a) == 1 and not a[0].startswith('-'):
        for p, t in targets(a[0]).items():
            os.makedirs(os.path.dirname(p), exist_ok=True)
            with open(p, 'w', encoding='utf-8', newline='') as f:
                f.write(t)
    else:
        die('usage: adopt.py --write NAME DIR | --paths | NAME | --check NAME | --split-check')


if __name__ == '__main__':
    main()
