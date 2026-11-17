#!/usr/bin/env python3
"""adopt.py --- a contract adopted by its owner: its one file in GLP, and what the Calculus's compilers print from it.

    python3 adopt.py --write NAME DIR   writes DIR/NAME.txt, the adopted file, from the Calculus's text of NAME and the
                                        owner's NAME.txt in GLP ($GLP_DIR), keeping its % lines and its Interface pragmas
    python3 adopt.py --paths            writes PATHS.txt beside this directory: a line "NAME path" for each contract, the
                                        path in GLP of its adopted file, programs/<dir>/NAME.txt with a % Owner line and
                                        a line "Logic pragmas", or calculus/NAME/ where GLP has none
    python3 adopt.py NAME               writes NAME.glp beside NAME's adopted file, and into cascade/NAME/ of GLP NAME.txt,
                                        a copy of the adopted file, NAME.glp, schemas.txt, full-contract.txt,
                                        full-pragmas.txt, flat.txt and full-schemas.txt, compiled from the contracts of
                                        PATHS.txt, or, where there is none, of the paths --paths would write, and
                                        relations.txt, the verdict of each relation of its first line on those
                                        schemas, as tools/relations.py prints it
    python3 adopt.py --check NAME       prints nothing and exits 0 where those files are what it would write; else
                                        names each that differs, and exits 1
    --out DIR, after NAME or --check NAME: the files are written, or checked, under DIR in place of the checkout
                                        (DIR/cascade/NAME/, DIR/<dir>/NAME.glp)
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
   pragmas ("Pragmas" or "Interface pragmas"), Implementation pragmas included, as they are.  Under the heading
   "Implementation pragmas" a line "Introduction: PRED." names the contract's introduction predicate, PRED a basic
   state's predicate, as knows; at most one such line.
It splits back: the contract is the lines from the first that does not begin with % up to "Logic pragmas"; the Logic
pragmas are the title and the lines after "Logic pragmas" up to "Interface pragmas" or the end.  No line of a
contract or of its Logic pragmas is either heading.

NAME.glp is the full schemas, the term of term.glp.txt, complete in itself, in the form of programs/gsc/README.md,
"Writing a contract": procedure contract_named(Constant?, SDeclared) and one clause, contract_named(name, Term), the
name in lower case, every other character an underscore.  A bare term would not load: programs/gsc, the checker,
is one project, and a NAME.glp there is one of its modules.  Where the adopted file has the line "Introduction: PRED."
under "Implementation pragmas", NAME.glp also has, after it, the type Intro, procedure introduction_named(Constant?,
Intro) and introduction_named(name, introduction(PRED)), as a NAME.glp written by hand had them, which cascade.sh and
the compile.sh of programs/cssn and programs/apps read; with no such line, none, and those read no_introduction.

A contract not adopted is read from calculus/NAME/ in the checkout, or, where the checkout has no calculus/NAME/, as
on the branches migration and main, from calculus/NAME/ of the branch Calculus (origin/Calculus or Calculus) of the
clone, by git show; with that branch not fetched, it names the contract as missing and the command that fetches it.

It reads GLP at $GLP_DIR, the checkout this calculus/ is in, or a clone named GLP beside the directory above this one.  It runs from tools/ on the branch
code and from calculus/tools/ in GLP: it uses the gcsas.py, toglp.py, expand.py and revtex.py beside it, and builds
their rev/ and gr/ in a temporary directory from NAME and every contract it is written from, by the first lines, so
that it compiles as it does in the Calculus.  Python's standard library and nothing else.
"""
import os, re, shutil, subprocess, sys, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM      # the names of the contracts and their files (tools/names.py, NAMES.txt)

TOOLS = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(TOOLS)
GLP = os.environ.get('GLP_DIR') or next((d for d in (os.path.dirname(ROOT), os.path.join(os.path.dirname(ROOT), 'GLP'), os.path.join(os.path.dirname(os.path.dirname(ROOT)), 'GLP'), '/home/user/GLP') if os.path.isdir(os.path.join(d, 'programs'))), None)
PATHS = os.path.join(ROOT, 'PATHS.txt')
CALC_REFS = ('origin/Calculus', 'Calculus')      # the branch of GLP where calculus/NAME/ of every contract is
LP, IP = 'Logic pragmas', 'Interface pragmas'
IMPL = 'Implementation pragmas'
INTRO = re.compile(r'^Introduction\s*:\s*([a-z][a-z0-9_]*)\.?\s*$')
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


_OWNERS = None


def owners():
    """{NAME: path} of every programs/**/NAME.txt in GLP with a '% Owner' line, the first found."""
    global _OWNERS
    if _OWNERS is None:
        _OWNERS = {}
        for d, ds, fs in os.walk(os.path.join(GLP, 'programs')):
            ds.sort()
            for f in sorted(fs):
                if f.endswith('.txt'):
                    p = os.path.join(d, f)
                    try:
                        with open(p, encoding='utf-8') as fh:
                            head = fh.read(8192)
                    except (OSError, UnicodeDecodeError):
                        continue
                    if re.search(r'^% Owner', head, re.M):
                        _OWNERS.setdefault(f[:-4], p)
    return _OWNERS


GLP_NAME = NM.glp_name      # renamed (Udi, 2026-11-16, NAMES.txt): the owner's file in GLP keeps its name on main, the first column, until Code renames it


def owner_file(name):
    """The owner's NAME.txt in GLP: programs/**/NAME.txt with a '% Owner' line, or None; for a contract renamed, its
    file under the old name until Code renames it (GLP_NAME)."""
    return owners().get(NM.file_of(name)) or owners().get(GLP_NAME(name))


def adopted_file(name):
    """The owner's NAME.txt where it is in the adopted form, a line 'Logic pragmas' in it; else None."""
    o = owner_file(name)
    return o if o and adopted(read(o)) else None


def git(*a):
    """git in the clone of GLP; where GLP is a scratch directory with calculus/ linked into it, as cascade.sh makes it,
    in the clone that calculus/ is in."""
    c = os.path.join(GLP, 'calculus')
    where = os.path.dirname(os.path.realpath(c)) if os.path.exists(c) else GLP
    return subprocess.run(['git', '-C', where] + list(a), capture_output=True)


def calc_ref():
    """The branch Calculus in GLP's clone, origin/Calculus or Calculus; None where the clone has neither."""
    return next((r for r in CALC_REFS if git('rev-parse', '--verify', '-q', r + ':calculus').returncode == 0), None)


def calc_names(ref):
    """The contracts of the branch Calculus: those with calculus/NAME/contract.txt."""
    r = git('ls-tree', '-r', '--name-only', ref, 'calculus/')
    return {NM.of_glp(l.split('/')[1]) for l in r.stdout.decode('utf-8').split('\n') if l.endswith('/contract.txt') and l.count('/') == 2}


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
    code; from calculus/NAME/ in the checkout of GLP; or else from calculus/NAME/ of GLP's branch Calculus, by git."""
    for c, g in ((os.path.join(ROOT, 'rev', NM.file_of(name) + '.txt'), os.path.join(ROOT, 'gr', NM.file_of(name) + '.txt')),
                 (os.path.join(ROOT, NM.file_of(name), 'contract.txt'), os.path.join(ROOT, NM.file_of(name), 'logic-pragmas.txt')),
                 (os.path.join(GLP, 'calculus', NM.file_of(name), 'contract.txt'), os.path.join(GLP, 'calculus', NM.file_of(name), 'logic-pragmas.txt')),
                 (os.path.join(GLP, 'calculus', GLP_NAME(name), 'contract.txt'), os.path.join(GLP, 'calculus', GLP_NAME(name), 'logic-pragmas.txt'))):
        if os.path.exists(c) and os.path.exists(g):
            return read(c), read(g)
    ref = calc_ref()
    for d in ((NM.file_of(name), GLP_NAME(name)) if ref else ()):
        c, g = git('show', '%s:calculus/%s/contract.txt' % (ref, d)), git('show', '%s:calculus/%s/logic-pragmas.txt' % (ref, d))
        if c.returncode == 0 and g.returncode == 0:
            return c.stdout.decode('utf-8'), g.stdout.decode('utf-8')
    if ref:
        die('no text of %s: not adopted in %s, and not in calculus/%s/ of the checkout or of %s' % (name, GLP, name, ref))
    die('no text of %s: not adopted in %s, and no calculus/%s/ in the checkout; fetch the branch Calculus: git -C %s fetch origin Calculus' % (name, GLP, name, GLP))


def write_adopted(name, out):
    rev, gr = calculus_text(name)
    o = owner_file(name)
    pct, iface = interface_of(read(o)) if o else ([], None)
    text = compose(pct, rev, gr, iface)
    s = split(text)
    if s[1] != rev or s[2] != gr:
        die('%s: the adopted file does not split back into its contract and its Logic pragmas' % name)
    os.makedirs(out, exist_ok=True)
    with open(os.path.join(out, NM.file_of(name) + '.txt'), 'w', encoding='utf-8', newline='') as f:
        f.write(text)


def all_names():
    """Every contract: the adopted ones in GLP, those of rev/ beside the tools, of calculus/*/ in the checkout, and of
    the branch Calculus."""
    names = {NM.of_glp(n) for n in owners() if adopted_file(NM.of_glp(n))}
    ref = calc_ref()
    if ref:
        names |= calc_names(ref)
    for base in (os.path.join(ROOT, 'rev'), ROOT, os.path.join(GLP, 'calculus')):
        if os.path.isdir(base):
            for f in os.listdir(base):
                if base.endswith('rev'):
                    if f.endswith('.txt') and not f.startswith('NOTES-'):
                        names.add(NM.name_of(f[:-4]))
                elif os.path.exists(os.path.join(base, f, 'contract.txt')):
                    names.add(NM.of_glp(f))
    return sorted(names)


def computed_paths():
    """{NAME: path}: the adopted file, programs/<dir>/NAME.txt, where GLP has it; else calculus/NAME/."""
    out = {}
    for n in all_names():
        a = adopted_file(n)
        out[n] = os.path.relpath(a, GLP) if a else 'calculus/%s/' % NM.file_of(n)
    return out


def write_paths():
    with open(PATHS, 'w', encoding='utf-8') as f:
        f.write(''.join('%s %s\n' % (NM.file_of(n), p) for n, p in sorted(computed_paths().items())))      # a line by the file name, a name having a space


def paths():
    """PATHS.txt, where --paths wrote one; else the paths as --paths would write them, worked out now."""
    if not os.path.exists(PATHS):
        return computed_paths()
    return {NM.name_of(l.split(None, 1)[0]): l.split(None, 1)[1] for l in read(PATHS).split('\n') if l.strip()}


def texts(name, path):
    """The contract and Logic pragmas of a line of PATHS.txt: an adopted file in GLP, or else the Calculus's text
    (calculus_text: rev/ and gr/ on the branch code, calculus/NAME/ in GLP)."""
    p = os.path.join(GLP, path.strip())
    if p.endswith('.txt'):
        _, rev, gr, _ = split(read(p))
        return rev, gr
    return calculus_text(name)


def written_from(name, ps):
    """{NAME: (contract, Logic pragmas)} of NAME and every contract it is written from, by the first lines, each from
    its line of the paths."""
    out, todo = {}, [name]
    while todo:
        n = todo.pop()
        if n in out:
            continue
        if n not in ps:
            die('%s, which a contract compiled is written from, is not among the contracts found' % n)
        out[n] = texts(n, ps[n])
        ls = lines(out[n][0])
        if len(ls) > 1 and re.match(re.escape(n) + r' (extends|restricts|strengthens|instantiates|composes|relaxes)\b', ls[1]):
            first = re.sub(r' \([^()]*\)', '', ls[1])
            todo += [w for _, _, w in NM.find(first[len(n):], ps)]      # the known names, longest first (tools/names.py)
    return out


def compiled(name):
    """{file: text} of what NAME compiles to, from a temporary rev/ and gr/ of NAME and the contracts it is written from."""
    ps = paths()
    if name not in ps:
        die('%s is not in %s: not adopted in %s%s' % (name, PATHS if os.path.exists(PATHS) else 'the contracts found', GLP,
            '' if calc_ref() else '; its text is on the branch Calculus, not in this clone: git -C %s fetch origin Calculus, then adopt.py --paths' % GLP))
    return compile_tree(name, written_from(name, ps)), ps[name].strip()


def compile_tree(name, texts):
    """{file: text} of what NAME compiles to, the Calculus's compilers run in a temporary directory on TEXTS,
    {contract: (rev, gr)}, NAME and the contracts it is written from."""
    tmp = tempfile.mkdtemp(prefix='adopt-')
    try:
        for d in ('rev', 'gr', 'tools'):
            os.makedirs(os.path.join(tmp, d))
        for n, (rev, gr) in texts.items():
            for d, t in (('rev', rev), ('gr', gr)):
                with open(os.path.join(tmp, d, NM.file_of(n) + '.txt'), 'w', encoding='utf-8', newline='') as f:
                    f.write(t)
        for t in ('gcsas.py', 'toglp.py', 'expand.py', 'revtex.py', 'names.py'):
            shutil.copy(os.path.join(TOOLS, t), os.path.join(tmp, 'tools', t))
        shutil.copy(os.path.join(TOOLS, '..', 'NAMES.txt'), os.path.join(tmp, 'NAMES.txt'))      # the names (tools/names.py)
        run = lambda *a: subprocess.run([sys.executable] + list(a), cwd=tmp, capture_output=True, text=True)
        out = {}
        for step in (('tools/gcsas.py', name), ('tools/expand.py', '--out', os.path.join(tmp, 'expanded.tex')), ('tools/toglp.py', name)):
            r = run(*step)          # a step that fails stops it: nothing it printed is taken
            if r.returncode != 0:
                ls = [l.replace(tmp, '.') for l in (r.stdout + r.stderr).split('\n') if l.strip()]
                why = [l for l in ls if l.startswith('FAIL') or 'ERROR' in l or ' NO' in l or 'not translated' in l or 'Traceback' in l] or ls[-1:]
                die('%s: %s failed, exit %d; nothing written: %s' % (name, os.path.basename(step[0]), r.returncode, ' | '.join(why)[-400:]))
        steps = []
        for d, pat, f in OUTS:
            p = os.path.join(tmp, d, pat % NM.file_of(name))
            if not os.path.exists(p):
                die('%s: no %s printed; %s' % (name, f, ' | '.join((r.stdout + r.stderr).strip()[-300:] for r in steps)))
            out[f] = read(p)
        sys.path.insert(0, TOOLS)
        import relations
        out['relations.txt'] = relations.text(name, tmp)      # the verdict of each relation of the first line, on these schemas
        return out
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def targets(name, root=None):
    """What adopt.py NAME writes: NAME.glp beside the adopted file; and into cascade/NAME/, under the owner's names,
    NAME.txt, a copy of the adopted file as it stands, NAME.glp, and the five files of the Calculus's compilers.  The
    rest of the record of a run (verdict.txt, vmat.tex, platform.vglp, platform.glp, run.log, interface.glp,
    manifest.md) is cascade.sh's.  A contract not yet adopted has no NAME.txt and no NAME.glp beside it."""
    root = root or GLP
    out, path = compiled(name)
    intro = introduction(name, read(os.path.join(GLP, path))) if path.endswith('.txt') else None
    glp = module(name, out.pop('term.glp.txt'), intro)      # the term goes into NAME.glp, and term.glp.txt is not written
    rec = os.path.join(root, 'cascade', NM.file_of(name))
    files = {os.path.join(rec, f): t for f, t in out.items()}
    files[os.path.join(rec, NM.file_of(name) + '.glp')] = glp
    if path.endswith('.txt'):
        files[os.path.join(root, os.path.dirname(path), NM.file_of(name) + '.glp')] = glp
        files[os.path.join(rec, NM.file_of(name) + '.txt')] = read(os.path.join(GLP, path))
    return files


def introduction(name, text):
    """The introduction predicate of an adopted file: PRED of its line "Introduction: PRED." under the heading
    "Implementation pragmas", among the lines after "Interface pragmas"; None where it has none."""
    iface = split(text)[3] or []
    found, impl = [], False
    for l in iface:
        s = l.strip()
        if s == IMPL:
            impl = True
        elif s in IFACE:
            impl = False
        elif impl and s.startswith('Introduction'):
            m = INTRO.match(s)
            if not m:
                die('%s: the line "%s" of its Implementation pragmas is not "Introduction: PRED.", PRED a predicate' % (name, s))
            found.append(m.group(1))
    if len(found) > 1:
        die('%s: %d lines "Introduction: ..." in its Implementation pragmas; a contract has one introduction predicate' % (name, len(found)))
    return found[0] if found else None


def module(name, term, intro=None):
    """NAME.glp: the full schemas, the Calculus's term.glp.txt, in the form of programs/gsc/README.md, "Writing a
    contract": a file of its own, procedure contract_named(Constant?, SDeclared), one clause returning the contract
    under its name, the name in lower case with every other character an underscore, as cascade.py names it."""
    ident = NM.atom_of(name)      # the fourth column of NAMES.txt
    head = ('%%%% %s.glp --- printed by adopt.py from %s.txt: the contract\'s full schemas, the Calculus\'s term.glp.txt,\n'
            '%%%% complete in itself.  Not to be edited: edit %s.txt and run adopt.py %s.\n' % (NM.file_of(name), NM.file_of(name), NM.file_of(name), "'%s'" % name if ' ' in name else name))
    body = head + ('%% contract_named(%s, C): the contract, %s.\n'
                   'procedure contract_named(Constant?, SDeclared).\n'
                   'contract_named(%s, %s).\n' % (ident, name, ident, term.rstrip('\n')))
    if intro:
        body += ('\n%% introduction_named(%s, I): the contract\'s introduction predicate, %s, from the line "Introduction: %s."\n'
                 '%% of its Implementation pragmas, in the form of programs/vglp/cva/contracts.glp.\n'
                 'Intro ::= no_introduction ; introduction(Constant).\n'
                 'procedure introduction_named(Constant?, Intro).\n'
                 'introduction_named(%s, introduction(%s)).\n' % (ident, intro, intro, ident, intro))
    return body


def main():
    a = sys.argv[1:]
    root = None
    if '--out' in a:                    # --out DIR: write, or check, the files under DIR in place of the checkout
        i = a.index('--out')
        if i + 1 >= len(a):
            die('--out needs a directory')
        root = os.path.abspath(a[i + 1])
        a = a[:i] + a[i + 2:]
    a = [x if x.startswith('-') else NM.name_of(x) for x in a]      # a contract is named by its name or by its file name: "Secure Friendship" or Secure-Friendship
    if a[:1] == ['--tree'] and len(a) == 3:       # the compile of NAME on DIR/rev and DIR/gr, a case of tests/
        d = a[1]
        names = NM.names_in(os.path.join(d, 'gr'))
        texts = {n: (read(os.path.join(d, 'rev', NM.file_of(n) + '.txt')), read(os.path.join(d, 'gr', NM.file_of(n) + '.txt'))) for n in names}
        out = compile_tree(a[2], texts)
        print('%s: compiled; printed %s' % (a[2], ', '.join(sorted(out))))
        glp = module(a[2], out['term.glp.txt'])      # NAME.glp: its file and the atom of contract_named, NAMES.txt's fourth column
        print('%s.glp: %s' % (NM.file_of(a[2]), ' '.join(l.split(', ')[0] + ', ...' for l in glp.split('\n') if l.startswith('contract_named('))))
        return
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
        bad = [p for p, t in targets(a[1], root).items() if not os.path.exists(p) or read(p) != t]
        for p in bad:
            print(os.path.relpath(p, root or GLP))
        sys.exit(1 if bad else 0)
    elif len(a) == 1 and not a[0].startswith('-'):
        for p, t in targets(a[0], root).items():
            os.makedirs(os.path.dirname(p), exist_ok=True)
            with open(p, 'w', encoding='utf-8', newline='') as f:
                f.write(t)
    else:
        die('usage: adopt.py --write NAME DIR | --paths | NAME [--out DIR] | --check NAME [--out DIR] | --split-check')


if __name__ == '__main__':
    main()
