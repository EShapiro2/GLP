#!/usr/bin/env python3
"""tests.py --- the regression cases of the reviews: each tests/CASE/gr is a small set of contracts, and
tests/CASE/expect.txt what tools/gcsas.py prints for it and the schemas it writes; a case with two publication
directories, tests/CASE/old and tests/CASE/new, is one of tools/changes.py, and expect.txt is what it prints.

    python3 tools/tests.py            run every case and compare
    python3 tools/tests.py --write    write expect.txt of every case from this run (read it before you commit it)
"""
import io, os, shutil, subprocess, sys, tempfile, contextlib
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import gcsas


def run(case):
    d = os.path.join(ROOT, 'tests', case)
    if os.path.isdir(os.path.join(d, 'old')) and os.path.isdir(os.path.join(d, 'new')):      # a case of tools/changes.py: two publications
        import changes
        cwd = os.getcwd()
        os.chdir(d)
        try:
            return changes.changes('old', 'new').rstrip('\n').split('\n')
        finally:
            os.chdir(cwd)
    gcsas.ROOT = d
    for x in ('sas', 'sasx', 'flat', 'gx', 'back'):     # what an earlier run wrote
        shutil.rmtree(os.path.join(d, x), ignore_errors=True)
    gcsas.NOUNS.clear()
    gcsas.binds_sas.__defaults__[0].clear()
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        sys.argv = ['gcsas.py']
        try:
            gcsas.main()
        except Exception as ex:
            print('EXCEPTION %s: %s' % (type(ex).__name__, ex))
    out = buf.getvalue().rstrip('\n').split('\n')
    sx = os.path.join(d, 'sasx')
    for f in sorted(os.listdir(sx)) if os.path.isdir(sx) else []:
        out += ['--- sasx/' + f] + [l.rstrip('\n') for l in open(os.path.join(sx, f), encoding='utf-8')]
    if os.path.exists(os.path.join(d, 'terms')) or os.path.exists(os.path.join(d, 'term')):      # a case of toglp.py: the term of each contract (F07, F09)
        import toglp
        root, toglp.ROOT = toglp.ROOT, d
        try:
            for f in sorted(os.listdir(sx)) if os.path.isdir(sx) else []:
                try:
                    out += ['--- term ' + f[:-4]] + toglp.contract_term(f[:-4])[0].split('\n')
                except Exception as ex:
                    out += ['--- term ' + f[:-4], 'EXCEPTION %s: %s' % (type(ex).__name__, ex)]
        finally:
            toglp.ROOT = root
    rev = os.path.join(d, 'rev')
    if os.path.isdir(rev):              # a case with the texts too: tools/expand.py's expanded texts, and its --check
        ex = os.path.join(d, 'exp')
        shutil.rmtree(ex, ignore_errors=True)
        tex = tempfile.mkdtemp(prefix='tests-')
        try:
            cmd = [sys.executable, os.path.join(ROOT, 'tools', 'expand.py'), '--rev', rev, '--exp', ex, '--out', os.path.join(tex, 'x.tex')]
            r = subprocess.run(cmd, capture_output=True, text=True)
            c = subprocess.run(cmd + ['--check'], capture_output=True, text=True)
        finally:
            shutil.rmtree(tex, ignore_errors=True)
        out += ['--- expand.py: exit %d; --check: exit %d' % (r.returncode, c.returncode)]
        out += ['FAIL: ' + l[len('FAIL: '):] for l in (r.stderr + c.stderr).split('\n') if l.startswith('FAIL: ')]
        for f in sorted(os.listdir(ex)) if os.path.isdir(ex) else []:
            out += ['--- exp/' + f] + [l.rstrip('\n') for l in open(os.path.join(ex, f), encoding='utf-8')]
    ad = os.path.join(d, 'adopt.txt')
    if os.path.exists(ad):              # a case of adopt.py: the compile of the contract it names, on the case's texts
        for n in open(ad, encoding='utf-8').read().split():
            r = subprocess.run([sys.executable, os.path.join(ROOT, 'tools', 'adopt.py'), '--tree', d, n], capture_output=True, text=True)
            out += ['--- adopt.py --tree %s: exit %d' % (n, r.returncode)] + [l for l in (r.stdout + r.stderr).split('\n') if l.strip()]
    return out


def main():
    bad, write = 0, '--write' in sys.argv
    for case in sorted(os.listdir(os.path.join(ROOT, 'tests'))):
        got, exp = run(case), os.path.join(ROOT, 'tests', case, 'expect.txt')
        if write:
            open(exp, 'w', encoding='utf-8').write('\n'.join(got) + '\n')
        ok = os.path.exists(exp) and open(exp, encoding='utf-8').read().rstrip('\n').split('\n') == got
        bad += not ok
        print('%-10s %s' % (case, 'same' if ok else 'DIFFERS'))
    print('%d cases, %d that differ' % (len(os.listdir(os.path.join(ROOT, 'tests'))), bad))
    return 1 if bad else 0


if __name__ == '__main__':
    args = sys.argv[1:]
    sys.exit(main())
