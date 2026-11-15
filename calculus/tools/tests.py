#!/usr/bin/env python3
"""tests.py --- the regression cases of the reviews: each tests/CASE/gr is a small set of contracts, and
tests/CASE/expect.txt what tools/gcsas.py prints for it and the schemas it writes.

    python3 tools/tests.py            run every case and compare
    python3 tools/tests.py --write    write expect.txt of every case from this run (read it before you commit it)
"""
import io, os, shutil, sys, contextlib
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
import gcsas


def run(case):
    d = os.path.join(ROOT, 'tests', case)
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
