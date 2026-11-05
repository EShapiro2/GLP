"""A package's warranties, by core/bin/warrant.dart (core/lib/warranty.dart):
the compiler's, GLP's certificate of the contract unit contract/, and the
creator's, a signed creator_cert over the unit, the contract's schemas and the
checker's verdict.

warranties([{id, files, text, schemas, verdict}, ...]) -> [warranty or error]
DART: the dart command (default dart).  The keys: GAS_COMPILER_KEY and
GAS_CREATOR_KEY (warrant.dart's defaults, servers/gas/keys/).
"""
import json
import os
import shlex
import subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
CORE = os.path.join(os.path.dirname(HERE), 'core')

# The files of a package: the harness and its contract unit.
FILES = ('self.glp', 'contract/self.glp', 'contract/platform.vglp')


class WarrantError(Exception):
    pass


def warranties(requests, timeout=1800):
    cmd = shlex.split(os.environ.get('DART', 'dart'), posix=os.name != 'nt')
    p = subprocess.run(cmd + ['run', os.path.join('bin', 'warrant.dart')], cwd=CORE,
                       input=json.dumps(requests), capture_output=True, text=True,
                       encoding='utf-8', timeout=timeout)
    out = p.stdout.split('%% warranties\n', 1)
    if p.returncode != 0 or len(out) != 2:
        raise WarrantError('the warranties were not written: ' +
                           ' '.join((p.stderr or p.stdout).strip().splitlines()[-3:]))
    answers = json.loads(out[1])
    return [a.get('warranty') or WarrantError(a.get('error', 'no warranty')) for a in answers]
