"""The GLP tools GAS2 runs: the REPL, and the steps of the cascade a contract takes
to become a mini-app of the Grassroots Super-App, as programs/apps/compile.sh takes
them (the contract, the reader, the checker, the pragmas, the compiler), then the
REPL's :artefact, which certifies a mini-app's directory.

Everything is read from a clone of the GLP repository, GLP; nothing here restates
any of GLP's programs."""
import os
import re
import shutil
import subprocess
import sys
import tempfile

IS_WIN = os.name == 'nt'


class Glp:
    def __init__(self, root, repl=None, bash=None):
        self.root = os.path.abspath(root)
        self.rt = os.path.join(self.root, 'glp_runtime')
        self.repl_exe = repl or os.environ.get('GLPC') or self._default_repl()
        self.bash = bash or os.environ.get('BASH') or self._find_bash()

    def _default_repl(self):
        for n in ('glp_repl.exe', 'glp_repl_exe'):
            p = os.path.join(self.rt, 'bin', n)
            if os.path.exists(p):
                return p
        return os.path.join(self.rt, 'bin', 'glp_repl.exe' if IS_WIN else 'glp_repl_exe')

    @staticmethod
    def _find_bash():
        if not IS_WIN:
            return shutil.which('bash') or '/bin/bash'
        for b in (r'C:\Program Files\Git\bin\bash.exe', r'C:\Program Files\Git\usr\bin\bash.exe'):
            if os.path.exists(b):
                return b
        return shutil.which('bash') or 'bash'

    def build_repl(self, dart='dart'):
        """The REPL, compiled ahead of time beside its source, whose place gives it its root."""
        subprocess.run([dart, 'pub', 'get'], cwd=self.rt, check=True, capture_output=True)
        subprocess.run([dart, 'compile', 'exe', os.path.join('bin', 'glp_repl.dart'), '-o', self.repl_exe],
                       cwd=self.rt, check=True, capture_output=True)

    def repl(self, lines, timeout=600):
        """Run one REPL session on the given input lines; its output lines, prompts stripped."""
        inp = '\n'.join(lines + [':quit']) + '\n'
        try:
            p = subprocess.run([self.repl_exe], input=inp, cwd=self.rt, capture_output=True,
                               text=True, encoding='utf-8', errors='replace', timeout=timeout)
            out = p.stdout + p.stderr
        except subprocess.TimeoutExpired as e:
            out = (e.stdout or b'').decode('utf-8', 'replace') if isinstance(e.stdout, bytes) else (e.stdout or '')
            out += '\n[timeout]'
        return [re.sub(r'^(GLP> )+', '', l) for l in out.split('\n')]

    @staticmethod
    def answer(lines, var):
        for l in lines:
            if l.startswith(var + ' = '):
                return l[len(var) + 3:].strip()
        return None

    def path(self, *p):
        return os.path.join(self.root, *p)

    # --- the cascade, as programs/apps/compile.sh takes it -----------------------------

    def contract(self, glp_file, name):
        return self.answer(self.repl([glp_file, f'contract_named({name}, C).']), 'C')

    def as_read(self, term):
        return self.answer(self.repl([self.path('programs', 'gsc') + os.sep, ':limit 10000000',
                                      f'as_read({term}, R).']), 'R')

    def verdict(self, term):
        return self.answer(self.repl([self.path('programs', 'gsc') + os.sep, ':limit 10000000',
                                      f'check({term}, V).']), 'V')

    def introduction(self, glp_file, name):
        src = open(glp_file, encoding='utf-8').read()
        if not re.search(rf'^introduction_named\({re.escape(name)},', src, re.M):
            return 'no_introduction'
        return self.answer(self.repl([glp_file, f'introduction_named({name}, I).']), 'I') or 'no_introduction'

    def pragmas(self, txt_file, read_term):
        """(pragmas term, None) or (None, the reason pragmas.sh gives)."""
        script = self.path('programs', 'vglp', 'cva', 'compiler', 'pragmas.sh')
        args = [self.bash, _posix(script, self.bash), _posix(txt_file, self.bash), read_term]
        p = subprocess.run(args, capture_output=True, text=True, encoding='utf-8', errors='replace')
        if p.returncode != 0:
            return None, (p.stderr.strip().split('\n') or ['pragmas not read'])[0]
        return p.stdout.strip(), None

    def compile_platform(self, name, read_term, verdict, intro, pragmas):
        """(platform text, None) or (None, why not), as compile.sh decides it."""
        out = self.repl([self.path('programs', 'vglp', 'cva', 'compiler') + os.sep, ':limit 1000000000',
                         f'compile({name}, {read_term}, {verdict}, {intro}, {pragmas}).'], timeout=1200)
        body, on, done = [], False, False
        for l in out:
            if l == '%% compile begin':
                on = True
                continue
            if l == '%% compile end' and on:
                on, done = False, True
                continue
            if on:
                body.append(l)
        if not done:
            errs = [l for l in out if re.search(r'Error|error|✗|not well-typed', l)]
            return None, '; '.join(errs[:3]) or 'the compiler printed no module'
        text = '\n'.join(body) + '\n'
        m = re.search(r'^%% not compiled.*$', text, re.M)
        if m:
            return None, m.group(0)[3:].strip()
        return text, None

    def artefact(self, app_dir, dest_dir):
        """Certify a mini-app's directory: (path of its .glpw, None) or (None, the refusal)."""
        os.makedirs(dest_dir, exist_ok=True)
        out = self.repl([f':artefact {app_dir} {dest_dir}'], timeout=900)
        name = os.path.basename(os.path.normpath(app_dir))
        p = os.path.join(dest_dir, name + '.glpw')
        if os.path.exists(p) and not any('refus' in l.lower() or 'error' in l.lower() for l in out):
            return p, None
        why = [l for l in out if l.strip() and 'Loaded' not in l and 'Goodbye' not in l]
        return None, '; '.join(why[:4]) or 'no artefact written'

    def boot(self, boot_file, program_dir, artefacts=(), timeout=300):
        """Run a boot: the REPL's :artefact for each mini-app the phones install, then :boot."""
        lines = [f':artefact {a} {program_dir}' for a in artefacts]
        lines.append(f':boot {boot_file} {program_dir}')
        return self.repl(lines, timeout=timeout)


def _posix(p, bash):
    """A path as Git for Windows' bash reads it."""
    if not IS_WIN:
        return p
    p = os.path.abspath(p).replace('\\', '/')
    m = re.match(r'^([A-Za-z]):/(.*)$', p)
    return f'/{m.group(1).lower()}/{m.group(2)}' if m else p
