"""Keep the site's papers current (Udi, 2026-11-08): for each paper of
tools/papers.py, pull its repository, the one Overleaf syncs with, build its
PDF when the source has changed, and record the date of the source's last
change; for a paper on arXiv, record too the date of its latest version there
and whether the source is ahead of it, in which case the site links the PDF
as Updated beside arXiv.

Run by the hourly sweep as root, before tools/build.py, which serves what
this writes:

    PAPERS_DIR   the PDFs and state.json (default /var/lib/glp/papers)
    PAPERS_SRC   the clones and the build copies (default /var/lib/glp/papers-src)
    PAPERS_TOKEN the file holding a read-only GitHub token for the private
                 repositories (default /etc/glp/github-token)

For a test, PAPERS_LOCAL names a directory holding clones already made, by
the repository's name in lower case, and nothing is fetched; ARXIV_FILE
names a saved reply of arXiv's API in place of asking it.  A paper whose
fetch or build fails keeps its last PDF and its state, and the failure is
printed; the rest go on.
"""
import base64
import datetime as dt
import json
import os
import re
import shutil
import subprocess
import sys
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from papers import FIXES, SOURCES, arxiv_of, pdf_name  # noqa: E402

OUT = os.environ.get('PAPERS_DIR', '/var/lib/glp/papers')
SRC = os.environ.get('PAPERS_SRC', '/var/lib/glp/papers-src')
TOKEN = os.environ.get('PAPERS_TOKEN', '/etc/glp/github-token')
LOCAL = os.environ.get('PAPERS_LOCAL')
ARXIV_FILE = os.environ.get('ARXIV_FILE')


def run(cmd, cwd=None, timeout=900):
    return subprocess.run(cmd, cwd=cwd, timeout=timeout, capture_output=True, text=True, errors='replace')


def git_auth():
    with open(TOKEN) as f:
        tok = f.read().strip()
    basic = base64.b64encode(f'x-access-token:{tok}'.encode()).decode()
    return ['-c', f'http.https://github.com/.extraheader=AUTHORIZATION: basic {basic}']


def fetch(repo):
    """The clone of repo, current; its path."""
    if LOCAL:
        return os.path.join(LOCAL, repo.lower())
    path = os.path.join(SRC, 'git', repo)
    url = f'https://github.com/EShapiro2/{repo}.git'
    auth = git_auth()
    if os.path.isdir(os.path.join(path, '.git')):
        r = run(['git', *auth, 'fetch', '-q', '--depth', '1', 'origin', 'HEAD'], cwd=path)
        if r.returncode == 0:
            r = run(['git', 'reset', '-q', '--hard', 'FETCH_HEAD'], cwd=path)
    else:
        os.makedirs(os.path.dirname(path), exist_ok=True)
        r = run(['git', *auth, 'clone', '-q', '--depth', '1', url, path])
    if r.returncode != 0:
        raise RuntimeError(f'git: {r.stderr.strip()[:300]}')
    return path


def build(d, path, main):
    """The PDF of the paper in directory d, built from a copy of path with
    FIXES applied; the path of the PDF."""
    work = os.path.join(SRC, 'build', d)
    shutil.rmtree(work, ignore_errors=True)
    shutil.copytree(path, work, ignore=shutil.ignore_patterns('.git'))
    for old, new in FIXES.get(d, []):
        mf = os.path.join(work, main)
        with open(mf) as f:
            t = f.read()
        with open(mf, 'w') as f:
            f.write(t.replace(old, new))
    # Built at the repository's root, as Overleaf builds it, whose inputs
    # are relative to it (CGMS's main file is ARXIV/main_ARXIV.tex and reads
    # ARXIV/sections/...); the PDF is written at the root.
    base = os.path.basename(main)
    r = run(['latexmk', '-pdf', '-interaction=nonstopmode', '-halt-on-error', main], cwd=work)
    pdf = os.path.join(work, base[:-4] + '.pdf')
    if r.returncode != 0 or not os.path.exists(pdf):
        tail = '\n'.join((r.stdout + r.stderr).strip().splitlines()[-8:])
        raise RuntimeError(f'latexmk:\n{tail}')
    return pdf


def arxiv_dates(ids):
    """The date of the latest version on arXiv of each identifier, from
    arXiv's API, an Atom feed: each entry's id is http://arxiv.org/abs/<id>v<n>
    and its updated element the date of that version."""
    if not ids:
        return {}
    if ARXIV_FILE:
        with open(ARXIV_FILE) as f:
            xml = f.read()
    else:
        url = 'https://export.arxiv.org/api/query?id_list=' + ','.join(ids) + f'&max_results={len(ids)}'
        req = urllib.request.Request(url, headers={'User-Agent': 'peoplesweb-papers-sync'})
        with urllib.request.urlopen(req, timeout=60) as r:
            xml = r.read().decode()
    out = {}
    for e in re.findall(r'<entry>(.*?)</entry>', xml, re.S):
        i = re.search(r'<id>https?://arxiv\.org/abs/([^<v]+)(?:v\d+)?</id>', e)
        u = re.search(r'<updated>([^<]+)</updated>', e)
        if i and u:
            out[i.group(1)] = u.group(1)
    return out


def when(s):
    return dt.datetime.fromisoformat(s.replace('Z', '+00:00'))


def main():
    os.makedirs(OUT, exist_ok=True)
    sp = os.path.join(OUT, 'state.json')
    try:
        with open(sp) as f:
            state = json.load(f)
    except (OSError, ValueError):
        state = {}
    try:
        on_arxiv = arxiv_dates([a for a in map(arxiv_of, SOURCES) if a])
    except Exception as e:  # arXiv unreachable: the recorded dates stand
        print(f'papers: arXiv: {e}')
        on_arxiv = {}
    for d, s in SOURCES.items():
        st = state.get(d, {})
        name = pdf_name(d)
        try:
            path = fetch(s['repo'])
            head = run(['git', 'rev-parse', 'HEAD'], cwd=path).stdout.strip()
            changed = run(['git', 'log', '-1', '--format=%cI'], cwd=path).stdout.strip()
            target = os.path.join(OUT, name + '.pdf')
            # What the PDF is built from besides the source: the main file and
            # the fixes; a change of either rebuilds it too.
            recipe = json.dumps({'main': s['main'], 'fixes': FIXES.get(d, [])}, sort_keys=True)
            if head != st.get('head') or recipe != st.get('recipe') or not os.path.exists(target):
                pdf = build(d, path, s['main'])
                shutil.copy(pdf, target + '.tmp')
                os.replace(target + '.tmp', target)
                print(f'papers: {d} built at {head[:8]}')
            st.update(head=head, changed=changed, recipe=recipe)
            st.pop('error', None)
        except Exception as e:
            st['error'] = str(e)[:500]
            print(f'papers: {d}: {e}')
        a = arxiv_of(d)
        if a:
            if a in on_arxiv:
                st['arxiv_updated'] = on_arxiv[a]
            if st.get('changed') and st.get('arxiv_updated'):
                st['ahead'] = when(st['changed']) > when(st['arxiv_updated'])
        state[d] = st
    # A PDF whose paper has left SOURCES, or been renamed, is removed, and
    # with it its state, so that neither the site nor the branch papers
    # keeps it (papers/GGGA.pdf, SRC's under its old name, Code #28,
    # 2026-11-12).
    names = {pdf_name(d) + '.pdf' for d in SOURCES}
    for f in os.listdir(OUT):
        if f.endswith('.pdf') and f not in names:
            os.remove(os.path.join(OUT, f))
            print(f'papers: {f} removed, no paper of that name')
    state = {d: st for d, st in state.items() if d in SOURCES}
    with open(sp + '.tmp', 'w') as f:
        json.dump(state, f, indent=1, sort_keys=True)
    os.replace(sp + '.tmp', sp)


if __name__ == '__main__':
    main()
