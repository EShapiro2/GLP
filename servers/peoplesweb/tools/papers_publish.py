"""Commit the papers' PDFs to the branch papers of GLP (Udi, 2026-11-11, to Code #27: the server builds each paper's PDF in its hourly sweep and commits it to a branch papers of GLP, as papers/<NAME>.pdf, and no session builds or pushes a PDF; reported to GSA by Code #27, 2026-11-11 19:44 UTC).

Run by the hourly sweep as root, after tools/papers_sync.py, whose PDFs in PAPERS_DIR it commits, and before tools/build.py, which links each published PDF on the branch:

    PAPERS_DIR        the PDFs, and published.json, which this writes (default /var/lib/glp/papers)
    PAPERS_SRC        where its own bare repository is kept (default /var/lib/glp/papers-src)
    PAPERS_PUSH_TOKEN the file holding a GitHub token with write access to the contents of EShapiro2/GLP, which Udi places (default /etc/glp/github-papers-token)

The branch holds one commit, the PDFs as they now are, with no parent: each change replaces it, so that the PDFs, a few megabytes each, are not kept in GLP's history once superseded.  A sweep whose PDFs are those last published pushes nothing.  Without the token, or when the push fails, nothing is published, the failure is printed, and the site links the PDFs it serves itself.  For a test, PAPERS_REMOTE names the repository to push to in place of GitHub's, and no token is read.
"""
import base64
import datetime as dt
import json
import os
import subprocess
import sys

OUT = os.environ.get('PAPERS_DIR', '/var/lib/glp/papers')
SRC = os.environ.get('PAPERS_SRC', '/var/lib/glp/papers-src')
TOKEN = os.environ.get('PAPERS_PUSH_TOKEN', '/etc/glp/github-papers-token')
REMOTE = os.environ.get('PAPERS_REMOTE')
URL = 'https://github.com/EShapiro2/GLP.git'
BRANCH = 'refs/heads/papers'
# The commits are in Udi's name, as every commit of GLP is (organisation/claude.md, "Git and shell").
WHO = {'GIT_AUTHOR_NAME': 'Ehud Shapiro', 'GIT_AUTHOR_EMAIL': 'udi.shapiro@gmail.com',
       'GIT_COMMITTER_NAME': 'Ehud Shapiro', 'GIT_COMMITTER_EMAIL': 'udi.shapiro@gmail.com'}


def git(repo, *args, env=None, data=None):
    r = subprocess.run(['git', '--git-dir', repo, *args], capture_output=True, text=True, errors='replace',
                       input=data, env={**os.environ, **(env or {})}, timeout=900)
    if r.returncode != 0:
        raise RuntimeError(f"git {args[0]}: {r.stderr.strip()[:300]}")
    return r.stdout.strip()


def remote_and_auth():
    """The repository to push to and the arguments that authorise the push."""
    if REMOTE:
        return REMOTE, []
    with open(TOKEN) as f:
        tok = f.read().strip()
    basic = base64.b64encode(f'x-access-token:{tok}'.encode()).decode()
    return URL, ['-c', f'http.https://github.com/.extraheader=AUTHORIZATION: basic {basic}']


def main():
    names = sorted(f for f in os.listdir(OUT) if f.endswith('.pdf')) if os.path.isdir(OUT) else []
    if not names:
        print('papers: no PDF to publish')
        return
    pp = os.path.join(OUT, 'published.json')
    try:
        with open(pp) as f:
            last = json.load(f)
    except (OSError, ValueError):
        last = {}
    repo = os.path.join(SRC, 'publish.git')
    if not os.path.isdir(repo):
        os.makedirs(repo, exist_ok=True)
        git(repo, 'init', '-q', '--bare')
    index = {'GIT_INDEX_FILE': os.path.join(SRC, 'publish.index')}
    git(repo, 'read-tree', '--empty', env=index)
    blobs = {}
    for n in names:
        blobs[n[:-4]] = git(repo, 'hash-object', '-w', os.path.join(OUT, n))
        git(repo, 'update-index', '--add', '--cacheinfo', f'100644,{blobs[n[:-4]]},papers/{n}', env=index)
    tree = git(repo, 'write-tree', env=index)
    if tree == last.get('tree'):
        return
    msg = 'papers: the PDFs of the papers the site presents, built by the hourly sweep of peoplesweb.org from their repositories (servers/peoplesweb/tools/papers_publish.py)'
    commit = git(repo, 'commit-tree', tree, '-m', msg, env=WHO)
    try:
        url, auth = remote_and_auth()
        r = subprocess.run(['git', '--git-dir', repo, *auth, 'push', '-q', '--force', url, f'{commit}:{BRANCH}'],
                           capture_output=True, text=True, errors='replace', timeout=900)
        if r.returncode != 0:
            raise RuntimeError(f'git push: {r.stderr.strip()[:300]}')
    except Exception as e:
        print(f'papers: not published: {e}')
        return
    now = dt.datetime.now(dt.timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')
    with open(pp + '.tmp', 'w') as f:
        json.dump({'tree': tree, 'commit': commit, 'pushed': now, 'pdfs': blobs}, f, indent=1, sort_keys=True)
    os.replace(pp + '.tmp', pp)
    print(f'papers: published {len(names)} PDFs at {commit[:8]}')


if __name__ == '__main__':
    try:
        main()
    except Exception as e:  # the sweep goes on; the site links what it serves
        print(f'papers: publishing failed: {e}')
        sys.exit(0)
