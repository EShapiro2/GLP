"""Commit the verified feedback posts to the branch feedback of GLP, from which Audit's session opens a pull request in the audit's repository for each (Udi, 2026-11-17, to Website #3: "ok", to Audit's session opening them, so that he places no token; Code #33 agreeing, 2026-11-17 13:10 UTC).

Run by the hourly sweep as root, beside tools/papers_publish.py, whose token and way of pushing it takes:

    FEEDBACK_DIR      the server's store; its outbox/ holds one file per verified post, as lib/feedback.dart writes it (default /var/lib/glp/feedback)
    FEEDBACK_SRC      where its own bare repository is kept (default /var/lib/glp/feedback-src)
    PAPERS_PUSH_TOKEN the token of tools/papers_publish.py, write access to the contents of EShapiro2/GLP (default /etc/glp/github-papers-token)

The branch holds one commit with no parent, feedback/<name>.md for every verified post, replaced at each change, and is never merged into main.  No file of it holds an email address: the server keeps the addresses apart, in posts.json, which this does not read.  A sweep with no new post pushes nothing.  For a test, FEEDBACK_REMOTE names the repository to push to in place of GitHub's, and no token is read.
"""
import datetime as dt
import json
import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from papers_publish import git, WHO, URL  # noqa: E402

DIR = os.environ.get('FEEDBACK_DIR', '/var/lib/glp/feedback')
SRC = os.environ.get('FEEDBACK_SRC', '/var/lib/glp/feedback-src')
TOKEN = os.environ.get('PAPERS_PUSH_TOKEN', '/etc/glp/github-papers-token')
REMOTE = os.environ.get('FEEDBACK_REMOTE')
BRANCH = 'refs/heads/feedback'


def auth():
    if REMOTE:
        return REMOTE, []
    import base64
    with open(TOKEN) as f:
        tok = f.read().strip()
    basic = base64.b64encode(f'x-access-token:{tok}'.encode()).decode()
    return URL, ['-c', f'http.https://github.com/.extraheader=AUTHORIZATION: basic {basic}']


def main():
    box = os.path.join(DIR, 'outbox')
    names = sorted(f for f in os.listdir(box) if f.endswith('.md') and not f.startswith('.')) if os.path.isdir(box) else []
    if not names:
        return
    pp = os.path.join(SRC, 'published.json')
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
    for n in names:
        b = git(repo, 'hash-object', '-w', os.path.join(box, n))
        git(repo, 'update-index', '--add', '--cacheinfo', f'100644,{b},feedback/{n}', env=index)
    tree = git(repo, 'write-tree', env=index)
    if tree == last.get('tree'):
        return
    msg = 'feedback: the verified posts of the feedback form of peoplesweb.org, each to be a pull request in the audit\'s repository (servers/peoplesweb/tools/feedback_publish.py)'
    commit = git(repo, 'commit-tree', tree, '-m', msg, env=WHO)
    try:
        url, a = auth()
        r = subprocess.run(['git', '--git-dir', repo, *a, 'push', '-q', '--force', url, f'{commit}:{BRANCH}'],
                           capture_output=True, text=True, errors='replace', timeout=900)
        if r.returncode != 0:
            raise RuntimeError(f'git push: {r.stderr.strip()[:300]}')
    except Exception as e:
        print(f'feedback: not published: {e}')
        return
    now = dt.datetime.now(dt.timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')
    with open(pp + '.tmp', 'w') as f:
        json.dump({'tree': tree, 'commit': commit, 'pushed': now, 'posts': names}, f, indent=1)
    os.replace(pp + '.tmp', pp)
    print(f'feedback: published {len(names)} posts at {commit[:8]}')


if __name__ == '__main__':
    try:
        main()
    except Exception as e:  # the sweep goes on
        print(f'feedback: publishing failed: {e}')
        sys.exit(0)
