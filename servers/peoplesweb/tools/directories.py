"""Who answers for each directory of GLP, as the site states it: the one record of it (Udi, 2026-11-11, to Coordination #14, reported by Coordination #15 to GSA, 2026-11-11 19:49 UTC: "each contract points to its directory, each foundational paper points to the directory(ies) it is responsible for, and paperless directories do the same, as discussed above, and also show on the website as such.  so all directories in /GLP are accounted for"; and "in general the website should reflect all aspects of project organisation, not other place").  Written from Coordination's Appendix B at Coordination 8da61a4 and 33c38d5, which Coordination deletes once the site states this.

Each entry is a path of GLP, the project that answers for it, the papers of that project, by their directory keys in tools/papers.py, and a note.  The most specific path that covers a file or a directory is the one that answers for it.  A project with no paper has a subdirectory of papers/ in GLP, named by its acronym, with a text that says what it is and does (Udi, 2026-11-11, to Coordination #14: "fine, so let's organise it as such"), which the project writes and the site links.
"""
import os

DIRECTORIES = [
    ('programs/self.glp', 'GLP', ['GLP-Spec'], 'the root scope of every program'),
    ('programs/system', 'GLP', ['GLP-Spec'], ''),
    ('programs/lib', 'GLP', ['GLP-Spec'], ''),
    ('programs/book', 'GLP', ['GLP-Spec'], ''),
    ('glp_runtime', 'GLP', ['IGLP'], 'all Dart'),
    ('glp_multiagent', 'GLP', ['IGLP'], 'all Dart'),
    ('programs/tests', 'GLP', ['IGLP'], ''),
    ('programs/vglp', 'vGLP', ['vGLP'], ''),
    ('programs/social/graph/ui', 'vGLP', ['vGLP'], "the user interface's modules inside the social graph, the mediator among them"),
    ('programs/sglp', 'sGLP', ['sGLP'], "an owner's own programs/sglp/<owner>/<NAME>_sglp.glp is that owner's"),
    ('programs/gsc', 'GSC', ['GSC'], 'GnW and programs/gsc/wards included'),
    ('programs/social', 'GSA', ['GSA'], 'the operating system: the social graph, the secure social graph and the boot files'),
    ('programs/social/graph/core/gsg_phone.glp', 'GSC', ['GSC'], "GSG's app on the phone"),
    ('programs/social/graph/core/gsn_phone.glp', 'GSC', ['GSC'], "GSN's app on the phone"),
    ('programs/social/graph/core/gkk_phone.glp', 'GSC', ['GSC'], "GKK's app on the phone"),
    ('programs/social/graph/core/gsnp_phone.glp', 'GAI', ['GAI'], "GSNP's app on the phone"),
    ('programs/social/graph/core/gai_phone.glp', 'GAI', ['GAI'], "GAI's app on the phone"),
    ('programs/social/graph/core/gc_phone.glp', 'Currencies', ['GC'], "GC's app on the phone"),
    ('servers/peoplesweb', 'GSA', ['GSA'], 'the Grassroots App Store at peoplesweb.org, GAS, which has no paper of its own'),
    ('networking', 'GNA', ['GNA'], 'the networking layer, imported from Dan Bachar\'s grassroots-networking'),
    ('programs/examples/rendezvous', 'GNA', ['GNA'], 'the rendezvous server and the directory, GSV, GRV and GDS'),
    ('servers/directory', 'GNA', ['GNA'], 'the directory, the contract GDS'),
    ('servers/gpw', 'GPW', [], ''),
    ('programs/cssn', 'CSSN', ['CSSN'], ''),
    ('programs/gai', 'GAI', ['GAI'], ''),
    ('programs/gff', 'GFF', ['GFF'], ''),
    ('programs/apps/gff', 'GFF', ['GFF'], ''),
    ('programs/ggga', 'GGGA', ['SRC'], 'its contracts, which the paper SRC prints'),
    ('programs/go', 'GO', ['GO'], ''),
    ('programs/gce', 'GCE', [], "Grassroots Contract Examples, Nimrod Talmon's; the paper responsible for each is named in its NAME.txt's % Owner line"),
    ('programs/lbm', 'GRH', ['GRH'], ''),
    ('programs/currencies', 'Currencies', ['GC', 'GB', 'SGC', 'GCC'], ''),
    ('programs/currencies/agora', 'Currencies', [], "Nimrod Talmon's, Currencies' until it has a paper"),
    ('programs/federation', 'GFWC', [], ''),
    ('programs/p99', 'P99', [], ''),
    ('phones', 'Phones', [], 'phone testing; GSA its specification, Code its code'),
    ('organisation', 'Coordination', [], "the organisation's rules: claude.md, the style guide and claudese.md"),
    ('programs/apps', 'Code', [], "the apps' boot files and compile.sh"),
    ('programs/apps/gkk', 'GSC', ['GSC'], "GKK's app"),
    ('programs/apps/gsg', 'GSC', ['GSC'], "GSG's app"),
    ('programs/apps/gsn', 'GSC', ['GSC'], "GSN's app"),
    ('programs/apps/gai', 'GAI', ['GAI'], "GAI's app"),
    ('programs/apps/gsnp', 'GAI', ['GAI'], "GSNP's app"),
    ('programs/apps/gb', 'Currencies', ['GB'], "GB's app"),
    ('programs/apps/gc', 'Currencies', ['GC'], "GC's app"),
    ('programs/apps/look.json', 'GSA', ['GSA'], "the families' names and colours"),
    ('cascade', 'Code', [], 'the recorded runs of the cascade'),
    ('cascade.sh', 'Code', [], 'the cascade'),
    ('simulation', 'Code', [], 'the recorded simulations'),
    ('test', 'Code', [], "the suite's harness; the Dart test trees are IGLP's"),
    ('tools', 'Code', [], ''),
    ('docs', 'Code', [], ''),
    ('servers/debuglog', 'Code', [], "the phones' debugging log service"),
    ('CLAUDE.md', 'Code', [], 'the rules for code'),
    ('README.md', 'Code', [], "the repository's layout"),
    ('papers', 'Code', [], 'the texts of the projects with no paper, each its own'),
]

# The projects with a paper, in the order the site lists them: each project's
# acronym, its name in full where it has one, and the papers it holds, by
# their keys in tools/papers.py (organisation/claude.md, "Mail", the table of
# inboxes, names the projects and the papers GLP, GSA, Currencies and GGGA
# hold; a project of one paper holds the paper of its name; Coordination #15
# checked the projects and their papers, 2026-11-12 09:27 UTC).  Every project
# has a name in full, that table's where it gives one and else one from its
# paper's title (Udi, 2026-11-12, to GSA #5, approving the nine: GLP, VMAT,
# vGLP, GSC, GAI, CSSN, CGP, DSA and CGMS).  The site's
# Papers and Projects pages are printed from it (tree, below).  A project of
# DIRECTORIES that is not here has no paper, and its directories are listed
# under Code with the project named (Udi, 2026-11-12, to GSA #5).
PROJECTS = [
    ('GLP', 'Grassroots Logic Programs', ['GLP-Spec', 'TGLP', 'IGLP']),
    ('VMAT', 'Volition-Guarded Multiagent Atomic Transactions', ['VMAT']),
    ('vGLP', 'Volition Elicitation', ['vGLP']),
    ('sGLP', 'Stochastic GLP', ['sGLP']),
    ('GSC', 'From Grassroots Contract to Grassroots App', ['GSC']),
    ('GSA', 'Grassroots Super-App and App Store', ['GSA', 'GNA']),
    ('SPM', 'Securing People and their Machines', ['SPM']),
    ('Currencies', 'Grassroots Currencies', ['GC', 'GB', 'SGC', 'GCC']),
    ('GAI', 'Accountable AI Agents', ['GAI']),
    ('CSSN', 'Child-Safe Social Networking', ['CSSN']),
    ('GFF', 'Grassroots Feeds and Followers', ['GFF']),
    ('GGGA', 'Sybil-resilient communities, Sybil-resilient governance', ['GGGA', 'SRC']),
    ('GO', 'Grassroots Organisations', ['GO']),
    ('GRH', 'Grassroots Ride-Hailing', ['GRH']),
    ('CGP', 'Characterising Global Platforms', ['CGP']),
    ('GPAT', 'Grassroots Platforms Atomic Transactions', ['GPAT']),
    ('GPBD', 'Grassroots Platforms Beyond Decentralisation', ['GPBD']),
    ('DSA', 'Digital Speech Acts', ['DSA']),
    ('CCDG', 'Constitutional Consensus', ['CCDG']),
    ('CGMS', 'Constitutional Governance in Metric Spaces', ['CGMS']),
]

# The directories whose every entry the site must account for, besides the
# repository's own top level: those that hold the work of several projects.
CONTAINERS = ['programs', 'programs/social', 'programs/apps', 'programs/examples', 'programs/currencies', 'servers']

TEXTS = 'papers'


def answering(path):
    """The entry of DIRECTORIES that answers for path, the most specific one that covers it, or None."""
    best = None
    for e in DIRECTORIES:
        if path == e[0] or path.startswith(e[0] + '/'):
            if best is None or len(e[0]) > len(best[0]):
                best = e
    return best


def unaccounted(root):
    """The entries of GLP's top level and of each container that no entry of DIRECTORIES covers, tracked files only."""
    import subprocess
    files = subprocess.run(['git', '-C', root, 'ls-files'], capture_output=True, text=True).stdout.split('\n')
    seen = set()
    for f in filter(None, files):
        parts = f.split('/')
        seen.add(parts[0])
        for c in CONTAINERS:
            cp = c.split('/')
            if parts[:len(cp)] == cp and len(parts) > len(cp):
                seen.add('/'.join(parts[:len(cp) + 1]))
    return sorted(p for p in seen if p not in CONTAINERS and answering(p) is None and not p.startswith('.'))


def table(root):
    """The site's list of directories: each entry that is in the repository, with who answers for it, its papers' keys, its note and, for a project with no paper, the path of its text where it has one."""
    out = []
    for path, who, papers, note in DIRECTORIES:
        if not os.path.exists(os.path.join(root, path)):
            continue
        e = {'path': path, 'dir': os.path.isdir(os.path.join(root, path)), 'who': who, 'papers': papers, 'note': note}
        if not papers:
            t = os.path.join(TEXTS, who)
            e['text'] = t if os.path.isdir(os.path.join(root, t)) else ''
        out.append(e)
    return out


def project_of(paper):
    """The project that holds the paper of this key, or None."""
    return next((p for p, _, ks in PROJECTS if paper in ks), None)


def tree(root, contracts):
    """The site's Projects page (Udi, 2026-11-12, to GSA #5): a tree in which every project, paper, contract and directory is once.  A project holds its papers; a paper holds the directories of DIRECTORIES that name it and no other paper, and its contracts, those whose % Owner line names it; a contract holds what is its own: its directory where no other contract's text is in it, else its text, then its app, its phone file and its record of the cascade, where each is.  A directory that names several papers of one project is the project's, beside its papers.  A directory with no paper is under Code, with the project that answers for it, and holds the contracts in it that name no paper.

    contracts is a list of (name, path of its NAME.txt from root, key of its paper or None).  Returns (projects, code, faults): projects a list of {id, name, papers: [{key, dirs, contracts}], dirs}; code a list of the directories with no paper, each with who, its text under papers/ where it has one, and its contracts; faults what does not fit the tree, for the build to print."""
    faults = []
    low = lambda n: n.lower().replace('-', '_')
    there = lambda p: os.path.exists(os.path.join(root, p))
    isdir = lambda p: os.path.isdir(os.path.join(root, p))
    entry_paths = {e[0] for e in DIRECTORIES}
    held = {}   # directory -> the contracts whose text is in it
    for n, p, _ in contracts:
        held.setdefault(os.path.dirname(p), []).append(n)
    taken = {}  # a path of DIRECTORIES that is a contract's own -> the contract

    def items(n, p):
        d = os.path.dirname(p)
        out = [{'k': 'directory', 'path': d, 'dir': True}] if len(held[d]) == 1 and d not in entry_paths else [{'k': 'text', 'path': p, 'dir': False}]
        for k, q in (('app', f'programs/apps/{low(n)}'), ('phone file', f'programs/social/graph/core/{low(n)}_phone.glp'), ('record', f'cascade/{n}')):
            if there(q):
                out.append({'k': k, 'path': q, 'dir': isdir(q)})
                if q in entry_paths:
                    taken[q] = n
        return out

    papers = {k: {'key': k, 'dirs': [], 'contracts': []} for _, _, ks in PROJECTS for k in ks}
    projects = [{'id': p, 'name': name, 'papers': [papers[k] for k in ks], 'dirs': []} for p, name, ks in PROJECTS]
    byid = {p['id']: p for p in projects}
    code, codeby = [], {}
    loose = []  # contracts that name no paper
    for n, p, key in sorted(contracts, key=lambda c: c[0].lower()):
        e = answering(p)
        if key is None and e is not None and len(e[2]) == 1:
            key = e[2][0]
        c = {'id': n, 'items': items(n, p)}
        if key in papers:
            papers[key]['contracts'].append(c)
        else:
            loose.append((c, e, p))
    for path, who, ks, note in DIRECTORIES:
        if not there(path):
            continue
        d = {'path': path, 'dir': isdir(path), 'note': note}
        if path in taken:
            owner = next((k for k in papers if any(c['id'] == taken[path] for c in papers[k]['contracts'])), None)
            if owner is None or project_of(owner) != who:
                faults.append(f'{path} is {who}\'s and the contract {taken[path]} is not')
            continue
        ps = {project_of(k) for k in ks}
        if not ks:
            t = os.path.join(TEXTS, who)
            d.update(who=who, text=t if isdir(t) else '', contracts=[])
            code.append(d)
            codeby[path] = d
        elif None in ps or len(ps) > 1:
            faults.append(f'{path} names {", ".join(ks)}, which no one project holds')
        elif len(ks) == 1:
            papers[ks[0]]['dirs'].append(d)
        else:
            byid[ps.pop()]['dirs'].append(d)
    for c, e, p in loose:
        if e is not None and e[0] in codeby:
            codeby[e[0]]['contracts'].append(c)
        else:
            faults.append(f'the contract {c["id"]}, {p}, names no paper and is in no directory without one')
    return projects, code, faults
