"""Who answers for each directory of GLP, as the site states it: the one record of it (Udi, 2026-11-11, to Coordination #14, reported by Coordination #15 to GSA, 2026-11-11 19:49 UTC: "each contract points to its directory, each foundational paper points to the directory(ies) it is responsible for, and paperless directories do the same, as discussed above, and also show on the website as such.  so all directories in /GLP are accounted for"; and "in general the website should reflect all aspects of project organisation, not other place").  Written from Coordination's Appendix B at Coordination 8da61a4, which Coordination deletes once the site states this.

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
    ('programs/apps', 'Code', [], "the apps' directories, until Code says whose each is (Coordination #15, 2026-11-11)"),
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
