"""Each contract's paper, as the site's balloons cite it: the full citation --- title and authors --- and an arXiv link where the paper is on arXiv, else a PDF the site serves, with its state.

Which paper a contract belongs to is read from the contract itself, the % Owner line of its NAME.txt, which names the paper's main file under /Grassroots/<DIR>/; this table only cites each paper.  The arXiv identifiers, titles and authors of the published papers are Bib-Grassroots' entries; the others' are their main files'.  The PDFs under web/papers/ are built from the paper repositories and go stale as the papers move; a sweep serves them as last built.
"""
import json
import os
import re

BY_DIR = {
    'GSC': {'title': 'From Grassroots Contract to Grassroots App',
            'authors': 'James Golike, Andy Lewis-Pye, Ehud Shapiro and Nimrod Talmon', 'arxiv': '2609.06231'},
    'GC': {'title': 'Grassroots Currencies: Turning Mutual Trust into Liquidity',
           'authors': 'Ehud Shapiro', 'arxiv': '2202.05619'},
    'GB': {'title': 'Grassroots Bonds: Financing by the People, for the People',
           'authors': 'Ehud Shapiro', 'arxiv': '2603.13671'},
    'SGC': {'title': 'Sovereign Grassroots Currencies: A CBDC Architecture for Credit and Monetary Policy',
            'authors': 'Ehud Shapiro', 'arxiv': '2609.27727'},
    'GCC': {'title': 'Grassroots Community Currencies', 'authors': 'Ehud Shapiro', 'pdf': 'GCC'},
    'GAI': {'title': 'Corralling AI Risks with Grassroots Contracts', 'authors': 'Ehud Shapiro',
            'pdf': 'GAI', 'state': 'uploaded to arXiv, number pending'},
    'CSSN': {'title': 'Child-Safe Social Networking: From Social Contract to App', 'authors': 'Ehud Shapiro', 'pdf': 'CSSN'},
    'SRC': {'title': 'Trust, Distrust and Committed Dynamics for Sybil-Resilient Communities in Social Graphs',
            'authors': '', 'pdf': 'GGGA'},
    'GRH': {'title': 'Grassroots Ride-Hailing', 'authors': '', 'pdf': 'GRH'},
    'GFF': {'title': 'Grassroots Feeds and Followers', 'authors': '', 'pdf': 'GFF'},
    'GSA': {'title': 'The Grassroots Super-App and App Store', 'authors': 'Ohad Eitan, Idit Keidar and Ehud Shapiro',
            'pdf': 'GSA', 'state': 'uploaded to arXiv, number not yet announced'},
    'GNA': {'title': 'GLP Networking API and Services', 'authors': 'Dan Bachar and Ehud Shapiro', 'pdf': 'GNA'},
}

# Where each paper's source is: its repository under EShapiro2 on GitHub, the
# one Overleaf syncs with, and its main file.  tools/papers_sync.py builds each
# from there; a paper on arXiv gets a PDF too, served as Updated while its
# repository is ahead of its latest arXiv version.
SOURCES = {
    'GSC': {'repo': 'Jurix-Formal-Grassroots-Social-Contracts', 'main': 'main.tex'},
    'GC': {'repo': 'GC-Grassroots-Currencies', 'main': 'main_Grassroots_Currencies.tex'},
    'GB': {'repo': 'Grassroots-Bonds', 'main': 'main_Grassroots_Bonds.tex'},
    'SGC': {'repo': 'SGC-Sovereign-Grassroots-Currencies', 'main': 'main_SGC.tex'},
    'GCC': {'repo': 'GCC-Grassroots-Commmunity-Currencies', 'main': 'main_SGC.tex'},
    'GAI': {'repo': 'GAI-Grassroots-Building-AI-Resilience-with-AI', 'main': 'main_characterisation_ArXiv.tex'},
    'CSSN': {'repo': 'SGLP', 'main': 'main_CSSN.tex'},
    'SRC': {'repo': 'SRC---Sybil-Resilient-Communities', 'main': 'main-distrust-layers.tex'},
    'GRH': {'repo': 'GRH-Grassroots-Ride-Hailing', 'main': 'main_GRH.tex'},
    'GFF': {'repo': 'GFF-Grassroots-Feeds-and-Followers', 'main': 'main_CSSN.tex'},
    'GSA': {'repo': 'GSG-Grassroots-Social-Graph', 'main': 'main_SGSG.tex'},
    'GNA': {'repo': 'GLP-Networking-API', 'main': 'main.tex'},
}
# Built from a copy with these replacements, the repository left as it is:
# acmart wants a country in every affiliation, which Overleaf forgives
# (Udi, 2026-11-07: not to be fixed in GFF's repository).
FIXES = {'GFF': [(r'\affiliation{}', r'\affiliation{\country{}}')]}
# The foundational papers' sources (FOUNDATIONAL below), each by its project's
# directory, repository and main file as Coordination's Appendix A records
# them (Grassroots-Project-Overview, sections/A-paper-map.tex); built and
# compared with arXiv as the contracts' papers are, so each is linked as
# Updated beside arXiv while its repository is ahead (Udi, 2026-11-10).
SOURCES.update({
    'GLP-Spec': {'repo': 'GLP-ICLP-2026', 'main': 'main-GLP-ICLP-2026.tex'},
    'TGLP': {'repo': 'Moded-Types', 'main': 'main_tglp.tex'},
    'IGLP': {'repo': 'CGLP', 'main': 'main_iglp.tex'},
    'VMAT': {'repo': 'Grassroots-atomic-transactions', 'main': 'main VMAT.tex'},
    'vGLP': {'repo': 'VEUI-Volition-Elicitation-Rationalizing-the-GLP-Smartphone-User-Interface', 'main': 'main_UIVE.tex'},
    'sGLP': {'repo': 'svGLP-Stochastic-Volitional-GLP', 'main': 'main_svGLP.tex'},
    'CGP': {'repo': 'Global-Platforms-Characterization-ArXiv-2025', 'main': 'main_characterisation_ArXiv.tex'},
    'GPAT': {'repo': 'ICDCN-2026', 'main': 'main_GPAT.tex'},
    'SPM': {'repo': 'SPM-Securing-People-and-their-Machines', 'main': 'main SPM.tex'},
    'DSA': {'repo': 'DSA', 'main': 'main.tex'},
    'Federation': {'repo': 'Grassroots-Federation', 'main': 'main.tex'},
    'CCDG': {'repo': 'Grassroots-Constitutional-Consensus-2026', 'main': 'mainConstitutionalConsensus.tex'},
    'CGMS': {'repo': 'SMA-Simple-Metric-Aggregation', 'main': 'ARXIV/main_ARXIV.tex'},
})
for _d, _p in {'GSC': 'GSC', 'GC': 'GC', 'GB': 'GB', 'SGC': 'SGC'}.items():
    BY_DIR[_d].setdefault('pdf', _p)


def pdf_name(d):
    """The served PDF's name for the paper in directory d."""
    return BY_DIR.get(d, {}).get('pdf', d)


def arxiv_of(d):
    """The arXiv identifier of the paper in directory d, a contract's paper
    or a foundational one, or None."""
    if d in BY_DIR:
        return BY_DIR[d].get('arxiv')
    return next((f['arxiv'] for f in FOUNDATIONAL if f['dir'] == d), None)


def updated(v, name, served_pdfs, st):
    """Add to the citation v of a paper on arXiv the link to its PDF as
    Updated, with the date of its source's last change, where the PDF is
    served and the source has changed since the latest arXiv version."""
    if name in served_pdfs and st.get('ahead'):
        v['updated'] = f'papers/{name}.pdf'
        v['updated_on'] = st.get('changed', '')[:10]


def load_state(papers_dir):
    """What tools/papers_sync.py recorded of each paper: the date of its
    source's last change, and for a paper on arXiv the date of its latest
    version there and whether the source is ahead of it."""
    try:
        with open(os.path.join(papers_dir, 'state.json')) as f:
            return json.load(f)
    except (OSError, ValueError):
        return {}


def paper_of(owner_line, served_pdfs=(), state=None):
    """The citation of the paper the % Owner line names: title, authors, and an arXiv identifier, or a served PDF (papers/<X>.pdf, where built) with its state; a contract with no paper says so."""
    m = re.search(r'/Grassroots/([A-Za-z0-9_-]+)/', owner_line or '')
    if not m or m.group(1) not in BY_DIR:
        return {'title': '', 'authors': '', 'state': 'no paper' if re.search(r'No paper', owner_line or '') else 'in preparation'}
    d = m.group(1)
    v = dict(BY_DIR[d])
    st = (state or {}).get(d, {})
    name = v.pop('pdf', None)
    served = name is not None and name in served_pdfs
    if 'arxiv' in v:
        # Updated: the source has changed since the latest arXiv version.
        updated(v, name, served_pdfs, st)
    elif name is not None:
        v.setdefault('state', 'in preparation')
        if served:
            v['pdf'] = f'papers/{name}.pdf'
            if st.get('changed'):
                v['pdf_on'] = st['changed'][:10]
    return v


# The foundational papers, those with no contract of their own (Udi,
# 2026-11-09, choosing them), listed under Foundational Papers on the site;
# titles, authors and arXiv identifiers as Bib-Grassroots has them, and each
# its project's directory, the key of its source in SOURCES.
FOUNDATIONAL = [
    {'dir': 'GLP-Spec', 'title': 'GLP: A Grassroots, Multiagent, Concurrent, Logic Programming Language for AI', 'authors': 'Ehud Shapiro', 'arxiv': '2510.15747'},
    {'dir': 'TGLP', 'title': 'Types for Grassroots Logic Programs', 'authors': 'Ehud Shapiro', 'arxiv': '2601.17957'},
    {'dir': 'IGLP', 'title': 'Implementing Grassroots Logic Programs with Multiagent Transition Systems and AI', 'authors': 'Ehud Shapiro', 'arxiv': '2602.06934'},
    {'dir': 'VMAT', 'title': 'Volition-Guarded Multiagent Atomic Transactions: Describing People and their Machines', 'authors': 'Andy Lewis-Pye and Ehud Shapiro', 'arxiv': '2604.25596'},
    {'dir': 'vGLP', 'title': 'Volition Elicitation: Operational Semantics for People and Their Machines', 'authors': 'Idit Keidar and Ehud Shapiro', 'arxiv': '2607.14138'},
    {'dir': 'sGLP', 'title': 'sGLP: Simulating People with Stochastic Agents', 'authors': 'Ehud Shapiro', 'arxiv': '2610.14738'},
    {'dir': 'CGP', 'title': 'Characterising Global Platforms: Centralised, Decentralised, Federated, and Grassroots', 'authors': 'Ehud Shapiro', 'arxiv': '2511.03286'},
    {'dir': 'GPAT', 'title': 'Grassroots Platforms with Atomic Transactions: Social Graphs, Cryptocurrencies, and Democratic Federations', 'authors': 'Ehud Shapiro', 'arxiv': '2502.11299', 'venue': 'Theoretical Computer Science, 2026'},
    {'dir': 'SPM', 'title': 'Securing People and their Machines Against Major Faults', 'authors': 'Ohad Eitan, Idit Keidar and Ehud Shapiro', 'arxiv': '2607.02304', 'venue': 'SSS 2026, to appear'},
    {'dir': 'DSA', 'title': 'Digital Speech Acts Retain Control of Copyright with People, Not Platforms', 'authors': 'James Golike and Ehud Shapiro', 'arxiv': '2606.19263'},
    {'dir': 'Federation', 'title': 'Grassroots Federation: Fair Democratic Governance at Scale', 'authors': 'Nimrod Talmon and Ehud Shapiro', 'arxiv': '2505.02208', 'venue': 'AAMAS 2026'},
    {'dir': 'CCDG', 'title': 'Constitutional Consensus', 'authors': 'Idit Keidar, Andrew Lewis-Pye and Ehud Shapiro', 'arxiv': '2505.19216'},
    {'dir': 'CGMS', 'title': 'Constitutional Governance in Metric Spaces', 'authors': 'Ehud Shapiro and Nimrod Talmon', 'arxiv': '2605.13362', 'venue': 'ADT 2026, to appear'},
]


def foundational(served_pdfs=(), state=None):
    """The foundational papers' citations, each with the link to its PDF as
    Updated where its source is ahead of arXiv, as a contract's paper has it."""
    out = []
    for f in FOUNDATIONAL:
        v = {k: x for k, x in f.items() if k != 'dir'}
        updated(v, pdf_name(f['dir']), served_pdfs, (state or {}).get(f['dir'], {}))
        out.append(v)
    return out
