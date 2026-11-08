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
    'GNA': {'title': 'GLP Networking API Specification', 'authors': 'Dan Bachar and Ehud Shapiro', 'pdf': 'GNA'},
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
for _d, _p in {'GSC': 'GSC', 'GC': 'GC', 'GB': 'GB', 'SGC': 'SGC'}.items():
    BY_DIR[_d].setdefault('pdf', _p)


def pdf_name(d):
    """The served PDF's name for the paper in directory d."""
    return BY_DIR[d].get('pdf', d)


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
        if served and st.get('ahead'):
            v['updated'] = f'papers/{name}.pdf'
            v['updated_on'] = st.get('changed', '')[:10]
    elif name is not None:
        v.setdefault('state', 'in preparation')
        if served:
            v['pdf'] = f'papers/{name}.pdf'
            if st.get('changed'):
                v['pdf_on'] = st['changed'][:10]
    return v
