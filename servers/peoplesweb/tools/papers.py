"""Each contract's paper, as the site's balloons cite it: the full citation --- title and authors --- and an arXiv link where the paper is on arXiv, else a PDF the site serves, with its state.

Which paper a contract belongs to is read from the contract itself, the % Owner line of its NAME.txt, which names the paper's main file under /Grassroots/<DIR>/; this table only cites each paper.  The arXiv identifiers, titles and authors of the published papers are Bib-Grassroots' entries; the others' are their main files'.  The PDFs under web/papers/ are built from the paper repositories and go stale as the papers move; a sweep serves them as last built.
"""
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


def paper_of(owner_line, served_pdfs=()):
    """The citation of the paper the % Owner line names: title, authors, and an arXiv identifier, or a served PDF (papers/<X>.pdf, where built) with its state; a contract with no paper says so."""
    m = re.search(r'/Grassroots/([A-Za-z0-9_-]+)/', owner_line or '')
    if not m or m.group(1) not in BY_DIR:
        return {'title': '', 'authors': '', 'state': 'no paper' if re.search(r'No paper', owner_line or '') else 'in preparation'}
    v = dict(BY_DIR[m.group(1)])
    if 'pdf' in v:
        name = v.pop('pdf')
        v.setdefault('state', 'in preparation')
        if name in served_pdfs:
            v['pdf'] = f'papers/{name}.pdf'
    return v
