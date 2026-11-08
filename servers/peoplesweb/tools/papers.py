"""Each contract's paper, as the site's balloons cite it: the full citation --- title and authors --- and an arXiv link where the paper is on arXiv, a PDF the site serves where it is not yet, with its state.

The arXiv identifiers, titles and authors of the published papers are Bib-Grassroots' entries, the source of truth for citations; the in-preparation papers' titles and authors are their main files', and a paper whose author field is empty or a placeholder is cited by its title.  The PDFs under web/papers/ are built from the paper repositories and go stale as the papers move; a sweep serves them as last built.
"""

GSC = {'title': 'Grassroots Social Contracts: From Legal Text to an App',
       'authors': 'James Golike, Andy Lewis-Pye and Ehud Shapiro', 'arxiv': '2609.06231'}
GAI = {'title': 'Corralling AI Risks with Grassroots Contracts',
       'authors': 'Ehud Shapiro', 'pdf': 'GAI', 'state': 'on arXiv, number pending'}
GCC = {'title': 'Grassroots Community Currencies', 'authors': 'Ehud Shapiro', 'pdf': 'GCC'}
GGGA = {'title': 'Trust, Distrust and Committed Dynamics for Sybil-Resilient Communities in Social Graphs',
        'authors': '', 'pdf': 'GGGA'}
GRH = {'title': 'Grassroots Ride-Hailing', 'authors': '', 'pdf': 'GRH'}
GFF = {'title': 'Grassroots Feeds and Followers', 'authors': '', 'pdf': 'GFF'}
CSSN = {'title': 'Child-Safe Social Networking: From Social Contract to App',
        'authors': 'Ehud Shapiro', 'pdf': 'CSSN'}

BY_CONTRACT = {
    'GKK': GSC, 'GSG': GSC, 'GSN': GSC,
    'GC': {'title': 'Grassroots Currencies: Turning Mutual Trust into Liquidity',
           'authors': 'Ehud Shapiro', 'arxiv': '2202.05619'},
    'GB': {'title': 'Grassroots Bonds: Financing by the People, for the People',
           'authors': 'Ehud Shapiro', 'arxiv': '2603.13671'},
    'DGC': {'title': 'Sovereign Grassroots Currencies: A CBDC Architecture for Credit and Monetary Policy',
            'authors': 'Ehud Shapiro', 'arxiv': '2609.27727'},
    'GDC': GCC, 'GCC': GCC, 'BCC': GCC,
    'GnW': GAI, 'GAI': GAI, 'GSNP': GAI,
    'CSSN': CSSN,
    'GD': GGGA, 'GD-S': GGGA,
    'GDO': GRH, 'GDO-LB': GRH, 'GRA': GRH, 'GRA-SS': GRH,
    'GDO-L': GFF, 'GFF': GFF, 'GFF-I': GFF, 'GFF-FR': GFF, 'GFF-PS': GFF, 'GFF-FR-BC': GFF,
    'AGORA': {'title': '', 'authors': '', 'state': 'an example contract; no paper'},
}

BY_OWNER = {'GSC': GSC, 'GAI': GAI, 'GGGA': GGGA, 'GRH': GRH, 'GFF': GFF, 'CSSN': CSSN}


def paper_of(contract, owner, served_pdfs=()):
    """The citation of [contract]'s paper, or its [owner]'s: title, authors, and an arXiv identifier or a served PDF (papers/<X>.pdf, only where [served_pdfs] holds <X>) with its state."""
    v = dict(BY_CONTRACT.get(contract) or BY_OWNER.get(owner) or {'title': '', 'authors': '', 'state': 'in preparation'})
    if 'pdf' in v:
        name = v.pop('pdf')
        v.setdefault('state', 'in preparation')
        if name in served_pdfs:
            v['pdf'] = f'papers/{name}.pdf'
    return v
