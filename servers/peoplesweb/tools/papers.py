"""Each contract's paper, as the site's balloons cite it: an arXiv identifier where the paper is on arXiv, a PDF the site serves where it is not yet, and the state otherwise.

The arXiv identifiers are Coordination Appendix A's (sections/A-paper-map.tex); a paper of ours not on arXiv is alluded to as in preparation (claude.md, "Papers").  The PDFs under web/papers/ are built from the paper repositories and go stale as the papers move; a sweep serves them as last built.
"""

GSC = {'name': 'the GSC paper', 'arxiv': '2609.06231'}
GAI = {'name': 'the GAI paper', 'state': 'on arXiv, number pending'}
GCC = {'name': "GCC's paper", 'pdf': 'GCC'}
GGGA = {'name': "GGGA's paper", 'pdf': 'GGGA'}
GRH = {'name': 'Grassroots Ride-Hailing', 'pdf': 'GRH'}
GFF = {'name': 'Grassroots Feeds and Followers', 'pdf': 'GFF'}
CSSN = {'name': 'the CSSN paper', 'pdf': 'CSSN'}

BY_CONTRACT = {
    'GKK': GSC, 'GSG': GSC, 'GSN': GSC,
    'GC': {'name': 'Grassroots Currencies', 'arxiv': '2202.05619'},
    'GB': {'name': 'Grassroots Bonds', 'arxiv': '2603.13671'},
    'DGC': {'name': 'the SGC paper', 'arxiv': '2609.27727'},
    'GDC': GCC, 'GCC': GCC, 'BCC': GCC,
    'GnW': GAI, 'GAI': GAI, 'GSNP': GAI,
    'CSSN': CSSN,
    'GD': GGGA, 'GD-S': GGGA,
    'GDO': GRH, 'GDO-LB': GRH, 'GRA': GRH, 'GRA-SS': GRH,
    'GDO-L': GFF, 'GFF': GFF, 'GFF-I': GFF, 'GFF-FR': GFF, 'GFF-PS': GFF, 'GFF-FR-BC': GFF,
    'AGORA': {'name': 'no paper; an example contract', 'state': ''},
}

BY_OWNER = {'GSC': GSC, 'GAI': GAI, 'GGGA': GGGA, 'GRH': GRH, 'GFF': GFF, 'CSSN': CSSN}


def paper_of(contract, owner):
    """The paper of [contract], or its [owner]'s, as the balloon shows it: name, and an arXiv identifier, a served PDF (papers/<X>.pdf, where built), or the state."""
    v = dict(BY_CONTRACT.get(contract) or BY_OWNER.get(owner)
             or {'name': f"{owner}'s paper", 'state': 'in preparation'})
    if 'pdf' in v:
        v['pdf'] = f"papers/{v['pdf']}.pdf"
        v['state'] = 'in preparation'
    return v
