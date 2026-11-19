#!/usr/bin/env python3
"""supplied.py --- GSC's sentence (GSC e51a633, Section 3.2) on the Logic pragmas: for each contract, every predicate an act
requires (sasx/) that no act adds and that is not an environment predicate, a reflexive predicate or a parameter.
Run from the top: python3 tools/supplied.py; its last line is the count, 0 when every required state is supplied."""
# GSC's sentence (e51a633, Section 3.2): the predicate of every atom a role requires is the predicate of an atom some
# schema of the contract adds, an environment predicate, a reflexive predicate or a parameter.  Prints, per contract,
# every required predicate that is none of the four; numbers' states left out (they are constraints).
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM      # the names of the contracts and their files (tools/names.py, NAMES.txt)
sys.path.insert(0, 'tools'); import gcsas
import toglp
PARAMS = set()
names = NM.names_in('sasx')
w = gcsas.World(NM.names_in('gr'))
NUM = {'le', 'ge', 'gt', 'lt', 'sum', 'product', 'difference', 'today', 'author'}      # author(x,t): eq(author_of(t), x), a constraint too (toglp.py)
bad = 0
for n in names:
    lex = w.lexicon(n)
    phrase = {p: t for t, p, a in lex.basic}
    env, refl = lex.env_preds(), lex.refl_preds()
    PARS = PARAMS | lex.env_preds('par')
    acts = [e for e in toglp.schemas(n) if e['kind'] == 'act']      # an act no binding enables is no act (toglp.unenabled)
    added = {l[1] for e in acts for l in e['head'] if not l[0]}
    req = {}
    for e in acts:
        for l in e['body']:
            p = l[1]
            if not l[0] and p not in NUM and p not in added and p not in env and p not in refl and p not in PARS:
                req.setdefault(p, set()).add(e['name'])
    # A role that guards an act is supplied when the vocabulary lists its agents ('listed by ...', lex.rlist) or it is a
    # parameter; else nothing says who is in it, and GSC's check refuses it, unsupplied(Schema, Role) (GSC-supplied 020fe5ea;
    # Calculus #6, 2026-11-15 17:46 UTC).
    rph = {p: t for t, p, a in lex.roles}
    for e in acts:
        for g in e['guards']:
            if '(' in g:
                p = gcsas.parse_atom(g)[1]
                if p not in lex.rlist and p not in PARS:
                    req.setdefault(p, set()).add(e['name'])
                    phrase.setdefault(p, rph.get(p, '?'))
    if req:
        bad += 1
        print(n + ': ' + '; '.join('%s ("%s"), required by %s' % (p, phrase.get(p, '?'), ', '.join(sorted(a))) for p, a in sorted(req.items())))
print('%d contracts with a required state or a guarding role none of the four supplies' % bad)
