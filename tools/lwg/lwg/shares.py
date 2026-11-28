"""Shares, for a contract drawn on Shareholding Corporation (Moloch's LWGM, Guild): its own clauses, not changes to Shareholding Corporation, which GO keeps as it is (Udi, GLP#61, 2026-11-19).

- lock: the contract restricts Shareholding Corporation, "a share of Orga is not passed on" (Guild:2);
- exit_schema: the contract extends Shareholding Corporation by an act of its own, Exit (Guild:3).
A motion moved by a shareholder is no clause: Organisation:2 lets a person the bylaws name propose an act, and the contract's bylaws name the shareholders.
Each is switched on in the bylaws (map.json, "shares"): {"locked": true, "exit": true}."""
import copy, os, sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'vendor', 'go_tools'))
from sas_lib import A, N, Q, T, S, AND, P, I, num
from sas_dgc import C
from sas_corp import SHARE

PASSING = ('spend', 'swap_cc', 'swap_cb', 'swap_bc', 'deposit_c')      # the acts by which a coin passes from one holder to another

def lock(schemas):
    """A share is not passed on: as SC:5 does for redemption, the acts that pass a coin on exclude the denomination Share. The organisation still pays out the shares it issues."""
    for x in schemas:
        if x.name in PASSING:
            x.cons = AND(x.cons, 'ne(num(f), num(%s))' % SHARE); x.note += '  LWG: not a share.'
    return schemas

def exit_schema():
    """Bob gives up w of his shares and takes the same part of the r-coins the organisation holds: the largest k with k n <= w t, of t coins and n shares. His act alone, as redemption is."""
    return S('sc_exit', [Q('Bob'), Q('Orga'), 'r'], ['f', 'r', 'w', 'n', 'n1', 't', 'k'],
        [T(num('k'), C('Bob', 'r')), A('knows', Q('Bob'), 'r'), N(T(num('w'), C('Bob', 'Orga', SHARE))), N(T(num('k'), C('Orga', 'r'))),
         N(A('shares', Q('Orga'), num('n'))), A('shares', Q('Orga'), num('n1'))],
        P('Bob') + [A('knows', Q('Bob'), Q('Orga')), T(num('w'), C('Bob', 'Orga', SHARE))] + I('Orga')
        + [A('shares', Q('Orga'), num('n')), T(num('k'), C('Orga', 'r'))],
        AND('ge(num(w), num(1))', 'eq(num(t), cardinality([], %s, true))' % C('Orga', 'r'),
            'le(mul(num(k), num(n)), mul(num(w), num(t)))', 'gt(mul(plus(num(k), num(1)), num(n)), mul(num(w), num(t)))',
            'eq(num(n1), minus(num(n), num(w)))'),
        ['Bob'], 'LWG, Exit: a shareholder, Bob, gives up w shares and takes the same part of the r-coins of the denomination f that Orga holds; the shares are cancelled.')
