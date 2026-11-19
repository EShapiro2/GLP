"""Map: take the bylaws an LLM drew from the rules (map.json), check them, and write them into the record."""
import json
from . import rules
BASES = {'GCoop': 'gcoop', 'GDAO': 'gdao', 'SC': 'sc'}
STATUS = ['built', 'absorbed', 'not built', 'open', 'no clause']
ACTS = {'*': 'every other act', 'coop_join': 'admit a member', 'dao_join': 'admit a member', 'expel': 'expel a member', 'appoint': 'appoint to a role',
        'dismiss': 'dismiss from a role', 'coop_board': 'elect a member to the board', 'amend': 'amend the bylaws', 'sc_issue': 'issue shares'}
KIND = {'GCoop': 'a cooperative', 'GDAO': 'a DAO', 'SC': 'a shareholding corporation'}

def by(m, r):
    """Who decides under the rule r."""
    return m['roles'][str(r['policy'][1] if 'policy' in r else r['body'])] + 's'

def compose(m):
    """The contract's wording, where the bylaws do not carry their own: a definition drawn from the table of acts, the names, and the clauses."""
    if 'definition' in m: return m
    body = lambda r: by(m, r)
    parts = ['the %s decide %s when {%s}' % (body(m['rules'][r]), 'every other act of the organization' if a == '*' else 'to ' + ACTS[a], r) for a, r in sorted(m['acts'].items(), key=lambda x: x[0] == '*')]
    m['definition'] = 'Orga is %s; ' % KIND[m['base']] + '; '.join(parts) + '.'
    m['names'] = 'Alice and Bob are persons, Orga %s.' % KIND[m['base']]
    m['clauses'] = ['○ ' + u for u in m.get('undertakings', [])]
    new = [r for r in m['rules'].values() if 'policy' not in r and rules.as_policy(r) is None]
    if any(r.get('weight') != 'shares' for r in new):
        m['clauses'].append('■ Voting against.  A holder of a role may vote against a proposed act of Orga, once, in place of voting for it.')
    if any(r.get('weight') == 'shares' for r in new):
        m['clauses'].append('■ Voting against by shares.  A shareholder may vote against a proposed act of Orga, once, the vote counting the shares held, in place of voting for it.')
    sh = m.get('shares', {})
    if sh.get('locked'): m['clauses'].append('■ Shares stay.  A shareholder does not pass a share on to another person.')
    if sh.get('exit'): m['clauses'].append('■ Exit.  A shareholder may at any time give up shares and take the same part of a coin that Orga holds.')
    if sh.get('holders_move'): m['clauses'].append('■ Motions.  A shareholder may move as a motion: to %s.' % ', to '.join(ACTS[a] for a in sh['holders_move']))
    m.setdefault('act_names', {a: ACTS[a] for a in m['acts'] if a != '*'})
    return m

def run(rows, map_path):
    m = map_path if isinstance(map_path, dict) else json.load(open(map_path, encoding='utf8'))
    assert m['base'] in BASES, m['base']
    assert all((r.get('weight') == 'shares') <= (m['base'] == 'SC') for r in m['rules'].values()), 'votes by shares need the shareholding corporation'
    for r in m['rules'].values():
        if 'policy' not in r: rules.validate(r)
    for act, name in m['acts'].items(): assert name in m['rules'] and act in ACTS, (act, name)
    assert '*' in m['acts'], 'no rule for every other act'
    compose(m)
    ids = {r['id'] for r in rows}
    assert set(m['rows']) == ids, sorted(ids ^ set(m['rows']))
    for r in rows:
        x = m['rows'][r['id']]
        assert x['status'] in STATUS, x
        r['mapped_to'], r['status'], r['question'] = x['mapped_to'], x['status'], x.get('question', '')
    return m

def bylaws_table(m):
    out = ['| Act | Decided by | Rule |', '|---|---|---|']
    for act, name in m['acts'].items():
        r = m['rules'][name]
        who = by(m, r)
        out.append('| %s | %s | %s |' % ('every other act' if act == '*' else m.get('act_names', {}).get(act, act), who, rules.describe(r, m['roles'])))
    return '\n'.join(out)
