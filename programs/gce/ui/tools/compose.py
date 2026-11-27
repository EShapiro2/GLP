"""A composed contract's input for the page, each act worded as in the contract that introduces it.

    python3 compose.py CALC COMPOSED.json OUT.json MEMBER.json ...

CALC is the scratch copy of the Calculus build.py reads; COMPOSED.json is build.py's output for a contract that
inherits from others, and each MEMBER.json build.py's output for one contract of its chain, alone.

The Calculus names two acts that two inherited contracts give one title `post` and `post_2`, and build.py
words each act by its title, so `post_2` has no sentence and `post` may have the other contract's.  Here a
contract *introduces* the acts its own program has and the programs of the contracts it inherits from do
not; an act `t` of the composition is the one the first contract of the chain to introduce `t` introduced,
and `t_k` the one the k-th introduced.  Each act takes from that contract its button, sentence, questions and
field, and `source`, the contract's name; two acts with one button are then told apart by their sentences,
as the UI language does.  The output also has `parents`: for each contract of the chain, the contracts it
inherits from, by its first line.
"""
import json, re, sys


def main():
    calc = sys.argv[1]
    comp = json.load(open(sys.argv[2]))
    members = {m['contract']: m for m in (json.load(open(p)) for p in sys.argv[4:])}
    chain = [c['contract'] for c in comp['chain']]
    parents = {}
    for n in chain:
        try:
            line = open(f'{calc}/contracts/{n}/contract.txt').read().split('\n')[1]
        except OSError:
            continue
        m = re.match(r'^.*? inherits from (.*?)(?: \(.*)?\.$', line)
        names = re.split(r', | and ', m.group(1)) if m else []
        parents[n] = [re.sub(r"[^\w-]", '', x.strip().replace(' ', '-')) for x in names]

    def names(n):
        return {s['name'] for s in members[n]['schemas']} if n in members else set()

    def ancestors(n, seen=None):
        seen = seen if seen is not None else set()
        for p in parents.get(n, []):
            if p not in seen:
                seen.add(p)
                ancestors(p, seen)
        return seen

    introduced = {n: names(n) - set().union(*[names(a) for a in ancestors(n)]) for n in chain if n in members}
    # the order the composition takes its contracts in: each after those it inherits from, in the order its
    # first line names them
    order = []

    def visit(n):
        for p in parents.get(n, []):
            visit(p)
        if n not in order:
            order.append(n)
    visit(comp['contract'])
    base = lambda x: re.match(r'^(.*?)(?:_(\d+))?$', x).group(1)
    for s in comp['schemas']:
        b, k = base(s['name']), int((re.match(r'^.*?_(\d+)$', s['name']) or [0, 1])[1])
        origins = [(n, x) for n in order for x in sorted(introduced.get(n, set())) if base(x) == b]
        # an act that does exactly what one of the contracts' acts of its title does is that act
        sh = lambda x: json.dumps(sorted({(c['p'], bool(c.get('neg'))) for c in x['pre'] + x['effects'] if c.get('p')}))
        exact = next(((n, x['name']) for n in order if n in members for x in members[n]['schemas'] if base(x['name']) == b and sh(x) == sh(s)), None)
        if exact:
            origins, k = [exact], 1
        if len(origins) >= k:
            o, own = origins[k - 1]
            hit = next(x for x in members[o]['schemas'] if x['name'] == own)
            for key in ('button', 'sentence', 'questions', 'field'):
                s[key] = hit[key]
            s['source'] = o
        else:
            # an act a contract has under an inherited title, made anew (Organisation's Resign, of an officer, from
            # Guardians' Resign, of a guardian): the k-th contract whose act of that title does something new
            shape = lambda x: json.dumps(sorted({(c['p'], bool(c.get('neg'))) for c in x['pre'] + x['effects'] if c.get('p')}))
            seen, alike = [], []
            for n in order:
                x = next((y for y in members[n]['schemas'] if y['name'] == b), None) if n in members else None
                if x and shape(x) not in seen:
                    seen.append(shape(x)); alike.append((n, x))
            if len(alike) >= k:
                o, hit = alike[k - 1]
                for key in ('button', 'sentence', 'questions', 'field'):
                    s[key] = hit[key]
                s['source'] = o
            else:
                s['source'] = comp['contract']
    # the contract a clause comes from: the first, in the composition's order, whose act of the title says it
    for s in comp['schemas']:
        b = base(s['name'])
        first = next((n for n in order if n in members and any(base(x['name']) == b and x['sentence'] == s['sentence'] and x['persons'] == s['persons'] for x in members[n]['schemas'])), None)
        if first:
            s['source'] = first
    # a sentence that names a place by another name than the act's (Guardians' Wendy, in Organisation's Orga):
    # renamed by position, from the act of the contract the sentence comes from
    for s in comp['schemas']:
        mine = s['persons'] + [v for v in s['values'] if v not in s['persons']]
        words = set(re.findall(r'\b[A-Z][a-z]\w*\b', s['sentence']))
        if words <= set(mine) | {'Alice', 'Those'}:
            continue
        b = base(s['name'])
        for x in (y for n in order if n in members for y in members[n]['schemas']):
            if base(x['name']) == b and x['sentence'] == s['sentence'] and set(re.findall(r'\b[A-Z][a-z]\w*\b', x['sentence'])) <= set(x['persons'] + x['values']) | {'Alice', 'Those'}:
                theirs = x['persons'] + [v for v in x['values'] if v not in x['persons']]
                if len(theirs) == len(mine):
                    for a2, b2 in zip(theirs, mine):
                        if a2 != b2:
                            s['sentence'] = re.sub(r'\b' + a2 + r'\b', b2, s['sentence'])
                break
    comp['parents'] = parents
    comp['order'] = order
    json.dump(comp, open(sys.argv[3], 'w'))


main()
