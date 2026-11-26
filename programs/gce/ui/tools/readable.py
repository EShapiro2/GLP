"""Readable sentences for the acts of a contract's input, where the Calculus words an act by a reference.

    python3 readable.py SIM.json OUT.json MEMBER.json ...

A contract of the Calculus often words an act by pointing to a clause of a contract it inherits from, as
Children's "Child acquaint: Guardians:3, only if Alice knows Bob", and acts it has by renaming another's have
no wording of their own at all ("Child befriend", Guardians' "Ward befriend" with a child for a ward and a
parent for a guardian).  The page shows an act by its sentence, so such an act would show the reference.
Here each act whose sentence is a reference, or only its button, is given a sentence:

1. its own clause in the contract that introduces it, if that clause is a sentence and not a reference
   (Organisation's "Found: Alice ... may create Orga (Guardians:1); Alice is then its first officer");
2. else the sentence of the act the reference names ("Guardians:3 Ward acquaint"), its names the act's own by
   position, unless that sentence speaks of wards or guardians, words the contract renames (a child, a parent,
   an officer), which this would not.

An act given no sentence this way keeps its button, and the page then shows the button alone: an act a
contract has by renaming another's, as Child befriend, has no wording in the Calculus to show.
"""
import json, re, sys

REF = re.compile(r"^([A-Z][\w ]*?):(\d+)(?: ([A-Z][\w ]*?))?(?:,|\.|$| only| and)")


def words_map(mine, theirs):
    """ward -> child: a phrase of `mine` that is a phrase of `theirs` but for one word."""
    out = {}
    for a in mine.values():
        for b in theirs.values():
            x, y = a['phrase'].split(), b['phrase'].split()
            if len(x) == len(y):
                d = [(p, q) for p, q in zip(x, y) if p != q]
                if len(d) == 1 and d[0][0].isalpha() and d[0][1].isalpha():
                    out[d[0][1]] = d[0][0]
    return out


def plural(m):
    return {**m, **{k + 's': v + ('ren' if v == 'child' else 's') for k, v in m.items()}}


def swap(text, m):
    def rep(mo):
        w = mo.group(0)
        low = w.lower()
        if low not in m:
            return w
        v = m[low]
        return v[0].upper() + v[1:] if w[0].isupper() else v
    return re.sub(r"\b[A-Za-z]+\b", rep, text)


def main():
    sim = json.load(open(sys.argv[1]))
    members = {m['contract']: m for m in (json.load(open(p)) for p in sys.argv[3:])}
    members[sim['contract']] = sim
    texts = {c['contract']: c['text'] for c in sim['chain']}
    base = lambda n: re.match(r'^(.*?)(?:_\d+)?$', n).group(1)

    def clause_titles(contract):
        out = {}
        for l in texts.get(contract, []):
            m = re.match(r'^(\d+)\. [■◆○] (.*)$', l)
            if m:
                out[int(m.group(1))] = re.findall(r'(?:^|\. )([A-Z][\w ]*?): ', m.group(2)) or [m.group(2).split(':')[0]]
        return out

    def own_clause(contract, title):
        for l in texts.get(contract, []):
            m = re.match(r'^\d+\. [■◆○] (.*)$', l)
            if not m:
                continue
            body = m.group(1)
            mm = re.search(r'(?:^|\. )' + re.escape(title) + r': (.*?)(?:\. [A-Z][\w ]*: |$)', body)
            if mm:
                return mm.group(1)
        return None

    def readable(s):
        return not (REF.match(s['sentence']) or s['sentence'] == s['button'])

    def rename(sent, theirs, mine):
        if len(theirs) != len(mine):
            return None
        tmp = sent
        for i, a in enumerate(theirs):
            tmp = re.sub(r'\b' + re.escape(a) + r'\b', '\x00%d\x00' % i, tmp)
        for i, b in enumerate(mine):
            tmp = tmp.replace('\x00%d\x00' % i, b)
        return tmp

    names = lambda x: x['persons'] + [v for v in x['values'] if v not in x['persons']]

    def preds(contract):
        return {c['p'] for x in members[contract]['schemas'] for c in x['pre'] + x['effects'] if c.get('p')}

    def pred_map(mine, theirs):
        """ward -> child: a predicate of the act that is one of the ancestor's but for one word (has_parent, has_guardian)"""
        out = {}
        for p in mine - theirs:
            for q in theirs - mine:
                x, y = p.split('_'), q.split('_')
                if len(x) == len(y):
                    d = [(a, b) for a, b in zip(x, y) if a != b]
                    if len(d) == 1:
                        out[d[0][1]] = d[0][0]
        if 'guardian' in out and out['guardian'] != 'guardian':
            out['guardianship'] = {'officer': 'office', 'parent': 'parenthood'}.get(out['guardian'], out['guardian'] + 'ship')
        return out

    def find(contract, title):
        m = members.get(contract.replace(' ', '-'))
        return next((x for x in m['schemas'] if x['button'] == title), None) if m else None

    def resolve(s, depth=0):
        if depth > 4:
            return None
        src = s.get('source', sim['contract'])
        # 1. its own clause, if a sentence
        own = own_clause(src, s['button'])
        if own and not REF.match(own):
            t = re.sub(r'\s*\([A-Z][\w ]*:\d+[^)]*\)', '', own).rstrip('.')
            t = re.split(r', (?:provided|only if|with the consent of)', t)[0]
            if set(re.findall(r'\b[A-Z][a-z]\w*\b', t)) <= set(names(s)) | {'Alice', 'Those', 'Either', 'An', 'A', 'The'}:
                return t
        # 2. the act a reference points to, or the act this one renames
        m = REF.match(s['sentence']) or (own and REF.match(own))
        target, wm = None, {}
        if not m:
            return None
        for ancestor in [x for x in members if x != src]:
            wm2 = {}
            if m:
                c, n, title = m.group(1).replace(' ', '-'), int(m.group(2)), m.group(3)
                if c != ancestor:
                    continue
                title = title or (clause_titles(c).get(n) or [None])[0]
                target = find(c, title) if title else None
            else:
                rev = {v: k for k, v in wm2.items()}
                target = find(ancestor, swap(s['button'], rev))
            if target:
                wm = wm2
                break
        if not target:
            return None
        t = target['sentence'] if readable(target) else resolve(target, depth + 1)
        if not t:
            return None
        # a sentence in words the contract renames (a guardian, for a parent or an officer) would say the wrong
        # thing: the act keeps its button instead
        if src != m.group(1).replace(' ', '-') and re.search(r'\b(ward|wards|guardian|guardians|guardianship)\b', t, re.I):
            return None
        t = rename(swap(t, wm), names(target), names(s))
        return t

    fixed = 0
    for s in sim['schemas']:
        if not readable(s):
            t = resolve(s)
            if t:
                s['sentence'], fixed = t, fixed + 1
            else:
                s['sentence'] = s['button']
    json.dump(sim, open(sys.argv[2], 'w'))
    print(sim['contract'], 'readable:', fixed, 'given;', sum(1 for s in sim['schemas'] if not readable(s)), 'left with the button alone')


main()
