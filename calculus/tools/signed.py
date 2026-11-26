"""signed.py DIR: Calculus #9's docs/checks-2026-11-25/no-guard-sure-change.py (Calculus main 4e94bf1), extended to read
signatures (Definition Signed, Section 2.2 at 344bd35): an agent of a clause with no guard meets it if it is the agent of
a literal of D(C), or the signer of an argument of an atom that D(C) adds, Item@Bob's Bob.  DIR is calculus/contracts,
or the directory of this repository's sasx/ written as full-schemas.txt."""
import os, re, sys, collections
ROOT = sys.argv[1]
def split_top(s):
    out, depth, cur = [], 0, ''
    for ch in s:
        if ch in '([': depth += 1
        if ch in ')]': depth -= 1
        if ch == ',' and depth == 0:
            out.append(cur.strip()); cur = ''
        else: cur += ch
    if cur.strip(): out.append(cur.strip())
    return out
ATOM = re.compile(r'^(~?)([a-z_][A-Za-z0-9_]*)(?:\((.*)\))?$')
def parse_lit(s):
    m = ATOM.match(s.strip())
    if not m: return None
    neg, pred, args = m.group(1) == '~', m.group(2), split_top(m.group(3) or '')
    return (neg, pred, tuple(args))
VAR = re.compile(r"[A-Z][A-Za-z0-9_']*")
def vars_of(args): 
    v = set()
    for a in args: v |= set(VAR.findall(a))
    return v
LINE = re.compile(r'^(.+?:\d+(?:\+.+?:\d+)*) ([■◆○]) ([^|:][^:]*): (.*)$')
results = {}
allkinds = collections.Counter()
for d in sorted(os.listdir(ROOT)):
    p = os.path.join(ROOT, d, 'full-schemas.txt')
    if not os.path.exists(p): continue
    clauses = []
    for line in open(p, encoding='utf-8'):
        line = line.rstrip('\n')
        m = LINE.match(line)
        if not m or '<-' not in m.group(4): continue
        label, mod, act, rest = m.groups()
        rest = rest.rstrip('.')
        poss = ''
        if ' possibly ' in rest: rest, poss = rest.split(' possibly ', 1)
        guards = []
        if ' guarded by ' in rest:
            rest, g = rest.split(' guarded by ', 1); guards = [x.strip() for x in g.split(',')]
        head, _, body = rest.partition('<-')
        H = [parse_lit(x) for x in split_top(head.strip())]
        B = [parse_lit(x) for x in split_top(body.strip())]
        if None in H or None in B:
            print('UNPARSED', d, line[:200]); continue
        clauses.append((label + ' ' + act, H, B, guards, poss))
    # literal-agents: first argument of any literal whose first arg is a plain variable; over the whole contract
    firsts = set()
    for _, H, B, g, _ in clauses:
        for (neg, pred, args) in H:
            if args and VAR.fullmatch(args[0]): firsts.add(args[0])
        firsts |= set(g)
    # an atom is a test if its first arg is never an agent of a head literal or guard anywhere: approximate agent set = firsts of heads + guards + firsts of body literals whose predicate appears in some head
    headpreds = {pred for _, H, B, g, _ in clauses for (n, pred, a) in H}
    for _, H, B, g, _ in clauses:
        for (neg, pred, args) in B:
            if pred in headpreds and args and VAR.fullmatch(args[0]): firsts.add(args[0])
    AG = {'Alice','Bob','Carol','Dave','Erin','Frank','Iris','Alicia','Wendy','Will','Alf','Orga','Gov','Club','Dora'}
    odd = firsts - AG
    if odd: print('FIRST-ARG NOT IN AGENT NAMES', d, sorted(odd))
    agents = AG
    ung = [c for c in clauses if not c[3]]
    # head (sign,pred) counts among unguarded clauses
    cnt = collections.Counter()
    for name, H, B, g, _ in ung:
        for (neg, pred, args) in H: cnt[(neg, pred)] += 1
    for name, H, B, g, poss in ung:
        Bset = set(B)
        certain = [l for l in H if ((not l[0]) and (True, l[1], l[2]) in Bset) or (l[0] and (False, l[1], l[2]) in Bset and '_' not in l[2])]
        certain_u = [l for l in certain if cnt[(l[0], l[1])] == 1]
        allv = set()
        for l in H + B: allv |= vars_of(l[2])
        agA = allv & agents
        agB = {l[2][0] for l in H + B if l[2] and l[2][0] in agents}
        det = set(); 
        for l in certain: det |= vars_of(l[2])
        detu = set()
        for l in certain_u: detu |= vars_of(l[2])
        def free(v):
            # some head literal not mentioning v that is not a forced no-op
            for l in H:
                if v in vars_of(l[2]): continue
                if (not l[0]) and (False, l[1], l[2]) in Bset: continue
                if l[0] and (True, l[1], l[2]) in Bset: continue
                return True
            return False
        violA = sorted(v for v in agA - det if free(v)) if len(agA) > 1 else []
        violB = sorted(v for v in agB - det if free(v)) if len(agB) > 1 else []
        needA = sorted(agA - detu) if len(agA) > 1 else []
        needB = sorted(agB - detu) if len(agB) > 1 else []
        # Calculus #8's condition: one positive certain literal x:s, s naming every other agent variable, predicate once among unguarded heads (unnegated)
        sureag = {l[2][0] for l in certain if l[2]} | {a.split('@')[-1] for l in certain if not l[0] for a in l[2] if '@' in a}
        allsure = len(agB) <= 1 or agB <= sureag
        stat = sorted(agB - sureag) if len(agB) > 1 else []
        c8 = len(agA) <= 1 or any((not l[0]) and cnt[(False, l[1])] == 1 and agA <= vars_of(l[2]) for l in certain)
        key = name
        r = results.setdefault(key, dict(contracts=[], violA=set(), violB=set(), needA=set(), needB=set(), c8=True, allsure=True, stat=set(), nA=len(agA), nB=len(agB), text=''))
        r['contracts'].append(d); r['violA'] |= set(violA); r['violB'] |= set(violB); r['needA'] |= set(needA); r['needB'] |= set(needB); r['c8'] &= c8; r['allsure'] &= allsure; r['stat'] |= set(stat)
        r['nA'] = max(r['nA'], len(agA)); r['nB'] = max(r['nB'], len(agB))
print('distinct clauses with no guard:', len(results))
multiA = {k: r for k, r in results.items() if r['nA'] > 1}
multiB = {k: r for k, r in results.items() if r['nB'] > 1}
print('with more than one agent variable:', len(multiA), '; with more than one agent of a literal:', len(multiB))
print('meet the condition of Section 4 as written:', sum(1 for r in multiA.values() if r['c8']), 'of', len(multiA))
print('violation constructible, participants = all agent variables:', sum(1 for r in multiA.values() if r['violA']))
print('violation constructible, participants = agents of literals:', sum(1 for r in multiB.values() if r['violB']))
print('every agent of a literal surely changes:', sum(r['allsure'] for r in results.values()), 'of', len(results))
for k in sorted(results):
    r=results[k]
    if not r['allsure']: print('  stationary or unsure:', k, sorted(r['stat']), r['contracts'][0])
import sys; sys.exit()
for k in sorted(results):
    r = results[k]
    if r['nA'] <= 1: continue
    print(f"{k} | nA={r['nA']} nB={r['nB']} | s4={'yes' if r['c8'] else 'no'} | violA={','.join(sorted(r['violA'])) or '-'} | violB={','.join(sorted(r['violB'])) or '-'} | in {len(r['contracts'])}")
