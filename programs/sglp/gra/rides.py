import sys,re
log=sys.argv[1]
rows=[l.rstrip('\n').split('\t') for l in open(log) if l.count('\t')>=3]
refused=set()
for r in rows:
    if r[2]=='output' and r[3].startswith('refused('):
        m=re.match(r"refused\((\w+\(.*?\)), ",r[3]); refused.add((r[0],m.group(1)))
for r in rows:
    t,a,ty,term=r
    if ty!='Request?' or not term.startswith('propose('): continue
    inner=term[len('propose('):-1]
    nm=inner.split('(')[0]
    if nm in ('withdraw','move','acquaint','befriend','establish','point_friend'): continue
    if (t,inner) in refused: continue
    print("min %5.1f  %-16s %s"%(float(t)/60,nm,inner[len(nm)+1:-1]))
