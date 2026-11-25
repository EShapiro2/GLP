"""An app directory for a contract's cascade platform: programs/apps/_ui/<dir>/, its platform.vglp the cascade's,
its miniapp.glp gsg's (the same in every app), and its self.glp the platform's types the mini-app's declarations
name, with gsg's own types of the router and the scripted person.
    python3 mkapp.py GLP NAME DIR"""
import re, sys, os, shutil
glp, name, d = sys.argv[1:4]
apps = os.path.join(glp, 'programs/apps')
out = os.path.join(apps, '_ui', d)
os.makedirs(out, exist_ok=True)
plat = open(os.path.join(glp, 'cascade', name, 'platform.vglp')).read()
shutil.copy(os.path.join(glp, 'cascade', name, 'platform.vglp'), os.path.join(out, 'platform.vglp'))
mini = open(os.path.join(apps, 'gsg/miniapp.glp')).read()
# A platform whose agent takes its initial atoms, agent(Peer?, Stream(Net)?, Stream(Net), Stream(Output), List(Atom)?), is started with none.
if re.search(r'exported procedure agent\(Peer\?, Stream\(Net\)\?, Stream\(Net\), Stream\(Output\), List\(Atom\)\?\)', plat):
    mini = mini.replace('Stream(Output), Stream(Ask(Question))).', 'Stream(Output), List(Atom)?, Stream(Ask(Question))).').replace('platform # agent(Id?, NetIn?, NetOut, Thread, Asks)', 'platform # agent(Id?, NetIn?, NetOut, Thread, [], Asks)')
open(os.path.join(out, 'miniapp.glp'), 'w').write(mini)
gsg = open(os.path.join(apps, 'gsg/self.glp')).read()
want = ['Peer', 'Class', 'Proposal', 'Request', 'YesNo', 'Reason', 'Vote', 'Decision', 'Rho', 'Names', 'Attempt', 'Cargo', 'Content', 'Net', 'Atom', 'Output', 'Tab', 'Asking', 'Approved', 'Approval', 'Word']
def block(text, n):
    m = re.search(r'^' + n + r'\s*::=.*?\.\s*$', text, re.M | re.S)
    if not m: return None
    # up to the first line ending in a period
    lines = []
    for l in text[m.start():].split('\n'):
        lines.append(l)
        if re.search(r'\.\s*(%.*)?$', l): break
    return '\n'.join(lines)
parts = []
for n in want:
    b = block(plat, n)
    if b: parts.append(b)
tail = gsg[gsg.index('%% What the mini-app'):gsg.index('%% The scripted person')]
open(os.path.join(out, 'self.glp'), 'w').write('%% The types of platform.vglp, for the declarations of this directory\'s modules (made by programs/gce/ui/tools/mkapp.py for the walk of described screens).\n\n' + '\n'.join(parts) + '\n\n' + tail)
print(out)
