"""inherits.py [--write] --- the first line of each contract of rev/ in the form of Definition Inheritance (record item 71,
Udi 2026-11-24 22:34 UTC: "Children inherits from Warded Groups."): 'NAME inherits from A, B and C.', the contracts named
by the first line of gr/NAME.txt, which keeps the operations by which NAME is obtained, in their order, each once, with the
references in parentheses that the line cites.  Prints each change; --write writes rev/."""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import names as NM
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def line_of(name, gline):
    refs = re.findall(r'\([^()]*\)', gline)
    body = re.sub(r'\s*\([^()]*\)', '', gline)[len(name):]
    known = NM.names_in(os.path.join(ROOT, 'rev'))
    got = []
    for _, _, w in NM.find(body, known):
        if w != name and w not in got:
            got.append(w)
    if not got:
        return None
    lst = got[0] if len(got) == 1 else ', '.join(got[:-1]) + ' and ' + got[-1]
    return '%s inherits from %s%s.' % (name, lst, ''.join(' ' + r for r in refs))

def main():
    write = '--write' in sys.argv
    n = 0
    for name in NM.names_in(os.path.join(ROOT, 'rev')):
        f = NM.file_of(name) + '.txt'
        rp, gp = os.path.join(ROOT, 'rev', f), os.path.join(ROOT, 'gr', f)
        rl, gl = open(rp, encoding='utf-8').read().split('\n'), open(gp, encoding='utf-8').read().split('\n')
        if len(gl) < 2 or not re.match(r'^%s (?:extends|restricts|strengthens|instantiates|composes|relaxes|weakens|augments|renames|lifts|unites)\b' % re.escape(name), gl[1]):
            continue
        new = line_of(name, gl[1])
        if new and rl[1] != new:
            print('%s\n    was: %s\n    now: %s' % (name, rl[1], new)); n += 1
            if write:
                rl[1] = new
                open(rp, 'w', encoding='utf-8').write('\n'.join(rl))
    print('%d first lines %s' % (n, 'written' if write else 'to write'))

if __name__ == '__main__':
    main()
