"""Build GAS, the Grassroots App Store, at peoplesweb.org, from this repository: build/www and the live worlds.

Usage (from servers/peoplesweb):  python3 -I tools/build.py

Reads the repository only: every contract with a compiled platform,
cascade/<NAME>/platform.vglp as cascade.sh records it, gets

  programs/vglp/cva/web_worlds/live_<name>/  its live harness, four persons in one
                              heap (gen_harness.py), in the scope of the canonical
                              platforms, as vGLP's plays are
  build/www/specs/<name>.json the menu the app page draws, read off the platform (menu_spec.py)
  build/www/stack/<NAME>/     the cascade's layers, which the site loads on demand

and build/www/data.js holds every contract's text, owner, the contracts it
extends, its schemas and verdict, from cascade/<NAME>/ and the NAME.txt files
under programs/, with each contract's owner and paper (tools/papers.py), and
the families' names and colours.  The pages are web/*.html, copied as they
are.  A contract's icon and family are the line its interface pragmas open
with, 'Look: <icon>, <family>' (GSC's Section 2), and web/about.json's until
its owner writes that line; web/about.json holds the site's own line for each
contract; and programs/apps/look.json each family's name and colours, which
the app's asset sync reads too.  build/ and
programs/vglp/cva/web_worlds/ are generated, gitignored, and this script rebuilds them whole.

The test site, peoplesweb.org/next (deploy/test.sh), is this script of another
branch run over the repository as main has it: GLP_ROOT names the repository,
BUILD_DIR where the site is written, and SITE_TEST=1 says that the build
writes no world, the test pages using the live server's worlds, and that the
pages are served one level below the site's root, where the API is.
"""
import ast, json, os, re, shutil, subprocess, sys

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))   # servers/peoplesweb
GLP = os.environ.get('GLP_ROOT') or os.path.dirname(os.path.dirname(HERE))   # the repository
BUILD = os.environ.get('BUILD_DIR') or os.path.join(HERE, 'build')
TEST = os.environ.get('SITE_TEST') == '1'                             # the test site (deploy/test.sh)
# The Villagers site, peoplesweb.org/villagers (deploy/villagers.sh; Udi,
# 2026-11-25 00:19 UTC: "a version of the website in /villagers that includes
# only the contracts in this repo, and one paper, the Villagers paper.  and
# the Villagers contract in the south pole"): VILLAGERS_DIR names the
# contracts/ of a clone of EShapiro2/Villagers, each contract in NAME/contract.txt;
# the site shows those contracts alone, the globe upside down with the
# contract built on all the others at the south pole, and the Villagers paper
# alone.  It is served one level below the site's root, as the test site is.
VILLAGERS = os.environ.get('VILLAGERS_DIR') or ''
if VILLAGERS:
    TEST = True
WORLDS, WWW = os.path.join(GLP, 'programs', 'vglp', 'cva', 'web_worlds'), os.path.join(BUILD, 'www')
sys.dont_write_bytecode = True
sys.path.insert(0, os.path.join(HERE, 'tools'))
from menu_spec import spec   # noqa: E402
from papers import cite_of, foundational, key_of, load_state, paper_of  # noqa: E402
import directories  # noqa: E402
import certify  # noqa: E402

LAYERS = [('schemas', 'schemas.txt'), ('verdict', 'verdict.txt'), ('vmat', 'vmat.tex'),
          ('platform', 'platform.vglp'), ('glp', 'platform.glp')]


def read(p):
    with open(p, encoding='utf-8') as f:
        return f.read()


def split_top(s):
    out, depth, buf = [], 0, ''
    for ch in s:
        if ch in '([': depth += 1
        if ch in ')]': depth -= 1
        if ch == ',' and depth == 0:
            out.append(buf.strip()); buf = ''; continue
        buf += ch
    if buf.strip(): out.append(buf.strip())
    return out


def not_compiled(text):
    """The compiler's reasons for printing no platform, as cascade.sh writes
    them into platform.vglp: each reason once, with the speech acts it names
    on the '%% the schema...(name...)' line after it."""
    reasons, last = {}, None
    for l in text.split('\n'):
        if not l.startswith('%% ') or l.startswith('%% not compiled'):
            continue
        l = l[3:].strip()
        m = re.match(r'^the schema[^(]*\(([a-z_0-9]+)', l)
        if m and last is not None:
            if m.group(1) not in reasons[last]:
                reasons[last].append(m.group(1))
            continue
        m = re.match(r'^(.*?):\s*\(([a-z_0-9]+)\)\s*$', l)
        last = (m.group(1) if m else l).rstrip(':').strip()
        reasons.setdefault(last, [])
        if m and m.group(2) not in reasons[last]:
            reasons[last].append(m.group(2))
    return [r + (f" ({', '.join(ns)})" if ns else '') for r, ns in reasons.items()]


def parse_rules(text):
    """schemas.txt in the form cascade.sh prints since 2026-11: each speech act
    'name: heads <-' and, indented, its body, ending 'guarded by Alice, Bob'
    where it is guarded.  A head 'not A' removes A; every other head adds."""
    header, out, cur = [], [], None
    for line in text.split('\n'):
        if not line.strip():
            cur = None
            continue
        m = re.match(r'^([a-z_0-9]+): (.*?)\s*<-\s*$', line)
        if m:
            heads = split_top(m.group(2))
            cur = {'name': m.group(1), 'head': f"{m.group(1)}: {m.group(2)}", 'form': 'rule',
                   'adds': [h for h in heads if not h.startswith('not ')],
                   'removes': [h[4:] for h in heads if h.startswith('not ')],
                   'body': '', 'needs': [], 'guarded': [], 'guard': ''}
            out.append(cur)
        elif cur is not None and line.startswith(' '):
            cur['body'] += ' ' + line.strip()
            body, _, g = cur['body'].strip().rpartition(' guarded by ')
            if not body:
                body, g = cur['body'].strip(), ''
            cur['needs'] = split_top(body)
            cur['guarded'] = [x.strip() for x in g.split(',') if x.strip()]
            cur['guard'] = g.strip()
        else:
            header.append(line.strip())
    return header, out


# A label is a contract's name, a colon and a clause's number: 'GSG:1',
# 'Community Bank:1', "Workers' Cooperative:2" (calculus/NAMES.txt); an act
# that a later contract's clause changes has the labels of both, joined by
# '+', 'Booking:1+Tennis Courts:1' (Audit #4, ticket 138: until 2026-11-25
# such acts were left out).
LABEL = r"[A-Z][A-Za-z' -]*?:\d+"
CALC_LINE = re.compile(r"^(" + LABEL + r"(?:\+" + LABEL + r")*) (\S) (.*)$")


def parse_calculus(text):
    """The schemas as the Calculus's tools print them (schemas.txt and
    full-schemas.txt of a contract adopted from the Calculus): a line for each
    speech act, 'GSG:1 ○ Befriend: heads <- body guarded by Alice, Bob.',
    labelled by the contract and clause it is from, a head or a body atom
    '~A' its negation; a line 'GSG:1 ○ | ...' is the clause's prose, and the
    lines with no label, the title and the Logic pragmas, are the header."""
    header, out = [], []
    for line in text.split('\n'):
        if not line.strip():
            continue
        m = CALC_LINE.match(line)
        if not m:
            header.append(line.strip())
            continue
        rest = m.group(3)
        labels = [(n, int(k)) for n, k in (x.rsplit(':', 1) for x in m.group(1).split('+'))]
        r = re.match(r'^([^|:][^:]*): (.*?)\s*<-\s*(.*?)\.?$', rest)
        if rest.startswith('|') or not r:
            continue
        body, sep, g = r.group(3).rpartition('guarded by ')
        if not sep:
            body, g = r.group(3), ''
        heads, needs = split_top(r.group(2)), split_top(body)
        out.append({'name': r.group(1), 'head': f"{r.group(1)}: {r.group(2)}", 'form': 'rule',
                    'origin': file_of(labels[0][0]), 'clause': labels[0][1],
                    'labels': [f"{n}:{k}" for n, k in labels], 'by': [file_of(n) for n, _ in labels],
                    'adds': [h for h in heads if not h.startswith('~')],
                    'removes': [h[1:] for h in heads if h.startswith('~')],
                    'body': body.strip(), 'needs': ['not ' + n[1:] if n.startswith('~') else n for n in needs],
                    'guarded': [x.strip() for x in g.split(',') if x.strip()], 'guard': g.strip()})
    return header, out


def parse_schemas(path):
    """schemas.txt, as cascade.sh prints it, into heads and roles: the form
    by roles of the records before 2026-11, or the form by rules since; and,
    of a contract adopted from the Calculus, the Calculus's form, read from
    full-schemas.txt beside it where it is, which has the acts of the
    contracts it is written from, each labelled by its origin."""
    full = os.path.join(os.path.dirname(path), 'full-schemas.txt')
    for q in (full, path):
        if os.path.exists(q) and any(CALC_LINE.match(l) for l in read(q).split('\n')):
            return parse_calculus(read(q))
    if not os.path.exists(path): return [], []
    if re.search(r'^[a-z_0-9]+: .*<-\s*$', read(path), re.M):
        return parse_rules(read(path))
    header, out, cur = [], [], None
    for line in read(path).split('\n'):
        if not line.strip(): continue
        if line.startswith('    at '):
            m = re.match(r'    at (.+?): (.*)$', line)
            r = {'role': m.group(1), 'need': [], 'not': [], 'add': [], 'del': []}
            for it in split_top(m.group(2)):
                if it.startswith('not '): r['not'].append(it[4:])
                elif it.startswith('+'): r['add'].append(it[1:])
                elif it.startswith('-'): r['del'].append(it[1:])
                else: r['need'].append(it)
            cur['roles'].append(r)
        elif re.match(r'^[a-z_0-9]+\(', line):
            m = re.match(r'^([a-z_0-9]+)\((.*?)\)(?: \| (.*?))? :$', line)
            roles, _, vars_ = m.group(2).partition(';')
            cur = {'name': m.group(1), 'head': line[:-2], 'constraint': m.group(3) or '', 'form': 'roles',
                   'parties': split_top(roles), 'vars': [x.strip() for x in vars_.split(',') if x.strip()],
                   'roles': []}
            out.append(cur)
        else:
            header.append(line.strip())
    return header, out


def contract_text(path):
    """A NAME.txt: its % lines, title, the declaration of its names, clauses, pragmas."""
    lines = read(path).split('\n')
    meta = [l for l in lines if l.startswith('%')]
    body = [l for l in lines if l.strip() and not l.startswith('%')]
    # A title line is 'NAME: the name in full', or, where the name is the
    # whole title, the name alone: 'Notes' (Calculus #8, 2026-11-16).
    name, title = body[0].split(': ', 1) if ': ' in body[0] else (body[0].strip(), body[0].strip())
    shown, name = name, file_of(name)
    # The pragmas follow the clauses.  A contract adopted from the Calculus
    # (calculus/MIGRATION.md) has its Logic pragmas after its clauses, under
    # 'Logic pragmas', and its Interface pragmas after them; an earlier text
    # has them under 'Pragmas', or under 'Interface pragmas' and
    # 'Implementation pragmas', their two kinds (GSC's Section 2;
    # Coordination, to_all 2026-11-07 22:13 UTC), the headings kept among them.
    heads = ('Pragmas', 'Interface pragmas', 'Implementation pragmas')
    box = next((i for i, l in enumerate(body) if l.strip() in heads + ('Logic pragmas',)), len(body))
    end = next((i for i, l in enumerate(body) if l.strip() in heads), len(body))
    clauses, preamble = [], []
    for l in body[1:box]:
        # A clause's title ends with a full stop, or, in the Calculus's text,
        # where the title is the clause's act, with a colon; 'sep' keeps
        # which, so the page prints the clause as it is written.
        m = re.match(r'(\d+)\. (\S) (.+?)([.:])\s+(.*)$', l)
        if m:
            clauses.append({'n': int(m.group(1)), 'mod': m.group(2), 'name': m.group(3), 'sep': m.group(4), 'text': m.group(5)})
        elif not clauses:
            # Every line of the box before its clauses, in its order: the
            # extension line, the definitions that are part of the contract
            # and the declaration of its names, whichever comes first (style
            # guide, 23 and 36; Udi, 2026-11-09, to GO #2).
            preamble.append(l.strip())
    return {'id': name, 'name': shown, 'title': title, 'preamble': '\n'.join(preamble), 'clauses': clauses,
            'pragmas': [l for l in body[end:] if l.strip() != 'Pragmas'], 'meta': meta}


# The names of the contracts (calculus/NAMES.txt, Calculus #8, Udi approving
# the names, 2026-11-16): a contract's name is its title line, one plain word
# or two, 'Community Bank'; its file name is the name with a hyphen for each
# space and no apostrophe, 'Community-Bank', which names its files, its
# record cascade/NAME/ and, here, its page.  A reader knows a name as one of
# the known names and nothing else, matched longest first.  The site shows
# the name and writes paths and addresses by the file name; it takes each
# name from the contract's title line, and reads no table of names.  The keys
# of web/about.json are file names.
def file_of(name):
    return name.replace("'", '').replace(' ', '-')

# Every contract's text, wherever its owner keeps it: programs/**/NAME.txt.
texts = {}
for d, _, fs in os.walk(os.path.join(GLP, 'programs')):
    for f in fs:
        if f.endswith('.txt') and f[:-4] == f[:-4].strip() and re.fullmatch(r'[A-Z][A-Za-z]*(?:-[A-Za-z]+)*', f[:-4]):
            p = os.path.join(d, f)
            first = next((l for l in read(p).split('\n') if l.strip() and not l.startswith('%')), '')
            if first.startswith(f[:-4] + ': ') or file_of(first.strip()) == f[:-4]:
                texts[f[:-4]] = p

# The Villagers site's contracts: contracts/NAME/contract.txt of the Villagers repository, beside
# every contract of programs/, of which the site keeps those they inherit from (KEEP, below).
VILL = set()
if VILLAGERS:
    for f in sorted(os.listdir(VILLAGERS)):
        if os.path.exists(os.path.join(VILLAGERS, f, 'contract.txt')):
            texts[f] = os.path.join(VILLAGERS, f, 'contract.txt')
            VILL.add(f)

# The owners, as cascade.sh's table names them; else the first word of the % Owner line.
owners = {}
for m in re.finditer(r'^#\s+[a-z]+\s+([A-Z][A-Za-z]*)\s+(\S+)\s+programs/', read(os.path.join(GLP, 'cascade.sh')), re.M):
    owners[m.group(1)] = m.group(2)


# The six relations of the Calculus (calculus/README.md, The relations):
# extends, restricts, strengthens, relaxes (the sixth, Calculus #8,
# 2026-11-16), instantiates and composes, the last read
# apart below; implements is GSC's, and amends stands while a text in force
# states it.
RELATIONS = ('extends', 'restricts', 'strengthens', 'relaxes', 'amends', 'implements', 'instantiates',
             # The Calculus's words since 2026-11-24 (calculus/contracts): a
             # contract unites, renames or lifts others, and weakens or
             # augments what it extends (Website #6).
             'unites', 'renames', 'lifts', 'weakens', 'augments',
             # 'Villagers inherits from Village Life and ...' (the Villagers
             # repository since 2026-11-24, Calculus item 71).
             'inherits')
# The Calculus's operations: of its verbs (calculus/tools/expand.py, VERBS),
# in their order, those the Calculus's own contracts state in their first
# lines (calculus/contracts/NAME/contract.txt, line 2), so the globe's key is
# the Calculus's and never falls behind it (Udi, 2026-11-25: "arrow legends in
# the globes not aligned with operations in Calculus, please revise").
_VERBS = ast.literal_eval(re.search(r'^VERBS = (\([^)]*\))', open(os.path.join(GLP, 'calculus', 'tools', 'expand.py')).read(), re.M).group(1))
_FIRST = ' '.join((open(os.path.join(GLP, 'calculus', 'contracts', d, 'contract.txt')).read().split('\n') + ['', ''])[1]
                  for d in sorted(os.listdir(os.path.join(GLP, 'calculus', 'contracts')))
                  if os.path.exists(os.path.join(GLP, 'calculus', 'contracts', d, 'contract.txt')))
OPERATIONS = [v for v in _VERBS if re.search(r'\b' + v + r'\b', _FIRST)]


NAMEID, NAMES_RE = {}, [None]


def names_in(text, possessive=False):
    """The contracts a text names, by their file names, in its order: a name
    is one of the known names and nothing else, matched longest first
    (calculus/NAMES.txt), so 'Community Bank restricts Bank and extends
    Coins' names three; with possessive, a name followed by 's is an owner
    and not a contract named."""
    if NAMES_RE[0] is None:
        NAMES_RE[0] = re.compile(r"(?<![A-Za-z])(" + '|'.join(re.escape(x) for x in sorted(NAMEID, key=len, reverse=True)) + r")(?![A-Za-z])" )
    out = []
    for m in NAMES_RE[0].finditer(text):
        if possessive and text[m.end():m.end() + 2] in ("'s", '\u2019s'):
            continue
        out.append(NAMEID[m.group(1)])
    return out


def extends_of(c, lines):
    """The relation a contract states and the contracts it relates to: the box's own first line, 'NAME amends GSN and GFP (cite).' (style guide, 36; GSC, Section 8), wherever it stands among the text's lines, or failing it the same sentence in a % line (DGC's); failing both, the % Extends or % Restricts line, its first sentence, less the contracts named only 'through' another."""
    # The sentence is read to its full stop, each reference in parentheses
    # skipped, wherever it stands: 'GNL extends GNO, GDO (GRH, ...) and REC.'
    # extends REC too (style guide, 36; GO #4, 2026-11-10).
    rel = re.compile(r'(?<![A-Za-z])' + re.escape(c['name']) + r' (' + '|'.join(RELATIONS + ('composes',)) + r')\b([^.]+)')
    for l in [l for l in lines if not l.startswith('%')] + [l for l in lines if l.startswith('%')]:
        m = rel.search(re.sub(r'\([^()]*\)', '', l))
        if m:
            # Each contract named takes the relation written before it: 'GAI
            # extends GnW and amends GFP' is two relations, one to each.
            # After 'instantiates X', a relation that names no contract is the
            # contract's relation to its instance of X ('CSSN instantiates GSNW
            # ... and restricts and extends it'), or, after 'composes it with
            # Y', to that composition ('GAI instantiates GnW, composes it with
            # GFP and restricts the composition'), each checked on its own
            # (GSC #16, 2026-11-11 19:45 UTC, from GSC's Section 9).
            c['relation'], c['rels'], cur = m.group(1), {}, m.group(1)
            # rels_all: every relation the line states to each contract, in
            # its order, 'GDO-L extends and strengthens GDO' two; the globe
            # draws an arrow of two relations split along its length.
            c['rels_all'] = {}
            c['rels_of'], inst, comp, pend = [], None, None, []
            for part in re.split(r'\b(' + '|'.join(RELATIONS + ('composes',)) + r')\b', m.group(2)):
                if part in RELATIONS + ('composes',):
                    cur = part
                    continue
                named = [t for t in names_in(part) if t != c['id']]
                # 'GDC extends and restricts GC': the contract named takes the
                # first of the relations written before it.
                first = pend[0] if pend and not (inst or comp) else cur
                for t in named:
                    c['rels'].setdefault(t, first)
                    for r in (pend if not (inst or comp) else []) + [cur]:
                        if r not in c['rels_all'].setdefault(t, []):
                            c['rels_all'][t].append(r)
                    if cur == 'instantiates':
                        inst = inst or t
                    if cur == 'composes':
                        comp = comp or inst or t
                pend = [] if named else pend + [cur]
                # 'AI Agents unites a renaming of Guardians and Forwarding, and
                # extends and augments the union': a relation that names no
                # contract, where there is no instance or composition, is to
                # what the line made of the contracts named before it, and so
                # to each of them (Udi, 2026-11-25).
                if not named and not (inst or comp) and c['rels_all'] and re.search(r'\b(it|them|the)\b', part):
                    for t in c['rels_all']:
                        for r in pend[:-1]:
                            if r not in c['rels_all'][t]:
                                c['rels_all'][t].append(r)
                        if cur not in c['rels_all'][t]:
                            c['rels_all'][t].append(cur)
                # 'GAS composes GPD with GSV, restricts the composition, and
                # extends it': a composition with no instance, named by the
                # first contract composed (GSC's Section 2, GSC 220151a).
                if not named and (inst or comp) and cur not in ('instantiates', 'composes'):
                    of = 'composition' if comp else 'instance'
                    if not any(x['rel'] == cur for x in c['rels_of']):
                        c['rels_of'].append({'rel': cur, 'of': of, 'base': inst or comp})
            return list(c['rels'])
    line = next((l for l in c['meta'] if l.startswith('% Extends:') or l.startswith('% Restricts:')), '% Extends:')
    c['relation'] = 'restricts' if line.startswith('% Restricts:') else 'extends'
    first = re.split(r'\.\s{2}', line.split(':', 1)[1].strip())[0]
    named = []
    for part in first.split(';'):   # each part names some contracts, then perhaps 'through it' their own
        # a name followed by 's is an owner (GAI's), not a contract extended
        named += names_in(re.split(r'\bthrough\b|\bfactored\b', part)[0], possessive=True)
    found = [t for t in dict.fromkeys(named) if t in texts and t != c['id']]
    c['rels'] = {t: c['relation'] for t in found}
    return found


cat = {n: contract_text(p) for n, p in texts.items()}
NAMEID.update({c['name']: n for n, c in cat.items()})
for c in cat.values():
    c['extends'] = extends_of(c, read(texts[c['id']]).split('\n'))
    own = next((l for l in c['meta'] if l.startswith('% Owner:')), '% Owner: ')
    c['owner'] = re.sub(r'[^A-Za-z].*$', '', own[len('% Owner:'):].strip()) or owners.get(c['id'], '')
    if c['id'] in VILL:
        c['owner'] = 'Villagers'
    c['source'] = 'example' if c['meta'] and re.search(r'an (example|experimental) contract', c['meta'][0]) else 'paper'

def has_platform(n):
    p = os.path.join(GLP, 'cascade', n, 'platform.vglp')
    # Four records hold the compiler's refusal, '%% not compiled(...)', where a
    # pragma is missing: no platform, no live world.
    return os.path.exists(p) and 'exported procedure agent(' in read(p)


compiled = sorted(n for n in os.listdir(os.path.join(GLP, 'cascade')) if has_platform(n) and n in cat)


def ancestors0(cid, seen=None):
    seen = set() if seen is None else seen
    for p in cat[cid]['extends']:
        if p in cat and p not in seen:
            seen.add(p)
            ancestors0(p, seen)
    return seen


# The Villagers site keeps the Villagers repository's contracts and every
# contract they inherit from, directly or through others (Udi, 2026-11-25
# 01:07 UTC: "it should include all contracts villagers inherit from, and show
# both terse and full contracts").
KEEP = set(cat)
if VILLAGERS:
    KEEP = set(VILL)
    for v in VILL:
        KEEP |= ancestors0(v)
    compiled = [n for n in compiled if n in KEEP]
# The test site writes no world: its apps are the live server's, so a contract
# is live there where the live build made its world.
# The live build's worlds are in the live clone, LIVE_WORLDS, which
# deploy/test.sh names, since the test site builds its own tree, which has
# none (Website #6, 2026-11-24: /next said "0 run as apps").
if TEST:
    WORLDS = os.environ.get('LIVE_WORLDS') or WORLDS
    compiled = [n for n in compiled if os.path.exists(os.path.join(WORLDS, f'live_{n.lower()}', 'platform.vglp'))]

shutil.rmtree(BUILD, ignore_errors=True)
os.makedirs(os.path.join(WWW, 'specs'))
if not TEST:
    shutil.rmtree(WORLDS, ignore_errors=True)
    os.makedirs(WORLDS)
    with open(os.path.join(WORLDS, '.gitignore'), 'w') as f:
        f.write('# Generated by servers/peoplesweb/tools/build.py, and rebuilt whole by it.\n*\n')

for n in compiled:
    c = n.lower()
    if not TEST:
        subprocess.run([sys.executable, '-I', os.path.join(HERE, 'tools', 'gen_harness.py'),
                        os.path.join(GLP, 'cascade', n, 'platform.vglp'), os.path.join(WORLDS, f'live_{c}'), n],
                       check=True, stdout=subprocess.DEVNULL)
    s = spec(os.path.join(WORLDS, f'live_{c}', 'platform.vglp'))
    s['id'], s['title'] = n, cat[n]['title']
    s['calendar'] = 'cal(alice, N)' in read(os.path.join(WORLDS, f'live_{c}', 'self.glp'))
    s.pop('raw_items', None)
    with open(os.path.join(WWW, 'specs', f'{c}.json'), 'w', encoding='utf-8') as f:
        json.dump(s, f)


# The Calculus (calculus/MIGRATION.md, the section Website, agreed by
# Website #2, 2026-11-15): until its owner adopts a contract, the Calculus's
# text of it is in CALCULUS_DIR, which until 2026-11-17 deploy/sweep.sh
# unpacked from the branch Calculus of GLP, now closed; after its adoption it
# is on main, and its printed artefacts in cascade/NAME/, as every contract is
# since 2026-11-16.  calculus/STATE.txt, where there is one, has a line for each
# contract: its name, its owner, and yes or no for certified by GLP's checker,
# compiled to GLP, and adopted by its owner.
# Since 2026-11-24 the Calculus keeps each contract in calculus/contracts/NAME/ (Code #37).
CALC = os.environ.get('CALCULUS_DIR') or next((d for d in (os.path.join(GLP, 'calculus', 'contracts'), os.path.join(GLP, 'calculus')) if os.path.exists(os.path.join(d, 'STATE.txt'))), os.path.join(GLP, 'calculus', 'contracts'))
calc_state = {}
if os.path.exists(os.path.join(CALC, 'STATE.txt')):
    for l in read(os.path.join(CALC, 'STATE.txt')).split('\n'):
        f = l.split()
        if len(f) == 5:
            calc_state[f[0]] = {'owner': f[1], 'certified': f[2] == 'yes', 'compiled': f[3] == 'yes', 'adopted': f[4] == 'yes'}


def calculus_of(n):
    """What the site shows of contract n from the Calculus: its state, and,
    until its owner adopts it, the text proposed; the full contract, from the
    branch until adoption and from cascade/n/ after it.  A contract is
    adopted when STATE.txt says so, or when its record of the cascade has
    its full contract, which its owner's adoption prints there; so a
    contract adopted on main shows its owner's full contract whatever the
    branch says, and still does when the branch is closed."""
    st = calc_state.get(n)
    own = os.path.join(GLP, 'cascade', n, 'full-contract.txt')
    if st is None and not os.path.exists(own):
        return None
    v = dict(st or {'owner': '', 'certified': True, 'compiled': True})
    v['adopted'] = bool(st and st['adopted']) or os.path.exists(own)
    if not v['adopted'] and os.path.exists(os.path.join(CALC, n, 'contract.txt')):
        v['proposed'] = read(os.path.join(CALC, n, 'contract.txt')).strip()
    # The Calculus's full contract, printed from the current texts, where it
    # has one: a record of the cascade keeps the full contract of the text it
    # was printed from, and 28 records were behind the Calculus on
    # 2026-11-24 (Calculus #11 to Website #6: the site shows the text of
    # calculus/contracts/NAME/ and its pragmas).
    calc_fc = os.path.join(CALC, n, 'full-contract.txt')
    fc = calc_fc if os.path.exists(calc_fc) else own if v['adopted'] else calc_fc
    return full_of(v, n, fc)


def villagers_of(n):
    """A contract of the Villagers repository: its full contract, as its
    owner prints it, contracts/n/full-contract.txt, where there is one."""
    fc = os.path.join(VILLAGERS, n, 'full-contract.txt')
    if not os.path.exists(fc):
        return None
    return full_of({'owner': 'Villagers', 'certified': False, 'compiled': False, 'adopted': True}, n, fc)


def full_of(v, n, fc):
    """v with the full contract fc, where it exists, copied into the site."""
    if os.path.exists(fc):
        os.makedirs(os.path.join(WWW, 'stack', n), exist_ok=True)
        shutil.copy(fc, os.path.join(WWW, 'stack', n, 'full-contract.txt'))
        v['full'] = f'stack/{n}/full-contract.txt'
        # The full contract in the form of the contract itself, for the page
        # to show the two in one box (Udi, 2026-11-16, to Website #2): its
        # lines before the clauses, and each clause with the contract and
        # number it is from, 'GKK:1'; a line that opens with % is the tools'.
        pre, cls = [], []
        for l in [l for l in read(fc).split('\n') if l.strip() and not l.startswith('%')][1:]:
            # A clause may be labelled with several, 'Booking:1, Tennis
            # Courts:1.' (Udi, 2026-11-19, to Website #4: the Booking clauses
            # were missing from Tennis Courts' full contract).
            m = re.match(r"([A-Z][A-Za-z' -]*?:\d+(?:, [A-Z][A-Za-z' -]*?:\d+)*)\. (\S) (.+?)([.:])\s+(.*)$", l)
            if m:
                cls.append({'n': m.group(1), 'mod': m.group(2), 'name': m.group(3), 'sep': m.group(4), 'text': m.group(5)})
            elif not cls:
                pre.append(l.strip())
        v['fullc'] = {'preamble': '\n'.join(pre), 'clauses': cls}
    return v


about = json.loads(read(os.path.join(HERE, 'web', 'about.json')))
look = json.loads(read(os.path.join(GLP, 'programs', 'apps', 'look.json')))
# The papers' PDFs: those tools/papers_sync.py keeps current in PAPERS_DIR on
# the server, with its state.json; a build with no PAPERS_DIR links none.
synced = os.environ.get('PAPERS_DIR') or ''
pdfs = {}
if synced and os.path.isdir(synced):
    for f in os.listdir(synced):
        if f.endswith('.pdf'):
            pdfs[f] = os.path.join(synced, f)
served = {f[:-4] for f in pdfs}
paper_state = load_state(synced) if synced else {}
byid = dict(cat)


def ancestors(cid):
    out = []
    for p in byid[cid]['extends']: out += [p] + ancestors(p)
    return out


shown = set(cat) & KEEP


def nearest(cid):
    out = []
    for p in byid[cid]['extends']:
        out += [p] if p in shown else nearest(p)
    return list(dict.fromkeys(out))


def look_line(pragmas):
    """The look a contract's interface pragmas open with, 'Look: <icon>,
    <family>' (GSC's Section 2, 34bc713), as (icon, family); None where the
    first line is not it."""
    for l in pragmas:
        if l.strip() in ('Interface pragmas', 'Implementation pragmas'):
            continue
        m = re.match(r'Look:\s*(.+?),\s*([\w-]+)\s*$', l.strip())
        return (m.group(1), m.group(2)) if m else None
    return None


contracts = []
for n in sorted((set(compiled) | set(cat)) & KEEP):
    c = dict(cat[n])
    d = os.path.join(GLP, 'cascade', n)
    # The icon and family the contract's Look line names, where its owner
    # wrote one, and else about.json's, which holds them until then.
    _a = about.get(n) or ['📄', 'other', '']
    _icon, _family = look_line(c.get('pragmas', [])) or (_a[0], _a[1])
    c['icon'], c['family'], c['about'] = _icon, _family, _a[2] or c['title'] + '.'
    # The shown name starts with the letters, and a contract built on another
    # names the base by its letters, never by its full title: "GDO-L: GDO with
    # Labels" (Udi, 2026-11-07).  The base is named by its shown name, so a
    # contract named in words, Backed Community Bank, keeps its name whole
    # (Website #6, 2026-11-24).
    t = c['title']
    for p in c['extends']:
        if p in cat:
            t = re.sub(re.escape(cat[p]['title']), cat[p]['name'], t, flags=re.I)
    c['label'] = f"{c['name']}: {t}" if t != c['name'] else c['name'] 
    c['cascade'] = n in compiled
    c['dir'] = os.path.relpath(os.path.dirname(texts[n]), GLP) if n in texts and n not in VILL else ''
    c['paper'] = cite_of('Villagers', served, paper_state) if n in VILL else paper_of(next((l for l in c['meta'] if l.startswith('% Owner:')), ''), served, paper_state)
    c['paperKey'] = key_of(next((l for l in c['meta'] if l.startswith('% Owner:')), ''))
    c['schemaHeader'], c['schemas'] = parse_schemas(os.path.join(d, 'schemas.txt'))
    c['verdict'] = read(os.path.join(d, 'verdict.txt')).strip() if os.path.exists(os.path.join(d, 'verdict.txt')) else ''
    c['live'], c['noteBefore'], c['layers'] = n in compiled, '', {}
    # Whether the record has a platform, whatever runs: on a test site built
    # over a tree with no world nothing is live, and the
    # table still counts the platforms printed and compiled.
    c['platform'] = has_platform(n)
    # Why a contract with a record has no app: the compiler's reasons, as
    # cascade.sh writes them into platform.vglp in place of a platform; a
    # contract with no record has not been through the cascade.
    c['record'] = os.path.isdir(d)
    # A parametric contract has no realisation, so no platform, by design
    # (GSC, Definition Parametric contract); its manifest says so on its
    # 'Parametric:' line (GCE's audit proposal, Grassroots-Audit PR 28).
    mf = os.path.join(d, 'manifest.md')
    c['parametric'] = os.path.exists(mf) and any(l.startswith('- Parametric:') for l in read(mf).split('\n'))
    pv = os.path.join(d, 'platform.vglp')
    # Asked only of a record that has no platform: on a test site with no
    # world nothing is live, and a whole platform read as its reasons made
    # /next's data.js 8 MB (2026-11-16).
    c['notCompiled'] = [] if c['live'] or has_platform(n) or not os.path.exists(pv) else not_compiled(read(pv))
    for k, f in LAYERS:
        if os.path.exists(os.path.join(d, f)):
            os.makedirs(os.path.join(WWW, 'stack', n), exist_ok=True)
            shutil.copy(os.path.join(d, f), os.path.join(WWW, 'stack', n, f))
            c['layers'][k] = {'file': f'stack/{n}/{f}', 'lines': read(os.path.join(d, f)).count('\n')}
    # The Full contract button's text (Udi, 2026-11-15, to Calculus #6,
    # verbatim: "i want the web to include for each contract a button you
    # press 'full contract' that show the full contract"), and the Calculus's
    # proposed text and state.
    c['calculus'] = villagers_of(n) if n in VILL else calculus_of(n)
    # A contract that builds on one with no platform is drawn building on the nearest shown ones.
    ps = nearest(n)
    c['extends'] = [p for p in ps if not any(p in set(ancestors(q)) for q in ps if q != p)]
    c.pop('meta')
    contracts.append(c)

for c in contracts:
    for s in c['schemas']:
        # The Calculus's form names each act's origin itself.
        # An act a later clause changes, 'Booking:1+Tennis Courts:1', is from
        # the contract it was first written in, and is this contract's own
        # where this contract's clause is among its labels.
        if s.get('origin'):
            by = s.pop('by', [])
            s['from'] = c['id'] if c['id'] in by else s.pop('origin')
            s.pop('origin', None)
            continue
        s['from'] = c['id']
        for a in ancestors(c['id']):
            if a in shown and any(x['name'] == s['name'] for x in next(k for k in contracts if k['id'] == a)['schemas']):
                s['from'] = a

# The relations each contract states, checked against the schemas (GSC,
# Section 8), and each app of programs/apps as a certified artefact, both
# through the REPL and cached (tools/certify.py); CERTIFY=0 skips both, for a
# quick build of the pages alone.
if os.environ.get('CERTIFY', '1') != '0':
    # The relations are not checked for now (Udi, 2026-11-16, to Website #2):
    # certify.relations checks them by GSC's definitions, 'an extension
    # deletes nothing', and the Calculus, the authority on the relations since
    # 2026-11-15, defines them otherwise, so its verdicts would mislead (28 of
    # 102 'disagree' on the adopted contracts).  RELATIONS_CHECK=1 runs it,
    # for when it follows the Calculus's definitions.
    checks = certify.relations([{'id': c['id'], 'path': texts[c['id']], 'rels': c.get('rels', {}), 'rels_of': c.get('rels_of', [])} for c in contracts]) if os.environ.get('RELATIONS_CHECK') == '1' else {}
    arts = certify.artefacts(os.path.join(WWW, 'artefacts'))
else:
    checks, arts = {}, {}
def relations_of(n):
    """The Calculus's verdict on each relation contract n's first line states,
    as calculus/tools print it into its record, cascade/n/relations.txt, one
    line a relation: 'GSN extends GSG: holds', or 'CnC restricts it (GCoop
    and TC): FAILS: <act>: <what>; ...' (Calculus #6, 2026-11-16 07:23 UTC).
    The site prints each line and says nothing of its own; a record with no
    such file has no verdict."""
    f = os.path.join(GLP, 'cascade', n, 'relations.txt')
    out = []
    for l in read(f).split('\n') if os.path.exists(f) else []:
        m = re.match(r'(.+?): (holds|FAILS)(?::\s*(.*))?$', l.strip())
        if m and not l.startswith('%'):
            out.append({'line': m.group(1), 'result': 'agrees' if m.group(2) == 'holds' else 'disagrees', 'why': m.group(3) or ''})
    return out


NOT_CHECKED = "the site's check does not yet follow the Calculus's definitions of the relations"
for c in contracts:
    c['checks'] = relations_of(c['id']) or checks.get(c['id']) or (
        [{'base': b, 'rel': r, 'result': 'open', 'why': NOT_CHECKED} for b, rs in (c.get('rels_all') or {k: [v] for k, v in (c.get('rels') or {}).items()}).items() for r in rs]
        + [{'base': x['base'], 'rel': x['rel'], 'of': x['of'], 'result': 'open', 'why': NOT_CHECKED} for x in c.get('rels_of', [])])
    a = arts.get(c['id'].lower().replace('-', '_'))
    c['artefact'] = ({'file': 'artefacts/' + a['file'], 'key': a['key']} if a and 'file' in a else {'why': a['why']} if a else None)
with open(os.path.join(WWW, 'relations.json'), 'w', encoding='utf-8') as f:
    json.dump({c['id']: c['checks'] for c in contracts if c['checks']}, f, ensure_ascii=False, indent=1)

# A contract the Calculus has and its owner has not written, SGSG today: listed
# from the Calculus with its proposed text and nothing in force, until its
# owner adopts it (calculus/MIGRATION.md, Website, item 2).
# A contract of the Calculus with no owner ('none' in calculus/contracts/STATE.txt),
# the Villagers repository's thirteen today, is not on the main globe: the
# Villagers contracts stay on the Villagers site (Udi, 2026-11-26 16:48 UTC:
# "villagers-specific contracts stay there and not imported back to the main
# globe").
calc_only = [n for n in sorted(set(calc_state) - set(cat) if not VILLAGERS else []) if calc_state[n]['owner'] != 'none' and 'proposed' in (calculus_of(n) or {})]
for n in calc_only:
    v = calculus_of(n)
    if not v or 'proposed' not in v:
        continue
    lines = v['proposed'].split('\n')
    title = lines[0].split(': ', 1)[1] if ': ' in lines[0] else n
    # The first line names what it builds on: 'X extends Y.' before
    # 2026-11-26, 'X inherits from Y, Z and W.' since the Calculus's first
    # lines (c63b27e), X its title; each name, as the title lines spell it
    # ('Lost and Found'), found longest first, a contract of the Calculus
    # alone that it builds on being one of them too.
    m = next((re.match(re.escape(t) + r' (?:extends|inherits from) ([^.]+)\.', l) for l in lines for t in (title, n, lines[0].strip()) if l.startswith((t + ' extends ', t + ' inherits from '))), None)
    known = {**{k: v for k, v in NAMEID.items() if v in cat}, **{x.replace('-', ' '): x for x in calc_only}}
    ext, rest = [], m.group(1) if m else ''
    for k in sorted(known, key=len, reverse=True):
        if re.search(r'\b' + re.escape(k) + r'\b', rest):
            rest = re.sub(r'\b' + re.escape(k) + r'\b', ' ', rest)
            ext.append(known[k])
    _a = about.get(n, ['📄', 'other', ''])
    contracts.append({'id': n, 'title': title, 'label': f'{n}: {title}', 'preamble': '', 'clauses': [], 'pragmas': [],
                      'extends': ext, 'rels': {x: 'extends' for x in ext}, 'relation': 'extends', 'owner': '' if v['owner'] == 'none' else v['owner'],
                      'source': 'calculus', 'icon': _a[0], 'family': _a[1], 'about': _a[2] or title + '.',
                      'cascade': False, 'dir': '', 'paper': {}, 'schemaHeader': [], 'schemas': [], 'verdict': '',
                      'live': False, 'noteBefore': '', 'layers': {}, 'record': False, 'notCompiled': [],
                      'checks': [], 'artefact': None, 'calculus': v})
commit = subprocess.run(['git', '-C', GLP, 'log', '-1', '--format=%H %cs'], capture_output=True, text=True).stdout.split()
for f in os.listdir(os.path.join(HERE, 'web')):
    if f.endswith(('.html', '.js', '.png', '.ico', '.jpg', '.svg')):
        shutil.copy(os.path.join(HERE, 'web', f), os.path.join(WWW, f))
# The page an invitation link opens, peoplesweb.org/i (GNA, Section 6,
# Invitation links), and the file that names the Grassroots App to iOS as the
# app that opens such a link.
for d in ('i', '.well-known', 'katex'):
    shutil.copytree(os.path.join(HERE, 'web', d), os.path.join(WWW, d), dirs_exist_ok=True)
# The Villagers site's link preview says what it is (Udi, 2026-11-25: "link
# preview of villagers should reflect what it is"): its title, line, address
# and picture, web/og-villagers.jpg (tools/og.js, OG_URL, OG_TITLE, OG_LINE).
if VILLAGERS:
    shutil.copy(os.path.join(HERE, 'web', 'og-villagers.jpg'), os.path.join(WWW, 'og.jpg'))
    os.remove(os.path.join(WWW, 'og-villagers.jpg'))
    LINE = 'A grassroots contract for village digital life: the Villagers contract and every contract it extends.'
    t = read(os.path.join(WWW, 'index.html'))
    for old, new in [('<meta name="description" content="Social contracts that run: apps owned and run by the people who use them, on their own phones.">', f'<meta name="description" content="{LINE}">'),
                     ('<meta property="og:title" content="The Grassroots Sphere">', '<meta property="og:title" content="Villagers: A Grassroots Contract for Village Digital Life">'),
                     ('<meta property="og:description" content="Social contracts that run: apps owned and run by the people who use them, on their own phones.">', f'<meta property="og:description" content="{LINE}">'),
                     ('<meta property="og:url" content="https://peoplesweb.org/">', '<meta property="og:url" content="https://peoplesweb.org/villagers/">'),
                     ('<meta property="og:image" content="https://peoplesweb.org/og.jpg">', '<meta property="og:image" content="https://peoplesweb.org/villagers/og.jpg">'),
                     ('<meta property="og:image:alt" content="The grassroots contracts on a sphere, beside the name of the Grassroots Sphere">', '<meta property="og:image:alt" content="The Villagers contract and the contracts it extends on a sphere, Villagers at the south pole">'),
                     ('<title>The Grassroots Sphere</title>', '<title>Villagers · The Grassroots Sphere</title>')]:
        if old not in t:
            raise SystemExit('villagers: index.html has no ' + old)
        t = t.replace(old, new)
    with open(os.path.join(WWW, 'index.html'), 'w', encoding='utf-8') as f:
        f.write(t)
if served:
    os.makedirs(os.path.join(WWW, 'papers'), exist_ok=True)
    for f, path in pdfs.items():
        shutil.copy(path, os.path.join(WWW, 'papers', f))
# The projects (tools/directories.py): each with its papers, each paper with
# its citation, its contracts and its directories, and under Code the
# directories with no paper, a tree in which each is once (Udi, 2026-11-12, to
# GSA #5); the Papers page lists the same projects' papers, and after them the
# papers no project holds.  What no entry covers, and what does not fit the
# tree, the build prints and the page shows, so that every directory is
# accounted for.
projects, code, faults = ([{'id': 'Villagers', 'name': cite_of('Villagers')['title'], 'dirs': [],
                             'papers': [{'key': 'Villagers', 'dirs': [], 'contracts': [{'id': c['id'], 'items': []} for c in contracts if c['id'] in VILL]}]}], [], []) if VILLAGERS else directories.tree(GLP, [(c['id'], os.path.relpath(texts[c['id']], GLP), c.pop('paperKey')) for c in contracts if c['id'] in texts])
for p in projects:
    for q in p['papers']:
        q['cite'] = cite_of(q['key'], served, paper_state) or {}
        if not q['cite']:
            faults.append(f"the paper {q['key']} of {p['id']} is not in tools/papers.py")
lost = [] if VILLAGERS else directories.unaccounted(GLP)
if lost:
    print('directories: no entry answers for ' + ', '.join(lost))
for f in faults:
    print('projects: ' + f)
# What a contract's page shows only when asked, its speech-act schemas and its
# full contract, is a file of its own, detail/<NAME>.json, which the page
# fetches then; data.js, which every page loads, keeps their counts.  With
# them in it data.js was 3.2 MB after the Calculus migration, the full schemas
# of GO's family being some ninety a contract, and the home page was slow
# (Udi, 2026-11-16, to Website #2).
os.makedirs(os.path.join(WWW, 'detail'), exist_ok=True)
for c in contracts:
    sch = c.pop('schemas')
    c.pop('schemaHeader', None)
    c.pop('pragmas', None)
    k = c.get('calculus') or {}
    with open(os.path.join(WWW, 'detail', c['id'] + '.json'), 'w', encoding='utf-8') as f:
        json.dump({'schemas': sch, 'fullc': k.pop('fullc', None)}, f, ensure_ascii=False)
    if c.get('calculus') is not None:
        c['calculus']['hasFull'] = os.path.exists(os.path.join(WWW, 'stack', c['id'], 'full-contract.txt'))
    c['acts'] = [len(sch), sum(1 for s in sch if s.get('from') == c['id'])]
with open(os.path.join(WWW, 'data.js'), 'w', encoding='utf-8') as f:
    data = {'contracts': contracts, 'families': look['families'], 'glpCommit': commit, 'projects': projects, 'code': code,
            'noPaper': [] if VILLAGERS else [{'id': i, 'name': n, 'repo': r} for i, n, r in directories.NO_PAPER],
            'otherPapers': [] if VILLAGERS else foundational(served, paper_state, but={k for _, _, ks in directories.PROJECTS for k in ks}),
            'unaccounted': lost, 'faults': faults, 'operations': OPERATIONS}
    if TEST:
        # The test site's pages are one level below the site's root, and the
        # API they call is the live server's, at the root.
        data['test'], data['api'] = True, ''
    if VILLAGERS:
        # The globe upside down: the contract built on all the others at the
        # south pole (web/index.html, D.flip).
        data['south'], data['site'] = 'Villagers', 'villagers'
        data['villagersCommit'] = subprocess.run(['git', '-C', os.path.dirname(VILLAGERS.rstrip('/')), 'log', '-1', '--format=%H %cs'], capture_output=True, text=True).stdout.split()
    f.write('window.DATA = ' + json.dumps(data, ensure_ascii=False) + ';\n')
print(len(contracts), 'contracts:', ' '.join(c['id'] for c in contracts))
