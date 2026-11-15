#!/usr/bin/env python3
"""revtex.py --- the appendix of the contracts as revised, each marked against its original.

    python3 tools/revtex.py [--rev DIR] [--glp DIR] [--out FILE]

Reads every rev/NAME.txt (not NOTES-*.txt), finds the original NAME.txt among the
files `grep -rl --include='*.txt' '^% Owner' programs` lists in the GLP repository,
and writes sections/contracts-revised.tex: one contractbox per contract, in
dependency order (a contract after those its relation line names, GKK first), the
revised text with deleted text in \\sout and inserted text in \\uline.
main.tex needs \\usepackage[normalem]{ulem}.

The header is compared sentence by sentence, wherever the sentence stands; a
clause with the original clause of its number where the two still correspond and
otherwise with the most similar one; within a clause, by words.  It prints a line
for each contract, and its warnings: a contract with no original, a character it
replaced or dropped.

Standard library only.  The defaults are the rev/ and sections/ beside tools/,
and the GLP clone beside this repository (or $GLP_DIR).
"""
import argparse
import difflib
import os
import re
import subprocess
import sys
import unicodedata

MARKS = {'■': r'\enf', '◆': r'\att', '○': r'\und'}
END_BOX = ('Interface pragmas', 'Implementation pragmas', 'Pragmas', 'Logic pragmas')
# A contract renamed in the revision: its revised name -> its original's name.
RENAMED = {'GCC': 'CC'}
# Dropped in the revision (Udi, 2026-11-13), said once under the lead-in.
DROPPED = ('Three contracts are dropped.  GCC (Grassroots Community Coins) and BCC, whose communities '
           'have no ward, give way to CC, renamed GCC, and to GBCC.  GFF-I\'s two conditions are now GFF:2 and GFF:3.')
LEAD_IN_Q = "A decision that the marking does not explain follows its contract, numbered D1 onward."
LEAD_IN = ('Each contract as revised, marked against its text in the repository: '
           'deleted text is struck through and inserted text is underlined.')

# Marks, mathematics and URLs inside a change are set by these two, which rule a box
# at the height of ulem's \sout and \uline; ulem itself is given words only.
MACROS = r'''\makeatletter
\providecommand{\revdelbox}[1]{\leavevmode\setbox\z@\hbox{#1}\rlap{\kern-.1em\vrule height \dimexpr.55ex+.4pt\relax depth -.55ex width \dimexpr\wd\z@+.2em\relax}\box\z@}
\providecommand{\revinsbox}[1]{\leavevmode\setbox\z@\hbox{#1}\setbox\tw@\hbox{(j}\dimen@\dp\tw@\advance\dimen@ .4pt\rlap{\vrule height \dimexpr.4pt-\dimen@\relax depth \dimen@ width \wd\z@}\box\z@}
\makeatother'''

WARNINGS = []


def warn(msg):
    if msg not in WARNINGS:
        WARNINGS.append(msg)


# ---------------------------------------------------------------- characters

TEXT = {
    '\\': r'\textbackslash{}', '{': r'\{', '}': r'\}', '$': r'\$', '&': r'\&',
    '#': r'\#', '%': r'\%', '_': r'\_', '^': r'\textasciicircum{}',
    '~': r'\textasciitilde{}', '|': r'\textbar{}', '"': r'\textquotedbl{}',
    '…': r'\ldots{}', '’': "'", '‘': '`', '“': '``', '”': "''",
    '—': '---', '–': '--', ' ': '~', '‑': '-', '«': r'\guillemotleft{}',
    '»': r'\guillemotright{}', '€': r'\texteuro{}', '£': r'\pounds{}',
    '¢': r'\textcent{}', '§': r'\S{}', '°': r'\textdegree{}', '©': r'\copyright{}',
    '•': r'\textbullet{}', '✓': r'\checkmark{}',
}
MATH = {
    '<': '<', '>': '>', '≤': r'\le ', '≥': r'\ge ', '≠': r'\ne ', '≈': r'\approx ',
    '−': '-', '×': r'\times ', '÷': r'\div ', '·': r'\cdot ', '±': r'\pm ',
    '→': r'\to ', '←': r'\leftarrow ', '↔': r'\leftrightarrow ', '⇒': r'\Rightarrow ',
    '⇐': r'\Leftarrow ', '⇔': r'\Leftrightarrow ', '↦': r'\mapsto ',
    '∈': r'\in ', '∉': r'\notin ', '∀': r'\forall ', '∃': r'\exists ', '¬': r'\neg ',
    '∧': r'\wedge ', '∨': r'\vee ', '∞': r'\infty ', '∑': r'\sum ', '∏': r'\prod ',
    '√': r'\surd ', '∪': r'\cup ', '∩': r'\cap ', '⊆': r'\subseteq ', '⊂': r'\subset ',
    '∅': r'\emptyset ', '⟨': r'\langle ', '⟩': r'\rangle ', '⌈': r'\lceil ',
    '⌉': r'\rceil ', '⌊': r'\lfloor ', '⌋': r'\rfloor ', '∘': r'\circ ', '∖': r'\setminus ',
    '½': r'\frac{1}{2}', '¼': r'\frac{1}{4}', '¾': r'\frac{3}{4}', '′': "'",
}
GREEK = ('alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi '
         'o pi rho varsigma sigma tau upsilon phi chi psi omega').split()
for _k, _n in enumerate(GREEK):
    MATH[chr(0x3b1 + _k)] = _n if _n == 'o' else '\\' + _n + ' '
for _c, _n in zip('ΓΔΘΛΞΠΣΦΨΩ', 'Gamma Delta Theta Lambda Xi Pi Sigma Phi Psi Omega'.split()):
    MATH[_c] = '\\' + _n + ' '
for _c, _n in zip('ΑΒΕΖΗΙΚΜΝΟΡΤΥΧ', 'ABEZHIKMNOPTYX'):
    MATH[_c] = _n
SUB = dict(zip('₀₁₂₃₄₅₆₇₈₉₊₋', '0123456789+-'))
SUP = dict(zip('⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻', '0123456789+-'))


def segments(text):
    """The LaTeX of a token's text: a list of ('t', text) and ('m', mathematics, no $),
    and ('b', boxed material), adjacent pieces of one kind joined."""
    out = []

    def add(kind, s):
        if out and out[-1][0] == kind and kind != 'b':
            out[-1][1] += s
        else:
            out.append([kind, s])

    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if c in SUB or c in SUP:
            table, op = (SUB, '_') if c in SUB else (SUP, '^')
            j = i
            while j < n and text[j] in table:
                j += 1
            script = op + '{' + ''.join(table[x] for x in text[i:j]) + '}'
            # a single letter before the script is its base: t₀ is $t_{0}$
            if out and out[-1][0] == 't' and re.search(r'(?:^|[^A-Za-z])[A-Za-z]$', out[-1][1]):
                base = out[-1][1][-1]
                out[-1][1] = out[-1][1][:-1]
                if not out[-1][1]:
                    out.pop()
                add('m', base + script)
            elif out and out[-1][0] == 'm':
                out[-1][1] = out[-1][1].rstrip() + script
            else:
                add('m', '{}' + script)
            i = j
            continue
        if c in MARKS:
            add('b', MARKS[c])
        elif c in MATH:
            add('m', MATH[c])
        elif c in TEXT:
            add('t', TEXT[c])
        elif ord(c) < 128:
            add('t', c)
        elif unicodedata.category(c)[0] == 'L' and ord(c) < 0x250:
            add('t', c)                       # accented Latin, which pdflatex reads
        elif unicodedata.category(c) in ('Mn', 'Me', 'Cf', 'Cc', 'Cs', 'Co', 'Cn', 'Sk'):
            warn('dropped the character U+%04X' % ord(c))
        else:
            name = unicodedata.name(c, 'U+%04X' % ord(c)).lower()
            warn('%s (U+%04X) is replaced by [%s]' % (c, ord(c), name))
            add('t', '[' + ''.join(TEXT.get(x, x) for x in name) + ']')
        i += 1
    return out


def url_tex(u):
    return r'\url{' + u.replace('%', r'\%').replace('#', r'\#') + '}'


# ---------------------------------------------------------------- tokens

class Tok(object):
    __slots__ = ('text', 'kind', 'sp', 'it')

    def __init__(self, text, kind, sp, it=False):
        self.text, self.kind, self.sp, self.it = text, kind, sp, it


TOKEN = re.compile(r"""
    (?P<u>https?://[^\s<>"{}\\]+)
  | (?P<n>\d+(?:[.,]\d+)+)
  | (?P<w>[^\W_]+(?:[-'’][^\W_]+)*)
  | (?P<p>\S)
""", re.X)
ITALIC = re.compile(r'\*(?=\S)([^*]*?)(?<=\S)\*')


def tokenize(line, italics):
    """Words, numbers, URLs and single punctuation characters; sp is the white space
    before the token (1 at the start of the line), it whether it is in *italics*."""
    mask = []
    if italics:
        clean, pos = [], 0
        for m in ITALIC.finditer(line):
            clean.append(line[pos:m.start()])
            mask += [False] * (m.start() - pos)
            clean.append(m.group(1))
            mask += [True] * len(m.group(1))
            pos = m.end()
        clean.append(line[pos:])
        mask += [False] * (len(line) - pos)
        line = ''.join(clean)
    toks, end = [], 0
    for m in TOKEN.finditer(line):
        kind, text, start = m.lastgroup, m.group(), m.start()
        sp = start - end if toks else 1
        if kind == 'u':
            stripped = text.rstrip('.,;:!?)]\'"')
            text = stripped or text
        end = start + len(text)
        toks.append(Tok(text, kind, sp, bool(mask) and mask[start]))
        for c in m.group()[len(text):]:          # punctuation cut from a URL's end
            toks.append(Tok(c, 'p', 0, False))
            end += 1
    return toks


PAR = '\n'      # the text of a paragraph-break token


def sentences(toks, original):
    """Split a header paragraph into sentences: after . ? ! before a capital, before a
    one-letter name (d > 1 is an integer) or, in an original, before two spaces."""
    out, cur = [], []
    for k, t in enumerate(toks):
        cur.append(t)
        if t.text in '.?!' and t.kind == 'p' and k + 1 < len(toks):
            nxt = toks[k + 1]
            if nxt.sp and (nxt.text[0].isupper() or nxt.it or (original and nxt.sp > 1)
                           or (nxt.kind == 'w' and len(nxt.text) == 1)):
                out.append(cur)
                cur = []
    if cur:
        out.append(cur)
    return out


# ---------------------------------------------------------------- contracts

CLAUSE = re.compile(r'^(\d+)\.\s+([■◆○])?\s*(.*)$')


class Contract(object):
    def __init__(self, lines, italics, where):
        lines = [l.rstrip() for l in lines if l.strip()]
        if not lines:
            raise ValueError('%s holds no text' % where)
        self.title = tokenize(lines[0], italics)
        self.title_line = lines[0]
        self.header = []          # lines, each a list of tokens
        self.header_lines = []
        self.clauses = []         # [number, mark, tokens]
        for l in lines[1:]:
            m = CLAUSE.match(l)
            if m:
                self.clauses.append([int(m.group(1)), m.group(2), tokenize(m.group(3), italics)])
            elif self.clauses:    # an unnumbered paragraph of the clause above
                self.clauses[-1][2] += [Tok(PAR, 'par', 1)] + tokenize(l, italics)
            else:
                self.header.append(tokenize(l, italics))
                self.header_lines.append(l)


def original_box(path):
    """The lines of a contract's box: not the % lines, and up to the pragmas."""
    lines = []
    with open(path, encoding='utf-8') as f:
        for l in f.read().split('\n'):
            if l.startswith('%'):
                continue
            if l.startswith(END_BOX):
                break
            lines.append(l)
    return lines


def find_originals(glp):
    """NAME -> path, of the files grep lists in the GLP repository."""
    try:
        out = subprocess.run(['grep', '-rl', '--include=*.txt', '^% Owner', 'programs'],
                             cwd=glp, capture_output=True, text=True).stdout.split('\n')
    except OSError:
        out = []
        for d, _, fs in os.walk(os.path.join(glp, 'programs')):
            for f in fs:
                if f.endswith('.txt'):
                    p = os.path.join(d, f)
                    with open(p, encoding='utf-8', errors='replace') as h:
                        if any(l.startswith('% Owner') for l in h):
                            out.append(os.path.relpath(p, glp))
    found = {}
    for p in sorted(x for x in out if x):
        name = os.path.basename(p)[:-4]
        if name in found:
            warn('%s has two originals, %s and %s; the first is used' % (name, found[name], p))
        else:
            found[name] = p
    return found


def relation_names(c, name, names):
    """The contracts named in the relation line, the line after the title that opens
    with the contract's own name."""
    for l in c.header_lines[:1]:
        if l.startswith(name + ' '):
            words = re.findall(r'[A-Za-z][A-Za-z0-9]*(?:-[A-Za-z0-9]+)*', l)
            return [w for w in dict.fromkeys(words) if w in names and w != name]
    return []


def order(contracts):
    """The canonical order (Udi, 2026-11-13): by layer (GKK is layer 0; any other contract
    is one more than the highest layer among the contracts its relation line names); within
    a layer by the list of those contracts, each list sorted in this order and compared
    entry by entry; ties by name.  tools/table.py computes the same order."""
    names = sorted(contracts)
    deps = dict((n, relation_names(contracts[n], n, contracts)) for n in names)
    lay = {}

    def layer(n, seen=()):
        if n not in lay:
            if n in seen:
                warn('%s is in a cycle of relation lines' % n)
                return 0
            ds = [d for d in deps[n] if d in contracts]
            lay[n] = 1 + max(layer(d, seen + (n,)) for d in ds) if ds else 0
        return lay[n]

    for n in names:
        layer(n)
    pos, out = {}, []
    for k in range(max(lay.values()) + 1):
        for n in sorted((m for m in names if lay[m] == k),
                        key=lambda m: (sorted(pos.get(d, -1) for d in deps[m] if d in contracts), m)):
            pos[n] = len(pos)
            out.append(n)
    return out, deps


# ---------------------------------------------------------------- the diff

def weight(toks):
    return sum(len(t.text) for t in toks if t.kind != 'p')


def tidy(blocks):
    """Drop empty blocks and join adjacent changes."""
    out = []
    for b in blocks:
        if not b[1] and not b[2]:
            continue
        if out and out[-1][0] == 'chg' and b[0] == 'chg':
            out[-1][1] += b[1]
            out[-1][2] += b[2]
        else:
            out.append(b)
    return out


def diff(a, b):
    """Blocks ['eq', original tokens, revised tokens] and ['chg', deleted, inserted],
    by words, tidied for reading."""
    sm = difflib.SequenceMatcher(None, [t.text for t in a], [t.text for t in b], autojunk=False)
    blocks = [['eq' if tag == 'equal' else 'chg', a[i1:i2], b[j1:j2]]
              for tag, i1, i2, j1, j2 in sm.get_opcodes()]
    # 1. A word or two common to both, between two changes that dwarf them, is no
    #    anchor: one change reads better than three.  The end of a sentence or of a
    #    title stays an anchor, and so do common words between a deletion and an insertion.
    again = True
    while again:
        again = False
        for k in range(1, len(blocks) - 1):
            e, l, r = blocks[k], blocks[k - 1], blocks[k + 1]
            if not (e[0] == 'eq' and l[0] == 'chg' and r[0] == 'chg'):
                continue
            puncts = [t for t in e[2] if t.kind == 'p']
            if len(e[2]) - len(puncts) > 2 or any(t.text in '.:;?!' or t.kind == 'par' for t in e[2]):
                continue
            if any(x and x[-1].text in '.:;?!' for x in (l[1], l[2])):
                continue
            replaces = sum(1 for x in (l, r) if x[1] and x[2])
            if not replaces:
                continue
            factor = 2 + (1 if puncts else 0) + (1 if replaces == 1 else 0)
            we = weight(e[2]) + len(puncts)
            wl = max(weight(l[1]), weight(l[2]))
            wr = max(weight(r[1]), weight(r[2]))
            if factor * we <= min(wl, wr):
                blocks[k - 1:k + 2] = [['chg', l[1] + e[1] + r[1], l[2] + e[2] + r[2]]]
                again = True
                break
    # 2. A side of a change that is punctuation alone takes the word before it, since
    #    a struck comma cannot be seen.
    for k in range(1, len(blocks)):
        b_ = blocks[k]
        if b_[0] != 'chg' or blocks[k - 1][0] != 'eq':
            continue
        sides = [x for x in (b_[1], b_[2]) if x]
        if (any(all(t.kind == 'p' for t in x) for x in sides) and not any(x[0].sp for x in sides)
                and blocks[k - 1][2][-1].kind != 'p'):
            b_[1].insert(0, blocks[k - 1][1].pop())
            b_[2].insert(0, blocks[k - 1][2].pop())
    return tidy(blocks)


# ---------------------------------------------------------------- LaTeX

def plain_tok(t):
    if t.kind == 'u':
        return url_tex(t.text)
    if t.kind == 'par':
        return r'\par'
    return ''.join(s if k == 't' else ('$%s$' % s.strip() if k == 'm' else s + '{}')
                   for k, s in segments(t.text))


def emit_plain(toks):
    out, i = [], 0
    while i < len(toks):
        t = toks[i]
        sp = ' ' if t.sp else ''
        if t.it:
            j = i
            while j < len(toks) and toks[j].it:
                j += 1
            inner = ''.join((' ' if x.sp and n else '') + plain_tok(x) for n, x in enumerate(toks[i:j]))
            out.append(sp + r'\emph{' + inner + '}')
            i = j
        else:
            out.append(sp + plain_tok(t))
            i += 1
    return ''.join(out)


def emit_marked(toks, cmd, boxcmd):
    """A run of deleted or inserted tokens: words in one \\sout or \\uline, the run
    broken around mathematics, marks, URLs and paragraph breaks."""
    out, cur = [], []

    def flush():
        if cur:
            out.append('\\%s{%s}' % (cmd, ''.join(cur)))
            del cur[:]

    fresh = True                      # at the start of the run or of a paragraph
    for t in toks:
        lead = ' ' if (t.sp and not fresh) else ''
        fresh = t.kind == 'par'
        if t.kind == 'par':
            flush()
            if cmd == 'uline':
                out.append(r'\par ')
            continue
        if t.kind == 'u':
            segs = [['b', url_tex(t.text)]]
        else:
            segs = segments(t.text)
        for k, s in segs:
            if k == 't':
                if t.kind in 'wn':
                    s = re.sub(r'(?<=[^\W\d_])-(?=[^\W\d_])', r'-\\hskip0pt ', s) if not t.it else s
                cur.append(lead + (r'\emph{%s}' % s if t.it else s))
            else:
                if lead and cur:
                    cur.append(' ')
                    lead = ''
                flush()
                body = '$%s$' % s.strip() if k == 'm' else s
                # a space between two boxes is marked inside the second, after a break point
                out.append('%s\\%s{%s%s}' % (r'\allowbreak ' if lead else '', boxcmd, lead,
                                             r'\emph{%s}' % body if t.it and k == 'm' else body))
            lead = ''
    flush()
    return ''.join(out)


def emit(blocks):
    s = ''
    for b in blocks:
        if b[0] == 'eq':
            s += emit_plain(b[2])
        else:
            if b[1]:
                s += (' ' if b[1][0].sp else '') + emit_marked(b[1], 'sout', 'revdelbox')
            if b[2]:
                s += (' ' if b[2][0].sp else '') + emit_marked(b[2], 'uline', 'revinsbox')
    return re.sub(r'  +', ' ', s).strip()


def deleted(toks):
    return emit([['chg', toks, []]])


def inserted(toks):
    return emit([['chg', [], toks]])


def keys(toks):
    return tuple(t.text for t in toks)


def header_tex(rev, orig):
    """The header paragraphs of the revision, sentence by sentence against the
    sentences of the original's header: one found there, anywhere, is plain."""
    osent = [s for line in orig for s in sentences(line, True)]
    paras = [sentences(line, False) for line in rev]
    rsent = [(p, k) for p, ss in enumerate(paras) for k in range(len(ss))]
    match = {}                       # (paragraph, sentence) -> original sentence
    free = list(range(len(osent)))
    for p, k in rsent:               # the same sentence
        for j in free:
            if keys(osent[j]) == keys(paras[p][k]):
                match[(p, k)] = j
                free.remove(j)
                break
    cand = []
    for p, k in rsent:               # the most similar sentence
        if (p, k) not in match:
            for j in free:
                r = difflib.SequenceMatcher(None, keys(paras[p][k]), keys(osent[j]), autojunk=False).ratio()
                if r >= 0.5:
                    cand.append((r, -j, p, k))
    for r, j, p, k in sorted(cand, reverse=True):
        if (p, k) not in match and -j in free:
            match[(p, k)] = -j
            free.remove(-j)
    # a deleted sentence follows the revised sentence of the original sentence before it
    back = dict((j, pk) for pk, j in match.items())
    after, first = {}, []
    for j in free:
        prev = [i for i in back if i < j]
        if prev:
            after.setdefault(back[max(prev)], []).append(j)
        else:
            first.append(j)
    out, stats = [], [0, 0, 0]       # changed, inserted, deleted sentences
    if first:
        out.append(emit(join([['chg', list(osent[j]), []] for j in first])) + r'\par')
        stats[2] += len(first)
    for p, ss in enumerate(paras):
        blocks = []
        for k, s in enumerate(ss):
            if (p, k) not in match:
                blocks.append(['chg', [], list(s)])
                stats[1] += 1
            elif keys(osent[match[(p, k)]]) == keys(s):
                blocks.append(['eq', list(s), list(s)])
            else:
                blocks += diff(osent[match[(p, k)]], s)
                stats[0] += 1
            for j in after.get((p, k), []):
                blocks.append(['chg', list(osent[j]), []])
                stats[2] += 1
        out.append(emit(join(blocks)) + r'\par')
    return out, stats


def join(blocks):
    """Adjacent sentences wholly deleted, or wholly inserted, are one run."""
    out = []
    for b in blocks:
        if (out and b[0] == 'chg' and out[-1][0] == 'chg'
                and bool(b[1]) == bool(out[-1][1]) and bool(b[2]) == bool(out[-1][2])
                and not (b[1] and b[2])):
            out[-1][1] += b[1]
            out[-1][2] += b[2]
        else:
            out.append(b)
    return out


def align(rev, orig):
    """Revised clause -> original clause: by number where the two still correspond,
    otherwise by the best similarity."""
    def words(toks):
        return [t.text.lower() for t in toks if t.kind not in ('p', 'par')]
    cand = []
    for i, (rn, _, rt) in enumerate(rev):
        for j, (on, _, ot) in enumerate(orig):
            r = difflib.SequenceMatcher(None, words(ot), words(rt), autojunk=False).ratio()
            same = rn == on
            if r >= (0.2 if same else 0.4):
                cand.append((r + (0.15 if same else 0), -abs(i - j), -i, -j))
    pairs, used = {}, set()
    for _, _, i, j in sorted(cand, reverse=True):
        if -i not in pairs and -j not in used:
            pairs[-i] = -j
            used.add(-j)
    whole = set()
    for i, c in enumerate(rev):       # rewritten whole, under the number it had
        for j, o in enumerate(orig):
            if i not in pairs and j not in used and c[0] == o[0]:
                pairs[i] = j
                used.add(j)
                whole.add(i)
    return pairs, whole


def title_apart(blocks):
    """A clause that opens with a change of its title and of the words after it shows
    two changes, the title's and the words', not one."""
    b = blocks[0] if blocks else None
    if b and b[0] == 'chg' and b[1] and b[2]:
        cut = []
        for side in (b[1], b[2]):
            k = next((n for n, t in enumerate(side[:9]) if t.text in ('.', ':')), None)
            if k is None:
                return blocks
            cut.append(k + 1)
        if cut[0] < len(b[1]) or cut[1] < len(b[2]):
            blocks[0:1] = [['chg', b[1][:cut[0]], b[2][:cut[1]]], ['chg', b[1][cut[0]:], b[2][cut[1]:]]]
    return blocks


def label(old, new):
    if old is None:
        return r'\uline{%d.}' % new
    if new is None:
        return r'\sout{%d.}' % old
    return r'\sout{%d.}\,\uline{%d.}' % (old, new)


def mark_tex(old, new):
    if old == new:
        return MARKS[new] + '~' if new else r'\relax '
    s = ''
    if old:
        s += r'\revdelbox{%s}' % MARKS[old]
    if old and new:
        s += r'\,'
    if new:
        s += r'\revinsbox{%s}' % MARKS[new]
    return s + '~'


def clauses_tex(rev, orig):
    pairs, whole = align(rev, orig)
    back = dict((j, i) for i, j in pairs.items())
    after = {}
    for j in range(len(orig)):
        if j not in back:
            prev = [x for x in back if x < j]
            after.setdefault(back[max(prev)] if prev else -1, []).append(j)
    items, counter, widest = [], 0, 0

    def gone(i):
        for j in after.get(i, []):
            n, mark, toks = orig[j]
            items.append(r'\item[%s] %s%s' % (label(n, None), mark_tex(mark, None), deleted(toks)))

    gone(-1)
    for i, (n, mark, toks) in enumerate(rev):
        if i not in pairs:
            items.append(r'\item[%s]\setcounter{enumi}{%d} %s%s'
                         % (label(None, n), n, mark_tex(None, mark), inserted(toks)))
        else:
            on, omark, otoks = orig[pairs[i]]
            if on != n:
                head = r'\item[%s]\setcounter{enumi}{%d} ' % (label(on, n), n)
                widest = max(widest, len(str(on)) + len(str(n)))
            elif counter + 1 != n:
                head = r'\setcounter{enumi}{%d}\item ' % (n - 1)
            else:
                head = r'\item '
            blocks = [['chg', list(otoks), list(toks)]] if i in whole else diff(otoks, toks)
            items.append(head + mark_tex(omark, mark) + emit(title_apart(blocks)))
        counter = n
        gone(i)
    if not items:
        return [], (0, 0, 0)
    opt = r'label=\arabic*.'
    if widest:                       # room for a struck number beside the new one
        opt += ',leftmargin=%.2fem' % (0.5 * widest + 1.25)
    stats = (len(pairs), len(orig) - len(pairs), len(rev) - len(pairs))
    return [r'\begin{enumerate}[%s]' % opt] + items + [r'\end{enumerate}'], stats


def box_tex(name, rev, orig):
    if orig is None:                 # no original: the whole text is inserted
        title = emit_plain(rev.title).strip()
        out = [inserted(l) + r'\par' for l in rev.header]
        body, cs = clauses_tex(rev.clauses, [])
        hs = [0, sum(len(sentences(l, False)) for l in rev.header), 0]
    else:
        title = emit(diff(orig.title, rev.title))
        out, hs = header_tex(rev.header, orig.header)
        body, cs = clauses_tex(rev.clauses, orig.clauses)
    lines = [r'\begin{contractbox}{%s}' % title] + out + body
    if not body and lines[-1].endswith(r'\par'):
        lines[-1] = lines[-1][:-4]
    return lines + [r'\end{contractbox}'], hs, cs


# ---------------------------------------------------------------- main

QLINE = re.compile(r'^K\d+\s*(?:\([^)]*\))?\.\s*')


def questions(rev_dir, names):
    """The decisions of rev/NOTES-*.txt shown to Udi, by contract: a section opens with a
    line whose first word is a contract's name, and such a decision is a line that opens with K<n>."""
    out, cur = {}, None
    for f in sorted(os.listdir(rev_dir)):
        if not (f.startswith('NOTES-') and f.endswith('.txt')):
            continue
        cur = None
        for line in open(os.path.join(rev_dir, f), encoding='utf-8'):
            line = line.strip()
            if not line:
                continue
            m = QLINE.match(line)
            if m:
                if cur:
                    out.setdefault(cur, []).append(line[m.end():])
                continue
            if re.match(r'^[ADOQX]\d+', line):
                continue
            first = line.split()[0].rstrip(':,')
            cur = None if line.startswith(('NOTES', 'For each', 'In all', 'Counts')) else (first if first in names else '*')
    return out


def main():
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    glps = [os.environ.get('GLP_DIR'), os.path.join(os.path.dirname(root), 'GLP'),
            '/home/user/GLP', '/Users/udi/Grassroots/GLP']
    glp = next((g for g in glps if g and os.path.isdir(os.path.join(g, 'programs'))), glps[1])
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--rev', default=os.path.join(root, 'rev'), help='the revised texts (default rev/)')
    ap.add_argument('--glp', default=glp, help='the GLP repository (default %s)' % glp)
    ap.add_argument('--out', default=os.path.join(root, 'sections', 'contracts-revised.tex'))
    args = ap.parse_args()

    originals = find_originals(args.glp)
    if not originals:
        sys.exit('revtex: no original under %s/programs' % args.glp)
    revised = {}
    for f in sorted(os.listdir(args.rev)):
        if f.endswith('.txt') and not f.startswith('NOTES-'):
            revised[f[:-4]] = Contract(original_box(os.path.join(args.rev, f)), True, f)
    if not revised:
        sys.exit('revtex: no contract in %s' % args.rev)
    names, deps = order(revised)
    asked, qn = questions(args.rev, set(names)), 0

    out = ['% Written by tools/revtex.py from rev/ and the originals in GLP/programs; do not edit.',
           r'% main.tex needs \usepackage[normalem]{ulem} in its preamble, and \input{sections/contracts-revised}.',
           r'\appendix', r'\section{The Contracts Revised}\label{app:contracts-revised}', '',
           LEAD_IN + ('  ' + LEAD_IN_Q if asked else '') + '  ' + DROPPED, '', MACROS]
    print('%-10s %-36s %-22s %s' % ('contract', 'original', 'after', 'clauses and header sentences'))
    for name in names:
        rev = revised[name]
        orig = None
        oname = RENAMED.get(name, name)
        if oname in originals:
            orig = Contract(original_box(os.path.join(args.glp, originals[oname])), False, originals[oname])
        else:
            warn('%s has no original; its whole text is marked as inserted' % name)
        if not rev.title_line.startswith(name + ':'):
            warn("%s: the title line does not open with '%s:'" % (name, name))
        lines, hs, cs = box_tex(name, rev, orig)
        out += [''] + lines
        if asked.get(name):
            out += ['', r'\begin{description}[leftmargin=2.6em,labelwidth=2.2em,labelsep=0.4em,nosep,font=\normalfont\bfseries]']
            for q in asked[name]:
                qn += 1
                out.append(r'\item[D%d.] ' % qn + emit_plain(tokenize(q, True)))
            out.append(r'\end{description}')
        print('%-10s %-36s %-22s clauses: %d kept, %d deleted, %d new; header: %d changed, %d new, %d deleted'
              % (name, originals.get(RENAMED.get(name, name), '(none)'), ' '.join(deps[name]) or '-', cs[0], cs[1], cs[2], hs[0], hs[1], hs[2]))
    if asked.get('*'):
        out += ['', r'\paragraph{Several contracts}', r'\begin{description}[leftmargin=2.6em,labelwidth=2.2em,labelsep=0.4em,nosep,font=\normalfont\bfseries]']
        for q in asked['*']:
            qn += 1
            out.append(r'\item[D%d.] ' % qn + emit_plain(tokenize(q, True)))
        out.append(r'\end{description}')
    with open(args.out, 'w', encoding='utf-8') as h:
        h.write('\n'.join(out) + '\n')
    for w in WARNINGS:
        print('warning: ' + w, file=sys.stderr)
    print('%s: %d contracts, %d warnings' % (args.out, len(names), len(WARNINGS)))


if __name__ == '__main__':
    main()
