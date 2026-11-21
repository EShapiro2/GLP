#!/usr/bin/env python3
"""check.py --- test a text against organisation/claudese.md and organisation/writing-style-guide.md
before Udi sees it (Udi, 2026-11-20: "yes" to a check of every text before he sees it).

    python3 -I tools/myenglish/check.py --udi < message.txt      a message to Udi
    python3 -I tools/myenglish/check.py --paper FILE ...         a paper's text or a document
    python3 -I tools/myenglish/check.py --clause < clause.txt    a contract's clauses

It reads the bans from the two files each time it runs, so it follows them as Coordination extends
them.  Each failure is printed with its line, what was found and, where the file gives one, the My
English to write instead.  It exits 1 on a failure and 0 on none.  A line starting "check:" names
something a program cannot decide, such as a ban that holds only in one sense of a word; it does not
fail the text, and the writer reads it and decides.

What it reads from the files:
  1. claudese.md: each entry is one line.  An entry whose banned text is a bare word or short phrase
     is a ban wherever it occurs; "X" in it stands for any word, and "(except in Y)" exempts Y.  An
     entry whose banned text carries a condition, "(of ...)", "(as in ...)", "--- with ...", is
     reported as a check.  Every quoted text of three words or more before "My English:" is a wrong
     text, failed wherever it occurs.
  2. writing-style-guide.md, section 41, "Wrong and Right": each block of lines "Wrong:", "Right:", "Why:"
     and "Source:" is a wrong text with its right text.  A text fails that holds the wrong text, or a
     passage of about its length whose words match it at 75% or more (difflib), the right text and the
     reason printed beside it.  This is the form in which Coordination adds a failure Udi points out.

What it tests besides (writing-style-guide.md, the section named):
  section 17's quoted hedges and insistences, from the file; hedges and apologies (8, 17); "users", hyperbole, "the author", "final" (1, 8, Talking to Udi);
  long sentences (2); in --paper and --clause, "I", "me", "my" (1, 5); in --udi, more than one
  sentence on a line, a dash or an arrow chain, more than one "Q:" (Talking to Udi; the project's
  instructions).
"""
import difflib
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
CLAUDESE = os.path.join(ROOT, 'organisation', 'claudese.md')
GUIDE = os.path.join(ROOT, 'organisation', 'writing-style-guide.md')

QUOTE = re.compile(r'"([^"]+)"|“([^”]+)”')
CONDITION = re.compile(r'^\s*\((of|as in|in the sense|for|with|a metaphor|use|write|meaning|and similar|a word|Udi)', re.I)

HEDGES = ['perhaps', 'maybe', 'possibly', 'arguably', 'presumably', 'it seems', 'seems to', 'appears to',
          'somewhat', 'fairly', 'rather', 'quite', 'probably', 'likely', 'i think', 'i believe', 'i guess',
          'sort of', 'kind of', 'basically', 'essentially', 'in a sense', 'to some extent',
          'more or less', 'if i understand', 'unfortunately', 'sorry', 'apologise', 'apologize', 'apologies',
          'even though', 'although still', 'while not']
WORDS = {
    'users': 'people, parties, agents or members (style guide, 1)',
    'user': 'a person, a party, an agent or a member (style guide, 1)',
    'revolutionary': 'state what it does (style guide, 1)',
    'disruption': 'state what it does (style guide, 1)',
    'game-changer': 'state what it does (style guide, 1)',
    'paradigm shift': 'state what it does (style guide, 1)',
    'web3': 'name the system (style guide, 1)',
    'trustless': 'name the system (style guide, 1)',
    'the author': 'the authors (style guide, 8)',
    'final': 'draft, current version or revised (Talking to Udi)',
    'arial': 'Calibri (style guide, 8)',
}
FIRST_PERSON = ['i', 'me', 'my', 'mine', 'myself']


def norm(s):
    return re.sub(r'\s+', ' ', s.strip().lower())


def forms(w):
    """A word in its inflections, from whichever form the ban lists: differ, differs, differed, differing."""
    bases = {w}
    if w.endswith('ing') and len(w) > 5:
        bases |= {w[:-3], w[:-3] + 'e'}
    if w.endswith('ed') and len(w) > 4:
        bases |= {w[:-2], w[:-1]}
    if w.endswith('ies') and len(w) > 4:
        bases.add(w[:-3] + 'y')
    elif w.endswith('es') and len(w) > 4:
        bases |= {w[:-2], w[:-1]}
    elif w.endswith('s') and not w.endswith('ss') and len(w) > 4:
        bases.add(w[:-1])
    out = set()
    for b in bases:
        out |= {b, b + 's', b + 'es', b + 'ed', b + 'd', b + 'ing'}
        if b.endswith('e'):
            out |= {b[:-1] + 'ing', b[:-1] + 'ed'}
        if b.endswith('y'):
            out |= {b[:-1] + 'ies', b[:-1] + 'ied'}
    return '(?:' + '|'.join(re.escape(f) for f in sorted(out, key=len, reverse=True)) + ')'


def phrase_regex(p, every_word=False):
    """A banned phrase as a regex: words at word boundaries, X and Y for any word.  The last word is
    matched in its inflections; with every_word, every word of four letters or more is (a ban of
    claudese.md: "differs in kind" catches "differ in kind")."""
    words = norm(p).split(' ')
    parts = []
    for k, w in enumerate(words):
        if w in ('x', 'y'):
            parts.append(r"[\w'-]+")
        elif every_word and re.fullmatch(r'[a-z]{4,}', w):
            parts.append(forms(w))
        elif k == len(words) - 1 and re.fullmatch(r'[a-z]{4,}', w):
            # the last word in its inflections: leverage, leverages, leveraged, leveraging
            stem = w[:-1] if w.endswith('e') else w
            parts.append(re.escape(stem) + r'(?:' + ('e|es|ed|ing' if w.endswith('e') else 's|es|ed|ing|d|') + r')')
        else:
            parts.append(re.escape(w))
    return re.compile(r'(?<![\w-])' + r'\s+'.join(parts) + r'(?![\w-])', re.I)


def alt_of(line):
    m = re.search(r'My English:\s*(.*)', line)
    if not m:
        m2 = re.search(r'\((use|write) ([^)]*)\)', line)
        return m2.group(0).strip('()') if m2 else ''
    a = m.group(1)
    a = re.split(r' \(Udi, | \(reported', a)[0]
    return a[:240]


def bare(head):
    return re.fullmatch(r"[A-Za-z][A-Za-z' -]{0,40}", head) is not None and len(head.split()) <= 5


def read_claudese(path):
    fails, checks = [], []
    for raw in open(path, encoding='utf-8').read().split('\n'):
        line = raw.strip()
        if not line or line.startswith('#') or line.startswith('**'):
            continue
        alt = alt_of(line)
        before = line.split('My English:')[0]
        for m in QUOTE.finditer(before):
            q = (m.group(1) or m.group(2)).strip()
            if len(q.split()) >= 3:
                fails.append(('wrong text', q, phrase_regex_literal(q), None, alt))
        if line.startswith('🔴') or re.match(r'\d+\.', line):
            continue
        head = re.split(r'\s+---\s+My English:', line)[0]
        head = re.split(r'\s+---\s+', head)[0]
        rest = ''
        m = re.match(r'([^(—]*)(.*)', head)
        name, rest = m.group(1).strip(), m.group(2).strip()
        if '—' in head or ' -- ' in head:
            name = re.split(r'\s*—\s*', head)[0].strip()
            rest = 'condition'
        exempt = None
        em = re.match(r'\(except in ([^)]*)\)', rest)
        if em:
            exempt = em.group(1).strip()
            rest = ''
        names = [n.strip() for n in re.split(r',\s*| / ', name) if n.strip()]
        if not names or not all(bare(n) for n in names):
            continue
        for n in names:
            if len(n) < 3 and n.lower() not in ('i',):
                continue
            entry = ('Claudese', n, phrase_regex(n, every_word=True), exempt, alt or 'no alternative is listed: say plainly what is meant')
            if rest and (CONDITION.match(rest) or rest == 'condition' or rest.startswith('(')):
                checks.append(entry + (rest[:120],))
            else:
                fails.append(entry)
    return fails, checks


def phrase_regex_literal(q):
    words = [re.escape(w) for w in norm(q).split(' ')]
    return re.compile(r'\s+'.join(words), re.I)


def read_guide(path):
    """Section 41's blocks: lines beginning Wrong:, Right:, Why:, Source:, a blank line between blocks."""
    entries = []
    block = {}
    for l in open(path, encoding='utf-8').read().split('\n') + ['']:
        m = re.match(r'\s*[-*]?\s*(Wrong|Right|Why|Source):\s*(.*)', l)
        if m:
            block[m.group(1)] = m.group(2).strip().strip('"“”')
        elif not l.strip():
            if block.get('Wrong'):
                entries.append(block)
            block = {}
    return entries


def read_insistence(path):
    """Section 17's bans: the quoted texts of the first sentence of its "No hedge:" and "No insistence:"
    lines, each failed wherever it occurs."""
    text = open(path, encoding='utf-8').read()
    m = re.search(r'^## 17\..*?(?=^## )', text, re.M | re.S)
    out = []
    if not m:
        return out
    for l in m.group(0).split('\n'):
        h = re.match(r'\s*-\s*No (hedge|insistence):\s*(.*)', l)
        if not h:
            continue
        first = re.split(r'(?<=[.!?"”])\s{2,}|(?<=[.!?])\s+(?=[A-Z])', h.group(2))[0]
        for q in QUOTE.finditer(first):
            t = (q.group(1) or q.group(2)).strip().rstrip('.,')
            if t:
                out.append(('style guide 17, ' + h.group(1), t, phrase_regex_literal_words(t), None,
                            'state it directly (style guide, 17)'))
    return out


def phrase_regex_literal_words(q):
    words = [re.escape(w) for w in norm(q).split(' ')]
    return re.compile(r'(?<![\w-])' + r'\s+'.join(words) + r'(?![\w-])', re.I)


def words_of(text):
    return re.findall(r"[\w'-]+", text.lower())


def close_matches(text, entries, threshold=0.75):
    """Each Wrong text that the text holds, or holds a passage close to: a window of the text's words whose
    word sequence matches the Wrong text's at the ratio threshold or more (difflib)."""
    out = []
    lines = text.split('\n')
    toks = []
    for no, line in enumerate(lines, 1):
        for w in words_of(strip_code(line)):
            toks.append((w, no))
    seq = [w for w, _ in toks]
    for e in entries:
        wrong = words_of(e['Wrong'])
        n = len(wrong)
        if n == 0:
            continue
        best = (0.0, None)
        for size in range(max(1, n - 2), n + 3):
            for i in range(0, max(0, len(seq) - size) + 1):
                win = seq[i:i + size]
                if not win:
                    continue
                r = difflib.SequenceMatcher(None, wrong, win, autojunk=False).ratio()
                if r > best[0]:
                    best = (r, i, size)
        if best[0] >= threshold:
            r, i, size = best
            no = toks[i][1]
            found = ' '.join(seq[i:i + size])
            kind = 'wrong text' if r >= 0.999 else 'close to a wrong text (%d%%)' % round(100 * r)
            alt = e.get('Right', '') + ((' (' + e['Why'] + ')') if e.get('Why') else '')
            out.append((no, kind, found, alt))
    return out


def sentences(text):
    t = re.sub(r'\b(e\.g|i\.e|cf|etc|vs|Fig|Sec|Def|No)\.', lambda m: m.group(0).replace('.', '§'), text)
    t = re.sub(r'(\d)\.(\d)', r'\1§\2', t)
    parts = re.split(r'(?<=[.?!])\s+(?=[A-Z"“(0-9])', t.strip())
    return [p for p in parts if re.search(r'[A-Za-z]', p)]


def strip_code(line):
    return re.sub(r'`[^`]*`', ' ', line)


def check(text, mode, fails, checks):
    out_fail, out_check = [], []
    lines = text.split('\n')
    qcount = 0
    for no, raw in enumerate(lines, 1):
        line = strip_code(raw)
        low = line.lower()
        for kind, name, rx, exempt, alt in fails:
            for m in rx.finditer(line):
                if exempt and exempt.lower() in low:
                    continue
                out_fail.append((no, kind, m.group(0), alt))
        for kind, name, rx, exempt, alt, cond in checks:
            for m in rx.finditer(line):
                out_check.append((no, kind, m.group(0), cond + ('; ' + alt if alt else '')))
        for h in HEDGES:
            for m in phrase_regex(h).finditer(line):
                out_fail.append((no, 'hedge or apology', m.group(0), 'state what is (style guide, 8 and 17)'))
        for w, alt in WORDS.items():
            for m in phrase_regex(w).finditer(line):
                out_fail.append((no, 'banned word', m.group(0), alt))
        if mode in ('paper', 'clause'):
            for w in FIRST_PERSON:
                for m in re.finditer(r'(?<![\w-])' + w + r'(?![\w-])', line, re.I if w != 'i' else 0):
                    if w == 'i' and m.group(0) != 'I':
                        continue
                    out_fail.append((no, 'first person singular', m.group(0), 'we (style guide, 1 and 5)'))
        limit = 25 if mode == 'udi' else 40
        for s in sentences(line):
            n = len(s.split())
            if n > limit:
                out_fail.append((no, 'long sentence', '%d words: %s ...' % (n, ' '.join(s.split()[:8])),
                                 'split it; at most %d words (style guide, 2)' % limit))
        if mode == 'udi':
            body = re.sub(r'^\s*(\d+[.)]|[-*])\s+', '', line)
            ss = [s for s in sentences(body) if not s.strip().startswith('Q:')]
            if len(ss) > 1:
                out_fail.append((no, 'dense line', '%d sentences on one line' % len(ss),
                                 'one fact a line (Talking to Udi)'))
            if '—' in line or re.search(r'\s--\s', line) or '–' in line:
                out_fail.append((no, 'dash', '—', 'write the sentence with its verb, or two sentences'))
            if re.search(r'(->|→|=>)', line):
                out_fail.append((no, 'arrow chain', re.search(r'(->|→|=>)', line).group(0), 'write the steps as sentences'))
            qcount += len(re.findall(r'(?<![\w])Q:', line))
            if '?' in line and 'Q:' not in line:
                out_check.append((no, 'question', line.strip()[:80], 'a question for Udi is one line, ending "Q: Do you approve?"'))
    if mode == 'udi' and qcount > 1:
        out_fail.append((0, 'questions', '%d questions' % qcount, 'one question at a time (project instructions)'))
    return out_fail, out_check


def main(argv):
    mode = 'paper'
    files = []
    for a in argv[1:]:
        if a in ('--udi', '--paper', '--clause'):
            mode = a[2:]
        elif a in ('-h', '--help'):
            print(__doc__)
            return 0
        else:
            files.append(a)
    fails, checks = read_claudese(CLAUDESE)
    fails += read_insistence(GUIDE)
    wrongs = read_guide(GUIDE)
    texts = [(f, open(f, encoding='utf-8').read()) for f in files] or [('-', sys.stdin.read())]
    failed = 0
    for name, text in texts:
        of, oc = check(text, mode, fails, checks)
        of = list(of) + close_matches(text, wrongs)
        pre = '' if name == '-' else name + ':'
        for no, kind, found, alt in sorted(set(of)):
            print('%sline %d: %s: "%s"%s' % (pre, no, kind, found, (' --- My English: ' + alt) if alt else ''))
        for no, kind, found, note in sorted(set(oc)):
            print('%scheck: line %d: %s: "%s" --- %s' % (pre, no, kind, found, note))
        failed += len(set(of))
    print('%d failure%s' % (failed, '' if failed == 1 else 's'), file=sys.stderr)
    return 1 if failed else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
