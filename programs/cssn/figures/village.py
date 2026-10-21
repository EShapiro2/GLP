#!/usr/bin/env python3
# The village figure of CSSN, Section 4.6, "Village Demonstration" (figures/cssn-village.png): one panel per agent, the narrative of the agent's screen in the run of play_village (programs/cssn/plays.glp).  The narrative is read off the screen the scripted person reports (person.glp): each request the person makes, did(R); each approval it gives, gave(K, yes); and, from each view the agent writes, view(S, K), what changed in the machine state S since the last view: a person known (GKK:2 and GKK:3, knows(q), but where the same view adds them as a friend, an entering adding both), a friend, a child or a parent gained or lost, an identity established, a group joined, left or closed, a member listed or no longer, and a message received, from a friend, received(x, p) (GSN:1), or in a group, received_in(x, a, n), delivered by its leader a (GSN:5).  The requests and approvals of GKK's meet and introduce and of CSSN's child_meet and child_meet_2 (CSSN:2) are drawn as those of befriending are.  Nothing is added that the screens do not show.
#
#   python3 programs/cssn/figures/village.py <log> [<png>]
#
# <log> is the REPL's output of play_village(A, B, G, F, C, D, E), or cascade/CSSN/run.log, whose first seven lists are the screens of alice, bob, grace, frank, carol, dave and eve, in that order; <png> defaults to programs/cssn/figures/cssn-village.png.  The font is Calibri where it is installed (the style guide: never Arial), with Helvetica in its place where it is not.

import os
import re
import sys

from PIL import Image, ImageDraw, ImageFont

AGENTS = ["alice", "bob", "grace", "frank", "carol", "dave", "eve"]
TITLES = {
    "alice": "Alice (Parent)",
    "bob": "Bob (Parent)",
    "grace": "Grace (Parent)",
    "frank": "Frank (Teacher)",
    "carol": "Carol (Alice's child)",
    "dave": "Dave (Bob and Grace's child)",
    "eve": "Eve (Bob and Grace's child)",
}
ACTS = "1 Identities · 2 Adult friendships · 3 Child friendships · 4 Child-adult friendships · 5 kids_chat · 6 Study group · 7 Parents ending · 8 Coming of age"


# --- GLP terms, as the REPL prints them ------------------------------------

class Tok:
    def __init__(self, s):
        self.s = s
        self.i = 0

    def ws(self):
        while self.i < len(self.s) and self.s[self.i] in " \t\n":
            self.i += 1

    def peek(self):
        self.ws()
        return self.s[self.i] if self.i < len(self.s) else ""


def parse(t):
    c = t.peek()
    if c == "[":
        t.i += 1
        items = []
        if t.peek() == "]":
            t.i += 1
            return ("list", items)
        while True:
            items.append(parse(t))
            c = t.peek()
            if c == ",":
                t.i += 1
                continue
            if c == "|":
                t.i += 1
                parse(t)
                if t.peek() != "]":
                    raise ValueError("list tail at %d" % t.i)
                t.i += 1
                return ("list", items)
            if c == "]":
                t.i += 1
                return ("list", items)
            raise ValueError("list at %d: %r" % (t.i, t.s[t.i:t.i + 20]))
    if c == "'":
        j = t.i + 1
        out = []
        while True:
            if t.s[j] == "\\":
                out.append(t.s[j + 1])
                j += 2
                continue
            if t.s[j] == "'":
                if j + 1 < len(t.s) and t.s[j + 1] == "'":
                    out.append("'")
                    j += 2
                    continue
                break
            out.append(t.s[j])
            j += 1
        t.i = j + 1
        name = "".join(out)
    else:
        m = re.compile(r"[A-Za-z0-9_]+").match(t.s, t.i)
        if not m:
            raise ValueError("term at %d: %r" % (t.i, t.s[t.i:t.i + 20]))
        name = m.group(0)
        t.i = m.end()
    if t.i < len(t.s) and t.s[t.i] == "(":
        t.i += 1
        args = []
        while True:
            args.append(parse(t))
            c = t.peek()
            t.i += 1
            if c == ",":
                continue
            if c == ")":
                return (name, args)
            raise ValueError("args at %d" % t.i)
    return (name, [])


def screens(path):
    found = []
    for line in open(path, encoding="utf-8"):
        line = re.sub(r"^(GLP> )+", "", line.rstrip("\n"))
        m = re.match(r"^[A-Z][A-Za-z0-9_]* = (\[.*)$", line)
        if m:
            found.append(parse(Tok(m.group(1)))[1])
    if len(found) < 7:
        sys.exit("village.py: %s holds %d screens, not seven" % (path, len(found)))
    return dict(zip(AGENTS, found[:7]))


# --- The narrative -----------------------------------------------------------

def name(t):
    return t[0].capitalize()


def text(x):
    # speech(utterance(T), P): the utterance's text
    while x[0] == "speech":
        x = x[1][0]
    if x[0] == "utterance":
        return x[1][0][0]
    return x[0]


def author(x):
    # speech(utterance(T), P): its author P
    return name(x[1][1])


def group(n):
    return text(n)


def leader(c):
    # member(dot(community(L), N))
    return c[1][0][1][0]


def says(t):
    return "“%s”" % t


def request(me, r):
    if r[0] != "propose":
        return None
    p = r[1][0]
    f, a = p
    if f == "establish":
        return "Establishing my identity"
    if f == "enter":
        return "Entering %s as my child" % name(a[1])
    if f == "enter_2":
        return "Entering %s as our child, with %s" % (name(a[2]), name(a[1]))
    if f == "meet":
        return "Asking to meet %s" % name(a[1])
    if f == "introduce":
        # introduce(p, q, c): p introduces c to q (GKK:3)
        return "Introducing %s to %s" % (name(a[2]), name(a[1]))
    if f == "befriend":
        return "Offering %s friendship" % name(a[1])
    if f == "message":
        return "Sending %s to %s" % (says(text(a[2])), name(a[1]))
    if f == "child_befriend":
        if a[2] == a[3]:
            return "Proposing that %s and %s befriend (siblings)" % (name(a[0]), name(a[1]))
        return "Proposing that %s and %s befriend, with %s" % (name(a[0]), name(a[1]), name(a[3]))
    if f == "child_adult_befriend":
        return "Proposing that %s and %s befriend" % (name(a[0]), name(a[1]))
    if f == "child_meet_2":
        if a[2] == a[3]:
            return "Proposing that %s and %s meet (siblings)" % (name(a[0]), name(a[1]))
        return "Proposing that %s and %s meet, with %s" % (name(a[0]), name(a[1]), name(a[3]))
    if f == "child_meet":
        return "Proposing that %s and %s meet" % (name(a[0]), name(a[1]))
    if f == "create":
        return "Creating the group %s" % group(a[1])
    if f == "child_join_2":
        return "Inviting %s to %s" % (name(a[1]), group(a[4]))
    if f == "child_join":
        return "Inviting %s to %s, with %s" % (name(a[1]), group(a[3]), name(a[2]))
    if f == "join":
        return "Adding %s to %s" % (name(a[1]), group(a[2]))
    if f == "post_own":
        return "Posting %s to %s" % (says(text(a[1])), group(a[2]))
    if f == "post":
        return "Posting %s to %s" % (says(text(a[2])), group(a[3]))
    if f == "leave":
        return "Leaving %s" % group(a[2])
    if f == "child_unfriend":
        return "Ending %s's friendship with %s" % (name(a[0]), name(a[1]))
    if f == "child_leave":
        return "Ending %s's membership of %s" % (name(a[1]), group(a[3]))
    if f == "child_close":
        return "Closing %s" % group(a[2])
    if f in ("of_age", "of_age_2"):
        return "Declaring %s of age" % name(a[-1])
    if f == "unfriend":
        return "Ending my friendship with %s" % name(a[1])
    return "Requesting %s" % f


def approval(me, k):
    f, a = k
    if f == "attempt":
        f, a = a[3]
    if f == "enter":
        return "Approving my entering, with %s" % name(a[0]) if me == a[1][0] else "Approving %s's entering" % name(a[1])
    if f == "enter_2":
        if me == a[2][0]:
            return "Approving my entering, with %s and %s" % (name(a[0]), name(a[1]))
        return "Approving %s's entering, with %s" % (name(a[2]), name(a[0]))
    if f == "meet":
        return "Agreeing to meet %s" % name(a[0])
    if f == "introduce":
        return "Accepting %s's introduction of %s" % (name(a[0]), name(a[2]))
    if f in ("child_meet", "child_meet_2"):
        return "Approving %s–%s meeting" % (name(a[0]), name(a[1]))
    if f == "befriend":
        return "Accepting %s's friendship" % name(a[0])
    if f == "child_befriend":
        return "Approving %s–%s friendship" % (name(a[0]), name(a[1]))
    if f == "child_adult_befriend":
        return "Approving %s–%s friendship" % (name(a[0]), name(a[1]))
    if f == "child_join_2":
        return "Approving %s joining %s" % (name(a[1]), group(a[4]))
    if f == "child_join":
        return "Approving %s joining %s" % (name(a[1]), group(a[3]))
    if f == "join":
        return "Approving my joining %s" % group(a[2])
    return "Approving %s" % f


def changes(me, old, new):
    def count(xs):
        d = {}
        for x in xs:
            k = repr(x)
            d[k] = (d.get(k, (0, x))[0] + 1, x)
        return d
    o, n = count(old), count(new)
    added = [x for k, (c, x) in n.items() if c > o.get(k, (0, None))[0]]
    removed = [x for k, (c, x) in o.items() if c > n.get(k, (0, None))[0]]
    lines = []
    closed = [repr(x[1][0]) for x in added if x[0] == "closed"]
    for x in removed:
        f, a = x
        if f == "friend":
            lines.append(("result", "No longer friends with %s" % name(a[0])))
        elif f == "parenting":
            lines.append(("result", "%s is no longer my child" % name(a[0])))
        elif f == "parent":
            lines.append(("result", "%s is no longer my parent" % name(a[0])))
        elif f == "member" and repr(a[0][1][1]) not in closed:
            lines.append(("result", "Left %s" % group(a[0][1][1])))
        elif f == "listed" and a[0][0] != me:
            lines.append(("result", "%s left %s" % (name(a[0]), group(a[1]))))
    if any(x[0] == "child" for x in removed) and not any(x[0] == "child" for x in new):
        lines.append(("result", "No parent on record: an adult"))
    befriended = [repr(x[1][0]) for x in added if x[0] == "friend"]
    for x in added:
        f, a = x
        if f == "knows" and repr(a[0]) not in befriended:
            lines.append(("result", "Now knows %s" % name(a[0])))
        elif f == "identity":
            lines.append(("result", "Identity established"))
        elif f == "friend":
            lines.append(("result", "Now friends with %s" % name(a[0])))
        elif f == "parenting":
            lines.append(("result", "%s is my child" % name(a[0])))
        elif f == "parent":
            lines.append(("result", "%s is my parent" % name(a[0])))
        elif f == "member":
            g = group(a[0][1][1])
            lines.append(("result", "Created %s" % g if a[0][1][0][1][0][0] == me else "Joined %s" % g))
        elif f == "listed" and a[0][0] != me:
            lines.append(("result", "%s joined %s" % (name(a[0]), group(a[1]))))
        elif f in ("received", "received_in"):
            # received(x, p), from the friend p; received_in(x, a, n), delivered by the leader a
            who, via = author(a[0]), name(a[1])
            lines.append(("message", says(text(a[0])) + (" (%s)" % who if who == via else " (%s, via %s)" % (who, via))))
        elif f == "closed":
            lines.append(("result", "Closed %s" % group(a[0])))
        elif f == "retired":
            lines.append(("result", "Retired"))
    return lines


def narrative(me, screen):
    lines = []
    last = []
    for e in screen:
        f, a = e
        if f == "view":
            state = a[0][1]
            lines += changes(me, last, state)
            last = state
        elif f == "did":
            r = request(me, a[0])
            if r:
                lines.append(("request", r))
        elif f == "gave" and a[1][0] == "yes":
            lines.append(("request", approval(me, a[0])))
        elif f == "refused":
            lines.append(("result", "%s: %s" % (f, a[0][0])))
    return lines


# --- The picture -------------------------------------------------------------

def font(size, bold=False, italic=False):
    calibri = "/Applications/Microsoft Word.app/Contents/Resources/DFonts/"
    names = {(False, False): "Calibri.ttf", (True, False): "Calibrib.ttf", (False, True): "Calibrii.ttf", (True, True): "Calibriz.ttf"}
    p = calibri + names[(bold, italic)]
    if os.path.exists(p):
        return ImageFont.truetype(p, size)
    return ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", size, index=1 if bold else 0)


COLORS = {
    "alice": (63, 72, 170), "bob": (46, 125, 110), "grace": (122, 82, 150), "frank": (92, 104, 112),
    "carol": (112, 122, 196), "dave": (92, 160, 146), "eve": (160, 128, 180),
}
INK = {"request": (20, 20, 20), "result": (32, 110, 92), "message": (40, 50, 150)}


def draw(screens_by_agent, out):
    stories = {a: narrative(a, screens_by_agent[a]) for a in AGENTS}
    S = 2
    W = 1600 * S
    pad = 14 * S
    line = 23 * S
    head = 34 * S
    f_line = font(17 * S)
    f_req = font(17 * S)
    f_msg = font(17 * S, italic=True)
    f_head = font(18 * S, bold=True)
    f_title = font(26 * S, bold=True)
    f_acts = font(15 * S)
    rows = [AGENTS[:4], AGENTS[4:]]
    heights = [max(len(stories[a]) for a in row) * line + head + 2 * pad for row in rows]
    top = 60 * S + 34 * S
    H = top + sum(heights) + pad
    img = Image.new("RGB", (W, H), (255, 255, 255))
    d = ImageDraw.Draw(img)
    d.rectangle([0, 0, W, 60 * S], fill=(40, 96, 84))
    t = "CSSN Village"
    tw = d.textlength(t, font=f_title)
    d.text(((W - tw) / 2, 14 * S), t, font=f_title, fill=(255, 255, 255))
    d.rectangle([0, 60 * S, W, top], fill=(232, 242, 238))
    d.text((pad, 60 * S + 8 * S), ACTS, font=f_acts, fill=(30, 30, 30))
    y = top
    for row, h in zip(rows, heights):
        cw = W / len(row)
        for i, a in enumerate(row):
            x0 = int(i * cw)
            x1 = int((i + 1) * cw)
            c = COLORS[a]
            d.rectangle([x0, y, x1, y + head], fill=c)
            d.text((x0 + pad, y + 7 * S), TITLES[a], font=f_head, fill=(255, 255, 255))
            d.rectangle([x0, y + head, x1, y + h], fill=tuple(int(v + (255 - v) * 0.9) for v in c))
            d.line([x1, y, x1, y + h], fill=(200, 200, 200), width=S)
            ly = y + head + pad
            for kind, s in stories[a]:
                fnt = f_msg if kind == "message" else (f_req if kind == "request" else f_line)
                indent = 10 * S if kind == "request" else 0
                while d.textlength(s, font=fnt) > cw - 2 * pad - indent and len(s) > 4:
                    s = s[:-2] + "…" if not s.endswith("…") else s[:-2] + "…"
                d.text((x0 + pad + indent, ly), s, font=fnt, fill=INK[kind])
                ly += line
        y += h
        d.line([0, y, W, y], fill=(200, 200, 200), width=S)
    img.save(out)
    return {a: len(stories[a]) for a in AGENTS}


if __name__ == "__main__":
    if len(sys.argv) not in (2, 3):
        sys.exit("usage: village.py <log> [<png>]")
    here = os.path.dirname(os.path.abspath(__file__))
    out = sys.argv[2] if len(sys.argv) == 3 else os.path.join(here, "cssn-village.png")
    counts = draw(screens(sys.argv[1]), out)
    print("%s: %s" % (out, ", ".join("%s %d lines" % (a, counts[a]) for a in AGENTS)))
