#!/usr/bin/env python3
"""check_typing.py --- the tight typing discipline over every GLP and vGLP file
the repository tracks (Code #17's subagent, 2026-10-30; Coordination #9's task
1, Code_inbox 2026-10-25 05:28 UTC).

The rule (claude.md, "Code", Udi, 2026-10-24): NO NEW GLP CODE IS WRITTEN
UNTYPED, OR WITH OPEN TYPES.  A procedure is untyped when it has no
declaration; an open type is `_` or `_?` in a type definition or a procedure
declaration; the one exception is a system builtin in `self.glp`, which the
runtime implements.  It is docs/typed-glp-manual.md section 18 without 18.3's
exception for meta-interpreters.

What is read, and from which paper:
  - A procedure's declaration is in its own module: TGLP Definition "Typed
    Procedure, Module" (modules.tex, def:module), a typed procedure being a
    procedure declaration immediately followed by the procedure, and a module a
    sequence of type definitions and typed procedures; with TGLP Definition
    "Typed GLP Program" (typed-glp.tex), every procedure having exactly one
    type declaration.  So a procedure with clauses in a file and no
    `procedure` or `exported procedure` declaration of its name and arity in
    that file is untyped, whatever an ancestor `self.glp` declares.
  - `_` and `_?` are the primitive wildcard types (TGLP "Type Declarations").
  - The exception: a declaration in the root programs/self.glp of a procedure
    that has no clauses there, a guard predicate or body kernel the runtime
    implements (TGLP Definition "Typed GLP Program", item 2, and Appendix
    "GLP Language Primitives"; GLP-Spec glp.tex, "System predicates and body
    kernels": a system predicate is defined by GLP clauses, a body kernel is
    runtime-implemented).  A kernel or seam predicate of the root self.glp,
    defined there by clauses, is not excepted.

Output: one line per finding, sorted, tab-separated:
    <file>\t<kind>\t<name>
kind is one of
    untyped     a procedure with clauses and no declaration in its file
    open-decl   `_` or `_?` in a procedure declaration (procedure, exported
                procedure or imported procedure)
    open-type   `_` or `_?` in a type definition
and name is name/arity for a procedure and the type's name for a type.

Usage: check_typing.py [<repo root>]   (default: this script's ../)
Exit 0 having printed the findings; exit 2 on a file it cannot read, naming it.
"""
import os
import subprocess
import sys

SYMBOL_CHARS = set("+-*/\\^<>=~:.?@#&$")
SOLO = set("()[]{},|;!")


class LexError(Exception):
    pass


def tokenize(text):
    """Tokens as (kind, value, adjacent) with kind in atom, var, num, str,
    punct, sym, end; `adjacent` is True when no layout precedes the token.
    A `?` after a variable, `)` or `]`, with or without layout between, is a
    token of its own (the reader and complement mark), as the printed modules
    of sGLP's transformation write it."""
    toks = []
    i, n = 0, len(text)
    adjacent = False
    while i < n:
        c = text[i]
        if c in " \t\r\n":
            i += 1
            adjacent = False
            continue
        if c == "%":
            j = text.find("\n", i)
            i = n if j < 0 else j
            adjacent = False
            continue
        if c == "/" and text.startswith("/*", i):
            j = text.find("*/", i + 2)
            if j < 0:
                raise LexError("unterminated /* comment")
            i = j + 2
            adjacent = False
            continue
        start = i
        if c == "'" or c == '"':
            q = c
            i += 1
            buf = []
            while True:
                if i >= n:
                    raise LexError("unterminated quote at offset %d" % start)
                d = text[i]
                if d == "\\":
                    buf.append(text[i:i + 2])
                    i += 2
                    continue
                if d == q:
                    if i + 1 < n and text[i + 1] == q:
                        buf.append(q)
                        i += 2
                        continue
                    i += 1
                    break
                buf.append(d)
                i += 1
            toks.append(("atom" if q == "'" else "str", "".join(buf), adjacent))
        elif c.isdigit():
            if text.startswith("0'", i) and i + 2 < n:
                i += 3
            else:
                while i < n and (text[i].isdigit() or text[i] == "_"):
                    i += 1
                if i + 1 < n and text[i] == "." and text[i + 1].isdigit():
                    i += 1
                    while i < n and text[i].isdigit():
                        i += 1
                if i < n and text[i] in "eE" and i + 1 < n and (text[i + 1].isdigit() or (text[i + 1] in "+-" and i + 2 < n and text[i + 2].isdigit())):
                    i += 2
                    while i < n and text[i].isdigit():
                        i += 1
            toks.append(("num", text[start:i], adjacent))
        elif c.isalpha() or c == "_":
            while i < n and (text[i].isalnum() or text[i] == "_"):
                i += 1
            word = text[start:i]
            kind = "var" if (word[0].isupper() or word[0] == "_") else "atom"
            toks.append((kind, word, adjacent))
        elif c in SOLO:
            i += 1
            toks.append(("punct", c, adjacent))
        elif c == "?" and toks and (toks[-1][0] == "var" or (toks[-1][0] == "punct" and toks[-1][1] in (")", "]"))):
            i += 1
            toks.append(("sym", "?", adjacent))
        elif c in SYMBOL_CHARS:
            while i < n and text[i] in SYMBOL_CHARS:
                i += 1
            run = text[start:i]
            # A '.' ending a run before layout or end of file ends a clause,
            # `+.` being `+` and the end, except the operator `=..`, the one
            # operator of GLP's that ends in '.'.
            if run.endswith(".") and run != "=.." and (i >= n or text[i] in " \t\r\n%"):
                if len(run) > 1:
                    toks.append(("sym", run[:-1], adjacent))
                toks.append(("end", ".", False))
            else:
                toks.append(("sym", run, adjacent))
        else:
            raise LexError("unexpected character %r at offset %d" % (c, i))
        adjacent = True
    return toks


def clauses(toks):
    cur = []
    for t in toks:
        if t[0] == "end":
            if cur:
                yield cur
            cur = []
        else:
            cur.append(t)
    if cur:
        raise LexError("text after the last clause's full stop")


OPEN = {"(": ")", "[": "]", "{": "}"}


def depth0(toks):
    """Yield (index, token) for the tokens at bracket depth 0."""
    d = 0
    for k, t in enumerate(toks):
        if t[0] == "punct" and t[1] in OPEN:
            if d == 0:
                yield k, t
            d += 1
        elif t[0] == "punct" and t[1] in (")", "]", "}"):
            d -= 1
            if d == 0:
                yield k, t
        elif d == 0:
            yield k, t


def args_of(toks, k):
    """toks[k] is '(': the number of arguments up to its matching ')' and the
    index after it."""
    d = 0
    commas = 0
    empty = True
    for j in range(k, len(toks)):
        t = toks[j]
        if t[0] == "punct" and t[1] in OPEN:
            d += 1
            if d == 1:
                continue
        elif t[0] == "punct" and t[1] in (")", "]", "}"):
            d -= 1
            if d == 0:
                return (0 if empty else commas + 1), j + 1
        elif d == 1 and t[0] == "punct" and t[1] == ",":
            commas += 1
        if d >= 1:
            empty = False
    raise LexError("unbalanced parentheses")


def head_key(toks):
    """The name/arity of the procedure a head (or a declared signature) names."""
    if not toks:
        raise LexError("empty head")
    # vGLP's question form, (T)*name(Args).
    if toks[0][0] == "punct" and toks[0][1] == "(":
        _, after = args_of(toks, 0)
        if after < len(toks) and toks[after][0] == "sym" and toks[after][1] == "*":
            return "*" + head_key(toks[after + 1:])
    # A cross-module qualifier, M#p(...), in an imported declaration.
    top = list(depth0(toks))
    for k, t in top:
        if t[0] == "sym" and t[1] == "#":
            left = "".join(x[1] for x in toks[:k])
            return left + "#" + head_key(toks[k + 1:])
    # An infix operator head, X? = X, R? := E, X? =.. L.
    for k, t in top:
        if k > 0 and t[0] == "sym" and t[1] != "?":
            return t[1] + "/2"
    t0 = toks[0]
    if t0[0] not in ("atom", "sym"):
        raise LexError("head begins with %s %r" % (t0[0], t0[1]))
    if len(toks) > 1 and toks[1][0] == "punct" and toks[1][1] == "(":
        n, after = args_of(toks, 1)
        return "%s/%d" % (t0[1], n)
    return "%s/0" % t0[1]


def is_open(t):
    return t[0] == "var" and t[1] == "_"


def scan_file(path, rel, root_self):
    with open(path, encoding="utf-8") as f:
        text = f.read()
    toks = tokenize(text)
    defined = set()       # procedures with clauses here
    declared = {}         # name/arity -> declaration tokens (local and exported)
    findings = set()
    for cl in clauses(toks):
        first = cl[0]
        # A directive: -mode(system). -expose(M). :- ...
        if first[0] == "sym" and first[1] in ("-", ":-"):
            continue
        tops = [t for _, t in depth0(cl)]
        if any(t[0] == "sym" and t[1] == "=::=" for t in tops):
            continue      # a vGLP widget declaration, neither a type nor a procedure
        if any(t[0] == "sym" and t[1] == "::=" for t in tops):
            name = first[1]
            if any(is_open(t) for t in cl):
                findings.add((rel, "open-type", name))
            continue
        words = []
        k = 0
        while k < len(cl) and cl[k][0] == "atom" and cl[k][1] in ("exported", "imported") and k + 1 < len(cl) and cl[k + 1][1] == "procedure":
            words.append(cl[k][1])
            k += 1
        if k < len(cl) and cl[k][0] == "atom" and cl[k][1] == "procedure" and k + 1 < len(cl) and not (cl[k + 1][0] == "sym" and cl[k + 1][1] in (":-",)):
            k += 1
            sig = cl[k:]
            # procedure(X, Y) p(...): the parameter list; vGLP's question form,
            # procedure (T)*p(...), is told from it by the `*` after the `)`.
            if sig and sig[0][0] == "punct" and sig[0][1] == "(":
                _, after = args_of(sig, 0)
                if not (after < len(sig) and sig[after][0] == "sym" and sig[after][1] == "*"):
                    sig = sig[after:]
            key = head_key(sig)
            if "imported" not in words:
                declared[key] = sig
            if any(is_open(t) for t in sig):
                findings.add((rel, "open-decl", key))
            continue
        # A clause.
        head = []
        for _, t in depth0(cl):
            if t[0] == "sym" and t[1] == ":-":
                break
        idx = len(cl)
        for k2, t in depth0(cl):
            if t[0] == "sym" and t[1] == ":-":
                idx = k2
                break
        head = cl[:idx]
        defined.add(head_key(head))
    for key in defined:
        if key not in declared:
            findings.add((rel, "untyped", key))
    if root_self:
        # The root self.glp's clause-less declarations are the runtime's guard
        # predicates and body kernels: excepted from open-decl.
        findings = {f for f in findings
                    if not (f[1] == "open-decl" and f[2] not in defined)}
    return findings


def main():
    root = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else
                           os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
    files = subprocess.run(["git", "-C", root, "ls-files", "-z", "--", "*.glp", "*.vglp"],
                           check=True, capture_output=True).stdout.decode().split("\0")
    findings = set()
    bad = []
    for rel in sorted(f for f in files if f):
        path = os.path.join(root, rel)
        if not os.path.isfile(path):
            continue      # deleted in the working tree
        try:
            findings |= scan_file(path, rel, rel == "programs/self.glp")
        except (LexError, UnicodeDecodeError) as e:
            bad.append("%s: %s" % (rel, e))
    for f in sorted(findings):
        print("\t".join(f))
    if bad:
        for b in bad:
            print("check_typing.py: cannot read " + b, file=sys.stderr)
        sys.exit(2)


if __name__ == "__main__":
    main()
