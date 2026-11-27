#!/usr/bin/env python3
"""deadline_control.py --- controls of Warnings' Expel deadline (Grassroots-Audit #136), on the schema as printed.

Reads the condition of sas(expel, ...) and sas(officer_expel, ...) in Warnings.glp beside this file, as
calculus/tools/adopt.py prints it --- cardinality([X1, EnoughD], atom(warning_on, [Orga, X1, Bob, num(EnoughD)]),
gt(num(Today), plus(num(EnoughD), num(P)))), num(Enough)), ge(num(Enough), num(K)) --- and evaluates it on states of
warnings, each a person and the date of the warning, at a date Today.  Prints one line per control, "<name> ok" or
"<name> WRONG", and exits 1 if any is wrong.  Standard library only.
"""
import os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
src = open(os.path.join(HERE, "Warnings.glp")).read()


def condition(act):
    line = next(l for l in src.split("\n") if l.strip().startswith("sas(%s," % act))
    m = re.search(r"cardinality\(\['X1', 'EnoughD'\], atom\(warning_on, \['Orga', 'X1', 'Bob', num\('EnoughD'\)\]\), "
                  r"gt\(num\('Today'\), plus\(num\('EnoughD'\), num\((\d+)\)\)\)\), num\('Enough'\)\), "
                  r"ge\(num\('Enough'\), num\((\d+)\)\)", line)
    if not m:
        sys.exit("deadline_control.py: the condition of %s is not the counted one this control reads" % act)
    return int(m.group(1)), int(m.group(2))


def enabled(act, warnings, today):
    """warnings: [(person, date)] that Orga holds to Bob; the act is enabled when Enough, the number of persons whose
    warning was given more than P days before Today, is at least K."""
    p, k = condition(act)
    enough = len({x for x, d in warnings if today > d + p})
    return enough >= k


CONTROLS = [
    # name, warnings to Bob held by Orga, today, enabled?
    ("two warnings, one at its deadline", [("Alice", 10), ("Carol", 12)], 19, False),
    ("two warnings, both past their deadline", [("Alice", 10), ("Carol", 12)], 20, True),
    ("two warnings, both at their deadline", [("Alice", 10), ("Carol", 10)], 17, False),
    ("two warnings, both a day past it", [("Alice", 10), ("Carol", 10)], 18, True),
    ("one warning, long past", [("Alice", 1)], 100, False),
    ("two warnings of one person cannot be held", [("Alice", 1), ("Alice", 2)], 100, False),
    ("a former officer's warning still held counts", [("Alice", 1), ("Dave", 2)], 100, True),
]

bad = 0
for act in ("expel", "officer_expel"):
    for name, ws, today, want in CONTROLS:
        got = enabled(act, ws, today)
        print("%s: %s %s" % (act, name, "ok" if got == want else "WRONG"))
        bad += got != want
sys.exit(1 if bad else 0)
