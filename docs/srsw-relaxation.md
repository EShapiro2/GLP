# SRSW Relaxation: Anonymous Variables

**Date:** 2026-02-01
**Status:** Approved
**Impact:** Parser, Specs, Documentation

---

## Definition

An anonymous variable is any variable whose name begins with `_` (e.g., `_`, `_In?`, `_Out`). Anonymous writers may appear in the head, denoting a fresh writer with no paired reader, so that a value assigned to it is discarded. This provides a controlled exception to the SRSW restriction, allowing a process to abandon an input (e.g. an input stream) they are no longer interested in.

---

## Examples

```glp
%% Discard head and tail of list
second([_, X | _], X?).

%% Discard first output of bar
foo(X) :- bar(_Result, X?).

%% Named anonymous variables improve readability
process([msg(_From, _To, Content)|Rest], Out?) :-
    handle(Content?, Out),
    process(Rest?, Out?).
```

---

## Implementation

### SRSW Checker (`occurrence.dart`)

Variables starting with `_` are skipped in SRSW checking - they are exempt from the single-reader/single-writer requirement.

### Clause Validation (`clause_validation.dart`)

- Anonymous writers (`_`, `_Out`) are allowed in heads
- Anonymous readers (`_?`, `_In?`) are rejected - there is no use case for an anonymous reader

---

## Reference

- **Paper (Moded-Types):** Section on SRSW Relaxations
- **Paper (GLP-ICLP):** Remark 5 (Anonymous Variables)
- **Spec:** `docs/typed-glp-manual.md` Section 9
