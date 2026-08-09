# Untyped-book coverage diff — orphan list (A5 Stage A, item 3)

**Date:** 2026-06-13 · **Slice:** A5 Stage A slice 5
**Purpose:** Before deleting the old untyped book trees (`programs/OLD typed book/`, `programs/book 2/`), determine which of their programs are already covered by the renamed typed book (`programs/book/`, formerly `typed_book`) and which are unique (orphans worth preserving). **No deletion performed — this is the check Udi asked to see first.**

## Method (and its caveat)

Comparison is by **`.glp` basename** (file name, ignoring directory): a file in an old tree is "covered" if a file of the same name exists anywhere under `programs/book/`. 🔴 **This is name-based, not content-verified** — a same-named file in `book/` may differ from the old version. The covered set below is therefore a *deletion-candidate* set, not a proven-redundant set; spot-check content before deleting. Orphans (no same-name file in `book/`) are the safe-to-preserve set.

## Results

| Old tree | unique basenames | covered by `book/` | orphans |
|---|---|---|---|
| `OLD typed book/` | 160 | 150 | 10 |
| `book 2/` | 147 | 145 | 2 |

`book/` itself has 201 unique `.glp` basenames.

### Orphans — `OLD typed book/` (10)
no same-name file under `book/`:
- `ancestor.glp`
- `bounded_buffer_difference_list.glp`
- `bsort.glp`
- `heapify.glp`
- `isort.glp`
- `merge.glp`
- `metainterpreter.glp`
- `multiply.glp`
- `opmerge.glp`
- `quicksort_backslash.glp`

### Orphans — `book 2/` (2)
- `ancestor.glp`
- `heapify.glp`

(Both are also orphans of `OLD typed book/`; `book 2/` is otherwise a near-subset of `OLD typed book/` w.r.t. names.)

## Reading

- ~150/160 of `OLD typed book/` and 145/147 of `book 2/` share names with `book/` — strong signal the new typed book supersedes most of the old trees.
- The 10 orphans are sorting/utility/meta programs (`bsort`, `isort`, `heapify`, `quicksort_backslash`, `opmerge`, `merge`, `multiply`, `ancestor`, `metainterpreter`, `bounded_buffer_difference_list`) absent by name from `book/`. These are the candidates to salvage (to `book/`, `examples/`, or `tests/`) before any deletion.

## Orphan descriptions (the deletion gate — Udi reviews before any deletion)

Per-file: one-line description, whether typed or untyped, and disposition class — **A6** = untyped, a typing-pass candidate; **CARRY** = already typed, unique content not in `book/`, carry-forward candidate; **DEAD** = superseded/redundant. (Name-coverage of the other ~150/145 files is accepted as sufficient per Udi — the deletion gate is only these orphans.)

| Orphan (`OLD typed book/…`) | typed? | demonstrates | class |
|---|---|---|---|
| `recursive/structure_processing/ancestor.glp` | typed (Any) | transitive closure: `parent` → `ancestor` relation | CARRY |
| `variants/bounded_buffer_difference_list.glp` | **untyped** | bounded-buffer stream comms via difference lists (CP Papers Ch18, Takeuchi & Furukawa) | **A6** — but cf. salvaged `examples/basics/bb_dlist.glp` (likely the same concept; check before typing) |
| `recursive/list_processing/bsort.glp` | typed (NumList) | bubble sort | CARRY (extends `book/recursive/list_processing`, which has quicksort + merge_sort but no bubble sort) |
| `recursive/structure_processing/heapify.glp` | typed (Tree) | heap construction / heapsort over a binary tree | CARRY |
| `recursive/list_processing/isort.glp` | typed (NumList) | insertion sort | CARRY |
| `streams/merge/merge.glp` | typed (Stream) | nondeterministic stream merge | CARRY-or-DEAD (basic merge; may be subsumed by `book/` stream content under another name) |
| `meta/basic/metainterpreter.glp` | typed (Goal) | meta-interpreter for `Goal` terms | CARRY |
| `streams/transform/multiply.glp` | typed (NumStream) | stream transformer: multiply each element | CARRY-or-DEAD |
| `streams/merge/opmerge.glp` | typed (NumStream) | ordered merge of two sorted number streams | CARRY |
| `variants/quicksort_backslash.glp` | **untyped** | quicksort via difference lists (backslash/diff-list notation) | **A6** — but `book/` already has typed `quicksort.glp`; likely **DEAD** (superseded), confirm |

`book 2/` orphans (`ancestor.glp`, `heapify.glp`) are the same two programs — a strict subset; no separate review needed.

**Summary for the deletion call:** 8 of 10 orphans are already **typed** (not A6 work) — mostly sorting/stream/meta algorithms absent from `book/` by name; the natural disposition is to **carry them into `book/`** rather than delete. Only 2 are **untyped** (A6 candidates), and one of those (`quicksort_backslash`) is likely already superseded by `book/quicksort.glp`. So almost nothing here is genuinely dead — deleting `OLD typed book/` + `book 2/` outright would drop ~8 typed algorithms the new `book/` lacks.

## Harvest finding (slice 7a) — the carried orphans DO NOT LOAD

After carrying the 7 typed orphans into `book/`, a load smoke-test (each file, full pipeline) shows **none load cleanly under the current pipeline**, despite having type declarations:

| carried file | failure |
|---|---|
| `recursive/list_processing/bsort.glp` | `[syntax] Procedure declaration for "sort" has no clauses` — grouped-declaration style (the **parser-adjacency** deferred item) |
| `recursive/list_processing/isort.glp` | same (parser-adjacency) |
| `recursive/structure_processing/ancestor.glp` | same (parser-adjacency) |
| `recursive/structure_processing/heapify.glp` | same (parser-adjacency) |
| `meta/basic/metainterpreter.glp` | same (parser-adjacency) |
| `streams/merge/opmerge.glp` | real type error: head mode mismatch (reader requires ↓, got ↑) — plus more |
| `streams/transform/multiply.glp` | real type errors (10) |

So these are **old-generation typed files that predate current parser/type-checker strictness** — "typed" by declaration, not by passing. Fixing them = a typing/repair pass (A6-adjacent), which Udi said NOT to start. They are carried (preserved in `book/` + git history) and **NOT added to the suite's POSITIVE_FILES list**, so the gate is unaffected. They sit in `book/` as carried-but-unverified content awaiting a fixing pass.

This overturns the slice premise ("harvest the good typed algorithms `book/` lacks"): the algorithms are absent from `book/` *and* don't currently compile. Deletion of the source trees is still **safe** (content preserved in `book/` + git), but whether to delete the sources *now* vs keep `OLD typed book/` as the working reference until the files are repaired is a call for Udi → **deletions (items 3–4) held pending his confirmation.**

## Status / next

STOPPED before any deletion (per Udi); A6 typing NOT started. Pending Udi's combined ruling on all four deletions:
1. Carry the ~8 CARRY orphans into `book/` (and where), before deleting the old trees?
2. Confirm the 2 A6/untyped orphans' fate (`quicksort_backslash` likely DEAD; `bounded_buffer_difference_list` → A6 backlog).
3. Then delete `OLD typed book/` + `book 2/` (name-coverage accepted for the rest).
4. Then delete `archive/` + retire `run_book_tests.sh` (it still reads `archive/book` — Q2 ordering).
