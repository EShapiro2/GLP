# GLP A6 backlog

Work deferred out of A5 (the restructure). A6 is the typing/repair pass.

## A6-1 — Repair the 7 carried book/ orphans, then sweep-delete the source trees

Slice 7a (2026-06-13) carried 7 typed-by-declaration orphans from `OLD typed book/` into `book/`, but a load smoke-test shows **none compile under the current pipeline**. They are preserved in `book/` and kept OUT of the suite's `POSITIVE_FILES` (gate unaffected). They need a repair pass.

**Files to repair (in `programs/book/`):**

Two failure modes:
- **Parser-adjacency** (grouped declarations separated from clauses → `Procedure declaration for "X" has no clauses`) — 5 files:
  - `recursive/list_processing/bsort.glp`
  - `recursive/list_processing/isort.glp`
  - `recursive/structure_processing/ancestor.glp`
  - `recursive/structure_processing/heapify.glp`
  - `meta/basic/metainterpreter.glp`
- **Real type errors** (head mode mismatch, reader/producer) — 2 files:
  - `streams/merge/opmerge.glp`
  - `streams/transform/multiply.glp`

(The parser-adjacency mode interacts with the deferred harmonisation item — see `glp-a5-restructure-plan.md` Deferred/Harmonisation. If the parser's adjacency requirement is relaxed there, the 5 fix themselves.)

**Working reference — DO NOT DELETE until this repair lands:**
- `programs/OLD typed book/` — original-context sources of the 7 files (surrounding declarations, comments, sibling files that explain the failures).
- `programs/book 2/` — near-subset of the above.
- `programs/archive/` — incl. `archive/book` (untyped book).
- `test/run_book_tests.sh` — reads `archive/book`.

These four were name-coverage-cleared for deletion in A5 (slices 5–7a) but their deletion is **gated on this repair completing** (Udi, 2026-06-13): keeping them on disk is the convenience the repair pass wants; recovering from git history is more friction. Once the 7 files compile and are added to `POSITIVE_FILES`, delete all four in one sweep (delete `run_book_tests.sh` in the same commit as `archive/`, so nothing references `archive/book` at deletion; also remove the `run_book_tests.sh` mention from `GLP/CLAUDE.md`).

## A6-2 — Type the remaining untyped salvage

- `programs/examples/basics/bounded_buffer_difference_list.glp` — untyped (CP Papers Ch18, Takeuchi & Furukawa); A6 typing candidate. Distinct from `examples/basics/bb_dlist.glp`.
- (other untyped `examples/` salvage if a curriculum home is wanted.)
