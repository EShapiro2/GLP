# GCE handover, GCE #1 to GCE #2 (2026-11-25, after GCE #1's first compaction)

GCE is Nimrod Talmon's contracts, `programs/gce`, mail `GCE_inbox.md`.  Branch `claude/project-thread-q08byy`.  Full log: project memory `gce-thread`.  Udi's retrospective list: `/mnt/project-files/gce/audit-135-136-marked-2026-11-25.md` (items 1-9).

## Next step

`programs/gce/chess/functions.glp`, asked by vGLP #34: Chess's functions as typed GLP procedures, one per function, places in order and value last (`first(B)`, `legal(M?,B?,V)`, `after(M?,B?,C)`, `side(B?,S)`, `mate(B?,V)`, `draw(B?,V)`), types of positions and moves in the same file.  GCE #1 wrote it and committed it with this handover **untested**: it has not been loaded in glpc.  GCE #2:

1. Load it and fix what the type checker and SRSW refuse:
   `export PATH=<scratchpad>/dart-sdk/bin:$PATH; cd glp_runtime; printf 'load ../programs/gce/chess/functions.glp\nfirst(B), side(B?,S).\n:quit\n' | bin/glpc`
   (Dart 3.14.1 was downloaded to GCE #1's scratchpad; a new session downloads its own.)  Doubts: `abs(...)` and `=?=` in guards, `ground` before reading a list twice, the `-1` literal, `otherwise` clauses.
2. Test: the first position, side 1; `legal(mv(12,28,0),...)` = 1 (e2-e4), `legal(mv(12,36,0),...)` = 0; `after` then `side` = 2; fool's mate gives `mate` 1; a stalemate gives `draw` 1.  Add these as a test file beside it.
3. Commit path-limited, push, mail vGLP #34 and Code #39 the commit.

Encoding: square = rank*8 + file (a1 = 0, h8 = 63); cell 0 empty, else colour*10 + kind (colour 1 white, 2 black; kind 1 pawn ... 6 king); `pos(Cells, Side, cas(WK,WQ,BK,BQ), Ep or -1, HalfMoves, Keys)`; `mv(From, To, PromoKind or 0)`.

## Waiting on others

- #135 Disputes, #136 Warnings (e4aacb8): verified by the auditor; wait for Code to adopt to GLP main, then give Audit #5 thread the main commit.
- #151 Warnings act/amend: answered See:, waits on Code/vGLP compiler (vGLP 33f81f6); give Audit #5 the main commit.
- #157 Chess (0ec40a3): Findings 2-4 corrected, 1 and 5 See:.  Calculus asked to publish; Chess.glp not printed again yet.  Finding 1 is functions.glp above.
- Udi's retrospective yes on items 6-9 of the marked list.
