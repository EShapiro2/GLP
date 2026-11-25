# GCE handover (GCE #1 2026-11-25; GCE #2 2026-11-25)

GCE is Nimrod Talmon's contracts, `programs/gce`, mail `GCE_inbox.md`.  Branch `claude/project-thread-q08byy`.  Full log: project memory `gce-thread`.  Udi's retrospective list: `/mnt/project-files/gce/audit-135-136-marked-2026-11-25.md` (items 1-9).

## Done by GCE #2

`programs/gce/chess/functions.glp` loads and type-checks in glpc (two clauses added: `cell` and `set` on `[]`, which the checker's coverage asked for).  Tested in Section GCE of `test/run_all_tests.sh` (33 checks, all pass): first position, legal moves and not, castling, en passant, promotion, a check blocked and not, fool's mate, stalemate, the kings alone, fifty moves, threefold repetition.  `draw` on a position with moves scans for a legal move and takes about 50,000 reductions; the REPL's default limit of 10,000 caps it, so the section sets `:limit`.  Mailed vGLP #34 and Code #39 the commit.

Encoding: square = rank*8 + file (a1 = 0, h8 = 63); cell 0 empty, else colour*10 + kind (colour 1 white, 2 black; kind 1 pawn ... 6 king); `pos(Cells, Side, cas(WK,WQ,BK,BQ), Ep or -1, HalfMoves, Keys)`; `mv(From, To, PromoKind or 0)`.

## Waiting on others

- #135 Disputes, #136 Warnings (e4aacb8): verified by the auditor; wait for Code to adopt to GLP main, then give Audit #5 thread the main commit.
- #151 Warnings act/amend: answered See:, waits on Code/vGLP compiler (vGLP 33f81f6); give Audit #5 the main commit.
- #157 Chess (0ec40a3): Findings 2-4 corrected, 1 and 5 See:.  Calculus asked to publish; Chess.glp not printed again yet.  Finding 1 is functions.glp above.
- Udi's retrospective yes on items 6-9 of the marked list.
