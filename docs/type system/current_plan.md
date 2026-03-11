# Current Plan: stdlib Cleanup — Remove stdlib Indirection

Started: 2026-03-11
Completed: 2026-03-11

## Steps
- [x] 1. Pull main, run baseline tests (expect 399/399), commit baseline
- [x] 2. Rename `stdlibDir` → `rootSelfGlpPath` in glp_runtime core (files 1–5)
- [x] 3. Run REPL tests — 399/399
- [x] 4. Commit core rename
- [x] 5. Rename in Flutter app (files 6–9) and test files
- [x] 6. Final REPL tests — 399/399
- [x] 7. Commit and push to main

## Context

DONE. The `stdlib` indirection has been fully removed. `GlpEngine` now takes `rootSelfGlpPath` — the absolute path to `programs/self.glp` — directly. No more phantom `stdlib` directory, no more `.replaceAll` hack, no more CWD-dependent path resolution.

Full instructions were in `docs/infra/stdlib-cleanup-instructions.md`.
