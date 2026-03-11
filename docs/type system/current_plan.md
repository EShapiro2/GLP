# Current Plan: stdlib Cleanup — Remove stdlib Indirection

Started: 2026-03-11

## Steps
- [ ] 1. Pull main, run baseline tests (expect 399/399), commit baseline
- [ ] 2. Rename `stdlibDir` → `rootSelfGlpPath` in glp_runtime core (files 1–5) ← CURRENT
- [ ] 3. Run REPL tests — must be 399/399
- [ ] 4. Commit core rename
- [ ] 5. Rename in Flutter app (files 6–9) and test files
- [ ] 6. Final REPL tests — 399/399
- [ ] 7. Commit and push

## Context

The GLP engine locates `programs/self.glp` via a nonexistent `programs/stdlib` directory. The REPL constructs a path to this phantom directory, passes it to `GlpEngine(stdlibDir: ...)`, and the engine does `stdlibDir.replaceAll('/stdlib', '/self.glp')` — a string hack. There is no stdlib directory.

Full instructions: `docs/infra/stdlib-cleanup-instructions.md`
