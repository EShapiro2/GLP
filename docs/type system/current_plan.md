# Current Plan: Fix Parameterized Procedure Declaration Type Checking

Started: 2026-03-07

## Steps
- [x] 1. Stage 1: Implement parameterized types (parser, expansion, tests)
- [x] 2. Step 1.4: Add parameterized type defs to self.glp alongside monomorphic
- [x] 3. Step 2.1: Convert test files to parameterized types
- [x] 4. Step 2.2: Convert typed_book to parameterized types (97 files)
- [x] 5. Step 2.3: Convert module applications (CSSG, CSSN, simulated UI)
- [x] 6. FIX: Parameterized proc decl type checking
- [ ] 7. **Step 2.4: Parameterize self.glp procedures + remove monomorphic types** ← CURRENT
- [ ] 8. Step 2.5: Remove renamed procedure copies (send_agent, send_user, etc.)
- [x] 9. Step 2.6: Archive book/ directory (done out of order, commit 1473fb31)
- [ ] 10. Step 2.7: Adopt tight typing discipline (documentation)
- [ ] 11. Step 2.8: Final validation

## Context

Parameterized types are implemented, most code is converted, and parameterized procedure declarations are properly type-checked (Step 6 fixed the shortcut). 390 REPL tests pass. Next: remove the remaining Section 14 workarounds (renamed procedure copies) that parameterized types were designed to replace.

## Current Task (Step 7)

Parameterize the prelude's generic procedures in `self.glp` and remove the old monomorphic type definitions. This must happen BEFORE removing renamed procedure copies (Step 2.5), because the renamed copies can only be replaced once the originals are parameterized.

Per the plan (`docs/type system/parameterized-types-plan.md`, Step 2.4).

Key changes in `self.glp`:
- `procedure merge(Stream?, Stream?, Stream).` → `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).`
- `procedure send(_?, Channel?, Channel).` → `procedure send(X?, Channel(Stream(X))?, Channel(Stream(X))).`
- `procedure receive(_, Channel?, Channel).` → `procedure receive(X, Channel(Stream(X))?, Channel(Stream(X))).`
- `procedure new_channel(Channel, Channel).` → `procedure new_channel(Channel(X, Y), Channel(Y, X)).`
- Remove monomorphic `Stream`, `OpenStream`, `Channel`, `DiffList` (keep only parameterized versions)
- Convert module-local monomorphic channel types (e.g., `AgentChannel`) to use `Channel(InType, OutType)` instantiations

Run full test suite after changes. All 390 tests must pass.

## Master Plan

See `docs/type system/parameterized-types-plan.md` for the full two-stage plan.
