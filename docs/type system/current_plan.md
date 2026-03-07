# Current Plan: Fix Parameterized Procedure Declaration Type Checking

Started: 2026-03-07

## Steps
- [x] 1. Stage 1: Implement parameterized types (parser, expansion, tests)
- [x] 2. Step 1.4: Add parameterized type defs to self.glp alongside monomorphic
- [x] 3. Step 2.1: Convert test files to parameterized types
- [x] 4. Step 2.2: Convert typed_book to parameterized types (97 files)
- [x] 5. Step 2.3: Convert module applications (CSSG, CSSN, simulated UI)
- [x] 6. FIX: Parameterized proc decl type checking
- [ ] 7. **Step 2.4: Remove renamed procedure copies (send_agent, send_user, etc.)** ← CURRENT
- [ ] 8. Step 2.5: Remove monomorphic definitions from self.glp
- [ ] 9. Step 2.6: Archive book/ directory
- [ ] 10. Step 2.7: Adopt tight typing discipline (documentation)
- [ ] 11. Step 2.8: Final validation

## Context

Parameterized types are implemented, most code is converted, and parameterized procedure declarations are properly type-checked (Step 6 fixed the shortcut). 390 REPL tests pass. Next: remove the remaining Section 14 workarounds (renamed procedure copies) that parameterized types were designed to replace.

## Current Task (Step 7)

With parameterized proc decl type checking now working (Step 6), remove the Section 14 workarounds: renamed procedure copies like `send_agent`, `send_user`, `new_agent_channel`, etc. The parameterized originals (`send`, `receive`, `new_channel`) should now serve the same purpose through call-site type parameter inference.

Per the plan (`docs/type system/parameterized-types-plan.md`, Step 2.4).

Files to search: `programs/typed_book/`, `programs/cssg_modules/`, `programs/cssn_modules/`, `programs/social_graph_simulated_ui_modules/` for renamed copies. Replace calls with the parameterized originals. Run full test suite after each directory. All 390 tests must pass.

## Master Plan

See `docs/type system/parameterized-types-plan.md` for the full two-stage plan.
