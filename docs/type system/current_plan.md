# Current Plan: Fix Parameterized Procedure Declaration Type Checking

Started: 2026-03-07

## Steps
- [x] 1. Stage 1: Implement parameterized types (parser, expansion, tests)
- [x] 2. Step 1.4: Add parameterized type defs to self.glp alongside monomorphic
- [x] 3. Step 2.1: Convert test files to parameterized types
- [x] 4. Step 2.2: Convert typed_book to parameterized types (97 files)
- [x] 5. Step 2.3: Convert module applications (CSSG, CSSN, simulated UI)
- [x] 6. FIX: Parameterized proc decl type checking
- [x] 7. Step 2.4: Parameterize self.glp procedures + remove monomorphic types
- [ ] 8. **Step 2.5: Remove renamed procedure copies (send_agent, send_user, etc.)** ← CURRENT
- [x] 9. Step 2.6: Archive book/ directory (done out of order, commit 1473fb31)
- [ ] 10. Step 2.7: Adopt tight typing discipline (documentation)
- [ ] 11. Step 2.8: Final validation

## Context

Step 2.4 complete (commit c3348bd1). Monomorphic types removed from self.glp, param_expansion.dart fixed to post-process expanded defs through _replaceParamRefs, proc decl signatures corrected for 2-arg Channel, test files updated. 390/390 REPL tests pass.

Next: Step 2.5 — remove renamed procedure copies (send_agent, send_user, merge_agent, etc.) that parameterized types make unnecessary.

## Master Plan

See `docs/type system/parameterized-types-plan.md` for the full two-stage plan.
