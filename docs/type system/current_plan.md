# Current Plan: Fix Parameterized Procedure Declaration Type Checking

Started: 2026-03-07

## Steps
- [x] 1. Stage 1: Implement parameterized types (parser, expansion, tests)
- [x] 2. Step 1.4: Add parameterized type defs to self.glp alongside monomorphic
- [x] 3. Step 2.1: Convert test files to parameterized types
- [x] 4. Step 2.2: Convert typed_book to parameterized types (97 files)
- [x] 5. Step 2.3: Convert module applications (CSSG, CSSN, simulated UI)
- [ ] 6. **FIX: Parameterized proc decl type checking** ← CURRENT
- [ ] 7. Step 2.4: Remove renamed procedure copies (send_agent, send_user, etc.)
- [ ] 8. Step 2.5: Remove monomorphic definitions from self.glp
- [ ] 9. Step 2.6: Archive book/ directory
- [ ] 10. Step 2.7: Adopt tight typing discipline (documentation)
- [ ] 11. Step 2.8: Final validation

## Context

Parameterized types are implemented and most code is converted. But parameterized procedure declarations (e.g., `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).`) currently **skip type checking entirely** — four places in the code return success without checking. This was a shortcut taken by a previous session that must be fixed before continuing.

## Current Task (Step 6)

**CLEANUP FIRST**: A previous session left broken uncommitted changes on main. Before doing anything:

```bash
cd /Users/udi/Grassroots/GLP && cp "docs/type system/fix-parameterized-proc-checking.md" /tmp/fix-param.md && cp docs/current_plan.md /tmp/current_plan.md && cp CLAUDE.md /tmp/CLAUDE.md && cp /Users/udi/Grassroots/claude.md /tmp/grassroots-claude.md && git checkout -- . && git clean -fd && mkdir -p "docs/type system" && cp /tmp/fix-param.md "docs/type system/fix-parameterized-proc-checking.md" && cp /tmp/current_plan.md docs/current_plan.md && cp /tmp/CLAUDE.md CLAUDE.md && cp /tmp/grassroots-claude.md /Users/udi/Grassroots/claude.md && git add -A && git commit -m "Add current plan, fix instructions, update CLAUDE.md startup protocol" && git pull --no-rebase --no-edit origin main && git push origin main
```

Then verify clean state: `bash test/run_all_tests.sh` — must show 389 pass.

**Detailed fix instructions**: Read `docs/type system/fix-parameterized-proc-checking.md`

**Additional source files to read** (after the mandatory CLAUDE.md reading):
- `glp_runtime/lib/analysis/type_checker/param_expansion.dart`
- `glp_runtime/lib/analysis/type_checker/type_ast.dart`
- `glp_runtime/lib/analysis/type_checker/well_typed_clause.dart`
- `glp_runtime/lib/analysis/type_checker/program_dfa.dart`
- `glp_runtime/lib/analysis/type_checker/type_checker.dart`

**The fix has two parts that MUST BOTH be implemented:**

**Case A**: For checking a parameterized proc decl's own clauses, instantiate type params to `_` (wildcard). `merge(Stream(X)?, Stream(X)?, Stream(X))` becomes `merge(Stream<_>?, Stream<_>?, Stream<_>)`. Check clauses against this concrete form. No skipping.

**Case B**: For checking calls to parameterized procs from other code, infer type param bindings from the caller's known variable types. `merge(A?, B?, C)` where A has type `Stream<AgentMsg>?` → infer X=AgentMsg → check against concrete `merge(Stream<AgentMsg>?, Stream<AgentMsg>?, Stream<AgentMsg>)`. No skipping.

A previous session attempted this and failed — it implemented Case A but not Case B, leaving tests broken. That work was reverted. Start from clean state (389 tests pass). Implement BOTH cases. All 389 tests must pass before committing.

## Master Plan

See `docs/type system/parameterized-types-plan.md` for the full two-stage plan.
