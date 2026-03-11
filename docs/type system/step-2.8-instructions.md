# Step 2.7 + 2.8: Documentation Update and Final Validation

**Date**: 2026-03-11

## Step 2.7: Commit Documentation Updates

The typed-glp-manual.md has been updated by Claude Chat:
- Added Section 18 (Tight Typing Discipline)
- Corrected Channel arity throughout (1-arg to 2-arg form)
- Updated Sections 4, 5, 8 with parameterized type examples
- Fixed Section 9.4 example
- Version bumped to 2.8

Commit: `fix(docs): typed-glp-manual v2.8 — tight typing discipline, correct Channel arity`

## Step 2.8: Final Validation

### 2.8a: Run full test suite

All 390 tests must pass.

### 2.8b: Verify tight typing discipline

Search all .glp files under programs/ (excluding archive/, OLD, book 2/) for type definitions or procedure declarations containing bare _ or _? as type arguments. Exempt: self.glp system builtins and documented meta-interpreter exceptions.

Report any violations found.

### 2.8c: Update current_plan.md

Mark Steps 2.7 and 2.8 as done.

### 2.8d: Push
