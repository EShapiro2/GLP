# Step 2.7 + 2.8: Documentation Update and Final Validation

**Date**: 2026-03-11
**Context**: Steps 2.4 and 2.5 are complete. typed-glp-manual.md has been updated by Claude Chat with Section 18 (Tight Typing Discipline), corrected Channel arity throughout, and Section 14 obsolescence note.

## Step 2.7: Commit Documentation Updates

The typed-glp-manual.md has already been edited. Review the changes, then commit:

```
fix(docs): update typed-glp-manual.md — Section 18 (tight typing discipline), correct Channel arity, update examples
```

## Step 2.8: Final Validation

### 2.8a: Run full test suite
```bash
bash test/run_all_tests.sh
```
All 390 tests must pass.

### 2.8b: Verify no user-mode files use `_` or `_?` in type definitions

Search all .glp files under `programs/` (excluding `archive/`, `OLD`, `book 2/`) for type definitions containing `_` or `_?`. Exclude:
- `programs/self.glp` (system builtins are exempt)
- Files that are documented meta-interpreter exceptions
- Comments

The search should find:
- Type definitions: lines matching `::=` that contain bare `_` as a type alternative or type argument
- Procedure declarations: lines matching `procedure` that contain bare `_` or `_?` as an argument type

For each finding, classify as:
1. **System builtin** in self.glp → exempt, OK
2. **Meta-interpreter** with comment → documented exception, OK
3. **Violation** → report for future cleanup (discipline-only, not blocking)

### 2.8c: Update current_plan.md

Mark Steps 2.7 and 2.8 as done. Note any remaining discipline violations found in 2.8b as future work.

### 2.8d: Commit and push

Commit the validation results and plan update. Push to origin.
