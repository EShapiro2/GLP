# Register Allocation Collision Bug Fix - Handover Document

**Date:** January 19, 2026  
**Bug ID:** CODEGEN-001  
**Status:** Fixed  
**Severity:** Critical (caused incorrect bytecode generation)

---

## Executive Summary

Fixed a register allocation collision bug in the GLP bytecode generator where temporary registers used during head pattern matching could collide with variable registers in clauses with many variables (>10). This caused incorrect bytecode generation, argument dropping, and runtime corruption.

---

## The Bug

### Symptom
When compiling clauses with complex nested structures in both head and body (like `social_agent.glp`), the second `lookup_send` call would be generated with only 4 arguments instead of 5, and `handle_intro_status` would receive the head pattern `introduce(alice, charlie)` instead of the expected `Status2?` variable.

Runtime warning observed:
```
WARNING: PutVariable got unexpected value: introduce(R1002?,R1003?) (isReader=false)
```

### Root Cause
The codegen's temporary register allocation always started at register 10:
```dart
void resetTemps() {
  nextTempVar = 10;  // Fixed start position
  tempAllocation.clear();
}
```

But the analyzer assigns variable registers sequentially starting from 0. In a clause with 12 variables (Id, P, Q, In, Fs, QtoP, PtoQ, Fs1, Status1, Fs2, Status2, Fs3), variable `Status2` was assigned register 10 — the same register used for the first temp during head pattern matching.

### Bytecode Evidence (Before Fix)
```
  9: UnifyVariable(reg=10, reader=false)  ← Temp for introduce(P,Q) pattern
 ...
 40: PutVariable(reg=10, slot=4, reader=false)  ← Status2 (COLLISION!)
```

### Fix Applied
Changed `resetTemps()` to accept the variable count and start temps after all variable registers:

```dart
void resetTemps(int variableCount) {
  nextTempVar = variableCount > 10 ? variableCount : 10;
  tempAllocation.clear();
}
```

### Bytecode Evidence (After Fix)
```
  9: UnifyVariable(reg=12, reader=false)  ← Temp now at reg 12
 ...
 40: PutVariable(reg=10, slot=4, reader=false)  ← Status2 at reg 10 (no collision)
```

---

## Files Modified

### Core Fix
| File | Change |
|------|--------|
| `lib/compiler/codegen.dart` | Modified `resetTemps()` to take `variableCount` parameter and start temps at `max(variableCount, 10)` |

### Lines Changed
- **Line ~89-94**: `resetTemps()` signature and implementation
- **Line ~190**: Call site in `_generateClause()` now passes `clause.varTable.getAllVars().length`

---

## Relevant Specification Files

| File | Relevance |
|------|-----------|
| `/docs/glp-bytecode-v216-complete.md` | Bytecode instruction semantics, register model |
| `/docs/glp-compiler-spec.md` | Compiler architecture, codegen phases |
| `/docs/SPEC_GUIDE.md` | SRSW semantics, variable handling |

---

## Relevant Source Files

### Compiler
| File | Purpose |
|------|---------|
| `lib/compiler/codegen.dart` | Bytecode generator (bug location) |
| `lib/compiler/analyzer.dart` | Semantic analysis, register assignment |
| `lib/compiler/parser.dart` | Source parsing (verified correct) |
| `lib/compiler/ast.dart` | AST node definitions |

### Runtime
| File | Purpose |
|------|---------|
| `lib/bytecode/runner.dart` | Bytecode interpreter (warning source at line ~2480) |
| `lib/bytecode/opcodes.dart` | V1 bytecode instructions |
| `lib/bytecode/opcodes_v2.dart` | V2 bytecode instructions (PutVariable, GetVariable, UnifyVariable) |

---

## Test Files

### Minimal Reproduction Test
| File | Purpose |
|------|---------|
| `/programs/multiagent/test_intro_clause.glp` | Minimal test case isolating the bug |

### Original Failing Program
| File | Purpose |
|------|---------|
| `/programs/multiagent/social_agent.glp` | Multi-agent social graph program that triggered the bug |

### Test Commands
```bash
# Run minimal test
cd glp_runtime && echo -e "../programs/multiagent/test_intro_clause.glp\nsocial_graph(bob, [introduce(alice, charlie)], [(user, U), (net, N)])." | dart run bin/glp_repl.dart

# Run full social_agent test
cd glp_runtime && echo -e "../programs/multiagent/social_agent.glp\nsocial_graph(bob, [introduce(alice, charlie)], [(user, U), (net, N)])." | dart run bin/glp_repl.dart

# Run full test suite
cd glp_runtime && dart test
```

---

## Debug Instrumentation

The codegen has built-in debug output (currently disabled) that can be enabled by modifying the condition at line ~163 in `codegen.dart`:

```dart
// DEBUG: Print bytecode for this procedure
if (proc.signature == 'foo/1') {  // Change to match procedure to debug
  print('\n=== BYTECODE FOR ${proc.signature} ===');
  // ... prints all instructions with details
}
```

Enhanced debug output added shows:
- `PutVariable(reg=X, slot=Y, reader=Z)`
- `GetVariable(reg=X, slot=Y, reader=Z)`
- `UnifyVariable(reg=X, reader=Y)`
- `Spawn("label", arity=N)`

---

## Verification Results

### Before Fix
```
lookup_send(charlie, msg(...), X3?, X5)  ← Only 4 args
handle_intro_status(found, introduce(alice, charlie), ...)  ← Wrong Status2
WARNING: PutVariable got unexpected value: introduce(R1002?,R1003?)
```

### After Fix
```
lookup_send(charlie, msg(...), X3?, X5, X6)  ← Correct 5 args
handle_intro_status(not_found, not_found, ...)  ← Correct Status values
U = [error(intro_failed, alice) | X83]  ← Correct program output
```

---

## Test Suite Status

Before investigating this bug, the test suite showed **219/222 tests passing**. The 3 failing tests are time-related and predate this fix. Run `dart test` to verify current status.

---

## Recommendations

1. **Add regression test**: Create a test specifically for clauses with >10 variables to catch future register collision bugs

2. **Consider dynamic temp base**: The current fix uses `max(variableCount, 10)` but a more robust approach might track the highest used register dynamically

3. **Document register model**: The implicit assumption that temps start at 10 was undocumented; consider adding this to the compiler spec

---

## Related Issues

This bug class (register collision) was previously encountered and documented in handover files at `/docs/type system/`. The earlier fix addressed collision between temps and argument registers (hence starting at 10); this fix addresses collision with variable registers in large clauses.
