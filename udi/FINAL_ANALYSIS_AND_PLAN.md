# Final Analysis: GLP Bytecode Architecture Issue

## Executive Summary

The GLP bytecode instruction set has a **fundamental architectural flaw**: it uses WAM-style occurrence-based encoding (GetVariable/GetValue for first/subsequent occurrences) instead of mode-based encoding (separate instructions for readers vs writers). This makes it impossible to correctly implement SRSW semantics for patterns like `qsort([], Rest?, Rest)` where a reader appears before its paired writer.

---

## The Problem

### Test Case
```prolog
qsort([ ], Rest?, Rest).
```

Called with: `qsort([],X,[])` where X=W1017

**Expected**: Match succeeds, X binds to []
**Actual**: Match fails

### Root Cause

**Current bytecode** (occurrence-based):
```
PC 40: HeadNil           # Match A0 = []
PC 41: GetVariable       # First occurrence of Rest (which is Rest?)
PC 42: GetValue          # Second occurrence of Rest (which is Rest)
PC 43: Commit
```

**Problem**:
- `GetVariable` has NO way to know if it's loading a reader (Rest?) or writer (Rest)
- It only knows "this is the first occurrence"
- Cannot implement mode conversion (writer arg → reader view)

---

## FCP Reference Architecture

FCP AM uses **mode-based opcodes**:

```c
#define load_we_var             0x010  // Write-Enabled (writer)
#define load_ref_to_we_var      0x011  // Reference to writer
#define load_ro_of_reg          0x012  // Read-Only (reader) of register
#define load_ref_to_ro_of_reg   0x013  // Reference to reader
```

FCP distinguishes:
- **we** = write-enabled = writer variable
- **ro** = read-only = reader variable

**For `qsort([], Rest?, Rest)`, FCP would emit**:
```
load_ro_of_reg   # For Rest? (reader)
load_we_var      # For Rest (writer)
```

---

## Current GLP Architecture Analysis

### Opcodes Inventory

**For top-level arguments** (the problem area):
- `GetVariable(varIndex, argSlot)` - First occurrence, NO mode flag
- `GetValue(varIndex, argSlot)` - Subsequent occurrence, NO mode flag

**For structure subterms** (working correctly):
- `HeadWriter(varIndex)` - Writer in structure
- `HeadReader(varIndex)` - Reader in structure
- `UnifyWriter(varIndex, isFirstOccurrence)` - Writer unification
- `UnifyReader(varIndex)` - Reader unification

**The inconsistency**: Structure operations distinguish readers/writers, but top-level argument operations don't!

### Why This Fails

For clause `qsort([], Rest?, Rest)` with query `qsort([],X,[])`:

1. **PC 41: GetVariable(Rest, A1)**
   - A1 contains W1017 (unbound writer)
   - GetVariable doesn't know Rest is accessed as **reader** (Rest?)
   - Just stores W1017 in clauseVars[Rest]
   - **WRONG**: Should allocate fresh variable and bind W1017 to reader of it

2. **PC 42: GetValue(Rest, A2)**
   - A2 contains R1001 (reader bound to `[]`)
   - storedValue = W1017
   - Current code fails because it doesn't know how to unify writer with bound reader
   - **EVEN IF FIXED**: Can't work because PC 41 already did the wrong thing

---

## Required Semantics (Validated Against User Requirements)

### When Clause Expects Reader, Argument Provides Writer

**Clause**: `p(X?, ...)`
**Goal**: `p(W1017, ...)`  where W1017 is unbound writer

**Correct behavior** (mode conversion):
1. Allocate fresh variable W_new
2. Add to σ̂w: `W1017 → VarRef(W_new, isReader: true)`
3. Store W_new in clauseVars for subsequent use
4. At commit: W1017 gets bound to reader view of W_new

**This requires knowing**: The clause variable is a READER, not just "first occurrence"

---

## Solution Options

### Option 1: Add Mode Flag to Existing Opcodes (Minimal Change)

**Modify opcodes**:
```dart
class GetVariable implements Op {
  final int varIndex;
  final int argSlot;
  final bool isReader;  // NEW: indicates if clause variable is reader
  GetVariable(this.varIndex, this.argSlot, {this.isReader = false});
}

class GetValue implements Op {
  final int varIndex;
  final int argSlot;
  final bool isReader;  // NEW
  GetValue(this.varIndex, this.argSlot, {this.isReader = false});
}
```

**Pros**:
- Minimal disruption to existing code
- Backward compatible (defaults to writer mode)

**Cons**:
- Less explicit than separate opcodes
- Mixing two concepts (occurrence + mode)

### Option 2: Create Separate Opcodes (FCP-style, Recommended)

**New opcodes**:
```dart
// First occurrence
class GetWriterVariable implements Op {
  final int varIndex;
  final int argSlot;
}

class GetReaderVariable implements Op {
  final int varIndex;
  final int argSlot;
}

// Subsequent occurrence
class GetWriterValue implements Op {
  final int varIndex;
  final int argSlot;
}

class GetReaderValue implements Op {
  final int varIndex;
  final int argSlot;
}
```

**Pros**:
- Explicit and clear
- Matches FCP architecture (proven design)
- Easier to implement correct semantics per opcode
- Follows existing pattern (HeadWriter/HeadReader for structures)

**Cons**:
- More opcodes (4 instead of 2)
- Requires compiler changes

---

## Implementation Plan

### Phase 1: Opcodes and Runtime (Claude Code)

1. **Add new opcodes** to `opcodes.dart`:
   - GetWriterVariable, GetReaderVariable
   - GetWriterValue, GetReaderValue

2. **Implement handlers** in `runner.dart`:

   **GetReaderVariable** (clause expects reader, first occurrence):
   ```dart
   if (op is GetReaderVariable) {
     final arg = _getArg(cx, op.argSlot);
     if (arg == null) { fail; }

     if (arg.isWriter) {
       // Mode conversion: writer arg → reader view
       final freshVar = cx.rt.heap.allocateFreshVar();
       cx.rt.heap.addVariable(freshVar);
       cx.sigmaHat[arg.writerId!] = VarRef(freshVar, isReader: true);
       cx.clauseVars[op.varIndex] = freshVar;
     } else if (arg.isReader) {
       // Reader to reader - store directly
       cx.clauseVars[op.varIndex] = arg.readerId!;
     } else if (arg.isKnown) {
       // Known term - allocate variable, bind in sigmaHat
       final freshVar = cx.rt.heap.allocateFreshVar();
       cx.rt.heap.addVariable(freshVar);
       cx.sigmaHat[freshVar] = arg.knownTerm!;
       cx.clauseVars[op.varIndex] = freshVar;
     }
   }
   ```

   **GetWriterVariable** (clause expects writer, first occurrence):
   ```dart
   if (op is GetWriterVariable) {
     final arg = _getArg(cx, op.argSlot);
     if (arg == null) { fail; }

     if (arg.isWriter) {
       cx.clauseVars[op.varIndex] = arg.writerId!;
     } else if (arg.isReader) {
       // Reader to writer - dereference and check bound
       final wid = cx.rt.heap.writerIdForReader(arg.readerId!);
       if (wid != null && cx.rt.heap.isWriterBound(wid)) {
         final value = cx.rt.heap.valueOfWriter(wid);
         // Store value for subsequent unification
         cx.clauseVars[op.varIndex] = value;
       } else {
         // Unbound reader - suspend
         cx.si.add(arg.readerId!);
       }
     } else if (arg.isKnown) {
       cx.clauseVars[op.varIndex] = arg.knownTerm!;
     }
   }
   ```

3. **Update spec** in `glp-bytecode-v216-complete.md`:
   - Replace GetVariable/GetValue with four new opcodes
   - Document mode conversion semantics
   - Align with FCP terminology (we/ro)

4. **Test** with existing tests:
   - Should maintain 86 passing (no regressions)
   - Old GetVariable/GetValue still work for backward compat

### Phase 2: Compiler Changes (Claude Web)

**Required compiler modifications**:

1. **Track variable modes** during parsing:
   - Distinguish `X` (writer) from `X?` (reader) in clause heads
   - Maintain mode information through AST

2. **Emit mode-specific opcodes**:
   - For `Rest?` (reader): emit GetReaderVariable
   - For `Rest` (writer): emit GetWriterVariable
   - Track first/subsequent occurrences per variable

3. **Example transformation**:
   ```prolog
   qsort([], Rest?, Rest).
   ```

   **Should compile to**:
   ```
   PC 40: HeadNil           # A0
   PC 41: GetReaderVariable # A1 → Rest? (reader, first)
   PC 42: GetWriterValue    # A2 → Rest (writer, subsequent)
   PC 43: Commit
   ```

### Phase 3: Testing and Validation

1. **Unit tests** for mode conversion:
   - Reader before writer: `p(X?, X)`
   - Writer before reader: `p(X, X?)`
   - Multiple readers: `p(X?, X?, X)` (should fail SRSW)

2. **Integration test**:
   - `qsort([],X,[])` should succeed with X=[]

3. **Existing tests**:
   - All 86 passing tests should continue to pass
   - No regressions in SRSW validation

---

## Migration Strategy

### Backward Compatibility

**Keep old opcodes** with default semantics:
- `GetVariable` → treat as GetWriterVariable (most common case)
- `GetValue` → treat as GetWriterValue

**Add deprecation warnings**:
- Log when old opcodes are used
- Guide migration to new opcodes

### Phased Rollout

1. **Phase 1** (This session): Runtime support for new opcodes
2. **Phase 2** (Next session): Compiler emits new opcodes for new code
3. **Phase 3** (Future): Migrate existing .glp files, remove old opcodes

---

## Files to Modify

### Runtime (Claude Code - This Session)

1. **`glp_runtime/lib/bytecode/opcodes.dart`**
   - Add 4 new opcode classes
   - Keep old GetVariable/GetValue for compatibility

2. **`glp_runtime/lib/bytecode/runner.dart`**
   - Implement handlers for 4 new opcodes
   - Add mode conversion logic
   - Update old handlers with deprecation warnings

3. **`docs/glp-bytecode-v216-complete.md`**
   - Replace Section 12 (System Instructions)
   - Document new opcodes with FCP reference
   - Add mode conversion semantics

### Compiler (Claude Web - Next Session)

4. **`glp_runtime/lib/compiler/analyzer.dart`**
   - Track variable modes (reader/writer) during analysis
   - Extend VariableInfo with mode tracking

5. **`glp_runtime/lib/compiler/codegen.dart`**
   - Emit GetReaderVariable vs GetWriterVariable
   - Emit GetReaderValue vs GetWriterValue
   - Track first/subsequent per variable

---

## Success Criteria

### Immediate (This Session)
- [ ] New opcodes defined
- [ ] Runtime handlers implemented
- [ ] Spec updated
- [ ] Tests pass (86/88)

### Next Session (Compiler)
- [ ] Compiler emits new opcodes
- [ ] `qsort([],X,[])` succeeds
- [ ] No SRSW false positives

### Long Term
- [ ] All .glp files migrated
- [ ] Old opcodes deprecated and removed
- [ ] Full FCP AM parity for argument handling

---

## Risk Assessment

**Low risk**:
- Adding new opcodes doesn't break existing code
- Old opcodes continue to work
- Incremental migration path

**Medium risk**:
- Compiler changes touch critical path
- Need comprehensive testing

**Mitigation**:
- Keep old opcodes functional
- Add extensive unit tests
- Validate against FCP behavior

---

## Recommendation

**Proceed with Option 2 (Separate Opcodes)**:
1. Aligns with proven FCP architecture
2. Clearer semantics than mode flags
3. Follows existing GLP pattern (HeadWriter/HeadReader)
4. Low risk with backward compatibility

**This session**: Implement runtime support
**Next session**: Update compiler with Claude Web

---

## References

- FCP AM Source: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah/Logix/EMULATOR/opcodes.h`
- FCP Paper: `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf`
- Current spec: `/Users/udi/GLP/docs/glp-bytecode-v216-complete.md`
- Previous report: `/Users/udi/GLP/udi/COMPREHENSIVE_SESSION_REPORT.md`
