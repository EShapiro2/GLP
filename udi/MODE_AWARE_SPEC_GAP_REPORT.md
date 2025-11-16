# Mode-Aware Opcodes Specification Gap Report

**Date**: 2025-11-16
**Issue**: Mode-aware opcodes (GetReaderVariable, GetWriterVariable, GetReaderValue, GetWriterValue) are implemented but not documented in the authoritative bytecode spec.

---

## Problem Statement

The four mode-aware opcodes were implemented in commit 9780570 (Nov 15, 2025) and are actively used by the compiler (commit 7d186be), but the authoritative specification document `docs/glp-bytecode-v216-complete.md` contains NO documentation for these opcodes.

This violates the core principle: "never modify code without consulting the spec."

---

## Current State

### 1. Where Opcodes Are Defined

**File**: `glp_runtime/lib/bytecode/opcodes.dart` (lines 231-262)

```dart
// ===== MODE-AWARE argument loading (FCP-style) =====
/// Load argument into clause WRITER variable (first occurrence)
/// Clause expects writer, implements mode conversion if needed
class GetWriterVariable implements Op {
  final int varIndex;  // clause variable index
  final int argSlot;   // argument register
  GetWriterVariable(this.varIndex, this.argSlot);
}

/// Load argument into clause READER variable (first occurrence)
/// Clause expects reader, implements mode conversion if needed
class GetReaderVariable implements Op {
  final int varIndex;  // clause variable index
  final int argSlot;   // argument register
  GetReaderVariable(this.varIndex, this.argSlot);
}

/// Unify argument with clause WRITER variable (subsequent occurrence)
/// Performs writer MGU, updates σ̂w during HEAD phase
class GetWriterValue implements Op {
  final int varIndex;  // clause variable index
  final int argSlot;   // argument register
  GetWriterValue(this.varIndex, this.argSlot);
}

/// Unify argument with clause READER variable (subsequent occurrence)
/// Performs three-valued unification with reader semantics
class GetReaderValue implements Op {
  final int varIndex;  // clause variable index
  final int argSlot;   // argument register
  GetReaderValue(this.varIndex, this.argSlot);
}
```

**Comments are minimal and incomplete** - they don't specify:
- What "mode conversion" means
- When to bind in σ̂w vs suspend
- How to handle bound vs unbound arguments

### 2. Where Implementation Exists

**File**: `glp_runtime/lib/bytecode/runner.dart`

- GetWriterVariable: lines 913-938
- GetReaderVariable: lines 940-972
- GetWriterValue: lines 975-1072
- GetReaderValue: lines 1074-1109

**Implementation is ~250 lines of complex logic** with no formal spec to validate against.

### 3. Where Compiler Emits Them

**File**: `glp_runtime/lib/compiler/codegen.dart` (lines 215-231)

```dart
if (isFirstOccurrence) {
  // First occurrence: emit mode-aware opcode
  if (term.isReader) {
    ctx.emit(bc.GetReaderVariable(regIndex, argSlot));
  } else {
    ctx.emit(bc.GetWriterVariable(regIndex, argSlot));
  }
  ctx.seenHeadVars.add(baseVarName);
} else {
  // Subsequent occurrence: emit mode-aware value opcode
  if (term.isReader) {
    ctx.emit(bc.GetReaderValue(regIndex, argSlot));
  } else {
    ctx.emit(bc.GetWriterValue(regIndex, argSlot));
  }
}
```

Compiler actively uses these opcodes - they are NOT experimental.

### 4. Where Partial Spec Exists

**File**: `udi/FINAL_ANALYSIS_AND_PLAN.md` (lines 190-238)

Contains implementation pseudocode from the original design:

**GetReaderVariable** (line 190-212):
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

**GetWriterVariable** (line 215-237):
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

**Note from line 240-243**:
> 3. **Update spec** in `glp-bytecode-v216-complete.md`:
>    - Replace GetVariable/GetValue with four new opcodes
>    - Document mode conversion semantics
>    - Align with FCP terminology (we/ro)

**THIS STEP WAS NEVER COMPLETED.**

### 5. Where Spec Should Be (but isn't)

**File**: `docs/glp-bytecode-v216-complete.md`

**Current state**: Contains only old GetVariable/GetValue (section 12.1-12.2), which are deprecated.

**No mention of**:
- GetReaderVariable
- GetWriterVariable
- GetReaderValue
- GetWriterValue

**Search results**:
```bash
$ grep -i "GetReaderVariable\|GetWriterVariable" docs/glp-bytecode-v216-complete.md
(no results)
```

---

## Impact

### Current Bug Being Investigated

**Test case**: `helper(a, Y)` where `helper(X?, X).`

**Expected**: Y binds to 'a'
**Actual**: Y = <unbound>

**Root cause analysis blocked** because:
1. Cannot verify if current implementation matches spec (no spec exists)
2. Cannot determine if bug is in implementation or design (no design documented)
3. Cannot fix without risking breaking other code (no contract to validate against)

### Compiler Test Case

**Example bytecode** for `helper(X?, X)`:
```
0: Label
1: ClauseTry
2: GetReaderVariable(varIndex=0, argSlot=0)  // X? - first occurrence
3: GetWriterValue(varIndex=0, argSlot=1)     // X - second occurrence
4: Commit
5: Proceed
```

**Question**: What should happen when called with `helper(a, Y)`?
- A0 = writer(a) bound to 'a'
- A1 = writer(Y) unbound

**Cannot answer without spec.**

---

## Specific Questions Requiring Spec Clarification

### GetReaderVariable: Mode Conversion (Writer → Reader)

When clause expects reader (X?) but argument is bound writer:

1. **Should the fresh variable be bound to the writer's value immediately?**
   - Current pseudocode (FINAL_ANALYSIS_AND_PLAN.md) does NOT bind it
   - Current implementation (runner.dart) I added binding (lines 955-961) but this may be wrong
   - What is the correct FCP semantics?

2. **What goes in σ̂w?**
   - Current: `σ̂w[arg.writerId] = VarRef(freshVar, isReader: true)`
   - Is this creating a reader-of-reader chain problem?
   - Should we bind the writer to the fresh var's value instead?

3. **Example case**: `helper(a, Y)` where `helper(X?, X)`
   - A0 = writer(a) bound to ConstTerm('a')
   - GetReaderVariable creates fresh=1000
   - Should fresh=1000 be bound to ConstTerm('a') immediately?
   - Or does binding happen only at Commit?

### GetWriterValue: Subsequent Occurrence

When clause variable was loaded by GetReaderVariable (contains freshVar ID):

1. **How to unify with writer argument?**
   - storedValue = freshVar (int ID)
   - arg = writer(Y) unbound
   - Current code fails because Y != freshVar
   - Should we bind Y to freshVar in σ̂w?
   - Or bind Y to the VALUE of freshVar?

2. **What if freshVar is bound vs unbound?**
   - If GetReaderVariable bound it: use that value?
   - If GetReaderVariable left it unbound: bind in σ̂w?

### GetReaderValue: Subsequent Occurrence of Reader

Current implementation (lines 1074-1109) but unclear:

1. **When does this opcode get emitted?**
   - Can a reader appear twice in HEAD? (SRSW violation unless guarded)
   - Is this for guards only?

2. **What are the mode conversion cases?**
   - Reader arg, reader param: verify same variable
   - Writer arg, reader param: ???
   - Bound term arg, reader param: ???

### GetWriterVariable: Mode Conversion (Reader → Writer)

Current pseudocode says:
```dart
if (arg.isReader) {
  if (wid != null && cx.rt.heap.isWriterBound(wid)) {
    final value = cx.rt.heap.valueOfWriter(wid);
    cx.clauseVars[op.varIndex] = value;  // Store VALUE not ID
  } else {
    cx.si.add(arg.readerId!);  // Suspend
  }
}
```

**Questions**:
1. Why store value instead of writer ID?
2. How does this interact with GetWriterValue later?
3. Is this correct FCP semantics?

---

## FCP Reference (from FINAL_ANALYSIS_AND_PLAN.md)

FCP uses mode-based opcodes:
```c
#define load_we_var             0x010  // Write-Enabled (writer)
#define load_ref_to_we_var      0x011  // Reference to writer
#define load_ro_of_reg          0x012  // Read-Only (reader) of register
#define load_ref_to_ro_of_reg   0x013  // Reference to reader
```

**FCP Source Files**:
- Local: `/Users/udi/Dropbox/Concurrent Prolog/FCP/Savannah`
- GitHub: https://github.com/EShapiro2/FCP
- Paper: `/Users/udi/GLP/docs/1-s2.0-0743106689900113-main.pdf`

**Needed**: Mapping from FCP opcodes to GLP opcodes with precise semantics.

---

## Required Spec Content

### Section Location

Add new section in `docs/glp-bytecode-v216-complete.md`:

**Suggested placement**: After section 11 (Guards), before section 12 (System Instructions)

**New section**: "Section 12: Mode-Aware Argument Loading (FCP-style)"

Move old GetVariable/GetValue to deprecated section or remove entirely.

### Minimum Required Documentation

For each of the 4 opcodes:

1. **Operation**: One-line description
2. **Syntax**: `opcode varIndex, argSlot`
3. **Behavior**: What happens in each case:
   - Argument is writer (bound vs unbound)
   - Argument is reader (bound vs unbound)
   - Argument is known term
4. **Mode Conversion**: Explicit algorithm for mode mismatch
5. **σ̂w Updates**: When to bind, what to bind, how to bind
6. **Suspension**: When to add to Si
7. **Example**: Simple clause showing compilation and execution

### Critical Design Questions

Must be answered in spec:

1. **When writer arg bound, reader expected**:
   - Bind fresh var immediately or wait for Commit?

2. **Fresh variable identity**:
   - How to track relationship between caller's writer and fresh reader view?

3. **GetWriterValue with freshVar**:
   - How to propagate bindings through mode conversion boundary?

4. **Commit semantics**:
   - What happens to VarRef(freshVar, isReader: true) bindings in σ̂w?

5. **Reader-of-reader chains**:
   - Are they allowed? How to prevent?

---

## Proposed Action

**For Claude Web**:

1. Review FINAL_ANALYSIS_AND_PLAN.md pseudocode (lines 190-238)
2. Consult FCP implementation for correct semantics
3. Write complete specification for all 4 opcodes
4. Add to `docs/glp-bytecode-v216-complete.md` as authoritative spec
5. Validate implementation in runner.dart matches spec
6. Fix any discrepancies

**Until spec is complete**:
- Cannot proceed with bug fix
- Cannot validate current implementation
- Cannot make informed code changes

---

## Git History

Relevant commits:
- `9780570` - feat: Implement mode-aware argument loading opcodes (Nov 15, 2025)
- `7d186be` - feat: Emit mode-aware argument loading opcodes (Nov 15, 2025)
- Both reference FINAL_ANALYSIS_AND_PLAN.md
- Both claim "approved by Claude Web"
- Neither updated the authoritative spec in docs/

Commit message 9780570 says:
> Based on FINAL_ANALYSIS_AND_PLAN.md and approved by Claude Web.

But FINAL_ANALYSIS_AND_PLAN.md line 240-243 explicitly requires:
> 3. **Update spec** in `glp-bytecode-v216-complete.md`

**This was never done.**

---

## Summary

The mode-aware opcodes are production code (compiler emits them, runtime executes them, tests depend on them) but have NO authoritative specification. This makes it impossible to:

1. Fix bugs correctly
2. Validate implementation
3. Maintain code quality
4. Follow "spec-first" development principle

**Blocking bug**: `helper(a, Y)` returns unbound instead of 'a'
**Cannot fix until**: Spec defines mode conversion semantics

**Request**: Claude Web to write complete spec in docs/glp-bytecode-v216-complete.md
