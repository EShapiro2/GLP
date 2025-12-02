# Spec Updates Report for Claude Web
**Date:** 2025-11-17
**Purpose:** Document spec clarifications made based on Claude Web's proposal

---

## Summary

Updated GLP specification documents to clarify variable occurrence tracking semantics for multi-structure HEAD patterns. These are **clarifications** of existing intended behavior, not semantic changes.

---

## Your Original Proposal

You correctly identified that the specs were **incomplete rather than incorrect** and proposed adding Section 8.3 to document multi-structure HEAD pattern occurrence tracking.

**Your key points:**
1. Variable occurrence tracking should span ALL structures in HEAD phase
2. "First occurrence" means first across all HEAD structures, not per-structure
3. The spec should explicitly state this to prevent compiler bugs

**Assessment:** ✅ **Completely correct**. Your analysis identified the exact gap in the specs.

---

## Changes Made to Your Proposal

### Refinements (Minor Improvements)

#### 1. Section Numbering
**Your proposal:** Add new Section 8.3
**Implemented:** Added as Section **8.6** (8.3-8.5 were already in use)

#### 2. Example Enhancement
**Your proposal:**
```prolog
clause(quicksort(X, Y?), qsort(X?, Y, []))
```

**Enhanced to:**
```prolog
clause(quicksort(X, Y?), qsort(X?, Y, []))

Variable Y appears twice in HEAD structures:
1. First occurrence: Y? in quicksort(X, Y?)
   → Emit: UnifyVariable(Yi, reader) (first occurrence of writer uses reader mode)
2. Second occurrence: Y in qsort(X?, Y, [])
   → Emit: UnifyValue(Yi) (subsequent occurrence uses value unification)
```

**Reason:** Made the expected bytecode explicit to help compiler implementers.

#### 3. Added Cross-References
**Your proposal:** Standalone section
**Enhancement:** Added explicit reference to Section 12 mode rules and back-reference from Section 12.0 to Section 8.6

**Reason:** Creates bidirectional navigation for readers approaching from either section.

#### 4. Compiler Implementation Guidance
**Your proposal:** General principle
**Enhancement:** Added explicit ❌/✅ examples:
```markdown
Compiler Implementation:
- ❌ Incorrect: Resetting occurrence tracking for each structure independently
- ✅ Correct: Maintaining shared seenHeadVars set across all HEAD structures
```

**Reason:** Makes the common bug pattern obvious to avoid.

---

## Files Updated

### 1. `docs/glp-bytecode-v216-complete.md`

**Addition 1 - New Section 8.6:**
- Location: After Section 8.5, before Section 9
- Content: Your proposed text with refinements listed above
- Purpose: Primary documentation of multi-structure HEAD occurrence tracking

**Addition 2 - Section 12.0 Enhancement:**
- Location: In Section 12.0 Overview, after Design Principles
- Content: New "CRITICAL - Occurrence Tracking Scope" subsection
- Purpose: Alert readers coming from mode-aware opcodes section
- Cross-reference: Points back to Section 8.6 for full explanation

### 2. `docs/glp-runtime-spec.txt`

**Addition - New Subsection:**
- Location: After "All head instructions are pure..." (line 468)
- Title: "Variable Occurrence Tracking During HEAD Phase"
- Content: Your proposed runtime spec text enhanced with:
  - Explicit tracking algorithm (3 steps)
  - More examples (simple, nested, multi-structure)
  - "Common Bug" warning paragraph
  - Metainterpreter-specific note

---

## Key Differences from Your Proposal

### What Stayed the Same ✅
- **Core principle**: Occurrence tracking spans ALL HEAD structures
- **Problem identification**: Resetting per-structure is the bug
- **Solution**: Shared seenHeadVars set
- **Target audience**: Compiler implementers

### What Changed 📝
1. **Section number**: 8.3 → 8.6 (due to existing content)
2. **Detail level**: Added explicit bytecode examples for clarity
3. **Navigation**: Added cross-references between Sections 8 and 12
4. **Visual markers**: Added ❌/✅ for correct/incorrect patterns
5. **Runtime spec**: Expanded with algorithm steps and more examples

### Why These Changes? 🎯
All changes serve to:
- Make the spec more **actionable** for implementers
- Provide **concrete examples** of correct bytecode
- **Prevent** the specific bug we found (compiler emitting wrong mode)
- Ensure **consistency** between bytecode and runtime specs

---

## Validation

The updated specs now clearly document that for:
```prolog
clause(quicksort(Unsorted, Sorted?), qsort(Unsorted?, Sorted, [])).
```

**Expected bytecode:**
```
PC 133: UnifyVariable(X1, reader)   # First occurrence of Sorted (as Sorted?)
PC 134: UnifyValue(X1)               # Second occurrence of Sorted
```

**Current buggy bytecode:**
```
PC 133: UnifyVariable(X1, writer)   # ❌ Bug: treats as first occurrence
```

The spec now makes it crystal clear that PC 133 should emit **reader mode** because it's the first occurrence of the variable Sorted, even though syntactically it appears as `Sorted?` (reader syntax).

---

## Impact on Compiler Fix

With these spec updates, when you ask Claude Web to fix the compiler:

1. **Clear specification**: The intended behavior is now unambiguous
2. **Example-driven**: Concrete bytecode examples show what to generate
3. **Bug pattern identified**: The ❌ incorrect pattern matches the current bug
4. **Implementation guidance**: The "shared seenHeadVars" approach is explicit

---

## Files to Upload to Claude Web

**Required:**
1. `glp_runtime/lib/compiler/codegen.dart` - contains the bug
2. `docs/glp-bytecode-v216-complete.md` - shows correct behavior (updated)
3. `udi/glp/qsort.glp` - test case (line 32)

**Optional but Helpful:**
4. `glp_runtime/lib/compiler/analyzer.dart` - variable tracking context
5. This report - shows what specs now say

---

## Conclusion

Your spec proposal was **fundamentally correct**. The changes made were:
- **Structural** (section numbering, cross-references)
- **Clarifying** (explicit examples, visual markers)
- **Completeness** (algorithm steps, more examples)

No changes to the **core semantics** you identified. The specs now clearly state that occurrence tracking must span all HEAD structures, with explicit examples of correct and incorrect compiler behavior.

**Status:** ✅ Specs updated and ready for compiler fix implementation.
