# Heap Duplication Bug Report

**Date:** November 11, 2025
**Severity:** CRITICAL ARCHITECTURE ISSUE
**Impact:** Inconsistent data between old and new heap implementations

---

## The Problem

The codebase has **two heap implementations** storing data simultaneously with **inconsistent values**:

1. **`lib/runtime/heap.dart`** - Original heap (62 lines)
2. **`lib/runtime/heap_v2.dart`** - New single-ID heap design
3. **`lib/runtime/heap_v2_adapter.dart`** - Adapter that inherits from BOTH

---

## The Architecture

```
HeapV2Adapter extends Heap {
  final HeapV2 _v2;

  void bindWriterStruct(writerId, f, args) {
    super.bindWriterStruct(writerId, f, args);  // Store in OLD heap
    _v2.bindVariableStruct(varId, f, v2Args);    // Store in NEW heap
  }
}
```

**Every binding is stored TWICE** - once in each heap!

---

## The Bug

### Old Heap (`heap.dart` lines 39-53):
```dart
void bindWriterStruct(int writerId, String f, List<Term> args) {
  // Dereference any ReaderTerms in args before storing
  final dereferencedArgs = args.map((arg) {
    if (arg is ReaderTerm) {
      final wid = writerIdForReader(arg.readerId);
      if (wid != null && isWriterBound(wid)) {
        return writerValue[wid]!;  // ← DEREFERENCES ReaderTerm!
      }
    }
    return arg;
  }).toList();

  writerValue[writerId] = StructTerm(f, dereferencedArgs);
}
```

**Result:** Old heap stores **dereferenced** values.

### HeapV2Adapter (`heap_v2_adapter.dart` lines 149-160):
```dart
void bindWriterStruct(int writerId, String f, List<Term> args) {
  // CRITICAL: Always update parent first for compatibility
  super.bindWriterStruct(writerId, f, args);  // ← Calls old heap (dereferences)

  // Then update V2 if this writer is mapped
  final varId = _writerToVar[writerId];
  if (varId != null) {
    // Convert args from old format to V2 format
    final v2Args = args.map((arg) => _convertToV2(arg)).toList();  // ← Uses ORIGINAL args!
    _v2.bindVariableStruct(varId, f, v2Args);
  }
}
```

**BUG:** Line 157 converts the **ORIGINAL args**, not the dereferenced ones!

### HeapV2 (`heap_v2.dart` lines 52-54):
```dart
void bindVariableStruct(int varId, String f, List<Term> args) {
  bindVariable(varId, StructTerm(f, args));  // ← Stores args as-is
}
```

**Result:** V2 heap stores **non-dereferenced** ReaderTerms!

---

## The Consequences

### Data Inconsistency
```
Example: bindWriterStruct(1002, ",", [ReaderTerm(1005), ReaderTerm(1009)])

Old Heap stores:
  writer[1002] = StructTerm(",", [StructTerm("bar", ...), StructTerm("baz", ...)])
  ↑ ReaderTerms dereferenced to actual structures

V2 Heap stores:
  var[1002] = StructTerm(",", [VarRef(1005, true), VarRef(1009, true)])
  ↑ ReaderTerms converted but NOT dereferenced
```

### Which Heap is Used?
- `runner.dart` calls `cx.rt.heap.isWriterBound()` → Uses adapter methods
- Adapter methods check V2 first in some cases, old heap in others
- **Inconsistent behavior depending on which heap answers the query!**

### Performance Impact
- Double memory usage
- Double CPU work on every binding
- Wasted cycles maintaining two data structures

---

## The Fix Options

### Option A: Fix the Adapter (Immediate)
```dart
void bindWriterStruct(int writerId, String f, List<Term> args) {
  // Call parent to get dereferenced version
  super.bindWriterStruct(writerId, f, args);

  // Get the dereferenced version from parent
  final dereferencedStruct = super.valueOfWriter(writerId) as StructTerm;

  // Then update V2 with DEREFERENCED args
  final varId = _writerToVar[writerId];
  if (varId != null) {
    final v2Args = dereferencedStruct.args.map((arg) => _convertToV2(arg)).toList();
    _v2.bindVariableStruct(varId, f, v2Args);
  }
}
```

### Option B: Refactor Adapter to Not Inherit (Better)
```dart
class HeapV2Adapter implements Heap {  // ← implements, not extends
  final HeapV2 _v2;

  // Implement all Heap methods by delegating to _v2
  // No duplicate storage!
}
```

### Option C: Remove Old Heap Entirely (Best Long-term)
1. Make all code use HeapV2 directly
2. Update runner.dart to use single-ID variable system
3. Delete heap.dart
4. Delete heap_v2_adapter.dart

---

## Impact on Current Bug

The nested structure bug we're debugging may be affected by this:

**Current behavior:**
- Conjunction bound in old heap with dereferenced ReaderTerms ✓
- Conjunction bound in V2 heap with NON-dereferenced VarRefs ✗
- When runtime reads the value, which heap does it use?

This could explain why `Z = <unbound>` - the runtime might be reading from V2 heap which has unresolved VarRefs!

---

## Immediate Action Required

1. **Fix the adapter** to dereference before storing in V2
2. **Add tests** to verify both heaps contain identical data
3. **Plan migration** to single heap implementation

---

## Test to Verify

```dart
test('heap consistency', () {
  final rt = GlpRuntime();
  final (wid1, rid1) = rt.heap.allocateFreshPair();
  final (wid2, rid2) = rt.heap.allocateFreshPair();

  rt.heap.bindWriterConst(wid1, 'a');
  rt.heap.bindWriterStruct(wid2, 'foo', [ReaderTerm(rid1)]);

  // Old heap should have: foo(ConstTerm('a'))
  // V2 heap should have:  foo(VarRef(...)) pointing to bound 'a'
  // Both should effectively represent foo(a)

  final value = rt.heap.valueOfWriter(wid2);
  print('Value: $value');
  // Should be: StructTerm('foo', [ConstTerm('a')])
  // NOT: StructTerm('foo', [VarRef(...)])
});
```

---

## Files to Fix

1. **`lib/runtime/heap_v2_adapter.dart`** - Fix `bindWriterStruct` to use dereferenced args
2. **Add tests** - Verify heap consistency

---

## Related Issues

- Nested structure bug (may be caused or worsened by this)
- Memory usage (double storage)
- Performance (double work)
- Maintainability (duplicate code, duplicate logic)

---

*This is a critical architectural issue that needs immediate attention.*
