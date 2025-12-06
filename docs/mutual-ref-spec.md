# MutualRef Specification

## 1. Overview

MutualRef enables O(1) stream append for multiway merge (mwm). Instead of traversing a stream to find its tail, MutualRef maintains a mutable pointer to the current tail, allowing constant-time append operations.

**Use Case:** Multiway merge of streams with constant delay per element.

## 2. MutualRefTerm Class

**File:** `glp_runtime/lib/runtime/terms.dart`

```dart
class MutualRefTerm implements Term {
  /// Current stream tail (mutable, updated by stream_append)
  WriterTerm current;

  /// Unique ID for debugging
  final int id;

  MutualRefTerm(this.current, this.id);

  @override
  String toString() => 'MutualRef#$id';
}
```

**Properties:**
- `current`: Mutable pointer to the current unbound writer (stream tail)
- `id`: Unique identifier for debugging/tracing

## 3. Guard: `is_mutual_ref/1`

**Semantics:**
- `is_mutual_ref(X?)` where X? bound to MutualRefTerm → **succeed**
- `is_mutual_ref(X?)` where X? bound to other term → **fail**
- `is_mutual_ref(X?)` where X? unbound → **fail** (NOT suspend)

**Implementation:**
```dart
case 'is_mutual_ref':
  if (args.isEmpty) return GuardResult.failure;
  final arg = args[0];
  final value = dereference(arg);
  if (value is MutualRefTerm) {
    return GuardResult.success;
  }
  return GuardResult.failure;  // Both wrong-type and unbound → fail
```

**SRSW Relaxation Effect:**
When `is_mutual_ref(X?)` appears in guards, variable `X` may have multiple reader occurrences in the clause body (same treatment as `ground(X?)`).

## 4. SRSW Analyzer Modification

**File:** `glp_runtime/lib/compiler/analyzer.dart`

`is_mutual_ref(X?)` permits multiple `X?` occurrences in body:

```dart
// In guard analysis section
if (guard.functor == 'is_mutual_ref' || guard.functor == 'ground') {
  if (guard.args[0] is ReaderVar) {
    relaxedVars.add(guard.args[0].name);
  }
}
```

## 5. Predicates

### 5.1 `allocate_mutual_reference/2` (Kernel)

**No GLP clause wrapper.** Called directly from GLP code with two writers.

**Signature:** `allocate_mutual_reference(Ref, Output)`
- `Ref`: Writer - will be bound to a fresh MutualRefTerm
- `Output`: Writer - the stream tail that MutualRef will track

**Implementation:**
```dart
void allocateMutualReference(WriterTerm refWriter, WriterTerm output) {
  int id = nextMutualRefId++;
  MutualRefTerm mutRef = MutualRefTerm(output, id);
  heap.bind(refWriter, mutRef);
}
```

### 5.2 `stream_append/3` (GLP wrapper + kernel)

**GLP Wrapper:**
```glp
stream_append(Value, RefIn, RefOut?) :-
  is_mutual_ref(RefIn?) |
  kernel_stream_append(Value?, RefIn?, RefOut).
```

**Kernel:** `kernel_stream_append/3`
```dart
void kernelStreamAppend(Term value, MutualRefTerm ref, WriterTerm refOut) {
  WriterTerm tail = ref.current;

  if (heap.isBound(tail)) {
    throw 'stream_append: tail already bound';
  }

  WriterTerm newTail = heap.freshWriter();
  ReaderTerm tailReader = ReaderTerm(newTail);
  ListTerm cell = ListTerm(value, tailReader);

  heap.bind(tail, cell);
  ref.current = newTail;  // Destructive update
  heap.bind(refOut, ref);
}
```

**Operation:**
1. Get current tail from MutualRef
2. Create new tail writer
3. Bind current tail to `[Value|NewTail?]`
4. Update MutualRef.current to new tail (destructive update)
5. Bind RefOut to the updated MutualRef

### 5.3 `close_mutual_reference/1` (GLP wrapper + kernel)

**GLP Wrapper:**
```glp
close_mutual_reference(Ref?) :-
  is_mutual_ref(Ref?) |
  kernel_close_mutual_reference(Ref?).
```

**Kernel:** `kernel_close_mutual_reference/1`
```dart
void kernelCloseMutualReference(MutualRefTerm ref) {
  WriterTerm tail = ref.current;
  if (!heap.isBound(tail)) {
    heap.bind(tail, emptyList);
  }
}
```

## 6. System Predicate: `mwm/2`

**File:** `glp/stdlib/mwm.glp`

```glp
% mwm/2 - Multiway merge with constant delay
% In: stream of stream(Xs) terms, may include merge(NewStream)
% Out: merged output stream
mwm(In, Out?) :-
    allocate_mutual_reference(Ref, Out),
    mwm1(In?, Ref?).

mwm1([stream(Xs)|Streams], Ref?) :-
    is_mutual_ref(Ref?) |
    copy(Xs?, Ref?),
    mwm1(Streams?, Ref?).

mwm1([merge(NewIn)|Streams], Ref?) :-
    is_mutual_ref(Ref?) |
    mwm1(NewIn?, Ref?),
    mwm1(Streams?, Ref?).

mwm1([], Ref?) :-
    close_mutual_reference(Ref?).

copy([X|Xs], Ref?) :-
    is_mutual_ref(Ref?) |
    stream_append(X?, Ref?, Ref1),
    copy(Xs?, Ref1?).

copy([], _).
```

**Pattern Explanation:**
- `Out?` in head reads the goal's output variable (an unbound writer)
- `Out` in body passes that writer to `allocate_mutual_reference`
- This is valid SRSW: 1 reader + 1 writer for `Out`

## 7. Implementation Checklist

- [ ] Add `MutualRefTerm` class to `terms.dart`
- [ ] Add `is_mutual_ref/1` guard handler
- [ ] Add `allocate_mutual_reference/2` kernel predicate
- [ ] Add `kernel_stream_append/3` kernel predicate
- [ ] Add `kernel_close_mutual_reference/1` kernel predicate
- [ ] Update SRSW analyzer to recognize `is_mutual_ref/1`
- [ ] Create `stdlib/mwm.glp`
- [ ] Add unit tests
- [ ] Add REPL tests

## 8. Example Usage

```glp
% Merge two streams using mwm
test_mwm(Out) :-
    mwm([stream([a,b,c]), stream([1,2,3])], Out).

% Result: Out = some fair interleaving of [a,b,c] and [1,2,3]
% e.g., [a,1,b,2,c,3] or similar
```

```glp
% Dynamic merge - add new stream mid-merge
test_dynamic(Out) :-
    mwm([stream([a,b]), merge([stream([x,y])]), stream([1,2])], Out).
```
