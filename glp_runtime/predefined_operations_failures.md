# Predefined Operations Test Failures

**Status:** 5 passing, 17 failing
**All failures:** Tests expect `isWellTyped = true` but get `false`

---

## 1. Self-Duality Failure

**Test:** "POSITIVE: Any and Any? are equivalent"
**Location:** test/analysis/type_checker/predefined_operations_test.dart:15

```dart
test('POSITIVE: Any and Any? are equivalent', () {
  final result = checkTypes('''
    MyEvery ::= _ ; _?.
    MyAny ::< MyEvery.
    procedure foo(MyAny, MyAny?).
    foo(X, X?).
  ''');
  expect(result.isWellTyped, isTrue,
      reason: 'Any = Any? by self-duality');
});
```

**Expected:** Type checking should pass because:
- `MyAny` is a subtype of `MyEvery` (which is self-dual: `_ ; _?`)
- By self-duality, `MyAny` and `MyAny?` should be equivalent
- The clause `foo(X, X?)` should satisfy the procedure signature `foo(MyAny, MyAny?)`

**Actual:** `isWellTyped = false`

**Similar failures in this category:**
- "Every and Every? are equivalent"
- "Writer at Any position is valid"
- "Reader at Any position is valid"
- "List with Any elements needs only two clauses"
- "Every with both modes covered"

---

## 2. DiffList Failure

**Test:** "POSITIVE: dl_append is well-moded"
**Location:** test/analysis/type_checker/predefined_operations_test.dart:97

```dart
test('POSITIVE: dl_append is well-moded', () {
  final result = checkTypes('''
    MyEvery ::= _ ; _?.
    MyAny ::< MyEvery.
    MyList ::= [MyAny | MyList] ; [].
    MyDiffList ::= MyList \\\\ MyList?.

    procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
    my_dl_append(A\\\\B?, B\\\\C?, A?\\\\C).
  ''');
  expect(result.isWellTyped, isTrue,
      reason: 'dl_append has correct mode annotations');
});
```

**Expected:** Type checking should pass because:
- `MyDiffList ::= MyList \ MyList?` defines difference list as `List \ List?`
- The clause `my_dl_append(A\\B?, B\\C?, A?\\C)` should match the signature
- The mode annotations follow the O(1) concatenation pattern:
  - First arg input: `A\\B?` (A is writer, B? is reader)
  - Second arg input: `B\\C?` (B is writer, C? is reader)
  - Third arg output: `A?\\C` (A? is reader, C is writer)
  - The hole `B`/`B?` threads through correctly

**Actual:** `isWellTyped = false`

**Similar failures in this category:**
- "dl_to_list is well-moded"
- "dl_append demonstrates O(1) concatenation"

---

## 3. Channel Failure

**Test:** "POSITIVE: send is well-moded"
**Location:** test/analysis/type_checker/predefined_operations_test.dart:181

```dart
test('POSITIVE: send is well-moded', () {
  final result = checkTypes('''
    MyEvery ::= _ ; _?.
    MyAny ::< MyEvery.
    MyList ::= [MyAny | MyList] ; [].
    MyStream ::< MyList.
    MyChannel ::= ch(MyStream?, MyStream).

    procedure my_send(MyAny, MyChannel?, MyChannel).
    my_send(X, ch(In, [X?|Out?]), ch(In?, Out)).
  ''');
  expect(result.isWellTyped, isTrue,
      reason: 'send adds message to output stream');
});
```

**Expected:** Type checking should pass because:
- `MyChannel ::= ch(MyStream?, MyStream)` defines channel as `ch(input_stream, output_stream)`
- The clause `my_send(X, ch(In, [X?|Out?]), ch(In?, Out))` should match:
  - First arg: `X` is writer (message to send)
  - Second arg input: `ch(In, [X?|Out?])` - input stream unchanged (In), output stream extended with X?
  - Third arg output: `ch(In?, Out)` - same streams with modes flipped
- The message `X` is written in first arg, read as `X?` in the output stream

**Actual:** `isWellTyped = false`

**Similar failures in this category:**
- "receive is well-moded"
- "Producer-consumer pattern"

---

## Summary

All 17 failures are actual type checking failures, not parse errors or redefinition errors.

The tests cover:
1. **Self-dual type semantics** - Types where `T` and `T?` should be equivalent
2. **Complex moded data structures** - Difference lists, channels
3. **Mode threading** - Ensuring modes flow correctly through data structures

These tests are checking that the moded type system correctly handles:
- Subtype relationships with self-dual types (`MyAny ::< MyEvery`)
- Mode annotations in compound types (`MyList \ MyList?`, `ch(MyStream?, MyStream)`)
- Mode coverage requirements for different type definitions
