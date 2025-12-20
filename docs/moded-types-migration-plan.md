# Moded Types Implementation, Testing & Migration Plan

**Status:** Ready for Implementation
**Date:** 2025-12-20
**Depends on:** Working type checker (✓ complete), all book programs well-typed (in progress)
**Goal:** Extend type system to moded types; migrate all 152 book programs to well-moded-typed

---

## Terminology

| Term | Meaning |
|------|---------|
| **Well-typed** | Program satisfies structural type fixpoint T_P^α(S) = S |
| **Well-moded-typed** | Program satisfies moded type fixpoint T_M^{α,m}(S) = S |
| **Mode annotation** | The `?` suffix indicating input mode (e.g., `List?`) |
| **Mode complementation** | The `(·)?` operator that inverts modes at call boundaries |
| **Embedded mode** | Mode annotation within a type definition (e.g., `show(Number?)`) |

---

## Prerequisites

Before migration can begin:
1. **All book programs must be well-typed** (structural types complete, no `Any` cop-outs)
2. **Type checker fully working** (✓ complete)
3. **Moded type checker implemented** (this plan)

Migration assumes well-typed programs as input.

---

## Part 1: Implementation Plan

### Phase 1: Parser Extension (1 day)

**Goal:** Parse `Type?` syntax in procedure declarations.

**Files to modify:**
- `lib/analysis/type_checker/type_ast.dart`
- `lib/analysis/type_checker/type_parser.dart`

**Tasks:**

1.1. **Extend TypeRef AST node**
```dart
class TypeRef extends TypeExpr {
  final String name;
  final bool isInput;  // NEW: true if Type?, false if Type

  TypeRef(this.name, {this.isInput = false});

  TypeRef complement() => TypeRef(name, isInput: !isInput);
}
```

1.2. **Update type parser to recognize `?` suffix**
```dart
TypeRef _parseTypeRef() {
  final name = _consume(TokenType.ATOM, 'Expected type name');
  final isInput = _match(TokenType.QUESTION);
  return TypeRef(name.lexeme, isInput: isInput);
}
```

1.3. **Update ProcDecl to expose mode information**
```dart
class ProcDecl {
  // ... existing fields ...

  bool isInputArg(int i) => argTypes[i].isInput;
  List<TypeRef> get calleeView => argTypes.map((t) => t.complement()).toList();
}
```

**Deliverables:**
- [ ] TypeRef with isInput field
- [ ] Parser recognizes `Type?` in procedure declarations
- [ ] ProcDecl exposes mode information
- [ ] Unit tests for parsing moded procedure declarations

---

### Phase 2: Mode Checker Core (2 days)

**Goal:** Implement core mode checking logic.

**Files to create:**
- `lib/analysis/type_checker/mode_checker.dart`

**Tasks:**

2.1. **Create Mode enum and operations**
```dart
enum Mode { output, input }

extension ModeOps on Mode {
  Mode get complement => this == Mode.output ? Mode.input : Mode.output;
}
```

2.2. **Implement variable mode checking**
```dart
class ModeChecker {
  /// Check variable mode at a leaf position
  ModeError? checkVariableMode(
    VarTerm variable,
    Mode expectedMode,
    bool isHeadPosition,
  ) {
    final varMode = variable.isReader ? Mode.input : Mode.output;
    var expected = expectedMode;
    if (isHeadPosition) {
      expected = expected.complement;
    }
    if (varMode != expected) {
      return ModeError(...);
    }
    return null;
  }
}
```

2.3. **Implement mode combination for nested positions**
```dart
Mode combineMode(Mode parentMode, Mode embeddedMode) {
  return parentMode == Mode.input
      ? embeddedMode.complement
      : embeddedMode;
}
```

2.4. **Implement recursive term/type mode checking**
```dart
List<ModeError> checkTermMode(
  Term term,
  TypeRef expectedType,
  Mode parentMode,
  bool isHeadPosition,
  TypeEnvironment env,
) {
  // Recursively walk term and type in parallel
  // Check modes at leaf (variable) positions
  // Combine modes when entering nested type references
}
```

**Deliverables:**
- [ ] Mode enum with complement operation
- [ ] ModeChecker class with checkVariableMode()
- [ ] Nested mode combination logic
- [ ] Recursive term/type mode traversal
- [ ] Unit tests for mode checking logic

---

### Phase 3: Integrate with Type Checker (2 days)

**Goal:** Unify structural type checking with mode checking.

**Files to modify:**
- `lib/analysis/type_checker/type_checker.dart`
- `lib/analysis/type_checker/type_dfa.dart` (optional: moded paths)

**Tasks:**

3.1. **Extend type checker to invoke mode checker**
```dart
class TypeChecker {
  final ModeChecker _modeChecker = ModeChecker();

  List<AnalysisError> checkClause(Clause clause, ProcDecl decl) {
    final errors = <AnalysisError>[];

    // Existing structural type checking
    errors.addAll(_checkStructuralTypes(clause, decl));

    // NEW: Mode checking
    errors.addAll(_modeChecker.checkClauseModes(clause, decl));

    return errors;
  }
}
```

3.2. **Check head argument modes**
```dart
List<ModeError> checkHeadModes(Atom head, ProcDecl decl) {
  final errors = <ModeError>[];
  for (int i = 0; i < head.args.length; i++) {
    final arg = head.args[i];
    final declaredType = decl.argTypes[i];
    final mode = declaredType.isInput ? Mode.input : Mode.output;
    errors.addAll(checkTermMode(arg, declaredType, mode, isHead: true));
  }
  return errors;
}
```

3.3. **Check body goal modes with complementation**
```dart
List<ModeError> checkBodyGoalModes(Goal goal, ProcDecl decl) {
  final errors = <ModeError>[];
  final calleeTypes = decl.calleeView;  // Complemented!
  for (int i = 0; i < goal.args.length; i++) {
    final arg = goal.args[i];
    final expectedType = calleeTypes[i];
    final mode = expectedType.isInput ? Mode.input : Mode.output;
    errors.addAll(checkTermMode(arg, expectedType, mode, isHead: false));
  }
  return errors;
}
```

3.4. **Unified error reporting**
- Structural type errors: `[TYPE ERROR] ...`
- Mode errors: `[MODE ERROR] ...`
- Both use same error infrastructure

**Deliverables:**
- [ ] Type checker invokes mode checker
- [ ] Head argument mode checking
- [ ] Body goal mode checking with complementation
- [ ] Unified error output
- [ ] Integration tests

---

### Phase 4: CLI & Flags (0.5 day)

**Goal:** Ensure `--type-check` performs moded type checking.

**Files to modify:**
- `bin/glpc.dart`
- Help text / documentation

**Tasks:**

4.1. **Update help text**
```
--type-check, -t    Type check program (includes mode checking)
--strict, -s        Abort on type/mode errors
```

4.2. **Ensure backward compatibility**
- Programs without mode annotations still work
- Unmoded procedure declarations default to `Any` at all positions
- Mode checking is effectively disabled for unmoded declarations

**Deliverables:**
- [ ] Updated help text
- [ ] Backward compatibility verified
- [ ] Documentation updated

---

## Part 2: Testing Plan

### Unit Tests

**Location:** `test/analysis/type_checker/`

| Test File | Coverage |
|-----------|----------|
| `type_ref_test.dart` | TypeRef with isInput, complement() |
| `mode_checker_test.dart` | Variable mode checking, mode combination |
| `moded_type_parser_test.dart` | Parsing `Type?` in proc declarations |
| `moded_type_checker_test.dart` | Full moded type checking |

### Test Cases for Mode Checker

```dart
group('Mode checking', () {
  test('writer at input position succeeds', () {
    // procedure foo(Nat?).  -- input mode
    // foo(X).               -- X is writer, should succeed
  });

  test('writer at output position fails', () {
    // procedure foo(Nat).   -- output mode
    // foo(X).               -- X is writer, should fail
  });

  test('reader at output position succeeds', () {
    // procedure foo(Nat).   -- output mode
    // foo(X?).              -- X? is reader, should succeed
  });

  test('reader at input position fails', () {
    // procedure foo(Nat?).  -- input mode
    // foo(X?).              -- X? is reader, should fail
  });

  test('mode complementation at call boundary', () {
    // procedure bar(Nat?, Nat).
    // foo(...) :- bar(A?, B).
    // At call site: A? at input→output (complemented), B at output→input
  });

  test('nested mode combination', () {
    // DiffList ::= dl(List?, List).
    // procedure foo(DiffList?).
    // Nested modes: dl(output, input) after complementation
  });
});
```

### Error Message Tests

```dart
group('Mode error messages', () {
  test('helpful message for writer at output', () {
    // Should suggest using reader X? instead
  });

  test('shows procedure declaration in context', () {
    // Error message includes the declared modes
  });
});
```

### Integration Tests

**Location:** `test/programs/moded_types/`

```
test/programs/moded_types/
├── valid/
│   ├── simple_io.glp        # Basic input/output modes
│   ├── merge.glp            # Classic merge with modes
│   ├── diff_list.glp        # Embedded modes in types
│   └── counter.glp          # Complex embedded modes
└── invalid/
    ├── writer_at_output.glp # Should fail with mode error
    ├── reader_at_input.glp  # Should fail with mode error
    └── wrong_call_mode.glp  # Mode mismatch at call site
```

### Regression Tests

- All existing type checker tests must still pass
- All 152 book programs must pass structural type checking (before migration)

---

## Part 3: Migration Plan

### Overview

Migrate all 152 book programs from **well-typed** to **well-moded-typed**.

**Input:** All programs in `/book/` are well-typed (structural types complete, no `Any` cop-outs).

**Output:** New directories `/book_moded_typed/` and `/repl_moded_typed/` containing well-moded-typed versions.

**Process:**
1. Create parallel directory structure
2. Copy each program
3. Add mode annotations to type definitions and procedure declarations
4. Verify with moded type checker
5. When all programs pass, commit to moded types

**Note:** Moded type checking remains an optional compilation step (`--type-check` flag).

### Migration Protocol

For each program file:

1. **Copy to moded directory**
   ```bash
   cp book/path/to/file.glp book_moded_typed/path/to/file.glp
   ```

2. **Add embedded modes to type definitions**
   - Identify constructors with response/output slots
   - Add `?` to mark input positions (e.g., `show(Number?)`)

3. **Add modes to procedure declarations**
   - Identify which arguments are inputs (caller provides data)
   - Identify which arguments are outputs (predicate produces data)
   - Add `?` suffix to input types

4. **Verify with moded type checker**
   ```bash
   dart bin/glpc.dart --type-check -s book_moded_typed/path/to/file.glp
   ```

5. **Fix any mode errors**
   - Ensure writer variables at input positions
   - Ensure reader variables at output positions
   - Adjust embedded modes in types if needed

6. **Document any issues**
   - Record tricky cases
   - Note any programs that needed clause changes

### Migration Order

Follow the book order for systematic coverage:

**Batch 1: Foundations (20 programs)**
```
book/constants/gates/*.glp
book/constants/arithmetic/*.glp
```

**Batch 2: Streams (35 programs)**
```
book/streams/producers_consumers/*.glp
book/streams/buffered_communication/*.glp
book/streams/objects_monitors/*.glp
```

**Batch 3: Recursive (40 programs)**
```
book/recursive/arithmetic_trees/*.glp
book/recursive/list_processing/*.glp
book/recursive/structure_processing/*.glp
```

**Batch 4: Meta (25 programs)**
```
book/meta/plain/*.glp
book/meta/enhanced/*.glp
book/meta/debugging/*.glp
```

**Batch 5: Multiagent (20 programs)**
```
book/multiagent/social_graph/*.glp
book/multiagent/social_networks/*.glp
```

**Batch 6: Library (12 programs)**
```
book/lib/*.glp
```

### Embedded Mode Examples

Mode annotations appear in both **type definitions** (embedded modes) and **procedure declarations** (argument modes).

**1. Counter with response slot:**
```glp
CounterMsg ::= clear ; up ; down ; show(Number?).
CounterStream ::= [] ; [CounterMsg | CounterStream].
procedure counter(CounterStream?, Number).
```
- `show(Number?)` — Number is input mode in type definition
- Counter receives `CounterStream?` (input), involution applies
- `show(Number?)` → `show(Number)` — counter WRITES the response

**2. Queue manager (dequeue has response slot):**
```glp
QueueMsg ::= enqueue(Any) ; dequeue(Any?).
QueueStream ::= [] ; [QueueMsg | QueueStream].
procedure qm(QueueStream?, List, List).
```
- `dequeue(Any?)` — response slot is input in type
- After complementation, queue manager WRITES the dequeued value

**3. Request/Response server:**
```glp
Request ::= get(Value?) ; put(Value).
RequestStream ::= [] ; [Request | RequestStream].
procedure server(RequestStream?, State).
```
- `get(Value?)` — Value is input in type → after complementation becomes output (server fills it)
- `put(Value)` — Value is output in type → after complementation becomes input (server receives it)

**4. Difference list:**
```glp
DiffList ::= List \ List?.
```
- Head `List` — output (the content produced)
- Tail `List?` — input (the hole where continuation connects)

### Migration Checklist Template

For each file:
```
[ ] File: book/streams/objects_monitors/counter.glp
[ ] Copied to: book_moded_typed/streams/objects_monitors/counter.glp
[ ] Embedded modes added to type definitions
[ ] Procedure declaration modes added
[ ] Moded type checker passes
[ ] Notes: (any issues encountered)
```

### Verification Script

Create a script to verify all migrated programs:

```bash
#!/bin/bash
# verify_moded_types.sh

ERRORS=0
for f in book_moded_typed/**/*.glp; do
  echo "Checking $f..."
  if ! dart bin/glpc.dart -t -s "$f" 2>/dev/null; then
    echo "FAILED: $f"
    ERRORS=$((ERRORS + 1))
  fi
done

echo "================================"
echo "Total errors: $ERRORS"
exit $ERRORS
```

### Completion Criteria

When all programs in `book_moded_typed/` and `repl_moded_typed/` pass moded type checking:
1. Replace original directories with moded versions
2. Update all documentation
3. Commit to moded types as the standard

---

## Timeline Summary

| Phase | Duration | Dependencies |
|-------|----------|--------------|
| 1. Parser Extension | 1 day | None |
| 2. Mode Checker Core | 2 days | Phase 1 |
| 3. Integration | 2 days | Phase 2 |
| 4. CLI & Flags | 0.5 day | Phase 3 |
| 5. Testing | 1.5 days | Phase 3 |
| 6. Migration Batch 1 | 1 day | Phase 4 |
| 7. Migration Batch 2-6 | 4 days | Batch 1 |

**Total: ~12 days**

---

## Success Criteria

1. **All unit tests pass** for mode checking logic
2. **All integration tests pass** for valid/invalid programs
3. **All 152 book programs** in `book_moded_typed/` pass `--type-check -s`
4. **All REPL tests** in `repl_moded_typed/` pass moded type checking
5. **No regression** in existing type checker functionality
6. **Clear error messages** that help users fix mode issues
7. **Documentation updated** to reflect moded types

---

## Resolved Design Decisions

1. **Embedded modes in type definitions**: ✓ REQUIRED
   - Type definitions include `?` on nested type references (e.g., `show(Number?)`)
   - Essential for incomplete messages, difference lists, meta-interpreters

2. **Default mode for unmoded declarations**: ✓ Output mode
   - `procedure foo(Nat).` without `?` = output mode
   - Consistent with "callee writes" semantics

3. **Migration approach**: ✓ Parallel directories
   - Create `book_moded_typed/` and `repl_moded_typed/`
   - Original directories unchanged until migration complete
   - Moded type checking remains optional (`--type-check` flag)

4. **Mode inference**: ✓ Explicit declarations required
   - No automatic mode inference in this phase
   - Mode inference is future work
