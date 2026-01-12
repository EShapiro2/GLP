# Type Checker Remediation Plan

**Version**: 1.0  
**Date**: 2025-01-12  
**Status**: Ready for execution  
**Reference**: SPEC_IMPLEMENTATION_GAPS.md

## Principles

1. Fix in dependency order (leaf modules first)
2. For each gap: write tests first, then implement
3. Each fix must pass all existing tests plus new tests
4. No fix is complete until the full test suite passes

---

## Phase 1: type-environment Module

### Fix 1.1: TypeRef.builtins Incomplete (Gap 2.4)

**Spec**: `Integer`, `Real`, `Number`, `String` are all predefined primitive types

**Files to modify**:
- `lib/analysis/type_checker/type_ast.dart`

**Tests to add** (`test/analysis/type_checker/type_environment_test.dart`):
```dart
group('Predefined types', () {
  test('Integer is a builtin type', () {
    expect(TypeRef.builtins.contains('Integer'), isTrue);
  });
  
  test('Real is a builtin type', () {
    expect(TypeRef.builtins.contains('Real'), isTrue);
  });
  
  test('Number is a builtin type', () {
    expect(TypeRef.builtins.contains('Number'), isTrue);
  });
  
  test('String is a builtin type', () {
    expect(TypeRef.builtins.contains('String'), isTrue);
  });
});
```

**Code change** in `type_ast.dart`:
```dart
// Line ~53, replace:
static const builtins = {'Number', 'String'};

// With:
static const builtins = {'Integer', 'Real', 'Number', 'String'};
```

---

### Fix 1.2: Type Alias Prohibition (Gap 2.1)

**Spec**: Type definitions must introduce structure, not alias existing types

**Files to modify**:
- `lib/analysis/type_checker/type_environment_builder.dart`

**Tests to add** (`test/analysis/type_checker/type_environment_test.dart`):
```dart
group('Type alias prohibition', () {
  test('NEGATIVE: alias to primitive _ is rejected', () {
    final source = 'Output ::= _.';
    expect(() => parseAndBuildEnv(source), throwsA(isA<TypeAliasError>()));
  });
  
  test('NEGATIVE: alias to primitive _? is rejected', () {
    final source = 'Input ::= _?.';
    expect(() => parseAndBuildEnv(source), throwsA(isA<TypeAliasError>()));
  });
  
  test('NEGATIVE: alias to defined type is rejected', () {
    final source = '''
      Stream ::= [] ; [_ | Stream].
      MyStream ::= Stream.
    ''';
    expect(() => parseAndBuildEnv(source), throwsA(isA<TypeAliasError>()));
  });
  
  test('NEGATIVE: alias to complement type is rejected', () {
    final source = '''
      Stream ::= [] ; [_ | Stream].
      MyInput ::= Stream?.
    ''';
    expect(() => parseAndBuildEnv(source), throwsA(isA<TypeAliasError>()));
  });
  
  test('POSITIVE: type with single compound alternative is valid', () {
    final source = 'Wrapper ::= wrap(_).';
    final env = parseAndBuildEnv(source);
    expect(env.hasType('Wrapper'), isTrue);
  });
  
  test('POSITIVE: type with multiple alternatives is valid', () {
    final source = 'Bool ::= true ; false.';
    final env = parseAndBuildEnv(source);
    expect(env.hasType('Bool'), isTrue);
  });
});
```

**Code change** in `type_environment_builder.dart`:

Add error class:
```dart
class TypeAliasError implements Exception {
  final String message;
  final int line;
  final int column;

  TypeAliasError(this.message, this.line, this.column);

  @override
  String toString() => '$message at line $line, column $column';
}
```

Add validation function:
```dart
/// Check if a type definition is an alias (no new structure)
bool _isTypeAlias(TypeDef def) {
  // Multiple alternatives = not an alias
  if (def.alternatives.length != 1) return false;
  
  final alt = def.alternatives.first;
  
  // Single PrimitiveModeAlt (_ or _?) = alias
  if (alt is PrimitiveModeAlt) return true;
  
  // Single TypeRef (T or T?) = alias
  if (alt is TypeRef) return true;
  
  // Compound structures are valid
  // ConstantAlt, ListNilAlt, ListConsAlt, StructAlt, DiffListAlt
  return false;
}
```

Add validation call in `_buildEnvironmentFromModule`:
```dart
// After existing checks, before adding to types map:
if (_isTypeAlias(typeDef)) {
  throw TypeAliasError(
    'Type definition must introduce structure, not alias: ${typeDef.name}',
    typeDef.line,
    typeDef.column,
  );
}
```

---

### Fix 1.3: Determinism Check (Gap 2.2)

**Spec**: Type alternatives must be distinguishable by top-level functor

**Files to modify**:
- `lib/analysis/type_checker/type_environment_builder.dart`

**Tests to add** (`test/analysis/type_checker/type_environment_test.dart`):
```dart
group('Determinism check', () {
  test('NEGATIVE: overlapping wildcards _ and _? rejected', () {
    final source = 'Any ::= _ ; _?.';
    expect(() => parseAndBuildEnv(source), throwsA(isA<NonDeterministicTypeError>()));
  });
  
  test('NEGATIVE: _ overlaps with Integer rejected', () {
    final source = 'Ambiguous ::= _ ; Integer.';
    expect(() => parseAndBuildEnv(source), throwsA(isA<NonDeterministicTypeError>()));
  });
  
  test('NEGATIVE: Number overlaps with Integer rejected', () {
    final source = 'BadNumeric ::= Number ; Integer.';
    expect(() => parseAndBuildEnv(source), throwsA(isA<NonDeterministicTypeError>()));
  });
  
  test('NEGATIVE: duplicate functor same arity rejected', () {
    final source = 'BadTree ::= leaf(Integer) ; leaf(String).';
    expect(() => parseAndBuildEnv(source), throwsA(isA<NonDeterministicTypeError>()));
  });
  
  test('NEGATIVE: duplicate constant rejected', () {
    final source = 'Bad ::= 0 ; 0.';
    expect(() => parseAndBuildEnv(source), throwsA(isA<NonDeterministicTypeError>()));
  });
  
  test('POSITIVE: different functors same arity is valid', () {
    final source = 'Tree ::= leaf(Integer) ; node(Tree, Tree).';
    final env = parseAndBuildEnv(source);
    expect(env.hasType('Tree'), isTrue);
  });
  
  test('POSITIVE: same functor different arity is valid', () {
    final source = 'Tree ::= leaf ; node(Tree, Tree).';
    final env = parseAndBuildEnv(source);
    expect(env.hasType('Tree'), isTrue);
  });
  
  test('POSITIVE: disjoint primitives Integer and String is valid', () {
    final source = 'Constant ::= Integer ; String.';
    final env = parseAndBuildEnv(source);
    expect(env.hasType('Constant'), isTrue);
  });
  
  test('POSITIVE: [] and [|] are distinguishable', () {
    final source = 'List ::= [] ; [_ | List].';
    final env = parseAndBuildEnv(source);
    expect(env.hasType('List'), isTrue);
  });
});
```

**Code change** in `type_environment_builder.dart`:

Add error class:
```dart
class NonDeterministicTypeError implements Exception {
  final String message;
  final int line;
  final int column;

  NonDeterministicTypeError(this.message, this.line, this.column);

  @override
  String toString() => '$message at line $line, column $column';
}
```

Add validation function:
```dart
/// Check if type alternatives are deterministic (distinguishable)
void _checkDeterminism(TypeDef def) {
  final functors = <String>{};      // "functor/arity" keys
  final constants = <String>{};     // constant values
  final primitives = <String>{};    // primitive type names
  
  for (final alt in def.alternatives) {
    if (alt is ConstantAlt) {
      final key = alt.value.toString();
      if (constants.contains(key)) {
        throw NonDeterministicTypeError(
          'Duplicate constant alternative: $key in ${def.name}',
          def.line, def.column);
      }
      constants.add(key);
      
    } else if (alt is ListNilAlt) {
      if (functors.contains('[]/0')) {
        throw NonDeterministicTypeError(
          'Duplicate [] alternative in ${def.name}',
          def.line, def.column);
      }
      functors.add('[]/0');
      
    } else if (alt is ListConsAlt) {
      if (functors.contains('[|]/2')) {
        throw NonDeterministicTypeError(
          'Duplicate [|] alternative in ${def.name}',
          def.line, def.column);
      }
      functors.add('[|]/2');
      
    } else if (alt is StructAlt) {
      final key = '${alt.functor}/${alt.arity}';
      if (functors.contains(key)) {
        throw NonDeterministicTypeError(
          'Duplicate functor alternative: $key in ${def.name}',
          def.line, def.column);
      }
      functors.add(key);
      
    } else if (alt is DiffListAlt) {
      if (functors.contains('\\/2')) {
        throw NonDeterministicTypeError(
          'Duplicate \\ alternative in ${def.name}',
          def.line, def.column);
      }
      functors.add('\\/2');
      
    } else if (alt is PrimitiveModeAlt) {
      _checkPrimitiveOverlap('_', primitives, def);
      primitives.add('_');
      
    } else if (alt is TypeRef) {
      // TypeRef in alternative position = primitive type reference
      if ({'Integer', 'Real', 'Number', 'String'}.contains(alt.name)) {
        _checkPrimitiveOverlap(alt.name, primitives, def);
        primitives.add(alt.name);
      }
    }
  }
}

void _checkPrimitiveOverlap(String newPrimitive, Set<String> existing, TypeDef def) {
  // _ overlaps with everything
  if (existing.contains('_') || newPrimitive == '_') {
    if (existing.isNotEmpty) {
      throw NonDeterministicTypeError(
        'Wildcard _ overlaps with other alternatives in ${def.name}',
        def.line, def.column);
    }
  }
  
  // Number overlaps with Integer and Real
  if (newPrimitive == 'Number' && 
      (existing.contains('Integer') || existing.contains('Real'))) {
    throw NonDeterministicTypeError(
      'Number overlaps with Integer/Real in ${def.name}',
      def.line, def.column);
  }
  if ((newPrimitive == 'Integer' || newPrimitive == 'Real') && 
      existing.contains('Number')) {
    throw NonDeterministicTypeError(
      'Integer/Real overlaps with Number in ${def.name}',
      def.line, def.column);
  }
  
  // Direct duplicate
  if (existing.contains(newPrimitive)) {
    throw NonDeterministicTypeError(
      'Duplicate primitive type $newPrimitive in ${def.name}',
      def.line, def.column);
  }
}
```

Add validation call in `_buildEnvironmentFromModule` after alias check:
```dart
_checkDeterminism(typeDef);
```

---

### Fix 1.4: TypeClassification Enum (Gap 2.3)

**Spec**: Types classified as output, input, or interactive

**Files to modify**:
- `lib/analysis/type_checker/type_ast.dart`

**Tests to add** (`test/analysis/type_checker/type_environment_test.dart`):
```dart
group('Type classification', () {
  test('Stream is output type (no internal complement)', () {
    final source = 'Stream ::= [] ; [_ | Stream].';
    final env = parseAndBuildEnv(source);
    expect(env.getType('Stream')!.classification, TypeClassification.output);
  });
  
  test('HollowIntegers is interactive type (has Integer?)', () {
    final source = 'HollowIntegers ::= [] ; [Integer? | HollowIntegers].';
    final env = parseAndBuildEnv(source);
    expect(env.getType('HollowIntegers')!.classification, TypeClassification.interactive);
  });
  
  test('CounterCall is interactive type (has Integer? inside)', () {
    final source = 'CounterCall ::= add ; clear ; read(Integer?).';
    final env = parseAndBuildEnv(source);
    expect(env.getType('CounterCall')!.classification, TypeClassification.interactive);
  });
  
  test('NatList is output type (Integer without ?)', () {
    final source = 'NatList ::= [] ; [Integer | NatList].';
    final env = parseAndBuildEnv(source);
    expect(env.getType('NatList')!.classification, TypeClassification.output);
  });
});
```

**Code change** in `type_ast.dart`:

Add enum after imports:
```dart
/// Classification of types by mode structure
enum TypeClassification {
  output,      // No complementation in definition
  input,       // Complement of an output type (not directly defined)
  interactive  // Contains internal complementation
}
```

Add getter to `TypeDef` class:
```dart
/// Classify this type based on mode structure
TypeClassification get classification {
  for (final alt in alternatives) {
    if (_containsComplement(alt)) {
      return TypeClassification.interactive;
    }
  }
  return TypeClassification.output;
}

static bool _containsComplement(TypeExpr expr) {
  if (expr is TypeRef && expr.isInput) return true;
  if (expr is PrimitiveModeAlt && expr.isInput) return true;
  
  if (expr is ListConsAlt) {
    return _containsComplement(expr.head) || _containsComplement(expr.tail);
  }
  if (expr is StructAlt) {
    return expr.args.any(_containsComplement);
  }
  if (expr is DiffListAlt) {
    return _containsComplement(expr.content) || _containsComplement(expr.hole);
  }
  
  return false;
}
```

---

## Phase 2: moded-term Module

### Fix 2.1: ModedConstant Numeric Properties (Gap 3.1)

**Spec**: ModedConstant should have `isInteger`, `isReal`, `isNumeric`, `isString`, `isAtom` properties

**Files to modify**:
- `lib/analysis/type_checker/moded_term.dart`

**Tests to add** (`test/analysis/type_checker/moded_term_test.dart`):
```dart
group('ModedConstant type properties', () {
  test('integer constant has isInteger=true', () {
    final c = ModedConstant(Mode.produce, 42);
    expect(c.isInteger, isTrue);
    expect(c.isReal, isFalse);
    expect(c.isNumeric, isTrue);
  });
  
  test('real constant has isReal=true', () {
    final c = ModedConstant(Mode.produce, 3.14);
    expect(c.isReal, isTrue);
    expect(c.isInteger, isFalse);
    expect(c.isNumeric, isTrue);
  });
  
  test('string constant has isString=true', () {
    final c = ModedConstant(Mode.produce, '"hello"');
    expect(c.isString, isTrue);
    expect(c.isAtom, isFalse);
  });
  
  test('atom constant has isAtom=true', () {
    final c = ModedConstant(Mode.produce, 'foo');
    expect(c.isAtom, isTrue);
    expect(c.isString, isFalse);
  });
  
  test('nil constant has isAtom=true', () {
    final c = ModedConstant.nil(Mode.produce);
    expect(c.isAtom, isTrue);
  });
});
```

**Code change** in `moded_term.dart`, add to `ModedConstant` class:
```dart
/// True if value is an integer
bool get isInteger => value is int;

/// True if value is a real (floating-point) number
bool get isReal => value is double;

/// True if value is numeric (integer or real)
bool get isNumeric => value is num;

/// True if value is a quoted string
bool get isString {
  if (value is! String) return false;
  final s = value as String;
  return (s.startsWith('"') && s.endsWith('"')) ||
         (s.startsWith("'") && s.endsWith("'"));
}

/// True if value is an unquoted atom
bool get isAtom {
  if (value is! String) return false;
  return !isString;
}
```

---

## Phase 3: type-dfa Module

### Fix 3.1: DFAState.isProcedure Property (Gap 5.1)

**Spec**: DFAState needs `isProcedure` field to distinguish procedure states from type states

**Files to modify**:
- `lib/analysis/type_checker/program_dfa.dart`

**Tests to add** (`test/analysis/type_checker/program_dfa_test.dart`):
```dart
group('DFAState.isProcedure', () {
  test('procedure state has isProcedure=true', () {
    final state = DFAState('merge/3', isComplement: false, isFinal: false, isProcedure: true);
    expect(state.isProcedure, isTrue);
  });
  
  test('type state has isProcedure=false', () {
    final state = DFAState('Stream', isComplement: false, isFinal: false, isProcedure: false);
    expect(state.isProcedure, isFalse);
  });
  
  test('buildProgramDFA sets isProcedure correctly for procedures', () {
    final env = buildTestEnv('procedure foo(_).'); 
    final dfa = buildProgramDFA(env);
    expect(dfa.getState('foo/1').isProcedure, isTrue);
  });
  
  test('buildProgramDFA sets isProcedure=false for types', () {
    final env = buildTestEnv('MyType ::= a ; b.');
    final dfa = buildProgramDFA(env);
    expect(dfa.getState('MyType').isProcedure, isFalse);
  });
});
```

**Code change** in `program_dfa.dart`:

Update `DFAState` class:
```dart
class DFAState {
  final String baseName;
  final bool isComplement;
  final bool isFinal;
  final bool isProcedure;  // ADD THIS

  DFAState(this.baseName, {
    required this.isComplement, 
    required this.isFinal,
    this.isProcedure = false,  // ADD THIS with default
  });

  // Update complement getter:
  DFAState get complement =>
      DFAState(baseName, 
        isComplement: !isComplement, 
        isFinal: isFinal,
        isProcedure: isProcedure);  // ADD THIS
  
  // ... rest unchanged
}
```

Update `buildProgramDFA` - system/type states already default to `isProcedure: false`.

Update procedure state creation:
```dart
// Create procedure states (no complement)
for (final procKey in env.procedures.keys) {
  states[procKey] = DFAState(procKey, 
    isComplement: false, 
    isFinal: false,
    isProcedure: true);  // ADD THIS
}
```

---

### Fix 3.2: DFAState.isPrimitiveType and isUserDefinedType (Gap 5.2)

**Spec**: Computed properties to classify state types

**Files to modify**:
- `lib/analysis/type_checker/program_dfa.dart`

**Tests to add** (`test/analysis/type_checker/program_dfa_test.dart`):
```dart
group('DFAState type classification', () {
  test('wildcard is primitive type', () {
    final state = DFAState('_', isComplement: false, isFinal: true);
    expect(state.isPrimitiveType, isTrue);
    expect(state.isUserDefinedType, isFalse);
  });
  
  test('Integer is primitive type', () {
    final state = DFAState('Integer', isComplement: false, isFinal: false);
    expect(state.isPrimitiveType, isTrue);
    expect(state.isUserDefinedType, isFalse);
  });
  
  test('Stream is user-defined type', () {
    final state = DFAState('Stream', isComplement: false, isFinal: false);
    expect(state.isPrimitiveType, isFalse);
    expect(state.isUserDefinedType, isTrue);
  });
  
  test('procedure state is not user-defined type', () {
    final state = DFAState('foo/1', isComplement: false, isFinal: false, isProcedure: true);
    expect(state.isUserDefinedType, isFalse);
  });
  
  test('anonymous final is not user-defined type', () {
    final state = DFAState('_FINAL_', isComplement: false, isFinal: true);
    expect(state.isUserDefinedType, isFalse);
  });
});
```

**Code change** in `program_dfa.dart`, add to `DFAState`:
```dart
/// True for primitive types: _, Integer, Real, Number, String
bool get isPrimitiveType => 
    isWildcard || isIntegerType || isRealType || isNumberType || isStringType;

/// True for user-defined types (not primitive, not procedure, not anonymous final)
bool get isUserDefinedType => 
    !isPrimitiveType && !isProcedure && !isAnonymousFinal;
```

---

### Fix 3.3: DFAState.isNumericType (Gap 5.3)

**Spec**: Combined property for numeric types

**Files to modify**:
- `lib/analysis/type_checker/program_dfa.dart`

**Tests to add** (`test/analysis/type_checker/program_dfa_test.dart`):
```dart
group('DFAState.isNumericType', () {
  test('Integer is numeric type', () {
    final state = DFAState('Integer', isComplement: false, isFinal: false);
    expect(state.isNumericType, isTrue);
  });
  
  test('Real is numeric type', () {
    final state = DFAState('Real', isComplement: false, isFinal: false);
    expect(state.isNumericType, isTrue);
  });
  
  test('Number is numeric type', () {
    final state = DFAState('Number', isComplement: false, isFinal: false);
    expect(state.isNumericType, isTrue);
  });
  
  test('String is not numeric type', () {
    final state = DFAState('String', isComplement: false, isFinal: false);
    expect(state.isNumericType, isFalse);
  });
});
```

**Code change** in `program_dfa.dart`, add to `DFAState`:
```dart
/// True for numeric types: Integer, Real, Number
bool get isNumericType => isIntegerType || isRealType || isNumberType;
```

---

## Phase 4: well-typed-term Module

### Fix 4.1: Automaton Switching at Type Boundaries (Gap 6.1)

**Spec**: When traversing into a different user-defined type, switch to that type's automaton

**Files to modify**:
- `lib/analysis/type_checker/well_typed_term.dart`

**Tests to add** (`test/analysis/type_checker/well_typed_term_test.dart`):
```dart
group('Automaton switching at type boundaries', () {
  test('nested type CounterCall inside Stream is checked correctly', () {
    final source = '''
      CounterCall ::= add ; clear ; read(Integer?).
      Stream(X) ::= [] ; [X | Stream(X)].
      procedure monitor(Stream(CounterCall)?).
    ''';
    final env = parseAndBuildEnv(source);
    final dfa = buildProgramDFA(env);
    
    // Build a moded term: [read(N?)|In?] with type Stream(CounterCall)?
    // The read(N?) should be checked against CounterCall? automaton
    // N? at mode ↑ inside read should get type Integer
    
    // This test verifies the automaton switch happens
    // ... detailed test setup ...
  });
  
  test('NEGATIVE: wrong constant in nested type fails', () {
    // A constant that matches Stream but not CounterCall should fail
    // ... detailed test setup ...
  });
});
```

**Code change** in `well_typed_term.dart`:

Update `checkPathAgainstAutomaton`:
```dart
PathCheckResult checkPathAgainstAutomaton(
  ModedPath path,
  Automaton automaton,
  ProgramDFA dfa,
) {
  var state = automaton.startState;
  var currentAutomaton = automaton;  // Track current automaton

  // Handle single-step paths (just a variable or constant at root)
  if (path.length == 1) {
    return _checkLeafConsistencyForPath(path.leaf, state, dfa);
  }

  // Traverse path, following automaton transitions
  for (int i = 0; i < path.length - 1; i++) {
    final step = path.steps[i];
    final nextStep = path.steps[i + 1];

    // Build transition label from path step
    final label = _buildTransitionLabel(step, nextStep);

    // Try to follow transition
    final nextState = currentAutomaton.transition(state, label);

    if (nextState == null) {
      return PathCheckResult.inconsistent(
          'No transition for $label from state ${state.name}');
    }

    // Switch automata at type boundaries
    if (nextState.isUserDefinedType && nextState.baseName != state.baseName) {
      try {
        currentAutomaton = dfa.getAutomaton(nextState.name);
      } catch (e) {
        return PathCheckResult.inconsistent(
            'Cannot get automaton for type ${nextState.name}');
      }
    }

    state = nextState;
  }

  // Check leaf consistency
  return _checkLeafConsistencyForPath(path.leaf, state, dfa);
}
```

---

### Fix 4.2: Real Literal Detection (Gap 6.2)

**Spec**: Real literals should be detected and handled by Real/Number types

**Files to modify**:
- `lib/analysis/type_checker/well_typed_term.dart`

**Tests to add** (`test/analysis/type_checker/well_typed_term_test.dart`):
```dart
group('Real literal detection', () {
  test('real literal 3.14 is detected as real', () {
    final step = PathStep(symbol: '3.14', argIndex: 1, mode: Mode.produce);
    final leaf = _pathStepToLeafTerm(step);
    expect(leaf.isReal, isTrue);
    expect(leaf.isInteger, isFalse);
  });
  
  test('real literal with exponent 2.5e10 is detected as real', () {
    final step = PathStep(symbol: '2.5e10', argIndex: 1, mode: Mode.produce);
    final leaf = _pathStepToLeafTerm(step);
    expect(leaf.isReal, isTrue);
  });
  
  test('real literal passes Real type check', () {
    // ... full integration test with Real type ...
  });
  
  test('real literal passes Number type check', () {
    // ... full integration test with Number type ...
  });
});
```

**Code change** in `well_typed_term.dart`:

Update `_pathStepToLeafTerm`:
```dart
LeafTerm _pathStepToLeafTerm(PathStep step) {
  if (step.isVariable) {
    if (step.isReader) {
      return LeafTerm.reader(step.symbol, mode: step.mode);
    } else {
      return LeafTerm.writer(step.symbol, mode: step.mode);
    }
  } else {
    final value = step.symbol;
    
    // Check for integer first (more specific)
    final intVal = int.tryParse(value);
    if (intVal != null) {
      return LeafTerm.integerConstant(intVal);
    }
    
    // Check for real (floating-point)
    final doubleVal = double.tryParse(value);
    if (doubleVal != null) {
      return LeafTerm.realConstant(doubleVal);
    }
    
    // Check for string (quoted)
    if ((value.startsWith("'") && value.endsWith("'")) ||
        (value.startsWith('"') && value.endsWith('"'))) {
      return LeafTerm.stringConstant(value.substring(1, value.length - 1));
    }
    
    // Otherwise it's an atom/constant
    return LeafTerm.constant(value);
  }
}
```

---

## Phase 5: well-typed-clause Module

### Fix 5.1: ClauseCheckResult Moded Term Fields (Gap 7.1)

**Spec**: ClauseCheckResult should include `modedHead` and `modedBodyAtoms` for inspection

**Files to modify**:
- `lib/analysis/type_checker/well_typed_clause.dart`

**Tests to add** (`test/analysis/type_checker/well_typed_clause_test.dart`):
```dart
group('ClauseCheckResult moded terms', () {
  test('successful check includes modedHead', () {
    final source = '''
      Stream ::= [] ; [_ | Stream].
      procedure merge(Stream?, Stream?, Stream).
      merge([], Ys, Ys?).
    ''';
    final result = checkClauseSource(source);
    expect(result.isWellTyped, isTrue);
    expect(result.modedHead, isNotNull);
  });
  
  test('successful check includes modedBodyAtoms', () {
    final source = '''
      Stream ::= [] ; [_ | Stream].
      procedure merge(Stream?, Stream?, Stream).
      merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
    ''';
    final result = checkClauseSource(source);
    expect(result.isWellTyped, isTrue);
    expect(result.modedBodyAtoms.length, equals(1));
  });
  
  test('failed check still includes modedHead', () {
    // Even on failure, we should have the constructed moded head
    // for debugging purposes
    // ... test setup ...
  });
});
```

**Code change** in `well_typed_clause.dart`:

Update `ClauseCheckResult`:
```dart
class ClauseCheckResult {
  final bool isWellTyped;
  final Map<String, VariableTypeInfo> variableTypes;
  final List<ClauseError> errors;
  final ModedTerm? modedHead;
  final List<ModedTerm> modedBodyAtoms;

  ClauseCheckResult({
    required this.isWellTyped,
    required this.variableTypes,
    required this.errors,
    this.modedHead,
    this.modedBodyAtoms = const [],
  });

  factory ClauseCheckResult.success(
    Map<String, VariableTypeInfo> variableTypes, {
    ModedTerm? modedHead,
    List<ModedTerm> modedBodyAtoms = const [],
  }) {
    return ClauseCheckResult(
      isWellTyped: true,
      variableTypes: variableTypes,
      errors: [],
      modedHead: modedHead,
      modedBodyAtoms: modedBodyAtoms,
    );
  }

  factory ClauseCheckResult.failure(
    List<ClauseError> errors, [
    Map<String, VariableTypeInfo>? variableTypes,
    ModedTerm? modedHead,
    List<ModedTerm>? modedBodyAtoms,
  ]) {
    return ClauseCheckResult(
      isWellTyped: false,
      variableTypes: variableTypes ?? {},
      errors: errors,
      modedHead: modedHead,
      modedBodyAtoms: modedBodyAtoms ?? [],
    );
  }
}
```

Update `checkClause` to track and return the moded terms:
```dart
ClauseCheckResult checkClause(
  TypedClause clause,
  ProgramDFA dfa,
  TypeEnvironment env,
) {
  final errors = <ClauseError>[];
  final allVariableTypes = <String, VariableTypeInfo>{};
  final variableLocations = <String, String>{};
  ModedTerm? constructedModedHead;
  final constructedModedBodyAtoms = <ModedTerm>[];

  // ... existing proc lookup ...

  // Step 1: Check head well-typing
  final (headResult, modedHeadTerm) = _checkHeadWithTerm(clause, procDecl, dfa, env);
  constructedModedHead = modedHeadTerm;
  
  // ... rest of existing logic ...

  // Step 2: Check each body atom
  for (int i = 0; i < clause.bodyAtoms.length; i++) {
    final atom = clause.bodyAtoms[i];
    final (atomResult, modedAtomTerm) = _checkBodyAtomWithTerm(atom, i, dfa, env);
    if (modedAtomTerm != null) {
      constructedModedBodyAtoms.add(modedAtomTerm);
    }
    // ... rest of existing logic ...
  }

  // ... complementarity check ...

  return ClauseCheckResult(
    isWellTyped: errors.isEmpty,
    variableTypes: allVariableTypes,
    errors: errors,
    modedHead: constructedModedHead,
    modedBodyAtoms: constructedModedBodyAtoms,
  );
}
```

Create helper functions that return both result and moded term:
```dart
(WellTypedResult, ModedTerm?) _checkHeadWithTerm(...) { ... }
(WellTypedResult, ModedTerm?) _checkBodyAtomWithTerm(...) { ... }
```

---

## Execution Checklist

| Phase | Fix | Tests Written | Tests Pass | Code Changed | All Tests Pass |
|-------|-----|---------------|------------|--------------|----------------|
| 1 | 1.1 TypeRef.builtins | ☐ | ☐ | ☐ | ☐ |
| 1 | 1.2 Type alias prohibition | ☐ | ☐ | ☐ | ☐ |
| 1 | 1.3 Determinism check | ☐ | ☐ | ☐ | ☐ |
| 1 | 1.4 TypeClassification | ☐ | ☐ | ☐ | ☐ |
| 2 | 2.1 ModedConstant properties | ☐ | ☐ | ☐ | ☐ |
| 3 | 3.1 DFAState.isProcedure | ☐ | ☐ | ☐ | ☐ |
| 3 | 3.2 DFAState.isUserDefinedType | ☐ | ☐ | ☐ | ☐ |
| 3 | 3.3 DFAState.isNumericType | ☐ | ☐ | ☐ | ☐ |
| 4 | 4.1 Automaton switching | ☐ | ☐ | ☐ | ☐ |
| 4 | 4.2 Real literal detection | ☐ | ☐ | ☐ | ☐ |
| 5 | 5.1 ClauseCheckResult fields | ☐ | ☐ | ☐ | ☐ |

---

## Verification

After all fixes are complete:

1. Run full type checker test suite:
   ```bash
   cd /Users/udi/GLP/glp_runtime
   dart test test/analysis/type_checker/
   ```

2. Run book programs check:
   ```bash
   dart run bin/check_types.dart ../programs/book/
   ```

3. Verify no regressions in passing programs count (was 116/142)
