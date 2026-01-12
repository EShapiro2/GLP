# Spec-Implementation Gaps Analysis

**Version**: 1.0  
**Date**: 2025-01-12  
**Author**: Claude (analysis session)  
**Status**: Analysis complete, awaiting remediation

## Overview

This document identifies all gaps and inconsistencies between the type system specification files in `/docs/type system/` and the implementation in `/glp_runtime/lib/analysis/type_checker/`.

The modules are listed in dependency order (leaf modules first) to enable logical testing during remediation.

---

## 1. Module: mode

**Spec**: `mode.md` v0.2  
**Implementation**: `mode.dart`

### Status: ✅ CONSISTENT

No gaps identified. The implementation correctly provides:
- `Mode` enum with `input`/`output` (aliased as `consume`/`produce`)
- `flip` method (aliased as `complement`)
- `combineMode()` function implementing XOR involution

---

## 2. Module: type-environment

**Spec**: `type-environment.md` v0.5  
**Implementation**: `type_ast.dart`, `type_environment_builder.dart`, `prelude.dart`

### Gap 2.1: Missing Type Alias Prohibition

**Spec requirement** (Section "Type Alias Prohibition"):
> Type definitions must introduce **new structure**, not rename existing types.
> 
> **Illegal aliases:**
> ```
> Output ::= _.             % alias for primitive
> Input ::= _?.             % alias for primitive
> MyList ::= List.          % alias for defined type
> MyStream ::= Stream?.     % alias for complement of defined type
> ```

**Implementation**: No validation exists. The parser and type environment builder accept alias definitions without error.

**Required fix**: Add `isTypeAlias(TypeDef)` validation in `type_environment_builder.dart` that throws `TypeAliasError` when:
- Definition has exactly one alternative AND
- That alternative is a single TypeRef or PrimitiveModeAlt with no compound structure

---

### Gap 2.2: Missing Determinism Check

**Spec requirement** (Section "Determinism Requirement"):
> Type definitions must be **deterministic**: alternatives must be distinguishable by their top-level functor or, for primitive types, by disjoint type membership.
>
> **Illegal overlapping definitions:**
> ```
> Any ::= _ ; _?.           % overlapping: both accept all terms
> AnyOne ::= 1 ; 1?.        % overlapping: 1 matches both alternatives  
> Ambiguous ::= _ ; Integer. % overlapping: integers match both
> ```

**Implementation**: No validation exists. Overlapping alternatives are silently accepted.

**Required fix**: Add `isDeterministic(TypeDef)` validation in `type_environment_builder.dart` that throws `NonDeterministicTypeError` when:
- Two alternatives have the same functor/arity
- Two constant alternatives have the same value
- Primitive type alternatives overlap (e.g., `_` with anything, `Number` with `Integer` or `Real`)

---

### Gap 2.3: Missing TypeClassification Enum

**Spec requirement** (Section "Type Classification"):
> Types are classified by their mode structure:
> 
> | Classification | Definition | Example |
> |----------------|------------|---------|
> | **Output type** | No complementation in definition | `Stream ::= [] ; [_\|Stream]` |
> | **Input type** | Complement of an output type | `Stream?` (all modes flipped) |
> | **Interactive type** | Contains internal complementation | `HollowStream ::= [] ; [_?\|HollowStream]` |

**Implementation**: `TypeClassification` enum does not exist. `TypeDef` has no `classification` property.

**Required fix**: Add to `type_ast.dart`:
```dart
enum TypeClassification {
  output,      // No complementation in definition
  input,       // Would be complement of output (not directly defined)
  interactive  // Contains internal complementation
}
```

Add `TypeClassification get classification` getter to `TypeDef` that:
- Returns `interactive` if any alternative contains a complement type reference (T? or _?)
- Returns `output` otherwise

---

### Gap 2.4: Incomplete Predefined Types in TypeRef.builtins

**Spec requirement** (Section "Predefined Types"):
> The following types are predefined and cannot be redefined:
> 
> | Type | Complement | Description |
> |------|------------|-------------|
> | `_` | `_?` | Any produced/consumed term (wildcard) |
> | `Integer` | `Integer?` | Any integer literal |
> | `Real` | `Real?` | Any real (floating-point) literal |
> | `Number` | `Number?` | Any numeric literal (Integer or Real) |
> | `String` | `String?` | Any string literal |

**Implementation** (`type_ast.dart` line 53):
```dart
static const builtins = {'Number', 'String'};
```

Missing: `Integer`, `Real`

**Note**: `prelude.dart` has `predefinedTypeNames` which includes `Integer` and `Real`, so the redefinition check works. But `TypeRef.builtins` is used elsewhere and is incomplete.

**Required fix**: Update `TypeRef.builtins` to:
```dart
static const builtins = {'Integer', 'Real', 'Number', 'String'};
```

Or consider removing `TypeRef.builtins` and using only `predefinedTypeNames` from `prelude.dart`.

---

## 3. Module: moded-term

**Spec**: `moded-term.md` v0.6  
**Implementation**: `moded_term.dart`

### Gap 3.1: Missing Numeric Type Properties on ModedConstant

**Spec requirement** (ModedConstant class):
> ```dart
> class ModedConstant extends ModedTerm {
>   final Mode mode;
>   final Object value;
>   
>   bool get isInteger => value is int;
>   bool get isReal => value is double;
>   bool get isNumeric => value is num;
>   bool get isString => value is String && !isAtom;
>   bool get isAtom => /* atom detection logic */;
> }
> ```

**Implementation**: `ModedConstant` only has `mode` and `value` fields. No type-detection boolean properties exist.

**Required fix**: Add to `ModedConstant` class in `moded_term.dart`:
```dart
bool get isInteger => value is int;
bool get isReal => value is double;
bool get isNumeric => value is num;
bool get isString => value is String && _isQuotedString(value);
bool get isAtom => value is String && !_isQuotedString(value);

static bool _isQuotedString(Object value) {
  if (value is! String) return false;
  return (value.startsWith('"') && value.endsWith('"')) ||
         (value.startsWith("'") && value.endsWith("'"));
}
```

---

## 4. Module: moded-head

**Spec**: `moded-head.md` v0.7  
**Implementation**: `moded_head.dart`

### Status: ✅ CONSISTENT

The implementation correctly implements Definition 4.8:
- Step 1: Builds I/O moded term with structural modes from type
- Step 2: `_ensureVariablesMatchModes()` conditionally flips variables to match structural modes
- Handles interactive types via `_getSubtermModes()` and `_getListSubtermModes()`

---

## 5. Module: type-dfa

**Spec**: `type-dfa.md` v1.0  
**Implementation**: `program_dfa.dart`

### Gap 5.1: Missing DFAState.isProcedure Property

**Spec requirement** (DFAState class):
> ```dart
> class DFAState {
>   final bool isProcedure;     // true for procedure states
>   ...
> }
> ```

**Implementation**: `DFAState` has no `isProcedure` field.

**Required fix**: Add `final bool isProcedure;` field to `DFAState` constructor and update all instantiation sites:
- Procedure states: `isProcedure: true`
- Type states: `isProcedure: false`

---

### Gap 5.2: Missing DFAState.isUserDefinedType Property

**Spec requirement** (DFAState class):
> ```dart
> bool get isUserDefinedType => !isPrimitiveType && !isProcedure && !isAnonymousFinal;
> ```

**Implementation**: This computed property does not exist.

**Required fix**: Add to `DFAState`:
```dart
bool get isPrimitiveType => isWildcard || isIntegerType || isRealType || 
                            isNumberType || isStringType;
bool get isUserDefinedType => !isPrimitiveType && !isProcedure && !isAnonymousFinal;
```

Note: `isProcedure` must be added first (Gap 5.1).

---

### Gap 5.3: Missing DFAState.isNumericType Property

**Spec requirement** (DFAState class):
> ```dart
> bool get isNumericType => isIntegerType || isRealType || isNumberType;
> ```

**Implementation**: Individual properties exist (`isIntegerType`, `isRealType`, `isNumberType`) but the combined `isNumericType` does not.

**Required fix**: Add to `DFAState`:
```dart
bool get isNumericType => isIntegerType || isRealType || isNumberType;
```

---

## 6. Module: well-typed-term

**Spec**: `well-typed-term.md` v0.5  
**Implementation**: `well_typed_term.dart`

### Gap 6.1: Missing Automaton Switching at Type Boundaries

**Spec requirement** (Algorithm: Path Consistency Check):
> ```
> // IMPORTANT: If we transition to a different type, switch automata
> if nextState.isUserDefinedType && nextState.baseName != state.baseName:
>   currentAutomaton = dfa.getAutomaton(nextState.name)
> ```

**Implementation** (`checkPathAgainstAutomaton` function): Does NOT switch automata when crossing into different user-defined types. The function uses the initial `automaton` parameter throughout the entire path traversal.

**Impact**: When checking paths through nested compound types (e.g., `Stream(CounterCall)`), transitions into `CounterCall` won't find the correct transitions because we're still using the `Stream` automaton.

**Required fix**: Update `checkPathAgainstAutomaton` in `well_typed_term.dart`:
```dart
PathCheckResult checkPathAgainstAutomaton(
  ModedPath path,
  Automaton automaton,
  ProgramDFA dfa,
) {
  var state = automaton.startState;
  var currentAutomaton = automaton;  // Track current automaton

  // ... existing single-step handling ...

  for (int i = 0; i < path.length - 1; i++) {
    // ... existing label building ...

    final nextState = currentAutomaton.transition(state, label);

    if (nextState == null) {
      return PathCheckResult.inconsistent(
          'No transition for $label from state ${state.name}');
    }

    // Switch automata at type boundaries
    if (nextState.isUserDefinedType && nextState.baseName != state.baseName) {
      currentAutomaton = dfa.getAutomaton(nextState.name);
    }

    state = nextState;
  }

  return _checkLeafConsistencyForPath(path.leaf, state, dfa);
}
```

Note: Requires `isUserDefinedType` property from Gap 5.2.

---

### Gap 6.2: Missing Real Literal Detection in Path Step Conversion

**Spec requirement** (LeafTerm factories):
> ```dart
> factory LeafTerm.realConstant(double value)
> ```

**Implementation** (`_pathStepToLeafTerm` function):
```dart
final intVal = int.tryParse(value);
if (intVal != null) {
  return LeafTerm.integerConstant(intVal);
}
// ... no double.tryParse check ...
return LeafTerm.constant(value);  // Falls through to generic constant
```

Real literals (e.g., `3.14`) are not detected and will fail `Real` type checking.

**Required fix**: Update `_pathStepToLeafTerm` in `well_typed_term.dart`:
```dart
LeafTerm _pathStepToLeafTerm(PathStep step) {
  if (step.isVariable) {
    // ... existing variable handling ...
  } else {
    final value = step.symbol;
    
    // Check for integer
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

## 7. Module: well-typed-clause

**Spec**: `well-typed-clause.md` v0.7  
**Implementation**: `well_typed_clause.dart`

### Gap 7.1: Missing modedHead and modedBodyAtoms Fields in ClauseCheckResult

**Spec requirement** (ClauseCheckResult class):
> ```dart
> class ClauseCheckResult {
>   final bool isWellTyped;
>   final Map<String, VariableTypeInfo> variableTypes;
>   final List<ClauseError> errors;
>   
>   /// The moded head constructed for this clause
>   final ModedTerm? modedHead;
>   
>   /// The moded body atoms constructed for this clause
>   final List<ModedTerm> modedBodyAtoms;
> }
> ```

**Implementation**: `ClauseCheckResult` only has `isWellTyped`, `variableTypes`, and `errors`. The `modedHead` and `modedBodyAtoms` fields are missing.

**Required fix**: Add fields to `ClauseCheckResult` and populate them in `checkClause`:
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
  
  // Update factory constructors accordingly
}
```

---

## 8. Module: well-typed-program

**Spec**: `well-typed-program.md` v0.6  
**Implementation**: `type_checker.dart`

### Status: ✅ CONSISTENT (pending upstream fixes)

The implementation correctly implements:
- Covariance checking (Condition 1): via `_checkClauseCovariance`
- Contravariance checking (Condition 2): via `_checkInputCoverage` with structural coverage

Note: Correctness depends on fixes to upstream modules (especially Gap 6.1 automaton switching).

---

## Summary of Required Fixes

### Ordered by Module Dependency (test order)

| Priority | Module | Gap | Description |
|----------|--------|-----|-------------|
| 1 | type-environment | 2.1 | Type alias prohibition |
| 2 | type-environment | 2.2 | Determinism check |
| 3 | type-environment | 2.3 | TypeClassification enum |
| 4 | type-environment | 2.4 | TypeRef.builtins incomplete |
| 5 | moded-term | 3.1 | ModedConstant numeric properties |
| 6 | type-dfa | 5.1 | DFAState.isProcedure |
| 7 | type-dfa | 5.2 | DFAState.isUserDefinedType |
| 8 | type-dfa | 5.3 | DFAState.isNumericType |
| 9 | well-typed-term | 6.1 | Automaton switching at type boundaries |
| 10 | well-typed-term | 6.2 | Real literal detection |
| 11 | well-typed-clause | 7.1 | ClauseCheckResult moded term fields |

### Test Strategy

For each fix:
1. Write negative test (illegal input should fail)
2. Write positive test (legal input should pass)
3. Implement the fix
4. Verify both tests pass
5. Run full type checker test suite

---

## Appendix: Files to Modify

| File | Gaps |
|------|------|
| `type_ast.dart` | 2.3, 2.4 |
| `type_environment_builder.dart` | 2.1, 2.2 |
| `moded_term.dart` | 3.1 |
| `program_dfa.dart` | 5.1, 5.2, 5.3 |
| `well_typed_term.dart` | 6.1, 6.2 |
| `well_typed_clause.dart` | 7.1 |
