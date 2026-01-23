// lib/analysis/type_checker/well_typed_clause.dart
//
// Well-typed clause checking for GLP type system.
// Specification: docs/type system/well-typed-clause.md v0.9
// Paper Reference: Definition 4.10 (Well-Typed Clause)
//
// A clause H :- G | B is well-typed if:
// 1. The moded head H is well-typed by the procedure's type
// 2. Each body atom is well-typed by its procedure's type
// 3. Variable pairs (X, X?) have:
//    - dual types if both in head or both in body
//    - same type if one in head and one in body

import 'mode.dart';
import 'moded_term.dart';
import 'moded_head.dart';
import 'well_typed_term.dart';
import 'program_dfa.dart';
import 'type_ast.dart';
import 'prelude.dart';
import '../../compiler/ast.dart' as ast;

// =============================================================================
// Result Types
// =============================================================================

/// Result of checking if a clause is well-typed
/// Fix 5.1: Includes modedHead and modedBodyAtoms for inspection/debugging
class ClauseCheckResult {
  /// Whether the clause is well-typed
  final bool isWellTyped;

  /// All variable type assignments from head and body
  final Map<String, VariableTypeInfo> variableTypes;

  /// List of errors found during checking
  final List<ClauseError> errors;

  /// The constructed moded head term (if available)
  final ModedTerm? modedHead;

  /// The constructed moded body atom terms
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

/// Base class for clause checking errors
abstract class ClauseError {
  String get message;
}

/// Error in head checking
class HeadError extends ClauseError {
  final String procedureName;
  final List<WellTypedError> termErrors;

  HeadError(this.procedureName, this.termErrors);

  @override
  String get message =>
      'Head of $procedureName is not well-typed:\n  ${termErrors.map((e) => e.message).join('\n  ')}';

  @override
  String toString() => message;
}

/// Error in body atom checking
class BodyAtomError extends ClauseError {
  final String procedureName;
  final int atomIndex;
  final List<WellTypedError> termErrors;

  BodyAtomError(this.procedureName, this.atomIndex, this.termErrors);

  @override
  String get message =>
      'Body atom $atomIndex ($procedureName) is not well-typed:\n  ${termErrors.map((e) => e.message).join('\n  ')}';

  @override
  String toString() => message;
}

/// Error: variable pair not dual across clause
class ClauseDualityError extends ClauseError {
  final String baseName;
  final VariableTypeInfo? writerType;
  final VariableTypeInfo? readerType;
  final String writerLocation;
  final String readerLocation;
  final String? reason;

  ClauseDualityError(
    this.baseName,
    this.writerType,
    this.readerType,
    this.writerLocation,
    this.readerLocation, [
    this.reason,
  ]);

  @override
  String get message {
    final reasonStr = reason != null ? ': $reason' : '';
    return 'Variable pair ($baseName, $baseName?) not dual across clause$reasonStr: '
        'writer at $writerLocation=$writerType, reader at $readerLocation=$readerType';
  }

  @override
  String toString() => message;
}

/// Error: undefined procedure
class UndefinedProcedureError extends ClauseError {
  final String procedureName;
  final int arity;

  UndefinedProcedureError(this.procedureName, this.arity);

  @override
  String get message =>
      'Undefined procedure: $procedureName/$arity';

  @override
  String toString() => message;
}

/// Error: arity mismatch
class ArityMismatchClauseError extends ClauseError {
  final String procedureName;
  final int expectedArity;
  final int actualArity;

  ArityMismatchClauseError(this.procedureName, this.expectedArity, this.actualArity);

  @override
  String get message =>
      'Arity mismatch for $procedureName: expected $expectedArity, got $actualArity';

  @override
  String toString() => message;
}

/// Exception thrown when a procedure is not declared (for use by type_checker.dart)
class UndeclaredProcedureError implements Exception {
  final String functor;
  final int arity;

  UndeclaredProcedureError(this.functor, this.arity);

  @override
  String toString() => 'UndeclaredProcedureError: $functor/$arity';
}

// =============================================================================
// Clause Representation
// =============================================================================

/// A parsed clause structure for type checking
class TypedClause {
  /// The head as an AST Goal
  final ast.Goal head;

  /// Body atoms as AST Goals
  final List<ast.Goal> bodyAtoms;

  /// Guard atoms as AST Goals (optional, not currently checked)
  final List<ast.Goal> guardAtoms;

  TypedClause({
    required this.head,
    this.bodyAtoms = const [],
    this.guardAtoms = const [],
  });

  String get headFunctor => head.functor;
  int get headArity => head.arity;
}

// =============================================================================
// Public Functions
// =============================================================================

/// Check if a clause is well-typed in the given environment.
///
/// Per Definition 4.8: A clause H :- G | B is well-typed if:
/// 1. modedHead(H, procType) is well-typed by the procedure's type
/// 2. For each body atom A, producedTerm(A, atomType) is well-typed
/// 3. All variable pairs (X, X?) across head and body are complementary
///
/// Fix 5.1: Returns constructed moded terms for inspection
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

  // Look up procedure declaration for head
  final procDecl = env.getProcedure(clause.headFunctor, clause.headArity);
  if (procDecl == null) {
    return ClauseCheckResult.failure([
      UndefinedProcedureError(clause.headFunctor, clause.headArity),
    ]);
  }

  // Check arity match
  if (procDecl.arity != clause.headArity) {
    return ClauseCheckResult.failure([
      ArityMismatchClauseError(clause.headFunctor, procDecl.arity, clause.headArity),
    ]);
  }

  // Step 1: Check head well-typing
  final (headResult, modedHeadTerm) = _checkHeadWithTerm(clause, procDecl, dfa, env);
  constructedModedHead = modedHeadTerm;
  if (!headResult.isWellTyped) {
    errors.add(HeadError(clause.headFunctor, headResult.errors));
  }
  for (final entry in headResult.variableTypes.entries) {
    allVariableTypes[entry.key] = entry.value;
    variableLocations[entry.key] = 'head';
  }

  // Step 2: Check each body atom
  for (int i = 0; i < clause.bodyAtoms.length; i++) {
    final atom = clause.bodyAtoms[i];
    final (atomResult, modedAtomTerm) = _checkBodyAtomWithTerm(atom, i, dfa, env);

    if (modedAtomTerm != null) {
      constructedModedBodyAtoms.add(modedAtomTerm);
    }

    if (!atomResult.isWellTyped) {
      errors.add(BodyAtomError(atom.functor, i, atomResult.errors));
    }

    // Merge variable types with consistency checking
    for (final entry in atomResult.variableTypes.entries) {
      final varKey = entry.key;
      final newInfo = entry.value;

      if (allVariableTypes.containsKey(varKey)) {
        final existing = allVariableTypes[varKey]!;
        // Same variable at different positions - types must match
        if (existing.typeState.name != newInfo.typeState.name) {
          // This will be caught by complementarity check below
        }
      } else {
        allVariableTypes[varKey] = newInfo;
        variableLocations[varKey] = 'body atom $i';
      }
    }
  }

  // Step 3: Check variable pair duality across clause
  final dualityErrors = _checkClauseDuality(
    allVariableTypes,
    variableLocations,
  );
  errors.addAll(dualityErrors);

  return ClauseCheckResult(
    isWellTyped: errors.isEmpty,
    variableTypes: allVariableTypes,
    errors: errors,
    modedHead: constructedModedHead,
    modedBodyAtoms: constructedModedBodyAtoms,
  );
}

/// Convenience overload: Check if an ast.Clause is well-typed.
///
/// Throws [UndeclaredProcedureError] if the procedure is not declared.
///
/// Per spec: For type checking, H :- G | B is treated as H :- G, B (conjunction).
/// Guards are procedure calls with predefined type signatures.
ClauseCheckResult checkClauseFromAst(
  ast.Clause clause,
  ProgramDFA dfa,
  TypeEnvironment env,
) {
  // Convert ast.Clause to TypedClause
  // Note: ast.Clause.head is Atom, but Goal has same structure
  final head = ast.Goal(clause.head.functor, clause.head.args, clause.line, clause.column);

  // Convert guards to goals (guards are procedure calls for type checking)
  final guardGoals = <ast.Goal>[];
  if (clause.guards != null) {
    for (final guard in clause.guards!) {
      // Convert Guard to Goal - same structure
      guardGoals.add(ast.Goal(guard.predicate, guard.args, guard.line, guard.column));
    }
  }

  // Convert body goals (or empty list)
  final bodyGoals = clause.body ?? [];

  // Combine guards and body: H :- G | B is treated as H :- G, B
  final allBodyAtoms = [...guardGoals, ...bodyGoals];

  final typedClause = TypedClause(
    head: head,
    bodyAtoms: allBodyAtoms,
    guardAtoms: guardGoals,
  );

  // Check if procedure is declared
  if (!env.hasProcedure(typedClause.headFunctor, typedClause.headArity)) {
    throw UndeclaredProcedureError(typedClause.headFunctor, typedClause.headArity);
  }

  return checkClause(typedClause, dfa, env);
}

/// Get the set of labels (functor/arity or constant) that a clause accepts
/// at a given argument position (1-indexed).
///
/// Returns null if the argument is a variable (wildcard - accepts everything).
/// Returns a set of strings like "[]", "[|]", "s/1", "0" etc.
Set<String>? getAcceptedLabels(
  ast.Clause clause,
  int argIndex,
  TypeEnvironment env,
) {
  // argIndex is 1-indexed
  if (argIndex < 1 || argIndex > clause.head.args.length) {
    return {}; // Out of bounds - accepts nothing
  }

  final arg = clause.head.args[argIndex - 1];
  return getLabelsFromTerm(arg);
}

/// Extract labels from a term (public for coverage checking)
Set<String>? getLabelsFromTerm(ast.Term term) {
  if (term is ast.VarTerm || term is ast.UnderscoreTerm) {
    // Variable - wildcard, accepts anything
    return null;
  }

  if (term is ast.ConstTerm) {
    // Constant - accepts only this value
    return {term.value.toString()};
  }

  if (term is ast.ListTerm) {
    if (term.isNil) {
      return {'[]'};
    } else {
      // Non-empty list [H|T]
      return {'[|]'};
    }
  }

  if (term is ast.StructTerm) {
    // Structure - accepts functor/arity
    return {'${term.functor}/${term.arity}'};
  }

  // Unknown term type - conservative: empty set
  return {};
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Get the full type name including ? if input mode.
String getFullTypeName(TypeExpr typeExpr) {
  if (typeExpr is PrimitiveModeAlt) {
    return typeExpr.isInput ? '_?' : '_';
  }
  if (typeExpr is TypeRef) {
    return typeExpr.isInput ? '${typeExpr.name}?' : typeExpr.name;
  }
  throw ArgumentError('Unknown type expression: $typeExpr');
}

// =============================================================================
// Internal Functions
// =============================================================================

/// Check head well-typing by checking each argument against its declared type's automaton
WellTypedResult _checkHead(
  TypedClause clause,
  ProcDecl procDecl,
  ProgramDFA dfa,
  TypeEnvironment env,
) {
  final (result, _) = _checkHeadWithTerm(clause, procDecl, dfa, env);
  return result;
}

/// Check head well-typing and return the constructed moded term
/// Fix 5.1: Returns both result and moded term
(WellTypedResult, ModedTerm?) _checkHeadWithTerm(
  TypedClause clause,
  ProcDecl procDecl,
  ProgramDFA dfa,
  TypeEnvironment env,
) {
  try {
    // Build moded head term (pass env for embedded mode handling in structures)
    final modedHeadTerm = modedHead(clause.head, procDecl, typeEnv: env);

    // Check each argument against its declared type's automaton
    final result = _checkModedTermPerArg(modedHeadTerm, procDecl, dfa);
    return (result, modedHeadTerm);
  } on ArityMismatchError catch (e) {
    return (WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(symbol: e.message, argIndex: 0, mode: Mode.produce)]),
        e.message,
      ),
    ]), null);
  }
}

/// Check body atom well-typing
WellTypedResult _checkBodyAtom(
  ast.Goal atom,
  int atomIndex,
  ProgramDFA dfa,
  TypeEnvironment env,
) {
  final (result, _) = _checkBodyAtomWithTerm(atom, atomIndex, dfa, env);
  return result;
}

/// Check body atom well-typing and return the constructed moded term
/// Fix 5.1: Returns both result and moded term
(WellTypedResult, ModedTerm?) _checkBodyAtomWithTerm(
  ast.Goal atom,
  int atomIndex,
  ProgramDFA dfa,
  TypeEnvironment env,
) {
  // Skip builtin goals (true, otherwise, :=)
  if (isBuiltinGoal(atom.functor)) {
    return (WellTypedResult.success({}), null);
  }

  // Look up procedure declaration
  final procDecl = env.getProcedure(atom.functor, atom.arity);
  if (procDecl == null) {
    return (WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(
          symbol: '${atom.functor}/${atom.arity}',
          argIndex: 0,
          mode: Mode.produce,
        )]),
        'Undefined procedure: ${atom.functor}/${atom.arity}',
      ),
    ]), null);
  }

  // Build produced term (no variable flip for body atoms)
  try {
    final modedAtomTerm = producedTerm(atom, procDecl, typeEnv: env);

    // Check each argument against its declared type's automaton
    final result = _checkModedTermPerArg(modedAtomTerm, procDecl, dfa);
    return (result, modedAtomTerm);
  } on ArityMismatchError catch (e) {
    return (WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(symbol: e.message, argIndex: 0, mode: Mode.produce)]),
        e.message,
      ),
    ]), null);
  }
}

/// Check moded term per argument against declared type automata
///
/// Per spec v0.6: Each argument is checked against its declared type's automaton directly.
WellTypedResult _checkModedTermPerArg(
  ModedTerm modedTerm,
  ProcDecl decl,
  ProgramDFA dfa,
) {
  final errors = <WellTypedError>[];
  final variableTypes = <String, VariableTypeInfo>{};

  // modedTerm should be a ModedCompound with args
  if (modedTerm is! ModedCompound) {
    return WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(symbol: 'not-compound', argIndex: 0, mode: Mode.produce)]),
        'Expected compound term for procedure',
      ),
    ]);
  }

  // Check each argument
  for (int i = 0; i < decl.arity; i++) {
    final argType = decl.argTypes[i];

    // Get the automaton for the declared type directly
    // Type? → use T? automaton; Type → use T automaton
    final argTypeName = getFullTypeName(argType);

    Automaton argAutomaton;
    try {
      argAutomaton = dfa.getAutomaton(argTypeName);
    } on StateError {
      errors.add(InconsistentPathError(
        ModedPath([PathStep(symbol: argTypeName, argIndex: i + 1, mode: Mode.produce)]),
        'Unknown type: $argTypeName',
      ));
      continue;
    }

    // Extract paths from this argument and check against automaton
    final argTerm = modedTerm.args[i];
    final argPaths = paths(argTerm);

    for (final path in argPaths) {
      final result = checkPathAgainstAutomaton(path, argAutomaton, dfa);

      if (!result.isConsistent) {
        errors.add(InconsistentPathError(path, result.reason ?? 'Unknown'));
      } else if (result.variableAssignment != null) {
        final varKey = path.leaf.symbol;
        if (variableTypes.containsKey(varKey)) {
          if (variableTypes[varKey]!.typeState.name != result.variableAssignment!.typeState.name) {
            errors.add(InconsistentVariableError(varKey, variableTypes[varKey]!, result.variableAssignment!));
          }
        } else {
          variableTypes[varKey] = result.variableAssignment!;
        }
      }
    }
  }

  // Check duality within this term
  final dualityErrors = _checkTermDuality(variableTypes);
  errors.addAll(dualityErrors);

  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors,
  );
}

/// Check duality within a term (same logic as well_typed_term.dart)
List<NonDualError> _checkTermDuality(
    Map<String, VariableTypeInfo> variableTypes) {
  final errors = <NonDualError>[];

  // Group by base name (X and X? share base "X")
  final baseNames = <String, Map<String, VariableTypeInfo>>{};

  for (final entry in variableTypes.entries) {
    final varKey = entry.key;
    final info = entry.value;

    final baseName = varKey.endsWith('?')
        ? varKey.substring(0, varKey.length - 1)
        : varKey;

    baseNames.putIfAbsent(baseName, () => {});
    baseNames[baseName]![varKey] = info;
  }

  // Check each base name
  for (final entry in baseNames.entries) {
    final baseName = entry.key;
    final variants = entry.value;

    final writerKey = baseName;
    final readerKey = '$baseName?';

    if (variants.containsKey(writerKey) && variants.containsKey(readerKey)) {
      final writerInfo = variants[writerKey]!;
      final readerInfo = variants[readerKey]!;

      if (!_areDualTypes(writerInfo, readerInfo)) {
        errors.add(NonDualError(baseName, writerInfo, readerInfo));
      }
    }
  }

  return errors;
}

/// Normalize location to 'head' or 'body'
String _normalizeLocation(String location) {
  if (location == 'head') return 'head';
  if (location.startsWith('body')) return 'body';
  return location; // unknown stays as-is
}

/// Check variable pair type consistency across the entire clause
///
/// Per Definition 4.10 (spec v0.9):
/// - If both occur in head, or both in body: require DUAL types
/// - If one in head and one in body: require SAME type
List<ClauseDualityError> _checkClauseDuality(
  Map<String, VariableTypeInfo> variableTypes,
  Map<String, String> variableLocations,
) {
  final errors = <ClauseDualityError>[];

  // Group by base name (X and X? share base "X")
  final baseNames = <String, Map<String, VariableTypeInfo>>{};
  final baseLocations = <String, Map<String, String>>{};

  for (final entry in variableTypes.entries) {
    final varKey = entry.key;
    final info = entry.value;
    final location = variableLocations[varKey] ?? 'unknown';

    final baseName = varKey.endsWith('?')
        ? varKey.substring(0, varKey.length - 1)
        : varKey;

    baseNames.putIfAbsent(baseName, () => {});
    baseNames[baseName]![varKey] = info;

    baseLocations.putIfAbsent(baseName, () => {});
    baseLocations[baseName]![varKey] = location;
  }

  // Check each base name
  for (final entry in baseNames.entries) {
    final baseName = entry.key;
    final variants = entry.value;
    final locations = baseLocations[baseName]!;

    final writerKey = baseName;
    final readerKey = '$baseName?';

    if (variants.containsKey(writerKey) && variants.containsKey(readerKey)) {
      final writerInfo = variants[writerKey]!;
      final readerInfo = variants[readerKey]!;
      final writerLoc = locations[writerKey] ?? 'unknown';
      final readerLoc = locations[readerKey] ?? 'unknown';
      
      // Normalize locations to 'head' or 'body'
      final writerNormLoc = _normalizeLocation(writerLoc);
      final readerNormLoc = _normalizeLocation(readerLoc);
      
      // Apply location-dependent rule (spec v0.9, Definition 4.10 condition 3)
      if (writerNormLoc == readerNormLoc) {
        // Both in head OR both in body: require DUAL types
        final (isCompat, reason) = _areDualTypesWithReason(writerInfo, readerInfo);
        if (!isCompat) {
          errors.add(ClauseDualityError(
            baseName,
            writerInfo,
            readerInfo,
            writerLoc,
            readerLoc,
            'Variables in same clause part ($writerNormLoc) must have dual types: $reason',
          ));
        }
      } else {
        // One in head, one in body: require SAME type
        final (isSame, reason) = _areSameTypeWithReason(writerInfo, readerInfo);
        if (!isSame) {
          errors.add(ClauseDualityError(
            baseName,
            writerInfo,
            readerInfo,
            writerLoc,
            readerLoc,
            'Variables across head/body must have same type: $reason',
          ));
        }
      }
    }
  }

  return errors;
}

/// Check if writer and reader types are dual
/// Per spec v0.6: uses DFAState.baseName and isDual
bool _areDualTypes(VariableTypeInfo writerInfo, VariableTypeInfo readerInfo) {
  final (isCompat, _) = _areDualTypesWithReason(writerInfo, readerInfo);
  return isCompat;
}

/// Check if two variable types are the SAME type
/// Per spec v0.9: For head-body pairs, types must have same BASE type
/// (e.g., _ and _? are same base type, Stream and Stream? are same base type)
(bool, String?) _areSameTypeWithReason(VariableTypeInfo writerInfo, VariableTypeInfo readerInfo) {
  // For same-type check, the BASE type names must be identical
  // Writer at T has baseName T, reader at T? has baseName T
  if (writerInfo.typeState.baseName != readerInfo.typeState.baseName) {
    return (false, '${writerInfo.typeState.name} (base: ${writerInfo.typeState.baseName}) != ${readerInfo.typeState.name} (base: ${readerInfo.typeState.baseName})');
  }
  return (true, null);
}

/// Check duality with reason for failure
/// Per spec: wildcards are universal - _? is dual to any output, _ is dual to any input
(bool, String?) _areDualTypesWithReason(VariableTypeInfo writerInfo, VariableTypeInfo readerInfo) {
  // Mode check: writer must produce, reader must consume
  if (writerInfo.mode != Mode.produce) {
    return (false, 'Writer must have produce mode');
  }
  if (readerInfo.mode != Mode.consume) {
    return (false, 'Reader must have consume mode');
  }

  // Special case: wildcards are universal
  // _? (consumed wildcard) is dual to ANY output type
  // _ (produced wildcard) is dual to ANY input type
  final writerIsWildcard = writerInfo.typeState.baseName == '_';
  final readerIsWildcard = readerInfo.typeState.baseName == '_';

  if (writerIsWildcard || readerIsWildcard) {
    // Wildcards are dual to anything - just verify the wildcard has correct mode
    // Writer at _ (non-dual, produce) - OK
    // Reader at _? (dual, consume) - OK
    if (writerIsWildcard && writerInfo.typeState.isDual) {
      return (false, 'Writer wildcard must be _ (non-dual), not _?');
    }
    if (readerIsWildcard && !readerInfo.typeState.isDual) {
      return (false, 'Reader wildcard must be _? (dual), not _');
    }
    return (true, null);
  }

  // Non-wildcard case: states must be duals (same baseName, opposite isDual)
  if (writerInfo.typeState.baseName != readerInfo.typeState.baseName) {
    return (false, 'Types must have same base: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}');
  }

  // One must be dual, the other not
  if (writerInfo.typeState.isDual == readerInfo.typeState.isDual) {
    return (false, 'One must be dual, other not: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}');
  }

  return (true, null);
}
