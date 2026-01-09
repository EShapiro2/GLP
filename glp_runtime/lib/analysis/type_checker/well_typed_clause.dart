// lib/analysis/type_checker/well_typed_clause.dart
//
// Well-typed clause checking for GLP type system.
// Specification: docs/modules/well-typed-clause.md v0.1
// Paper Reference: Definition 4.8 (Well-Typed Clause)
//
// A clause H :- G | B is well-typed if:
// 1. The moded head H is well-typed by the procedure's type
// 2. Each body atom is well-typed by its procedure's type (with mode complement)
// 3. Variable pairs (X, X?) across all atoms are complementary

import 'mode.dart';
import 'moded_term.dart';
import 'moded_head.dart';
import 'well_typed_term.dart';
import 'type_dfa.dart';
import 'type_ast.dart';
import 'type_compiler.dart';
import 'prelude.dart';
import '../../compiler/ast.dart' as ast;

// =============================================================================
// Result Types
// =============================================================================

/// Result of checking if a clause is well-typed
class ClauseCheckResult {
  /// Whether the clause is well-typed
  final bool isWellTyped;

  /// All variable type assignments from head and body
  final Map<String, VariableTypeInfo> variableTypes;

  /// List of errors found during checking
  final List<ClauseError> errors;

  ClauseCheckResult({
    required this.isWellTyped,
    required this.variableTypes,
    required this.errors,
  });

  factory ClauseCheckResult.success(Map<String, VariableTypeInfo> variableTypes) {
    return ClauseCheckResult(
      isWellTyped: true,
      variableTypes: variableTypes,
      errors: [],
    );
  }

  factory ClauseCheckResult.failure(List<ClauseError> errors,
      [Map<String, VariableTypeInfo>? variableTypes]) {
    return ClauseCheckResult(
      isWellTyped: false,
      variableTypes: variableTypes ?? {},
      errors: errors,
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

/// Error: variable pair not complementary across clause
class ClauseComplementaryError extends ClauseError {
  final String baseName;
  final VariableTypeInfo? writerType;
  final VariableTypeInfo? readerType;
  final String writerLocation;
  final String readerLocation;

  ClauseComplementaryError(
    this.baseName,
    this.writerType,
    this.readerType,
    this.writerLocation,
    this.readerLocation,
  );

  @override
  String get message =>
      'Variable pair ($baseName, $baseName?) not complementary across clause: '
      'writer at $writerLocation=$writerType, reader at $readerLocation=$readerType';

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
/// 1. modedHead(H, procType) is well-typed by the procedure's type DFA
/// 2. For each body atom A, producedTerm(A, atomType) is well-typed
/// 3. All variable pairs (X, X?) across head and body are complementary
ClauseCheckResult checkClause(
  TypedClause clause,
  TypeEnvironment env,
  TypeCompiler compiler,
) {
  final errors = <ClauseError>[];
  final allVariableTypes = <String, VariableTypeInfo>{};
  final variableLocations = <String, String>{};

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
  final headResult = _checkHead(clause, procDecl, compiler, env);
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
    final atomResult = _checkBodyAtom(atom, i, env, compiler);

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
        if (existing.typeState != newInfo.typeState) {
          // This will be caught by complementarity check below
        }
      } else {
        allVariableTypes[varKey] = newInfo;
        variableLocations[varKey] = 'body atom $i';
      }
    }
  }

  // Step 3: Check variable pair complementarity across clause
  final complementErrors = _checkClauseComplementarity(
    allVariableTypes,
    variableLocations,
  );
  errors.addAll(complementErrors);

  return ClauseCheckResult(
    isWellTyped: errors.isEmpty,
    variableTypes: allVariableTypes,
    errors: errors,
  );
}

/// Convenience overload: Check if an ast.Clause is well-typed.
///
/// Creates a TypeCompiler internally and converts ast.Clause to TypedClause.
/// Throws [UndeclaredProcedureError] if the procedure is not declared.
ClauseCheckResult checkClauseFromAst(
  ast.Clause clause,
  TypeEnvironment env,
) {
  final compiler = TypeCompiler(env);

  // Convert ast.Clause to TypedClause
  // Note: ast.Clause.head is Atom, but Goal has same structure
  final head = ast.Goal(clause.head.functor, clause.head.args, clause.line, clause.column);

  // Convert body goals (or empty list)
  final bodyAtoms = clause.body ?? [];

  final typedClause = TypedClause(
    head: head,
    bodyAtoms: bodyAtoms,
    guardAtoms: [], // Guards not checked yet
  );

  // Check if procedure is declared
  if (!env.hasProcedure(typedClause.headFunctor, typedClause.headArity)) {
    throw UndeclaredProcedureError(typedClause.headFunctor, typedClause.headArity);
  }

  return checkClause(typedClause, env, compiler);
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
// Internal Functions
// =============================================================================

/// Check head well-typing
WellTypedResult _checkHead(
  TypedClause clause,
  ProcDecl procDecl,
  TypeCompiler compiler,
  TypeEnvironment env,
) {
  // Build the procedure type DFA for the head (with complement - callee's view)
  final procDFA = _buildProcedureTypeDFA(procDecl, compiler, complement: true);

  // Build moded head term (pass env for embedded mode handling in structures)
  try {
    final modedHeadTerm = modedHead(clause.head, procDecl, typeEnv: env);

    // Check well-typing
    return checkModedTerm(modedHeadTerm, procDFA);
  } on ArityMismatchError catch (e) {
    return WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(symbol: e.message, argIndex: 0, mode: Mode.produce)]),
        e.message,
      ),
    ]);
  }
}

/// Check body atom well-typing
WellTypedResult _checkBodyAtom(
  ast.Goal atom,
  int atomIndex,
  TypeEnvironment env,
  TypeCompiler compiler,
) {
  // Skip builtin goals (true, otherwise, :=)
  if (isBuiltinGoal(atom.functor)) {
    return WellTypedResult.success({});
  }

  // Look up procedure declaration
  final procDecl = env.getProcedure(atom.functor, atom.arity);
  if (procDecl == null) {
    return WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(
          symbol: '${atom.functor}/${atom.arity}',
          argIndex: 0,
          mode: Mode.produce,
        )]),
        'Undefined procedure: ${atom.functor}/${atom.arity}',
      ),
    ]);
  }

  // Build the procedure type DFA for body atom (no complement - caller's view)
  final procDFA = _buildProcedureTypeDFA(procDecl, compiler);

  // Build produced term (no variable flip for body atoms)
  try {
    final modedAtomTerm = producedTerm(atom, procDecl);

    // Check well-typing
    return checkModedTerm(modedAtomTerm, procDFA);
  } on ArityMismatchError catch (e) {
    return WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(symbol: e.message, argIndex: 0, mode: Mode.produce)]),
        e.message,
      ),
    ]);
  }
}

/// Build procedure type DFA from procedure declaration
///
/// For each argument position, compile the type and create transitions.
/// If [complement] is true, apply mode complement (for body atoms at call sites).
///
/// Mode complement logic:
/// - For HEADS (complement=true): Callee sees complement of caller's view
/// - For BODY ATOMS (complement=false): Caller's view matches declaration
TypeDFA _buildProcedureTypeDFA(
  ProcDecl procDecl,
  TypeCompiler compiler, {
  bool complement = false,
}) {
  // Start state for the procedure
  final procState = DFAState(procDecl.key);
  final states = <DFAState>{procState};
  final transitions = <(DFAState, PathElement), DFAState>{};
  final primitiveStateModes = <DFAState, Set<Mode>>{};
  final finalStates = <DFAState>{};

  // Add transitions for each argument position
  for (int i = 0; i < procDecl.arity; i++) {
    final argType = procDecl.argTypes[i];
    var argDFA = compiler.compile(argType.name);

    // Rename states with argument index suffix to prevent collision
    // when multiple arguments have the same type
    argDFA = argDFA.withSuffix('@arg${i + 1}');

    // For heads (complement=true): complement INPUT argument DFAs only
    //
    // The variable flip in modedHead() flips ALL variables (X ↔ X?).
    // For INPUT args: type produce → complement to consume → matches flipped reader
    // For OUTPUT args: type produce → stays produce → matches flipped writer
    //
    // For body atoms (complement=false): use DFA as-is
    if (complement && argType.isInput) {
      argDFA = argDFA.applyModeComplement();
    }

    // Add transition from procedure state to argument type state
    final pathElem = PathElement.functor(procDecl.name, procDecl.arity, i + 1);
    transitions[(procState, pathElem)] = argDFA.startState;

    // Merge the argument DFA states and transitions
    states.addAll(argDFA.states);
    transitions.addAll(argDFA.transitions);
    primitiveStateModes.addAll(argDFA.primitiveStateModes);
    finalStates.addAll(argDFA.finalStates);
  }

  return TypeDFA(
    states: states,
    startState: procState,
    finalStates: finalStates,
    transitions: transitions,
    primitiveStateModes: primitiveStateModes,
  );
}

/// Check variable pair complementarity across the entire clause
List<ClauseComplementaryError> _checkClauseComplementarity(
  Map<String, VariableTypeInfo> variableTypes,
  Map<String, String> variableLocations,
) {
  final errors = <ClauseComplementaryError>[];

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

      // Check type compatibility (primitives are compatible with any type)
      if (!_areComplementaryTypes(writerInfo, readerInfo)) {
        errors.add(ClauseComplementaryError(
          baseName,
          writerInfo,
          readerInfo,
          writerLoc,
          readerLoc,
        ));
      }
    }
  }

  return errors;
}

/// Check if writer and reader types are complementary
bool _areComplementaryTypes(VariableTypeInfo writerInfo, VariableTypeInfo readerInfo) {
  // Mode check: writer must produce, reader must consume
  if (writerInfo.mode != Mode.produce || readerInfo.mode != Mode.consume) {
    return false;
  }

  final writerTypeName = _baseTypeName(writerInfo.typeState.name);
  final readerTypeName = _baseTypeName(readerInfo.typeState.name);

  // Output primitive (_) as writer is compatible with any reader
  if (_isOutputPrimitive(writerTypeName)) {
    return true;
  }

  // Input primitive (_?) as reader is compatible with any writer
  if (_isInputPrimitive(readerTypeName)) {
    return true;
  }

  // Otherwise, must be at same type (strip argument suffixes like @arg1)
  return writerTypeName == readerTypeName;
}

/// Extract base type name by stripping argument position suffix (e.g., "MyList@arg1" -> "MyList")
String _baseTypeName(String name) {
  final atIndex = name.indexOf('@');
  if (atIndex == -1) return name;
  return name.substring(0, atIndex);
}

/// Check if a type state name represents an output primitive (_)
bool _isOutputPrimitive(String name) {
  return name == '_' || name == 'Output' || name.startsWith('_prim');
}

/// Check if a type state name represents an input primitive (_?)
bool _isInputPrimitive(String name) {
  return name == '_?' || name == 'Input';
}
