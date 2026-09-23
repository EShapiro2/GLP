// lib/analysis/type_checker/well_typed_clause.dart
//
// Well-typed clause checking for GLP type system.
// Specification: TGLP (Moded-Types), sections/well-typing.tex Definition "Well-Typed Clause"
// Paper Reference: Definition 5.7 (Well-Typed Clause)
//
// A clause H :- G | B is well-typed if:
// 1. The moded head H is well-typed by the procedure's type
// 2. Each body atom is well-typed by its procedure's type
// 3. Variable pairs (X, X?) have:
//    - dual types if both occur in the head
//    - writer a subtype of the dual of the reader if both occur in the body
//    - head occurrence's dual a subtype of the body occurrence's dual if one
//      occurs in each (def:well-typed-clause-subtyping 3(b), relaxed from
//      equality in Moded-Types a5221fd)

import 'mode.dart';
import 'moded_term.dart';
import 'moded_head.dart';
import 'well_typed_term.dart';
import 'program_dfa.dart';
import 'subtyping.dart';
import 'type_ast.dart';
import 'root_scope.dart';
import 'param_expansion.dart';
import '../../compiler/ast.dart' as ast;

// =============================================================================
// Per-instantiation collection (clause-template rule)
// =============================================================================

/// What call-site instantiation needs of the checker above it.
///
/// [of] gives the defining clauses of a procedure by "name/arity", or null where
/// it is defined outside the unit being checked.  [verify] answers whether a
/// candidate instantiation satisfies the two conditions TGLP def:instantiation
/// places on the callee: its clauses are well-typed by the expanded declaration,
/// and every input path of that declaration is accepted by some clause
/// (def:input-accepting-clause).
class CalleeClauses {
  final List<ast.Clause>? Function(String procKey) of;
  final bool Function(
          ProcDecl decl, TypeEnvironment env, List<ast.Clause> clauses) verify;
  const CalleeClauses(this.of, this.verify);
}

/// A concrete instantiation of a parameterized procedure, inferred at a call
/// site (Case B). Carries the monomorphic declaration the instantiation
/// produces plus the scope (env/dfa) in which it was inferred — that scope
/// already holds the concrete types, so the parameterized procedure's defining
/// clauses can be re-checked against this instantiation without any merged env.
/// See TGLP (Moded-Types), sections/parameterized-types.tex (Parameterised
/// Procedure Declarations)
/// and docs/glp-a5-stage-b-plan.md Step 1.
class CollectedInstantiation {
  final String procKey;        // "name/arity" of the parameterized procedure
  final ProcDecl monoDecl;     // concrete declaration this instantiation produces
  final TypeEnvironment env;   // caller scope where inferred (holds concrete types)
  final ProgramDFA dfa;        // caller DFA (built from env)

  CollectedInstantiation(this.procKey, this.monoDecl, this.env, this.dfa);

  /// Concrete-argument signature, for deduplication of identical instantiations.
  String get signature => monoDecl.argTypes.map(getFullTypeName).join(',');
}

/// Accumulates the distinct call-site instantiations of parameterized
/// procedures across a program's modules. Deduplicated by (procKey, signature)
/// so each distinct instantiation is checked once in Phase 2.
class InstantiationCollector {
  final Map<String, CollectedInstantiation> _byKeySig = {};

  void record(CollectedInstantiation inst) {
    _byKeySig.putIfAbsent('${inst.procKey}#${inst.signature}', () => inst);
  }

  Iterable<CollectedInstantiation> get all => _byKeySig.values;
}

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
  TypeEnvironment env, {
  InstantiationCollector? collector,
  Map<String, ProcDecl> activeInstantiations = const {},
  CalleeClauses? callee,
}) {
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
    final (atomResult, modedAtomTerm) = _checkBodyAtomWithTerm(atom, i, dfa, env,
        callerVarTypes: allVariableTypes, collector: collector,
        activeInstantiations: activeInstantiations,
        callee: callee);

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
    dfa,
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
  TypeEnvironment env, {
  InstantiationCollector? collector,
  Map<String, ProcDecl> activeInstantiations = const {},
  CalleeClauses? callee,
}) {
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

  return checkClause(typedClause, dfa, env, collector: collector,
      activeInstantiations: activeInstantiations,
      callee: callee);
}

/// The base names of the variables of [clause] whose type at some occurrence
/// admits only ground terms (TGLP `typed-glp.tex`, \mypara{Readers of ground
/// types}): "A reader whose type admits only ground terms may occur more than
/// once in a clause, its paired writer occurring once: a ground value contains
/// no writer, so no occurrence of it produces a second one ... and the
/// relaxation holds wherever the occurrences sit --- in the head, nested within
/// an argument, or in the body."
///
/// So the question is asked of EVERY occurrence, and of the type the occurrence
/// has (Definition "Type Assignment": the state the automaton reaches by the
/// path from the root to that position), not of the top-level type name of a
/// head argument.  A head occurrence's type comes off the moded head
/// (Definition "Moded Head"), a body occurrence's off the produced moded term of
/// its unit goal, and a guard's off the guard atom, guards being type-checked as
/// a conjunction with the body.  One occurrence carrying a ground-admitting type
/// settles it: the type of any occurrence bounds the values the variable may
/// carry, and a bound admitting only ground terms makes the value ground.
///
/// The key is the BASE name --- the moded head carries `X` at the key `X?` and
/// `X?` at the key `X` (Definition "Moded Head", step 2), and SRSW counts a
/// variable and its pair together.
///
/// Errors are not collected: this is asked of clauses the checker has passed or
/// will reject on its own, and an occurrence whose path is inconsistent simply
/// yields no type and licenses nothing.
Set<String> groundTypedVariables(
    ast.Clause clause, ProgramDFA dfa, TypeEnvironment env) {
  final procDecl = env.getProcedure(clause.head.functor, clause.head.args.length);
  if (procDecl == null) return const {};

  final head = ast.Goal(
      clause.head.functor, clause.head.args, clause.line, clause.column);
  final guardGoals = [
    for (final g in clause.guards ?? const <ast.Guard>[])
      ast.Goal(g.predicate, g.args, g.line, g.column)
  ];
  final typedClause = TypedClause(
    head: head,
    bodyAtoms: [...guardGoals, ...(clause.body ?? const <ast.Goal>[])],
    guardAtoms: guardGoals,
  );

  final ground = <String>{};
  void take(Map<String, VariableTypeInfo> types) {
    for (final entry in types.entries) {
      if (!admitsOnlyGroundTerms(entry.value.typeState, dfa, env.types)) continue;
      final key = entry.key;
      ground.add(key.endsWith('?') ? key.substring(0, key.length - 1) : key);
    }
  }

  final (headResult, _) = _checkHeadWithTerm(typedClause, procDecl, dfa, env);
  take(headResult.variableTypes);

  for (final atom in typedClause.bodyAtoms) {
    take(_bodyAtomVariableTypes(atom, dfa, env));
  }

  return ground;
}

/// The types [atom]'s variable occurrences have, as a body unit goal: the
/// produced moded term of the goal, checked per argument against the declaration
/// in scope (Definition "Well-Typed Clause" condition 2).
///
/// The declaration in scope is the MONOMORPHIC one, which for a parameterised
/// procedure is its wildcard instantiation (param_expansion.dart, step 5): every
/// position the type parameter does not reach keeps its declared type, and every
/// position it does reach becomes `_`, which admits more than ground terms and so
/// licenses nothing.  That is what this is for --- call-site instantiation is the
/// closure's, and a call to a parameterised procedure contributes no variable
/// type at all in the pass that runs before it (the inferred instantiation names
/// types this DFA has not materialised), so asking the closure here would answer
/// nothing where the wildcard declaration answers `Integer` for
/// `measure(Stream(X)?, Integer, Stream(X))`'s second argument.
Map<String, VariableTypeInfo> _bodyAtomVariableTypes(
    ast.Goal atom, ProgramDFA dfa, TypeEnvironment env) {
  if (atom is ast.SpawnGoal) {
    return _bodyAtomVariableTypes(atom.innerGoal, dfa, env);
  }
  // A remote goal and a builtin goal contribute no type here; nothing is
  // relaxed on them.
  if (atom is ast.RemoteGoal) return const {};
  if (isBuiltinGoal(atom.functor)) return const {};

  final procDecl = env.getProcedure(atom.functor, atom.arity);
  if (procDecl == null || procDecl.arity != atom.arity) return const {};
  try {
    final term = producedTerm(atom, procDecl, typeEnv: env);
    return _checkModedTermPerArg(term, procDecl, dfa).variableTypes;
  } on ArityMismatchError {
    return const {};
  } on UnknownTypeError {
    return const {};
  } on StateError {
    return const {};
  }
}

/// Check if a goal is well-typed in the given environment.
///
/// Specification: TGLP `sections/glp-semantics.tex` (Well-Typed Outcomes):
/// "A goal G0 is well-typed by D if it is well-typed as a body." Being
/// well-typed as a body is Definition~\ref{def:well-typed-clause}
/// (`sections/well-typing.tex`) restricted to its body part:
///   - condition 2: for each unit goal A in the goal, the produced moded term A'
///     is well-typed by D; and
///   - condition 3: every variable pair X / X? in the goal has dual types
///     (relaxed to subtyping for body-body pairs per
///     Definition~\ref{def:well-typed-clause-subtyping}).
/// There is no head, so condition 1 (head well-typing) does not apply, and every
/// variable pair is body-body — so [_checkClauseDuality] applies the body-body
/// rule to all of them.
///
/// [goalAtoms] is the conjunction of unit goals (guards included, since a guard
/// is a body goal for type-checking, as in [checkClauseFromAst]). Returns a
/// [ClauseCheckResult] whose [ClauseCheckResult.errors] name the offending unit
/// goal or variable pair; [ClauseCheckResult.modedHead] is null (a goal has no
/// head).
ClauseCheckResult checkGoal(
  List<ast.Goal> goalAtoms,
  ProgramDFA dfa,
  TypeEnvironment env, {
  InstantiationCollector? collector,
  Map<String, ProcDecl> activeInstantiations = const {},
  CalleeClauses? callee,
}) {
  final errors = <ClauseError>[];
  final allVariableTypes = <String, VariableTypeInfo>{};
  final variableLocations = <String, String>{};
  final constructedModedBodyAtoms = <ModedTerm>[];

  // Condition 2: each unit goal's produced moded term is well-typed by D.
  for (int i = 0; i < goalAtoms.length; i++) {
    final atom = goalAtoms[i];
    final (atomResult, modedAtomTerm) = _checkBodyAtomWithTerm(atom, i, dfa, env,
        callerVarTypes: allVariableTypes, collector: collector,
        activeInstantiations: activeInstantiations,
        callee: callee);

    if (modedAtomTerm != null) {
      constructedModedBodyAtoms.add(modedAtomTerm);
    }

    if (!atomResult.isWellTyped) {
      errors.add(BodyAtomError(atom.functor, i, atomResult.errors));
    }

    for (final entry in atomResult.variableTypes.entries) {
      allVariableTypes.putIfAbsent(entry.key, () => entry.value);
      variableLocations.putIfAbsent(entry.key, () => 'body atom $i');
    }
  }

  // Condition 3: variable-pair duality. A goal is a body, so every pair is
  // body-body; _checkClauseDuality applies the body-body rule to all pairs.
  final dualityErrors = _checkClauseDuality(
    allVariableTypes,
    variableLocations,
    dfa,
  );
  errors.addAll(dualityErrors);

  return ClauseCheckResult(
    isWellTyped: errors.isEmpty,
    variableTypes: allVariableTypes,
    errors: errors,
    modedHead: null,
    modedBodyAtoms: constructedModedBodyAtoms,
  );
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
  TypeEnvironment env, {
  Map<String, VariableTypeInfo>? callerVarTypes,
  InstantiationCollector? collector,
}) {
  final (result, _) = _checkBodyAtomWithTerm(atom, atomIndex, dfa, env,
      callerVarTypes: callerVarTypes, collector: collector);
  return result;
}

/// Check body atom well-typing and return the constructed moded term
/// Fix 5.1: Returns both result and moded term
(WellTypedResult, ModedTerm?) _checkBodyAtomWithTerm(
  ast.Goal atom,
  int atomIndex,
  ProgramDFA dfa,
  TypeEnvironment env, {
  Map<String, VariableTypeInfo>? callerVarTypes,
  InstantiationCollector? collector,
  Map<String, ProcDecl> activeInstantiations = const {},
  CalleeClauses? callee,
}) {
  // Handle SpawnGoal (Goal@Agent) - type-check the inner goal
  if (atom is ast.SpawnGoal) {
    // Recursively type-check the inner goal
    return _checkBodyAtomWithTerm(atom.innerGoal, atomIndex, dfa, env,
        callerVarTypes: callerVarTypes, collector: collector,
        activeInstantiations: activeInstantiations,
        callee: callee);
  }

  // Handle RemoteGoal (M # proc(...)) - type-check against imported declaration
  if (atom is ast.RemoteGoal) {
    return _checkRemoteGoal(atom, atomIndex, dfa, env);
  }

  // Skip builtin goals (true, otherwise, :=)
  if (isBuiltinGoal(atom.functor)) {
    return (WellTypedResult.success({}), null);
  }

  // Look up procedure declaration
  var procDecl = env.getProcedure(atom.functor, atom.arity);
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

  // Case B: Call-site instantiation for parameterized procedures.
  // If a parameterized template exists, try to infer type param bindings
  // from the caller's variable types and create a concrete proc decl.
  final paramTemplate = env.paramProcDecls[procDecl.key];
  if (paramTemplate != null) {
    final enclosing = activeInstantiations[procDecl.key];
    if (enclosing != null) {
      // Recursive call: the callee is already being instantiated on the current
      // cycle. Recursion is monomorphic (typed-program: Parameterised Procedure
      // Declarations) — the call is checked at the enclosing instantiation, never
      // inducing a new one. Falling through with procDecl = enclosing checks the
      // call's arguments against that instantiation; a call that would require a
      // different instantiation fails the per-argument / duality check, which is
      // exactly the rejection of polymorphic recursion. Nothing is recorded, so
      // recursion induces no instantiation.
      procDecl = enclosing;
    } else if (callerVarTypes != null && callerVarTypes.isNotEmpty) {
      final inferredDecl = _inferConcreteDecl(
          paramTemplate, atom, callerVarTypes, dfa, env, callee);
      if (inferredDecl != null) {
        // Clause-template rule: record this instantiation so the parameterized
        // procedure's defining clauses are re-checked against it (Phase 2 /
        // instantiation closure). Then fall through to type the call site's own
        // arguments against the inferred concrete declaration: the call site is
        // itself a clause that must be well-typed (its variable-pair duality is
        // checked against the concrete element type), and typing the arguments
        // is also what lets closure infer instantiations through a parameterized
        // call's output (the output variable receives its concrete type here).
        collector?.record(
            CollectedInstantiation(inferredDecl.key, inferredDecl, env, dfa));
        procDecl = inferredDecl;
        // The inferred instantiation may reference types that arise only through
        // the closure (e.g. Stream<Box<Msg>> from a type-changing procedure) and
        // are not yet materialized in this DFA. Skip the per-argument check this
        // round; the instantiation has been recorded, so the closure materializes
        // the types and re-checks against the complete DFA.
        final present = inferredDecl.argTypes
            .every((t) => dfa.automata.containsKey(getFullTypeName(t)));
        if (!present) {
          return (_checkArgumentModes(atom, inferredDecl, env), null);
        }
      } else {
        // Inference failed (e.g. caller uses monomorphic types instead of the
        // parameterized form). The element type is the closure's to supply, and
        // the proc's own clauses are checked per concrete instantiation there;
        // but the MODES of this call are fixed by the template and are checked
        // here, not deferred.
        return (_checkArgumentModes(atom, paramTemplate, env), null);
      }
    } else {
      // No caller variable types available — can't infer type params. Same as
      // above: the element type waits for the closure, the modes do not.
      return (_checkArgumentModes(atom, paramTemplate, env), null);
    }
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

/// Condition 2 of `def:well-typed-clause` (`sections/well-typing.tex`) restricted
/// to what a call to a parameterised procedure decides without its instantiation.
///
/// The mode of each TOP-LEVEL argument of a parameterised declaration is fixed by
/// the template — `merge(Stream(X)?, Stream(X)?, Stream(X))` consumes at 1 and 2
/// and produces at 3 whatever `X` turns out to be — so an argument that is itself
/// a variable must be a reader at 1 and 2 and a writer at 3, whether or not the
/// element type is known. That is the mode-correspondence half of consistency
/// (`def:consistent-paths` rows 2 and 3, the same test `checkLeafConsistency`
/// case 1 applies); the element-type half genuinely needs the instantiation and
/// stays with the closure.
///
/// 🔴 **Only the top level.** A mode NESTED inside an argument is not fixed by the
/// template: it complements at each embedded `?` of the element type's definition
/// (`def:moded-head` step 1), and the element type is precisely what inference
/// could not supply. Checking nested positions here propagates the argument's own
/// mode into them and rejects correct writer-forwarding — the hollow message of
/// `sections/typed-glp.tex`, and `NetColdCall ::= intro(Constant, Response?)` in
/// `programs/social/graph/self.glp`, where the writer at slot 2 is right because
/// the type says `?` there. Measured 2026-08-02: a first cut of this function
/// that walked every path reported 29 such rejections across `social/graph`,
/// `cssn` and `social_graph_simulated_ui`, every one of them false.
///
/// This runs at the three points where the full per-argument check cannot: when
/// call-site inference binds no parameter, when no caller variable types are
/// available, and when the inferred instantiation names types this DFA has not
/// materialised. Until 2026-08-02 all three returned success, so a call to the
/// root scope's `merge`, `send`, `receive` or `new_channel` with a writer and a
/// reader transposed was passed in silence — the error class
/// `sections/introduction.tex` gives as the paper's motivating example, and the
/// one the same call to a monomorphic procedure has always been rejected for.
WellTypedResult _checkArgumentModes(
    ast.Goal atom, ProcDecl decl, TypeEnvironment env) {
  final ModedTerm modedTerm;
  try {
    modedTerm = producedTerm(atom, decl, typeEnv: env);
  } on ArityMismatchError catch (e) {
    return WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(symbol: e.message, argIndex: 0, mode: Mode.produce)]),
        e.message,
      ),
    ]);
  }
  if (modedTerm is! ModedCompound) return WellTypedResult.success({});

  final errors = <WellTypedError>[];
  for (int i = 0; i < decl.arity && i < modedTerm.args.length; i++) {
    final arg = modedTerm.args[i];
    if (arg is! ModedVariable) continue; // nested modes are the closure's
    final wanted = arg.isReader ? Mode.consume : Mode.produce;
    if (arg.mode == wanted) continue;
    final expected = arg.isReader ? '↓ (consume)' : '↑ (produce)';
    final actual = arg.mode == Mode.consume ? '↓ (consume)' : '↑ (produce)';
    errors.add(InconsistentPathError(
        ModedPath([
          PathStep(
              symbol: arg.isReader ? '${arg.name}?' : arg.name,
              argIndex: i + 1,
              mode: arg.mode)
        ]),
        'Variable mode mismatch: ${arg.isReader ? "reader" : "writer"} '
        'requires $expected, got $actual'));
  }

  return errors.isEmpty
      ? WellTypedResult.success({})
      : WellTypedResult.failure(errors);
}

/// Check a remote goal (M # proc(...)) against the imported procedure declaration.
///
/// Per spec Section 5.1: type checking is local — we look up the imported
/// declaration in the local TypeEnvironment, not the remote module.
///
/// Dynamic dispatch (variable module) is skipped — can't resolve at compile time.
(WellTypedResult, ModedTerm?) _checkRemoteGoal(
  ast.RemoteGoal remote,
  int atomIndex,
  ProgramDFA dfa,
  TypeEnvironment env,
) {
  // Dynamic dispatch (variable module) — skip type checking
  if (remote.isDynamic) {
    return (WellTypedResult.success({}), null);
  }

  // Flatten nested RemoteGoals to extract full module path and actual goal.
  // Example: ui#actors # render(X?) parses as RemoteGoal(ui, RemoteGoal(actors, render(X?)))
  // We need: modulePath = "ui#actors", innerGoal = render(X?)
  final pathParts = <String>[];
  ast.Goal innerGoal = remote;
  while (innerGoal is ast.RemoteGoal) {
    final rg = innerGoal as ast.RemoteGoal;
    if (rg.isDynamic) {
      // If any part of the path is dynamic, skip type checking
      return (WellTypedResult.success({}), null);
    }
    pathParts.add(rg.staticModuleName!);
    innerGoal = rg.goal;
  }
  final modulePath = pathParts.join('#');
  final goalFunctor = innerGoal.functor;
  final goalArity = innerGoal.arity;

  // Look up: 'modulePath#goalFunctor/arity'
  final qualifiedKey = '$modulePath#$goalFunctor/$goalArity';
  final procDecl = env.procedures[qualifiedKey];

  if (procDecl == null) {
    return (WellTypedResult.failure([
      InconsistentPathError(
        ModedPath([PathStep(
          symbol: qualifiedKey,
          argIndex: 0,
          mode: Mode.produce,
        )]),
        'No imported declaration for $modulePath#$goalFunctor/$goalArity — '
        'add "imported procedure $modulePath#$goalFunctor(...)" to this module',
      ),
    ]), null);
  }

  // Type-check the inner goal's arguments against the imported declaration
  try {
    final modedAtomTerm = producedTerm(innerGoal, procDecl, typeEnv: env);
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
  final dualityErrors = _checkTermDuality(variableTypes, dfa);
  errors.addAll(dualityErrors);

  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors,
  );
}

/// Check duality within a term (same logic as well_typed_term.dart)
List<NonDualError> _checkTermDuality(
    Map<String, VariableTypeInfo> variableTypes, ProgramDFA dfa) {
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

      if (!_areDualTypes(writerInfo, readerInfo, dfa)) {
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
  ProgramDFA dfa,
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
        if (writerNormLoc == 'head') {
          // Both in head: require exact DUAL types (unchanged)
          final (isCompat, reason) = _areDualTypesWithReason(writerInfo, readerInfo, dfa);
          if (!isCompat) {
            errors.add(ClauseDualityError(
              baseName,
              writerInfo,
              readerInfo,
              writerLoc,
              readerLoc,
              'Variables in same clause part (head) must have dual types: $reason',
            ));
          }
        } else {
          // Both in body: require subtyping S <: T (Definition 4.8)
          // Writer X has output type S. Reader X? has dual type T?.
          // Need: S <: T (both output types).
          final writerOutputState = writerInfo.typeState; // S (output, not dual)
          final readerDualState = readerInfo.typeState;   // T? (dual)
          final readerOutputState = dfa.getState(readerDualState.baseName); // T (output)
          final isSub = isSubtype(writerOutputState, readerOutputState, dfa);
          if (!isSub) {
            errors.add(ClauseDualityError(
              baseName,
              writerInfo,
              readerInfo,
              writerLoc,
              readerLoc,
              'Body variable pair: writer type ${writerOutputState.name} is not a subtype of ${readerOutputState.name}',
            ));
          }
        }
      } else {
        // One in head, one in body: a DIRECTED subtyping check, not equality.
        //
        // TGLP def:well-typed-clause-subtyping 3(b), as relaxed in Moded-Types
        // a5221fd: "if one occurs in the head and the other in the body, the
        // dual of the type of the head occurrence is a subtype of the dual of
        // the type of the body occurrence."  The prose above it says what the
        // check means: the two occurrences carry the SAME mode, and what the
        // head occurrence receives must be within what the body occurrence
        // accepts.  Taking the dual of each puts both in output polarity, which
        // is where <: is defined; sec:subtyping extends <: to input types by
        // complementation (A? <: B? if B <: A), which is what makes the
        // comparison well-formed at consumed positions.
        //
        // Equality was the base definition's condition 3(b) and is strictly
        // stronger, so nothing that passed before fails here.
        final headInfo = writerNormLoc == 'head' ? writerInfo : readerInfo;
        final bodyInfo = writerNormLoc == 'head' ? readerInfo : writerInfo;
        final headOutput = dfa.getState(headInfo.typeState.baseName);
        final bodyOutput = dfa.getState(bodyInfo.typeState.baseName);
        if (!isSubtype(headOutput, bodyOutput, dfa)) {
          errors.add(ClauseDualityError(
            baseName,
            writerInfo,
            readerInfo,
            writerLoc,
            readerLoc,
            'Variables across head/body: the head occurrence receives '
            '${headOutput.name}, which is not within what the body occurrence '
            'accepts (${bodyOutput.name})',
          ));
        }
      }
    }
  }

  return errors;
}

/// Check if writer and reader types are dual
/// Per spec v0.6: uses DFAState.baseName and isDual
bool _areDualTypes(VariableTypeInfo writerInfo, VariableTypeInfo readerInfo, ProgramDFA dfa) {
  final (isCompat, _) = _areDualTypesWithReason(writerInfo, readerInfo, dfa);
  return isCompat;
}

// _areSameTypeWithReason is gone with the head-body equality check it served.
// Condition 3(b) is a directed subtyping check since Moded-Types a5221fd; see
// _checkClauseDuality.

/// Check duality with reason for failure
/// Per paper Definition 5.6: head-head and body-body pairs must have dual types.
/// Dual types have the same baseName and opposite isDual flag.
/// Example: Stream is dual to Stream?, _ is dual to _?
/// Note: Stream is NOT dual to _ (different base names)
(bool, String?) _areDualTypesWithReason(VariableTypeInfo writerInfo, VariableTypeInfo readerInfo, ProgramDFA dfa) {
  // Mode check: writer must produce, reader must consume
  if (writerInfo.mode != Mode.produce) {
    return (false, 'Writer must have produce mode');
  }
  if (readerInfo.mode != Mode.consume) {
    return (false, 'Reader must have consume mode');
  }

  // States must be duals: the same base type (up to structural identity,
  // typed-program §20.3), opposite isDual.  Applies to all types including
  // wildcards: _ is dual to _?, Stream is dual to Stream?.
  if (!sameBaseType(writerInfo.typeState.baseName, readerInfo.typeState.baseName, dfa)) {
    return (false, 'Types must have same base: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}');
  }

  // One must be dual, the other not
  if (writerInfo.typeState.isDual == readerInfo.typeState.isDual) {
    return (false, 'One must be dual, other not: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}');
  }

  return (true, null);
}

// =============================================================================
// Case B: Call-site instantiation for parameterized procedures
// =============================================================================

/// Infer a concrete proc decl by matching a parameterized template against
/// the actual argument types at a call site.
///
/// Returns null if inference fails (e.g., no matching variable types found).
ProcDecl? _inferConcreteDecl(
  ProcDecl paramTemplate,
  ast.Goal atom,
  Map<String, VariableTypeInfo> callerVarTypes,
  ProgramDFA dfa,
  TypeEnvironment env,
  CalleeClauses? callee,
) {
  final bindings = <String, String>{}; // typeParam -> concreteTypeName

  // For each argument, try to infer type param bindings
  for (int i = 0; i < paramTemplate.arity && i < atom.args.length; i++) {
    final declaredType = paramTemplate.argTypes[i];
    final actualArg = atom.args[i];

    // Get the actual variable's type from callerVarTypes.
    // The type a call site imposes on a parameter is carried by whichever half
    // of the SRSW pair was already recorded from the head or a prior body atom.
    // A reader argument (S?) is fed by its paired writer (S) recorded earlier,
    // and vice versa, so resolve polarity-agnostically by base name: try the
    // same-polarity key, then the paired half. Both halves report the same
    // DFAState.baseName (the dual marker is stripped), so either yields the
    // element type needed to instantiate the type parameter. Without this the
    // common case — a polymorphic parameter passed a reader — failed inference
    // and the body's polarity obligation was never re-checked (Issue 14).
    //
    // Only an argument that is a VARIABLE gives an equation. A constructed term
    // constrains by containment, not by equation --- its type must be admitted
    // by whatever the parameter is bound to --- and the call site's own argument
    // check below is where that containment is tested. Reading the containment
    // as the equation binds the parameter to the term's own type, which is one
    // alternative of the union the callee's clauses require, and breaks duality
    // in the callee's head (TGLP e56c303, def:instantiation, which replaced the
    // sentence that said a constructed argument binds the parameter).
    String? actualTypeName;
    if (actualArg is ast.VarTerm) {
      final info =
          callerVarTypes[actualArg.name] ?? callerVarTypes['${actualArg.name}?'];
      if (info != null) {
        actualTypeName = info.typeState.baseName;
      }
    }
    if (actualTypeName == null) continue;

    // Match declared type against actual type to extract bindings
    _matchTypeForInference(declaredType, actualTypeName, paramTemplate.typeParams, bindings, env);
  }

  // The caller's arguments settle only the parameters a variable argument puts
  // opposite a declared type.  The rest come from the callee's own clauses:
  // TGLP def:instantiation makes an instantiation a map under which the caller's
  // clause AND the clauses of the called procedure are well-typed, so a variable
  // pair of the callee's head that the declaration types by a parameter on one
  // side and by a concrete type on the other is an equation for that parameter
  // --- condition 3(a) of def:well-typed-clause requires the two to be dual, and
  // duality is equality of base type.  It is what fixes `M` in
  // `send_user(M?, Stream(Ent)?, Stream(Ent))` once the call has fixed `Ent`:
  // the clause writes `Msg` at `M?` and reads `Msg?` at the element type of the
  // stream `Ent` carries, so `M` is that element type.
  _solveFromCalleeClauses(
      paramTemplate, atom, callerVarTypes, bindings, env, callee);

  // A parameter no equation fixes is fixed by the constructors the callee's
  // heads match at its positions, coverage selecting it (def:instantiation, and
  // Udi 2026-09-20): construct it and verify, rather than search.
  if (callee != null) {
    final defining = callee.of(paramTemplate.key);
    if (defining != null && defining.isNotEmpty) {
      _thetaFromHeads(paramTemplate, bindings, defining, dfa, env, callee);
    }
  }

  // If no bindings found, this call's instantiation can't be inferred.
  if (bindings.isEmpty) return null;

  // Check all type params are bound
  for (final tp in paramTemplate.typeParams) {
    if (!bindings.containsKey(tp)) return null;
  }

  // A parameter bound to the wildcard `_`/`_?` is NOT a concrete instantiation
  // (it can arise when a concrete type carries a `_` field at the matched
  // position). Treat it as not inferable — like inference failure — so it
  // neither drives a call-site check against `_` nor is recorded for
  // per-instantiation checking. A parameterized procedure is checked only at
  // concrete instantiations (typed-program.md "Programs and Modules").
  for (final v in bindings.values) {
    if (v == '_' || v == '_?') return null;
  }

  // Create concrete arg types by substituting bindings
  final concreteArgTypes = <TypeExpr>[];
  for (final argType in paramTemplate.argTypes) {
    concreteArgTypes.add(_substituteTypeParams(argType, bindings));
  }

  // A referenced type may legitimately not be in the DFA yet if it arises only
  // through the procedure-instantiation closure (e.g. Stream<Box<Msg>> from a
  // type-changing procedure). Such a type is materializable — a parameterized
  // name whose template is known — and the closure will materialize it. Bail
  // only when a referenced type is neither present nor materializable (it cannot
  // be a real type), in which case this is not a usable instantiation.
  for (final argType in concreteArgTypes) {
    final typeName = getFullTypeName(argType);
    if (dfa.automata.containsKey(typeName)) continue;
    var base = typeName.endsWith('?')
        ? typeName.substring(0, typeName.length - 1)
        : typeName;
    final lt = base.indexOf('<');
    final materializable =
        lt > 0 && env.typeTemplates.containsKey(base.substring(0, lt));
    if (!materializable) {
      return null; // not present and not materializable — unusable instantiation
    }
  }

  return ProcDecl(paramTemplate.name, concreteArgTypes,
      paramTemplate.line, paramTemplate.column,
      exported: paramTemplate.exported,
      imported: paramTemplate.imported,
      modulePath: paramTemplate.modulePath);
}

/// The name of the abstract type standing for an as-yet-unbound parameter while
/// the callee's clauses are probed for the equation that fixes it.
const String _paramProbePrefix = r'$param_';

/// Read from the clauses of [paramTemplate]'s procedure the equations that fix
/// the parameters [bindings] does not yet carry.
///
/// TGLP parameterized-types.tex def:instantiation: "A map $\theta$ from those
/// parameters to types of the program is an instantiation of $A$ if $C$ and the
/// clauses of $q$ are well-typed (Definition "Well-Typed Clause") when $q$'s
/// declaration is replaced by its expansion under $\theta$."  The caller's
/// clause is one half of that and gives an equation only where an argument is a
/// variable; the callee's clauses are the other half, and their variable pairs
/// give the rest.
///
/// The probe: substitute what is bound, and each unbound parameter by a distinct
/// abstract type (def:abstract-type), which has no transitions and so is dual to
/// nothing.  Check the callee's clauses by that declaration.  A variable pair
/// whose two occurrences lie in the same part of the clause must be dual
/// (def:well-typed-clause 3(a)), and duality is equality of base type, so a pair
/// reported not dual with the probe on one side and a concrete type on the other
/// states the equation: the parameter is that concrete type.  Reading only pairs
/// whose occurrences lie in the same part keeps to 3(a); a head/body pair is
/// 3(b), a containment, and constrains without fixing.
///
/// Equations are necessary conditions of well-typing, so every instantiation of
/// the call satisfies them: where they fix a parameter, they fix it to the only
/// value an instantiation can give it, and the caller's clause and the callee's
/// clauses are then checked against it as usual --- this proposes $\theta$, it
/// does not pronounce on it.  Two clauses requiring different types for one
/// parameter leave it unbound, and so does a parameter no equation reaches: the
/// call then has no instantiation, and the program is rejected unless the
/// procedure is parametrically well-typed (sec:abstract-parameters).
void _solveFromCalleeClauses(
  ProcDecl paramTemplate,
  ast.Goal atom,
  Map<String, VariableTypeInfo> callerVarTypes,
  Map<String, String> bindings,
  TypeEnvironment env,
  CalleeClauses? callee,
) {
  if (callee == null) return;
  final clauses = callee.of(paramTemplate.key);
  if (clauses == null || clauses.isEmpty) return;

  var progress = true;
  while (progress) {
    progress = false;
    final unbound = [
      for (final tp in paramTemplate.typeParams)
        if (!bindings.containsKey(tp)) tp
    ];
    if (unbound.isEmpty) return;

    final probeOf = {for (final tp in unbound) tp: '$_paramProbePrefix$tp'};
    final probeNames = probeOf.values.toSet();
    final subst = {...bindings, ...probeOf};
    final probeDecl = ProcDecl(
      paramTemplate.name,
      [for (final t in paramTemplate.argTypes) _substituteTypeParams(t, subst)],
      paramTemplate.line,
      paramTemplate.column,
      exported: paramTemplate.exported,
      imported: paramTemplate.imported,
      modulePath: paramTemplate.modulePath,
    );

    // The abstract types themselves, plus any template instantiation the
    // substitution names that the environment does not yet hold --- `Stream(X)`
    // over a probe becomes `Stream<$param_X>`, which nothing has materialized.
    final probeTypes = <String, TypeDef>{
      for (final n in probeNames) n: TypeDef(n, const [], 0, 0)
    };
    final needed = <String>{};
    for (final t in probeDecl.argTypes) {
      var n = getFullTypeName(t);
      if (n.endsWith('?')) n = n.substring(0, n.length - 1);
      if (n.contains('<') && !env.types.containsKey(n)) needed.add(n);
    }
    if (needed.isNotEmpty) {
      probeTypes.addAll(materializeInstantiations(needed, env.typeTemplates,
          {...env.types.keys, ...probeNames}));
    }

    final probeEnv = TypeEnvironment(
      {...env.types, ...probeTypes},
      {...env.procedures, probeDecl.key: probeDecl},
      paramProcDecls: env.paramProcDecls,
      typeTemplates: env.typeTemplates,
      typeOrigins: env.typeOrigins,
    );
    final ProgramDFA probeDfa;
    try {
      probeDfa = buildProgramDFA(probeEnv);
    } on UnknownTypeError {
      return; // a substituted type is not in scope; no equation to read here
    }

    // Equations from the variables INSIDE a constructed argument of the call.
    // The argument's own type does not bind the parameter, but a variable within
    // it does, exactly as a variable argument does: the caller's clause pairs it
    // with an occurrence of its declared type, and the position it stands at in
    // the declaration is the parameter's.  Partial evaluation delivers many
    // calls this way --- `intro_await_peer(Other?, ch(PE16?, PE17), Result)` in
    // place of a channel variable --- and without this the parameter inside
    // `Channel(Stream(C), Stream(C))?` is reached by nothing.
    final paramOfProbe = {for (final e in probeOf.entries) e.value: e.key};
    for (var i = 0; i < probeDecl.arity && i < atom.args.length; i++) {
      final arg = atom.args[i];
      if (arg is ast.VarTerm) continue;
      final start = probeDfa.states[getFullTypeName(probeDecl.argTypes[i])];
      if (start == null) continue;
      final inside = <String, DFAState>{};
      _collectVarStates(arg, start, probeDfa, inside);
      for (final e in inside.entries) {
        final bare =
            e.key.endsWith('?') ? e.key.substring(0, e.key.length - 1) : e.key;
        final info = callerVarTypes[e.key] ??
            callerVarTypes[bare] ??
            callerVarTypes['$bare?'];
        if (info == null) continue;
        final before = bindings.length;
        _unifyProbeNames(
            e.value.name, info.typeState.baseName, paramOfProbe, bindings);
        if (bindings.length != before) progress = true;
      }
    }
    if (progress) continue;

    for (final clause in clauses) {
      final ClauseCheckResult res;
      try {
        res = checkClauseFromAst(clause, probeDfa, probeEnv,
            activeInstantiations: {probeDecl.key: probeDecl});
      } on Object {
        continue; // this clause yields no equation
      }
      for (final e in res.errors) {
        if (e is! ClauseDualityError) continue;
        if (e.writerLocation != e.readerLocation) continue; // 3(a) only
        final w = e.writerType?.typeState.baseName;
        final r = e.readerType?.typeState.baseName;
        if (w == null || r == null) continue;
        final String probe, other;
        if (probeNames.contains(w) && !probeNames.contains(r)) {
          probe = w;
          other = r;
        } else if (probeNames.contains(r) && !probeNames.contains(w)) {
          probe = r;
          other = w;
        } else {
          continue;
        }
        if (other == '_' || other == '_?') continue;
        final tp = probe.substring(_paramProbePrefix.length);
        final had = bindings[tp];
        if (had == null) {
          bindings[tp] = other;
          progress = true;
        } else if (had != other && !sameBaseType(had, other, probeDfa)) {
          // Two clauses require different types of one parameter: no map makes
          // them all well-typed, so the call has no instantiation.
          bindings.remove(tp);
          return;
        }
      }
    }
  }
}

/// Bind, in [bindings], the parameters that matching the declared type name
/// [declName] against the actual [actualName] settles.
///
/// [paramOfProbe] maps each abstract stand-in to the parameter it stands for.
/// The two names are walked together through the `T<A,B>` form the checker
/// writes expanded monomorphic types in, so a parameter at any depth of a
/// template's arguments is reached.
void _unifyProbeNames(String declName, String actualName,
    Map<String, String> paramOfProbe, Map<String, String> bindings) {
  var d = declName, a = actualName;
  if (d.endsWith('?')) d = d.substring(0, d.length - 1);
  if (a.endsWith('?')) a = a.substring(0, a.length - 1);
  final param = paramOfProbe[d];
  if (param != null) {
    if (a != '_' && !paramOfProbe.containsKey(a)) {
      bindings.putIfAbsent(param, () => a);
    }
    return;
  }
  final di = d.indexOf('<'), ai = a.indexOf('<');
  if (di < 0 || ai < 0) return;
  if (d.substring(0, di) != a.substring(0, ai)) return;
  final da = _splitTypeArgs(d.substring(di + 1, d.length - 1));
  final aa = _splitTypeArgs(a.substring(ai + 1, a.length - 1));
  if (da.length != aa.length) return;
  for (var i = 0; i < da.length; i++) {
    _unifyProbeNames(da[i], aa[i], paramOfProbe, bindings);
  }
}

/// Build, for each parameter no equation fixes, the type the callee's heads
/// require, and adopt it if it verifies.
///
/// TGLP parameterized-types.tex def:instantiation ends "...and every input path
/// of that declaration is accepted by some clause of $q$", and coverage is what
/// selects $\theta$ where the equations do not: a $\theta$ carrying an
/// alternative no clause of $q$ matches leaves an input path unaccepted, and a
/// $\theta$ missing one a head matches makes that head inconsistent, so
/// $\theta$ carries exactly the constructors the callee's heads match at that
/// position, up to automaton.  This builds that type from the heads and then
/// verifies it; it does not search.
///
/// The positions are found by walking each head argument through the automaton
/// of its declared type with the parameter standing as an abstract type: a
/// sub-term sitting where that type is reached is a sub-term at the parameter.
/// A sub-term that is a constructed term gives an alternative, its fields being
/// the types of its constants and, for a variable, the type of the variable's
/// other occurrence --- which is at a concrete position, so the clause's own
/// variable-pair condition is what supplies it.  A head carrying only a variable
/// there contributes nothing; a head carrying something this does not read (a
/// list) abandons the parameter, which stays unbound.
///
/// The constructed type is then put to [CalleeClauses.verify], and adopted only
/// if the callee's clauses are well-typed by it and cover its input paths.
void _thetaFromHeads(
  ProcDecl paramTemplate,
  Map<String, String> bindings,
  List<ast.Clause> clauses,
  ProgramDFA dfa,
  TypeEnvironment env,
  CalleeClauses callee,
) {
  final unbound = [
    for (final tp in paramTemplate.typeParams)
      if (!bindings.containsKey(tp)) tp
  ];
  if (unbound.isEmpty) return;

  final probeOf = {for (final tp in unbound) tp: '$_paramProbePrefix$tp'};
  final probeNames = probeOf.values.toSet();
  final subst = {...bindings, ...probeOf};
  final probeDecl = ProcDecl(
    paramTemplate.name,
    [for (final t in paramTemplate.argTypes) _substituteTypeParams(t, subst)],
    paramTemplate.line,
    paramTemplate.column,
    exported: paramTemplate.exported,
    imported: paramTemplate.imported,
    modulePath: paramTemplate.modulePath,
  );
  // A parameter inside a template gives a name nothing has materialized ---
  // `Channel(Stream(C), Stream(C))?` over a probe becomes
  // `Channel<Stream<$param_C>,Stream<$param_C>>` --- and without it the probe
  // DFA cannot be built at all.
  final probeTypes = <String, TypeDef>{
    for (final n in probeNames) n: TypeDef(n, const [], 0, 0)
  };
  final needed = <String>{};
  for (final t in probeDecl.argTypes) {
    var n = getFullTypeName(t);
    if (n.endsWith('?')) n = n.substring(0, n.length - 1);
    if (n.contains('<') && !env.types.containsKey(n)) needed.add(n);
  }
  if (needed.isNotEmpty) {
    probeTypes.addAll(materializeInstantiations(
        needed, env.typeTemplates, {...env.types.keys, ...probeNames}));
  }
  final probeEnv = TypeEnvironment(
    {...env.types, ...probeTypes},
    {...env.procedures, probeDecl.key: probeDecl},
    paramProcDecls: env.paramProcDecls,
    typeTemplates: env.typeTemplates,
    typeOrigins: env.typeOrigins,
  );
  final ProgramDFA probeDfa;
  try {
    probeDfa = buildProgramDFA(probeEnv);
  } on UnknownTypeError {
    return;
  }

  // Per probe: the alternatives its positions require, keyed by top-level
  // functor so two heads matching one constructor give one alternative (type
  // definitions are deterministic: distinct top-level functors).
  final alts = {for (final n in probeNames) n: <String, TypeExpr>{}};
  final built = <String, TypeDef>{}; // nested types the fields reference
  final abandoned = <String>{};

  for (final clause in clauses) {
    // The type each variable of the head carries at its occurrences, by the same
    // walk: a variable inside a parameter position is not reached (an abstract
    // type has no transitions), so what this holds for a field's variable is its
    // OTHER, concrete occurrence --- which is what the clause's variable-pair
    // condition makes the field's type dual to.
    final vars = <String, DFAState>{};
    for (var i = 0; i < probeDecl.arity && i < clause.head.args.length; i++) {
      final start = probeDfa.states[getFullTypeName(probeDecl.argTypes[i])];
      if (start == null) continue;
      _collectVarStates(clause.head.args[i], start, probeDfa, vars);
    }
    for (var i = 0;
        i < probeDecl.arity && i < clause.head.args.length;
        i++) {
      final at = <(ast.Term, DFAState)>[];
      final start = probeDfa.states[getFullTypeName(probeDecl.argTypes[i])];
      if (start == null) continue;
      _collectAtProbe(clause.head.args[i], start, probeDfa, probeNames, at);
      for (final (term, state) in at) {
        final probe = state.baseName;
        if (abandoned.contains(probe)) continue;
        final alt = _headAlternative(term, state.isDual, vars, built);
        if (alt == null) {
          if (term is ast.VarTerm || term is ast.UnderscoreTerm) continue;
          abandoned.add(probe); // a head shape this does not read
          continue;
        }
        final key = alt is StructAlt
            ? '${alt.functor}/${alt.arity}'
            : alt.toString();
        final had = alts[probe]![key];
        if (had == null) {
          alts[probe]![key] = alt;
        } else if (had.toString() != alt.toString()) {
          abandoned.add(probe); // two heads, one constructor, different fields
        }
      }
    }
  }

  // Every parameter is settled together and the whole put to one verification:
  // def:instantiation is a condition on the map, not on one parameter of it.
  final candidate = {...bindings};
  final newTypes = <String, TypeDef>{};
  for (final tp in unbound) {
    final probe = probeOf[tp]!;
    if (abandoned.contains(probe)) return;
    final collected = alts[probe]!.values.toList();
    if (collected.isEmpty) return; // no head reaches it: it stays unbound
    final name = '\$t<${collected.map(_renderAlt).join(';')}>';
    newTypes[name] =
        TypeDef(name, collected, paramTemplate.line, paramTemplate.column);
    candidate[tp] = name;
  }
  if (candidate.length != paramTemplate.typeParams.length) return;
  newTypes.addAll(built);

  final thetaDecl = ProcDecl(
    paramTemplate.name,
    [
      for (final t in paramTemplate.argTypes)
        _substituteTypeParams(t, candidate)
    ],
    paramTemplate.line,
    paramTemplate.column,
    exported: paramTemplate.exported,
    imported: paramTemplate.imported,
    modulePath: paramTemplate.modulePath,
  );
  final thetaTypes = {...env.types, ...probeTypes, ...newTypes};
  thetaTypes.removeWhere((k, _) => probeNames.contains(k));
  final thetaEnv = TypeEnvironment(
    thetaTypes,
    {...env.procedures, thetaDecl.key: thetaDecl},
    paramProcDecls: env.paramProcDecls,
    typeTemplates: env.typeTemplates,
    typeOrigins: env.typeOrigins,
  );
  if (!callee.verify(thetaDecl, thetaEnv, clauses)) return;
  try {
    env.types.addAll(newTypes);
  } on UnsupportedError {
    return;
  }
  // The call site's own DFA was built before these types existed; add them there
  // too, so the call's arguments are checked against the instantiation here
  // rather than deferred.
  for (final d in newTypes.values) {
    addTypeToProgramDFA(dfa, d, env.types);
  }
  bindings.addAll(candidate);
}

/// The sub-terms of [term] that sit where a type in [probeNames] is reached,
/// with the state they sit at, found by walking [term] through the automaton of
/// [state].  Transitions are matched by functor, arity and argument position,
/// not by mode: the mode of the position is read off the state reached.
void _collectAtProbe(ast.Term term, DFAState state, ProgramDFA dfa,
    Set<String> probeNames, List<(ast.Term, DFAState)> out) {
  if (probeNames.contains(state.baseName)) {
    out.add((term, state));
    return;
  }
  final automaton = dfa.automata[state.name];
  if (automaton == null) return;
  if (term is ast.StructTerm) {
    for (var i = 0; i < term.args.length; i++) {
      final next =
          _stepTo(automaton, state, term.functor, term.args.length, i + 1);
      if (next != null) {
        _collectAtProbe(term.args[i], next, dfa, probeNames, out);
      }
    }
  } else if (term is ast.ListTerm && !term.isNil) {
    final h = _stepTo(automaton, state, '[|]', 2, 1);
    final t = _stepTo(automaton, state, '[|]', 2, 2);
    if (h != null) _collectAtProbe(term.head!, h, dfa, probeNames, out);
    if (t != null) _collectAtProbe(term.tail!, t, dfa, probeNames, out);
  }
}

/// Record the state each variable of [term] sits at, walking from [state].
void _collectVarStates(
    ast.Term term, DFAState state, ProgramDFA dfa, Map<String, DFAState> out) {
  if (term is ast.VarTerm) {
    out['${term.name}${term.isReader ? '?' : ''}'] = state;
    return;
  }
  final automaton = dfa.automata[state.name];
  if (automaton == null) return;
  if (term is ast.StructTerm) {
    for (var i = 0; i < term.args.length; i++) {
      final next =
          _stepTo(automaton, state, term.functor, term.args.length, i + 1);
      if (next != null) _collectVarStates(term.args[i], next, dfa, out);
    }
  } else if (term is ast.ListTerm && !term.isNil) {
    final h = _stepTo(automaton, state, '[|]', 2, 1);
    final t = _stepTo(automaton, state, '[|]', 2, 2);
    if (h != null) _collectVarStates(term.head!, h, dfa, out);
    if (t != null) _collectVarStates(term.tail!, t, dfa, out);
  }
}

DFAState? _stepTo(
    Automaton a, DFAState from, String symbol, int arity, int argIndex) {
  for (final entry in a.transitions.entries) {
    final (f, label) = entry.key;
    if (f == from &&
        label.symbol == symbol &&
        label.arity == arity &&
        label.argIndex == argIndex) {
      return entry.value;
    }
  }
  return null;
}

/// The type alternative the head sub-term [term] requires at a position whose
/// polarity is [consumed], or null where this does not read the shape.
///
/// A variable's type is the type of its other occurrence: the clause's
/// variable-pair condition (def:well-typed-clause 3) makes the two dual, so the
/// base name is that one's and the mode marker is what makes them dual ---
/// a head complements every variable (def:moded-head), so a source reader lands
/// at a produced position and a source writer at a consumed one.
TypeExpr? _headAlternative(ast.Term term, bool consumed,
    Map<String, DFAState> vars, Map<String, TypeDef> built) {
  if (term is ast.ConstTerm) return ConstantAlt(term.value ?? '', term.line, term.column);
  if (term is! ast.StructTerm || term.args.isEmpty) return null;
  final fields = <TypeExpr>[];
  for (final arg in term.args) {
    final f = _headFieldType(arg, consumed, vars, built);
    if (f == null) return null;
    fields.add(f);
  }
  return StructAlt(term.functor, fields, term.line, term.column);
}

TypeExpr? _headFieldType(ast.Term term, bool consumed,
    Map<String, DFAState> vars, Map<String, TypeDef> built) {
  if (term is ast.VarTerm) {
    final other = term.isReader ? term.name : '${term.name}?';
    final state = vars[other];
    if (state == null) return null; // no other occurrence: nothing supplies it
    return TypeRef(state.baseName, term.line, term.column,
        isInput: term.isReader == consumed);
  }
  if (term is ast.UnderscoreTerm) {
    return PrimitiveModeAlt(term.isReader == consumed, term.line, term.column);
  }
  if (term is ast.ConstTerm) {
    return TypeRef('Constant', term.line, term.column);
  }
  if (term is ast.StructTerm && term.args.isNotEmpty) {
    final alt = _headAlternative(term, consumed, vars, built);
    if (alt == null) return null;
    final name = '\$t<${_renderAlt(alt)}>';
    built[name] = TypeDef(name, [alt], term.line, term.column);
    return TypeRef(name, term.line, term.column);
  }
  return null;
}

/// A type alternative rendered so that the name built from it determines it, and
/// so that every comma it carries lies inside `<>` --- the bracket the type-name
/// splitters ([_splitTypeArgs], param_expansion's `_splitTopLevelArgs`) respect.
String _renderAlt(TypeExpr alt) {
  if (alt is StructAlt) {
    return '${alt.functor}<${alt.args.map(_renderAlt).join(',')}>';
  }
  if (alt is ConstantAlt) return '#${alt.value}';
  if (alt is TypeRef) return alt.isInput ? '${alt.name}?' : alt.name;
  if (alt is PrimitiveModeAlt) return alt.isInput ? '_?' : '_';
  return alt.toString();
}

/// Match a declared type expression against an actual type name to infer
/// type parameter bindings.
void _matchTypeForInference(
  TypeExpr declaredType,
  String actualTypeName,
  List<String> typeParams,
  Map<String, String> bindings,
  TypeEnvironment env,
) {
  if (declaredType is TypeRef) {
    if (declaredType.typeArgs.isEmpty && typeParams.contains(declaredType.name)) {
      // Bare type parameter: X → actualTypeName
      bindings.putIfAbsent(declaredType.name, () => actualTypeName);
      return;
    }

    if (declaredType.typeArgs.isNotEmpty) {
      // Parameterized type ref: Stream(X) vs Stream<AgentMsg>
      // Parse the actual type name to extract template and args
      var resolvedActual = actualTypeName;
      var ltIdx = resolvedActual.indexOf('<');
      if (ltIdx < 0) {
        // Actual is a named type.  Honor structural type identity (typed-program
        // §20.3): a named recursive list alias `T ::= [] ; [E | T]` IS Stream<E>,
        // so resolve it to its structural parameterized form before matching.
        // Without this a named alias binds no parameter, the call records no
        // instantiation, and the parametric procedure is never checked at this
        // element type — a soundness hole (e.g. graph's OutputsList cannot route
        // through the shared parametric lib routers).  Resolution is a single
        // lookup (terminating) and the element type is unique.
        final structForm = _structuralFormOfNamedType(resolvedActual, env);
        if (structForm == null) return; // not structurally parameterized
        resolvedActual = structForm;
        ltIdx = resolvedActual.indexOf('<');
        if (ltIdx < 0) return;
      }

      final actualTemplate = resolvedActual.substring(0, ltIdx);
      if (actualTemplate != declaredType.name) return; // template name mismatch

      // Extract actual type args from "Stream<AgentMsg>" format
      final argsStr = resolvedActual.substring(ltIdx + 1, resolvedActual.length - 1);
      final actualArgs = _splitTypeArgs(argsStr);

      if (actualArgs.length != declaredType.typeArgs.length) return;

      for (int j = 0; j < actualArgs.length; j++) {
        // Recurse: a parameter may sit at any depth of a template's arguments.
        // `Channel(Stream(C), Stream(C))?` against `Channel<Stream<X>,Stream<X>>`
        // binds C only by descending into the argument, and until 2026-09-20
        // only a BARE argument bound, so C stayed free and the call had no
        // instantiation --- which is what left befriend_commit/7 and
        // intro_await_peer/3 uninstantiated in every program using them.
        _matchTypeForInference(
            declaredType.typeArgs[j], actualArgs[j], typeParams, bindings, env);
      }
    }
  }
}

/// Resolve a named (non-parameterized) type to its structural parameterized
/// form, honoring structural type identity (typed-program §20.3).  Recognizes
/// the canonical list shape `T ::= [] ; [E | T]`, whose structural form is
/// `Stream<E>`; every list-typed alias (OutputsList, NetInStream, UserInStream,
/// …) takes this shape.  Returns null when [typeName] is unknown, parameterized,
/// or not structurally a self-recursive list.  A single lookup — no recursion,
/// so it terminates — and the element type (the cons head) is unique.
String? _structuralFormOfNamedType(String typeName, TypeEnvironment env) {
  final def = env.getType(typeName);
  if (def == null || def.typeParams.isNotEmpty) return null;
  if (def.alternatives.length != 2) return null;
  var hasNil = false;
  ListConsAlt? cons;
  for (final alt in def.alternatives) {
    if (alt is ListNilAlt) {
      hasNil = true;
    } else if (alt is ListConsAlt) {
      cons = alt;
    }
  }
  if (!hasNil || cons == null) return null;
  // Tail must recurse on the type itself (the canonical Stream shape).
  final tail = cons.tail;
  if (tail is! TypeRef || tail.name != typeName || tail.typeArgs.isNotEmpty) {
    return null;
  }
  // Element type is the cons head, a simple named/concrete type.
  final head = cons.head;
  if (head is! TypeRef || head.typeArgs.isNotEmpty) return null;
  return 'Stream<${head.name}>';
}

/// Split comma-separated type args, respecting nested angle brackets.
List<String> _splitTypeArgs(String s) {
  final result = <String>[];
  var depth = 0;
  var start = 0;
  for (int i = 0; i < s.length; i++) {
    if (s[i] == '<') depth++;
    if (s[i] == '>') depth--;
    if (s[i] == ',' && depth == 0) {
      result.add(s.substring(start, i).trim());
      start = i + 1;
    }
  }
  if (start < s.length) {
    result.add(s.substring(start).trim());
  }
  return result;
}

/// Substitute type parameter names in a TypeExpr with concrete type names.
TypeExpr _substituteTypeParams(TypeExpr expr, Map<String, String> bindings) {
  if (expr is TypeRef) {
    if (expr.typeArgs.isEmpty && bindings.containsKey(expr.name)) {
      // Bare type param → concrete type name
      return TypeRef(bindings[expr.name]!, expr.line, expr.column, isInput: expr.isInput);
    }
    if (expr.typeArgs.isNotEmpty) {
      // Parameterized ref: substitute args and create expanded name
      final newArgs = expr.typeArgs.map((a) => _substituteTypeParams(a, bindings)).toList();
      // Check if all args are now concrete (no more type params)
      final allConcrete = newArgs.every((a) =>
          a is TypeRef && a.typeArgs.isEmpty && !bindings.containsKey(a.name));
      if (allConcrete) {
        // Create expanded name: Stream<AgentMsg>
        final expandedName = '${expr.name}<${newArgs.map((a) => (a as TypeRef).name).join(',')}>';
        return TypeRef(expandedName, expr.line, expr.column, isInput: expr.isInput);
      }
      return TypeRef(expr.name, expr.line, expr.column, isInput: expr.isInput, typeArgs: newArgs);
    }
    return expr;
  }
  if (expr is PrimitiveModeAlt) return expr;
  return expr;
}
