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
//    - dual types if both in head or both in body
//    - same type if one in head and one in body

import 'mode.dart';
import 'moded_term.dart';
import 'moded_head.dart';
import 'well_typed_term.dart';
import 'program_dfa.dart';
import 'subtyping.dart';
import 'type_ast.dart';
import 'root_scope.dart';
import '../../compiler/ast.dart' as ast;

// =============================================================================
// Per-instantiation collection (clause-template rule)
// =============================================================================

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
        activeInstantiations: activeInstantiations);

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
      activeInstantiations: activeInstantiations);
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
        activeInstantiations: activeInstantiations);

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
}) {
  // Handle SpawnGoal (Goal@Agent) - type-check the inner goal
  if (atom is ast.SpawnGoal) {
    // Recursively type-check the inner goal
    return _checkBodyAtomWithTerm(atom.innerGoal, atomIndex, dfa, env,
        callerVarTypes: callerVarTypes, collector: collector,
        activeInstantiations: activeInstantiations);
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
      final inferredDecl = _inferConcreteDecl(paramTemplate, atom, callerVarTypes, dfa, env);
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
        // One in head, one in body: require SAME type
        final (isSame, reason) = _areSameTypeWithReason(writerInfo, readerInfo, dfa);
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
bool _areDualTypes(VariableTypeInfo writerInfo, VariableTypeInfo readerInfo, ProgramDFA dfa) {
  final (isCompat, _) = _areDualTypesWithReason(writerInfo, readerInfo, dfa);
  return isCompat;
}

/// Check if two variable types are the SAME type
/// Per spec v0.9: For head-body pairs, types must have same BASE type
/// (e.g., _ and _? are same base type, Stream and Stream? are same base type)
(bool, String?) _areSameTypeWithReason(VariableTypeInfo writerInfo, VariableTypeInfo readerInfo, ProgramDFA dfa) {
  // For same-type check, the BASE types must be the same up to structural
  // identity (typed-program §20.3): a named alias and its structural form match.
  if (!sameBaseType(writerInfo.typeState.baseName, readerInfo.typeState.baseName, dfa)) {
    return (false, '${writerInfo.typeState.name} (base: ${writerInfo.typeState.baseName}) != ${readerInfo.typeState.name} (base: ${readerInfo.typeState.baseName})');
  }
  return (true, null);
}

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
        final declArg = declaredType.typeArgs[j];
        if (declArg is TypeRef && declArg.typeArgs.isEmpty && typeParams.contains(declArg.name)) {
          bindings.putIfAbsent(declArg.name, () => actualArgs[j]);
        }
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
