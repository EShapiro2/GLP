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
import 'meet.dart';
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

  /// Whether the instantiation binds a parameter to an input type.  The
  /// abstract instance certifies a parametric procedure only for bindings of
  /// the parameter's own mode (Lemma "Parametricity": "sigma replaces the
  /// parameter by a type of the same mode"), so such an instantiation is
  /// checked on its own.
  final bool bindsInputType;

  CollectedInstantiation(this.procKey, this.monoDecl, this.env, this.dfa,
      {this.bindsInputType = false});

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

/// Error: a guard atom tests a head occurrence at a type it cannot have.
///
/// TGLP typed-glp.tex, "Type checking of guards": the guard atom is well-typed
/// if the MEET of the occurrence's type and the type declared for the position
/// it occupies in the guard is non-empty.  An empty meet is a guard that can
/// never succeed --- no term is of both types --- and this is where it is named.
class GuardMeetError extends ClauseError {
  final String variableKey;
  final String guardFunctor;
  final DFAState occurrenceType;
  final DFAState guardType;

  GuardMeetError(this.variableKey, this.guardFunctor, this.occurrenceType,
      this.guardType);

  @override
  String get message =>
      'Guard $guardFunctor tests $variableKey at ${guardType.name}, which has no '
      'term in common with its type ${occurrenceType.name}: the meet is empty';

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
///
/// [isParametric] says whether the procedure of a "name/arity" key is
/// parametrically well-typed (TGLP parameterized-types.tex, Definition
/// "Parametrically Well-Typed").  A call to a parameterised procedure for which
/// no instantiation is found is refused unless it is (TGLP
/// appendix-implementation-notes.tex, "The instantiation of a call").  Null
/// takes every callee to be, and checks every such call with the callee's
/// parameters open.
ClauseCheckResult checkClause(
  TypedClause clause,
  ProgramDFA dfa,
  TypeEnvironment env, {
  InstantiationCollector? collector,
  Map<String, ProcDecl> activeInstantiations = const {},
  CalleeClauses? callee,
  bool Function(String procKey)? isParametric,
}) {
  final errors = <ClauseError>[];
  final allVariableTypes = <String, VariableTypeInfo>{};
  final variableLocations = <String, String>{};
  ModedTerm? constructedModedHead;
  final constructedModedBodyAtoms = <ModedTerm>[];

  // The body side of each head/body variable pair of condition 3(b).
  //
  // The head's variable types below are read off the MODED head, whose step 2
  // (def:moded-head) has already replaced every head variable by its paired
  // variable.  A source head occurrence `X?` is therefore recorded here under
  // the key `X`, and a source head occurrence `X` under the key `X?` --- which
  // is the very key its body partner carries.  So a head/body pair of
  // def:well-typed-clause condition 3 is ONE key in this map, not two, and it
  // is the keys that collide, below, that are those pairs.
  //
  // EVERY body occurrence is kept, not the first.  Condition 3 is "for every
  // variable pair X and X? in C", and a reader of a constant type may occur
  // more than once (TGLP typed-glp.tex, SRSW*), so each of its body occurrences
  // is a pair with the head's and is compared with it.  Until 2026-09-27 only
  // the first was kept (putIfAbsent), and a second occurrence at a type the
  // head's is not within went unchecked.
  final headBodyPairs = <String, List<(VariableTypeInfo, String)>>{};

  // Every body occurrence of each key whose first occurrence is in the body,
  // for condition 3(a) on every body/body pair ([_checkBodyBodyPairs]).
  final bodyOccurrences = <String, List<(VariableTypeInfo, String)>>{};

  // A head occurrence a guard atom has NARROWED, by the key the head carries it
  // under.  "A guard atom that tests the type of a head occurrence narrows it
  // ... the occurrence has that meet as its type in the body, where condition 3
  // of Definition (Well-Typed Clause) is applied to it" (TGLP typed-glp.tex,
  // "Type checking of guards").  So it is this type, not the head's own, that
  // condition 3(b) compares against the body below.
  final narrowedByGuard = <String, VariableTypeInfo>{};

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

  // The instantiation of each call to a parameterised procedure, read over the
  // whole clause before any goal is checked, the body goals in no order (TGLP
  // appendix-implementation-notes.tex, "The instantiation of a call";
  // [_instantiateCalls]).  It types the head for itself, and Step 1 types it
  // again, which numbers the clause's anonymous variables from the start as
  // before.
  final plans = _instantiateCalls(clause, procDecl, dfa, env,
      activeInstantiations: activeInstantiations,
      callee: callee,
      isParametric: isParametric);

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

  // The calls to parameterised procedures no instantiation was inferred for,
  // with their goal index and the callee's template: asked of below, once the
  // clause is typed (Step 5).
  final uninstantiated = <(int, ast.Goal, ProcDecl)>[];

  // Step 2: Check each body atom
  for (int i = 0; i < clause.bodyAtoms.length; i++) {
    final atom = clause.bodyAtoms[i];
    final (atomResult, modedAtomTerm) = _checkBodyAtomWithTerm(atom, i, dfa, env,
        callerVarTypes: allVariableTypes, collector: collector,
        activeInstantiations: activeInstantiations,
        callee: callee,
        plan: plans[i],
        onUninstantiated: (call, template) =>
            uninstantiated.add((i, call, template)));

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
        // A key the head already carries is this occurrence's partner in the
        // head, by the complementation above: keep the body side for condition
        // 3(b).  Until 2026-09-23 it was dropped here, on the comment that the
        // complementarity check below would catch it; that check pairs `X` with
        // `X?` and never sees a head/body pair, so condition 3(b) went
        // unchecked entirely and `held2(N?) :- self_key(N).` --- an `Integer`
        // handed out where the body produces a `Key` --- loaded.
        //
        // A key an EARLIER BODY ATOM carries is the same source form occurring
        // again in the body, which SRSW admits only under a relaxation (a
        // reader of a constant type, SRSW*).  It is no pair with the earlier
        // occurrence, but it is one with the body occurrence of its partner,
        // so it is kept for condition 3(a) ([_checkBodyBodyPairs]).
        if (variableLocations[varKey] != 'head') {
          bodyOccurrences[varKey]?.add((newInfo, 'body atom $i'));
        }
        if (variableLocations[varKey] == 'head') {
          if (i < clause.guardAtoms.length) {
            // A GUARD atom: it narrows the occurrence rather than being
            // measured against it.  S is the type the occurrence has so far ---
            // the head's, or what an earlier guard already narrowed it to --- and
            // T is the type declared for the position it occupies in this guard.
            final have = narrowedByGuard[varKey] ?? allVariableTypes[varKey]!;
            final met =
                meetOfTypes(have.typeState, newInfo.typeState, dfa, env);
            if (met == null) {
              errors.add(GuardMeetError(
                  varKey, atom.functor, have.typeState, newInfo.typeState));
            } else {
              narrowedByGuard[varKey] = VariableTypeInfo(
                typeState: met,
                mode: have.mode,
                isReader: have.isReader,
              );
            }
          } else {
            // The body goal is named, not just numbered: a 3(b) refusal is
            // almost always a DECLARATION at fault, and the reader has to know
            // which procedure's declaration to look at.
            headBodyPairs.putIfAbsent(varKey, () => []).add(
                (newInfo, '${atom.functor}/${atom.arity} (body atom $i)'));
          }
        }
      } else {
        allVariableTypes[varKey] = newInfo;
        variableLocations[varKey] = 'body atom $i';
        bodyOccurrences[varKey] = [(newInfo, 'body atom $i')];
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
  errors.addAll(_checkBodyBodyPairs(bodyOccurrences, dfa));

  // Step 4: condition 3(b) of def:well-typed-clause on the head/body pairs,
  // applied to the type a guard narrowed the head occurrence to where one did.
  errors.addAll(_checkHeadBodyPairs(
    {...allVariableTypes, ...narrowedByGuard},
    variableLocations,
    headBodyPairs,
    dfa,
  ));

  // Step 5: a call whose bindings conflict --- types supplied or fixed for
  // every parameter, and none serving --- is refused: "where types are
  // supplied or fixed and none serves, the bindings conflict and the call is
  // refused" (TGLP appendix-implementation-notes.tex, "The instantiation of a
  // call", cc4a891).  It was checked above at the binding leaving the fewest
  // sites ill-typed, so the clause check has named the sites that binding
  // does not fit; this names the conflict.
  for (final e in plans.entries) {
    final plan = e.value;
    if (plan.conflict == null) continue;
    errors.add(BodyAtomError(plan.goal.functor, e.key, [
      ConflictingBindingsError(
          plan.goal, plan.template, plan.tried, plan.conflict!,
          calleeRead: plan.calleeRead)
    ]));
  }

  // A call to a parameterised procedure for which no instantiation is found,
  // some parameter having no type supplied by a site or fixed by the callee's
  // clauses: "A parameter for which no type is supplied or fixed is left open
  // where the callee is parametrically well-typed (Section
  // sec:abstract-parameters), the call checked with it open ...; otherwise the
  // call is refused" (TGLP appendix-implementation-notes.tex, "The
  // instantiation of a call", cc4a891).  [_instantiateCalls] has read each
  // such call: refused where its callee is not parametrically well-typed;
  // else checked with those parameters open, the others at the types tried
  // for them, and refused where no map of the open ones can make it
  // well-typed (TGLP parameterized-types.tex, Definition "Instantiation"),
  // its arguments compared with the other occurrence of each variable in the
  // clause as condition 3 pairs them: a head occurrence (the type a guard
  // narrowed it to, where one did) under 3(b), a body occurrence of the other
  // polarity under 3(a).  A call with no reading --- none is, in a clause ---
  // is asked so here.
  List<(VariableTypeInfo, bool)> partnersOf(String name, bool reader) {
    final own = reader ? '$name?' : name;
    final other = reader ? name : '$name?';
    final head = variableLocations[own] == 'head'
        ? (narrowedByGuard[own] ?? allVariableTypes[own])
        : null;
    return [
      if (head != null) (head, true),
      for (final (info, _) in bodyOccurrences[other] ??
          const <(VariableTypeInfo, String)>[])
        (info, false),
    ];
  }
  for (final (i, call, template) in uninstantiated) {
    final plan = plans[i];
    if (plan == null) {
      if (isParametric != null && !isParametric(template.key)) {
        errors.add(BodyAtomError(call.functor, i,
            [UninstantiatedCallError(call, template, template.typeParams)]));
        continue;
      }
      final reason = _noInstantiationReason(call, template, partnersOf, env);
      if (reason != null) {
        errors.add(BodyAtomError(
            call.functor, i, [NoInstantiationError(call, template, reason)]));
      }
      continue;
    }
    if (plan.refutation != null) {
      // No map of the parameters can make the call well-typed
      // ([_instantiateCalls]).
      errors.add(BodyAtomError(call.functor, i,
          [NoInstantiationError(call, template, plan.refutation!)]));
      continue;
    }
    if (plan.conflict != null) continue; // named above
    if (plan.refused) {
      errors.add(BodyAtomError(call.functor, i,
          [UninstantiatedCallError(call, template, plan.unsupplied)]));
    }
    // Otherwise left open, and well-typed so ([_instantiateCalls]).
  }

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
  bool Function(String procKey)? isParametric,
}) {
  // Convert ast.Clause to TypedClause
  // Note: ast.Clause.head is Atom, but Goal has same structure
  final head = ast.Goal(clause.head.functor, clause.head.args, clause.line, clause.column);

  // Convert guards to goals (guards are procedure calls for type checking).
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
      callee: callee,
      isParametric: isParametric);
}

/// The guard meet errors of [clause]'s DEFINED guards, the clause taken as
/// written, before its defined guards are unfolded.
///
/// A defined guard is a guard atom: its argument is checked as a built-in
/// guard's is.  "Let S be the type of the occurrence and T the type declared
/// for the position it occupies in the guard.  The guard atom is well-typed if
/// the meet of S and T ... is non-empty" (TGLP typed-glp.tex, "Type checking of
/// guards").  The partial evaluator unfolds a defined guard into the clause
/// before the clause is checked (GLP-Spec appendix-guards.tex, "Defined guard
/// predicates"), so the atom is gone from the clause [checkClause] sees, and it
/// is asked of the clause as written here.  [definedGuards] names the guard
/// predicates (name/arity) the partial evaluator unfolds.  A guard over a type
/// with no term in common with the occurrence's can never succeed: until
/// 2026-10-02 `p(A) :- close(A?) | true.` with `A` at `Request?` was refused
/// only for the head the unfolding wrote, `p(ch([], []))`, and not against
/// `close(Channel(Closed, Closed)?)` (GLP 2026-10-01 23:58 UTC item 5).
List<GuardMeetError> definedGuardMeetErrors(ast.Clause clause,
    Set<String> definedGuards, ProgramDFA dfa, TypeEnvironment env) {
  final guards = [
    for (final g in clause.guards ?? const <ast.Guard>[])
      if (definedGuards.contains('${g.predicate}/${g.args.length}'))
        ast.Goal(g.predicate, g.args, g.line, g.column)
  ];
  if (guards.isEmpty) return const [];
  final procDecl =
      env.getProcedure(clause.head.functor, clause.head.args.length);
  if (procDecl == null) return const [];
  final head =
      ast.Goal(clause.head.functor, clause.head.args, clause.line, clause.column);
  final typed = TypedClause(head: head, bodyAtoms: guards, guardAtoms: guards);
  final (headResult, _) = _checkHeadWithTerm(typed, procDecl, dfa, env);

  // As [checkClause] meets a guard atom with the head occurrence it tests:
  // the occurrence's type so far --- the head's, or what an earlier guard
  // narrowed it to --- against the type the guard declares for its position.
  final narrowed = <String, VariableTypeInfo>{};
  final errors = <GuardMeetError>[];
  for (var i = 0; i < guards.length; i++) {
    final atom = guards[i];
    final (atomResult, _) = _checkBodyAtomWithTerm(atom, i, dfa, env,
        callerVarTypes: headResult.variableTypes);
    for (final entry in atomResult.variableTypes.entries) {
      final have = narrowed[entry.key] ?? headResult.variableTypes[entry.key];
      if (have == null) continue; // not an occurrence the head carries
      final met = meetOfTypes(have.typeState, entry.value.typeState, dfa, env);
      if (met == null) {
        errors.add(GuardMeetError(
            entry.key, atom.functor, have.typeState, entry.value.typeState));
      } else {
        narrowed[entry.key] = VariableTypeInfo(
            typeState: met, mode: have.mode, isReader: have.isReader);
      }
    }
  }
  return errors;
}

/// The base names of the variables of [clause] whose type at some occurrence
/// is a CONSTANT TYPE (TGLP `typed-glp.tex`, \mypara{Readers of constant
/// types}).  Proposition "Readers of Constant Types" licenses several
/// occurrences of such a reader, its paired writer occurring once, and the
/// relaxation "holds wherever the occurrences sit --- in the head, nested within
/// an argument, or in the body --- since it rests on what the term is and not on
/// where it is read."
///
/// So the question is asked of EVERY occurrence, and of the type the occurrence
/// has (Definition "Type Assignment": the state the automaton reaches by the
/// path from the root to that position), not of the top-level type name of a
/// head argument.  A head occurrence's type comes off the moded head
/// (Definition "Moded Head"), a body occurrence's off the produced moded term of
/// its unit goal, and a guard's off the guard atom, guards being type-checked as
/// a conjunction with the body.  One occurrence carrying a constant type
/// settles it: the type of any occurrence bounds the values the variable may
/// carry, and a constant type bounds them to constants.
///
/// The key is the BASE name --- the moded head carries `X` at the key `X?` and
/// `X?` at the key `X` (Definition "Moded Head", step 2), and SRSW counts a
/// variable and its pair together.
///
/// Errors are not collected: this is asked of clauses the checker has passed or
/// will reject on its own, and an occurrence whose path is inconsistent simply
/// yields no type and licenses nothing.
Set<String> constantTypedVariables(
        ast.Clause clause, ProgramDFA dfa, TypeEnvironment env) =>
    _variablesTypedAtSomeOccurrence(
        clause, dfa, env, (state) => isConstantType(state, env.types));

/// The base names of the variables of [clause] a reader of which may occur
/// more than once by the type of some occurrence ([licensesRepeatedReader]): a
/// constant type, as [constantTypedVariables] asks, or `MutualRef` --- "A
/// reader of type `MutualRef` may also occur more than once" (TGLP
/// `typed-glp.tex`, 350eb7d).  Each occurrence's type is asked as there: the
/// head's off the moded head, a body goal's off its produced moded term, and a
/// guard's off the guard atom, so `is_mutual_ref(Ref?)` --- declared
/// `procedure is_mutual_ref(MutualRef?)` --- gives the occurrence it tests the
/// type `MutualRef?` it narrows it to ("Type checking of guards").  This is what
/// the analyzer's SRSW check asks.
Set<String> repeatableReaderVariables(
        ast.Clause clause, ProgramDFA dfa, TypeEnvironment env) =>
    _variablesTypedAtSomeOccurrence(
        clause, dfa, env, (state) => licensesRepeatedReader(state, env.types));

Set<String> _variablesTypedAtSomeOccurrence(ast.Clause clause, ProgramDFA dfa,
    TypeEnvironment env, bool Function(DFAState) qualifies) {
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

  final licensed = <String>{};
  void take(Map<String, VariableTypeInfo> types) {
    for (final entry in types.entries) {
      if (!qualifies(entry.value.typeState)) continue;
      final key = entry.key;
      licensed.add(key.endsWith('?') ? key.substring(0, key.length - 1) : key);
    }
  }

  final (headResult, _) = _checkHeadWithTerm(typedClause, procDecl, dfa, env);
  take(headResult.variableTypes);

  for (final atom in typedClause.bodyAtoms) {
    take(_bodyAtomVariableTypes(atom, dfa, env));
  }

  return licensed;
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
  final bodyOccurrences = <String, List<(VariableTypeInfo, String)>>{};
  final uninstantiated = <(int, ast.Goal, ProcDecl)>[];

  // Condition 2: each unit goal's produced moded term is well-typed by D.
  for (int i = 0; i < goalAtoms.length; i++) {
    final atom = goalAtoms[i];
    final (atomResult, modedAtomTerm) = _checkBodyAtomWithTerm(atom, i, dfa, env,
        callerVarTypes: allVariableTypes, collector: collector,
        activeInstantiations: activeInstantiations,
        callee: callee,
        onUninstantiated: (call, template) =>
            uninstantiated.add((i, call, template)));

    if (modedAtomTerm != null) {
      constructedModedBodyAtoms.add(modedAtomTerm);
    }

    if (!atomResult.isWellTyped) {
      errors.add(BodyAtomError(atom.functor, i, atomResult.errors));
    }

    for (final entry in atomResult.variableTypes.entries) {
      allVariableTypes.putIfAbsent(entry.key, () => entry.value);
      variableLocations.putIfAbsent(entry.key, () => 'body atom $i');
      bodyOccurrences
          .putIfAbsent(entry.key, () => [])
          .add((entry.value, 'body atom $i'));
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
  errors.addAll(_checkBodyBodyPairs(bodyOccurrences, dfa));

  // A call no instantiation was inferred for, refused where no map of the
  // callee's parameters can make it well-typed, as in [checkClause]: a goal is
  // a body, so each argument is compared with the occurrences of the other
  // polarity (condition 3(a)).
  List<(VariableTypeInfo, bool)> partnersOf(String name, bool reader) => [
        for (final (info, _) in bodyOccurrences[reader ? name : '$name?'] ??
            const <(VariableTypeInfo, String)>[])
          (info, false),
      ];
  for (final (i, call, template) in uninstantiated) {
    final reason = _noInstantiationReason(call, template, partnersOf, env);
    if (reason != null) {
      errors.add(BodyAtomError(
          call.functor, i, [NoInstantiationError(call, template, reason)]));
    }
  }

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
///
/// [onUninstantiated] is told of a call to a parameterised procedure for which
/// no instantiation is inferred, with the callee's template; the caller asks of
/// it, once the clause is typed, whether any instantiation can exist
/// ([_noInstantiationReason]).
///
/// [plan] is the instantiation [_instantiateCalls] read for the call over the
/// whole clause, which a clause check gives for every call to a parameterised
/// procedure; with none --- a goal ([checkGoal]), a guard as written
/// ([definedGuardMeetErrors]) --- the instantiation is inferred from
/// [callerVarTypes], the occurrences typed before the call.
(WellTypedResult, ModedTerm?) _checkBodyAtomWithTerm(
  ast.Goal atom,
  int atomIndex,
  ProgramDFA dfa,
  TypeEnvironment env, {
  Map<String, VariableTypeInfo>? callerVarTypes,
  InstantiationCollector? collector,
  Map<String, ProcDecl> activeInstantiations = const {},
  CalleeClauses? callee,
  _CallPlan? plan,
  void Function(ast.Goal call, ProcDecl template)? onUninstantiated,
}) {
  // Handle SpawnGoal (Goal@Agent) - type-check the inner goal
  if (atom is ast.SpawnGoal) {
    // Recursively type-check the inner goal
    return _checkBodyAtomWithTerm(atom.innerGoal, atomIndex, dfa, env,
        callerVarTypes: callerVarTypes, collector: collector,
        activeInstantiations: activeInstantiations,
        callee: callee,
        plan: plan,
        onUninstantiated: onUninstantiated);
  }

  // Handle RemoteGoal (M # proc(...)) - type-check against imported declaration
  if (atom is ast.RemoteGoal) {
    return _checkRemoteGoal(atom, atomIndex, dfa, env,
        callerVarTypes: callerVarTypes, plan: plan);
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

  // The environment the call's arguments are checked in: [env], unless the
  // call's instantiation needs types [env] cannot be given (see
  // [_buildDeclTypes]).
  var checkEnv = env;

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
    } else if (plan != null) {
      // The instantiation read over the whole clause ([_instantiateCalls]).
      final decl = plan.decl;
      if (decl == null) {
        // Some parameter is supplied no type by any site of the call, so no
        // instantiation is found.  The modes the template fixes are checked
        // here; the clause check refuses the call unless its callee is
        // parametrically well-typed, and checks it with the callee's
        // parameters open where it is ([checkClause], Step 5).
        onUninstantiated?.call(atom, paramTemplate);
        return (_checkArgumentModes(atom, paramTemplate, env), null);
      }
      // Clause-template rule: the instantiation is recorded, so that the
      // callee's defining clauses are checked by it (Phase 2 / instantiation
      // closure), and the call's own arguments are checked by the declaration
      // it produces, its types built first (TGLP def:instantiation; see the
      // inference below).  A call whose bindings conflict is refused
      // ([checkClause], Step 5) and is checked at the nearest binding, so that
      // the clause check names the sites that binding does not fit and the
      // closure the callee's clauses that are not well-typed by it.
      collector?.record(CollectedInstantiation(decl.key, decl, env, dfa,
          bindsInputType: plan.bindsInputType));
      procDecl = decl;
      checkEnv = _buildDeclTypes(decl, dfa, env);
    } else if (callerVarTypes != null && callerVarTypes.isNotEmpty) {
      final inferred = _inferConcreteDecl(
          paramTemplate, atom, callerVarTypes, dfa, env, callee);
      if (inferred != null) {
        final (inferredDecl, bindsInputType) = inferred;
        // Clause-template rule: record this instantiation so the parameterized
        // procedure's defining clauses are re-checked against it (Phase 2 /
        // instantiation closure). Then fall through to type the call site's own
        // arguments against the inferred concrete declaration: the call site is
        // itself a clause that must be well-typed (its variable-pair duality is
        // checked against the concrete element type), and typing the arguments
        // is also what lets closure infer instantiations through a parameterized
        // call's output (the output variable receives its concrete type here).
        collector?.record(CollectedInstantiation(
            inferredDecl.key, inferredDecl, env, dfa,
            bindsInputType: bindsInputType));
        procDecl = inferredDecl;
        // The inferred instantiation may name types that arise only through the
        // closure (e.g. Stream<Box<Msg>> from a type-changing procedure, or
        // Stream<NetInMsg> where the caller's stream is a named list) and are
        // not yet built.  They are built here, before the call's arguments are
        // checked: TGLP def:instantiation makes the CALLER's clause part of what
        // the instantiation must make well-typed ("C and the clauses of q are
        // well-typed when q's declaration is replaced by its expansion under
        // theta"), so the call is checked by that expansion and not by the
        // modes alone.  Until 2026-09-27 the modes alone were checked here and
        // the calling clause was never checked again once the closure had built
        // the types, so typed_social_agent.glp's agent/4 handed handle_response/6
        // a Constant where it takes a Key, unrefused.
        checkEnv = _buildDeclTypes(inferredDecl, dfa, env);
      } else {
        // No instantiation inferred.  The inference reads equations from the
        // call's variables and the callee's clauses, and a call whose
        // arguments fix no parameter at this point --- a fresh writer a later
        // goal types, a constructed term --- leaves it with none although the
        // call has one, so the call is not refused for that alone: TGLP
        // parameterized-types.tex, Definition "Instantiation", refuses a call
        // only where no map of the parameters makes it well-typed.  The MODES
        // the template fixes are checked here, and the call goes to
        // [onUninstantiated], so that the clause check asks, once every
        // occurrence in the clause is typed, whether any map could make the
        // call well-typed, and refuses it where none can
        // ([_noInstantiationReason]).  Until 2026-10-02 nothing asked, so a
        // call no map makes well-typed --- list_to_bst/2 handing split_at/5 a
        // Stream(X) where it reads a NonEmptyList(X)? --- loaded.
        onUninstantiated?.call(atom, paramTemplate);
        return (_checkArgumentModes(atom, paramTemplate, env), null);
      }
    } else {
      // No caller variable types available — can't infer type params.  As
      // above: the modes are checked here, and the clause check asks whether
      // any map can make the call well-typed.
      onUninstantiated?.call(atom, paramTemplate);
      return (_checkArgumentModes(atom, paramTemplate, env), null);
    }
  }

  // Build produced term (no variable flip for body atoms)
  try {
    final modedAtomTerm = producedTerm(atom, procDecl, typeEnv: checkEnv);

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

/// Build, in [env] and [dfa], the monomorphic types [decl] names that neither
/// holds yet: the expansions (TGLP parameterized-types.tex sec:param-expansion)
/// of the template instantiations an inferred instantiation of a call names,
/// nested ones included, so that the call is checked by the declaration the
/// instantiation produces (def:instantiation).  Returns the environment to
/// check the call in: [env] itself, grown by the new definitions, or --- where
/// [env]'s map of types cannot grow --- an environment extending it by them.
/// A name no template builds is left unbuilt, and the argument check names it.
TypeEnvironment _buildDeclTypes(
    ProcDecl decl, ProgramDFA dfa, TypeEnvironment env) {
  final needed = <String>{};
  for (final t in decl.argTypes) {
    var n = getFullTypeName(t);
    if (n.endsWith('?')) n = n.substring(0, n.length - 1);
    if (n.contains('<') && !dfa.automata.containsKey(n)) needed.add(n);
  }
  if (needed.isEmpty) return env;
  final built = materializeInstantiations(
      needed, env.typeTemplates, {...env.types.keys});
  if (built.isEmpty) return env;
  var into = env;
  try {
    env.types.addAll(built);
  } on UnsupportedError {
    into = TypeEnvironment(
      {...env.types, ...built},
      env.procedures,
      paramProcDecls: env.paramProcDecls,
      typeTemplates: env.typeTemplates,
      typeOrigins: env.typeOrigins,
    );
  }
  addTypesToProgramDFA(dfa, built.values, into.types);
  return into;
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
/// This runs at the two points where the full per-argument check cannot: when
/// call-site inference binds no parameter, and when no caller variable types are
/// available.  (It ran at a third until 2026-09-27, where the inferred
/// instantiation named types not yet built; they are built there now, and the
/// call is checked by the instantiation --- [_buildDeclTypes].)  Until 2026-08-02
/// every such point returned success, so a call to the
/// root scope's `merge`, `send`, `receive` or `new_channel` with a writer and a
/// reader transposed was passed in silence — the error class
/// `sections/introduction.tex` gives as the paper's motivating example, and the
/// one the same call to a monomorphic procedure has always been rejected for.
///
/// 🔴 **Not at a bare parameter.** An argument whose declared type is a
/// parameter itself, `X` or `X?`, has the mode of the type the instantiation
/// binds there, and Definition (Instantiation) (`parameterized-types.tex`) maps
/// a parameter to any type of the program, an input type included
/// (`typed-glp.tex`, "Type Declarations": `Stream?` is a type), complementation
/// being an involution, $(T?)? = T$ (`appendix-type-automaton.tex`, Definition
/// "Dual Type Automaton").  So `procedure(X) p(Constant?, X)` takes a writer at 2 where
/// `X` is bound to an output type and a reader where it is bound to an input
/// type, and neither is fixed by the template: the mode there is decided with
/// the binding, in [_inferConcreteDecl], or by the atoms that type the variable.
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
    if (_isBareParameter(decl.argTypes[i], decl.typeParams)) continue;
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
/// An imported declaration that names type parameters is checked as a local
/// call to a parameterised procedure is, and never at its wildcard copy.  TGLP
/// `modules.tex`, "Cross-module type checking": "Where the imported declaration
/// names type parameters, as an exported one may, the call instantiates them as
/// a local call does (Definition (Instantiation)): a parameter the importing
/// module holds open stays open across the module boundary and is fixed at the
/// call, the clauses of the called procedure being those of the linked
/// program."  So the call's instantiation is inferred from [callerVarTypes] ---
/// where the caller holds a parameter open, as in the abstract instance of a
/// forwarding clause, the call is fixed at that abstract type --- and the call
/// is checked by the expansion under it.  The callee's clauses are not this
/// module's, so none are consulted and no instantiation is recorded: the
/// linked program, where the call is local, checks them.  The types the
/// instantiation names are built first where this DFA lacks them
/// ([_buildDeclTypes]).  Where the caller's arguments do not fix the
/// instantiation, only the modes the template fixes are checked here and the
/// rest is the linked program's, where the call is local and is refused if no
/// instantiation can make it well-typed ([_noInstantiationReason]).
///
/// In a clause the instantiation is the one [_instantiateCalls] read over the
/// whole clause, [plan]; with none --- a goal --- it is inferred from
/// [callerVarTypes].
(WellTypedResult, ModedTerm?) _checkRemoteGoal(
  ast.RemoteGoal remote,
  int atomIndex,
  ProgramDFA dfa,
  TypeEnvironment env, {
  Map<String, VariableTypeInfo>? callerVarTypes,
  _CallPlan? plan,
}) {
  // Flatten nested RemoteGoals to extract full module path and actual goal.
  // Example: ui#actors # render(X?) parses as RemoteGoal(ui, RemoteGoal(actors, render(X?)))
  // We need: modulePath = "ui#actors", innerGoal = render(X?)
  final pathParts = <String>[];
  ast.Goal innerGoal = remote;
  while (innerGoal is ast.RemoteGoal) {
    final rg = innerGoal as ast.RemoteGoal;
    pathParts.add(rg.staticModuleName);
    innerGoal = rg.goal;
  }
  final modulePath = pathParts.join('#');
  final goalFunctor = innerGoal.functor;
  final goalArity = innerGoal.arity;

  // Look up: 'modulePath#goalFunctor/arity'
  final qualifiedKey = '$modulePath#$goalFunctor/$goalArity';

  // A parameterised import: instantiate at the call, never the wildcard copy
  // env.procedures holds for it (param_expansion.dart, step 5).
  final paramTemplate = env.paramProcDecls[qualifiedKey];
  if (paramTemplate != null) {
    final ProcDecl? inferred = plan != null
        ? plan.decl
        : (callerVarTypes != null && callerVarTypes.isNotEmpty)
            ? _inferConcreteDecl(
                    paramTemplate, innerGoal, callerVarTypes, dfa, env, null)
                ?.$1
            : null;
    if (inferred != null) {
      // No instantiation is recorded here, so whether it binds an input type
      // is the linked program's to act on, where the call is local.
      // The types the instantiation names are built before the call is
      // checked by it, as for a local call (_checkBodyAtomWithTerm).
      final checkEnv = _buildDeclTypes(inferred, dfa, env);
      try {
        final modedAtomTerm =
            producedTerm(innerGoal, inferred, typeEnv: checkEnv);
        return (
          _checkModedTermPerArg(modedAtomTerm, inferred, dfa),
          modedAtomTerm
        );
      } on ArityMismatchError catch (e) {
        return (WellTypedResult.failure([
          InconsistentPathError(
            ModedPath([PathStep(symbol: e.message, argIndex: 0, mode: Mode.produce)]),
            e.message,
          ),
        ]), null);
      }
    }
    return (_checkArgumentModes(innerGoal, paramTemplate, env), null);
  }

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
          if (!sameOccurrenceType(
              variableTypes[varKey]!, result.variableAssignment!, dfa)) {
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
          final e = _bodyBodyPairError(
              baseName, writerInfo, readerInfo, writerLoc, readerLoc, dfa);
          if (e != null) errors.add(e);
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

/// Condition 3(a) on one body/body pair: both in body, so subtyping S <: T
/// (Definition 4.8).  Writer X has output type S; reader X? has dual type T?.
/// Need S <: T, both output types.  Null when the pair is well-typed.
ClauseDualityError? _bodyBodyPairError(
  String baseName,
  VariableTypeInfo writerInfo,
  VariableTypeInfo readerInfo,
  String writerLoc,
  String readerLoc,
  ProgramDFA dfa,
) {
  final writerOutputState = writerInfo.typeState; // S (output, not dual)
  final readerDualState = readerInfo.typeState;   // T? (dual)
  final readerOutputState = dfa.getState(readerDualState.baseName); // T (output)
  if (isSubtype(writerOutputState, readerOutputState, dfa)) return null;
  return ClauseDualityError(
    baseName,
    writerInfo,
    readerInfo,
    writerLoc,
    readerLoc,
    'Body variable pair: writer type ${writerOutputState.name} is not a subtype of ${readerOutputState.name}',
  );
}

/// Condition 3(a) on EVERY body/body pair, where [_checkClauseDuality] sees
/// the first body occurrence of each key only.
///
/// TGLP def:well-typed-clause condition 3 is "for every variable pair X and X?
/// in C", and a reader of a constant type may occur in the body more than once
/// (TGLP typed-glp.tex, SRSW*), so each body occurrence of X? is a pair with
/// the body occurrence of X and is compared with it.  [bodyOccurrences] holds,
/// for each key whose first occurrence is in the body, every body occurrence in
/// order, with the body atom it sits in; the pair of the two first occurrences
/// is [_checkClauseDuality]'s and is skipped here.  Until 2026-09-29 the later
/// occurrences were dropped (putIfAbsent), and a second `X?` at a type `X` is
/// not within went unchecked.
List<ClauseDualityError> _checkBodyBodyPairs(
  Map<String, List<(VariableTypeInfo, String)>> bodyOccurrences,
  ProgramDFA dfa,
) {
  final errors = <ClauseDualityError>[];
  for (final entry in bodyOccurrences.entries) {
    final writerKey = entry.key;
    if (writerKey.endsWith('?')) continue;
    final readers = bodyOccurrences['$writerKey?'];
    if (readers == null) continue;
    final writers = entry.value;
    for (var w = 0; w < writers.length; w++) {
      for (var r = 0; r < readers.length; r++) {
        if (w == 0 && r == 0) continue;
        final (writerInfo, writerLoc) = writers[w];
        final (readerInfo, readerLoc) = readers[r];
        final e = _bodyBodyPairError(
            writerKey, writerInfo, readerInfo, writerLoc, readerLoc, dfa);
        if (e != null) errors.add(e);
      }
    }
  }
  return errors;
}

/// Condition 3(b) of def:well-typed-clause-subtyping on the head/body pairs:
/// "if one occurs in the head and the other in the body, the dual of the type of
/// the head occurrence is a subtype of the dual of the type of the body
/// occurrence."  Equality --- condition 3(b) of the base def:well-typed-clause,
/// "they have the same type" --- is strictly stronger, so nothing that reading
/// admits is refused here.
///
/// The two occurrences carry the same mode (sec:subtyping), so both types are
/// output or both are input, and dualising turns the one case into the other:
///
/// * At a CONSUMED position the types are `T?` and `U?`, their duals `T` and
///   `U`, and the condition is `T <: U` --- what the head occurrence receives is
///   within what the body occurrence accepts.
/// * At a PRODUCED position the types are `T` and `U`, their duals `T?` and
///   `U?`, and `A? <: B?` is `B <: A` (sec:subtyping, "Subtyping extends to
///   input types by complementation"), so the condition is `U <: T` --- what the
///   body occurrence produces is within what the head occurrence hands out.
///
/// [headTypes] / [headLocations] are the clause's variable types, in which the
/// head's entries hold the key; [bodyOccurrences] holds, for each key, the body
/// side of EVERY pair it is in --- each body occurrence with the body goal it
/// sits in --- collected in [checkClause] where the body key met the head's.
/// Each is compared with the head's: condition 3 is "for every variable pair",
/// and a reader of a constant type may occur in the body more than once
/// (SRSW*).
List<ClauseDualityError> _checkHeadBodyPairs(
  Map<String, VariableTypeInfo> headTypes,
  Map<String, String> headLocations,
  Map<String, List<(VariableTypeInfo, String)>> bodyOccurrences,
  ProgramDFA dfa,
) {
  final errors = <ClauseDualityError>[];

  for (final entry in bodyOccurrences.entries) {
    for (final (bodyInfo, bodyLocation) in entry.value) {
      errors.addAll(_checkHeadBodyPair(
          entry.key, headTypes[entry.key], headLocations, bodyInfo,
          bodyLocation, dfa));
    }
  }

  return errors;
}

/// Condition 3(b) on one head/body pair: the head's occurrence of [varKey]
/// against one body occurrence, [bodyInfo] at [bodyLocation].
List<ClauseDualityError> _checkHeadBodyPair(
  String varKey,
  VariableTypeInfo? headInfo,
  Map<String, String> headLocations,
  VariableTypeInfo bodyInfo,
  String bodyLocation,
  ProgramDFA dfa,
) {
  if (headInfo == null) return const [];

  // A base name with no state of its own has no type to compare; the type
  // error, if there is one, is the term check's to report.
  final headBase = dfa.states[headInfo.typeState.baseName];
  final bodyBase = dfa.states[bodyInfo.typeState.baseName];
  if (headBase == null || bodyBase == null) return const [];
  if (headBase.isDual || bodyBase.isDual) return const [];

  // Consumed: T <: U.  Produced: U <: T.
  final consumed = headInfo.typeState.isDual;
  final (sub, sup) = consumed ? (headBase, bodyBase) : (bodyBase, headBase);
  if (isSubtype(sub, sup, dfa)) return const [];

  final baseName =
      varKey.endsWith('?') ? varKey.substring(0, varKey.length - 1) : varKey;
  return [
    ClauseDualityError(
      baseName,
      headInfo,
      bodyInfo,
      headLocations[varKey] ?? 'head',
      bodyLocation,
      consumed
          ? 'Variables across head/body: the head occurrence receives '
              '${headBase.name}, which is not within what the body occurrence '
              'accepts (${bodyBase.name})'
          : 'Variables across head/body: the body occurrence produces '
              '${bodyBase.name}, which is not within what the head occurrence '
              'hands out (${headBase.name})',
    )
  ];
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

/// Error: a call to a parameterised procedure has no instantiation.
///
/// TGLP `parameterized-types.tex`, Definition "Instantiation": a map of the
/// callee's parameters to types of the program is an instantiation of the call
/// if the calling clause and the callee's clauses are well-typed when the
/// callee's declaration is replaced by its expansion under the map.  The error
/// names the call and the callee, the declaration no map instantiates, and
/// [reason], the argument no expansion of it admits ([_noInstantiationReason]).
class NoInstantiationError extends WellTypedError {
  final ast.Goal call;
  final ProcDecl callee;
  final String reason;

  NoInstantiationError(this.call, this.callee, this.reason);

  @override
  String get message {
    final params = callee.typeParams.join(', ');
    final s = callee.typeParams.length == 1 ? '' : 's';
    return 'No instantiation of ${callee.key} for the call $call: $reason, so '
        'no map of its parameter$s $params to types of the program makes the '
        'call well-typed by procedure($params) '
        '${callee.name}(${callee.argTypes.join(', ')}) (TGLP '
        'parameterized-types.tex, Definition "Instantiation")';
  }

  @override
  String toString() => message;
}

/// The name of the open type standing for a parameter of [template] while
/// [_noInstantiationReason] asks whether any map of the parameters can make a
/// call well-typed: distinct from the probes of [_solveFromCalleeClauses],
/// whose environment a callee's clause may be checked in.
const String _openParamPrefix = r'$open_';

/// Why no instantiation of [template]'s parameters can make [call] well-typed,
/// or null where one may.
///
/// TGLP parameterized-types.tex, Definition "Instantiation": a map $\theta$ is
/// an instantiation of the call if the calling clause is well-typed (among
/// other conditions) when the callee's declaration is replaced by its
/// expansion under $\theta$; and well-typing.tex, Definition "Well-Typed
/// Clause with Subtyping", condition 3, relates each argument variable to its
/// other occurrence in the clause.  [partnersOf] gives, for a variable and the
/// polarity it has in the call, those occurrences: a head occurrence (true),
/// whose pair is 3(b) --- the head's type within the call's where the head
/// occurrence is consumed, the call's within the head's where it is produced
/// --- and the body occurrences of the other polarity (false), whose pair is
/// 3(a), the writer's type within the reader's.
///
/// Each such relation is asked with every parameter of the declaration left
/// open ([isSubtype]'s `open`): a comparison that reaches a parameter holds,
/// since a map could bind it to whatever stands there.  So a relation that
/// fails fails under every map, and the call has no instantiation: a
/// `Stream(T)` handed where `OpenStream(X)?` is read carries `[]`, which no
/// expansion of `OpenStream(X)` has.  A relation that holds leaves the call to
/// the checks it has always had, its modes; that one map makes every relation
/// hold at once is not asked here, so this refuses no call that has an
/// instantiation and may pass one that has none.
///
/// [bound] binds some parameters to types --- those a call's sites supply or
/// its callee's clauses fix ([_instantiateCalls]) --- and only the rest are
/// left open: "A parameter for which no type is supplied or fixed is left open
/// where the callee is parametrically well-typed, the call checked with it
/// open" (TGLP appendix-implementation-notes.tex, "The instantiation of a
/// call", cc4a891).  Empty, every parameter is open.
///
/// A bare open parameter of the declaration, `X` or `X?`, is skipped (any
/// type, at either polarity, may stand there), as is a declaration reaching a
/// template that has a parameter as an alternative: an open parameter there
/// would hide alternatives a map adds, so the open relation would no longer be
/// weaker than every expansion's.
String? _noInstantiationReason(
  ast.Goal call,
  ProcDecl template,
  List<(VariableTypeInfo, bool)> Function(String name, bool reader) partnersOf,
  TypeEnvironment env, {
  Map<String, String> bound = const {},
}) {
  if (paramUsedAsTypeAlternative(template, env.typeTemplates)) return null;
  final open = [
    for (final tp in template.typeParams)
      if (!bound.containsKey(tp)) tp
  ];

  // The argument positions there is anything to compare at.
  final positions = <(int, List<(VariableTypeInfo, bool)>)>[];
  for (var i = 0; i < template.arity && i < call.args.length; i++) {
    final arg = call.args[i];
    if (arg is! ast.VarTerm) continue;
    if (_isBareParameter(template.argTypes[i], open)) continue;
    final partners = partnersOf(arg.name, arg.isReader);
    if (partners.isNotEmpty) positions.add((i, partners));
  }
  if (positions.isEmpty) return null;

  // The declaration with each bound parameter at its type and each other an
  // open type, and the types it names.
  final openOf = {for (final tp in open) tp: '$_openParamPrefix$tp'};
  final openNames = openOf.values.toSet();
  final openDecl = ProcDecl(
    template.name,
    [
      for (final t in template.argTypes)
        _substituteTypeParams(t, {...bound, ...openOf})
    ],
    template.line,
    template.column,
    exported: template.exported,
    imported: template.imported,
    modulePath: template.modulePath,
  );
  final openTypes = <String, TypeDef>{
    for (final n in openNames) n: TypeDef(n, const [], 0, 0)
  };
  final needed = <String>{};
  for (final t in openDecl.argTypes) {
    var n = getFullTypeName(t);
    if (n.endsWith('?')) n = n.substring(0, n.length - 1);
    if (n.contains('<') && !env.types.containsKey(n)) needed.add(n);
  }
  if (needed.isNotEmpty) {
    openTypes.addAll(materializeInstantiations(
        needed, env.typeTemplates, {...env.types.keys, ...openNames}));
  }
  final ProgramDFA openDfa;
  try {
    openDfa = buildProgramDFA(TypeEnvironment(
      {...env.types, ...openTypes},
      {...env.procedures, openDecl.key: openDecl},
      paramProcDecls: env.paramProcDecls,
      typeTemplates: env.typeTemplates,
      typeOrigins: env.typeOrigins,
    ));
  } on UnknownTypeError {
    return null; // a type the declaration names is not in scope
  }

  for (final (i, partners) in positions) {
    final arg = call.args[i] as ast.VarTerm;
    final declared = openDecl.argTypes[i];
    // The call's occurrence has the mode of its position: a reader where the
    // declaration takes an input type, a writer where it takes an output one.
    // Any other is the mode check's to report.
    final declaredName = getFullTypeName(declared);
    final declaredInput = declaredName.endsWith('?');
    if (arg.isReader != declaredInput) continue;
    final atCall = openDfa.states[declaredInput
        ? declaredName.substring(0, declaredName.length - 1)
        : declaredName];
    if (atCall == null) continue;
    for (final (partner, inHead) in partners) {
      final other = openDfa.states[partner.typeState.baseName];
      if (other == null) continue;
      // 3(b): a consumed head occurrence within the call's, the call's within a
      // produced one.  3(a): the writer's within the reader's.
      final otherWithin = inHead ? partner.typeState.isDual : arg.isReader;
      final (sub, sup) = otherWithin ? (other, atCall) : (atCall, other);
      if (isSubtype(sub, sup, openDfa, open: openNames)) continue;
      final shown = template.argTypes[i];
      return otherWithin
          ? 'argument ${i + 1}, $arg, holds ${other.name}, which no expansion '
              'of $shown accepts'
          : 'argument ${i + 1}, $arg, is to hold ${other.name}, and no '
              'expansion of $shown is within it';
    }
  }
  return null;
}


// =============================================================================
// The instantiation of a call, read over the whole clause
// =============================================================================
//
// TGLP appendix-implementation-notes.tex, "The instantiation of a call"
// (cc4a891):
//
//   "Definition~\ref{def:instantiation} asks that an instantiation exist and
//    orders nothing; the checker reads the sites of a call over the whole
//    clause, the body goals in no order, and a posted goal, checked as a body
//    (Section~\ref{sec:runtime-boundary}), the same way.  For each parameter
//    it tries the types the sites supply and the types the callee's clauses
//    fix for it---a head occurrence of the parameter paired by condition~3
//    with a body occurrence of a concrete type---and takes one under which the
//    clause and the callee's clauses are well-typed with subtyping; where
//    types are supplied or fixed and none serves, the bindings conflict and
//    the call is refused.  A parameter for which no type is supplied or fixed
//    is left open where the callee is parametrically well-typed
//    (Section~\ref{sec:abstract-parameters}), the call checked with it open
//    and every argument typed at its position; otherwise the call is
//    refused."
//
// A SITE of a call is an occurrence of a variable in it at a position a
// parameter of the callee reaches.  The type it SUPPLIES for a parameter is the
// type its partner in the clause --- the other occurrence condition 3 of
// Definition "Well-Typed Clause with Subtyping" pairs it with --- has at the
// parameter's position: the head's occurrence of the same key under 3(b) (the
// type a guard narrowed it to, where one did), or a body occurrence of the
// other polarity under 3(a), whose dual stands at the call's polarity.  A
// variable inside a term that stands at a parameter itself is a site whose
// type the binding decides, and supplies nothing: it names no type.  A site is
// WELL-TYPED WITH SUBTYPING under a binding when its pair satisfies that
// condition 3.

/// The checker's reading of one call to a parameterised procedure in a clause
/// ([_instantiateCalls]).
class _CallPlan {
  /// The call itself, the inner goal of a spawn or a remote goal, and the
  /// callee's template, for the diagnostics.
  final ast.Goal goal;
  final ProcDecl template;

  /// The declaration the call is checked by: the expansion under the
  /// instantiation found, or --- where types are supplied or fixed for every
  /// parameter and none of the bindings they give serves --- the expansion
  /// under the binding leaving the fewest sites ill-typed, at which the clause
  /// check names the sites it does not fit.  Null where some parameter has no
  /// type supplied or fixed for it.
  final ProcDecl? decl;

  /// Whether [decl] binds a parameter to an input type
  /// ([CollectedInstantiation.bindsInputType]).
  final bool bindsInputType;

  /// Where types were supplied or fixed for every parameter and none of the
  /// bindings serves: why the nearest, [decl], does not.  The bindings
  /// conflict and the call is refused ([ConflictingBindingsError]).  Null
  /// otherwise.
  final String? conflict;

  /// The types tried for each parameter: those the sites of the call supply,
  /// then those the callee's clauses fix for it ([_addCalleeFixed]).
  final Map<String, List<String>> tried;

  /// Whether the callee's clauses were read: they are where they are the
  /// unit's own.
  final bool calleeRead;

  /// The parameters for which no type is supplied by a site of the call or
  /// fixed by the callee's clauses, where [decl] is null; empty where types
  /// were tried and none gives a declaration this scope can build.
  final List<String> unsupplied;

  /// Where no binding tried serves and no map of the parameters can make the
  /// call well-typed either: the argument no expansion admits
  /// ([_noInstantiationReason]), by which the call is refused; [decl] is then
  /// null.
  final String? refutation;

  /// Where some parameter has no type supplied or fixed for it and the callee
  /// is not parametrically well-typed: the call is refused
  /// ([UninstantiatedCallError]); [decl] is then null.
  final bool refused;

  /// Where some parameter has no type supplied or fixed for it and the callee
  /// is parametrically well-typed: the binding of the other parameters under
  /// which the call, checked with those [unsupplied] open, is well-typed
  /// ([_noInstantiationReason]); [decl] is then null.
  final Map<String, String>? openUnder;

  const _CallPlan(this.goal, this.template, this.decl, this.bindsInputType,
      {this.conflict,
      this.tried = const {},
      this.calleeRead = false,
      this.unsupplied = const [],
      this.refutation,
      this.refused = false,
      this.openUnder});
}

/// Error: a call to a parameterised procedure for which no instantiation is
/// found, and whose callee is not parametrically well-typed.
///
/// TGLP appendix-implementation-notes.tex, "The instantiation of a call" (TGLP
/// cc4a891): "For each parameter it tries the types the sites supply and the
/// types the callee's clauses fix for it ... A parameter for which no type is
/// supplied or fixed is left open where the callee is parametrically
/// well-typed ...; otherwise the call is refused."
class UninstantiatedCallError extends WellTypedError {
  final ast.Goal call;
  final ProcDecl callee;
  final List<String> unsupplied;

  UninstantiatedCallError(this.call, this.callee, this.unsupplied);

  @override
  String get message {
    final what = unsupplied.isEmpty
        ? 'no type tried gives a declaration this scope can build'
        : 'no site of the call supplies a type for ${unsupplied.join(', ')} '
            'and the clauses of ${callee.key} fix none';
    return 'No instantiation of ${callee.key} is found for the call $call: '
        '$what; ${callee.key} is not parametrically well-typed, so the call '
        'is refused (TGLP appendix-implementation-notes.tex, "The '
        'instantiation of a call"; parameterized-types.tex, Definition '
        '"Instantiation")';
  }

  @override
  String toString() => message;
}

/// Error: types were supplied or fixed for every parameter of a call and none
/// of the bindings they give serves.
///
/// TGLP appendix-implementation-notes.tex, "The instantiation of a call"
/// (cc4a891): the checker "takes one under which the clause and the callee's
/// clauses are well-typed with subtyping; where types are supplied or fixed
/// and none serves, the bindings conflict and the call is refused."
class ConflictingBindingsError extends WellTypedError {
  final ast.Goal call;
  final ProcDecl callee;
  final Map<String, List<String>> tried;
  final String reason;

  /// Whether the callee's clauses were read for the types they fix: they are
  /// where they are the unit's own.
  final bool calleeRead;

  ConflictingBindingsError(this.call, this.callee, this.tried, this.reason,
      {this.calleeRead = true});

  @override
  String get message {
    final shown = [
      for (final e in tried.entries) '${e.key}: ${e.value.join(', ')}'
    ].join('; ');
    final whence = calleeRead
        ? 'the types the sites supply and the clauses of ${callee.key} fix'
        : 'the types the sites supply';
    return 'The bindings tried for the call $call conflict: $whence ($shown) '
        'give no binding under which the clause and the callee\'s clauses are '
        'well-typed with subtyping --- $reason --- so the call is refused '
        '(TGLP appendix-implementation-notes.tex, "The instantiation of a '
        'call"; parameterized-types.tex, Definition "Instantiation")';
  }

  @override
  String toString() => message;
}

/// A call to a parameterised procedure: the goal itself --- the inner goal of
/// a spawn or a remote goal --- the callee's template, and whether the call is
/// a remote goal, whose callee's clauses are another module's.
class _ParamCall {
  final ast.Goal goal;
  final ProcDecl template;
  final bool remote;
  _ParamCall(this.goal, this.template, this.remote);
}

/// [atom] as a call to a parameterised procedure the clause is to instantiate,
/// or null: not a call to one, or a recursive call, which is checked at the
/// enclosing instantiation ([activeInstantiations]) and induces none.
_ParamCall? _parametricCall(ast.Goal atom, TypeEnvironment env,
    Map<String, ProcDecl> activeInstantiations) {
  var goal = atom;
  while (goal is ast.SpawnGoal) {
    goal = goal.innerGoal;
  }
  if (goal is ast.RemoteGoal) {
    final parts = <String>[];
    ast.Goal inner = goal;
    while (inner is ast.RemoteGoal) {
      parts.add(inner.staticModuleName);
      inner = inner.goal;
    }
    final template = env.paramProcDecls[
        '${parts.join('#')}#${inner.functor}/${inner.arity}'];
    if (template == null || template.arity != inner.arity) return null;
    return _ParamCall(inner, template, true);
  }
  if (isBuiltinGoal(goal.functor)) return null;
  final decl = env.getProcedure(goal.functor, goal.arity);
  if (decl == null) return null;
  final template = env.paramProcDecls[decl.key];
  if (template == null ||
      template.arity != goal.arity ||
      activeInstantiations.containsKey(decl.key)) {
    return null;
  }
  return _ParamCall(goal, template, false);
}

/// A parameterised declaration with each parameter an abstract type standing
/// for it ([_paramProbePrefix]), in a DFA holding those types and the template
/// instantiations they name: the automaton a call's sites are read in.
class _Probe {
  final ProcDecl decl;
  final ProgramDFA dfa;

  /// The abstract type standing for each parameter, to the parameter.
  final Map<String, String> paramOf;

  _Probe(this.decl, this.dfa, this.paramOf);
}

/// The probes built in an environment, by template: a probe names only the
/// template's own types, which an environment's growth does not change.
final Expando<Map<String, _Probe?>> _probes = Expando('callProbes');

_Probe? _probeOf(ProcDecl template, TypeEnvironment env) {
  final cache = _probes[env] ??= <String, _Probe?>{};
  final key = '${template.modulePath}|${template.name}/${template.arity}|'
      '${template.typeParams.join(',')}|${template.argTypes.join(',')}';
  if (cache.containsKey(key)) return cache[key];
  final probeOf = {
    for (final tp in template.typeParams) tp: '$_paramProbePrefix$tp'
  };
  final paramOf = {for (final e in probeOf.entries) e.value: e.key};
  final decl = ProcDecl(
    template.name,
    [for (final t in template.argTypes) _substituteTypeParams(t, probeOf)],
    template.line,
    template.column,
    exported: template.exported,
    imported: template.imported,
    modulePath: template.modulePath,
  );
  final probeTypes = <String, TypeDef>{
    for (final n in paramOf.keys) n: TypeDef(n, const [], 0, 0)
  };
  final needed = <String>{};
  for (final t in decl.argTypes) {
    var n = getFullTypeName(t);
    if (n.endsWith('?')) n = n.substring(0, n.length - 1);
    if (n.contains('<') && !env.types.containsKey(n)) needed.add(n);
  }
  _Probe? probe;
  try {
    if (needed.isNotEmpty) {
      probeTypes.addAll(materializeInstantiations(
          needed, env.typeTemplates, {...env.types.keys, ...paramOf.keys}));
    }
    probe = _Probe(
        decl,
        buildProgramDFA(TypeEnvironment(
          {...env.types, ...probeTypes},
          env.procedures,
          paramProcDecls: env.paramProcDecls,
          typeTemplates: env.typeTemplates,
          typeOrigins: env.typeOrigins,
        )),
        paramOf);
  } on UnknownTypeError {
    probe = null; // a type the declaration names is not in scope
  }
  cache[key] = probe;
  return probe;
}

/// Whether some parameter of [probe] is reached from [state].
bool _reachesParam(DFAState state, _Probe probe) {
  final seen = <String>{};
  bool walk(DFAState s) {
    if (probe.paramOf.containsKey(s.baseName)) return true;
    if (!seen.add(s.name)) return false;
    final automaton = probe.dfa.automata[s.name];
    if (automaton == null) return false;
    for (final e in automaton.transitions.entries) {
      if (e.key.$1 == s && walk(e.value)) return true;
    }
    return false;
  }

  return walk(state);
}

/// The variable keys of [term]: `X` for a writer, `X?` for a reader.
Iterable<String> _termVarKeys(ast.Term term) sync* {
  if (term is ast.VarTerm) {
    yield '${term.name}${term.isReader ? '?' : ''}';
  } else if (term is ast.StructTerm) {
    for (final a in term.args) {
      yield* _termVarKeys(a);
    }
  } else if (term is ast.ListTerm) {
    if (term.head != null) yield* _termVarKeys(term.head!);
    if (term.tail != null) yield* _termVarKeys(term.tail!);
  }
}

/// The sites of [goal], in the order they stand in it: each variable
/// occurrence at a position a parameter reaches, by its key, with the probe
/// state of that position --- null for a variable inside a term that stands at
/// a parameter itself, which supplies nothing.
Map<String, DFAState?> _sitesOf(ast.Goal goal, _Probe probe) {
  final out = <String, DFAState?>{};
  void walk(ast.Term term, DFAState state) {
    if (!_reachesParam(state, probe)) return;
    if (term is ast.VarTerm) {
      out.putIfAbsent('${term.name}${term.isReader ? '?' : ''}', () => state);
      return;
    }
    if (probe.paramOf.containsKey(state.baseName)) {
      for (final k in _termVarKeys(term)) {
        out.putIfAbsent(k, () => null);
      }
      return;
    }
    final automaton = probe.dfa.automata[state.name];
    if (automaton == null) return;
    if (term is ast.StructTerm) {
      for (var i = 0; i < term.args.length; i++) {
        final next =
            _stepTo(automaton, state, term.functor, term.args.length, i + 1);
        if (next != null) walk(term.args[i], next);
      }
    } else if (term is ast.ListTerm && !term.isNil) {
      final h = _stepTo(automaton, state, '[|]', 2, 1);
      final t = _stepTo(automaton, state, '[|]', 2, 2);
      if (h != null && term.head != null) walk(term.head!, h);
      if (t != null && term.tail != null) walk(term.tail!, t);
    }
  }

  for (var i = 0; i < probe.decl.arity && i < goal.args.length; i++) {
    final start = probe.dfa.states[getFullTypeName(probe.decl.argTypes[i])];
    if (start != null) walk(goal.args[i], start);
  }
  return out;
}

/// The types the partner type [actual] supplies for the parameters of [probe]
/// at a site whose probe state is [state]: walking the two automata together,
/// label by label, [actual] in the clause's [dfa], each parameter reached is
/// given the type [actual] has there, as an input type where the two stand at
/// opposite polarities.  The wildcard names no type of the program, and is not
/// supplied: binding a parameter to it is the unsound reading
/// (parameterized-types.tex, sec:programs-and-modules).
void _supplied(DFAState state, DFAState actual, _Probe probe, ProgramDFA dfa,
    void Function(String param, String type) emit,
    [Set<String>? seen]) {
  final visited = seen ?? <String>{};
  if (!visited.add('${state.name}|${actual.name}')) return;
  final param = probe.paramOf[state.baseName];
  if (param != null) {
    if (actual.isWildcard ||
        actual.isAnonymousFinal ||
        probe.paramOf.containsKey(actual.baseName) ||
        actual.baseName.startsWith(_openParamPrefix)) {
      return;
    }
    emit(param,
        state.isDual != actual.isDual ? '${actual.baseName}?' : actual.baseName);
    return;
  }
  if (!_reachesParam(state, probe)) return;
  final declared = probe.dfa.automata[state.name];
  final given = dfa.automata[actual.name];
  if (declared == null || given == null) return;
  for (final e in declared.transitions.entries) {
    final (from, label) = e.key;
    if (from != state) continue;
    final next = given.transition(actual, label);
    if (next != null) _supplied(e.value, next, probe, dfa, emit, visited);
  }
}

/// Every binding of [params] to the types [supply] holds for each, in order:
/// the first parameter's first type first.
Iterable<Map<String, String>> _bindingsOf(
    List<String> params, Map<String, List<String>> supply) sync* {
  if (params.isEmpty) {
    yield const {};
    return;
  }
  final rest = params.sublist(1);
  for (final type in supply[params.first]!) {
    for (final more in _bindingsOf(rest, supply)) {
      yield {params.first: type, ...more};
    }
  }
}

/// How a site's partner stands to it: the head's occurrence of the same key
/// (condition 3(b)), the head's occurrence of the other key, or a body
/// occurrence of the other key (condition 3(a)).
enum _Partner { headSame, headOther, body }

String _otherKey(String key) =>
    key.endsWith('?') ? key.substring(0, key.length - 1) : '$key?';

/// Whether the pair of the call's occurrence [key], typed [own], with
/// [partner] satisfies condition 3 of Definition "Well-Typed Clause with
/// Subtyping", as the clause check asks it ([_checkHeadBodyPair],
/// [_checkClauseDuality], [_bodyBodyPairError]).
bool _pairHolds(String key, VariableTypeInfo own, VariableTypeInfo partner,
    _Partner kind, ProgramDFA dfa) {
  try {
    switch (kind) {
      case _Partner.headSame:
        return _checkHeadBodyPair(key, partner, const {}, own, 'body', dfa)
            .isEmpty;
      case _Partner.headOther:
        final other = _otherKey(key);
        return _checkClauseDuality(
                {key: own, other: partner}, {key: 'body', other: 'head'}, dfa)
            .isEmpty;
      case _Partner.body:
        final base = key.endsWith('?') ? key.substring(0, key.length - 1) : key;
        final error = key.endsWith('?')
            ? _bodyBodyPairError(base, partner, own, 'body', 'body', dfa)
            : _bodyBodyPairError(base, own, partner, 'body', 'body', dfa);
        return error == null;
    }
  } on StateError {
    return false;
  }
}

/// A binding of a call's parameters, with what it gives the call: the
/// binding itself, the declaration it produces, the call's variable types by
/// that declaration, how many of its sites are not well-typed with subtyping
/// and the first of them, whether the call's goal is well-typed (condition 2)
/// and the first error where it is not, and, once asked, whether the callee's
/// clauses are ([_instantiateCalls]).
class _Binding {
  final Map<String, String> binding;
  final ProcDecl decl;
  final TypeEnvironment checkEnv;
  final bool bindsInputType;
  final Map<String, VariableTypeInfo> types;
  final int failingSites;
  final String? firstFailingSite;
  final bool goalHolds;
  final String? goalError;
  bool calleeFails = false;
  _Binding(this.binding, this.decl, this.checkEnv, this.bindsInputType,
      this.types, this.failingSites, this.firstFailingSite, this.goalHolds,
      this.goalError);

  bool get sitesHold => failingSites == 0;

  /// Why this binding does not serve, for [ConflictingBindingsError].
  String get reason {
    final under = [
      for (final e in binding.entries) '${e.key} = ${e.value}'
    ].join(', ');
    if (failingSites > 0) {
      return 'under $under the occurrence $firstFailingSite is not '
          'well-typed with subtyping with its pair in the clause';
    }
    if (!goalHolds) return 'under $under the call is not well-typed: $goalError';
    return 'under $under the clauses of ${decl.key} are not well-typed by '
        '${decl.name}(${decl.argTypes.map(getFullTypeName).join(', ')}) or do '
        'not accept its every input path';
  }
}

/// What one round of [_instantiateCalls] reads for a call: the binding taken
/// (the clause and the callee's clauses well-typed under it), the binding
/// under which the fewest sites are not --- the first such in the order the
/// types are tried ---, whether every site's partner is typed, the
/// parameters for which no type is supplied or fixed, and the types tried.
class _Reading {
  final _Binding? taken;
  final _Binding? nearest;
  final bool complete;
  final List<String> unsupplied;
  final Map<String, List<String>> tried;
  _Reading(this.taken, this.nearest, this.complete, this.unsupplied,
      this.tried);
}

/// The instantiation of each call to a parameterised procedure in [clause],
/// read over the whole clause, by body-atom index (TGLP
/// appendix-implementation-notes.tex, "The instantiation of a call", cc4a891).
///
/// The occurrences the rest of the clause types are read first, as the clause
/// check types them: the moded head, a guard's narrowing of a head occurrence,
/// and every body goal that is not such a call.  Then in rounds, each call
/// read against the same occurrences, so that no goal's place in the body
/// decides anything.  For each parameter the types tried are those the sites
/// of the call supply and those the callee's clauses fix for it
/// ([_addCalleeFixed]), and the binding taken is the first, in the order the
/// types are tried, under which the clause and the callee's clauses are
/// well-typed with subtyping: every site well-typed with its pair, the call's
/// goal well-typed (condition 2), and --- where the callee's clauses are this
/// unit's --- the callee's clauses well-typed by the declaration the binding
/// produces and accepting its every input path ([CalleeClauses.verify],
/// Definition "Instantiation").  A call whose every site's partner is typed
/// and which a binding serves takes it first; failing any, a call some of
/// whose sites' partners are still untyped --- they stand in another such call
/// --- takes one serving its typed sites.  The variable types the binding
/// gives the call are then typed occurrences for the rounds after.
///
/// Where types are supplied or fixed for every parameter and none of the
/// bindings serves, the bindings conflict and the call is refused: by the
/// argument no expansion admits, where no map of the parameters can make the
/// call well-typed ([_noInstantiationReason]); else at the binding leaving the
/// fewest sites ill-typed (the first such, in the order the types are tried),
/// which the clause check checks the call by, naming the sites it does not
/// fit, and [ConflictingBindingsError] names why it does not serve.  A call
/// some parameter of which has no type supplied or fixed for it gets no
/// declaration, and [checkClause] refuses it unless its callee is
/// parametrically well-typed.
Map<int, _CallPlan> _instantiateCalls(
  TypedClause clause,
  ProcDecl procDecl,
  ProgramDFA dfa,
  TypeEnvironment env, {
  Map<String, ProcDecl> activeInstantiations = const {},
  CalleeClauses? callee,
  bool Function(String procKey)? isParametric,
}) {
  final calls = <int, _ParamCall>{};
  for (var i = 0; i < clause.bodyAtoms.length; i++) {
    final call =
        _parametricCall(clause.bodyAtoms[i], env, activeInstantiations);
    if (call != null) calls[i] = call;
  }
  if (calls.isEmpty) return const {};

  // The occurrences the rest of the clause types.  A body occurrence of a key
  // the head carries is the head's pair (3(b)), and a guard's narrows the
  // head's (TGLP typed-glp.tex, "Type checking of guards"), as in
  // [checkClause]; every other body occurrence is kept with its atom.
  final (headResult, _) = _checkHeadWithTerm(clause, procDecl, dfa, env);
  final head = headResult.variableTypes;
  final narrowed = <String, VariableTypeInfo>{};
  final body = <String, List<(VariableTypeInfo, int)>>{};
  void take(int i, Map<String, VariableTypeInfo> types) {
    for (final e in types.entries) {
      if (!head.containsKey(e.key)) {
        body.putIfAbsent(e.key, () => []).add((e.value, i));
      } else if (i < clause.guardAtoms.length) {
        final have = narrowed[e.key] ?? head[e.key]!;
        final met = meetOfTypes(have.typeState, e.value.typeState, dfa, env);
        if (met != null) {
          narrowed[e.key] = VariableTypeInfo(
              typeState: met, mode: have.mode, isReader: have.isReader);
        }
      }
    }
  }

  for (var i = 0; i < clause.bodyAtoms.length; i++) {
    if (calls.containsKey(i)) continue;
    final (result, _) = _checkBodyAtomWithTerm(clause.bodyAtoms[i], i, dfa, env,
        activeInstantiations: activeInstantiations);
    take(i, result.variableTypes);
  }

  final probes = <int, _Probe?>{
    for (final e in calls.entries) e.key: _probeOf(e.value.template, env)
  };
  final sites = <int, Map<String, DFAState?>>{
    for (final e in calls.entries)
      e.key: probes[e.key] == null
          ? const <String, DFAState?>{}
          : _sitesOf(e.value.goal, probes[e.key]!)
  };
  final keys = <int, Set<String>>{
    for (final e in calls.entries)
      e.key: {for (final a in e.value.goal.args) ..._termVarKeys(a)}
  };

  // The partners of the call at [i]'s occurrence [key], typed.
  List<(VariableTypeInfo, _Partner)> partnersOf(int i, String key) {
    final own = head[key];
    if (own != null) return [(narrowed[key] ?? own, _Partner.headSame)];
    final other = _otherKey(key);
    return [
      if (head[other] != null) (head[other]!, _Partner.headOther),
      for (final (info, j) in body[other] ?? const <(VariableTypeInfo, int)>[])
        if (j != i) (info, _Partner.body),
    ];
  }

  // The callee's defining clauses, where they are this unit's: a remote
  // goal's are another module's, and the linked program, where the call is
  // local, asks them.
  List<ast.Clause>? definingOf(_ParamCall call) {
    if (call.remote || callee == null) return null;
    final defining = callee.of(call.template.key);
    return (defining == null || defining.isEmpty) ? null : defining;
  }

  _Binding? bind(_ParamCall call, int i, Map<String, String> binding) {
    final decl = _concreteDecl(call.template, binding, const {});
    bool built() => decl.argTypes.every((t) {
          var n = getFullTypeName(t);
          if (n.endsWith('?')) n = n.substring(0, n.length - 1);
          return dfa.automata.containsKey(n);
        });
    final checkEnv = _buildDeclTypes(decl, dfa, env);
    if (!built()) return null;
    final WellTypedResult result;
    try {
      result = _checkModedTermPerArg(
          producedTerm(call.goal, decl, typeEnv: checkEnv), decl, dfa);
    } on ArityMismatchError {
      return null;
    } on UnknownTypeError {
      return null;
    } on StateError {
      return null;
    }
    var failing = 0;
    String? firstFailing;
    for (final key in sites[i]!.keys) {
      final own = result.variableTypes[key];
      if (own == null) continue; // untyped by this binding: the goal's to say
      for (final (partner, kind) in partnersOf(i, key)) {
        if (!_pairHolds(key, own, partner, kind, dfa)) {
          failing++;
          firstFailing ??= key;
          break;
        }
      }
    }
    return _Binding(
        binding,
        decl,
        checkEnv,
        binding.values.any((t) => t.endsWith('?')),
        result.variableTypes,
        failing,
        firstFailing,
        result.isWellTyped,
        result.errors.isEmpty ? null : result.errors.first.message);
  }

  // Whether the callee's clauses [defining] are well-typed by the declaration
  // [b] produces and accept its every input path (Definition "Instantiation"),
  // asked once per declaration in this environment, in the environment the
  // call was checked in, which holds the types the declaration names.
  bool calleeHolds(_Binding b, List<ast.Clause> defining) {
    final byClauses = _verifiedCache[env] ??=
        Map<Object, Map<String, bool>>.identity();
    final verdicts = byClauses.putIfAbsent(defining, () => {});
    final key = '${b.decl.key}|${b.decl.argTypes.map(getFullTypeName).join(',')}';
    final at = b.checkEnv;
    return verdicts[key] ??= callee!.verify(
        b.decl,
        TypeEnvironment(
          {...at.types},
          {...at.procedures, b.decl.key: b.decl},
          paramProcDecls: at.paramProcDecls,
          typeTemplates: at.typeTemplates,
          typeOrigins: at.typeOrigins,
        ),
        defining);
  }

  _Reading read(int i, Set<int> pending) {
    final call = calls[i]!;
    final probe = probes[i];
    final params = call.template.typeParams;
    // A site's partner in another call still to be read is not typed yet.
    var complete = true;
    for (final key in sites[i]!.keys) {
      if (head.containsKey(key)) continue;
      final other = _otherKey(key);
      if (pending.any((j) => j != i && keys[j]!.contains(other))) {
        complete = false;
        break;
      }
    }
    // The types the sites supply, per parameter, in the order the sites
    // stand in the call: a site's head partner first, then its body partners,
    // whose supplies are ordered by name, so that no goal's place decides.
    final tried = {for (final tp in params) tp: <String>[]};
    void add(String param, String type) {
      final have = tried[param];
      if (have == null) return;
      if (have.any((t) => t == type || _sameBinding(t, type, dfa))) return;
      have.add(type);
    }

    if (probe != null) {
      for (final site in sites[i]!.entries) {
        final state = site.value;
        if (state == null) continue;
        final fromBody = <(String, String)>[];
        for (final (partner, kind) in partnersOf(i, site.key)) {
          final actual = kind == _Partner.headSame
              ? partner.typeState
              : partner.typeState.dual;
          if (kind == _Partner.body) {
            _supplied(state, actual, probe, dfa, (p, t) => fromBody.add((p, t)));
          } else {
            _supplied(state, actual, probe, dfa, add);
          }
        }
        fromBody.sort((a, b) => '${a.$1} ${a.$2}'.compareTo('${b.$1} ${b.$2}'));
        for (final (p, t) in fromBody) {
          add(p, t);
        }
      }
    }
    // Then the types the callee's clauses fix for each parameter.
    final defining = definingOf(call);
    if (defining != null) {
      _addCalleeFixed(call.template, tried, defining, dfa, env);
    }
    final unsupplied = [
      for (final tp in params)
        if (tried[tp]!.isEmpty) tp
    ];
    if (unsupplied.isNotEmpty) {
      return _Reading(null, null, complete, unsupplied, tried);
    }

    _Binding? nearest;
    _Binding? taken;
    for (final binding in _bindingsOf(params, tried)) {
      final b = bind(call, i, binding);
      if (b == null) continue;
      if (nearest == null || b.failingSites < nearest.failingSites) {
        nearest = b;
      }
      if (!b.sitesHold || !b.goalHolds) continue;
      if (defining != null && !calleeHolds(b, defining)) {
        b.calleeFails = true;
        continue;
      }
      taken = b;
      break;
    }
    return _Reading(taken, nearest, complete, const [], tried);
  }

  // The partners of the call at [i]'s argument variable [name], of the
  // polarity [reader], as [_noInstantiationReason] asks them: the head's
  // occurrence of the same key (true), and the body occurrences of the other
  // polarity (false).
  List<(VariableTypeInfo, bool)> Function(String, bool) openPartnersOf(
          int i) =>
      (name, reader) {
        final own = reader ? '$name?' : name;
        final other = reader ? name : '$name?';
        final h = head[own];
        return [
          if (h != null) (narrowed[own] ?? h, true),
          for (final (info, j)
              in body[other] ?? const <(VariableTypeInfo, int)>[])
            if (j != i) (info, false),
        ];
      };

  final plans = <int, _CallPlan>{};
  final pending = calls.keys.toSet();
  var last = <int, _Reading>{};
  while (pending.isNotEmpty) {
    final readings = {for (final i in pending) i: read(i, pending)};
    last = readings;
    List<int> where(bool Function(_Reading r) test) => [
          for (final e in readings.entries)
            if (test(e.value)) e.key
        ];
    var decided = where((r) => r.taken != null && r.complete);
    if (decided.isEmpty) decided = where((r) => r.taken != null);
    if (decided.isEmpty) {
      decided = where((r) => r.nearest != null && r.complete);
    }
    if (decided.isEmpty) decided = where((r) => r.nearest != null);
    if (decided.isEmpty) break;
    for (final i in decided) {
      final r = readings[i]!;
      final call = calls[i]!;
      if (r.taken == null && !call.remote) {
        // No binding tried serves.  Where no map of the parameters can make
        // the call well-typed either, the call is refused by the argument no
        // expansion admits, as a call with no instantiation is ([checkClause],
        // Step 5).
        final refutation = _noInstantiationReason(
            call.goal, call.template, openPartnersOf(i), env);
        if (refutation != null) {
          plans[i] = _CallPlan(call.goal, call.template, null, false,
              refutation: refutation, tried: r.tried);
          pending.remove(i);
          continue;
        }
      }
      final b = r.taken ?? r.nearest!;
      plans[i] = _CallPlan(call.goal, call.template, b.decl, b.bindsInputType,
          conflict: r.taken == null ? b.reason : null,
          tried: r.tried,
          calleeRead: definingOf(call) != null);
      take(i, b.types);
      pending.remove(i);
    }
  }
  // The calls some parameter of which has no type supplied or fixed for it,
  // read against every occurrence the rounds typed: "A parameter for which no
  // type is supplied or fixed is left open where the callee is parametrically
  // well-typed (Section sec:abstract-parameters), the call checked with it
  // open ...; otherwise the call is refused" (cc4a891).  The other parameters
  // take the types tried for them, the first binding under which the call,
  // checked with the rest open, is well-typed; none serving, the bindings
  // conflict and the call is refused.  A remote goal's callee is another
  // module's: the linked program, where the call is local, decides it.
  for (final i in pending) {
    final call = calls[i]!;
    final params = call.template.typeParams;
    final tried = last[i]?.tried ?? {for (final tp in params) tp: <String>[]};
    final open = [
      for (final tp in params)
        if (tried[tp]?.isEmpty ?? true) tp
    ];
    if (isParametric != null && !isParametric(call.template.key)) {
      plans[i] = _CallPlan(call.goal, call.template, null, false,
          unsupplied: open, tried: tried, refused: true);
      continue;
    }
    if (call.remote) {
      plans[i] = _CallPlan(call.goal, call.template, null, false,
          unsupplied: open, tried: tried, openUnder: const {});
      continue;
    }
    final boundParams = [
      for (final tp in params)
        if (!open.contains(tp)) tp
    ];
    Map<String, String>? openUnder;
    String? firstReason;
    for (final binding in _bindingsOf(boundParams, tried)) {
      final reason = _noInstantiationReason(
          call.goal, call.template, openPartnersOf(i), env,
          bound: binding);
      if (reason == null) {
        openUnder = binding;
        break;
      }
      final under = [
        for (final e in binding.entries) '${e.key} = ${e.value}'
      ].join(', ');
      firstReason ??= binding.isEmpty
          ? reason
          : 'under $under with ${open.join(', ')} open, $reason';
    }
    if (openUnder != null) {
      plans[i] = _CallPlan(call.goal, call.template, null, false,
          unsupplied: open, tried: tried, openUnder: openUnder);
    } else if (boundParams.isEmpty) {
      plans[i] = _CallPlan(call.goal, call.template, null, false,
          unsupplied: open, tried: tried, refutation: firstReason);
    } else {
      plans[i] = _CallPlan(call.goal, call.template, null, false,
          unsupplied: open,
          tried: tried,
          conflict: firstReason,
          calleeRead: definingOf(call) != null);
    }
  }
  return plans;
}

/// Add to [tried], after the types the sites of a call supply, the types the
/// callee's clauses [clauses] fix for each parameter of [template].
///
/// TGLP appendix-implementation-notes.tex, "The instantiation of a call"
/// (cc4a891): "For each parameter it tries the types the sites supply and the
/// types the callee's clauses fix for it---a head occurrence of the parameter
/// paired by condition 3 with a body occurrence of a concrete type---".  A
/// type a clause fixes for one parameter may rest on the binding of another
/// --- `send_user(M?, Stream(Ent)?, Stream(Ent))`'s first clause fixes `M`
/// only once `Ent` is bound, the type the message stands at being inside
/// `Ent`'s alternatives --- so the types are read under each binding of the
/// other parameters to the types tried for them so far, a parameter with none
/// standing unbound, until no reading adds one ([_calleeFixedTypes]).
void _addCalleeFixed(ProcDecl template, Map<String, List<String>> tried,
    List<ast.Clause> clauses, ProgramDFA dfa, TypeEnvironment env) {
  final params = template.typeParams;
  final byClauses = _fixedCache[env] ??=
      Map<Object, Map<String, Map<String, List<String>>>>.identity();
  final cache = byClauses.putIfAbsent(clauses, () => {});
  Map<String, List<String>> fixedUnder(Map<String, String> bound) {
    final key = '${template.key}|${template.argTypes.join(',')}|'
        '${[for (final p in params) bound[p] ?? '-'].join(',')}';
    return cache[key] ??= _calleeFixedTypes(template, bound, clauses, env);
  }

  // The types are finitely many --- each is a type the callee's clauses
  // carry --- so the readings end; the bound guards against a binding that
  // keeps renaming one.
  var changed = true;
  for (var round = 0; changed && round < 8; round++) {
    changed = false;
    for (final p in params) {
      final others = [
        for (final q in params)
          if (q != p) q
      ];
      for (final bound in _partialBindingsOf(others, tried)) {
        for (final t in fixedUnder(bound)[p] ?? const <String>[]) {
          final have = tried[p]!;
          if (have.any((x) => x == t || _sameBinding(x, t, dfa))) continue;
          have.add(t);
          changed = true;
        }
      }
    }
  }
}

/// Whether a callee's clauses are well-typed by a declaration and accept its
/// every input path ([CalleeClauses.verify]), by the environment, the clauses
/// and the declaration.
final Expando<Map<Object, Map<String, bool>>> _verifiedCache =
    Expando('calleeVerified');

/// The types [_calleeFixedTypes] read, by the environment, the callee's
/// clauses and the binding they were read under.
final Expando<Map<Object, Map<String, Map<String, List<String>>>>>
    _fixedCache = Expando('calleeFixed');

/// Every binding of [params] to the types [tried] holds for each, in order, a
/// parameter with none left unbound.
Iterable<Map<String, String>> _partialBindingsOf(
    List<String> params, Map<String, List<String>> tried) sync* {
  if (params.isEmpty) {
    yield const {};
    return;
  }
  final rest = params.sublist(1);
  final types = tried[params.first] ?? const <String>[];
  if (types.isEmpty) {
    yield* _partialBindingsOf(rest, tried);
    return;
  }
  for (final type in types) {
    for (final more in _partialBindingsOf(rest, tried)) {
      yield {params.first: type, ...more};
    }
  }
}

/// The types the clauses of [template]'s procedure fix for each parameter
/// [bound] does not bind, the parameters it does bind standing at their
/// types.
///
/// "A head occurrence of the parameter paired by condition 3 with a body
/// occurrence of a concrete type" (TGLP appendix-implementation-notes.tex,
/// cc4a891): each unbound parameter stands as an abstract type
/// (def:abstract-type), the clause's head is typed by the declaration so
/// formed (Definition "Moded Head") and each body goal by the declaration in
/// scope (condition 2, [_bodyAtomVariableTypes]; a recursive call by the same
/// declaration, recursion being monomorphic), and an occurrence in the head
/// whose type reaches a parameter is read against its pair in the clause
/// where that pair's type is concrete there: the type the pair has at the
/// parameter's position is the type fixed for it, walking the two automata
/// together as a site's supply is read ([_supplied]).  A body pair (condition
/// 3(b)) is to have the head occurrence's type, a head pair (condition 3(a))
/// its dual.  The head pair is read too: `send_user(Msg, [user_output([Msg?|
/// Out1?])|Rest], ...)` relates `Msg`, at `M?`, to `Msg?`, at the element of
/// the stream `user_output` carries, in its head and not its body, and the
/// rule's sentence names what Currencies' and GSG's calls with a constructed
/// message are to be loaded by (GLP #3 Cowork, 2026-10-02 15:46 UTC, item 1).
/// Guards narrow and are not pairs; a body pair of two body occurrences is
/// not a head occurrence of the parameter.
///
/// Each unbound parameter is read at its output type and again at its input
/// type: an input type is a type (typed-glp.tex, "Type Declarations"), and an
/// occurrence at a position whose mode is not its own is given no type
/// (def:consistent-paths rows 2 and 3), so a clause that reads a parameter at
/// its input type fixes it only so.
Map<String, List<String>> _calleeFixedTypes(ProcDecl template,
    Map<String, String> bound, List<ast.Clause> clauses, TypeEnvironment env) {
  final unbound = [
    for (final p in template.typeParams)
      if (!bound.containsKey(p)) p
  ];
  final out = {for (final p in unbound) p: <String>[]};
  if (unbound.isEmpty) return out;
  final probeOf = {for (final p in unbound) p: '$_paramProbePrefix$p'};
  final paramOf = {for (final e in probeOf.entries) e.value: e.key};

  for (final flip in const [false, true]) {
    final built = _declInAbstractTypes(template, {...bound, ...probeOf},
        {for (final p in unbound) p: flip}, paramOf.keys.toSet(), env);
    if (built == null) continue;
    final (decl, probeEnv, probeDfa) = built;
    final probe = _Probe(decl, probeDfa, paramOf);
    void emit(String param, String type) {
      // [_supplied] reads the polarity off a probe standing at the
      // parameter's own polarity; at its input type, the other.
      final t = flip
          ? (type.endsWith('?') ? type.substring(0, type.length - 1) : '$type?')
          : type;
      final have = out[param];
      if (have == null || have.contains(t)) return;
      have.add(t);
    }

    for (final clause in clauses) {
      final head =
          ast.Goal(clause.head.functor, clause.head.args, clause.line, clause.column);
      final Map<String, VariableTypeInfo> headTypes;
      try {
        final (result, _) =
            _checkHeadWithTerm(TypedClause(head: head), decl, probeDfa, probeEnv);
        headTypes = result.variableTypes;
      } on Object {
        continue; // a head this declaration cannot type fixes nothing
      }
      final bodyTypes = <String, List<VariableTypeInfo>>{};
      for (final goal in clause.body ?? const <ast.Goal>[]) {
        final Map<String, VariableTypeInfo> types;
        try {
          types = _bodyAtomVariableTypes(goal, probeDfa, probeEnv);
        } on Object {
          continue;
        }
        for (final e in types.entries) {
          bodyTypes.putIfAbsent(e.key, () => []).add(e.value);
        }
      }
      for (final e in headTypes.entries) {
        final at = e.value.typeState;
        if (!_reachesParam(at, probe)) continue;
        final pair = headTypes[_otherKey(e.key)];
        if (pair != null && !paramOf.containsKey(pair.typeState.baseName)) {
          _supplied(at, pair.typeState.dual, probe, probeDfa, emit);
        }
        for (final b in bodyTypes[e.key] ?? const <VariableTypeInfo>[]) {
          if (paramOf.containsKey(b.typeState.baseName)) continue;
          _supplied(at, b.typeState, probe, probeDfa, emit);
        }
      }
    }
  }
  return out;
}

/// [template] with each parameter replaced as [subst] says --- a type of the
/// program, or one of [abstractNames], each an abstract type with no
/// alternatives (def:abstract-type) --- a parameter named in [input] with
/// `true` standing at the input type of its replacement; with the environment
/// and DFA holding the abstract types and the template instantiations the
/// declaration names.  Null where a type it names is not in scope.
(ProcDecl, TypeEnvironment, ProgramDFA)? _declInAbstractTypes(
    ProcDecl template,
    Map<String, String> subst,
    Map<String, bool> input,
    Set<String> abstractNames,
    TypeEnvironment env) {
  final decl = ProcDecl(
    template.name,
    [for (final t in template.argTypes) _substituteTypeParams(t, subst, input)],
    template.line,
    template.column,
    exported: template.exported,
    imported: template.imported,
    modulePath: template.modulePath,
  );
  final types = <String, TypeDef>{
    for (final n in abstractNames) n: TypeDef(n, const [], 0, 0)
  };
  final needed = <String>{};
  for (final t in decl.argTypes) {
    var n = getFullTypeName(t);
    if (n.endsWith('?')) n = n.substring(0, n.length - 1);
    if (n.contains('<') && !env.types.containsKey(n)) needed.add(n);
  }
  try {
    if (needed.isNotEmpty) {
      types.addAll(materializeInstantiations(
          needed, env.typeTemplates, {...env.types.keys, ...abstractNames}));
    }
    final declEnv = TypeEnvironment(
      {...env.types, ...types},
      {...env.procedures, decl.key: decl},
      paramProcDecls: env.paramProcDecls,
      typeTemplates: env.typeTemplates,
      typeOrigins: env.typeOrigins,
    );
    return (decl, declEnv, buildProgramDFA(declEnv));
  } on UnknownTypeError {
    return null;
  }
}

/// Infer a concrete proc decl by matching a parameterized template against
/// the actual argument types at a call site.
///
/// Returns null if inference fails (e.g., no matching variable types found).
///
/// A clause's calls are not instantiated here but by [_instantiateCalls],
/// which reads them over the whole clause and tries only the types their sites
/// supply (TGLP appendix-implementation-notes.tex, "The instantiation of a
/// call").  This inference, from the occurrences typed before the call and the
/// callee's own clauses ([_solveFromCalleeClauses], [_thetaFromHeads]), serves
/// what that paragraph does not speak of: a goal ([checkGoal]) and a defined
/// guard as written ([definedGuardMeetErrors]).
///
/// The inferred map is an instantiation (TGLP `parameterized-types.tex`,
/// Definition "Instantiation"): it sends each parameter to a type of the
/// program, and an input type is a type (`typed-glp.tex`, "Type Declarations":
/// `Stream?` is the dual of `Stream`, an input type).  A parameter bound at a
/// bare occurrence, `X` or `X?` as an argument's whole type, takes its type
/// from the variable there, which fixes the base type and not its polarity: a
/// writer at `X` binds `X` to the output type `T`, a reader at `X` to the input
/// type `T?`, and at `X?` the other way about, complementation being an
/// involution, $(T?)? = T$ (`appendix-type-automaton.tex`, Definition "Dual Type
/// Automaton").  Both bindings are considered, and the one under which the call
/// is well-typed is taken; every other condition of the definition --- the
/// caller's clause, the callee's clauses, input coverage --- is checked of it
/// as of any instantiation.  A parameter bound only inside a template, as `X`
/// in `Stream(X)`, takes the type argument of the actual type as it stands, its
/// polarity included, and has no second binding to consider.
(ProcDecl, bool)? _inferConcreteDecl(
  ProcDecl paramTemplate,
  ast.Goal atom,
  Map<String, VariableTypeInfo> callerVarTypes,
  ProgramDFA dfa,
  TypeEnvironment env,
  CalleeClauses? callee,
) {
  final bindings = <String, String>{}; // typeParam -> concreteTypeName
  // The parameters bound at a bare occurrence: base type known, polarity open.
  final bare = <String>{};

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
    // base type needed to instantiate the type parameter; the polarity of a
    // bare parameter's binding is decided below. Without this the common case
    // --- a polymorphic parameter passed a reader --- failed inference and the
    // body's polarity obligation was never re-checked (Issue 14).
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
    _matchTypeForInference(declaredType, actualTypeName, paramTemplate.typeParams,
        bindings, env, bare: bare);
  }

  // The polarity of each bare-bound parameter, decided before the callee's
  // clauses are consulted: the candidate instantiations take each at its output
  // type and at its input type, and the call's own arguments decide between
  // them.  The variable a bare parameter was bound from sits at a position whose
  // mode is that of the binding, and a writer is consistent only with an output
  // type there and a reader only with an input one (`def:consistent-paths` rows
  // 2 and 3), so at most one candidate passes, and where none does the call is
  // ill-typed under every binding and is reported at the output one, as before.
  // A parameter no caller equation reaches is left out of the test (its
  // positions are skipped as bare parameters of [_checkArgumentModes]), since
  // the choice concerns only the parameters the call binds.
  //
  // Deciding first is what TGLP def:instantiation asks: "C and the clauses of q
  // are well-typed when q's declaration is replaced by its expansion under
  // theta".  The caller's clause C admits one polarity only, so the callee's
  // clauses are probed and verified below under that polarity, never under the
  // other, which C already rules out.
  final open = [
    for (final tp in paramTemplate.typeParams)
      if (bare.contains(tp)) tp
  ];
  var chosenInput = const <String, bool>{};
  if (open.isNotEmpty) {
    final unbound = [
      for (final tp in paramTemplate.typeParams)
        if (!bindings.containsKey(tp)) tp
    ];
    final passing = <Map<String, bool>>[];
    for (var mask = 0; mask < (1 << open.length); mask++) {
      final input = <String, bool>{
        for (var j = 0; j < open.length; j++) open[j]: (mask >> j) & 1 == 1,
      };
      final candidate =
          _concreteDecl(paramTemplate, bindings, input, open: unbound);
      if (_checkArgumentModes(atom, candidate, env).isWellTyped) {
        passing.add(input);
      }
    }
    if (passing.length == 1) chosenInput = passing.single;
  }

  // The caller's arguments settle only the parameters a variable argument puts
  // opposite a declared type.  The rest come from the callee's own clauses:
  // TGLP def:instantiation makes an instantiation a map under which the caller's
  // clause AND the clauses of the called procedure are well-typed, so a variable
  // pair of the callee's head that the declaration types by a parameter on one
  // side and by a concrete type on the other is an equation for that parameter
  // --- condition 3(a) of def:well-typed-clause requires the two to be dual:
  // the same base type, and opposite polarities, which the equation's binding
  // supplies (an input type where the two sides stand at one polarity).  It is
  // what fixes `M` in
  // `send_user(M?, Stream(Ent)?, Stream(Ent))` once the call has fixed `Ent`:
  // the clause writes `Msg` at `M?` and reads `Msg?` at the element type of the
  // stream `Ent` carries, so `M` is that element type.
  _solveFromCalleeClauses(
      paramTemplate, atom, callerVarTypes, bindings, chosenInput, env, callee);

  // A parameter no equation fixes is fixed by the constructors the callee's
  // heads match at its positions, coverage selecting it (def:instantiation, and
  // Udi 2026-09-20): construct it and verify, rather than search.
  if (callee != null) {
    final defining = callee.of(paramTemplate.key);
    if (defining != null && defining.isNotEmpty) {
      _thetaFromHeads(
          paramTemplate, bindings, chosenInput, defining, dfa, env, callee);
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

  final chosen = _concreteDecl(paramTemplate, bindings, chosenInput);
  final concreteArgTypes = chosen.argTypes;

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

  final bindsInputType = chosenInput.values.any((b) => b) ||
      bindings.values.any((v) => v.endsWith('?'));
  return (chosen, bindsInputType);
}

/// The declaration [paramTemplate] produces under [bindings], each parameter
/// named in [input] with `true` bound to the input type of its binding.  The
/// parameters [bindings] does not carry stay parameters, named in [open].
ProcDecl _concreteDecl(ProcDecl paramTemplate, Map<String, String> bindings,
    Map<String, bool> input, {List<String> open = const []}) {
  return ProcDecl(
      paramTemplate.name,
      [
        for (final argType in paramTemplate.argTypes)
          _substituteTypeParams(argType, bindings, input)
      ],
      paramTemplate.line,
      paramTemplate.column,
      typeParams: open,
      exported: paramTemplate.exported,
      imported: paramTemplate.imported,
      modulePath: paramTemplate.modulePath);
}

/// The name of the abstract type standing for a parameter while a call's sites
/// are read ([_probeOf]) and while the callee's clauses are read for the types
/// they fix for it ([_calleeFixedTypes]).
const String _paramProbePrefix = r'$param_';

/// Read from the clauses of [paramTemplate]'s procedure the equations that fix
/// the parameters [bindings] does not yet carry.
///
/// For a goal and a guard as written only ([_inferConcreteDecl]): in a clause a
/// type no site of the call supplies is not tried (TGLP
/// appendix-implementation-notes.tex, "The instantiation of a call"), and a
/// type read off the callee's clauses is not one a site supplies.
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
  Map<String, bool> input,
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

    // The probe, with every unbound parameter at its output type, or with every
    // one at its input type where [flip] is set.  A bare-bound parameter stands
    // at the polarity the call chose for it.
    (ProcDecl, TypeEnvironment, ProgramDFA)? probe(bool flip) {
      final probeInput = {
        ...input,
        for (final tp in unbound) tp: flip,
      };
      final probeDecl = ProcDecl(
        paramTemplate.name,
        [
          for (final t in paramTemplate.argTypes)
            _substituteTypeParams(t, subst, probeInput)
        ],
        paramTemplate.line,
        paramTemplate.column,
        exported: paramTemplate.exported,
        imported: paramTemplate.imported,
        modulePath: paramTemplate.modulePath,
      );

      // The abstract types themselves, plus any template instantiation the
      // substitution names that the environment does not yet hold ---
      // `Stream(X)` over a probe becomes `Stream<$param_X>`, which nothing has
      // materialized.
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
      try {
        return (probeDecl, probeEnv, buildProgramDFA(probeEnv));
      } on UnknownTypeError {
        return null; // a substituted type is not in scope
      }
    }

    final atOutput = probe(false);
    if (atOutput == null) return; // no equation to read here
    final (probeDecl, _, probeDfa) = atOutput;

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
        // Where the variable stands at a parameter itself, its own mode sets
        // the polarity of the binding, as at a bare parameter of the call: a
        // writer at `X` binds `X` to the output type, a reader to the input
        // type, and at `X?` the other way about.  (The probe here has every
        // unbound parameter at its output type, which [e.value] reflects.)
        _unifyProbeNames(
            e.value.name, info.typeState.baseName, paramOfProbe, bindings,
            inputAtTop: e.key.endsWith('?') != e.value.isDual);
        if (bindings.length != before) progress = true;
      }
    }
    if (progress) continue;

    // Equations from the callee's clauses, each unbound parameter probed at its
    // output type and then at its input type.  An input type is a type
    // (typed-glp.tex, "Type Declarations"), so def:instantiation leaves a
    // parameter's polarity open as it leaves its base type, and the clauses
    // decide it: an occurrence at a position whose mode is not its own is an
    // inconsistent path and is given no type (def:consistent-paths rows 2 and
    // 3), so a variable pair states its equation only under the polarity at
    // which its probe-side occurrence is consistent, and that polarity is the
    // binding's.  Probed at the output type alone, a parameter whose clauses
    // read it at its input type is reached by no equation, as the call's own
    // bare parameters would be without the choice [_inferConcreteDecl] makes.
    for (final flip in const [false, true]) {
      final pr = flip ? probe(true) : atOutput;
      if (pr == null) continue;
      final (flipDecl, flipEnv, flipDfa) = pr;
      for (final clause in clauses) {
        final ClauseCheckResult res;
        try {
          res = checkClauseFromAst(clause, flipDfa, flipEnv,
              activeInstantiations: {flipDecl.key: flipDecl});
        } on Object {
          continue; // this clause yields no equation
        }
        for (final e in res.errors) {
          if (e is! ClauseDualityError) continue;
          if (e.writerLocation != e.readerLocation) continue; // 3(a) only
          final ws = e.writerType?.typeState;
          final rs = e.readerType?.typeState;
          if (ws == null || rs == null) continue;
          final w = ws.baseName;
          final r = rs.baseName;
          final String probeName, other;
          if (probeNames.contains(w) && !probeNames.contains(r)) {
            probeName = w;
            other = r;
          } else if (probeNames.contains(r) && !probeNames.contains(w)) {
            probeName = r;
            other = w;
          } else {
            continue;
          }
          if (other == '_' || other == '_?') continue;
          // Dual is the same base type at opposite polarities.  The probe
          // side stands at the polarity [flip] gives the parameter, so the
          // binding keeps that polarity where the two sides stand at opposite
          // ones, and takes the other where they stand at one.
          final samePolarity = ws.isDual == rs.isDual;
          final bound = flip != samePolarity ? '$other?' : other;
          final tp = probeName.substring(_paramProbePrefix.length);
          final had = bindings[tp];
          if (had == null) {
            bindings[tp] = bound;
            progress = true;
          } else if (had != bound && !_sameBinding(had, bound, flipDfa)) {
            // Two clauses, or two occurrences, require different types of one
            // parameter: no map makes them all well-typed, so the call has no
            // instantiation.
            bindings.remove(tp);
            return;
          }
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
///
/// The polarity of a binding follows the one rule a bare parameter of the call
/// follows (see [_inferConcreteDecl]): at the top, where the variable stands at
/// the parameter itself, [inputAtTop] carries it, decided by the variable's
/// mode; inside a template's arguments the type argument is taken as it
/// stands, its polarity included, `X?` against `T` giving `X` the input type
/// `T?` and against `T?` the output type `T`, complementation being an
/// involution (TGLP appendix-type-automaton.tex, Definition "Dual Type
/// Automaton").
void _unifyProbeNames(String declName, String actualName,
    Map<String, String> paramOfProbe, Map<String, String> bindings,
    {bool? inputAtTop}) {
  var d = declName, a = actualName;
  final dIn = d.endsWith('?'), aIn = a.endsWith('?');
  if (dIn) d = d.substring(0, d.length - 1);
  if (aIn) a = a.substring(0, a.length - 1);
  final param = paramOfProbe[d];
  if (param != null) {
    if (a != '_' && !paramOfProbe.containsKey(a)) {
      final input = inputAtTop ?? (dIn != aIn);
      bindings.putIfAbsent(param, () => input ? '$a?' : a);
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

/// Whether two bindings of one parameter bind it to one type: the same polarity,
/// and base types with one automaton (type identity being structural).
bool _sameBinding(String a, String b, ProgramDFA dfa) {
  final aIn = a.endsWith('?'), bIn = b.endsWith('?');
  if (aIn != bIn) return false;
  final ab = aIn ? a.substring(0, a.length - 1) : a;
  final bb = bIn ? b.substring(0, b.length - 1) : b;
  return ab == bb || sameBaseType(ab, bb, dfa);
}

/// Build, for each parameter no equation fixes, the type the callee's heads
/// require, and adopt it if it verifies.
///
/// For a goal and a guard as written only ([_inferConcreteDecl]): in a clause a
/// type no site of the call supplies is not tried (TGLP
/// appendix-implementation-notes.tex, "The instantiation of a call"), and a
/// type built from the callee's heads is not one a site supplies.
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
  Map<String, bool> input,
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
  // A bare-bound parameter stands at the polarity the call chose for it, here
  // and in the verification below.
  final probeDecl = ProcDecl(
    paramTemplate.name,
    [
      for (final t in paramTemplate.argTypes)
        _substituteTypeParams(t, subst, input)
    ],
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
        _substituteTypeParams(t, candidate, input)
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
/// type parameter bindings.  A parameter bound at a bare occurrence is added
/// to [bare].
void _matchTypeForInference(
  TypeExpr declaredType,
  String actualTypeName,
  List<String> typeParams,
  Map<String, String> bindings,
  TypeEnvironment env, {
  Set<String>? bare,
}) {
  if (declaredType is TypeRef) {
    if (declaredType.typeArgs.isEmpty && typeParams.contains(declaredType.name)) {
      // Bare type parameter: X → actualTypeName, its polarity still open
      if (!bindings.containsKey(declaredType.name)) {
        bindings[declaredType.name] = actualTypeName;
        bare?.add(declaredType.name);
      }
      return;
    }

    if (declaredType.typeArgs.isNotEmpty) {
      // Parameterized type ref: Stream(X) vs Stream<AgentMsg>
      // Parse the actual type name to extract template and args
      var resolvedActual = actualTypeName;
      var ltIdx = resolvedActual.indexOf('<');
      if (ltIdx < 0) {
        // Actual is a named type.  "Type identity is structural, so two types
        // with the same automaton bind the parameter consistently whatever
        // their names or defining modules" (TGLP parameterized-types.tex, after
        // Definition "Instantiation"): a named type whose alternatives are the
        // declared template's under some arguments IS that template's instance
        // --- `T ::= [] ; [E | T]` is Stream<E>, and `IntroChannel ::=
        // ch(IntroStream, IntroStream?)` is Channel<IntroStream,IntroStream> ---
        // so it is read as its template form before matching.  Without this a
        // named type binds no parameter, the call records no instantiation, and
        // the parametric procedure is never checked at this type.  Until
        // 2026-10-02 only the list shape was read so, and a named channel type
        // fixed no parameter of a Channel(Stream(C), Stream(C)) declaration
        // (GLP 2026-10-01 23:58 UTC item 3).
        final structForm =
            _structuralFormOfNamedType(resolvedActual, declaredType.name, env);
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
          // `X?` against the argument `T` binds `X` to `T?`, and against `T?`
          // to `T`: the involution again.  A parameter bound inside a template
          // takes the type argument as it stands, its polarity included, and is
          // not polarity-open (it is not added to [bare]).
          var actual = actualArgs[j];
          if (declArg.isInput) {
            actual = actual.endsWith('?')
                ? actual.substring(0, actual.length - 1)
                : '$actual?';
          }
          bindings.putIfAbsent(declArg.name, () => actual);
          continue;
        }
        // Recurse: a parameter may sit at any depth of a template's arguments.
        // `Channel(Stream(C), Stream(C))?` against `Channel<Stream<X>,Stream<X>>`
        // binds C only by descending into the argument, and until 2026-09-20
        // only a BARE argument bound, so C stayed free and the call had no
        // instantiation --- which is what left befriend_commit/7 and
        // intro_await_peer/3 uninstantiated in every program using them.
        _matchTypeForInference(
            declArg, actualArgs[j], typeParams, bindings, env);
      }
    }
  }
}

/// The named (non-parameterized) type [typeName] read as an instance of the
/// template [templateName]: `templateName<A1,...,Ak>`, the arguments under which
/// the template's alternatives are the named type's, or null where there are
/// none.  Type identity is structural (TGLP parameterized-types.tex, after
/// Definition "Instantiation"), and the expansion of `T(S1, ..., Sk)` is the
/// template's alternatives with each `Xi` replaced by `Si` (Section "Expansion",
/// "Expansion rule"), so a named type whose alternatives are those of
/// `T(S1, ..., Sk)` is that instance: `MsgStream ::= [] ; [Msg | MsgStream]` is
/// `Stream<Msg>`, `IntroChannel ::= ch(IntroStream, IntroStream?)` is
/// `Channel<IntroStream,IntroStream>`.
///
/// The alternatives are paired by their top-level functor, which identifies an
/// alternative ("alternatives are distinguished by their top-level functor",
/// TGLP typed-glp.tex), and matched position by position: a parameter takes the
/// type at its position, its polarity included --- `Out?` against `S?` binds
/// `Out` to `S`, against `S` to `S?`, complementation being an involution ---
/// and binds consistently or not at all; the template's reference to itself
/// stands for the named type; a reference to another template is read the same
/// way, recursively.  A template with a parameter as an alternative is not
/// matched (its alternatives are not determined by their functors).
String? _structuralFormOfNamedType(
    String typeName, String templateName, TypeEnvironment env,
    [Set<String>? visiting]) {
  final def = env.getType(typeName);
  if (def == null || def.typeParams.isNotEmpty) return null;
  final template = env.typeTemplates[templateName];
  if (template == null || template.typeParams.isEmpty) return null;
  if (template.alternatives.length != def.alternatives.length) return null;
  // A cycle through templates referring to one another reads nothing.
  final key = '$typeName@$templateName';
  final seen = visiting ?? <String>{};
  if (!seen.add(key)) return null;
  try {
    final byShape = <String, TypeExpr>{};
    for (final alt in def.alternatives) {
      final k = _alternativeShape(alt);
      if (k == null || byShape.containsKey(k)) return null;
      byShape[k] = alt;
    }
    final theta = <String, String>{};
    for (final talt in template.alternatives) {
      final k = _alternativeShape(talt, template.typeParams);
      if (k == null) return null;
      final nalt = byShape[k];
      if (nalt == null) return null;
      if (!_bindTemplateAlt(talt, nalt, typeName, template, theta, env, seen)) {
        return null;
      }
    }
    if (!template.typeParams.every(theta.containsKey)) return null;
    return '$templateName<${template.typeParams.map((p) => theta[p]).join(',')}>';
  } finally {
    seen.remove(key);
  }
}

/// The top-level functor that identifies alternative [alt], or null for an
/// alternative that is not identified by one: a parameter of [params], or a
/// reference to a template instance.
String? _alternativeShape(TypeExpr alt, [List<String> params = const []]) {
  if (alt is ConstantAlt) return 'const:${alt.value}';
  if (alt is StructAlt) return 'struct:${alt.functor}/${alt.args.length}';
  if (alt is ListNilAlt) return 'nil';
  if (alt is ListConsAlt) return 'cons';
  if (alt is DiffListAlt) return 'difflist';
  if (alt is PrimitiveModeAlt) return alt.isInput ? 'any?' : 'any';
  if (alt is TypeRef) {
    if (alt.typeArgs.isNotEmpty || params.contains(alt.name)) return null;
    return 'ref:${alt.name}${alt.isInput ? '?' : ''}';
  }
  return null;
}

/// Match the template alternative (or position) [t] against the named type's
/// [n], extending [theta]; false on a mismatch.
bool _bindTemplateAlt(TypeExpr t, TypeExpr n, String typeName, TypeDef template,
    Map<String, String> theta, TypeEnvironment env, Set<String> visiting) {
  if (t is TypeRef) {
    if (n is! TypeRef || n.typeArgs.isNotEmpty) return false;
    final selfRef = t.name == template.name &&
        t.typeArgs.length == template.typeParams.length &&
        [
          for (var i = 0; i < t.typeArgs.length; i++)
            t.typeArgs[i] is TypeRef &&
                (t.typeArgs[i] as TypeRef).name == template.typeParams[i] &&
                (t.typeArgs[i] as TypeRef).typeArgs.isEmpty &&
                !(t.typeArgs[i] as TypeRef).isInput
        ].every((b) => b);
    if (selfRef && n.name == typeName) return t.isInput == n.isInput;
    return _bindTemplateRef(t, n.isInput ? '${n.name}?' : n.name,
        template.typeParams, theta, env, visiting);
  }
  if (t is PrimitiveModeAlt) return n is PrimitiveModeAlt && n.isInput == t.isInput;
  if (t is ConstantAlt) return n is ConstantAlt && n.value == t.value;
  if (t is ListNilAlt) return n is ListNilAlt;
  if (t is ListConsAlt) {
    return n is ListConsAlt &&
        _bindTemplateAlt(t.head, n.head, typeName, template, theta, env, visiting) &&
        _bindTemplateAlt(t.tail, n.tail, typeName, template, theta, env, visiting);
  }
  if (t is StructAlt) {
    if (n is! StructAlt || n.functor != t.functor || n.args.length != t.args.length) {
      return false;
    }
    for (var i = 0; i < t.args.length; i++) {
      if (!_bindTemplateAlt(t.args[i], n.args[i], typeName, template, theta, env, visiting)) {
        return false;
      }
    }
    return true;
  }
  if (t is DiffListAlt) {
    return n is DiffListAlt &&
        _bindTemplateAlt(t.content, n.content, typeName, template, theta, env, visiting) &&
        _bindTemplateAlt(t.hole, n.hole, typeName, template, theta, env, visiting);
  }
  return false;
}

/// Match the template reference [t] against the type named [actual] (a full
/// name, `?` marking an input type), extending [theta]: a parameter is bound to
/// it, a concrete type must be it, and a template instance `T(A1, ..., Ak)`
/// must be `T<...>`, by name or as a named type read as its template form
/// ([_structuralFormOfNamedType]), its arguments matched in turn.
bool _bindTemplateRef(TypeExpr t, String actual, List<String> params,
    Map<String, String> theta, TypeEnvironment env, Set<String> visiting) {
  if (t is! TypeRef) return false;
  final actualInput = actual.endsWith('?');
  final actualBase =
      actualInput ? actual.substring(0, actual.length - 1) : actual;
  if (t.typeArgs.isEmpty && params.contains(t.name)) {
    final value = t.isInput != actualInput ? '$actualBase?' : actualBase;
    final have = theta[t.name];
    if (have == null) {
      theta[t.name] = value;
      return true;
    }
    return have == value;
  }
  if (t.isInput != actualInput) return false;
  if (t.typeArgs.isEmpty) return t.name == actualBase;
  var form = actualBase;
  if (!form.startsWith('${t.name}<')) {
    if (form.contains('<')) return false;
    final named = _structuralFormOfNamedType(form, t.name, env, visiting);
    if (named == null) return false;
    form = named;
  }
  final args = _splitTypeArgs(form.substring(t.name.length + 1, form.length - 1));
  if (args.length != t.typeArgs.length) return false;
  for (var i = 0; i < args.length; i++) {
    if (!_bindTemplateRef(t.typeArgs[i], args[i], params, theta, env, visiting)) {
      return false;
    }
  }
  return true;
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
///
/// A parameter named in [input] with `true` is bound to the input type of its
/// binding, and a binding written with a trailing `?` is an input type too.
/// Either way it complements as any type does: `X?` under `X = T?` is `T`,
/// complementation being an involution, $(T?)? = T$ (TGLP
/// `appendix-type-automaton.tex`, Definition "Dual Type Automaton").
TypeExpr _substituteTypeParams(TypeExpr expr, Map<String, String> bindings,
    [Map<String, bool> input = const {}]) {
  if (expr is TypeRef) {
    if (expr.typeArgs.isEmpty && bindings.containsKey(expr.name)) {
      // Bare type param → concrete type, complemented where it occurs as X?
      var name = bindings[expr.name]!;
      var boundInput = input[expr.name] ?? false;
      if (name.endsWith('?')) {
        name = name.substring(0, name.length - 1);
        boundInput = !boundInput;
      }
      return TypeRef(name, expr.line, expr.column,
          isInput: expr.isInput != boundInput);
    }
    if (expr.typeArgs.isNotEmpty) {
      // Parameterized ref: substitute args and create expanded name
      final newArgs = expr.typeArgs
          .map((a) => _substituteTypeParams(a, bindings, input))
          .toList();
      // Check if all args are now concrete (no more type params).  The
      // wildcard is a concrete argument: the expansion names `Stream(_)`
      // `Stream<_>` (param_expansion.dart, _expandedName).  Until 2026-10-02 it
      // was not taken for one, so no instantiation of a declaration carrying
      // `Stream(_)` beside a parameter could be built, and the call went
      // uninstantiated.
      final allConcrete = newArgs.every((a) =>
          (a is TypeRef &&
              a.typeArgs.isEmpty &&
              !bindings.containsKey(a.name)) ||
          a is PrimitiveModeAlt);
      if (allConcrete) {
        // Create expanded name: Stream<AgentMsg>, or Stream<Menu?> where the
        // argument is an input type, as the expansion names it
        // (param_expansion.dart, _expandedName).
        final expandedName =
            '${expr.name}<${newArgs.map(getFullTypeName).join(',')}>';
        return TypeRef(expandedName, expr.line, expr.column, isInput: expr.isInput);
      }
      return TypeRef(expr.name, expr.line, expr.column, isInput: expr.isInput, typeArgs: newArgs);
    }
    return expr;
  }
  if (expr is PrimitiveModeAlt) return expr;
  return expr;
}

/// Is [type] one of [typeParams] standing bare, `X` or `X?`, as an argument's
/// whole type?
bool _isBareParameter(TypeExpr type, List<String> typeParams) =>
    type is TypeRef && type.typeArgs.isEmpty && typeParams.contains(type.name);
