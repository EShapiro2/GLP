// lib/analysis/type_checker/type_checker.dart
//
// Main type checker implementing GLP well-typed program checking.
// Specification: docs/modules/well-typed-program.md v0.7
// Paper Reference: Definition 4.10 (lines 351-357)
//
// A typed GLP program P = (Cs, D) is well-typed if:
// 1. Covariance: Every clause C ∈ Cs is well-typed by D
// 2. Contravariance: Every input path in every procedure type in D has
//    a clause C ∈ Cs that accepts it

import 'type_ast.dart';
import 'param_expansion.dart';
import 'program_dfa.dart';
import 'mode.dart';
import 'type_environment_builder.dart';
import 'well_typed_clause.dart' as wtc;
import 'clause_validation.dart';
import '../../compiler/ast.dart' as ast;
import '../../compiler/lexer.dart';
import '../../compiler/parser.dart';
import '../../compiler/error.dart';

// =============================================================================
// Result Types
// =============================================================================

/// Result of type checking a program
class TypeCheckResult {
  final List<TypeError> errors;
  final List<TypeWarning> warnings;

  TypeCheckResult(this.errors, this.warnings);

  bool get isWellTyped => errors.isEmpty;

  @override
  String toString() {
    final sb = StringBuffer();
    if (errors.isNotEmpty) {
      sb.writeln('Type Errors:');
      for (final e in errors) {
        sb.writeln('  $e');
      }
    }
    if (warnings.isNotEmpty) {
      sb.writeln('Warnings:');
      for (final w in warnings) {
        sb.writeln('  $w');
      }
    }
    if (isWellTyped && warnings.isEmpty) {
      sb.writeln('Program is well-typed.');
    }
    return sb.toString();
  }
}

/// A type error
class TypeError {
  final String message;
  final int line;
  final int column;
  final String? clauseText;

  TypeError(this.message, this.line, this.column, [this.clauseText]);

  @override
  String toString() {
    final loc = 'line $line, column $column';
    return '$message at $loc${clauseText != null ? '\n    in: $clauseText' : ''}';
  }
}

/// A type warning
class TypeWarning {
  final String message;
  final int line;
  final int column;

  TypeWarning(this.message, this.line, this.column);

  @override
  String toString() => '$message at line $line, column $column';
}

/// Error for uncovered input alternative
class CoverageError {
  final String procedure;
  final int argIndex;
  final String uncoveredLabel;
  final String path;

  CoverageError({
    required this.procedure,
    required this.argIndex,
    required this.uncoveredLabel,
    required this.path,
  });

  @override
  String toString() =>
      '$procedure argument $argIndex: uncovered alternative "$uncoveredLabel" at path: $path';
}

// =============================================================================
// Main Type Checker
// =============================================================================

/// The main type checker implementing Definition 4.10
class TypeChecker {
  final TypeEnvironment typeEnv;
  final ProgramDFA dfa;

  /// When non-null, call-site instantiations of parameterized procedures are
  /// recorded into it (for per-instantiation checking by the closure). A
  /// parameterized procedure has no well-typing of its own and is never given a
  /// verdict here under the wildcard `_` declaration — it is checked only per
  /// concrete instantiation (typed-program.md "Programs and Modules").
  final wtc.InstantiationCollector? collector;

  TypeChecker(this.typeEnv, {this.collector}) : dfa = buildProgramDFA(typeEnv);

  /// Check a program (list of clauses) against declared types
  ///
  /// Implements Definition 4.10:
  /// 1. Covariance: Every clause is well-typed
  /// 2. Contravariance: Every input path is covered
  TypeCheckResult check(List<ast.Clause> clauses) {
    final errors = <TypeError>[];
    final warnings = <TypeWarning>[];

    // =======================================================================
    // Phase 0: Validate clause terms (anonymous variable restrictions)
    // Per spec: clause-validation.md - reject _? everywhere, reject _ in bodies
    // =======================================================================
    for (final clause in clauses) {
      try {
        // Validate head arguments
        for (final arg in clause.head.args) {
          validateClauseHead(arg);
        }
        // Validate guard arguments
        if (clause.guards != null) {
          for (final guard in clause.guards!) {
            for (final arg in guard.args) {
              validateGuard(arg);
            }
          }
        }
        // Validate body arguments
        if (clause.body != null) {
          for (final goal in clause.body!) {
            for (final arg in goal.args) {
              validateClauseBody(arg);
            }
          }
        }
      } on CompileError catch (e) {
        errors.add(TypeError(
          e.message,
          e.line,
          e.column,
          _clauseToString(clause),
        ));
      }
    }

    // If validation errors, return early
    if (errors.isNotEmpty) {
      return TypeCheckResult(errors, warnings);
    }

    // Group clauses by procedure (name/arity)
    final procedureClauses = <String, List<ast.Clause>>{};
    for (final clause in clauses) {
      final key = '${clause.head.functor}/${clause.head.arity}';
      procedureClauses.putIfAbsent(key, () => []).add(clause);
    }

    // Check each declared procedure
    for (final procDecl in typeEnv.procedures.values) {
      final key = procDecl.key;

      // A parameterized procedure is syntactic sugar with no well-typing of its
      // own: it is checked only per concrete instantiation (the closure below /
      // checkInstantiationsClosed), never under the wildcard `_` declaration —
      // which would accept a parameter-inspecting clause vacuously (unsound;
      // typed-program.md "Programs and Modules"). Skip its wildcard verdict
      // here. Its call-site instantiations are collected when the monomorphic
      // callers' bodies are traversed below; calls inside its own (polymorphic)
      // body induce instantiations only once it is itself instantiated.
      if (typeEnv.paramProcDecls.containsKey(key)) {
        continue;
      }

      final procClauses = procedureClauses[key];

      if (procClauses == null || procClauses.isEmpty) {
        // Skip warning for builtins (implemented in Dart, no GLP clauses)
        if (!procDecl.isBuiltin) {
          warnings.add(TypeWarning(
            'Procedure ${procDecl.name}/${procDecl.arity} declared but not defined',
            procDecl.line,
            procDecl.column,
          ));
        }
        continue;
      }

      final procResult = _checkProcedure(procDecl, procClauses);
      errors.addAll(procResult.errors);
      warnings.addAll(procResult.warnings);
    }

    // Warn about undefined procedures (clauses without type declarations)
    for (final entry in procedureClauses.entries) {
      if (!typeEnv.procedures.containsKey(entry.key)) {
        final firstClause = entry.value.first;
        warnings.add(TypeWarning(
          'Procedure ${entry.key} has no type declaration',
          firstClause.line,
          firstClause.column,
        ));
      }
    }

    return TypeCheckResult(errors, warnings);
  }

  /// Check one procedure's clauses against a specific declaration, in this
  /// checker's environment. Used by the per-instantiation closure
  /// (checkInstantiationsClosed) to check a parameterized procedure's defining
  /// clauses against a concrete instantiation — the env is the instantiation's
  /// caller scope, with [decl] overriding the procedure entry.
  ///
  /// Covariance resolves the declaration through the environment, so the
  /// environment passed to the constructor must already bind this procedure to
  /// [decl]; contravariance uses [decl] directly.
  TypeCheckResult checkSingleProcedure(ProcDecl decl, List<ast.Clause> clauses,
      {Map<String, ProcDecl> activeInstantiations = const {}}) {
    return _checkProcedure(decl, clauses, activeInstantiations);
  }

  /// Check a single procedure against its declared type.
  ///
  /// [activeInstantiations] maps the key of each parameterized procedure being
  /// instantiated on the current derivation cycle to its monomorphic
  /// declaration. A body call to such a procedure is checked at that
  /// instantiation (monomorphic recursion), not re-inferred. Empty for the
  /// ordinary (monomorphic-clause) pass.
  TypeCheckResult _checkProcedure(ProcDecl decl, List<ast.Clause> clauses,
      [Map<String, ProcDecl> activeInstantiations = const {}]) {
    final errors = <TypeError>[];
    final warnings = <TypeWarning>[];

    // =======================================================================
    // Condition 1: Covariance — check each clause is well-typed
    // =======================================================================
    for (final clause in clauses) {
      final clauseErrors =
          _checkClauseCovariance(clause, decl, activeInstantiations);
      errors.addAll(clauseErrors);
    }

    // =======================================================================
    // Condition 2: Contravariance — check input coverage
    // =======================================================================
    // Defined guards (receive/close/...) are unfolded into the clause head by
    // partial evaluation before type checking, so the (transformed) clauses are
    // the ones cond. 2 quantifies over: coverage runs on the guard-unfolded
    // heads. A channel consumer that unfolds only `receive` leaves ch([]) —
    // the closed read stream — uncovered, and must add a `close` clause.
    for (int argIndex = 1; argIndex <= decl.arity; argIndex++) {
      if (decl.isInputArg(argIndex - 1)) {
        final coverageErrors = _checkInputCoverage(clauses, decl, argIndex);
        errors.addAll(coverageErrors);
      }
    }

    return TypeCheckResult(errors, warnings);
  }

  // ===========================================================================
  // Covariance Checking (Condition 1)
  // ===========================================================================

  /// Check covariance for a single clause using well_typed_clause module
  List<TypeError> _checkClauseCovariance(ast.Clause clause, ProcDecl decl,
      [Map<String, ProcDecl> activeInstantiations = const {}]) {
    final errors = <TypeError>[];

    try {
      final result = wtc.checkClauseFromAst(clause, dfa, typeEnv,
          collector: collector, activeInstantiations: activeInstantiations);

      if (!result.isWellTyped) {
        // Convert ClauseErrors to TypeErrors
        for (final error in result.errors) {
          errors.add(TypeError(
            error.message,
            clause.line,
            clause.column,
            _clauseToString(clause),
          ));
        }
      }
    } on wtc.UndeclaredProcedureError catch (e) {
      errors.add(TypeError(
        'Undeclared procedure: ${e.functor}/${e.arity}',
        clause.line,
        clause.column,
        _clauseToString(clause),
      ));
    } catch (e) {
      // Catch other errors (type compilation failures, etc.)
      errors.add(TypeError(
        'Error checking clause: $e',
        clause.line,
        clause.column,
        _clauseToString(clause),
      ));
    }

    return errors;
  }

  // ===========================================================================
  // Contravariance Checking (Condition 2) - Structural Coverage
  // ===========================================================================

  /// Check that all input alternatives are covered by clause heads
  ///
  /// For input argument at position argIndex, we traverse the input type DFA
  /// and verify each transition (alternative) is covered by some clause.
  List<TypeError> _checkInputCoverage(
    List<ast.Clause> clauses,
    ProcDecl decl,
    int argIndex,
  ) {
    final errors = <TypeError>[];

    // Get the input type
    final argType = decl.argTypes[argIndex - 1];

    // Handle primitive/wildcard types (_ or _?)
    // Per spec v0.7: Wildcard types are FINAL STATES requiring NO coverage checking.
    // The type _? means "accept any consumed term" - it does NOT mean
    // "clauses must cover all possible terms".
    if (argType is PrimitiveModeAlt) {
      return errors;  // No coverage check needed for wildcards
    }

    // Handle named type references
    final typeRef = argType as TypeRef;

    // For input arguments, we need the complement automaton (T?)
    // to check what values the argument can receive
    final inputTypeName = typeRef.isInput ? '${typeRef.name}?' : typeRef.name;

    // Get automaton for the input type
    Automaton inputAutomaton;
    try {
      inputAutomaton = dfa.getAutomaton(inputTypeName);
    } catch (e) {
      errors.add(TypeError(
        'Cannot get automaton for type $inputTypeName: $e',
        decl.line,
        decl.column,
      ));
      return errors;
    }

    // Track visited states to handle recursive types
    final visited = <String>{};

    // Check coverage starting from the start state
    final coverageErrors = _checkStateCoverage(
      inputAutomaton.startState,
      clauses,
      argIndex,
      typeRef.name,
      visited,
      inputAutomaton,
      decl,
    );

    for (final coverageError in coverageErrors) {
      errors.add(TypeError(
        coverageError.toString(),
        decl.line,
        decl.column,
      ));
    }

    return errors;
  }

  /// Recursively check coverage at a DFA state, tracking structural path
  ///
  /// structPath: list of argument indices taken from root (e.g., [1, 2] means
  /// "arg 1 of root, then arg 2 of that substructure")
  List<CoverageError> _checkStateCoverage(
    DFAState state,
    List<ast.Clause> clauses,
    int argIndex,
    String pathPrefix,
    Set<String> visited,
    Automaton automaton,
    ProcDecl decl, {
    List<int> structPath = const [],
  }) {
    final errors = <CoverageError>[];

    // Prevent infinite recursion on cyclic types. Key on (state, structPath):
    // a type may be reached at several structural positions (e.g. a list head
    // vs. a nested field), and coverage at a state depends on which clause
    // subterms sit at that path, so each (state, path) is a distinct check.
    // Termination holds because structPath only grows by descending into a
    // constructor argument, and descent stops once clauses run out of depth or
    // a clause variable covers the position (finite clause terms bound depth).
    final visitKey = '${state.name}@$structPath';
    if (visited.contains(visitKey)) {
      return errors;
    }
    visited.add(visitKey);

    // Leaf states (primitives) don't need structural coverage
    // They're covered by variables matching the appropriate mode
    // Primitive states are _ or _?
    if (state.baseName == '_') {
      return errors;
    }

    // Final states also don't need further coverage
    if (state.isFinal) {
      return errors;
    }

    // Check if any clause has a VARIABLE at this path position
    // If so, the variable covers ALL alternatives - no need to check further
    if (_anyClauseHasVariableAtPath(clauses, argIndex, structPath)) {
      return errors;  // Variable covers everything
    }

    // Enumerate this state's alternatives from the automaton that OWNS it.
    // The type DFA is modular: each named type has its own automaton holding
    // only its one-level transitions, and a nested type reference resolves to
    // the referenced type's start state, whose alternatives live in that
    // type's automaton (not the caller's). Re-resolving per state is what
    // lets coverage cross type-reference boundaries into nested unions.
    Automaton stateAutomaton;
    try {
      stateAutomaton = dfa.getAutomaton(state.name);
    } catch (_) {
      stateAutomaton = automaton;
    }

    // Get all transitions (alternatives) from this state
    final transitions = _getLabeledTransitionsFromState(state, stateAutomaton);

    for (final (transLabel, targetState) in transitions) {
      // Coverage is an input-only (contravariant) property: it ranges over the
      // consumed portion of D. A produce-mode (↑) edge marks a mode inversion —
      // the subtree below it is an output the clause *produces*, not an input it
      // must *accept* — so coverage does not descend past it (Prop Input
      // Coverage; input paths traverse the consumed arguments). Constant edges
      // carry no mode and terminate at a leaf, so they are kept.
      if (transLabel.mode == Mode.produce) {
        continue;
      }

      final label = transLabel.toString();

      // Check if some clause accepts this transition at the current path
      if (_clauseAcceptsLabelAtPath(clauses, argIndex, structPath, label)) {
        // Recursively check the target state with extended path
        final newPath = '$pathPrefix → $label';
        // Extract arg index from symbol like "f(2,1)" or "\(2,1)" or "[|](2,1)"
        final argIdxFromLabel = _extractArgIndex(label);
        final newStructPath = argIdxFromLabel != null
            ? [...structPath, argIdxFromLabel]
            : structPath;
        final nestedErrors = _checkStateCoverage(
          targetState,
          clauses,
          argIndex,
          newPath,
          visited,
          automaton,
          decl,
          structPath: newStructPath,
        );
        errors.addAll(nestedErrors);
      } else {
        // No clause covers this alternative
        errors.add(CoverageError(
          procedure: decl.name,
          argIndex: argIndex,
          uncoveredLabel: label,
          path: '$pathPrefix → $label',
        ));
      }
    }

    return errors;
  }

  /// Check if any clause has a variable at the given structural path
  bool _anyClauseHasVariableAtPath(
    List<ast.Clause> clauses,
    int argIndex,
    List<int> structPath,
  ) {
    for (final clause in clauses) {
      if (argIndex > clause.head.args.length) continue;
      final topArg = clause.head.args[argIndex - 1];
      final termAtPath = _navigateToPath(topArg, structPath);
      if (termAtPath is ast.VarTerm || termAtPath is ast.UnderscoreTerm) {
        return true;  // Variable at this path covers all alternatives
      }
    }
    return false;
  }

  /// Check if any clause accepts the label at the given structural path
  bool _clauseAcceptsLabelAtPath(
    List<ast.Clause> clauses,
    int argIndex,
    List<int> structPath,
    String labelStr,
  ) {
    for (final clause in clauses) {
      if (argIndex > clause.head.args.length) continue;
      final topArg = clause.head.args[argIndex - 1];
      final termAtPath = _navigateToPath(topArg, structPath);

      if (termAtPath == null) continue;

      // Variable accepts anything
      if (termAtPath is ast.VarTerm || termAtPath is ast.UnderscoreTerm) {
        return true;
      }

      // Get labels from the term at this path
      final labels = wtc.getLabelsFromTerm(termAtPath);
      if (labels == null) {
        return true;  // null means wildcard
      }

      if (_labelsMatch(labels, labelStr)) {
        return true;
      }
    }
    return false;
  }

  /// Navigate into a term following a structural path
  ///
  /// structPath is a list of 1-based argument indices
  ast.Term? _navigateToPath(ast.Term term, List<int> structPath) {
    ast.Term? current = term;
    for (final idx in structPath) {
      if (current == null) return null;

      if (current is ast.StructTerm) {
        if (idx < 1 || idx > current.args.length) return null;
        current = current.args[idx - 1];
      } else if (current is ast.ListTerm && !current.isNil) {
        // List [H|T]: idx 1 is head, idx 2 is tail
        if (idx == 1) {
          current = current.head;
        } else if (idx == 2) {
          current = current.tail;
        } else {
          return null;
        }
      } else {
        // Can't navigate into constants, variables, or nil
        return null;
      }
    }
    return current;
  }

  /// Extract argument index from a path element symbol
  ///
  /// Symbols have format functor(arity,argIndex) optionally followed by a mode
  /// suffix ":↓"/":↑" — e.g. "f(2,1)", "\(2,1)", "[|](2,1)", "[|](2,1):↓".
  /// Type-internal transition labels carry the mode suffix (procedure labels do
  /// not), so the (arity,argIndex) group is not anchored to end-of-string.
  int? _extractArgIndex(String symbol) {
    // Match "(arity,argIndex)" allowing an optional trailing mode annotation.
    final match = RegExp(r'\((\d+),(\d+)\)(?::.*)?$').firstMatch(symbol);
    if (match != null) {
      return int.tryParse(match.group(2)!);
    }
    // For constants or other formats, no arg index
    return null;
  }

  /// Get all transitions from a state, keeping the TransitionLabel objects so
  /// callers can inspect the edge mode (coverage must not descend past a
  /// produce-mode mode inversion).
  List<(TransitionLabel, DFAState)> _getLabeledTransitionsFromState(
      DFAState state, Automaton automaton) {
    final result = <(TransitionLabel, DFAState)>[];

    for (final entry in automaton.transitions.entries) {
      final (fromState, label) = entry.key;
      if (fromState == state) {
        result.add((label, entry.value));
      }
    }

    return result;
  }

  /// Check if any clause accepts the given label at the argument position
  bool _clauseAcceptsLabel(List<ast.Clause> clauses, int argIndex, String labelStr) {
    for (final clause in clauses) {
      final acceptedLabels = wtc.getAcceptedLabels(clause, argIndex, typeEnv);

      // null means wildcard (variable) - accepts anything
      if (acceptedLabels == null) {
        return true;
      }

      // Check if clause explicitly matches this label
      if (_labelsMatch(acceptedLabels, labelStr)) {
        return true;
      }
    }

    return false;
  }

  /// Check if any of the accepted labels match the DFA transition label
  bool _labelsMatch(Set<String> acceptedLabels, String labelStr) {
    // Direct match
    if (acceptedLabels.contains(labelStr)) {
      return true;
    }

    // Handle list labels: [|](2,1) and [|](2,2) should match [|]
    if (labelStr.startsWith('[|](')) {
      if (acceptedLabels.contains('[|]')) {
        return true;
      }
    }

    // Handle nil: [] is both a label and constant
    if (labelStr == '[]') {
      return acceptedLabels.contains('[]');
    }

    // Handle DiffList labels: \(2,1) and \(2,2) should match \/2
    if (labelStr.startsWith(r'\(')) {
      // Extract arity from \(2,1) format -> 2
      final diffMatch = RegExp(r'\\\((\d+),').firstMatch(labelStr);
      if (diffMatch != null) {
        final arity = diffMatch.group(1)!;
        if (acceptedLabels.contains('\\/$arity')) {
          return true;
        }
      }
      // Also check raw \ or \\
      if (acceptedLabels.contains(r'\') || acceptedLabels.contains(r'\\')) {
        return true;
      }
    }

    // Handle functor labels: <functor>(<arity>,<argIndex>)[:mode] should match
    // <functor>/<arity>. The functor may be any symbol, including operator
    // functors like the tuple constructor "," (e.g. ",(2,1):↓" matches ",/2"),
    // so it is not restricted to word characters, and an optional trailing
    // mode annotation (":↓"/":↑") on type-internal labels is tolerated.
    final match =
        RegExp(r'^(.+)\((\d+),(\d+)\)(?::.*)?$').firstMatch(labelStr);
    if (match != null) {
      final functor = match.group(1)!;
      final arity = match.group(2)!;
      if (acceptedLabels.contains('$functor/$arity')) {
        return true;
      }
    }

    return false;
  }

  // ===========================================================================
  // Helper Functions
  // ===========================================================================

  /// Convert clause to string for error messages
  String _clauseToString(ast.Clause clause) {
    final head = '${clause.head.functor}(${clause.head.args.length} args)';
    if (clause.body == null || clause.body!.isEmpty) {
      return '$head.';
    }
    return '$head :- ${clause.body!.length} goals.';
  }
}

// =============================================================================
// Convenience functions for type checking
// =============================================================================

/// Type-check a parsed Module
///
/// This is the primary entry point for type checking.
/// The module should be parsed using the main parser.
///
/// If transformedProcedures is provided, uses those instead of module.procedures.
/// This allows running partial evaluation (defined guard expansion) before type checking.
///
/// If [ancestorScope] is provided, it is used as the base type environment
/// (root scope + ancestor self.glp definitions) instead of just the root scope.
/// See module_hierarchy.dart for how ancestor scopes are assembled.
/// If [collector] is provided (program mode), call-site instantiations of
/// parameterized procedures are recorded into it; the caller (program linker)
/// runs the cross-module instantiation closure itself. With no [collector]
/// (single-file/REPL), this function runs the closure over the module's own
/// clauses. A parameterized procedure is never checked under the wildcard `_`
/// declaration — only per concrete instantiation (typed-program.md "Programs
/// and Modules"); one never instantiated is not type-checked.
TypeCheckResult checkModule(ast.Module module, {List<ast.Procedure>? transformedProcedures, TypeEnvironment? ancestorScope, wtc.InstantiationCollector? collector, Set<String>? certifiedKeys, bool rejectUninstantiatedInspecting = true}) {
  // Boundary for Issue 19: a type referenced but not in scope (e.g. an agent
  // isolate loaded with an incomplete self.glp scope) makes buildProgramDFA
  // throw UnknownTypeError. Catch it here and surface it as a locatable type
  // diagnostic, so a missing-scope condition is a reported well-typing failure
  // rather than an unhandled error that kills the caller. (IGLP's isolate-side
  // catch is the backstop.)
  try {
    return _checkModuleImpl(module,
        transformedProcedures: transformedProcedures,
        ancestorScope: ancestorScope,
        collector: collector,
        certifiedKeys: certifiedKeys,
        rejectUninstantiatedInspecting: rejectUninstantiatedInspecting);
  } on UnknownTypeError catch (e) {
    return TypeCheckResult(
      [TypeError('Unresolved type: ${e.typeName}', e.line, e.column)],
      [],
    );
  }
}

TypeCheckResult _checkModuleImpl(ast.Module module, {List<ast.Procedure>? transformedProcedures, TypeEnvironment? ancestorScope, wtc.InstantiationCollector? collector, Set<String>? certifiedKeys, bool rejectUninstantiatedInspecting = true}) {
  // Build base environment first so we know all type names for expansion.
  // This avoids mistaking root scope type names for type parameters.
  final baseEnv = ancestorScope ?? buildRootScopeEnvironment();

  // Expand parameterized types to monomorphic equivalents before type checking.
  // Pass root scope/ancestor templates so downstream modules can expand references
  // to parameterized types defined in ancestor scopes.
  final expandedModule = expandParameterizedTypes(module,
      knownTypeNames: baseEnv.types.keys.toSet(),
      externalTemplates: baseEnv.typeTemplates);

  // Build type environment from expanded module (reuses baseEnv), carrying the
  // parameterized-type templates (root/ancestor + this module's own, which
  // expansion removed from the module) so the instantiation closure can
  // materialize types that arise only through one (e.g. Stream<Box<Msg>> from a
  // type-changing procedure). buildTypeEnvironment resolves their simple-alias
  // references, so a template expanded after this point cannot name an alias
  // the build erased.
  final typeEnv = buildTypeEnvironment(expandedModule,
      ancestorScope: baseEnv,
      typeTemplates: {
        for (final td in module.typeDefs)
          if (td.isParameterized) td.name: td,
      });

  // Extract clauses - from transformed procedures if provided, otherwise from module
  final clauses = <ast.Clause>[];
  final procedures = transformedProcedures ?? module.procedures;
  for (final proc in procedures) {
    clauses.addAll(proc.clauses);
  }

  // Program mode: the caller (program linker) supplies a collector and runs the
  // cross-module instantiation closure itself.
  if (collector != null) {
    final checker = TypeChecker(typeEnv, collector: collector);
    final result = checker.check(clauses);

    // Phase A (modular checking via abstract parameters), per module: certify
    // each parametric procedure that takes the abstract route, against this
    // module's own defining clauses. The certified keys accumulate into the
    // caller-supplied set so the program-level closure suppresses re-reporting.
    final clausesByKey = <String, List<ast.Clause>>{};
    for (final c in clauses) {
      clausesByKey
          .putIfAbsent('${c.head.functor}/${c.head.arity}', () => [])
          .add(c);
    }
    final cert =
        certifyParametricProcedures(typeEnv, (procKey) => clausesByKey[procKey]);
    certifiedKeys?.addAll(cert.certifiedKeys);
    return TypeCheckResult(
      [...result.errors, ...cert.errors],
      [...result.warnings, ...cert.warnings],
    );
  }

  // Single-file / REPL mode: there is no program linker to run the closure, so
  // run a self-contained per-instantiation check over this module's own
  // clauses. A parameterized procedure's body obligations are discharged at
  // each concrete instantiation inferred from a call site (closes the
  // polymorphic-polarity soundness gap, known-issues Issue 14); a procedure
  // that is never instantiated has no well-typing and is not checked.
  final localCollector = wtc.InstantiationCollector();
  final checker = TypeChecker(typeEnv, collector: localCollector);
  final result = checker.check(clauses);

  final errors = <TypeError>[...result.errors];
  final warnings = <TypeWarning>[...result.warnings];

  // Defining clauses for "name/arity" among this module's own clauses.
  final clausesByKey = <String, List<ast.Clause>>{};
  for (final c in clauses) {
    clausesByKey
        .putIfAbsent('${c.head.functor}/${c.head.arity}', () => [])
        .add(c);
  }

  // Phase A: modular checking via abstract parameters. Certify each parametric
  // procedure that takes the abstract route by checking it once against its
  // abstract instance; the certified keys are then suppressed in the closure.
  // See certifyParametricProcedures (typed-program.md "Modular Checking via
  // Abstract Parameters").
  final cert = certifyParametricProcedures(
    typeEnv,
    (procKey) => clausesByKey[procKey],
  );
  errors.addAll(cert.errors);
  warnings.addAll(cert.warnings);

  // Phase 2: re-check each instantiation's defining clauses against the concrete
  // monomorphic declaration it produces, closed under calls (calls inside an
  // instantiated body induce further instantiations) to a fixpoint, rejecting
  // polymorphic recursion. A parametric procedure certified by Phase A is not
  // re-reported here (its abstract verdict already stands by lem:parametricity),
  // but its instantiations are still traversed for callee discovery. See
  // checkInstantiationsClosed.
  final instResults = checkInstantiationsClosed(
    localCollector,
    (procKey) => clausesByKey[procKey],
    certifiedKeys: cert.certifiedKeys,
  );
  for (final ir in instResults) {
    errors.addAll(ir.result.errors);
    warnings.addAll(ir.result.warnings);
  }

  // A parameterized procedure that did NOT take the abstract route inspects a
  // type parameter (a functor/constant at a parameter position) or uses a
  // parameter as a type-definition alternative, so it has no well-typing of its
  // own and acquires one only per instantiation. Loaded standalone (single
  // file/REPL) with no collected instantiation, there is nothing to certify, so
  // it is rejected: the abstract-parameter route is the sole means of certifying
  // a parametric procedure outside a program (typed-program.md "Modular Checking
  // via Abstract Parameters", sec:abstract-parameters). There is no wildcard
  // fallback — checking it under the wildcard `_` declaration is unsound. Within
  // a program (program linker) an instantiation supplies the verdict; a callerless
  // procedure there goes unchecked, not rejected (typed-program.md "Programs and
  // Modules").
  // The linked-program check (program linker) passes rejectUninstantiatedInspecting
  // = false: it checks the whole program as one flattened module, where a
  // callerless parametric procedure goes unchecked, not rejected (typed-program.md
  // "Programs and Modules"). The standalone reject below is for single-file/REPL
  // loads only.
  final instantiatedKeys = <String>{
    for (final ir in instResults) ir.inst.procKey,
  };
  for (final entry in rejectUninstantiatedInspecting
      ? typeEnv.paramProcDecls.entries
      : const Iterable<MapEntry<String, ProcDecl>>.empty()) {
    final key = entry.key;
    if (cert.certifiedKeys.contains(key)) continue; // abstract route — verdict already given
    final cls = clausesByKey[key];
    if (cls == null || cls.isEmpty) continue; // defined outside this unit
    if (instantiatedKeys.contains(key)) continue; // checked per instantiation
    final decl = entry.value;
    errors.add(TypeError(
      'Parameterized procedure ${decl.name}/${decl.arity} inspects a type '
      'parameter (or uses a parameter as a type-definition alternative) and has '
      'no instantiation, so it has no standalone well-typing. Declare a concrete '
      'element type for the inspected argument, or load it within a program that '
      'instantiates it (typed-program.md "Modular Checking via Abstract '
      'Parameters").',
      decl.line,
      decl.column,
      '${decl.name}/${decl.arity}',
    ));
  }

  return TypeCheckResult(errors, warnings);
}

// =============================================================================
// Per-instantiation checking, closed under calls (clause-template rule)
// =============================================================================

/// The check result for one parameterized-procedure instantiation.
class InstantiationCheckResult {
  final wtc.CollectedInstantiation inst;
  final TypeCheckResult result;
  InstantiationCheckResult(this.inst, this.result);
}

/// A queued instantiation plus the active set: the parameterized procedures
/// being instantiated on the derivation path to it (procKey → monomorphic
/// declaration), used to enforce monomorphic recursion.
class _Pending {
  final wtc.CollectedInstantiation inst;
  final Map<String, ProcDecl> active;
  _Pending(this.inst, this.active);
}

/// The outcome of Phase A (modular checking via abstract parameters): the
/// verdict reported for the certified parametric procedures, plus the set of
/// procedure keys that were certified (so the per-instantiation closure can
/// suppress re-reporting them — lem:parametricity carries the abstract-instance
/// verdict to every instantiation).
class ParametricCertification {
  final List<TypeError> errors;
  final List<TypeWarning> warnings;
  final Set<String> certifiedKeys;
  ParametricCertification(this.errors, this.warnings, this.certifiedKeys);
}

/// Phase A — modular checking via abstract parameters
/// (typed-program.md "Modular Checking via Abstract Parameters",
/// sec:abstract-parameters / lem:parametricity).
///
/// For each parameterized procedure declared in [typeEnv], decide its route
/// structurally and, when it takes the abstract route, check it once against its
/// *abstract instance* (each type parameter replaced by a distinct zero-alternative
/// abstract type):
///
///  - inspects a parameter (a functor/constant at a parameter position) OR uses a
///    parameter as a type-definition top-level alternative → per-instantiation
///    route: not decided here; the closure checks each concrete instantiation.
///  - otherwise → abstract route: a FULL check (covariance + input coverage)
///    against the abstract instance, run by seeding it into the per-instantiation
///    closure so body-induced types are materialized and monomorphic recursion is
///    enforced. Only the seeded instantiation's verdict is reported; the callee
///    instantiations it induces are filtered out (they belong to the program
///    closure / their own certification). The key is certified whether the check
///    passes or fails (Decision 1: the abstract route is a commitment), so the
///    closure does not re-check it at each instantiation (lem:parametricity).
///
/// [definingClauses] returns the defining clauses for a "name/arity", or null if
/// the procedure is defined outside the checked unit (then it is not certified
/// here).
ParametricCertification certifyParametricProcedures(
  TypeEnvironment typeEnv,
  List<ast.Clause>? Function(String procKey) definingClauses,
) {
  final errors = <TypeError>[];
  final warnings = <TypeWarning>[];
  final certified = <String>{};
  final templates = typeEnv.typeTemplates;
  final knownMono = typeEnv.types.keys.toSet();

  for (final entry in typeEnv.paramProcDecls.entries) {
    final key = entry.key;
    final paramDecl = entry.value;
    final clauses = definingClauses(key);
    if (clauses == null || clauses.isEmpty) {
      continue; // defined outside the checked unit (or never defined)
    }

    // Structural routing: a parameter-inspecting clause or a parameter used as a
    // type-definition alternative routes to the per-instantiation closure.
    if (procInspectsParameter(clauses, paramDecl, paramDecl.typeParams, templates) ||
        paramUsedAsTypeAlternative(paramDecl, templates)) {
      continue;
    }

    // Abstract route: build the abstract instance and check it by seeding it
    // into the per-instantiation closure. The closure materializes any
    // body-induced types (e.g. a type-changing recursive call producing
    // Stream<Box<$abstract_X>>) across rounds and applies monomorphic-recursion
    // checking, so a duality clash that surfaces only against a materialized type
    // is caught — which a single-shot check would miss. We report ONLY the seeded
    // instantiation's own verdict; the callee instantiations it induces belong to
    // the program closure (and to their own certification), so they are filtered
    // out here. Per Decision 1, the abstract route is a commitment: if the
    // abstract instance fails, the procedure is rejected regardless of whether it
    // is ever instantiated. The key is certified either way, so the main closure
    // never re-reports it (lem:parametricity carries the verdict to every
    // instantiation).
    final ai = buildAbstractInstance(paramDecl, paramDecl.typeParams, templates,
        knownMonoTypes: knownMono);
    final aiEnv = TypeEnvironment(
      {...typeEnv.types, ...ai.typeDefs},
      {...typeEnv.procedures, ai.decl.key: ai.decl},
      paramProcDecls: typeEnv.paramProcDecls,
      typeTemplates: typeEnv.typeTemplates,
    );
    final inst = wtc.CollectedInstantiation(
        key, ai.decl, aiEnv, buildProgramDFA(aiEnv));
    final seed = wtc.InstantiationCollector();
    seed.record(inst);
    final results = checkInstantiationsClosed(seed, definingClauses);
    final seedSigKey = '${inst.procKey}#${inst.signature}';
    for (final r in results) {
      if ('${r.inst.procKey}#${r.inst.signature}' == seedSigKey) {
        errors.addAll(r.result.errors);
        warnings.addAll(r.result.warnings);
      }
    }
    certified.add(key);
  }

  return ParametricCertification(errors, warnings, certified);
}

/// Close the parameterized-procedure instantiation set under calls and check
/// each instantiation's defining clauses.
///
/// Per the typed-program spec (Parameterized Procedure Declarations): the
/// instantiations of a program form the least set containing the instantiation
/// of every call in a monomorphic clause and closed under induction — a call in
/// an instantiated parameterized body induces a further instantiation. This
/// re-checks each instantiation WITH a fresh collector, so calls in its
/// instantiated body record their own instantiations, which are enqueued and
/// checked until the set is closed (a fixpoint).
///
/// Termination: the types reachable from a program are finite (enforced at the
/// type-parsing/expansion stage — a parameterised type referring to itself may
/// not have a parameter as a proper subterm of an argument; see
/// expandParameterizedTypes), so the instantiation set is finite. No growth
/// guard is needed here.
///
/// [seed] holds the base instantiations collected over the monomorphic clauses.
/// [definingClauses] returns the defining clauses for a "name/arity", or null if
/// the procedure is defined outside the checked unit (e.g. the root scope), in
/// which case it is not re-checked here. Returns the per-instantiation results
/// in dequeue order.
List<InstantiationCheckResult> checkInstantiationsClosed(
  wtc.InstantiationCollector seed,
  List<ast.Clause>? Function(String procKey) definingClauses, {
  Set<String> certifiedKeys = const {},
}) {
  // Types that arise only through the closure (e.g. Stream<Box<Msg>> from a
  // type-changing procedure) are not produced by the initial declaration-driven
  // expansion. They are materialized here and accumulated across rounds. A round
  // that discovers new such types re-runs the whole closure, so any
  // instantiation checked before the type existed is re-checked against the
  // complete DFA. Monomorphic recursion keeps the type set finite, so this
  // converges (a round adding no new type is the fixpoint).
  final extraTypes = <String, TypeDef>{};

  while (true) {
    final results = <InstantiationCheckResult>[];
    final processed = <String>{}; // "procKey#signature"
    final needed = <String>{}; // expanded type names referenced but not present
    Map<String, TypeDef>? templates;
    Set<String> knownTypes = const {}; // a representative env's type names
    final queue = <_Pending>[
      for (final inst in seed.all) _Pending(inst, const {}),
    ];

    void noteNeeded(ProcDecl decl, TypeEnvironment env) {
      for (final t in decl.argTypes) {
        var n = wtc.getFullTypeName(t);
        if (n.endsWith('?')) n = n.substring(0, n.length - 1);
        if (n.contains('<') &&
            !env.types.containsKey(n) &&
            !extraTypes.containsKey(n)) {
          needed.add(n);
        }
      }
    }

    while (queue.isNotEmpty) {
      final pending = queue.removeAt(0);
      final inst = pending.inst;
      final sigKey = '${inst.procKey}#${inst.signature}';
      if (!processed.add(sigKey)) continue;

      templates ??= inst.env.typeTemplates;
      if (knownTypes.isEmpty) knownTypes = inst.env.types.keys.toSet();
      noteNeeded(inst.monoDecl, inst.env);

      final defining = definingClauses(inst.procKey);
      if (defining == null) continue; // defined outside the checked unit

      // Defer this instantiation if its own declaration references types not yet
      // materialized in this round (noted above). Checking it now would fault on
      // the missing automaton; it is re-induced and checked next round, once the
      // type has been materialized.
      final declTypesPresent = inst.monoDecl.argTypes.every((t) {
        var n = wtc.getFullTypeName(t);
        if (n.endsWith('?')) n = n.substring(0, n.length - 1);
        return !n.contains('<') ||
            inst.env.types.containsKey(n) ||
            extraTypes.containsKey(n);
      });
      if (!declTypesPresent) continue;

      // Focused env augmented with types materialized so far this fixpoint.
      final focusedEnv = TypeEnvironment(
        {...inst.env.types, ...extraTypes},
        {...inst.env.procedures, inst.monoDecl.key: inst.monoDecl},
        paramProcDecls: inst.env.paramProcDecls,
        typeTemplates: inst.env.typeTemplates,
      );

      // This instantiation is on the current cycle: a recursive (self or mutual)
      // call to it is checked at this instantiation, not re-inferred. Recursion
      // therefore records no new instantiation into [sub].
      final active = {...pending.active, inst.procKey: inst.monoDecl};
      final sub = wtc.InstantiationCollector();
      final res = TypeChecker(focusedEnv, collector: sub).checkSingleProcedure(
          inst.monoDecl, defining,
          activeInstantiations: active);
      // A parametric procedure certified by Phase A (abstract-instance check) is
      // well-typed at every instantiation by lem:parametricity, so its concrete
      // instantiation is not re-reported here; its body is still traversed so the
      // instantiations its calls induce are discovered and checked below.
      if (!certifiedKeys.contains(inst.procKey)) {
        results.add(InstantiationCheckResult(inst, res));
      }

      // Enqueue the instantiations this body induces (recorded into [sub]),
      // noting any types they reference that still need materializing. They
      // carry the extended active set so mutual recursion is detected downstream.
      for (final next in sub.all) {
        noteNeeded(next.monoDecl, next.env);
        if (!processed.contains('${next.procKey}#${next.signature}')) {
          queue.add(_Pending(next, active));
        }
      }
    }

    if (needed.isEmpty || templates == null) return results;
    final have = {...knownTypes, ...extraTypes.keys};
    final newDefs = materializeInstantiations(needed, templates, have);
    if (newDefs.isEmpty) return results; // nothing further can be materialized
    extraTypes.addAll(newDefs);
    // loop: re-run the closure with the enlarged type set.
  }
}

/// Parse and type-check GLP source code
///
/// Convenience function that parses source and runs type checker.
TypeCheckResult checkSource(String source) {
  // Parse using main parser
  final lexer = Lexer(source);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final module = parser.parseModule();

  // Type check the module
  return checkModule(module);
}
