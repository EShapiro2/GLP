// lib/analysis/type_checker/type_checker.dart
//
// Main type checker implementing GLP well-typed program checking.
// Specification: docs/modules/well-typed-program.md v0.5
// Paper Reference: Definition 4.10 (lines 351-357)
//
// A typed GLP program P = (Cs, D) is well-typed if:
// 1. Covariance: Every clause C ∈ Cs is well-typed by D
// 2. Contravariance: Every input path in every procedure type in D has
//    a clause C ∈ Cs that accepts it

import 'type_ast.dart';
import 'type_dfa.dart';
import 'type_compiler.dart';
import 'type_parser.dart';
import 'well_typed_clause.dart' as wtc;
import '../../compiler/ast.dart' as ast;

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
  final TypeCompiler compiler;

  TypeChecker(this.typeEnv) : compiler = TypeCompiler(typeEnv);

  /// Check a program (list of clauses) against declared types
  ///
  /// Implements Definition 4.10:
  /// 1. Covariance: Every clause is well-typed
  /// 2. Contravariance: Every input path is covered
  TypeCheckResult check(List<ast.Clause> clauses) {
    final errors = <TypeError>[];
    final warnings = <TypeWarning>[];

    // Group clauses by procedure (name/arity)
    final procedureClauses = <String, List<ast.Clause>>{};
    for (final clause in clauses) {
      final key = '${clause.head.functor}/${clause.head.arity}';
      procedureClauses.putIfAbsent(key, () => []).add(clause);
    }

    // Check each declared procedure
    for (final procDecl in typeEnv.procedures.values) {
      final key = procDecl.key;
      final procClauses = procedureClauses[key];

      if (procClauses == null || procClauses.isEmpty) {
        warnings.add(TypeWarning(
          'Procedure ${procDecl.name}/${procDecl.arity} declared but not defined',
          procDecl.line,
          procDecl.column,
        ));
        continue;
      }

      // Check this procedure
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

  /// Check a single procedure against its declared type
  TypeCheckResult _checkProcedure(ProcDecl decl, List<ast.Clause> clauses) {
    final errors = <TypeError>[];
    final warnings = <TypeWarning>[];

    // =======================================================================
    // Condition 1: Covariance — check each clause is well-typed
    // =======================================================================
    for (final clause in clauses) {
      final clauseErrors = _checkClauseCovariance(clause, decl);
      errors.addAll(clauseErrors);
    }

    // =======================================================================
    // Condition 2: Contravariance — check input coverage
    // =======================================================================
    for (int argIndex = 1; argIndex <= decl.arity; argIndex++) {
      final argType = decl.argTypes[argIndex - 1];
      if (argType.isInput) {
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
  List<TypeError> _checkClauseCovariance(ast.Clause clause, ProcDecl decl) {
    final errors = <TypeError>[];

    try {
      final result = wtc.checkClauseFromAst(clause, typeEnv);

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

    // Compile base type to DFA
    TypeDFA inputDFA;
    try {
      inputDFA = compiler.compile(argType.name);
    } catch (e) {
      errors.add(TypeError(
        'Cannot compile type ${argType.name}: $e',
        decl.line,
        decl.column,
      ));
      return errors;
    }

    // Track visited states to handle recursive types
    final visited = <String>{};

    // Check coverage starting from the start state
    final coverageErrors = _checkStateCoverage(
      inputDFA.startState,
      clauses,
      argIndex,
      argType.name,
      visited,
      inputDFA,
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

  /// Recursively check coverage at a DFA state
  List<CoverageError> _checkStateCoverage(
    DFAState state,
    List<ast.Clause> clauses,
    int argIndex,
    String pathPrefix,
    Set<String> visited,
    TypeDFA dfa,
    ProcDecl decl,
  ) {
    final errors = <CoverageError>[];

    // Prevent infinite recursion on recursive types
    if (visited.contains(state.name)) {
      return errors;
    }
    visited.add(state.name);

    // Leaf states (primitives) don't need structural coverage
    // They're covered by variables matching the appropriate mode
    if (dfa.isPrimitiveState(state)) {
      return errors;
    }

    // Final states also don't need further coverage
    if (dfa.finalStates.contains(state)) {
      return errors;
    }

    // Get all transitions (alternatives) from this state
    final transitions = _getTransitionsFromState(state, dfa);

    for (final entry in transitions.entries) {
      final pathElem = entry.key;
      final targetState = entry.value;
      final labelStr = pathElem.symbol;

      // Check if some clause accepts this transition at this argument position
      if (_clauseAcceptsLabel(clauses, argIndex, labelStr)) {
        // Recursively check the target state
        final newPath = '$pathPrefix → $labelStr';
        final nestedErrors = _checkStateCoverage(
          targetState,
          clauses,
          argIndex,
          newPath,
          visited,
          dfa,
          decl,
        );
        errors.addAll(nestedErrors);
      } else {
        // No clause covers this alternative
        errors.add(CoverageError(
          procedure: decl.name,
          argIndex: argIndex,
          uncoveredLabel: labelStr,
          path: '$pathPrefix → $labelStr',
        ));
      }
    }

    return errors;
  }

  /// Get all transitions from a state
  Map<PathElement, DFAState> _getTransitionsFromState(DFAState state, TypeDFA dfa) {
    final result = <PathElement, DFAState>{};

    for (final entry in dfa.transitions.entries) {
      final (fromState, label) = entry.key;
      if (fromState == state) {
        result[label] = entry.value;
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

    // Handle functor labels: f(2,1) should match f/2
    final match = RegExp(r'(\w+)\((\d+),').firstMatch(labelStr);
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
// Convenience function for checking GLP source
// =============================================================================

/// Parse and type-check GLP source code
TypeCheckResult checkSource(String source, List<ast.Clause> clauses) {
  // Parse type declarations from source
  final typeEnv = parseTypes(source);

  // Run type checker
  final checker = TypeChecker(typeEnv);
  return checker.check(clauses);
}
