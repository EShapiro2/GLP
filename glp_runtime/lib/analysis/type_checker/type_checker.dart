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

  TypeChecker(this.typeEnv) : dfa = buildProgramDFA(typeEnv);

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
  List<TypeError> _checkClauseCovariance(ast.Clause clause, ProcDecl decl) {
    final errors = <TypeError>[];

    try {
      final result = wtc.checkClauseFromAst(clause, dfa, typeEnv);

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

    // Prevent infinite recursion on recursive types
    if (visited.contains(state.name)) {
      return errors;
    }
    visited.add(state.name);

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

    // Get all transitions (alternatives) from this state
    final transitions = _getTransitionsFromState(state, automaton);

    for (final entry in transitions.entries) {
      final label = entry.key;
      final targetState = entry.value;

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
  /// Symbols like "f(2,1)" or "\(2,1)" or "[|](2,1)" have format functor(arity,argIndex)
  int? _extractArgIndex(String symbol) {
    // Try pattern like "name(arity,argIndex)"
    final match = RegExp(r'\((\d+),(\d+)\)$').firstMatch(symbol);
    if (match != null) {
      return int.tryParse(match.group(2)!);
    }
    // For constants or other formats, no arg index
    return null;
  }

  /// Get all transitions from a state
  Map<String, DFAState> _getTransitionsFromState(DFAState state, Automaton automaton) {
    final result = <String, DFAState>{};

    for (final entry in automaton.transitions.entries) {
      final (fromState, label) = entry.key;
      if (fromState == state) {
        // Convert TransitionLabel to string for label matching
        result[label.toString()] = entry.value;
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
/// (prelude + ancestor self.glp definitions) instead of just the prelude.
/// See module_hierarchy.dart for how ancestor scopes are assembled.
TypeCheckResult checkModule(ast.Module module, {List<ast.Procedure>? transformedProcedures, TypeEnvironment? ancestorScope}) {
  // Build base environment first so we know all type names for expansion.
  // This avoids mistaking prelude type names for type parameters.
  final baseEnv = ancestorScope ?? buildPreludeEnvironment();

  // Expand parameterized types to monomorphic equivalents before type checking.
  // Pass prelude/ancestor templates so downstream modules can expand references
  // to parameterized types defined in ancestor scopes.
  final expandedModule = expandParameterizedTypes(module,
      knownTypeNames: baseEnv.types.keys.toSet(),
      externalTemplates: baseEnv.typeTemplates);

  // Build type environment from expanded module (reuses baseEnv)
  final typeEnv = buildTypeEnvironment(expandedModule, ancestorScope: baseEnv);

  // Extract clauses - from transformed procedures if provided, otherwise from module
  final clauses = <ast.Clause>[];
  final procedures = transformedProcedures ?? module.procedures;
  for (final proc in procedures) {
    clauses.addAll(proc.clauses);
  }

  // Run type checker
  final checker = TypeChecker(typeEnv);
  return checker.check(clauses);
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
