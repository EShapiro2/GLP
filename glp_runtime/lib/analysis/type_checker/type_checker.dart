// lib/analysis/type_checker/type_checker.dart
//
// Main type checker implementing the Yardeni-Shapiro algorithm.
// Checks that programs are well-typed according to declared procedure types.
//
// A program P is well-typed by S iff:
// 1. No clause is useless relative to S
// 2. T_P^α(S) = S (S is a fixpoint of the abstract consequence operator)

import 'type_ast.dart';
import 'type_dfa.dart';
import 'type_compiler.dart';
import 'type_parser.dart';
import '../../compiler/ast.dart' as ast;

/// Result of type checking
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

/// A type warning (e.g., useless clause)
class TypeWarning {
  final String message;
  final int line;
  final int column;
  
  TypeWarning(this.message, this.line, this.column);
  
  @override
  String toString() => '$message at line $line, column $column';
}

/// The main type checker
class TypeChecker {
  final TypeEnvironment typeEnv;
  final TypeCompiler compiler;
  
  TypeChecker(this.typeEnv) : compiler = TypeCompiler(typeEnv);
  
  /// Check a program (list of clauses) against declared types
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
          procDecl.line, procDecl.column
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
          firstClause.line, firstClause.column
        ));
      }
    }
    
    return TypeCheckResult(errors, warnings);
  }
  
  /// Check a single procedure against its declared type
  TypeCheckResult _checkProcedure(ProcDecl decl, List<ast.Clause> clauses) {
    final errors = <TypeError>[];
    final warnings = <TypeWarning>[];
    
    // Compile argument types to DFAs
    final argDFAs = <TypeDFA>[];
    for (final argType in decl.argTypes) {
      try {
        argDFAs.add(compiler.compile(argType.name));
      } catch (e) {
        errors.add(TypeError(
          'Cannot compile type ${argType.name}: $e',
          argType.line, argType.column
        ));
        return TypeCheckResult(errors, warnings);
      }
    }
    
    // Track clause contributions for fixpoint check
    final clauseContributions = <ClauseContribution>[];
    
    for (final clause in clauses) {
      final clauseResult = _checkClause(clause, decl, argDFAs);
      
      errors.addAll(clauseResult.errors);
      warnings.addAll(clauseResult.warnings);
      
      if (clauseResult.contribution != null) {
        clauseContributions.add(clauseResult.contribution!);
      }
    }
    
    // Fixpoint check: union of contributions should equal declared type
    // This is a simplified check - full implementation would compute T_P^α
    if (clauseContributions.isEmpty && clauses.isNotEmpty) {
      errors.add(TypeError(
        'All clauses for ${decl.name}/${decl.arity} are useless',
        decl.line, decl.column
      ));
    }
    
    return TypeCheckResult(errors, warnings);
  }
  
  /// Check a single clause against procedure type
  ClauseCheckResult _checkClause(ast.Clause clause, ProcDecl decl, List<TypeDFA> argDFAs) {
    final errors = <TypeError>[];
    final warnings = <TypeWarning>[];

    // Step 1: Check head arguments match declared types
    if (clause.head.args.length != decl.arity) {
      errors.add(TypeError(
        'Clause head arity ${clause.head.args.length} does not match declaration ${decl.arity}',
        clause.line, clause.column, clause.toString()
      ));
      return ClauseCheckResult(errors, warnings, null);
    }

    // Step 2: Infer types for variables from head patterns
    final varTypes = <String, TypeDFA>{};

    for (int i = 0; i < clause.head.args.length; i++) {
      final arg = clause.head.args[i];
      final argDFA = argDFAs[i];

      // Check ground parts of argument against type
      final groundCheck = _checkGroundPaths(arg, argDFA);
      if (!groundCheck.success) {
        errors.add(TypeError(
          'Argument ${i + 1} ground path not in type ${decl.argTypes[i].name}: ${groundCheck.failedPath}',
          clause.line, clause.column, clause.toString()
        ));
      }

      // Collect variable types from pattern
      _inferVariableTypes(arg, argDFA, varTypes, TermPath.empty());
    }

    // Step 3: Check guards
    if (clause.guards != null) {
      for (final guard in clause.guards!) {
        // TODO: Type check guard arguments
        // Need procedure type declarations for guards
        guard; // Suppress unused warning
      }
    }

    // Step 4: Check body goals use variables consistently with inferred types
    for (final goal in (clause.body ?? [])) {
      final goalCheck = _checkGoal(goal, varTypes);
      errors.addAll(goalCheck.errors);
    }
    
    // Step 4: If no errors, compute clause contribution
    ClauseContribution? contribution;
    if (errors.isEmpty) {
      contribution = ClauseContribution(clause, varTypes);
    } else {
      warnings.add(TypeWarning(
        'Clause is useless (has type errors)',
        clause.line, clause.column
      ));
    }
    
    return ClauseCheckResult(errors, warnings, contribution);
  }
  
  /// Extract ground paths from an AST term
  Set<TermPath> _extractGroundPaths(ast.Term term) {
    if (term is ast.VarTerm || term is ast.UnderscoreTerm) {
      return {};  // Variables have no ground paths
    } else if (term is ast.StructTerm) {
      final paths = <TermPath>{};
      for (int i = 0; i < term.args.length; i++) {
        final argPaths = _extractGroundPaths(term.args[i]);
        for (final p in argPaths) {
          paths.add(TermPath([PathElement.functor(term.functor, term.arity, i + 1), ...p.elements]));
        }
      }
      return paths;
    } else if (term is ast.ListTerm) {
      if (term.isNil) {
        return {TermPath([PathElement.nil()])};
      }
      final paths = <TermPath>{};
      if (term.head != null) {
        for (final p in _extractGroundPaths(term.head!)) {
          paths.add(TermPath([PathElement.listHead(), ...p.elements]));
        }
      }
      if (term.tail != null) {
        for (final p in _extractGroundPaths(term.tail!)) {
          paths.add(TermPath([PathElement.listTail(), ...p.elements]));
        }
      }
      return paths;
    } else if (term is ast.ConstTerm) {
      if (term.value != null) {
        return {TermPath([PathElement.constant(term.value!)])};
      }
    }
    return {};
  }

  /// Check that ground paths in a term are accepted by the DFA
  GroundPathCheck _checkGroundPaths(ast.Term term, TypeDFA dfa) {
    // Use type-aware extraction that stops at Any positions
    final paths = _extractTypedGroundPaths(term, dfa, TermPath.empty());
    for (final path in paths) {
      if (!dfa.acceptsPath(path)) {
        return GroundPathCheck(false, path);
      }
    }
    return GroundPathCheck(true, null);
  }

  /// Extract ground paths from term, consulting type to know when to stop
  /// When we reach a position typed as Any, we accept it without descending further
  Set<TermPath> _extractTypedGroundPaths(ast.Term term, TypeDFA dfa, TermPath currentPath) {
    // Check if current position is typed as Any
    final state = dfa.stateAfterPath(currentPath);
    if (state != null && state.name.startsWith('_builtin_Any')) {
      // This position accepts Any - don't descend further
      // Return empty set since we don't need to check sub-paths
      return {};
    }

    // For variables, no ground paths
    if (term is ast.VarTerm || term is ast.UnderscoreTerm) {
      return {};
    }

    // For structures, extract paths from arguments
    if (term is ast.StructTerm) {
      final paths = <TermPath>{};
      for (int i = 0; i < term.args.length; i++) {
        final elemPath = PathElement.functor(term.functor, term.arity, i + 1);
        final newPath = TermPath([...currentPath.elements, elemPath]);
        final argPaths = _extractTypedGroundPaths(term.args[i], dfa, newPath);
        paths.addAll(argPaths);
      }
      return paths;
    }

    // For lists, extract from head and tail
    if (term is ast.ListTerm) {
      if (term.isNil) {
        return {TermPath([...currentPath.elements, PathElement.nil()])};
      }
      final paths = <TermPath>{};
      if (term.head != null) {
        final headPath = TermPath([...currentPath.elements, PathElement.listHead()]);
        final headPaths = _extractTypedGroundPaths(term.head!, dfa, headPath);
        paths.addAll(headPaths);
      }
      if (term.tail != null) {
        final tailPath = TermPath([...currentPath.elements, PathElement.listTail()]);
        final tailPaths = _extractTypedGroundPaths(term.tail!, dfa, tailPath);
        paths.addAll(tailPaths);
      }
      return paths;
    }

    // For constants
    if (term is ast.ConstTerm && term.value != null) {
      return {TermPath([...currentPath.elements, PathElement.constant(term.value!)])};
    }

    return {};
  }

  /// Infer variable types from a pattern and accumulate in varTypes map
  void _inferVariableTypes(
    ast.Term term,
    TypeDFA dfa,
    Map<String, TypeDFA> varTypes,
    TermPath pathToHere
  ) {
    if (term is ast.VarTerm) {
      // Variable at this position gets the type reachable from current DFA state
      final state = dfa.stateAfterPath(pathToHere);
      if (state != null) {
        final varName = term.name;
        // Intersect with existing type for this variable (if any)
        if (varTypes.containsKey(varName)) {
          varTypes[varName] = varTypes[varName]!.intersect(
            _dfaFromState(dfa, state)
          );
        } else {
          varTypes[varName] = _dfaFromState(dfa, state);
        }
      }
    } else if (term is ast.StructTerm) {
      for (int i = 0; i < term.args.length; i++) {
        final newPath = pathToHere.append(
          PathElement.functor(term.functor, term.arity, i + 1)
        );
        _inferVariableTypes(term.args[i], dfa, varTypes, newPath);
      }
    } else if (term is ast.ListTerm) {
      if (!term.isNil) {
        if (term.head != null) {
          _inferVariableTypes(term.head!, dfa, varTypes,
            pathToHere.append(PathElement.listHead()));
        }
        if (term.tail != null) {
          _inferVariableTypes(term.tail!, dfa, varTypes,
            pathToHere.append(PathElement.listTail()));
        }
      }
    }
    // Constants and UnderscoreTerm have no variables
  }
  
  /// Create a DFA that accepts paths reachable from a given state
  TypeDFA _dfaFromState(TypeDFA originalDfa, DFAState fromState) {
    // This creates a DFA with the given state as start state
    return TypeDFA(
      states: originalDfa.states,
      startState: fromState,
      finalStates: originalDfa.finalStates,
      transitions: originalDfa.transitions,
    );
  }
  
  /// Check that a body goal uses variables consistently with their types
  GoalCheckResult _checkGoal(ast.Goal goal, Map<String, TypeDFA> varTypes) {
    final errors = <TypeError>[];

    // For procedure calls, we'd check that argument types match
    // For now, this is a placeholder

    return GoalCheckResult(errors);
  }
}

/// Result of checking ground paths
class GroundPathCheck {
  final bool success;
  final TermPath? failedPath;
  
  GroundPathCheck(this.success, this.failedPath);
}

/// Result of checking a single clause
class ClauseCheckResult {
  final List<TypeError> errors;
  final List<TypeWarning> warnings;
  final ClauseContribution? contribution;
  
  ClauseCheckResult(this.errors, this.warnings, this.contribution);
}

/// What a clause contributes to T_P^α(S)
class ClauseContribution {
  final ast.Clause clause;
  final Map<String, TypeDFA> variableTypes;

  ClauseContribution(this.clause, this.variableTypes);
}

/// Result of checking a body goal
class GoalCheckResult {
  final List<TypeError> errors;
  
  GoalCheckResult(this.errors);
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
