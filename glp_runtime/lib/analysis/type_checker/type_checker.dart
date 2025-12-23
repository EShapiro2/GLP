// lib/analysis/type_checker/type_checker.dart
//
// Main type checker implementing the Yardeni-Shapiro algorithm.
// Checks that programs are well-typed according to declared procedure types.
//
// A program P is well-typed by S iff:
// 1. No clause is useless relative to S
// 2. T_P^α(S) = S (S is a fixpoint of the abstract consequence operator)

import 'mode.dart';
import 'type_ast.dart';
import 'type_dfa.dart';
import 'type_compiler.dart';
import 'type_parser.dart';
import 'mode_checker.dart';
import 'mode_error.dart';
import 'clause_contribution.dart';
import 'guard_types.dart';
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
  final ModeChecker modeChecker;

  TypeChecker(this.typeEnv)
      : compiler = TypeCompiler(typeEnv),
        modeChecker = ModeChecker(typeEnv);
  
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

    // Mode checking split into two parts:
    // 1. Per-variable mode checking: handled in _inferVariableTypes() via primitiveStateModes
    // 2. Mode coverage checking: handled here (ensures ::= types with primitive modes are covered)
    //
    // Coverage checking only applies to ::= types (exact), not ::< types (subtype).
    // Every ::= _ ; _? requires coverage, Any ::< Every does not.
    final modeCoverageErrors = modeChecker.checkModeCoverageOnly(decl.name, decl.arity, clauses);
    for (final modeError in modeCoverageErrors) {
      errors.add(TypeError(
        modeError.message,
        modeError.line,
        modeError.column,
      ));
    }

    // Fixpoint check: compute T_P^α(S) and verify it equals S
    // T_P^α(S) = tuple-distributive closure = union of clause contributions per argument
    if (clauseContributions.isEmpty) {
      if (clauses.isNotEmpty) {
        errors.add(TypeError(
          'All clauses for ${decl.name}/${decl.arity} are useless',
          decl.line, decl.column
        ));
      }
      return TypeCheckResult(errors, warnings);
    }

    // Create contribution computer
    final contributionComputer = ClauseContributionComputer(typeEnv);

    // Compute DFA contributions for each argument position across all clauses
    for (int argIndex = 0; argIndex < decl.arity; argIndex++) {
      // Input-mode arguments receive values from callers, not from clauses.
      // Skip the "produces values" check for input positions.
      if (decl.argTypes[argIndex].isInput) {
        continue;
      }

      // Union all clause contributions for this argument position
      var inferredDFA = TypeDFA.empty();

      for (final contribution in clauseContributions) {
        final clause = contribution.clause;
        final varTypes = contribution.variableTypes;

        if (argIndex < clause.head.args.length) {
          final argPattern = clause.head.args[argIndex];
          final argContribution = contributionComputer.computeArgContribution(
            argPattern,
            varTypes,
          );
          inferredDFA = inferredDFA.union(argContribution);
        }
      }

      // Check if inferred equals declared
      final declaredDFA = argDFAs[argIndex];

      if (!inferredDFA.isEquivalent(declaredDFA)) {
        // Diagnose the type of mismatch
        if (inferredDFA.isEmpty && !declaredDFA.isEmpty) {
          errors.add(TypeError(
            'Procedure ${decl.name}/${decl.arity} argument ${argIndex + 1}: '
            'no clauses produce values for this argument',
            decl.line, decl.column
          ));
        } else if (inferredDFA.isSubsetOf(declaredDFA)) {
          // Inferred ⊂ Declared: incomplete definition
          errors.add(TypeError(
            'Procedure ${decl.name}/${decl.arity} argument ${argIndex + 1}: '
            'clauses do not cover full declared type ${decl.argTypes[argIndex].name} (incomplete definition)',
            decl.line, decl.column
          ));
        } else if (declaredDFA.isSubsetOf(inferredDFA)) {
          // Declared ⊂ Inferred: produces values outside type
          errors.add(TypeError(
            'Procedure ${decl.name}/${decl.arity} argument ${argIndex + 1}: '
            'clauses produce values outside declared type ${decl.argTypes[argIndex].name}',
            decl.line, decl.column
          ));
        } else {
          // Neither is subset of other
          errors.add(TypeError(
            'Procedure ${decl.name}/${decl.arity} argument ${argIndex + 1}: '
            'inferred type does not match declared type ${decl.argTypes[argIndex].name}',
            decl.line, decl.column
          ));
        }
      }
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
    final varTypeNames = <String, String>{}; // Track type names for ground guard checking

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
      final declaredMode = decl.argTypes[i].isInput ? Mode.input : Mode.output;
      if (!_inferVariableTypes(arg, argDFA, varTypes, TermPath.empty(),
          declaredMode, true)) {
        errors.add(TypeError(
          'Variable has inconsistent types across occurrences in clause head',
          clause.line, clause.column, clause.toString()
        ));
      }

      // Track variable to type name mapping for ground guard checking
      _collectVariableTypeNames(arg, decl.argTypes[i].name, varTypeNames);
    }

    // Step 3: Apply guard constraints
    if (clause.guards != null && clause.guards!.isNotEmpty) {
      final guardConstraints = extractGuardConstraints(
        clause.guards,
        typeEnv,
        compiler,
      );

      for (final entry in guardConstraints.entries) {
        final varName = entry.key;
        final guardType = entry.value;

        if (varTypes.containsKey(varName)) {
          final intersected = varTypes[varName]!.intersect(guardType);
          if (intersected.isEmpty) {
            errors.add(TypeError(
              'Guard type inconsistent with pattern type for variable $varName',
              clause.line, clause.column, clause.toString()
            ));
          }
          varTypes[varName] = intersected;
        } else {
          // Guard introduces type constraint for body-only variable
          varTypes[varName] = guardType;
        }
      }
    }

    // Step 3.5: Check ground guards are WMT
    if (clause.guards != null && clause.guards!.isNotEmpty) {
      final groundGuardErrors = modeChecker.checkGroundGuardsWithTypeNames(clause, varTypeNames);
      for (final modeError in groundGuardErrors) {
        errors.add(TypeError(
          modeError.message,
          modeError.line,
          modeError.column,
        ));
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
  /// Returns true if successful, false if intersection is empty (inconsistent types)
  bool _inferVariableTypes(
    ast.Term term,
    TypeDFA dfa,
    Map<String, TypeDFA> varTypes,
    TermPath pathToHere,
    Mode declaredArgMode,  // Mode from procedure declaration
    bool isHeadPosition,   // true for head, false for body goals
  ) {
    if (term is ast.VarTerm) {
      // Variable at this position gets the type reachable from current DFA state
      final state = dfa.stateAfterPath(pathToHere);
      if (state != null) {
        // Check mode at primitive positions
        if (dfa.isPrimitiveState(state)) {
          final primitiveModes = dfa.getModesAt(state);

          // For head positions, apply call boundary complementation
          // Callee sees complement of what caller declares
          final effectiveParentMode = isHeadPosition
              ? declaredArgMode.complement
              : declaredArgMode;

          // Combine parent mode with primitive type's mode
          // If parent is input, embedded mode is complemented
          Mode combineModeFn(Mode parent, Mode embedded) {
            return parent == Mode.input ? embedded.complement : embedded;
          }

          // The primitive modes tell us what the TYPE position accepts
          // We need to check if ANY primitive mode, when combined, matches the variable
          bool modeOK = false;
          for (final primitiveMode in primitiveModes) {
            final combinedMode = combineModeFn(effectiveParentMode, primitiveMode);
            // At combined INPUT position, expect WRITER (output variable)
            // At combined OUTPUT position, expect READER (input variable)
            final expectedVarMode = combinedMode == Mode.input ? Mode.output : Mode.input;
            final actualVarMode = term.isReader ? Mode.input : Mode.output;
            if (actualVarMode == expectedVarMode) {
              modeOK = true;
              break;
            }
          }

          if (!modeOK) {
            // Mode error at primitive position - reject this clause
            return false;
          }
        }

        final varName = term.name;
        final typeAtPosition = _dfaFromState(dfa, state);

        // Intersect with existing type for this variable (if any)
        if (varTypes.containsKey(varName)) {
          final existingType = varTypes[varName]!;

          // Check if types are compatible (same type, just different representations)
          if (_areCompatibleTypes(existingType, typeAtPosition)) {
            // Types are compatible, keep existing (or could intersect for precision)
            // For built-ins, prefer the direct NumberTypeDFA over _builtin_ states
            if (typeAtPosition is NumberTypeDFA || typeAtPosition is StringTypeDFA) {
              varTypes[varName] = typeAtPosition;
            }
            // else: keep existing
          } else {
            // Different types - must intersect
            final intersected = existingType.intersect(typeAtPosition);
            if (intersected.isEmpty) {
              // Variable has inconsistent types across occurrences
              return false;
            }
            varTypes[varName] = intersected;
          }
        } else {
          varTypes[varName] = typeAtPosition;
        }
      }
      return true;
    } else if (term is ast.StructTerm) {
      for (int i = 0; i < term.args.length; i++) {
        final newPath = pathToHere.append(
          PathElement.functor(term.functor, term.arity, i + 1)
        );
        if (!_inferVariableTypes(term.args[i], dfa, varTypes, newPath,
            declaredArgMode, isHeadPosition)) {
          return false;  // Propagate failure
        }
      }
      return true;
    } else if (term is ast.ListTerm) {
      if (!term.isNil) {
        if (term.head != null) {
          if (!_inferVariableTypes(term.head!, dfa, varTypes,
              pathToHere.append(PathElement.listHead()),
              declaredArgMode, isHeadPosition)) {
            return false;
          }
        }
        if (term.tail != null) {
          if (!_inferVariableTypes(term.tail!, dfa, varTypes,
              pathToHere.append(PathElement.listTail()),
              declaredArgMode, isHeadPosition)) {
            return false;
          }
        }
      }
      return true;
    }
    // Constants and UnderscoreTerm have no variables
    return true;
  }

  /// Check if two DFAs represent compatible types
  /// Handles the case where built-in types have different representations:
  /// - _builtin_Number (from embedded position) vs NumberTypeDFA (direct compilation)
  /// - Same start state name
  bool _areCompatibleTypes(TypeDFA type1, TypeDFA type2) {
    // Same start state → same type
    if (type1.startState.name == type2.startState.name) {
      return true;
    }

    // Both are NumberTypeDFA
    if (type1 is NumberTypeDFA && type2 is NumberTypeDFA) {
      return true;
    }

    // Both are StringTypeDFA
    if (type1 is StringTypeDFA && type2 is StringTypeDFA) {
      return true;
    }

    // One is _builtin_Number, other is NumberTypeDFA
    if ((type1.startState.name == '_builtin_Number' && type2 is NumberTypeDFA) ||
        (type1 is NumberTypeDFA && type2.startState.name == '_builtin_Number')) {
      return true;
    }

    // One is _builtin_String, other is StringTypeDFA
    if ((type1.startState.name == '_builtin_String' && type2 is StringTypeDFA) ||
        (type1 is StringTypeDFA && type2.startState.name == '_builtin_String')) {
      return true;
    }

    return false;
  }

  /// Create a DFA that accepts paths reachable from a given state
  TypeDFA _dfaFromState(TypeDFA originalDfa, DFAState fromState) {
    // This creates a DFA with the given state as start state
    return TypeDFA(
      states: originalDfa.states,
      startState: fromState,
      finalStates: originalDfa.finalStates,
      transitions: originalDfa.transitions,
      primitiveStateModes: originalDfa.primitiveStateModes,  // Must preserve mode information
    );
  }
  
  /// Check that a body goal uses variables consistently with their types
  GoalCheckResult _checkGoal(ast.Goal goal, Map<String, TypeDFA> varTypes) {
    final errors = <TypeError>[];

    // Look up procedure declaration for this goal
    final procDecl = typeEnv.getProcedure(goal.functor, goal.arity);

    if (procDecl == null) {
      // No declaration - skip body checking for this goal
      // This is not an error; unmoded predicates are allowed
      return GoalCheckResult(errors);
    }

    // Compile argument types to DFAs (cache these to avoid recompilation)
    final argDFAs = <TypeDFA>[];
    for (final argType in procDecl.argTypes) {
      try {
        argDFAs.add(compiler.compile(argType.name));
      } catch (e) {
        // If we can't compile the type, skip this goal
        return GoalCheckResult(errors);
      }
    }

    // Check each argument against declared type
    for (int i = 0; i < goal.args.length; i++) {
      final arg = goal.args[i];
      final argDFA = argDFAs[i];

      // Check ground paths in body argument
      final groundCheck = _checkGroundPaths(arg, argDFA);
      if (!groundCheck.success) {
        errors.add(TypeError(
          'Body goal ${goal.functor}/${goal.arity} argument ${i + 1}: '
          'ground path ${groundCheck.failedPath} not in declared type ${procDecl.argTypes[i].name}',
          goal.line, goal.column,
        ));
      }

      // Infer/constrain variable types from body occurrence
      final declaredMode = procDecl.argTypes[i].isInput ? Mode.input : Mode.output;
      if (!_inferVariableTypes(arg, argDFA, varTypes, TermPath.empty(),
          declaredMode, false)) {
        errors.add(TypeError(
          'Variable has inconsistent types between head and body goal ${goal.functor}/${goal.arity}',
          goal.line, goal.column,
        ));
      }
    }

    return GoalCheckResult(errors);
  }
}

/// Result of checking ground paths
class GroundPathCheck {
  final bool success;
  final TermPath? failedPath;

  GroundPathCheck(this.success, this.failedPath);
}

  /// Collect variable to type name mappings from a term pattern
  void _collectVariableTypeNames(ast.Term term, String typeName, Map<String, String> varTypeNames) {
    if (term is ast.VarTerm) {
      // Map variable to its type name (without the ? for input types)
      // Example: procedure f(List?), clause f(X), then X maps to "List" not "List?"
      final baseTypeName = typeName.endsWith('?') ? typeName.substring(0, typeName.length - 1) : typeName;
      varTypeNames[term.name] = baseTypeName;
    } else if (term is ast.StructTerm) {
      // Recursively collect from structure arguments
      // We don't track nested structure types here since ground guards only check top-level variable types
      for (final arg in term.args) {
        _collectVariableTypeNames(arg, typeName, varTypeNames);
      }
    } else if (term is ast.ListTerm) {
      if (!term.isNil && term.head != null) {
        _collectVariableTypeNames(term.head!, typeName, varTypeNames);
      }
      if (term.tail != null) {
        _collectVariableTypeNames(term.tail!, typeName, varTypeNames);
      }
    }
    // For other term types (constants, etc.), no variables to collect
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
