// lib/analysis/type_checker/type_checker.dart
//
// Main type checker implementing the Yardeni-Shapiro algorithm.
// Checks that programs are well-typed according to declared procedure types.
//
// A program P is well-typed by S iff:
// 1. No clause is useless relative to S
// 2. T_P^α(S) = S (S is a fixpoint of the abstract consequence operator)

import 'mode.dart';
import 'moded_label.dart';
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
      final declaredDFA = argDFAs[argIndex];
      final isInputArg = decl.argTypes[argIndex].isInput;

      // Union all clause contributions for this argument position
      var inferredDFA = TypeDFA.empty();

      for (final contribution in clauseContributions) {
        final clause = contribution.clause;
        final varTypes = contribution.variableTypes;

        if (argIndex < clause.head.args.length) {
          final argPattern = clause.head.args[argIndex];

          // For INPUT arguments: variable patterns accept ALL values of the type
          // A variable X at input position can receive any value of the declared type
          if (isInputArg && argPattern is ast.VarTerm) {
            inferredDFA = inferredDFA.union(declaredDFA);
          } else {
            final argContribution = contributionComputer.computeArgContribution(
              argPattern,
              varTypes,
              declaredDFA,
            );
            inferredDFA = inferredDFA.union(argContribution);
          }
        }
      }

      // For OUTPUT arguments with only variable patterns: skip check
      // Variables at output positions need body analysis to determine contribution
      // (e.g., body goal Y = ok determines Y's value)
      if (!isInputArg && inferredDFA.isEmpty) {
        final allVariables = clauseContributions.every((c) {
          if (argIndex >= c.clause.head.args.length) return false;
          return c.clause.head.args[argIndex] is ast.VarTerm;
        });
        if (allVariables) {
          continue;  // Skip fixpoint check for this argument
        }
      }

      // Fixpoint check: inferred ⊆ declared
      // Note: We use subset rather than equality because:
      // 1. Clauses may only cover a subset of the declared type (partial impl)
      // 2. For subtype declarations (::< ), subset is the requirement
      if (!inferredDFA.isSubsetOf(declaredDFA)) {
        // Only report if we have concrete evidence of mismatch
        // Skip if inferred is empty (common for complex patterns)
        if (!inferredDFA.isEmpty) {
          errors.add(TypeError(
            'Argument ${argIndex + 1} of ${decl.name}/${decl.arity}: '
            'inferred type is not a subset of declared type ${decl.argTypes[argIndex]}',
            decl.line, decl.column
          ));
        }
      }
    }

    return TypeCheckResult(errors, warnings);
  }

  /// Check a single clause against its procedure declaration
  ClauseCheckResult _checkClause(ast.Clause clause, ProcDecl decl, List<TypeDFA> argDFAs) {
    final errors = <TypeError>[];
    final warnings = <TypeWarning>[];

    // Step 1: Check that head has correct arity
    if (clause.head.arity != decl.arity) {
      errors.add(TypeError(
        'Clause head has arity ${clause.head.arity}, expected ${decl.arity}',
        clause.line, clause.column
      ));
      return ClauseCheckResult(errors, warnings, null);
    }

    // Step 2: Check ground paths in head against declared type
    for (int i = 0; i < clause.head.args.length; i++) {
      final arg = clause.head.args[i];
      final argDFA = argDFAs[i];

      final groundCheck = _checkGroundPaths(arg, argDFA);
      if (!groundCheck.success) {
        errors.add(TypeError(
          'Head argument ${i + 1}: ground path ${groundCheck.failedPath} not in declared type ${decl.argTypes[i].name}',
          clause.line, clause.column
        ));
        // Mark clause as useless but continue checking
      }
    }

    // Step 3: Infer variable types from head pattern
    final varTypes = <String, TypeDFA>{};
    final varTypeNames = <String, String>{};  // For guard checking

    for (int i = 0; i < clause.head.args.length; i++) {
      final arg = clause.head.args[i];
      final argDFA = argDFAs[i];
      final declaredMode = decl.argTypes[i].isInput ? Mode.input : Mode.output;

      // Collect variable to type name mappings for guard checking
      _collectVariableTypeNames(arg, decl.argTypes[i].name, varTypeNames);

      if (!_inferVariableTypes(arg, argDFA, varTypes, <ModedLabel>[],
          declaredMode, true)) {
        errors.add(TypeError(
          'Head argument ${i + 1}: type inference failed (empty intersection)',
          clause.line, clause.column
        ));
      }
    }

    // Step 4: Check guard constraints
    if (clause.guard != null) {
      final guardErrors = _checkGuard(clause.guard!, varTypes, varTypeNames);
      errors.addAll(guardErrors);
    }

    // Step 5: Check body goals
    for (final goal in clause.body) {
      final goalResult = _checkGoal(goal, varTypes);
      errors.addAll(goalResult.errors);
    }

    // If there were errors, clause may be useless
    if (errors.isNotEmpty) {
      warnings.add(TypeWarning(
        'Clause may be useless due to type errors',
        clause.line, clause.column
      ));
    }

    return ClauseCheckResult(
      errors,
      warnings,
      ClauseContribution(clause, varTypes)
    );
  }

  /// Check guard for type consistency
  List<TypeError> _checkGuard(ast.Guard guard, Map<String, TypeDFA> varTypes, Map<String, String> varTypeNames) {
    final errors = <TypeError>[];

    // Handle compound guards
    if (guard is ast.AndGuard) {
      errors.addAll(_checkGuard(guard.left, varTypes, varTypeNames));
      errors.addAll(_checkGuard(guard.right, varTypes, varTypeNames));
      return errors;
    }

    if (guard is ast.OrGuard) {
      errors.addAll(_checkGuard(guard.left, varTypes, varTypeNames));
      errors.addAll(_checkGuard(guard.right, varTypes, varTypeNames));
      return errors;
    }

    if (guard is ast.NotGuard) {
      errors.addAll(_checkGuard(guard.inner, varTypes, varTypeNames));
      return errors;
    }

    // Handle atomic guards
    if (guard is ast.AtomicGuard) {
      final guardInfo = GuardTypeRegistry.getGuardInfo(guard.functor, guard.args.length);
      if (guardInfo == null) {
        // Unknown guard - skip checking
        return errors;
      }

      // Check argument types against guard signature
      for (int i = 0; i < guard.args.length && i < guardInfo.argTypes.length; i++) {
        final arg = guard.args[i];
        final expectedType = guardInfo.argTypes[i];

        if (arg is ast.VarTerm) {
          final varTypeName = varTypeNames[arg.name];
          if (varTypeName != null && expectedType != 'Any') {
            // Check if variable's type is compatible with guard's expected type
            // For now, just check if it's the same type or a subtype
            if (varTypeName != expectedType &&
                !_isSubtype(varTypeName, expectedType)) {
              errors.add(TypeError(
                'Guard ${guard.functor}: argument ${i + 1} has type $varTypeName, expected $expectedType',
                guard.line, guard.column
              ));
            }
          }
        }
      }

      // Handle ground guard specially
      if (guard.functor == 'ground' && guard.args.length == 1) {
        final arg = guard.args[0];
        if (arg is ast.VarTerm) {
          final varTypeName = varTypeNames[arg.name];
          if (varTypeName != null) {
            // Check if type has no mode complementations (required for ground guard)
            if (!_typeHasNoModeComplementations(varTypeName)) {
              errors.add(TypeError(
                'Guard ground(${arg.name}): type $varTypeName has mode complementations, '
                'which is incompatible with ground guard',
                guard.line, guard.column
              ));
            }
          }
        }
      }
    }

    return errors;
  }

  /// Check if type1 is a subtype of type2
  bool _isSubtype(String type1, String type2) {
    if (type1 == type2) return true;
    if (type2 == 'Any' || type2 == 'Every') return true;

    // Check subtype declarations
    final typeDef = typeEnv.getType(type1);
    if (typeDef != null && !typeDef.isExact) {
      // type1 ::< supertype
      for (final alt in typeDef.alternatives) {
        if (alt is TypeRef && alt.name == type2) {
          return true;
        }
      }
    }

    return false;
  }

  /// Check if a type has no mode complementations (no ? in its definition)
  bool _typeHasNoModeComplementations(String typeName) {
    final typeDef = typeEnv.getType(typeName);
    if (typeDef == null) {
      // Built-in types have no mode complementations
      return TypeRef.builtins.contains(typeName);
    }

    return _exprHasNoModeComplementations(typeDef.alternatives);
  }

  bool _exprHasNoModeComplementations(List<TypeExpr> alts) {
    for (final alt in alts) {
      if (!_singleExprHasNoModeComplementations(alt)) {
        return false;
      }
    }
    return true;
  }

  bool _singleExprHasNoModeComplementations(TypeExpr expr) {
    if (expr is TypeRef) {
      if (expr.isInput) return false;  // T? has mode complementation
      return _typeHasNoModeComplementations(expr.name);
    }
    if (expr is StructAlt) {
      for (final arg in expr.args) {
        if (!_singleExprHasNoModeComplementations(arg)) {
          return false;
        }
      }
      return true;
    }
    if (expr is ListConsAlt) {
      return _singleExprHasNoModeComplementations(expr.head) &&
             _singleExprHasNoModeComplementations(expr.tail);
    }
    if (expr is DiffListAlt) {
      return _singleExprHasNoModeComplementations(expr.content) &&
             _singleExprHasNoModeComplementations(expr.hole);
    }
    // Constants, nil, primitives have no mode complementations
    return true;
  }

  /// Check ground paths in a term against a type DFA
  GroundPathCheck _checkGroundPaths(ast.Term term, TypeDFA dfa) {
    // Extract all ground paths from the term
    final paths = _extractGroundPaths(term, <ModedLabel>[]);

    // Check each ground path against the DFA
    for (final path in paths) {
      // For ground paths, we use output mode as default
      if (!dfa.acceptsModedPath(path, Mode.output)) {
        return GroundPathCheck(false, path);
      }
    }

    return GroundPathCheck(true, null);
  }

  /// Extract ground paths from a term (paths ending in constants)
  Set<List<ModedLabel>> _extractGroundPaths(ast.Term term, List<ModedLabel> currentPath) {
    if (term is ast.VarTerm || term is ast.UnderscoreTerm) {
      // Variables are not ground - no paths to extract
      return {};
    }

    if (term is ast.StructTerm) {
      final paths = <List<ModedLabel>>{};
      for (int i = 0; i < term.args.length; i++) {
        final elemPath = ModedLabel.functor(term.functor, term.arity, i + 1);
        final subPaths = _extractGroundPaths(term.args[i], [...currentPath, elemPath]);
        paths.addAll(subPaths);
      }
      // If no args have ground paths but this is a constant functor, add this path
      if (paths.isEmpty && term.args.isEmpty) {
        paths.add([...currentPath, ModedLabel.constant(term.functor)]);
      }
      return paths;
    }

    if (term is ast.ListTerm) {
      if (term.isNil) {
        return {[...currentPath, ModedLabel.nil()]};
      }
      final paths = <List<ModedLabel>>{};
      if (term.head != null) {
        final headPath = [...currentPath, ModedLabel.listHead()];
        paths.addAll(_extractGroundPaths(term.head!, headPath));
      }
      if (term.tail != null) {
        final tailPath = [...currentPath, ModedLabel.listTail()];
        paths.addAll(_extractGroundPaths(term.tail!, tailPath));
      }
      return paths;
    }

    if (term is ast.ConstTerm) {
      return {[...currentPath, ModedLabel.constant(term.value!)]};
    }

    return {};
  }

  /// Infer variable types by traversing term and type DFA in parallel
  /// Returns false if type inference fails (empty intersection)
  bool _inferVariableTypes(
    ast.Term term,
    TypeDFA dfa,
    Map<String, TypeDFA> varTypes,
    List<ModedLabel> pathToHere,
    Mode declaredArgMode,
    bool isHeadPosition,
  ) {
    // Find current state by following path from start
    var state = dfa.startState;
    for (final elem in pathToHere) {
      final next = dfa.transitions[(state, elem)];
      if (next == null) {
        // Try to find transition by symbol only (ignoring mode)
        next as DFAState?;
        var found = false;
        for (final entry in dfa.transitions.entries) {
          final (fromState, label) = entry.key;
          if (fromState == state && label.pathElement == elem.pathElement) {
            state = entry.value;
            found = true;
            break;
          }
        }
        if (!found) {
          return false;  // Path not in type
        }
      } else {
        state = next;
      }
    }

    if (term is ast.VarTerm) {
      // Check mode at this position
      if (dfa.isPrimitiveState(state)) {
        final acceptedModes = dfa.getModesAt(state);
        final varMode = term.isReader ? Mode.input : Mode.output;

        // In head position, modes are complemented
        // (what caller provides as output, clause sees as needing input)
        final expectedMode = isHeadPosition ? declaredArgMode.complement : declaredArgMode;

        // Mode check: variable mode should match expected mode
        // But for bi-moded types (Every), both are accepted
        if (acceptedModes.length == 1 && !acceptedModes.contains(varMode)) {
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
      return true;
    } else if (term is ast.StructTerm) {
      for (int i = 0; i < term.args.length; i++) {
        final newPath = [...pathToHere, ModedLabel.functor(term.functor, term.arity, i + 1)];
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
              [...pathToHere, ModedLabel.listHead()],
              declaredArgMode, isHeadPosition)) {
            return false;
          }
        }
        if (term.tail != null) {
          if (!_inferVariableTypes(term.tail!, dfa, varTypes,
              [...pathToHere, ModedLabel.listTail()],
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
      if (!_inferVariableTypes(arg, argDFA, varTypes, <ModedLabel>[],
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
  final List<ModedLabel>? failedPath;

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
