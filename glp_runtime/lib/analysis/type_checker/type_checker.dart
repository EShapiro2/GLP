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
        var dfa = compiler.compile(argType.name);
        // Apply mode complement for reader arguments (Type?)
        if (argType.isInput) {
          dfa = dfa.applyModeComplement();
        }
        argDFAs.add(dfa);
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

    // Mode coverage checking
    final modeCoverageErrors = modeChecker.checkModeCoverageOnly(decl.name, decl.arity, clauses);
    for (final modeError in modeCoverageErrors) {
      errors.add(TypeError(
        modeError.message,
        modeError.line,
        modeError.column,
      ));
    }

    // Fixpoint check
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
        final clauseVarTypeNames = contribution.variableTypeNames;

        if (argIndex < clause.head.args.length) {
          final argPattern = clause.head.args[argIndex];

          if (isInputArg && argPattern is ast.VarTerm) {
            inferredDFA = inferredDFA.union(declaredDFA);
          } else {
            final argContribution = contributionComputer.computeArgContribution(
              argPattern,
              varTypes,
              clauseVarTypeNames,
              declaredDFA,
              isInputArg,
            );
            inferredDFA = inferredDFA.union(argContribution);
          }
        }
      }

      // For OUTPUT arguments with only variable patterns: skip check
      if (!isInputArg && inferredDFA.isModedEmpty) {
        final allVariables = clauseContributions.every((c) {
          if (argIndex >= c.clause.head.args.length) return false;
          return c.clause.head.args[argIndex] is ast.VarTerm;
        });
        if (allVariables) {
          continue;
        }
      }

      // Structural containment check: inferred ⊆ declared
      // Uses STRUCTURAL comparison (ignores modes) per spec v1.13 Section 6.1
      // Modes are checked separately by mode_checker
      if (!inferredDFA.structuralIsSubsetOf(declaredDFA)) {
        if (!inferredDFA.isModedEmpty) {
          errors.add(TypeError(
            'Argument ${argIndex + 1} of ${decl.name}/${decl.arity}: '
            'inferred type is not a subset of declared type ${decl.argTypes[argIndex]}',
            decl.line, decl.column
          ));
        }
      }

      // Coverage check for ::= types: declared ⊆ inferred
      // Per spec 6.1: For ::= semantics, also check S ⊆ inferred
      final typeDef = typeEnv.getType(decl.argTypes[argIndex].name);
      final isExactType = typeDef?.isExact ?? true;

      if (isInputArg) {
        if (!declaredDFA.structuralIsSubsetOf(inferredDFA)) {
          // Check if this is due to all-variable patterns (which is OK)
          final allVariables = clauseContributions.every((c) {
            if (argIndex >= c.clause.head.args.length) return false;
            return c.clause.head.args[argIndex] is ast.VarTerm;
          });
          if (!allVariables && !declaredDFA.isModedEmpty) {
            errors.add(TypeError(
              'Argument ${argIndex + 1} of ${decl.name}/${decl.arity}: '
              'clauses do not cover all alternatives of declared type ${decl.argTypes[argIndex].name}',
              decl.line, decl.column
            ));
          }
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
      }
    }

    // Step 3: Infer variable types from head pattern
    final varTypes = <String, TypeDFA>{};
    final varTypeNames = <String, String>{};

    for (int i = 0; i < clause.head.args.length; i++) {
      final arg = clause.head.args[i];
      final argDFA = argDFAs[i];
      final declaredMode = decl.argTypes[i].isInput ? Mode.input : Mode.output;

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
    if (clause.guards != null && clause.guards!.isNotEmpty) {
      for (final guard in clause.guards!) {
        final guardErrors = _checkGuard(guard, varTypes, varTypeNames);
        errors.addAll(guardErrors);
      }
    }

    // Step 5: Check body goals
    if (clause.body != null) {
      for (final goal in clause.body!) {
        final goalResult = _checkGoal(goal, varTypes);
        errors.addAll(goalResult.errors);
      }
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
      ClauseContribution(clause, varTypes, varTypeNames)
    );
  }

  /// Check a single guard for type consistency
  List<TypeError> _checkGuard(ast.Guard guard, Map<String, TypeDFA> varTypes, Map<String, String> varTypeNames) {
    final errors = <TypeError>[];

    final guardSig = GuardTypeRegistry.getSignature(guard.predicate);
    if (guardSig == null) {
      // Unknown guard - skip checking
      return errors;
    }

    // Check argument types against guard signature
    for (int i = 0; i < guard.args.length && i < guardSig.argTypeNames.length; i++) {
      final arg = guard.args[i];
      final expectedType = guardSig.argTypeNames[i];

      if (arg is ast.VarTerm) {
        final varTypeName = varTypeNames[arg.name];
        if (varTypeName != null && expectedType != 'Any') {
          if (varTypeName != expectedType &&
              !_isSubtype(varTypeName, expectedType)) {
            errors.add(TypeError(
              'Guard ${guard.predicate}: argument ${i + 1} has type $varTypeName, expected $expectedType',
              guard.line, guard.column
            ));
          }
        }
      }
    }

    // Handle ground guard specially
    if (guard.predicate == 'ground' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is ast.VarTerm) {
        final varTypeName = varTypeNames[arg.name];
        if (varTypeName != null) {
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

    return errors;
  }

  /// Check if type1 is a subtype of type2
  bool _isSubtype(String type1, String type2) {
    if (type1 == type2) return true;
    if (type2 == 'Any' || type2 == 'Every') return true;

    final typeDef = typeEnv.getType(type1);
    if (typeDef != null && !typeDef.isExact) {
      for (final alt in typeDef.alternatives) {
        if (alt is TypeRef && alt.name == type2) {
          return true;
        }
      }
    }

    return false;
  }

  /// Check if a type has no mode complementations
  bool _typeHasNoModeComplementations(String typeName) {
    return _typeHasNoModeComplementationsWithVisited(typeName, <String>{});
  }

  bool _typeHasNoModeComplementationsWithVisited(String typeName, Set<String> visited) {
    if (visited.contains(typeName)) return true;  // Break cycles

    final typeDef = typeEnv.getType(typeName);
    if (typeDef == null) {
      return TypeRef.builtins.contains(typeName);
    }

    visited.add(typeName);
    return _exprHasNoModeComplementationsWithVisited(typeDef.alternatives, visited);
  }

  bool _exprHasNoModeComplementations(List<TypeExpr> alts) {
    return _exprHasNoModeComplementationsWithVisited(alts, <String>{});
  }

  bool _exprHasNoModeComplementationsWithVisited(List<TypeExpr> alts, Set<String> visited) {
    for (final alt in alts) {
      if (!_singleExprHasNoModeComplementations(alt, visited)) {
        return false;
      }
    }
    return true;
  }

  bool _singleExprHasNoModeComplementations(TypeExpr expr, [Set<String>? visited]) {
    visited ??= <String>{};

    if (expr is TypeRef) {
      if (expr.isInput) return false;
      // Check for recursive types
      if (visited.contains(expr.name)) return true;  // Assume OK to break cycle
      return _typeHasNoModeComplementationsWithVisited(expr.name, visited);
    }
    if (expr is StructAlt) {
      for (final arg in expr.args) {
        if (!_singleExprHasNoModeComplementations(arg, visited)) {
          return false;
        }
      }
      return true;
    }
    if (expr is ListConsAlt) {
      return _singleExprHasNoModeComplementations(expr.head, visited) &&
             _singleExprHasNoModeComplementations(expr.tail, visited);
    }
    if (expr is DiffListAlt) {
      return _singleExprHasNoModeComplementations(expr.content, visited) &&
             _singleExprHasNoModeComplementations(expr.hole, visited);
    }
    return true;
  }

  /// Check ground paths in a term against a type DFA
  GroundPathCheck _checkGroundPaths(ast.Term term, TypeDFA dfa) {
    final paths = _extractGroundPaths(term, <ModedLabel>[]);

    for (final path in paths) {
      if (!dfa.acceptsStructuralPath(path)) {
        return GroundPathCheck(false, path);
      }
    }

    return GroundPathCheck(true, null);
  }

  /// Extract ground paths from a term
  Set<List<ModedLabel>> _extractGroundPaths(ast.Term term, List<ModedLabel> currentPath) {
    if (term is ast.VarTerm || term is ast.UnderscoreTerm) {
      return {};
    }

    if (term is ast.StructTerm) {
      final paths = <List<ModedLabel>>{};
      for (int i = 0; i < term.args.length; i++) {
        final elemPath = ModedLabel.functor(term.functor, term.arity, i + 1);
        final subPaths = _extractGroundPaths(term.args[i], [...currentPath, elemPath]);
        paths.addAll(subPaths);
      }
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
          return false;
        }
      } else {
        state = next;
      }
    }

    if (term is ast.VarTerm) {
      if (dfa.isPrimitiveState(state)) {
        final acceptedModes = dfa.getModesAt(state);
        final varMode = term.isReader ? Mode.input : Mode.output;

        final expectedMode = isHeadPosition ? declaredArgMode.complement : declaredArgMode;

        if (acceptedModes.length == 1 && !acceptedModes.contains(varMode)) {
          return false;
        }
      }

      final varName = term.name;
      final typeAtPosition = _dfaFromState(dfa, state);

      if (varTypes.containsKey(varName)) {
        final existingType = varTypes[varName]!;

        if (_areCompatibleTypes(existingType, typeAtPosition)) {
          if (typeAtPosition is NumberTypeDFA || typeAtPosition is StringTypeDFA) {
            varTypes[varName] = typeAtPosition;
          }
        } else {
          final intersected = existingType.modedIntersect(typeAtPosition);
          if (intersected.isModedEmpty) {
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
          return false;
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
    return true;
  }

  bool _areCompatibleTypes(TypeDFA type1, TypeDFA type2) {
    if (type1.startState.name == type2.startState.name) {
      return true;
    }

    if (type1 is NumberTypeDFA && type2 is NumberTypeDFA) {
      return true;
    }

    if (type1 is StringTypeDFA && type2 is StringTypeDFA) {
      return true;
    }

    if ((type1.startState.name == '_builtin_Number' && type2 is NumberTypeDFA) ||
        (type1 is NumberTypeDFA && type2.startState.name == '_builtin_Number')) {
      return true;
    }

    if ((type1.startState.name == '_builtin_String' && type2 is StringTypeDFA) ||
        (type1 is StringTypeDFA && type2.startState.name == '_builtin_String')) {
      return true;
    }

    return false;
  }

  TypeDFA _dfaFromState(TypeDFA originalDfa, DFAState fromState) {
    return TypeDFA(
      states: originalDfa.states,
      startState: fromState,
      finalStates: originalDfa.finalStates,
      transitions: originalDfa.transitions,
      primitiveStateModes: originalDfa.primitiveStateModes,
    );
  }

  GoalCheckResult _checkGoal(ast.Goal goal, Map<String, TypeDFA> varTypes) {
    final errors = <TypeError>[];

    final procDecl = typeEnv.getProcedure(goal.functor, goal.arity);

    if (procDecl == null) {
      return GoalCheckResult(errors);
    }

    final argDFAs = <TypeDFA>[];
    for (final argType in procDecl.argTypes) {
      try {
        var dfa = compiler.compile(argType.name);
        // Apply mode complement for reader arguments (Type?)
        if (argType.isInput) {
          dfa = dfa.applyModeComplement();
        }
        argDFAs.add(dfa);
      } catch (e) {
        return GoalCheckResult(errors);
      }
    }

    for (int i = 0; i < goal.args.length; i++) {
      final arg = goal.args[i];
      final argDFA = argDFAs[i];

      final groundCheck = _checkGroundPaths(arg, argDFA);
      if (!groundCheck.success) {
        errors.add(TypeError(
          'Body goal ${goal.functor}/${goal.arity} argument ${i + 1}: '
          'ground path ${groundCheck.failedPath} not in declared type ${procDecl.argTypes[i].name}',
          goal.line, goal.column,
        ));
      }

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

  void _collectVariableTypeNames(ast.Term term, String typeName, Map<String, String> varTypeNames) {
    if (term is ast.VarTerm) {
      final baseTypeName = typeName.endsWith('?') ? typeName.substring(0, typeName.length - 1) : typeName;
      varTypeNames[term.name] = baseTypeName;
    } else if (term is ast.StructTerm) {
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
  }
}

/// Result of checking ground paths
class GroundPathCheck {
  final bool success;
  final List<ModedLabel>? failedPath;

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
  final Map<String, String> variableTypeNames;

  ClauseContribution(this.clause, this.variableTypes, this.variableTypeNames);
}

/// Result of checking a body goal
class GoalCheckResult {
  final List<TypeError> errors;

  GoalCheckResult(this.errors);
}

/// Parse and type-check GLP source code
TypeCheckResult checkSource(String source, List<ast.Clause> clauses) {
  final typeEnv = parseTypes(source);
  final checker = TypeChecker(typeEnv);
  return checker.check(clauses);
}
