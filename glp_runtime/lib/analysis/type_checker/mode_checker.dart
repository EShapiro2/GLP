// lib/analysis/type_checker/mode_checker.dart
//
// Mode checker for moded type system.
// Verifies that variable modes (reader/writer) match type modes (input/output).

import '../../compiler/ast.dart' as ast;
import 'type_ast.dart';
import 'mode.dart';
import 'mode_error.dart';
import 'guard_types.dart';

/// Mode checker for procedure definitions
///
/// Checks that variable usage (reader X? vs writer X) is consistent with
/// type mode annotations (input Type? vs output Type) in procedure declarations.
class ModeChecker {
  final TypeEnvironment typeEnv;

  ModeChecker(this.typeEnv);

  /// Check all clauses for a procedure against its type declaration
  ///
  /// Returns list of mode errors found (empty if all clauses are mode-correct)
  List<ModeError> checkProcedure(String name, int arity, List<ast.Clause> clauses) {
    final errors = <ModeError>[];

    // Look up procedure declaration
    final procDecl = typeEnv.getProcedure(name, arity);
    if (procDecl == null) {
      // No type declaration - skip mode checking
      return errors;
    }

    // Check each clause
    for (final clause in clauses) {
      errors.addAll(checkClause(clause, procDecl));
    }

    // Check mode coverage for primitive types (Any, _, _?)
    errors.addAll(_checkModeCoverage(procDecl, clauses));

    return errors;
  }

  /// Check mode coverage only (no per-variable checking)
  ///
  /// This checks that clauses collectively cover all mode alternatives
  /// for primitive types, without checking individual variable modes.
  List<ModeError> checkModeCoverageOnly(String name, int arity, List<ast.Clause> clauses) {
    final errors = <ModeError>[];

    // Look up procedure declaration
    final procDecl = typeEnv.getProcedure(name, arity);
    if (procDecl == null) {
      return errors;
    }

    // Check mode coverage for primitive types (Any, _, _?)
    errors.addAll(_checkModeCoverage(procDecl, clauses));

    return errors;
  }

  /// Check a single clause against procedure declaration
  List<ModeError> checkClause(ast.Clause clause, ProcDecl procDecl) {
    final errors = <ModeError>[];

    // Check head arguments
    errors.addAll(_checkHead(clause, procDecl));

    // Check body goals (if any)
    if (clause.body != null) {
      for (final goal in clause.body!) {
        errors.addAll(_checkBodyGoal(goal, procDecl.name));
      }
    }

    return errors;
  }

  /// Check clause head arguments match procedure declaration modes
  List<ModeError> _checkHead(ast.Clause clause, ProcDecl procDecl) {
    final errors = <ModeError>[];
    final head = clause.head;

    // Check each argument
    for (int i = 0; i < head.args.length && i < procDecl.argTypes.length; i++) {
      final arg = head.args[i];
      final typeRef = procDecl.argTypes[i];

      // Determine expected mode from type declaration
      final expectedMode = typeRef.isInput ? Mode.input : Mode.output;

      // Check term mode against expected mode
      final error = _checkTermMode(
        arg,
        expectedMode,
        typeRef,
        procDecl.name,
        i,
      );
      if (error != null) {
        errors.add(error);
      }
    }

    return errors;
  }

  /// Check body goal with call boundary mode complementation
  ///
  /// At a call site, modes are complemented:
  /// - Callee declares input (Type?) → caller provides writer (X)
  /// - Callee declares output (Type) → caller provides reader (X?)
  List<ModeError> _checkBodyGoal(ast.Goal goal, String callerName) {
    final errors = <ModeError>[];

    // Look up callee procedure declaration
    final calleeProcDecl = typeEnv.getProcedure(goal.functor, goal.args.length);
    if (calleeProcDecl == null) {
      return errors; // No type declaration - skip checking
    }

    // Check each argument with call boundary complementation
    for (int i = 0; i < goal.args.length && i < calleeProcDecl.argTypes.length; i++) {
      final arg = goal.args[i];
      final calleeTypeRef = calleeProcDecl.argTypes[i];

      // At call boundary, complement the callee's mode to get caller's expected mode
      // Callee's input (Type?) → caller must provide output mode (writer X)
      // Callee's output (Type) → caller must provide input mode (reader X?)
      final calleeMode = calleeTypeRef.isInput ? Mode.input : Mode.output;
      final callerExpectedMode = calleeMode.complement;

      // Check term mode against caller's expected mode
      final error = _checkTermMode(
        arg,
        callerExpectedMode,
        calleeTypeRef,
        goal.functor,
        i,
      );
      if (error != null) {
        errors.add(error);
      }
    }

    return errors;
  }

  /// Recursively check term mode against type with embedded modes
  ///
  /// Returns ModeError if mode mismatch found, null if ok
  ModeError? _checkTermMode(
    ast.Term term,
    Mode expectedMode,
    TypeExpr typeExpr,
    String predicate,
    int argIndex,
  ) {
    // Variable terms: check reader/writer matches expected mode
    if (term is ast.VarTerm) {
      return _checkVariableMode(term, expectedMode, predicate, argIndex);
    }

    // Primitive mode types (_, _?, Any) require variable patterns
    // Constructor patterns at these positions are ill-typed
    if (typeExpr is TypeRef && _isPrimitiveType(typeExpr.name)) {
      // term is NOT a variable but position has primitive/Any type
      return ModeError(
        'Constructor pattern at primitive type position\n'
        'Argument ${argIndex + 1} of $predicate has type ${typeExpr.name}, '
        'which requires a variable pattern. Found ${_termDescription(term)} instead.\n\n'
        'Primitive types (_, _?, Any) must handle all ground terms, so only '
        'variable patterns are allowed at these positions.',
        term.line,
        term.column,
      );
    }

    // Compound terms: recursively check subterms with embedded modes
    if (term is ast.StructTerm) {
      // If typeExpr is a StructAlt, use it directly
      if (typeExpr is StructAlt) {
        return _checkCompoundMode(term, expectedMode, typeExpr, predicate, argIndex);
      }

      // If typeExpr is a TypeRef, resolve it to find the matching constructor
      if (typeExpr is TypeRef) {
        final typeDef = typeEnv.getType(typeExpr.name);
        if (typeDef != null) {
          // Find matching constructor in type definition
          for (final alt in typeDef.alternatives) {
            if (alt is StructAlt &&
                alt.functor == term.functor &&
                alt.arity == term.args.length) {
              // Found matching constructor - check with embedded modes
              return _checkCompoundMode(term, expectedMode, alt, predicate, argIndex);
            }
          }
        }
      }
    }

    // List terms: recursively check head and tail with embedded modes
    if (term is ast.ListTerm) {
      // Nil list - no checking needed
      if (term.isNil) return null;

      // Resolve TypeRef to find ListConsAlt
      if (typeExpr is TypeRef) {
        final typeDef = typeEnv.getType(typeExpr.name);
        if (typeDef != null) {
          for (final alt in typeDef.alternatives) {
            if (alt is ListConsAlt) {
              // Check head element against head type with same parent mode
              if (term.head != null) {
                final headError = _checkTermMode(
                  term.head!,
                  expectedMode,
                  alt.head,
                  predicate,
                  argIndex,
                );
                if (headError != null) return headError;
              }

              // Check tail against tail type with same parent mode
              if (term.tail != null) {
                return _checkTermMode(
                  term.tail!,
                  expectedMode,
                  alt.tail,
                  predicate,
                  argIndex,
                );
              }
            }
          }
        }
      }
    }

    // Constants, etc. - no mode checking needed
    return null;
  }

  /// Check variable mode (reader/writer) matches expected mode
  ///
  /// Mode matching rules:
  /// - Output position expects reader (X?) - callee will write
  /// - Input position expects writer (X) - caller will provide value
  ModeError? _checkVariableMode(
    ast.VarTerm variable,
    Mode expectedMode,
    String predicate,
    int argIndex,
  ) {
    final isReader = variable.isReader;

    switch (expectedMode) {
      case Mode.output:
        // Output position: callee writes, so caller must provide reader (X?)
        if (!isReader) {
          return ModeError.writerAtOutput(
            variable: variable.name,
            predicate: predicate,
            argIndex: argIndex,
            line: variable.line,
            column: variable.column,
          );
        }
        break;

      case Mode.input:
        // Input position: caller provides value, so must be writer (X)
        if (isReader) {
          return ModeError.readerAtInput(
            variable: variable.name,
            predicate: predicate,
            argIndex: argIndex,
            line: variable.line,
            column: variable.column,
          );
        }
        break;
    }

    return null; // Mode correct
  }

  /// Check compound term with embedded modes (involution property)
  ///
  /// When a type has embedded structure like s(Nat?), the inner mode
  /// combines with outer mode via involution:
  /// - output ⊕ output = output
  /// - output ⊕ input  = input
  /// - input  ⊕ output = input
  /// - input  ⊕ input  = output (two inversions cancel)
  ModeError? _checkCompoundMode(
    ast.StructTerm term,
    Mode parentMode,
    StructAlt typeAlt,
    String predicate,
    int argIndex,
  ) {
    // Check each subterm against corresponding type argument
    for (int i = 0; i < term.args.length && i < typeAlt.args.length; i++) {
      final subterm = term.args[i];
      final subtypeExpr = typeAlt.args[i];

      // Determine embedded mode from subtype
      Mode embeddedMode = Mode.output; // default
      if (subtypeExpr is TypeRef && subtypeExpr.isInput) {
        embeddedMode = Mode.input;
      }

      // Combine parent mode with embedded mode (involution)
      final effectiveMode = combineMode(parentMode, embeddedMode);

      // Recursively check subterm
      final error = _checkTermMode(
        subterm,
        effectiveMode,
        subtypeExpr,
        predicate,
        argIndex,
      );

      if (error != null) {
        return error;
      }
    }

    return null; // All subterms ok
  }

  /// Check if a type name is a primitive mode type
  ///
  /// Primitive types (_, _?, Any) require variable patterns because they
  /// must handle all ground terms.
  bool _isPrimitiveType(String typeName) {
    return typeName == 'Any' || typeName == '_' || typeName == '_?';
  }

  /// Find all positions within a type that have Any (or types with primitive modes)
  /// Returns list of (path, typeName) pairs where path describes how to navigate there
  ///
  /// insideSubtype: if true, we entered through a ::< type and should not report
  List<({String path, String typeName})> _findPrimitiveTypePositions(
    TypeRef typeRef,
    String currentPath,
    Set<String> visited,  // Prevent infinite recursion on recursive types
    bool insideSubtype,  // true if we entered through a ::< type
  ) {
    final positions = <({String path, String typeName})>[];

    final typeDef = typeEnv.getType(typeRef.name);

    // If this type is ::< (subtype), mark that we're inside a subtype
    // and don't require coverage for anything nested within
    final nowInsideSubtype = insideSubtype || (typeDef != null && !typeDef.isExact);

    // Check if this type itself has primitive modes
    if (typeDef != null && typeDef.isExact) {
      // Check if any alternative is a primitive mode
      bool hasPrimitiveModes = typeDef.alternatives.any((alt) => alt is PrimitiveModeAlt);
      if (hasPrimitiveModes && !insideSubtype) {
        // Only report if we didn't enter through a ::< type
        positions.add((path: currentPath, typeName: typeRef.name));
        return positions;  // Don't recurse into primitive type definitions
      }
    }

    // If we're inside a subtype (entered via ::<), don't recurse further
    // Subtype semantics means no coverage requirement
    if (nowInsideSubtype) {
      return positions;
    }

    // Recurse into type definition to find nested Any positions
    if (typeDef != null && !visited.contains(typeRef.name)) {
      visited.add(typeRef.name);

      for (final alt in typeDef.alternatives) {
        if (alt is ListConsAlt) {
          // Check head type
          if (alt.head is TypeRef) {
            final headPath = currentPath.isEmpty ? 'head' : '$currentPath.head';
            positions.addAll(_findPrimitiveTypePositions(
              alt.head as TypeRef, headPath, visited, nowInsideSubtype));
          }
          // Check tail type (but avoid infinite recursion - already in visited)
          if (alt.tail is TypeRef) {
            final tailPath = currentPath.isEmpty ? 'tail' : '$currentPath.tail';
            positions.addAll(_findPrimitiveTypePositions(
              alt.tail as TypeRef, tailPath, visited, nowInsideSubtype));
          }
        } else if (alt is StructAlt) {
          for (int i = 0; i < alt.args.length; i++) {
            if (alt.args[i] is TypeRef) {
              final argPath = currentPath.isEmpty
                  ? '${alt.functor}[$i]'
                  : '$currentPath.${alt.functor}[$i]';
              positions.addAll(_findPrimitiveTypePositions(
                alt.args[i] as TypeRef, argPath, visited, nowInsideSubtype));
            }
          }
        }
        // ConstantAlt, ListNilAlt, PrimitiveModeAlt - no nested types
      }
    }

    return positions;
  }

  /// Navigate into a term following a path like "head" or "head.tail"
  ast.Term? _getTermAtPath(ast.Term term, String path) {
    if (path.isEmpty) return term;

    final parts = path.split('.').where((p) => p.isNotEmpty).toList();
    ast.Term? current = term;

    for (final part in parts) {
      if (current == null) return null;

      if (part == 'head' && current is ast.ListTerm && !current.isNil) {
        current = current.head;
      } else if (part == 'tail' && current is ast.ListTerm && !current.isNil) {
        current = current.tail;
      } else if (part.contains('[') && current is ast.StructTerm) {
        // Parse functor[index]
        final match = RegExp(r'(\w+)\[(\d+)\]').firstMatch(part);
        if (match != null && current.functor == match.group(1)) {
          final index = int.parse(match.group(2)!);
          if (index < current.args.length) {
            current = current.args[index];
          } else {
            return null;
          }
        } else {
          return null;
        }
      } else {
        // Pattern doesn't match this path component
        return null;
      }
    }

    return current;
  }

  /// Check mode coverage for primitive type positions (including nested)
  ///
  /// For types with primitive modes (e.g., Any ::= _ ; _?), verify that
  /// the clauses collectively cover both writer (_) and reader (_?) modes.
  List<ModeError> _checkModeCoverage(ProcDecl procDecl, List<ast.Clause> clauses) {
    final errors = <ModeError>[];

    // Check each argument position
    for (int argIndex = 0; argIndex < procDecl.argTypes.length; argIndex++) {
      final typeRef = procDecl.argTypes[argIndex];

      // Find all positions with primitive modes (including nested)
      final primitivePositions = _findPrimitiveTypePositions(typeRef, '', <String>{}, false);

      for (final position in primitivePositions) {
        // Track which modes appear in clause heads for this position
        bool hasWriter = false;
        bool hasReader = false;

        for (final clause in clauses) {
          if (argIndex < clause.head.args.length) {
            final topArg = clause.head.args[argIndex];

            // Navigate to the nested position
            final termAtPosition = _getTermAtPath(topArg, position.path);

            // Only variables count towards coverage
            if (termAtPosition is ast.VarTerm) {
              if (termAtPosition.isReader) {
                hasReader = true;
              } else {
                hasWriter = true;
              }
            }
            // Constructor at this position doesn't contribute to mode coverage
          }
        }

        // Check for missing modes
        final typeDef = typeEnv.getType(position.typeName);
        if (typeDef == null) continue;

        // Find which primitive modes are required
        bool requiresWriter = false;
        bool requiresReader = false;

        for (final alt in typeDef.alternatives) {
          if (alt is PrimitiveModeAlt) {
            if (alt.isInput) {
              requiresReader = true;
            } else {
              requiresWriter = true;
            }
          }
        }

        // Report missing modes
        if ((requiresWriter && !hasWriter) || (requiresReader && !hasReader)) {
          final missingModes = <String>[];
          if (requiresWriter && !hasWriter) {
            missingModes.add('writer (_)');
          }
          if (requiresReader && !hasReader) {
            missingModes.add('reader (_?)');
          }

          final positionDesc = position.path.isEmpty
              ? 'argument ${argIndex + 1}'
              : 'argument ${argIndex + 1} at path "${position.path}"';

          errors.add(ModeError(
            'Incomplete mode coverage for $positionDesc of ${procDecl.name}/${procDecl.arity}\n'
            'Type ${position.typeName} at this position requires both writer and reader modes.\n'
            'Clauses provide: ${hasWriter ? "writer (_)" : "(no writer)"}, ${hasReader ? "reader (_?)" : "(no reader)"}\n'
            'Missing: ${missingModes.join(", ")}\n\n'
            'Under ::= semantics, clauses must collectively cover all mode alternatives.',
            procDecl.line,
            procDecl.column,
          ));
        }
      }
    }

    return errors;
  }

  /// Get a human-readable description of a term for error messages
  String _termDescription(ast.Term term) {
    if (term is ast.StructTerm) {
      return 'constructor ${term.functor}/${term.args.length}';
    } else if (term is ast.ListTerm) {
      if (term.isNil) {
        return 'empty list []';
      }
      return 'list pattern [...]';
    } else if (term is ast.ConstTerm) {
      return 'constant ${term.value}';
    } else {
      return term.toString();
    }
  }

  /// Check if a moded type has no mode complementations (no ? in definition).
  /// Per spec Section 7.3: Output ∩ T = T iff T has no mode complementations.
  bool _hasNoModeComplementations(String typeName, Set<String> visited) {
    // Prevent infinite recursion on recursive types
    if (visited.contains(typeName)) return true;

    // Built-in types have no complementations
    if (typeName == 'Number' || typeName == 'String') return true;
    if (typeName == 'Output' || typeName == 'Input') return true;

    // Primitive modes
    if (typeName == '_') return true;
    if (typeName == '_?') return false;  // Has complementation

    final typeDef = typeEnv.getType(typeName);
    if (typeDef == null) return true;

    visited.add(typeName);

    for (final alt in typeDef.alternatives) {
      if (_altHasModeComplementation(alt, visited)) return false;
    }
    return true;
  }

  bool _altHasModeComplementation(TypeExpr alt, Set<String> visited) {
    if (alt is PrimitiveModeAlt) {
      return alt.isInput;  // _? has complementation
    }
    if (alt is TypeRef) {
      if (alt.isInput) return true;  // T? has complementation
      return !_hasNoModeComplementations(alt.name, visited);
    }
    if (alt is StructAlt) {
      for (final arg in alt.args) {
        if (_altHasModeComplementation(arg, visited)) return true;
      }
      return false;
    }
    if (alt is ListConsAlt) {
      return _altHasModeComplementation(alt.head, visited) ||
             _altHasModeComplementation(alt.tail, visited);
    }
    if (alt is DiffListAlt) {
      return _altHasModeComplementation(alt.content, visited) ||
             _altHasModeComplementation(alt.hole, visited);
    }
    // ConstantAlt, ListNilAlt - no complementation
    return false;
  }

  /// Check ground guards are WMT: ground(X?) requires X's type has no mode complementations.
  /// Per spec Section 7.3.1: ground(X?) is WMT iff Input ∩ T_X = T_X.
  List<ModeError> checkGroundGuards(ast.Clause clause, Map<String, String> varTypes) {
    final errors = <ModeError>[];

    if (clause.guards == null) return errors;

    for (final guard in clause.guards!) {
      if (guard.predicate == 'ground' && guard.args.length == 1) {
        final arg = guard.args[0];
        if (arg is ast.VarTerm) {
          final typeName = varTypes[arg.name];
          if (typeName != null) {
            if (!_hasNoModeComplementations(typeName, <String>{})) {
              errors.add(ModeError(
                'Ground guard not WMT: type $typeName has mode complementations. '
                'ground(${arg.name}) requires Input ∩ $typeName = $typeName, '
                'but $typeName contains ? in its definition.',
                guard.line,
                guard.column,
              ));
            }
          }
        }
      }
    }

    return errors;
  }
}
