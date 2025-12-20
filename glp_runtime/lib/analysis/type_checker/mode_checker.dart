// lib/analysis/type_checker/mode_checker.dart
//
// Mode checker for moded type system.
// Verifies that variable modes (reader/writer) match type modes (input/output).

import '../../compiler/ast.dart' as ast;
import 'type_ast.dart';
import 'mode.dart';
import 'mode_error.dart';

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

    // Compound terms: recursively check subterms with embedded modes
    if (term is ast.StructTerm && typeExpr is StructAlt) {
      return _checkCompoundMode(term, expectedMode, typeExpr, predicate, argIndex);
    }

    // Constants, lists, etc. - no mode checking needed
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
}
