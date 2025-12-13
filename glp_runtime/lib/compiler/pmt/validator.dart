/// PMT Validator: High-level API for validating GLP programs against mode declarations
///
/// Provides static methods to validate source code or parsed ASTs.

import '../ast.dart';
import '../lexer.dart';
import '../parser.dart';
import 'mode_table.dart';
import 'checker.dart';
import 'errors.dart';

class PmtValidator {
  /// Validate source code against mode declarations.
  ///
  /// Returns a list of PMT errors. Empty list means the program is valid.
  ///
  /// Example:
  /// ```dart
  /// final errors = PmtValidator.validateSource('''
  ///   Merge(A) := merge(List(A)?, List(A)?, List(A)).
  ///   merge([], Ys, Ys).
  ///   merge([X|Xs], Ys, [X|Zs]) :- merge(Xs, Ys, Zs).
  /// ''');
  /// if (errors.isEmpty) print('Valid!');
  /// ```
  static List<PmtError> validateSource(String source) {
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final ast = parser.parseModule();
    return validateAst(ast);
  }

  /// Validate a parsed AST against its mode declarations.
  ///
  /// Returns a list of PMT errors. Empty list means the program is valid.
  ///
  /// Only procedures with corresponding mode declarations are checked.
  /// Procedures without mode declarations are skipped.
  static List<PmtError> validateAst(Module ast) {
    // No mode declarations = nothing to check
    if (ast.modeDeclarations.isEmpty) {
      return [];
    }

    // Build mode table from declarations
    final modeTable = ModeTable.fromDeclarations(ast.modeDeclarations);

    // Create checker with mode table
    final checker = PmtChecker(modeTable);

    // Check all procedures
    final errors = <PmtError>[];
    for (final proc in ast.procedures) {
      errors.addAll(checker.checkProcedure(proc));
    }

    return errors;
  }

  /// Check if source code is valid according to mode declarations.
  ///
  /// Returns true if no PMT errors are found.
  static bool isValid(String source) {
    return validateSource(source).isEmpty;
  }

  /// Validate source and throw [PmtErrors] if invalid.
  ///
  /// Use this when you want to halt on validation failure.
  static void assertValid(String source) {
    final errors = validateSource(source);
    if (errors.isNotEmpty) {
      throw PmtErrors(errors);
    }
  }
}
