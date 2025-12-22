// test/analysis/type_checker/test_helpers.dart
//
// Helper functions for type checker tests

import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/compiler/parser.dart';

/// Parse and type-check GLP source code
TypeCheckResult checkTypes(String source) {
  // Parse GLP source to get clauses
  final parser = Parser(source);
  final parseResult = parser.parse();

  if (parseResult.errors.isNotEmpty) {
    throw Exception('Parse errors: ${parseResult.errors}');
  }

  // Parse type declarations
  final typeEnv = parseTypes(source);

  // Run type checker
  final checker = TypeChecker(typeEnv);
  return checker.check(parseResult.clauses);
}
