// test/analysis/type_checker/test_helpers.dart
//
// Helper functions for type checker tests

import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';

/// Parse and type-check GLP source code
TypeCheckResult checkTypes(String source) {
  // Separate type declarations from clauses
  // Type declarations contain ::=
  // Procedure declarations contain 'procedure'
  // Everything else is clause code
  final lines = source.split('\n');
  final clauseLines = <String>[];

  for (final line in lines) {
    final trimmed = line.trim();
    // Skip type declarations and procedure declarations
    if (trimmed.contains('::=') ||
        trimmed.startsWith('procedure ')) {
      continue;
    }
    // Keep clause lines
    if (trimmed.isNotEmpty && !trimmed.startsWith('%')) {
      clauseLines.add(line);
    }
  }

  final clauseSource = clauseLines.join('\n');

  // Parse clauses with GLP lexer/parser
  final lexer = Lexer(clauseSource);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final program = parser.parse();

  // Extract clauses from program
  final clauses = program.procedures.expand((p) => p.clauses).toList();

  // Parse type declarations with TypeLexer/TypeParser
  final typeEnv = parseTypes(source);

  // Run type checker
  final checker = TypeChecker(typeEnv);
  return checker.check(clauses);
}
