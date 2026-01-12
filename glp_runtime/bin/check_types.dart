// bin/check_types.dart
//
// Standalone type checker for GLP programs
// Uses the integrated parser that handles types, procedures, and clauses

import 'dart:io';
import '../lib/compiler/lexer.dart';
import '../lib/compiler/parser.dart';
import '../lib/analysis/type_checker/type_checker.dart';

void main(List<String> args) {
  if (args.isEmpty) {
    print('Usage: dart run bin/check_types.dart <file.glp>');
    exit(1);
  }

  final filePath = args[0];
  final file = File(filePath);

  if (!file.existsSync()) {
    print('Error: File not found: $filePath');
    exit(1);
  }

  final source = file.readAsStringSync();
  
  // Verbose mode
  final verbose = args.contains('-v') || args.contains('--verbose');

  if (verbose) {
    print('=== Type Checking: $filePath ===\n');
  }

  try {
    // Parse using main parser (handles types, procedures, clauses)
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final module = parser.parseModule();

    if (verbose) {
      print('Parsed:');
      print('  ${module.typeDefs.length} type definition(s)');
      print('  ${module.procDeclarations.length} procedure declaration(s)');
      print('  ${module.procedures.length} procedure(s) with clauses');
      final clauseCount = module.procedures
          .fold<int>(0, (sum, proc) => sum + proc.clauses.length);
      print('  $clauseCount total clause(s)');
      print('');
    }

    // Type check the module
    final result = checkModule(module);

    // Display results
    if (result.isWellTyped && result.warnings.isEmpty) {
      print('✓ Program is well-typed');
      exit(0);
    } else {
      if (result.errors.isNotEmpty) {
        print('Type Errors (${result.errors.length}):');
        for (final err in result.errors) {
          print('  ✗ $err');
        }
      }
      if (result.warnings.isNotEmpty) {
        print('Warnings (${result.warnings.length}):');
        for (final warn in result.warnings) {
          print('  ⚠ $warn');
        }
      }
      exit(result.isWellTyped ? 0 : 1);
    }
  } on FormatException catch (e) {
    print('Parse Error: $e');
    exit(2);
  } catch (e, st) {
    print('Error: $e');
    if (verbose) {
      print('Stack trace:\n$st');
    }
    exit(2);
  }
}
