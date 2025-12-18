// bin/check_types.dart
//
// Standalone type checker for GLP programs
// Tests the type checking integration on a GLP file

import 'dart:io';
import '../lib/compiler/ast.dart' as ast;
import '../lib/compiler/lexer.dart';
import '../lib/compiler/parser.dart';
import '../lib/analysis/type_checker/type_parser.dart';
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

  print('=== Type Checking: $filePath ===\n');

  // Step 1: Parse type declarations
  print('Step 1: Parsing type declarations...');
  final typeEnv = parseTypes(source);
  print('  Found ${typeEnv.types.length} type(s)');
  print('  Found ${typeEnv.procedures.length} procedure(s)');
  for (final procDecl in typeEnv.procedures.values) {
    print('    - $procDecl');
  }
  print('');

  // Step 2: Strip type declarations from source (main lexer doesn't recognize ::=)
  final sourceWithoutTypes = _stripTypeDeclarations(source);

  // Step 3: Parse clauses
  print('Step 2: Parsing clauses...');
  final lexer = Lexer(sourceWithoutTypes);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);

  ast.Program program;
  try {
    program = parser.parse();
    final clauseCount = program.procedures
        .fold<int>(0, (sum, proc) => sum + proc.clauses.length);
    print('  Parsed ${program.procedures.length} procedure(s)');
    print('  Total $clauseCount clause(s)');
  } catch (e) {
    print('  Parse error: $e');
    exit(1);
  }
  print('');

  // Step 3: Collect all clauses
  final allClauses = <ast.Clause>[];
  for (final proc in program.procedures) {
    allClauses.addAll(proc.clauses);
  }

  // Step 4: Run type checker
  print('Step 3: Running type checker...');
  final checker = TypeChecker(typeEnv);
  final result = checker.check(allClauses);
  print('');

  // Step 5: Display results
  print('=== Results ===');
  if (result.isWellTyped && result.warnings.isEmpty) {
    print('✓ Program is well-typed!');
  } else {
    if (result.errors.isNotEmpty) {
      print('Errors (${result.errors.length}):');
      for (final err in result.errors) {
        print('  $err');
      }
    }
    if (result.warnings.isNotEmpty) {
      print('Warnings (${result.warnings.length}):');
      for (final warn in result.warnings) {
        print('  $warn');
      }
    }
  }

  exit(result.isWellTyped ? 0 : 1);
}

/// Strip type declarations (lines with ::= and procedure) from source
/// so the main GLP lexer doesn't choke on them
String _stripTypeDeclarations(String source) {
  final lines = source.split('\n');
  final filtered = <String>[];

  for (final line in lines) {
    final trimmed = line.trim();
    // Skip type definitions (TypeName ::= ...)
    if (trimmed.contains('::=')) continue;
    // Skip procedure declarations (procedure name(...))
    if (trimmed.startsWith('procedure ')) continue;
    // Skip module directives (-stdlib, -module, etc.)
    if (trimmed.startsWith('-') && trimmed.endsWith('.')) continue;
    // Keep everything else
    filtered.add(line);
  }

  return filtered.join('\n');
}

