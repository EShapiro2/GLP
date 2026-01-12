// test/golden_test.dart
//
// Golden test runner for .glp type checker tests
// Discovers all .glp files in test/programs/typechecker/ and runs them

import 'dart:io';
import 'package:test/test.dart';
import 'package:path/path.dart' as path;
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/analysis/type_checker/type_checker.dart';
import 'package:glp_runtime/analysis/type_checker/program_dfa.dart';
import 'package:glp_runtime/analysis/type_checker/type_parser.dart';
import 'golden/golden_parser.dart';
import 'golden/golden_verifier.dart';

void main() {
  final testDir = Directory('test/programs/typechecker');
  
  if (!testDir.existsSync()) {
    fail('Test directory not found: ${testDir.path}');
  }

  // Find all .glp files recursively
  final glpFiles = testDir
      .listSync(recursive: true)
      .whereType<File>()
      .where((f) => f.path.endsWith('.glp'))
      .toList();

  if (glpFiles.isEmpty) {
    fail('No .glp test files found in ${testDir.path}');
  }

  // Group by subdirectory for better test organization
  final positiveFiles = glpFiles
      .where((f) => f.path.contains('/positive/'))
      .toList()
    ..sort((a, b) => a.path.compareTo(b.path));

  final negativeFiles = glpFiles
      .where((f) => f.path.contains('/negative/'))
      .toList()
    ..sort((a, b) => a.path.compareTo(b.path));

  group('Golden Tests - Positive (should pass)', () {
    for (final file in positiveFiles) {
      final relativePath = path.relative(file.path, from: testDir.path);
      
      test(relativePath, () {
        runGoldenTest(file);
      });
    }
  });

  group('Golden Tests - Negative (should fail)', () {
    for (final file in negativeFiles) {
      final relativePath = path.relative(file.path, from: testDir.path);
      
      test(relativePath, () {
        runGoldenTest(file);
      });
    }
  });
}

void runGoldenTest(File file) {
  final source = file.readAsStringSync();
  
  // Parse expectations from %% comments
  final expectations = parseGoldenExpectations(source);
  
  // Parse and type check the program
  late TypeCheckResult result;
  late ProgramDFA dfa;
  
  try {
    // Parse using main parser
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final module = parser.parseModule();
    
    // Get the type environment and DFA
    final typeEnv = parseTypes(source);
    dfa = buildProgramDFA(typeEnv);
    
    // Type check
    result = checkModule(module);
  } catch (e, st) {
    if (expectations.expectWellTyped) {
      fail('Parse/check error for ${file.path}: $e\n$st');
    } else {
      // For ill-typed programs, parse errors might be acceptable
      // depending on the expected error category
      if (expectations.expectedErrors.any((e) => e.startsWith('type-def:'))) {
        // Type definition errors might cause parse failures
        return;
      }
      fail('Unexpected parse error for ${file.path}: $e');
    }
  }

  // Verify basic pass/fail expectation
  if (expectations.expectWellTyped) {
    expect(result.isWellTyped, isTrue,
        reason: 'Expected well-typed but got errors:\n'
            '${result.errors.map((e) => "  - $e").join("\n")}\n'
            'Test: ${expectations.testName}\n'
            'File: ${file.path}');
  } else {
    expect(result.isWellTyped, isFalse,
        reason: 'Expected ill-typed but program passed\n'
            'Test: ${expectations.testName}\n'
            'File: ${file.path}');
  }

  // For positive tests with automaton expectations, verify them
  if (expectations.expectWellTyped && expectations.hasAutomatonExpectations) {
    final autoResult = verifyAutomaton(dfa, expectations);
    if (!autoResult.passed) {
      fail('Automaton verification failed:\n'
          '${autoResult.failures.map((f) => "  - $f").join("\n")}\n'
          'Test: ${expectations.testName}\n'
          'File: ${file.path}');
    }
  }

  // For positive tests with clause expectations, verify them
  if (expectations.expectWellTyped && expectations.hasClauseExpectations) {
    final clauseResult = verifyModedClauses(result, expectations);
    if (!clauseResult.passed) {
      fail('Moded clause verification failed:\n'
          '${clauseResult.failures.map((f) => "  - $f").join("\n")}\n'
          'Test: ${expectations.testName}\n'
          'File: ${file.path}');
    }
    // Report warnings but don't fail
    if (clauseResult.warnings.isNotEmpty) {
      print('Warnings for ${expectations.testName}:');
      for (final w in clauseResult.warnings) {
        print('  ⚠ $w');
      }
    }
  }
}
