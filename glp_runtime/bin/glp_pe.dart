// GLP Partial Evaluator - Source-to-Source Transformation
//
// Usage: dart run bin/glp_pe.dart <input.glp> [output.glp]
//
// Performs:
//   Stage 1: Unfold defined guards (unit clauses in guard position)
//   Stage 2: Unfold reduce/2 calls in body against reduce/2 facts

import 'dart:io';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/partial_evaluator.dart';
import 'package:glp_runtime/compiler/glp_printer.dart';

void main(List<String> args) {
  if (args.isEmpty) {
    print('Usage: dart run bin/glp_pe.dart <input.glp> [output.glp]');
    print('');
    print('Partial evaluator for GLP programs.');
    print('Specializes meta-interpreters by unfolding reduce/2 calls.');
    exit(1);
  }

  final inputPath = args[0];
  final outputPath = args.length > 1
      ? args[1]
      : inputPath.replaceAll('.glp', '_specialized.glp');

  print('GLP Partial Evaluator');
  print('  Input:  $inputPath');
  print('  Output: $outputPath');
  print('');

  // Read input file
  final inputFile = File(inputPath);
  if (!inputFile.existsSync()) {
    print('Error: File not found: $inputPath');
    exit(1);
  }

  final source = inputFile.readAsStringSync();

  // Parse
  final lexer = Lexer(source);
  final tokens = lexer.tokenize();
  final parser = Parser(tokens);
  final program = parser.parse();

  // Count clauses across all procedures
  int clauseCount = 0;
  for (final proc in program.procedures) {
    clauseCount += proc.clauses.length;
  }
  print('Parsed ${program.procedures.length} procedures, $clauseCount clauses');
  print('');

  // Stage 1: Unfold defined guards
  print('Stage 1: Unfolding defined guards...');
  final pe = PartialEvaluator();
  final stage1 = pe.transformDefinedGuards(program);
  int stage1Clauses = 0;
  for (final proc in stage1.procedures) {
    stage1Clauses += proc.clauses.length;
  }
  print('  $stage1Clauses clauses after Stage 1');

  // Stage 2: Unfold reduce/2 calls
  print('Stage 2: Unfolding reduce/2 calls...');
  final stage2 = pe.unfoldReduceCalls(stage1);
  int stage2Clauses = 0;
  for (final proc in stage2.procedures) {
    stage2Clauses += proc.clauses.length;
  }
  print('  $stage2Clauses clauses after Stage 2');
  print('');

  // Serialize back to source
  final printer = GlpPrinter();
  final output = printer.printProgram(stage2);

  // Write output
  File(outputPath).writeAsStringSync(output);
  print('Written to: $outputPath');
}
