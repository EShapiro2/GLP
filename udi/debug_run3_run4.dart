// Debug script to compare run3 vs run4 bytecode
import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';

void main() {
  final compiler = GlpCompiler();

  // Compile both
  final source = '''
    run3(Ys?) :- run(merge([],[a],Ys)).
    run4(Ys?) :- run(merge([],[],Ys)).
  ''';

  final program = compiler.compile(source);

  print('=== BYTECODE FOR run3/1 ===');
  final run3Start = program.labels['run3/1'] ?? 0;
  for (var i = run3Start; i < program.ops.length; i++) {
    final op = program.ops[i];
    print('$i: $op');
    if (op is NoMoreClauses) break;
  }

  print('');
  print('=== BYTECODE FOR run4/1 ===');
  final run4Start = program.labels['run4/1'] ?? 0;
  for (var i = run4Start; i < program.ops.length; i++) {
    final op = program.ops[i];
    print('$i: $op');
    if (op is NoMoreClauses) break;
  }
}
