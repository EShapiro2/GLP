// Debug script to examine bytecode for run6 and run7
import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';

void main() {
  final compiler = GlpCompiler();

  final source = '''
    run4(Ys?) :- run(merge([],[],Ys)).
    run6(Ys?) :- run(merge([a],[],Ys)).
    run7(Ys?) :- run(merge(f(a),[],Ys)).
  ''';

  final program = compiler.compile(source);

  print('=== BYTECODE FOR run4/1 (WORKS) ===');
  final run4Start = program.labels['run4/1'] ?? 0;
  for (var i = run4Start; i < program.ops.length; i++) {
    final op = program.ops[i];
    if (op is PutStructure) {
      print('$i: PutStructure(${op.functor}, ${op.arity}, slot=${op.argSlot})');
    } else if (op is UnifyConstant) {
      print('$i: UnifyConstant(${op.value})');
    } else {
      print('$i: $op');
    }
    if (op is NoMoreClauses) break;
  }

  print('\n=== BYTECODE FOR run6/1 (FAILS - no reductions) ===');
  final run6Start = program.labels['run6/1'] ?? 0;
  for (var i = run6Start; i < program.ops.length; i++) {
    final op = program.ops[i];
    if (op is PutStructure) {
      print('$i: PutStructure(${op.functor}, ${op.arity}, slot=${op.argSlot})');
    } else if (op is UnifyConstant) {
      print('$i: UnifyConstant(${op.value})');
    } else {
      print('$i: $op');
    }
    if (op is NoMoreClauses) break;
  }

  print('\n=== BYTECODE FOR run7/1 (FAILS - no reductions) ===');
  final run7Start = program.labels['run7/1'] ?? 0;
  for (var i = run7Start; i < program.ops.length; i++) {
    final op = program.ops[i];
    if (op is PutStructure) {
      print('$i: PutStructure(${op.functor}, ${op.arity}, slot=${op.argSlot})');
    } else if (op is UnifyConstant) {
      print('$i: UnifyConstant(${op.value})');
    } else {
      print('$i: $op');
    }
    if (op is NoMoreClauses) break;
  }
}
