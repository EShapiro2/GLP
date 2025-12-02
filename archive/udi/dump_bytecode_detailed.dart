import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' as bc;

void main(List<String> args) {
  if (args.isEmpty) {
    print('Usage: dart dump_bytecode_detailed.dart <file.glp>');
    exit(1);
  }

  final filename = args[0];
  final source = File(filename).readAsStringSync();

  final result = compile(source, filename);
  final bytecode = result.program;

  for (int i = 0; i < bytecode.instructions.length; i++) {
    final instr = bytecode.instructions[i];
    String details = instr.toString();

    if (instr is bc.HeadStructure) {
      details = 'HeadStructure("${instr.functor}", ${instr.arity}, argSlot: ${instr.argSlot})';
    } else if (instr is bc.UnifyConstant) {
      details = 'UnifyConstant(${instr.value})';
    } else if (instr is bc.UnifyVariable) {
      details = 'UnifyVariable(X${instr.regIndex}, ${instr.isReader ? "reader" : "writer"})';
    } else if (instr is bc.UnifyReader) {
      details = 'UnifyReader(X${instr.regIndex})';
    } else if (instr is bc.UnifyWriter) {
      details = 'UnifyWriter(X${instr.regIndex})';
    } else if (instr is bc.PutStructure) {
      details = 'PutStructure("${instr.functor}", ${instr.arity}, ${instr.argSlot})';
    } else if (instr is bc.Label) {
      details = 'Label("${instr.name}")';
    }

    print('PC $i: $details');
  }
}
