import 'package:glp_runtime/glp_runtime.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/compiler/compiler.dart';
import 'dart:io';

void main(List<String> args) {
  if (args.isEmpty) {
    print('Usage: dart dump_bc.dart <filename.glp>');
    exit(1);
  }

  final filename = args[0];
  final source = File(filename).readAsStringSync();

  print('Compiling: $filename');
  print('=' * 70);

  final compiler = GlpCompiler();
  BytecodeProgram prog;

  try {
    final result = compiler.compileWithMetadata(source);
    prog = result.program;
    print('✓ Compilation successful\n');
  } catch (e) {
    print('✗ Compilation failed: $e');
    exit(1);
  }

  print('BYTECODE DISASSEMBLY:');
  print('=' * 70);

  for (int i = 0; i < prog.ops.length; i++) {
    final op = prog.ops[i];
    print('PC $i: ${_formatOp(op)}');
  }
}

String _formatOp(Object op) {
  if (op is bc.Label) return 'Label(${op.name})';
  if (op is bc.ClauseTry) return 'ClauseTry';
  if (op is bc.ClauseNext) return 'ClauseNext(${op.label})';
  if (op is bc.Commit) return 'Commit';
  if (op is bc.Proceed) return 'Proceed';
  if (op is bc.NoMoreClauses) return 'NoMoreClauses';

  if (op is bc.HeadStructure) return 'HeadStructure(${op.functor}, ${op.arity}, A${op.argSlot})';
  if (op is bc.HeadNil) return 'HeadNil(A${op.argSlot})';
  if (op is bc.HeadConstant) return 'HeadConstant(${op.value}, A${op.argSlot})';

  if (op is bc.UnifyWriter) return 'UnifyWriter(X${op.varIndex})';
  if (op is bc.UnifyReader) return 'UnifyReader(X${op.varIndex})';
  if (op is bc.UnifyConstant) return 'UnifyConstant(${op.value})';
  if (op is bc.UnifyVoid) return 'UnifyVoid(${op.count})';

  if (op is bc.GetWriterVariable) return 'GetWriterVariable(X${op.varIndex}, A${op.argSlot})';
  if (op is bc.GetReaderVariable) return 'GetReaderVariable(X${op.varIndex}, A${op.argSlot})';
  if (op is bc.GetWriterValue) return 'GetWriterValue(X${op.varIndex}, A${op.argSlot})';
  if (op is bc.GetReaderValue) return 'GetReaderValue(X${op.varIndex}, A${op.argSlot})';

  if (op is bc.GetVariable) return 'GetVariable(X${op.varIndex}, A${op.argSlot})';
  if (op is bc.GetValue) return 'GetValue(X${op.varIndex}, A${op.argSlot})';

  if (op is bc.PutStructure) return 'PutStructure(${op.functor}, ${op.arity})';
  if (op is bc.PutBoundNil) return 'PutBoundNil';

  if (op is bc.Spawn) return 'Spawn(${op.procedureLabel}, ${op.arity})';
  if (op is bc.Requeue) return 'Requeue(${op.procedureLabel}, ${op.arity})';

  if (op is bc.Guard) return 'Guard(${op.procedureLabel}, ${op.arity})';
  if (op is bc.Otherwise) return 'Otherwise';

  if (op is bc.Push) return 'Push(X${op.regIndex})';
  if (op is bc.Pop) return 'Pop(X${op.regIndex})';
  if (op is bc.UnifyStructure) return 'UnifyStructure(${op.functor}, ${op.arity})';

  return op.toString();
}
