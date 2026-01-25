import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/opcodes_v2.dart' as v2;

void main(List<String> args) {
  final src = args[0];
  final compiler = GlpCompiler();
  final prog = compiler.compile(src);

  for (int i = 0; i < prog.ops.length; i++) {
    final op = prog.ops[i];
    if (op is HeadStructure) {
      print('  $i: HeadStructure(argSlot=${op.argSlot}, functor="${op.functor}", arity=${op.arity})');
    } else if (op is Ground) {
      print('  $i: Ground(varIndex=${op.varIndex})');
    } else if (op is v2.UnifyVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      print('  $i: UnifyVariable(varIndex=${op.varIndex}, mode=$mode)');
    } else if (op is v2.GetVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      print('  $i: GetVariable(varIndex=${op.varIndex}, argSlot=${op.argSlot}, mode=$mode)');
    } else if (op is v2.PutVariable) {
      final mode = op.isReader ? 'reader' : 'writer';
      print('  $i: PutVariable(varIndex=${op.varIndex}, argSlot=${op.argSlot}, mode=$mode)');
    } else {
      print('  $i: $op');
    }
  }
}
