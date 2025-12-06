import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';

void main(List<String> args) {
  final src = args[0];
  final compiler = GlpCompiler();
  final prog = compiler.compile(src);

  for (int i = 0; i < prog.ops.length; i++) {
    final op = prog.ops[i];
    if (op is PutStructure) {
      print('  $i: PutStructure(functor="${op.functor}", arity=${op.arity}, argSlot=${op.argSlot})');
    } else if (op is SetConstant) {
      print('  $i: SetConstant(value=${op.value})');
    } else if (op is Spawn) {
      print('  $i: Spawn(label=${op.procedureLabel}, arity=${op.arity})');
    } else {
      print('  $i: $op');
    }
  }
}
