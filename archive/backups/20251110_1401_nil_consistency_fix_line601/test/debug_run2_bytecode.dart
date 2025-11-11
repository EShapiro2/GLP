import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';

void main() {
  final source = 'run2(Ys?) :- run((merge([],[],Xs1), merge([],Xs1?,Ys))).';

  final compiler = GlpCompiler();
  final result = compiler.compileWithMetadata(source);

  print('Variable map: ${result.variableMap}');
  print('Xs1 has varIndex: ${result.variableMap['Xs1']}');
  print('Ys has varIndex: ${result.variableMap['Ys']}');
  print('');
  print('Bytecode for run2/1:');
  for (int i = 0; i < result.program.ops.length; i++) {
    final op = result.program.ops[i];
    String detail = '';
    if (op is PutStructure) {
      detail = ' - functor=${op.functor}, arity=${op.arity}, argSlot=${op.argSlot}';
    } else if (op is UnifyWriter) {
      detail = ' - varIndex=${op.varIndex}';
    } else if (op is UnifyReader) {
      detail = ' - varIndex=${op.varIndex}';
    } else if (op is UnifyConstant) {
      detail = ' - value=${op.value}';
    }
    print('  $i: ${op.runtimeType}$detail');
  }
}
