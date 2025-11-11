import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = 'clause(merge([], [], []), true).';

  final compiler = GlpCompiler();
  final result = compiler.compileWithMetadata(source);

  print('Variable map: ${result.variableMap}');
  print('');
  print('Bytecode:');
  for (int i = 0; i < result.program.ops.length; i++) {
    final op = result.program.ops[i];
    String details = '';

    if (op.runtimeType.toString() == 'UnifyWriter') {
      try {
        final varIndex = (op as dynamic).varIndex;
        details = ' (varIndex=$varIndex)';
      } catch (e) {}
    }

    if (op.runtimeType.toString() == 'HeadNil') {
      try {
        final argSlot = (op as dynamic).argSlot;
        details = ' (argSlot=$argSlot)';
      } catch (e) {}
    }

    print('  $i: ${op.runtimeType}$details');
  }
}
