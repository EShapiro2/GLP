import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  print('Testing Spawn instruction labels');
  print('Source: p(X?) :- q(Y), r(Y?,X).');
  print('');

  final source = 'p(X?) :- q(Y), r(Y?,X).';
  final compiler = GlpCompiler();

  try {
    final result = compiler.compileWithMetadata(source);

    print('Compilation successful!');
    print('Variable map: ${result.variableMap}');
    print('');
    print('Bytecode:');
    for (int i = 0; i < result.program.ops.length; i++) {
      final op = result.program.ops[i];
      String details = '';

      // Extract details for each instruction type
      if (op.runtimeType.toString() == 'Spawn') {
        try {
          final label = (op as dynamic).procedureLabel;
          final arity = (op as dynamic).arity;
          details = ' (label=$label, arity=$arity)';
        } catch (e) {}
      }

      if (op.runtimeType.toString() == 'Requeue') {
        try {
          final label = (op as dynamic).procedureLabel;
          final arity = (op as dynamic).arity;
          details = ' (label=$label, arity=$arity)';
        } catch (e) {}
      }

      if (op.runtimeType.toString() == 'PutWriter' ||
          op.runtimeType.toString() == 'PutReader') {
        try {
          final varIndex = (op as dynamic).varIndex;
          final argSlot = (op as dynamic).argSlot;
          details = ' (varIndex=$varIndex, argSlot=$argSlot)';
        } catch (e) {}
      }

      print('  $i: ${op.runtimeType}$details');
    }
  } catch (e, stack) {
    print('Compilation failed: $e');
    print(stack);
  }
}
