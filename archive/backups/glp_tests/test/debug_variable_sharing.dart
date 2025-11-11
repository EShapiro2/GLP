import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  print('Testing variable sharing in regular clause body');
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

      // Extract varIndex for instructions that have it
      if (op.runtimeType.toString() == 'UnifyWriter' ||
          op.runtimeType.toString() == 'UnifyReader' ||
          op.runtimeType.toString() == 'GetVariable' ||
          op.runtimeType.toString() == 'SetWriter' ||
          op.runtimeType.toString() == 'SetReader' ||
          op.runtimeType.toString() == 'PutWriter' ||
          op.runtimeType.toString() == 'PutReader') {
        try {
          final varIndex = (op as dynamic).varIndex;
          final argSlot = (op as dynamic).argSlot;
          details = ' (varIndex=$varIndex, argSlot=$argSlot)';
        } catch (e) {
          // Ignore if field doesn't exist
        }
      }

      if (op.runtimeType.toString() == 'HeadStructure') {
        try {
          final functor = (op as dynamic).functor;
          final arity = (op as dynamic).arity;
          final argSlot = (op as dynamic).argSlot;
          details = ' ($functor/$arity, arg=$argSlot)';
        } catch (e) {}
      }

      print('  $i: ${op.runtimeType}$details');
    }
  } catch (e, stack) {
    print('Compilation failed: $e');
    print(stack);
  }
}
