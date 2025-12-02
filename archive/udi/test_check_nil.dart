import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/terms.dart';

void main() {
  final compiler = GlpCompiler();

  // Test what the compiler produces for empty lists in structures
  final source = '''
test1 :- foo(merge([],[],X)).
test2(Y?) :- bar((merge([],[],X), Y)).
''';

  print('Compiling: $source');
  final program = compiler.compile(source);

  print('\nGenerated ${program.ops.length} instructions');
  for (int i = 0; i < program.ops.length && i < 30; i++) {
    final op = program.ops[i];
    print('$i: $op');

    // If it's UnifyConstant, show what value it has
    if (op.toString().contains('UnifyConstant')) {
      // Try to access the value
      try {
        final opDynamic = op as dynamic;
        if (opDynamic.value != null) {
          final val = opDynamic.value;
          if (val is ConstTerm) {
            print('   -> ConstTerm(${val.value})');
          } else {
            print('   -> Value: $val (${val.runtimeType})');
          }
        }
      } catch (e) {
        print('   -> Could not inspect value');
      }
    }
  }
}
