/// Phase 0: Smoke Test - Baseline for Non-Ground Structure Bug
///
/// This test captures the CURRENT broken behavior where variables
/// in nested structures are converted to ConstTerm(null).
///
/// Expected BEFORE fix:
///   merge(Const(nil), Const(nil), Const(null))  <- BUG: X becomes null
///
/// Expected AFTER fix:
///   merge(Const(nil), Const(nil), Writer(N))    <- CORRECT: X is a writer

import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/terms.dart';

void main() {
  final compiler = GlpCompiler();

  print('='.padRight(70, '='));
  print('SMOKE TEST: Non-Ground Structure Compilation');
  print('='.padRight(70, '='));
  print('');

  // Test case: Variable X in nested structure
  final source = '''
test(Y?) :- bar((merge([],[],X), Y)).
''';

  print('Source code:');
  print(source);
  print('');

  try {
    final program = compiler.compile(source);

    print('Generated ${program.ops.length} instructions:');
    print('');

    for (int i = 0; i < program.ops.length; i++) {
      final op = program.ops[i];
      print('$i: $op');

      // Inspect UnifyConstant instructions - this is where the bug shows up
      if (op.toString().contains('UnifyConstant')) {
        try {
          final opDynamic = op as dynamic;
          if (opDynamic.value != null) {
            final val = opDynamic.value;

            if (val is StructTerm) {
              print('   📦 StructTerm: ${val.functor}/${val.args.length}');
              for (int j = 0; j < val.args.length; j++) {
                final arg = val.args[j];
                if (arg is ConstTerm) {
                  if (arg.value == null) {
                    print('   ❌ Arg $j: Const(null) <- BUG! Should be Writer');
                  } else {
                    print('   ✓ Arg $j: Const(${arg.value})');
                  }
                } else if (arg is WriterTerm) {
                  print('   ✓ Arg $j: Writer(${arg.writerId})');
                } else if (arg is ReaderTerm) {
                  print('   ✓ Arg $j: Reader(${arg.readerId})');
                } else {
                  print('   ? Arg $j: ${arg.runtimeType}');
                }
              }
            } else if (val is ConstTerm) {
              print('   -> ConstTerm(${val.value})');
            } else {
              print('   -> Value: $val (${val.runtimeType})');
            }
          }
        } catch (e) {
          print('   -> Could not inspect: $e');
        }
      }
    }

    print('');
    print('='.padRight(70, '='));
    print('BASELINE CAPTURED - Variable X converted to Const(null)');
    print('='.padRight(70, '='));

  } catch (e) {
    print('');
    print('='.padRight(70, '='));
    print('ERROR (Expected after adding explicit error checks):');
    print('$e');
    print('='.padRight(70, '='));
  }
}
