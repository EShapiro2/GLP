import 'package:glp_runtime/compiler/compiler.dart';
import 'dart:io';

void main() {
  final source = 'qsort([], Rest?, Rest).';
  final compiler = GlpCompiler();
  final result = compiler.compile(source);

  print('=== qsort([], Rest?, Rest) ===');
  for (var i = 0; i < result.ops.length; i++) {
    print('  $i: ${result.ops[i]}');
  }
}
