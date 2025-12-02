import 'package:glp_runtime/compiler/compiler.dart';
import 'dart:io';

void main() {
  final source = File('glp/test_partition.glp').readAsStringSync();
  final compiler = GlpCompiler();
  final result = compiler.compile(source);

  print('=== PARTITION GUARD CLAUSE ===');
  print('Instructions:');
  for (var i = 0; i < result.ops.length; i++) {
    final instr = result.ops[i];
    print('  $i: $instr');
  }
}
