import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = File('/Users/udi/GLP/udi/glp/test_merge_reader.glp').readAsStringSync();

  final compiler = GlpCompiler();
  final prog = compiler.compile(source);

  print('Program labels:');
  final sortedLabels = prog.labels.entries.toList()
    ..sort((a, b) => a.value.compareTo(b.value));
  for (final entry in sortedLabels) {
    print('  PC ${entry.value.toString().padLeft(3)}: ${entry.key}');
  }
}
