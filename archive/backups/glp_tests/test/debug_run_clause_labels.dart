import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final runSource = File('/Users/udi/GLP/udi/glp/run.glp').readAsStringSync();
  final clauseSource = File('/Users/udi/GLP/udi/glp/clause.glp').readAsStringSync();
  final combined = runSource + '\n' + clauseSource;

  final compiler = GlpCompiler();
  final prog = compiler.compile(combined);

  print('Program labels:');
  final sortedLabels = prog.labels.entries.toList()
    ..sort((a, b) => a.value.compareTo(b.value));
  for (final entry in sortedLabels) {
    print('  PC ${entry.value.toString().padLeft(3)}: ${entry.key}');
  }
}
