import 'package:glp_runtime/compiler/compiler.dart';
import 'dart:io';

void main() {
  final testFiles = [
    'glp/merge.glp',
    'glp/qsort.glp',
    'glp/hello.glp',
  ];

  var passed = 0;
  var failed = 0;

  for (final file in testFiles) {
    try {
      final fullPath = '/home/user/GLP/udi/$file';
      if (!File(fullPath).existsSync()) {
        print('SKIP: $file (file not found)');
        continue;
      }
      final source = File(fullPath).readAsStringSync();
      final compiler = GlpCompiler();
      final result = compiler.compile(source);
      print('PASS: $file (${result.ops.length} instructions)');
      passed++;
    } catch (e) {
      print('FAIL: $file - $e');
      failed++;
    }
  }

  print('\n=== Summary: $passed passed, $failed failed ===');
}
