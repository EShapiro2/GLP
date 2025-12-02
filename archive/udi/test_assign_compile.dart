import 'package:glp_runtime/compiler/compiler.dart';
import 'dart:io';

void main() {
  try {
    final source = File('../stdlib/assign.glp').readAsStringSync();
    // Use skipSRSW for system predicates
    final compiler = GlpCompiler(skipSRSW: true);
    final result = compiler.compile(source);
    print('=== COMPILATION SUCCESSFUL ===');
    print('Instructions: ${result.ops.length}');
  } catch (e) {
    print('ERROR: $e');
  }
}
