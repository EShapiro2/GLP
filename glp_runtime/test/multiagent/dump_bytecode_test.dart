/// Debug: Full bytecode dump for social_graph/3
import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:test/test.dart';

void main() {
  test('dump all social_graph bytecode', () async {
    final source = File('/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp').readAsStringSync();
    final compiler = GlpCompiler();
    final program = compiler.compile(source);
    
    // Find all labels
    print('=== All Labels ===');
    for (final entry in program.labels.entries) {
      print('  ${entry.key} -> PC ${entry.value}');
    }
    
    // Print bytecode for social_graph/3 until we hit another label at /3 or /4
    final sgPC = program.labels['social_graph/3']!;
    print('\n=== Full Bytecode for social_graph/3 (starting at PC $sgPC) ===');
    for (int i = sgPC; i < program.ops.length; i++) {
      final op = program.ops[i];
      print('  $i: $op');
      // Stop at NoMoreClauses or SuspendEnd for this predicate
      if (op.runtimeType.toString() == 'NoMoreClauses' || 
          op.runtimeType.toString() == 'SuspendEnd') {
        print('  ${i+1}: ${program.ops[i+1]}');
        break;
      }
    }
  });
}
