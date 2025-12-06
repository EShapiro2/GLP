import 'package:glp_runtime/compiler/compiler.dart';
import 'dart:io';

void main() {
  final compiler = GlpCompiler();
  
  // Load stdlib
  final stdlibFiles = ['glp/stdlib/assign.glp', 'glp/stdlib/univ.glp', 
                        'glp/stdlib/unify.glp', 'glp/stdlib/mwm.glp'];
  for (final f in stdlibFiles) {
    try {
      compiler.compileFile(File(f).readAsStringSync(), f);
    } catch (e) {}
  }
  
  // Load test file
  final src = File('glp/struct_in_list_bug.glp').readAsStringSync();
  final prog = compiler.compileFile(src, 'struct_in_list_bug.glp');
  
  print('Procedure table:');
  for (final entry in compiler.procTable.entries) {
    if (entry.key.contains('test') || entry.key.contains('bar')) {
      print('  ${entry.key} -> PC ${entry.value}');
    }
  }
  
  print('\nBytecode from PC 1140 onwards:');
  final allOps = compiler.allOps;
  for (int i = 1140; i < allOps.length && i < 1185; i++) {
    final op = allOps[i];
    print('  $i: $op');
  }
}
