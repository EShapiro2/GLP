import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';

void main(List<String> args) {
  final file = args[0];
  final src = File(file).readAsStringSync();
  final compiler = GlpCompiler();
  final prog = compiler.compile(src);
  
  for (int i = 0; i < prog.ops.length; i++) {
    print('  $i: ${prog.ops[i]}');
  }
}
