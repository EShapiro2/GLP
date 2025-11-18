import 'package:glp_runtime/compiler/compiler.dart';

void main(List<String> args) async {
  if (args.isEmpty) {
    print('Usage: dart dump_bc_detailed.dart <file.glp>');
    return;
  }
  
  final compiler = GlpCompiler();
  final result = compiler.compileFile(args[0]);
  
  print('DETAILED BYTECODE for ${args[0]}');
  print('=' * 70);
  
  for (var i = 0; i < result.ops.length; i++) {
    final op = result.ops[i];
    print('PC $i: $op');
  }
}
