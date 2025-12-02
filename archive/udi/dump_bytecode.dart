// dump_bytecode.dart - Compile .glp file and dump bytecode disassembly
import 'dart:io';
import 'package:glp_runtime/compiler/compiler.dart';

void main(List<String> args) {
  if (args.isEmpty) {
    print('Usage: dart dump_bytecode.dart <file.glp>');
    exit(1);
  }

  final glpFile = args[0];
  final file = File(glpFile);

  if (!file.existsSync()) {
    print('Error: File not found: $glpFile');
    exit(1);
  }

  print('Compiling: $glpFile');
  print('=' * 70);

  final source = file.readAsStringSync();
  final compiler = GlpCompiler();

  try {
    final program = compiler.compile(source);

    print('✓ Compilation successful\n');
    print('BYTECODE DISASSEMBLY:');
    print('=' * 70);
    print(program.toDisassembly());
  } catch (e) {
    print('COMPILATION FAILED:');
    print('  $e');
    exit(1);
  }
}
