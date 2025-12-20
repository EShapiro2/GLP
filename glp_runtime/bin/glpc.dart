// bin/glpc.dart
//
// GLP Compiler - Standalone compilation tool with type checking support

import 'dart:io';
import '../lib/compiler/compiler.dart';

void main(List<String> args) {
  // Parse command-line arguments
  var typeCheck = false;
  var strictTypes = false;
  String? inputFile;

  for (int i = 0; i < args.length; i++) {
    final arg = args[i];
    if (arg == '--type-check' || arg == '-t') {
      typeCheck = true;
    } else if (arg == '--strict' || arg == '-s') {
      strictTypes = true;
    } else if (arg == '--help' || arg == '-h') {
      _printUsage();
      exit(0);
    } else if (!arg.startsWith('-')) {
      inputFile = arg;
    } else {
      print('Unknown option: $arg');
      _printUsage();
      exit(1);
    }
  }

  if (inputFile == null) {
    print('Error: No input file specified');
    _printUsage();
    exit(1);
  }

  // Read input file
  final file = File(inputFile);
  if (!file.existsSync()) {
    print('Error: File not found: $inputFile');
    exit(1);
  }

  final source = file.readAsStringSync();

  // Compile with options
  print('Compiling: $inputFile');
  if (typeCheck) {
    print('Type checking: enabled');
    if (strictTypes) {
      print('Mode: strict (abort on errors)');
    } else {
      print('Mode: warning (continue on errors)');
    }
  }
  print('');

  try {
    final compiler = GlpCompiler();
    final options = CompileOptions(
      typeCheck: typeCheck,
      strictTypes: strictTypes,
    );

    final result = compiler.compileWithMetadata(source, options);

    print('✓ Compilation successful!');
    print('  Generated ${result.program.ops.length} bytecode instructions');

    exit(0);
  } catch (e) {
    print('✗ Compilation failed:');
    print('  $e');
    exit(1);
  }
}

void _printUsage() {
  print('''
GLP Compiler (glpc)

Usage: dart run bin/glpc.dart [options] <file.glp>

Options:
  -t, --type-check    Enable type checking (Yardeni-Shapiro types)
  -s, --strict        Abort compilation on type errors (requires --type-check)
  -h, --help          Show this help message

Examples:
  dart run bin/glpc.dart counter.glp                  # Compile without type checking
  dart run bin/glpc.dart --type-check counter.glp     # Compile with type checking (warnings)
  dart run bin/glpc.dart -t -s counter.glp            # Compile with strict type checking

Type Declarations:
  Add type declarations to your .glp files:
    CounterMsg ::= clear ; up ; down ; show(Number).
    procedure counter(CounterStream, Number).
''');
}
