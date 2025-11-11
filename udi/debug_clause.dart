import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' as bc;

void main() {
  final compiler = GlpCompiler();

  // Compile just the actual run/1 metainterpreter
  final source = '''
    % Metainterpreter clause
    run(A) :- otherwise | clause(A?, B), run(B?).

    % Dummy clause/2 for completeness
    clause(merge([], [], []), true).
  ''';

  final program = compiler.compile(source);

  print('=== BYTECODE for test/2 ===');
  final testStart = program.labels['test/2'];
  if (testStart != null) {
    for (int i = testStart; i < program.ops.length; i++) {
      final op = program.ops[i];
      print('$i: $op');
      if (op is bc.Proceed) break;
    }
  }

  print('\n=== BYTECODE for clause/2 ===');
  final clauseStart = program.labels['clause/2'];
  if (clauseStart != null) {
    for (int i = clauseStart; i < program.ops.length; i++) {
      final op = program.ops[i];
      print('$i: $op');
      if (op is bc.Proceed) break;
    }
  }
}
