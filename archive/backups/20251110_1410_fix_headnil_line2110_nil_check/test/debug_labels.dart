import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  final source = '''
    % Clean test - only the minimal program
    clause(p(X?), (q(Y), r(Y?,X))).
    clause(q(a), true).
    clause(r(a,b), true).

    run(true).
    run((A, B)) :- run(A?), run(B?).
    run(A) :- otherwise | clause(A?, B), run(B?).
  ''';

  final compiler = GlpCompiler();
  final prog = compiler.compile(source);

  print('Program labels:');
  for (final entry in prog.labels.entries) {
    print('  ${entry.key} -> PC ${entry.value}');
  }
}
