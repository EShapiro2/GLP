import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' as bc;

void main() {
  group('GlpCompiler', () {
    test('compiles simple fact', () {
      final compiler = GlpCompiler();
      final source = 'p(a).';

      final program = compiler.compile(source);

      expect(program.ops, isNotEmpty);
      expect(program.labels, contains('p/1'));

      // Should have: Label, ClauseTry, HeadConstant, Commit, Proceed, Label, NoMoreClauses
      expect(program.ops[0], isA<bc.Label>());
      expect(program.ops[1], isA<bc.ClauseTry>());
      expect(program.ops[2], isA<bc.HeadConstant>());
      expect(program.ops[3], isA<bc.Commit>());
      expect(program.ops[4], isA<bc.Proceed>());
      expect(program.ops[5], isA<bc.Label>());
      expect(program.ops[6], isA<bc.NoMoreClauses>());
    });

    test('compiles clause with variables', () {
      final compiler = GlpCompiler();
      final source = 'p(X) :- q(X?).';

      final program = compiler.compile(source);

      expect(program.labels, contains('p/1'));

      // Should have GetVariable for X in head, PutReader for X? in body
      final hasGetVariable = program.ops.any((op) => op is bc.GetVariable);
      final hasPutReader = program.ops.any((op) => op is bc.PutReader);
      final hasRequeue = program.ops.any((op) => op is bc.Requeue);

      expect(hasGetVariable, isTrue);
      expect(hasPutReader, isTrue);
      expect(hasRequeue, isTrue);  // Tail call
    });

    test('compiles clause with guards', () {
      final compiler = GlpCompiler();
      final source = 'p(X) :- ground(X?) | q(X?).';

      final program = compiler.compile(source);

      // Should have Ground guard instruction
      final hasGround = program.ops.any((op) => op is bc.Ground);
      expect(hasGround, isTrue);
    });

    test('compiles list matching', () {
      final compiler = GlpCompiler();
      final source = 'p([X|Xs]).';

      final program = compiler.compile(source);

      // Should have HeadStructure (for '.') and Writer instructions
      final hasHeadStruct = program.ops.any((op) => op is bc.HeadStructure);
      final hasWriter = program.ops.any((op) => op is bc.UnifyWriter);

      expect(hasHeadStruct, isTrue);
      expect(hasWriter, isTrue);
    });

    test('compiles empty list', () {
      final compiler = GlpCompiler();
      final source = 'p([]).';

      final program = compiler.compile(source);

      // Should have HeadNil
      final hasHeadNil = program.ops.any((op) => op is bc.HeadNil);
      expect(hasHeadNil, isTrue);
    });

    test('compiles multiple clauses', () {
      final compiler = GlpCompiler();
      final source = '''
        p(a).
        p(b).
      ''';

      final program = compiler.compile(source);

      expect(program.labels, contains('p/1'));

      // Should have 2 ClauseTry instructions (one per clause)
      final clauseTries = program.ops.where((op) => op is bc.ClauseTry).length;
      expect(clauseTries, 2);

      // Should have 2 Commit instructions (one per clause)
      final commits = program.ops.where((op) => op is bc.Commit).length;
      expect(commits, 2);

      // Should have labels for second clause and end
      expect(program.labels, contains('p/1_c1'));
    });

    test('compiles merge/3 example', () {
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
        merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
        merge([], [], []).
      ''';

      final program = compiler.compile(source);

      expect(program.labels, contains('merge/3'));

      // Should have 3 clauses
      final clauseTries = program.ops.where((op) => op is bc.ClauseTry).length;
      expect(clauseTries, 3);

      // Should have HeadStructure instructions for lists (now using '.' functor)
      final headStructs = program.ops.where((op) => op is bc.HeadStructure).length;
      expect(headStructs, greaterThan(0));

      // Should have Reader instructions
      final readers = program.ops.where((op) => op is bc.UnifyReader).length;
      expect(readers, greaterThan(0));

      // Should have Requeue for tail calls
      final requeues = program.ops.where((op) => op is bc.Requeue).length;
      expect(requeues, 2);  // First two clauses have body
    });

    test('compiles structure matching', () {
      final compiler = GlpCompiler();
      final source = 'p(f(X, Y)).';

      final program = compiler.compile(source);

      // Should have HeadStructure
      final hasHeadStruct = program.ops.any((op) => op is bc.HeadStructure);
      expect(hasHeadStruct, isTrue);
    });

    test('compiles spawn for non-tail goals', () {
      final compiler = GlpCompiler();
      final source = 'p(X, Y) :- q(X?), r(Y?).';  // Use different variables to avoid SRSW violation

      final program = compiler.compile(source);

      // First goal should use Spawn, second should use Requeue
      final spawns = program.ops.where((op) => op is bc.Spawn).length;
      final requeues = program.ops.where((op) => op is bc.Requeue).length;

      expect(spawns, 1);      // q(X?)
      expect(requeues, 1);    // r(Y?) in tail position
    });

    test('generates correct labels', () {
      final compiler = GlpCompiler();
      final source = '''
        p(a).
        p(b).
        q(X).
      ''';

      final program = compiler.compile(source);

      // Should have labels for procedures and clause ends
      expect(program.labels, contains('p/1'));
      expect(program.labels, contains('p/1_end'));
      expect(program.labels, contains('p/1_c1'));  // Second clause of p
      expect(program.labels, contains('q/1'));
      expect(program.labels, contains('q/1_end'));
    });

    test('compiles otherwise guard', () {
      final compiler = GlpCompiler();
      final source = 'p(X) :- otherwise | q(X?).';  // Use reader to avoid SRSW violation

      final program = compiler.compile(source);

      final hasOtherwise = program.ops.any((op) => op is bc.Otherwise);
      expect(hasOtherwise, isTrue);
    });

    test('compiles known guard', () {
      final compiler = GlpCompiler();
      final source = 'p(X, Y) :- known(X?) | q(Y?).';  // Use different variables

      final program = compiler.compile(source);

      final hasKnown = program.ops.any((op) => op is bc.Known);
      expect(hasKnown, isTrue);
    });

    test('handles underscore correctly', () {
      final compiler = GlpCompiler();
      final source = 'p(_, X).';

      final program = compiler.compile(source);

      // Underscore should generate UnifyVoid instruction
      final hasVoid = program.ops.any((op) => op is bc.UnifyVoid);
      expect(hasVoid, isTrue);
    });
  });
}
