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
      final hasSpawn = program.ops.any((op) => op is bc.Spawn);
      final hasProceed = program.ops.any((op) => op is bc.Proceed);

      expect(hasGetVariable, isTrue);
      expect(hasPutReader, isTrue);
      expect(hasSpawn, isTrue);    // All goals spawned
      expect(hasProceed, isTrue);  // Parent terminates
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

      // Should have Reader instructions (v1 UnifyReader or v2 UnifyVariable with isReader=true)
      final readersV1 = program.ops.where((op) => op is bc.UnifyReader).length;
      final readersV2 = program.ops.where((op) {
        // Check if it's a UnifyVariable with isReader=true
        if (op.runtimeType.toString() == 'UnifyVariable') {
          try {
            // Use dynamic access to check isReader field
            final dynamic dynOp = op;
            return dynOp.isReader == true;
          } catch (e) {
            return false;
          }
        }
        return false;
      }).length;
      expect(readersV1 + readersV2, greaterThan(0));

      // Should have Spawn for all goals (tail recursion removed)
      final spawns = program.ops.where((op) => op is bc.Spawn).length;
      final proceeds = program.ops.where((op) => op is bc.Proceed).length;
      expect(spawns, 2);     // First two clauses each spawn one goal
      expect(proceeds, 3);   // All three clauses proceed (third has empty body)
    });

    test('compiles structure matching', () {
      final compiler = GlpCompiler();
      final source = 'p(f(X, Y)).';

      final program = compiler.compile(source);

      // Should have HeadStructure
      final hasHeadStruct = program.ops.any((op) => op is bc.HeadStructure);
      expect(hasHeadStruct, isTrue);
    });

    test('compiles spawn for all goals (tail recursion removed)', () {
      final compiler = GlpCompiler();
      final source = 'p(X, Y) :- q(X?), r(Y?).';  // Use different variables to avoid SRSW violation

      final program = compiler.compile(source);

      // ALL goals should use Spawn (tail recursion removed)
      final spawns = program.ops.where((op) => op is bc.Spawn).length;
      final requeues = program.ops.where((op) => op is bc.Requeue).length;
      final proceeds = program.ops.where((op) => op is bc.Proceed).length;

      expect(spawns, 2);      // q(X?) and r(Y?)
      expect(requeues, 0);    // No tail calls
      expect(proceeds, 1);    // Parent terminates after spawning
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
