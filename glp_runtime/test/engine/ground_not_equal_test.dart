/// =?\= (GLP-Spec appendix-guards.tex at 30e382c): "=?\= succeeds where =?=
/// fails, fails where it succeeds, and suspends where it suspends", =?=
/// succeeding "if both arguments are ground and equal", failing "as soon as
/// the two differ at a pair of ground subterms", and suspending otherwise; the
/// catalogue gives it Ground "yes (both)".  So a pair of ground subterms that
/// differ makes it succeed, whatever readers stand beside the pair, and where
/// no pair differs an unbound reader suspends it.  Where no pair differs and an
/// unbound writer stands in an argument it fails, as before 30e382c: the
/// appendix's "suspends otherwise" and glp.tex's Guards (a guard fails where no
/// instance under a readers substitution succeeds) read differently there, and
/// the paper is to settle it.  test/engine/ground_equality_test.dart takes
/// both guards case by case.
///
/// Fixtures: programs/tests/typed/test_ground_not_equal.glp, and
/// programs/tests/typed/test_ground_equal.glp for =?= beside it.
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/glp_printer.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/token.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' show StructTerm;
import 'package:test/test.dart';

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  for (final f in ['test_ground_equal.glp', 'test_ground_not_equal.glp']) {
    expect(
        engine.loadFile(File('../programs/tests/typed/$f').absolute.path),
        isTrue,
        reason: f);
  }
  return engine;
}

/// The value a binding prints as: `differ` for `Const(differ)`.
String _v(Object? term) =>
    '$term'.replaceAllMapped(RegExp(r'^Const\((.*)\)$'), (m) => m[1]!);

Future<ExecutionResult> _run(String goal) => _engine().runGoal(goal);

void main() {
  group('=?\\= succeeds on ground terms that differ', () {
    test('two constants, both operands variables', () async {
      final r = await _run('test_neq(a, b, R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
    });

    test('two structures differing deep inside', () async {
      final r = await _run('neq_only(f(1, [a, b]), f(1, [a, c]), R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
    });

    test('against a constant, the generic guard call', () async {
      final r = await _run('test_neq_stop(go, R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'go_on');
    });
  });

  group('=?\\= fails on ground terms that are equal', () {
    test('equal structures: the call has no other clause and fails',
        () async {
      final r = await _run('neq_only(f(1, [a, b]), f(1, [a, b]), R)');
      expect(r.status, ExecutionStatus.failed);
    });

    test('equal constants: otherwise takes the call', () async {
      final r = await _run('test_neq(a, a, R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'equal');
    });

    test('equal to the constant, the generic guard call', () async {
      final r = await _run('test_neq_stop(stop, R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'stopped');
    });
  });

  group('=?\\= suspends where =?= suspends', () {
    test('an unbound reader on the left', () async {
      final r = await _run('neq_only(X?, a, R)');
      expect(r.status, ExecutionStatus.suspended);
      final eq = await _run('test(X?, a, R)');
      expect(eq.status, ExecutionStatus.suspended, reason: '=?= suspends');
    });

    test('an unbound reader on the right', () async {
      final r = await _run('neq_only(a, Y?, R)');
      expect(r.status, ExecutionStatus.suspended);
      final eq = await _run('test(a, Y?, R)');
      expect(eq.status, ExecutionStatus.suspended, reason: '=?= suspends');
    });

    test('an unbound reader against a constant, the generic guard call',
        () async {
      final r = await _run('test_neq_stop(X?, R)');
      expect(r.status, ExecutionStatus.suspended);
    });
  });

  group('a pair of ground subterms that differ decides it; where none does, '
      'an unbound reader suspends it', () {
    test('a difference at a pair of ground subterms decides it, unbound '
        'readers beside it', () async {
      final r = await _run('neq_only(f(a, Z?), f(b, W?), R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
    });

    test('nested in a structure against a constant, the generic guard call',
        () async {
      final r = await _run('test_neq_stop(f(Z?), R)');
      expect(r.status, ExecutionStatus.suspended);
    });

    test('it succeeds as well where the readers beside the pair are assigned',
        () async {
      final r = await _run(
          'neq_only(f(a, Z?), f(b, W?), R), test_neq(c, c, Z), test_neq(c, d, W)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
    });

    test('an unbound writer: a pair of ground subterms that differ beside it '
        'decides it; with none, it fails', () async {
      final r = await _run('neq_only(f(a, W), f(b, c), R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
      // No pair differs: the answer ground_equal gave before 30e382c, which
      // the paper is to settle (the library comment).
      final s = await _run('test_neq_stop(f(W), R)');
      expect(s.status, ExecutionStatus.succeeded, reason: '${s.error}');
      expect(_v(s.bindings['R']), 'stopped');
    });

    // No pair differs: the answer ground_equal gave before 30e382c, which the
    // paper is to settle (the library comment).
    test('an unbound writer fails it beside an unbound reader, as ground_equal '
        'takes the writer first', () async {
      final r = await _run('neq_only(X?, f(W), R)');
      expect(r.status, ExecutionStatus.failed);
      // =?= beside it, ground_equal (0x45): it fails too, and otherwise takes
      // the call.
      final eq = await _run('test(X?, f(W), R)');
      expect(eq.status, ExecutionStatus.succeeded, reason: '${eq.error}');
      expect(_v(eq.bindings['R']), 'not_equal');
    });
  });

  group('Ground: yes (both) --- =?\\= grounds both arguments for SRSW', () {
    test('each reader may occur twice in the body', () {
      final program = GlpCompiler().compile(r'''
procedure quad(_?, _?, _?, _?, _).
quad(_, _, _, _, done).
procedure k(_?, _?, _).
k(X, Y, Z?) :- X? =?\= Y? | quad(X?, X?, Y?, Y?, Z).
''');
      expect(program, isNotNull);
    });

    test('the control: a guard that grounds neither refuses the clause', () {
      expect(
          () => GlpCompiler().compile(r'''
procedure quad(_?, _?, _?, _?, _).
quad(_, _, _, _, done).
procedure k(_?, _?, _).
k(X, Y, Z?) :- known(X?), known(Y?) | quad(X?, X?, Y?, Y?, Z).
'''),
          throwsA(predicate((e) => e.toString().contains('SRSW'),
              'an SRSW violation')));
    });

    test('the fixture\'s clause reads each argument twice, and runs', () async {
      final engine = _engine();
      final r = await engine.runGoal('neq_pair(a, b, P)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      final p = r.bindings['P'];
      expect(p, isA<StructTerm>());
      expect((p as StructTerm).functor, 'pair');
      expect(p.args.map((a) => _v(engine.runtime.heap.dereference(a))),
          ['a', 'a', 'b', 'b']);
    });
  });

  group('compilation', () {
    // ground_equal (0x45) carries no negated operand (IGLP
    // code-format-fragment.tex, 9b45225), so =?\= is the generic guard call,
    // by name, whatever its operands.
    test('two variable operands: the generic guard call, =?\\= by name', () {
      final program = GlpCompiler().compile(r'''
procedure ne(_?, _?, _).
ne(X, Y, yes) :- X? =?\= Y? | true.
''');
      final guards = program.ops.whereType<bc.Guard>().toList();
      expect(guards.map((g) => g.procedureLabel), contains(r'=?\='));
      expect(program.ops.whereType<bc.GroundEqual>(), isEmpty);
    });

    test('=?= beside it: two variable operands are ground_equal (0x45)', () {
      final program = GlpCompiler().compile(r'''
procedure eq(_?, _?, _).
eq(X, Y, yes) :- X? =?= Y? | true.
''');
      expect(program.ops.whereType<bc.GroundEqual>(), isNotEmpty);
      expect(program.ops.whereType<bc.Guard>(), isEmpty);
    });

    test('a constant operand: the generic guard call, =?\\= by name', () {
      final program = GlpCompiler().compile(r'''
procedure ne(_?, _).
ne(X, yes) :- X? =?\= stop | true.
''');
      final guards = program.ops.whereType<bc.Guard>().toList();
      expect(guards.map((g) => g.procedureLabel), contains(r'=?\='));
    });
  });

  group('syntax', () {
    test('=?\\= is one token', () {
      final tokens = Lexer(r'X? =?\= Y?').tokenize();
      expect(tokens.map((t) => t.type).toList(), [
        TokenType.READER,
        TokenType.GROUND_NOT_EQUAL,
        TokenType.READER,
        TokenType.EOF,
      ]);
      expect(tokens[1].lexeme, r'=?\=');
    });

    test('a declaration may name it, as the root self.glp does', () {
      final module =
          Parser(Lexer(r'procedure =?\=(_?, _?).').tokenize()).parseModule();
      expect(module.procDeclarations.map((d) => '${d.name}/${d.arity}'),
          [r'=?\=/2']);
    });

    test('it prints infix, and the print parses back to itself', () {
      const source = r'ne(X, Y, yes) :- X? =?\= Y?, Y? =?\= stop | true.';
      final printer = GlpPrinter();
      final clause =
          Parser(Lexer(source).tokenize()).parse().procedures.single.clauses
              .single;
      final printed = printer.printClause(clause);
      expect(printed, contains(r'(X? =?\= Y?)'));
      expect(printed, contains(r'(Y? =?\= stop)'));
      final reparsed =
          Parser(Lexer(printed).tokenize()).parse().procedures.single.clauses
              .single;
      expect(printer.printClause(reparsed), printed);
    });
  });
}
