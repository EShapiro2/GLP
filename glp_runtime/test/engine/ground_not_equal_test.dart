/// =?\= by GLP-Spec at bbff21d: "=?\= succeeds if no readers substitution
/// makes them ground and equal" (appendix-guards.tex), and it suspends and
/// fails by the guard semantics (glp.tex, Guards): "A guard suspends if it does
/// not succeed but some instance of it under a readers substitution would
/// succeed.  A guard fails if no such instance exists."  So it succeeds where
/// the two clash or an unbound writer stands in either, whatever readers stand
/// elsewhere, fails where both are ground and equal, and suspends where they
/// are not but some readers substitution makes them so.  The catalogue gives it
/// Ground "no", so it licenses no repeated reader, where =?= keeps "yes
/// (both)".  test/engine/ground_equality_test.dart takes =?= and =?\= case by
/// case.
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

  group('=?\\= suspends where some readers substitution makes them equal', () {
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

    // "f(X?) =?\= f(Y?) suspends" (GLP #3 Cowork, 2026-10-02 13:09 UTC).
    test('two unbound readers in structures that agree', () async {
      final r = await _run('neq_only(f(X?), f(Y?), R)');
      expect(r.status, ExecutionStatus.suspended);
    });
  });

  group('no readers substitution makes them ground and equal: it succeeds, '
      'whatever readers stand elsewhere', () {
    test('two constants that differ decide it, unbound readers beside them',
        () async {
      final r = await _run('neq_only(f(a, Z?), f(b, W?), R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
    });

    test('a structure with a reader in it against a constant, the generic '
        'guard call: a clash', () async {
      final r = await _run('test_neq_stop(f(Z?), R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'go_on');
    });

    test('it succeeds as well where the readers beside the pair are assigned',
        () async {
      final r = await _run(
          'neq_only(f(a, Z?), f(b, W?), R), test_neq(c, c, Z), test_neq(c, d, W)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
    });

    // An unbound writer, which no readers substitution grounds: "f(W) =?= f(c)
    // fails ... and =?\= succeeds on it" (GLP #3 Cowork, 2026-10-02 13:09 UTC).
    // Until bbff21d =?\= failed where no pair of ground subterms differed and
    // a writer stood in either, held for the paper.
    test('an unbound writer decides it, beside two constants that differ or '
        'where the rest agrees', () async {
      final r = await _run('neq_only(f(a, W), f(b, c), R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
      final w = await _run('neq_only(f(W), f(c), R)');
      expect(w.status, ExecutionStatus.succeeded, reason: '${w.error}');
      expect(_v(w.bindings['R']), 'not_equal');
      final s = await _run('test_neq_stop(f(W), R)');
      expect(s.status, ExecutionStatus.succeeded, reason: '${s.error}');
      expect(_v(s.bindings['R']), 'go_on');
    });

    test('an unbound writer decides it beside an unbound reader', () async {
      final r = await _run('neq_only(X?, f(W), R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
      // =?= beside it, ground_equal (0x45): it fails, and otherwise takes the
      // call.
      final eq = await _run('test(X?, f(W), R)');
      expect(eq.status, ExecutionStatus.succeeded, reason: '${eq.error}');
      expect(_v(eq.bindings['R']), 'not_equal');
    });

    test('a clash where neither side is ground decides it', () async {
      final r = await _run('neq_only(f(X?), g(Y?), R)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['R']), 'not_equal');
      final l = await _run('neq_only([a | T?], [], R)');
      expect(l.status, ExecutionStatus.succeeded, reason: '${l.error}');
      expect(_v(l.bindings['R']), 'not_equal');
    });
  });

  // The catalogue's Ground column (GLP-Spec appendix-guards.tex, bbff21d):
  // "no" for =?\=, which succeeds where readers stand unbound, and "yes
  // (both)" for =?=.  Until 2026-10-02 =?\= had "yes (both)" and the analyzer
  // licensed a repeated reader after it, so the clause
  // neq_pair(X, Y, pair(X?, X?, Y?, Y?)) :- X? =?\= Y? | true loaded, and its
  // call neq_pair(f(a, Z?), f(b, W?), P) bound P to a term holding one unbound
  // variable twice (Integration #4 Code, 2026-10-02 10:52 UTC).
  group('Ground: no --- =?\\= grounds nothing, and licenses no repeated '
      'reader', () {
    test('a reader twice in the body after it is refused', () {
      expect(
          () => GlpCompiler().compile(r'''
procedure quad(_?, _?, _?, _?, _).
quad(_, _, _, _, done).
procedure k(_?, _?, _).
k(X, Y, Z?) :- X? =?\= Y? | quad(X?, X?, Y?, Y?, Z).
'''),
          throwsA(predicate(
              (e) => e.toString().contains('Reader variable "X?" occurs 2 times'),
              'an SRSW violation naming X?')));
    });

    test('a reader twice in the head after it is refused: neq_pair, the '
        'clause of the breach', () {
      expect(
          () => GlpCompiler().compile(r'''
procedure neq_pair(_?, _?, _).
neq_pair(X, Y, pair(X?, X?, Y?, Y?)) :- X? =?\= Y? | true.
'''),
          throwsA(predicate(
              (e) => e.toString().contains('Reader variable "X?" occurs 2 times'),
              'an SRSW violation naming X?')));
      final engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      bool? loaded;
      try {
        loaded = engine.loadFile(
            File('../programs/tests/srsw/neq_not_ground.glp').absolute.path);
      } catch (_) {
        loaded = false;
      }
      expect(loaded, isNot(isTrue),
          reason: 'programs/tests/srsw/neq_not_ground.glp, the fixture');
    });

    test('each argument read once, the call of the breach commits with each '
        'reader once', () async {
      final engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      expect(
          engine.loadSource(r'''
exported procedure neq_once(_?, _?, _).
neq_once(X, Y, pair(X?, Y?)) :- X? =?\= Y? | true.
''', filename: 'neq_once.glp'),
          isTrue);
      final r = await engine.runGoal('neq_once(f(a, Z?), f(b, W?), P)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      final heap = engine.runtime.heap;
      final p = heap.dereference(r.bindings['P']!);
      expect(p, isA<StructTerm>());
      expect((p as StructTerm).args, hasLength(2));
      final left = heap.dereference(p.args[0]) as StructTerm;
      final right = heap.dereference(p.args[1]) as StructTerm;
      expect([left.functor, _v(heap.dereference(left.args[0]))], ['f', 'a']);
      expect([right.functor, _v(heap.dereference(right.args[0]))], ['f', 'b']);
      // Z? and W?, unbound, each once.
      expect(left.args[1], isNot(right.args[1]));
    });

    test('=?= beside it grounds both: a reader twice after it loads', () {
      final program = GlpCompiler().compile(r'''
procedure quad(_?, _?, _?, _?, _).
quad(_, _, _, _, done).
procedure k(_?, _?, _).
k(X, Y, Z?) :- X? =?= Y? | quad(X?, X?, Y?, Y?, Z).
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
