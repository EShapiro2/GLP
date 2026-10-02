/// A structure, or a constant, on the left of an infix guard parses as one on
/// the right does.
///
/// GLP-Spec glp.tex, Definition "Guarded Clause": a guard is a conjunction of
/// guard predicates, and appendix-guards.tex writes `=?=`, `=?\=`, `@<` and
/// the arithmetic comparisons infix, of any two terms (`procedure =?=(_?,
/// _?).`), so no side of one is privileged.  Until 2026-10-02 the parser took
/// a name at the start of a guard for a predicate, and `w(X?) =?= Y?` was a
/// syntax error at `=?=`, where `[X?] =?= Y?`, `X? =?= w(Y?)` and
/// `1 + X? > 3` parsed (Integration, 2026-10-02 17:06 UTC, S2; GLP #3 Cowork,
/// 17:12 UTC: "S1, S2, S3: yes, each a task, compliance").  A name followed
/// by a comparison or an arithmetic operator is now the left operand, parsed
/// as an expression as the right one is; a name followed by anything else is
/// a predicate as before, `foo(a) = X` the unification goal it was.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

/// The guards of the single clause of [source].
List<Guard> _guards(String source) =>
    Parser(Lexer(source).tokenize()).parse().procedures.single.clauses.single
        .guards!;

const _source = r'''
procedure w2(_?, _?, Constant).
w2(X, Y, R?) :- w(X?) =?= Y? | R = yes.
w2(_, _, R?) :- otherwise | R = no.

procedure w3(_?, _?, Constant).
w3(X, Y, R?) :- f(X?, g(a)) =?\= Y? | R = yes.
w3(_, _, R?) :- otherwise | R = no.

procedure w4(Constant?, Constant).
w4(X, R?) :- b @< X? | R = yes.
w4(_, R?) :- otherwise | R = no.
''';

const _ok = ExecutionStatus.succeeded;
const _waits = ExecutionStatus.suspended;

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'infix_guard_left.glp'), isTrue);
  return engine;
}

/// [goal] ends [status], and where [r] is given, with R bound to it.
void _runs(String goal, ExecutionStatus status, {String? r}) {
  test('$goal ${status.name}${r == null ? '' : ', R = $r'}', () async {
    final result = await _engine().runGoal(goal);
    expect(result.status, status, reason: '${result.error}');
    if (r != null) expect(result.bindings['R'].toString(), 'Const($r)');
  });
}

void main() {
  group('the parser', () {
    test('w(X?) =?= Y? is the guard =?= of a structure and a reader', () {
      final g = _guards('p(X, Y, R?) :- w(X?) =?= Y? | R = yes.').single;
      expect(g.predicate, '=?=');
      expect(g.args[0], isA<StructTerm>());
      expect((g.args[0] as StructTerm).functor, 'w');
      expect(g.args[1], isA<VarTerm>());
    });

    test('as Y? =?= w(X?) is, the sides exchanged', () {
      final g = _guards('p(X, Y, R?) :- Y? =?= w(X?) | R = yes.').single;
      expect(g.predicate, '=?=');
      expect((g.args[1] as StructTerm).functor, 'w');
    });

    test('a structure in an arithmetic expression on the left', () {
      final g = _guards('p(X, R?) :- f(X?) + 1 > 2 | R = yes.').single;
      expect(g.predicate, '>');
      final left = g.args[0] as StructTerm;
      expect(left.functor, '+');
      expect((left.args[0] as StructTerm).functor, 'f');
    });

    test('a constant on the left, b @< X?, and a structure of two', () {
      expect(_guards('p(X, R?) :- b @< X? | R = yes.').single.predicate, '@<');
      final g =
          _guards('p(X, Y, R?) :- f(X?, g(a)) =?\\= Y? | R = yes.').single;
      expect(g.predicate, '=?\\=');
      expect((g.args[0] as StructTerm).args, hasLength(2));
    });

    test('a guard predicate and the unification goal parse as before', () {
      expect(_guards('p(X, R?) :- ground(X?) | R = yes.').single.predicate,
          'ground');
      final module = Parser(Lexer('p(X?) :- foo(a) = X.').tokenize()).parse();
      final goal = module.procedures.single.clauses.single.body!.single;
      expect(goal.functor, '=');
      expect((goal.args[0] as StructTerm).functor, 'foo');
    });
  });

  group('run', () {
    _runs('w2(a, w(a), R)', _ok, r: 'yes');
    _runs('w2(a, w(b), R)', _ok, r: 'no');
    _runs('w2(a, Q?, R)', _waits);
    _runs('w3(a, f(a, g(a)), R)', _ok, r: 'no');
    _runs('w3(a, f(b, g(a)), R)', _ok, r: 'yes');
    _runs('w4(c, R)', _ok, r: 'yes');
    _runs('w4(a, R)', _ok, r: 'no');
  });
}
