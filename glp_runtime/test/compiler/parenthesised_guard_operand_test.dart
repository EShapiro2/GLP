/// A parenthesised arithmetic expression at an argument of a comparison guard
/// is read as the expression.
///
/// GLP-Spec appendix-guards.tex, "Arithmetic comparison guards": the guards
/// `<`, `>`, `=<`, `>=`, `=:=` and `=\=` "evaluate their arguments as
/// arithmetic expressions of type Exp", `procedure =:=(Exp?, Exp?).`, and
/// `(I? + 3)` is one.  Until 2026-10-10 the parser took a "(" at the start
/// of a guard for a parenthesised goal, and `p(I) :- (I? + 3) =:= 0 | true.`
/// was "[syntax] Expected predicate name or comparison" at its ")", where
/// `I? + 3 =:= 0` and `I? =:= (3 mod 2)` were read (GLP 2026-10-10 09:33
/// UTC).  It met sGLP: vGLP's compiler prints every operator term in
/// parentheses, and the guard `I? mod 3 =:= 0` of sGLP's
/// `programs/sglp/tests/delivery/chat.vglp` it prints
/// `((I? mod 3) =:= 0)`.  A parenthesised term followed by a comparison or
/// an arithmetic operator is now the left operand, parsed as an expression
/// as the right one is, nested parentheses included; a parenthesised goal,
/// `(G)` or `(G1 ; G2)`, parses as before.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

/// The guards of the single clause of [source], as text.
String _guards(String source) =>
    Parser(Lexer(source).tokenize()).parse().procedures.single.clauses.single
        .guards!
        .join(', ');

/// [written] reads as [plain] does, each the guards of `p(I, J) :- _ | true.`
void _readsAs(String written, String plain) {
  test('$written reads as $plain', () {
    final w = _guards('p(I, J) :- $written | true.');
    expect(w, _guards('p(I, J) :- $plain | true.'));
  });
}

const _comparisons = ['<', '>', '=<', '>=', '=:=', r'=\='];

const _source = r'''
Status ::= sent ; delivered ; read.

procedure status(Integer?, Status).
status(I, sent) :- ((I? mod 3) =:= 0) | true.
status(I, delivered) :- (I? mod 3) =:= 1 | true.
status(I, read) :- (I? mod 3) =:= (2) | true.

procedure c(Integer?, Constant).
c(I, R?) :- (I? + 1) < 3, (I? * 2) =< (4), 5 > (I? - 1), ((I? + 0)) >= 1,
    (I? mod 2) =\= 0, ((I? + 1) * 2) =:= 4 | R = yes.
c(_, R?) :- otherwise | R = no.
''';

const _ok = ExecutionStatus.succeeded;
const _waits = ExecutionStatus.suspended;

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'parenthesised_operand.glp'),
      isTrue);
  return engine;
}

/// [goal] ends [status], and where [binding] is given, [variable] is bound
/// to it.
void _runs(String goal, ExecutionStatus status,
    {String variable = 'R', String? binding}) {
  test('$goal ${status.name}'
      '${binding == null ? '' : ', $variable = $binding'}', () async {
    final result = await _engine().runGoal(goal);
    expect(result.status, status, reason: '${result.error}');
    if (binding != null) {
      expect(result.bindings[variable].toString(), 'Const($binding)');
    }
  });
}

void main() {
  group('the parser', () {
    test('(I? + 3) =:= 0 is the guard =:= of I? + 3 and 0', () {
      expect(_guards('p(I) :- (I? + 3) =:= 0 | true.'), '=:=(+(I?, 3), 0)');
    });

    group('each comparison, its left, its right or both parenthesised', () {
      for (final op in _comparisons) {
        _readsAs('(I? + 3) $op 0', 'I? + 3 $op 0');
        _readsAs('I? $op (J? * 2)', 'I? $op J? * 2');
        _readsAs('(I? mod 3) $op (J? - 1)', 'I? mod 3 $op J? - 1');
      }
    });

    group('nested parentheses', () {
      _readsAs('((I? + 3)) =:= 0', 'I? + 3 =:= 0');
      _readsAs('(((I? mod 3))) > ((1))', 'I? mod 3 > 1');
      _readsAs('(I? // 2) + (J? mod 2) < 5', 'I? // 2 + J? mod 2 < 5');
      test('((I? + 3) * 2) =< J? and (I? + 3) * 2 >= J?', () {
        expect(_guards('p(I, J) :- ((I? + 3) * 2) =< J? | true.'),
            '=<(*(+(I?, 3), 2), J?)');
        expect(_guards('p(I, J) :- (I? + 3) * 2 >= J? | true.'),
            '>=(*(+(I?, 3), 2), J?)');
      });
    });

    test('vGLP\'s compiler\'s print, ((I? mod 3) =:= 0), the guard whole', () {
      expect(_guards('status(I, sent) :- ((I? mod 3) =:= 0) | true.'),
          _guards('status(I, sent) :- I? mod 3 =:= 0 | true.'));
    });

    test('the other infix guards, as a name on their left is read', () {
      expect(_guards('p(X, Y) :- (w(X?)) =?= Y? | true.'),
          _guards('p(X, Y) :- w(X?) =?= Y? | true.'));
      expect(_guards('p(X) :- (b) @< X? | true.'),
          _guards('p(X) :- b @< X? | true.'));
    });

    test('a parenthesised operand among other guards', () {
      expect(_guards('p(I, J) :- integer(I?), (I? + 1) < J?, J? > 0 | true.'),
          'integer(I?), <(+(I?, 1), J?), >(J?, 0)');
    });

    test('a parenthesised goal and a disjunction parse as before', () {
      expect(_guards('p(X) :- (ground(X?)) | true.'), 'ground(X?)');
      expect(_guards('p(X) :- (X? > 0) | true.'), '>(X?, 0)');
      expect(_guards('p(X) :- (known(X?) ; ground(X?)) | true.'),
          ';(known(X?), ground(X?))');
    });

    test('a parenthesised expression that is no comparison is refused', () {
      expect(
          () => _guards('p(I) :- (I? + 3) | true.'),
          throwsA(isA<CompileError>().having((e) => e.message, 'message',
              contains('Expected predicate name or comparison'))));
    });
  });

  group('run', () {
    _runs('status(0, S)', _ok, variable: 'S', binding: 'sent');
    _runs('status(4, S)', _ok, variable: 'S', binding: 'delivered');
    _runs('status(5, S)', _ok, variable: 'S', binding: 'read');
    _runs('status(Q?, S)', _waits);
    _runs('c(1, R)', _ok, binding: 'yes');
    _runs('c(2, R)', _ok, binding: 'no');
    _runs('c(0, R)', _ok, binding: 'no');
  });
}
