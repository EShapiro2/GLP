/// =?= and =?\= by GLP-Spec at bbff21d.  Each guard states its success
/// condition and nothing else (appendix-guards.tex): "=?= succeeds if both
/// arguments are ground and equal.  =?\= succeeds if no readers substitution
/// makes them ground and equal."  Suspension and failure follow from the guard
/// semantics (glp.tex, Guards): "A guard suspends if it does not succeed but
/// some instance of it under a readers substitution would succeed.  A guard
/// fails if no such instance exists."  So where both are ground and equal =?=
/// succeeds and =?\= fails; where no readers substitution makes them ground
/// and equal --- a clash, or an unbound writer, whatever readers stand
/// elsewhere --- =?= fails and =?\= succeeds; and where one does but they are
/// not both ground, each suspends.
///
/// One decision serves both guards on both paths (runner.dart,
/// `_decideGroundEquality`): ground_equal (0x45), which =?= compiles to where
/// both operands are variables, and the generic guard call (0x40), which every
/// other =?= and every =?\= compiles to.  Each case is run on four procedures
/// whose guard stands alone, so that its failure fails the call and its
/// suspension suspends it: `eq` (=?=, 0x45) and `ne` (=?\=) on the two
/// arguments, and `eq_r` and `ne_r`, which compare w(Left) with w(Right), the
/// right operand built in the guard (the generic guard call for =?=).
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart' show rootScope;
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = r'''
exported procedure eq(_?, _?, Constant).
eq(X, Y, yes) :- X? =?= Y? | true.

exported procedure eq_r(_?, _?, Constant).
eq_r(X, Y, yes) :- X? =?= w(Y?) | true.

exported procedure ne(_?, _?, Constant).
ne(X, Y, yes) :- X? =?\= Y? | true.

exported procedure ne_r(_?, _?, Constant).
ne_r(X, Y, yes) :- X? =?\= w(Y?) | true.

exported procedure eq_ab(_?, Constant).
eq_ab(X, yes) :- X? =?= f(a, b) | true.

exported procedure ne_ab(_?, Constant).
ne_ab(X, yes) :- X? =?\= f(a, b) | true.

exported procedure give(_?, _).
give(V, V?).

exported procedure occ(_?, Constant).
occ(X, yes) :- X? =?= f(X?) | true.

exported procedure nocc(_?, Constant).
nocc(X, yes) :- X? =?\= f(X?) | true.

exported procedure twice(_?, _?, Constant).
twice(X, Y, yes) :- Y? =?= f(X?, X?) | true.

exported procedure ntwice(_?, _?, Constant).
ntwice(X, Y, yes) :- Y? =?\= f(X?, X?) | true.
''';

/// The scope a source directly under the root is compiled in, programs/self.glp
/// its one layer, passed in: Constant is the root's.
final _scope = rootScope(File('../programs/self.glp').absolute.path);

const _ok = ExecutionStatus.succeeded;
const _fails = ExecutionStatus.failed;
const _waits = ExecutionStatus.suspended;

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'ground_equality.glp'), isTrue);
  return engine;
}

Future<ExecutionStatus> _status(String goal) async {
  final r = await _engine().runGoal(goal);
  return r.status;
}

/// [left] and [right] under =?= on both paths, expecting [eq], and under =?\=,
/// expecting [ne].
void _case(String name, String left, String right,
    {required ExecutionStatus eq, required ExecutionStatus ne}) {
  test(name, () async {
    expect(await _status('eq($left, $right, R)'), eq,
        reason: '=?=, ground_equal (0x45)');
    expect(await _status('eq_r(w($left), $right, R)'), eq,
        reason: '=?=, the generic guard call');
    expect(await _status('ne($left, $right, R)'), ne, reason: '=?\\=');
    expect(await _status('ne_r(w($left), $right, R)'), ne,
        reason: '=?\\=, the right operand built in the guard');
  });
}

void main() {
  group('both arguments ground', () {
    _case('equal constants', 'a', 'a', eq: _ok, ne: _fails);
    _case('differing constants', 'a', 'b', eq: _fails, ne: _ok);
    _case('equal numbers', '42', '42', eq: _ok, ne: _fails);
    _case('equal nested structures', 'f(a, g([1, h(2)]))',
        'f(a, g([1, h(2)]))',
        eq: _ok, ne: _fails);
    _case('nested structures differing deep inside', 'f(a, g([1, h(2)]))',
        'f(a, g([1, h(3)]))',
        eq: _fails, ne: _ok);
    _case('equal lists', '[1, 2, 3]', '[1, 2, 3]', eq: _ok, ne: _fails);
    _case('lists of different lengths', '[1, 2]', '[1, 2, 3]',
        eq: _fails, ne: _ok);
    _case('a clash of functor', 'f(a)', 'g(a)', eq: _fails, ne: _ok);
    _case('a clash of arity', 'f(a)', 'f(a, b)', eq: _fails, ne: _ok);
    _case('a constant against a structure', 'a', 'f(a)', eq: _fails, ne: _ok);
  });

  group(
      'no readers substitution makes them ground and equal, whatever readers '
      'stand elsewhere: =?= fails and =?\\= succeeds', () {
    _case('unbound readers beside two constants that differ', 'f(a, X?)',
        'f(b, Z?)',
        eq: _fails, ne: _ok);
    _case('unbound readers before two constants that differ', 'f(X?, a)',
        'f(Y?, b)',
        eq: _fails, ne: _ok);
    _case('deep in nested structures', 'f(g(X?), h(1, [a, b]))',
        'f(g(Y?), h(1, [a, c]))',
        eq: _fails, ne: _ok);
    _case('lists with unbound tails', '[1, 2 | T?]', '[1, 3 | U?]',
        eq: _fails, ne: _ok);
    _case('two numbers that differ, readers beside them', 'f(X?, 1)',
        'f(Y?, 2)',
        eq: _fails, ne: _ok);
    _case('two ground subterms that clash in functor', 'f(X?, g(a))',
        'f(Y?, k(a))',
        eq: _fails, ne: _ok);
    // GLP #3 Cowork, 2026-10-02 13:09 UTC: a clash where a side is not ground
    // decides it as well, no readers substitution unifying the two.
    _case('a clash of functor where neither side is ground', 'f(X?)', 'g(Y?)',
        eq: _fails, ne: _ok);
    _case('a list cell with an unbound tail against the empty list',
        '[a | T?]', '[]',
        eq: _fails, ne: _ok);
    _case('a clash of arity where neither side is ground', 'f(X?)',
        'f(Y?, Z?)',
        eq: _fails, ne: _ok);
    _case('an unbound reader against a structure of another functor',
        'f(X?, a)', 'g(b)',
        eq: _fails, ne: _ok);
  });

  group(
      'an unbound writer, which no readers substitution grounds: =?= fails and '
      '=?\\= succeeds', () {
    // GLP #3 Cowork, 2026-10-02 13:09 UTC: "f(W) =?= f(c) fails --- no
    // readers substitution grounds a writer --- and =?\= succeeds on it".
    _case('an unbound writer where the rest agrees', 'f(W)', 'f(c)',
        eq: _fails, ne: _ok);
    _case('an unbound writer on the right', 'f(c)', 'f(W)',
        eq: _fails, ne: _ok);
    _case('an unbound writer beside two constants that differ', 'f(a, W)',
        'f(b, c)',
        eq: _fails, ne: _ok);
    _case('an unbound writer beside an unbound reader', 'X?', 'f(W)',
        eq: _fails, ne: _ok);
    _case('an unbound writer and its own reader', 'f(W)', 'f(W?)',
        eq: _fails, ne: _ok);
  });

  group(
      'no readers substitution makes them ground and equal, a reader standing '
      'twice', () {
    test('a reader that would have to stand for a term containing itself',
        () async {
      expect(await _status('occ(X?, R)'), _fails, reason: '=?=');
      expect(await _status('nocc(X?, R)'), _ok, reason: '=?\\=');
    });

    test('a reader that would have to stand for two different terms',
        () async {
      expect(await _status('twice(X?, f(a, b), R)'), _fails, reason: '=?=');
      expect(await _status('ntwice(X?, f(a, b), R)'), _ok, reason: '=?\\=');
    });

    test('a reader twice where one term will do: each suspends', () async {
      expect(await _status('twice(X?, f(a, a), R)'), _waits, reason: '=?=');
      expect(await _status('ntwice(X?, f(a, a), R)'), _waits,
          reason: '=?\\=');
    });
  });

  group(
      'not both ground, and some readers substitution makes them ground and '
      'equal: each suspends', () {
    _case('an unbound reader on the left', 'X?', 'a', eq: _waits, ne: _waits);
    _case('an unbound reader on the right', 'a', 'Y?',
        eq: _waits, ne: _waits);
    _case('two unbound readers', 'X?', 'Y?', eq: _waits, ne: _waits);
    // GLP #3 Cowork, 2026-10-02 13:09 UTC: "f(X?) =?\= f(Y?) suspends".
    _case('two unbound readers in structures that agree', 'f(X?)', 'f(Y?)',
        eq: _waits, ne: _waits);
    // A number is a constant, never a variable's address.
    _case('an unbound reader against a number', 'X?', '2',
        eq: _waits, ne: _waits);
    _case('a number against an unbound reader', '2', 'Y?',
        eq: _waits, ne: _waits);
    _case('numbers that agree, a reader beside them', 'f(1, X?)', 'f(1, 2)',
        eq: _waits, ne: _waits);
    _case('a nested reader where the ground parts agree', 'f(a, X?)',
        'f(a, b)',
        eq: _waits, ne: _waits);
    _case('a reader against a structure', 'f(X?, b)', 'f(g(a), b)',
        eq: _waits, ne: _waits);
    _case('a reader against a structure holding a reader', 'f(X?, b)',
        'f(g(Y?), b)',
        eq: _waits, ne: _waits);
    _case('a list whose tail is unbound', '[1, 2 | T?]', '[1, 2, 3]',
        eq: _waits, ne: _waits);
  });

  group('a suspended guard is decided when its readers are assigned', () {
    test('=?= succeeds once the reader is assigned the agreeing value',
        () async {
      expect(await _status('eq(f(a, X?), f(a, b), R), give(b, X)'), _ok);
      expect(await _status('eq_r(w(f(a, X?)), f(a, b), R), give(b, X)'), _ok);
    });

    test('=?= on a number succeeds once the reader is assigned it', () async {
      expect(await _status('eq(X?, 2, R), give(2, X)'), _ok);
      expect(await _status('eq_r(w(X?), 2, R), give(2, X)'), _ok);
    });

    test('=?= fails once the reader is assigned another value', () async {
      expect(await _status('eq(f(a, X?), f(a, b), R), give(c, X)'), _fails);
      expect(await _status('eq_r(w(f(a, X?)), f(a, b), R), give(c, X)'),
          _fails);
    });

    test('=?\\= succeeds once the reader is assigned another value', () async {
      expect(await _status('ne(f(a, X?), f(a, b), R), give(c, X)'), _ok);
      expect(await _status('ne_r(w(f(a, X?)), f(a, b), R), give(c, X)'), _ok);
    });

    test('=?\\= fails once the reader is assigned the agreeing value',
        () async {
      expect(await _status('ne(f(a, X?), f(a, b), R), give(b, X)'), _fails);
      expect(await _status('ne_r(w(f(a, X?)), f(a, b), R), give(b, X)'),
          _fails);
    });
  });

  group('against a ground structure operand, the generic guard call', () {
    test('ground arguments: equal and differing', () async {
      expect(await _status('eq_ab(f(a, b), R)'), _ok);
      expect(await _status('eq_ab(f(a, c), R)'), _fails);
      expect(await _status('ne_ab(f(a, b), R)'), _fails);
      expect(await _status('ne_ab(f(a, c), R)'), _ok);
    });

    test('a differing pair beside an unbound reader', () async {
      expect(await _status('eq_ab(f(X?, c), R)'), _fails);
      expect(await _status('ne_ab(f(X?, c), R)'), _ok);
    });

    test('a nested unbound reader where the ground parts agree', () async {
      expect(await _status('eq_ab(f(X?, b), R)'), _waits);
      expect(await _status('ne_ab(f(X?, b), R)'), _waits);
    });
  });

  group('compilation', () {
    test('=?= with two variable operands is ground_equal (0x45)', () {
      final program = GlpCompiler().compile(r'''
procedure eq(_?, _?, Constant).
eq(X, Y, yes) :- X? =?= Y? | true.
''', ancestorScope: _scope);
      expect(program.ops.whereType<bc.GroundEqual>(), isNotEmpty);
      expect(program.ops.whereType<bc.Guard>(), isEmpty);
    });

    test('=?= with a structure operand is the generic guard call', () {
      for (final source in [
        'procedure e(_?, _?, Constant).\ne(X, Y, yes) :- X? =?= w(Y?) | true.\n',
        'procedure e(_?, Constant).\ne(X, yes) :- X? =?= f(a, b) | true.\n',
      ]) {
        final program = GlpCompiler().compile(source, ancestorScope: _scope);
        expect(program.ops.whereType<bc.GroundEqual>(), isEmpty);
        expect(program.ops.whereType<bc.Guard>().map((g) => g.procedureLabel),
            contains('=?='));
      }
    });
  });
}
