/// =?= and =?\= by GLP-Spec appendix-guards.tex at 30e382c: "=?= succeeds if
/// both arguments are ground and equal, fails as soon as the two differ at a
/// pair of ground subterms, and suspends otherwise.  =?\= succeeds where =?=
/// fails, fails where it succeeds, and suspends where it suspends."
///
/// One decision serves both guards on both paths (runner.dart,
/// `_decideGroundEquality`): ground_equal (0x45), which =?= compiles to where
/// both operands are variables, and the generic guard call (0x40), which every
/// other =?= and every =?\= compiles to.  Each case is run on four procedures
/// whose guard stands alone, so that its failure fails the call and its
/// suspension suspends it: `eq` (=?=, 0x45) and `ne` (=?\=) on the two
/// arguments, and `eq_r` and `ne_r`, which compare w(Left) with w(Right), the
/// right operand built in the guard (the generic guard call for =?=).
///
/// Where no pair of ground subterms differs and an unbound writer stands in an
/// argument, the guards appendix's "suspends otherwise" and glp.tex's Guards (a
/// guard fails where no instance under a readers substitution succeeds) read
/// differently; the runtime keeps ground_equal's former answer there, a failure
/// of each guard, until the paper settles it, and no case below asks it
/// (test/engine/ground_not_equal_test.dart pins that answer).
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
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
''';

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
      'a pair of ground subterms that differ decides it, whatever else stands '
      'in either', () {
    _case('unbound readers beside the pair', 'f(a, X?)', 'f(b, Z?)',
        eq: _fails, ne: _ok);
    _case('unbound readers before the pair', 'f(X?, a)', 'f(Y?, b)',
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
    _case('an unbound writer beside the pair', 'f(a, W)', 'f(b, c)',
        eq: _fails, ne: _ok);
  });

  group('no pair of ground subterms differs: an unbound reader suspends it',
      () {
    _case('an unbound reader on the left', 'X?', 'a', eq: _waits, ne: _waits);
    _case('an unbound reader on the right', 'a', 'Y?',
        eq: _waits, ne: _waits);
    _case('two unbound readers', 'X?', 'Y?', eq: _waits, ne: _waits);
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
    _case('a list whose tail is unbound', '[1, 2 | T?]', '[1, 2, 3]',
        eq: _waits, ne: _waits);
    // A pair that clashes where a side is not ground is no pair of ground
    // subterms, and nothing below it is compared: no pair differs.
    _case('a clash of functor where neither side is ground', 'f(X?)', 'g(Y?)',
        eq: _waits, ne: _waits);
    _case('a list cell with an unbound tail against the empty list',
        '[a | T?]', '[]',
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
''');
      expect(program.ops.whereType<bc.GroundEqual>(), isNotEmpty);
      expect(program.ops.whereType<bc.Guard>(), isEmpty);
    });

    test('=?= with a structure operand is the generic guard call', () {
      for (final source in [
        'procedure e(_?, _?, Constant).\ne(X, Y, yes) :- X? =?= w(Y?) | true.\n',
        'procedure e(_?, Constant).\ne(X, yes) :- X? =?= f(a, b) | true.\n',
      ]) {
        final program = GlpCompiler().compile(source);
        expect(program.ops.whereType<bc.GroundEqual>(), isEmpty);
        expect(program.ops.whereType<bc.Guard>().map((g) => g.procedureLabel),
            contains('=?='));
      }
    });
  });
}
