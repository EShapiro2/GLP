/// `@<` decides the standard order of constants.
///
/// GLP-Spec appendix-guards.tex (2bfb42b): "@< succeeds if both arguments are
/// ground constants and the first precedes the second in the standard order
/// of constants: a number precedes a string; numbers compare by value, and
/// strings by the codes of their characters, lexicographically."  A `Key` is a
/// `String` (`Key ::= String`, the root self.glp), so of two keys the
/// lexicographically smaller precedes (GLP, 2026-10-09 19:25 UTC).
///
/// Until 2026-10-09 the guard compared the printed text of its two constants,
/// so `10 @< 9` and `-1 @< -2` succeeded, `9 @< 10` and `5 @< '!'` failed, and
/// two strings compared by UTF-16 code unit, which puts a character beyond
/// U+FFFF before U+FF01; and a structure against an unbound reader waited on
/// the reader, though no instance of the guard succeeds (glp.tex, Guards: "A
/// guard fails if no such instance exists").  The order of the empty list `[]`, which no paper
/// places, is left as it was and is not tested here.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = r'''
exported procedure lt(Constant?, Constant?, Constant).
lt(X, Y, yes) :- X? @< Y? | true.
lt(_, _, no) :- otherwise | true.

exported procedure key_lt(Key?, Key?, Constant).
key_lt(X, Y, yes) :- X? @< Y? | true.
key_lt(_, _, no) :- otherwise | true.

exported procedure any_lt(_?, _?, Constant).
any_lt(X, Y, yes) :- X? @< Y? | true.
any_lt(_, _, no) :- otherwise | true.
''';

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'standard_order_guard.glp'),
      isTrue);
  return engine;
}

/// [goal] succeeds with R bound to [r]: `yes` where the guard succeeds, `no`
/// where it fails and the otherwise clause is taken.
Future<void> _answers(String goal, String r) async {
  final result = await _engine().runGoal(goal);
  expect(result.status, ExecutionStatus.succeeded,
      reason: '$goal: ${result.error}');
  expect(result.bindings['R'].toString(), 'Const($r)', reason: goal);
}

/// Of [a] and [b], `@<` holds of the first preceding the second and not of
/// the converse.
void _precedes(String name, String a, String b) {
  test('$name: $a @< $b, and not $b @< $a', () async {
    await _answers('lt($a, $b, R)', 'yes');
    await _answers('lt($b, $a, R)', 'no');
  });
}

void main() {
  group('two numbers compare by value', () {
    _precedes('more digits, a greater value', '9', '10');
    _precedes('two negatives', '-2', '-1');
    _precedes('an integer and a real', '1', '1.5');
    _precedes('a real and an integer', '1.5', '2');
    _precedes('a real and a longer integer', '2.5', '10');
    test('equal values: neither precedes, 1 and 1.0', () async {
      await _answers('lt(1, 1.0, R)', 'no');
      await _answers('lt(1.0, 1, R)', 'no');
      await _answers('lt(7, 7, R)', 'no');
    });
  });

  group('two strings compare by the codes of their characters', () {
    _precedes('a lesser code at the first difference', 'abc', 'abd');
    _precedes('a proper prefix', 'ab', 'abc');
    _precedes('an upper-case code below a lower-case one', "'B'", 'a');
    _precedes('codes, not the text of numbers', "'10'", "'9'");
    // U+FF01 and U+1F600: by UTF-16 code unit the second, a surrogate pair
    // beginning 0xD83D, would come first.
    _precedes('a character beyond U+FFFF by its code', "'\u{FF01}'",
        "'\u{1F600}'");
    test('a string does not precede itself', () async {
      await _answers('lt(abc, abc, R)', 'no');
    });
  });

  group('a number precedes a string', () {
    _precedes('a number and a word', '10', 'a');
    _precedes('whatever the string begins with', '5', "'!'");
    _precedes('a string of digits', '9', "'10'");
    _precedes('a negative real', '-1.5', "'+'");
  });

  group('an argument that is no constant fails the guard', () {
    test('a structure against a constant', () async {
      await _answers('any_lt(f(a), b, R)', 'no');
      await _answers('any_lt(1, f(a), R)', 'no');
    });
    test('a structure against an unbound reader of the goal: no instance '
        'succeeds, so it fails and does not wait', () async {
      await _answers('any_lt(f(a), Y?, R)', 'no');
      await _answers('any_lt(Y?, f(a), R)', 'no');
    });
    test('a constant against an unbound reader of the goal waits on it',
        () async {
      final result = await _engine().runGoal('any_lt(1, Y?, R)');
      expect(result.status, ExecutionStatus.suspended,
          reason: '${result.error}');
    });
  });

  group('two keys: the lexicographically smaller precedes', () {
    test('fixed keys, by their first differing character', () async {
      final k1 = '${'0' * 63}a';
      final k2 = '${'0' * 62}1a';
      await _answers("key_lt('$k1', '$k2', R)", 'yes');
      await _answers("key_lt('$k2', '$k1', R)", 'no');
      await _answers("key_lt('$k1', '$k1', R)", 'no');
    });

    test('generated keys agree with the order of their characters', () async {
      for (var i = 0; i < 4; i++) {
        final a = PersonIdentity.generate().pub.hex;
        final b = PersonIdentity.generate().pub.hex;
        final aFirst = a.compareTo(b) < 0;
        await _answers("key_lt('$a', '$b', R)", aFirst ? 'yes' : 'no');
        await _answers("key_lt('$b', '$a', R)", aFirst ? 'no' : 'yes');
      }
    });
  });
}
