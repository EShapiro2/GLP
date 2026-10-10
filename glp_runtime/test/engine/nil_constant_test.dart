/// `[]` is not the string `nil`.  GLP-Spec appendix-lp.tex, Definition "Logic
/// Programs Syntax": "a constant (numbers, strings, or the empty list `[]`)";
/// IGLP's code format gives the empty list a constant tag of its own (0 nil;
/// strings 3).  The two are two constants: a head, a guard and =?= tell them
/// apart, and the REPL shows each as itself.  The empty list's type is String
/// (TGLP appendix-root-self.tex: "The empty list is a String, hence a
/// Constant"), so the string and constant guards hold of it.
///
/// Until 2026-10-07 the runtime held `[]` as the string 'nil': `X = nil.`
/// showed `[]`, `nil =?= []` succeeded, and the head `[]` matched `nil`.
library;

import 'dart:convert';
import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

const _source = r'''
exported procedure eq(_?, _?, Constant).
eq(X, Y, yes) :- X? =?= Y? | true.
eq(_, _, no) :- otherwise | true.

exported procedure is_empty(_?, Constant).
is_empty([], yes).
is_empty(_, no) :- otherwise | true.

exported procedure is_nil(_?, Constant).
is_nil(nil, yes).
is_nil(_, no) :- otherwise | true.

exported procedure lst(_?, Constant).
lst(X, yes) :- list(X?) | true.
lst(_, no) :- otherwise | true.

exported procedure str(_?, Constant).
str(X, yes) :- string(X?) | true.
str(_, no) :- otherwise | true.

exported procedure cst(_?, Constant).
cst(X, yes) :- constant(X?) | true.
cst(_, no) :- otherwise | true.
''';

/// The answer R of [goal], run on a fresh engine with [_source] loaded.
Future<String> _answer(String goal) async {
  final e = GlpEngine(rootSelfGlpPath: _root);
  expect(e.loadSource(_source, filename: 'nil_constant.glp'), isTrue);
  final r = await e.runGoal(goal);
  expect(r.error, isNull, reason: goal);
  expect(r.status, ExecutionStatus.succeeded, reason: goal);
  final v = r.bindings['R'];
  expect(v, isA<ConstTerm>(), reason: goal);
  return '${(v as ConstTerm).value}';
}

void main() {
  group('the goal builder holds [] and nil apart', () {
    test('X = [] binds the empty list, X = nil the string nil', () async {
      final e = GlpEngine(rootSelfGlpPath: _root);
      final empty = await e.runGoal('X = []');
      final named = await e.runGoal('X = nil');
      expect((empty.bindings['X'] as ConstTerm).value, same(nil));
      expect((named.bindings['X'] as ConstTerm).value, 'nil');
      expect((named.bindings['X'] as ConstTerm).value == nil, isFalse);
    });
  });

  group('=?= tells them apart', () {
    test('nil =?= [] fails, both ways', () async {
      expect(await _answer('eq(nil, [], R)'), 'no');
      expect(await _answer('eq([], nil, R)'), 'no');
    });
    test('[] =?= [] and nil =?= nil succeed', () async {
      expect(await _answer('eq([], [], R)'), 'yes');
      expect(await _answer('eq(nil, nil, R)'), 'yes');
    });
    test('inside a structure and a list', () async {
      expect(await _answer('eq(f([]), f(nil), R)'), 'no');
      expect(await _answer('eq([a|nil], [a], R)'), 'no');
      expect(await _answer('eq([a|[]], [a], R)'), 'yes');
    });
  });

  group('a head tells them apart', () {
    test('the head [] does not match nil', () async {
      expect(await _answer('is_empty(nil, R)'), 'no');
      expect(await _answer('is_empty([], R)'), 'yes');
    });
    test('the head nil does not match []', () async {
      expect(await _answer('is_nil([], R)'), 'no');
      expect(await _answer('is_nil(nil, R)'), 'yes');
    });
  });

  group('the type guards', () {
    test('list holds of [] and not of nil', () async {
      expect(await _answer('lst([], R)'), 'yes');
      expect(await _answer('lst(nil, R)'), 'no');
    });
    test('string and constant hold of both, [] being a String', () async {
      expect(await _answer('str([], R)'), 'yes');
      expect(await _answer('str(nil, R)'), 'yes');
      expect(await _answer('cst([], R)'), 'yes');
      expect(await _answer('cst(nil, R)'), 'yes');
    });
  });

  test(
      'the REPL shows X = nil. as nil and X = []. as [] '
      '(bin/glp_repl.dart, _formatTerm)', () async {
    final repl =
        await Process.start(Platform.resolvedExecutable, ['bin/glp_repl.dart']);
    repl.stdin.write('X = nil.\nY = [].\nZ = [nil, [] | nil].\n:quit\n');
    await repl.stdin.close();
    final err = repl.stderr.transform(utf8.decoder).join();
    final out = await repl.stdout.transform(utf8.decoder).join();
    await repl.exitCode;
    expect(out, contains('X = nil\n'), reason: '$out\n${await err}');
    expect(out, contains('Y = []\n'), reason: out);
    expect(out, contains('Z = [nil, [] | nil]\n'), reason: out);
  }, timeout: Timeout(Duration(minutes: 3)));
}
