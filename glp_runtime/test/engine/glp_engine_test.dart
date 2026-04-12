/// Tests for GlpEngine - the unified GLP execution core
import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

void main() {
  group('GlpEngine', () {
    late GlpEngine engine;

    setUp(() {
      engine = GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
    });

    test('runs simple goal with binding', () async {
      engine.loadSource('''
procedure test(_?, _).
test(a, b).
test(b, c).
''');

      final result = await engine.runGoal('test(a, X)');

      print('Status: ${result.status}, error: ${result.error}');
      expect(result.succeeded, isTrue, reason: 'Error: ${result.error}');
      expect(result.bindings['X'], isNotNull);
      print('X = ${result.bindings['X']}');
    });

    test('clause selection by constant matching', () async {
      engine.loadSource('''
procedure pick(_?, _).
pick(alice, picked_alice).
pick(bob, picked_bob).
pick(charlie, picked_charlie).
''');

      // Test alice
      var result = await engine.runGoal('pick(alice, X)');
      expect(result.succeeded, isTrue);
      print('pick(alice, X) -> X = ${result.bindings['X']}');

      // Test bob - need fresh engine for clean state
      final engine2 = GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      engine2.loadSource('''
procedure pick(_?, _).
pick(alice, picked_alice).
pick(bob, picked_bob).
pick(charlie, picked_charlie).
''');
      result = await engine2.runGoal('pick(bob, X)');
      expect(result.succeeded, isTrue);
      print('pick(bob, X) -> X = ${result.bindings['X']}');
    });

    test('loads and runs actor-style clauses', () async {
      engine.loadSource('''
procedure actor(_?, _?).
actor(alice, Ch) :- ground(Ch?) | alice_done.
actor(bob, Ch) :- ground(Ch?) | bob_done.
actor(charlie, Ch) :- ground(Ch?) | charlie_done.

procedure alice_done.
alice_done.

procedure bob_done.
bob_done.

procedure charlie_done.
charlie_done.
''');

      final result = await engine.runGoal('actor(alice, some_channel)');
      expect(result.succeeded, isTrue);
      print('actor(alice, some_channel) succeeded');
    });

    test('fails on unknown predicate', () async {
      final result = await engine.runGoal('unknown_predicate(x)');
      expect(result.failed, isTrue);
      expect(result.error, contains('not found'));
    });

    test('runs conjunction', () async {
      engine.loadSource('''
procedure set(_?, _).
set(a, X?) :- X := 1.
set(b, X?) :- X := 2.
''');

      final result = await engine.runGoal('set(a, X), set(b, Y)');
      expect(result.status, isNot(ExecutionStatus.failed));
      print('X = ${result.bindings['X']}, Y = ${result.bindings['Y']}');
    });
  });
}
