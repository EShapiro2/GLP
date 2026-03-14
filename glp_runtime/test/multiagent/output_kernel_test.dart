/// Tests for '_output'/1 kernel and send_to_user/1 GLP predicate.
import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  group('_output kernel', () {
    late GlpEngine engine;
    late List<String> outputLines;

    setUp(() {
      engine = GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path)..strictTypes = false;
      outputLines = [];
      engine.runtime.outputCallback = (line) => outputLines.add(line);
    });

    test('prints a constant', () async {
      engine.loadSource('''
-mode(system).
procedure test.
test :- '_output'(hello).
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['hello']);
    });

    test('prints a struct', () async {
      engine.loadSource('''
-mode(system).
procedure test.
test :- '_output'(msg(alice, bob, text(hi))).
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['msg(alice, bob, text(hi))']);
    });

    test('prints a list', () async {
      engine.loadSource('''
-mode(system).
procedure test.
test :- '_output'([a, b, c]).
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['[a, b, c]']);
    });
  });

  group('send_to_user', () {
    late GlpEngine engine;
    late List<String> outputLines;

    setUp(() {
      engine = GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path)..strictTypes = false;
      outputLines = [];
      engine.runtime.outputCallback = (line) => outputLines.add(line);
    });

    test('consumes a ground stream and prints each term', () async {
      // send_to_user is embedded in mad predicates, so we provide it inline
      engine.loadSource('''
-mode(system).

procedure send_to_user(_?).
send_to_user([T | In]) :- ground(T?) | '_output'(T?), send_to_user(In?).
send_to_user([]).

procedure test.
test :- send_to_user([hello, world, msg(a, b)]).
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['hello', 'world', 'msg(a, b)']);
    });

    test('waits for stream elements to become ground', () async {
      // Test that send_to_user suspends on non-ground and resumes when bound
      engine.loadSource('''
-mode(system).

procedure send_to_user(_?).
send_to_user([T | In]) :- ground(T?) | '_output'(T?), send_to_user(In?).
send_to_user([]).

procedure test.
test :- send_to_user([hello | Tail?]), Tail = [world].
''');
      final result = await engine.runGoal('test');
      expect(result.succeeded, isTrue);
      expect(outputLines, ['hello', 'world']);
    });
  });
}
