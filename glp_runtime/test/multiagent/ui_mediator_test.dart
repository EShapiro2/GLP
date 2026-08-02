/// Tests for ui_mediator.glp — ground-term mediator between agent/4 and Dart.
///
/// Uses GlpEngine to load social_agent.glp + ui_mediator.glp, then tests
/// the mediator's grounding of agent output and forwarding of user input.
import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  final selfPath = '../programs/tests/agent_roundtrip/self.glp';
  final socialAgentPath =
      '../programs/tests/agent_roundtrip/typed_social_agent.glp';
  final uiMediatorPath =
      '../programs/tests/agent_roundtrip/typed_ui_mediator.glp';

  group('ui_mediator', () {
    late GlpEngine engine;
    late List<String> outputLines;

    setUp(() {
      engine = GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path)..strictTypes = false;
      outputLines = [];
      engine.runtime.outputCallback = (line) => outputLines.add(line);
    });

    test('grounds befriend output with request ID', () async {
      final selfSource = File(selfPath).readAsStringSync();
      final socialSource = File(socialAgentPath).readAsStringSync();
      final mediatorSource = File(uiMediatorPath).readAsStringSync()
          .replaceAll(RegExp(r'-mode\s*\(\s*system\s*\)\s*\.'), '');

      engine.loadSource('''
$selfSource
$socialSource
$mediatorSource

procedure consume(_?).
consume([_|Rest]) :- consume(Rest?).
consume([]).

procedure test.
test :-
    ui_mediator(alice,
        ch([msg(agent, '_user', befriend(bob, _))], AgentOut),
        ch([], UserOut),
        [], 1),
    send_to_user(UserOut?),
    consume(AgentOut?).
''');

      final result = await engine.runGoal('test');
      print('Status: ${result.status}');
      print('Output: $outputLines');
      expect(outputLines, contains('befriend(bob, req(1))'));
    });

    test('passes ground connected message through', () async {
      final selfSource = File(selfPath).readAsStringSync();
      final socialSource = File(socialAgentPath).readAsStringSync();
      final mediatorSource = File(uiMediatorPath).readAsStringSync()
          .replaceAll(RegExp(r'-mode\s*\(\s*system\s*\)\s*\.'), '');

      engine.loadSource('''
$selfSource
$socialSource
$mediatorSource

procedure consume(_?).
consume([_|Rest]) :- consume(Rest?).
consume([]).

procedure test.
test :-
    ui_mediator(alice,
        ch([msg(agent, '_user', connected(bob))], AgentOut),
        ch([], UserOut),
        [], 1),
    send_to_user(UserOut?),
    consume(AgentOut?).
''');

      final result = await engine.runGoal('test');
      print('Status: ${result.status}');
      print('Output: $outputLines');
      expect(outputLines, contains('connected(bob)'));
    });

    test('passes ground received message through', () async {
      final selfSource = File(selfPath).readAsStringSync();
      final socialSource = File(socialAgentPath).readAsStringSync();
      final mediatorSource = File(uiMediatorPath).readAsStringSync()
          .replaceAll(RegExp(r'-mode\s*\(\s*system\s*\)\s*\.'), '');

      engine.loadSource('''
$selfSource
$socialSource
$mediatorSource

procedure consume(_?).
consume([_|Rest]) :- consume(Rest?).
consume([]).

procedure test.
test :-
    ui_mediator(alice,
        ch([msg(agent, '_user', received(bob, hello))], AgentOut),
        ch([], UserOut),
        [], 1),
    send_to_user(UserOut?),
    consume(AgentOut?).
''');

      final result = await engine.runGoal('test');
      print('Status: ${result.status}');
      print('Output: $outputLines');
      expect(outputLines, contains('received(bob, hello)'));
    });
  }, skip: 'Skipped for a different cause than before, measured 2026-08-02 when '
      'the fixture moved to programs/tests/agent_roundtrip. The 2026-06-22 '
      'reason — the sources loaded without their own self.glp scope, so the '
      'goal-check hard-failed on Unresolved type: Response — is fixed: self.glp '
      'is now loaded first and that error is gone. What remains is that agent/4 '
      'and inject_msg/5 inspect a bare type parameter and have no instantiation '
      'here, so they take the per-instantiation route and have nothing to '
      'certify standalone; the goal then fails with no output. Same cause as the '
      'two agent_roundtrip entries in NEGATIVE_FILES. It clears when the fixture '
      'is swept — a concrete element type, or a named parameter list per '
      'Moded-Types "Declaration parameters" — which is IGLP\'s and is 179 '
      'declarations.');
}
