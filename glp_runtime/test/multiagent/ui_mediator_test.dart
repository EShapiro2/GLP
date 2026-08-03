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
  }, skip: 'Still skipped, and the reason that stood here until 2026-08-03 has '
      'been ruled out. That reason was that agent/4 and inject_msg/5 inspect a '
      'bare type parameter with no instantiation here. The fixture was swept on '
      '2026-08-03 (GLP: per-kind OutputEntry, agent/4 declared at ActorOut and '
      'NetInStream): play_madglp now loads with ZERO unchecked procedures and '
      'typed_social_agent.glp type-checks standalone, so it moved out of '
      'NEGATIVE_FILES. Unskipped and measured anyway: all three still fail, the '
      'goal failing with no output and no load-time diagnostic at all. So the '
      'cause is on the co-loaded goal path rather than in the fixture types, and '
      'it needs its own diagnosis rather than another sweep.');
}
