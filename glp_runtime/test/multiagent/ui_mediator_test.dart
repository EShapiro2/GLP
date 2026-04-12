/// Tests for ui_mediator.glp — ground-term mediator between agent/4 and Dart.
///
/// Uses GlpEngine to load social_agent.glp + ui_mediator.glp, then tests
/// the mediator's grounding of agent output and forwarding of user input.
import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  final socialAgentPath =
      '../programs/typed_book/social_graph/typed_social_agent.glp';
  final uiMediatorPath =
      '../programs/typed_book/social_graph/typed_ui_mediator.glp';

  group('ui_mediator', () {
    late GlpEngine engine;
    late List<String> outputLines;

    setUp(() {
      engine = GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path)..strictTypes = false;
      outputLines = [];
      engine.runtime.outputCallback = (line) => outputLines.add(line);
    });

    test('grounds befriend output with request ID', () async {
      final socialSource = File(socialAgentPath).readAsStringSync();
      final mediatorSource = File(uiMediatorPath).readAsStringSync()
          .replaceAll(RegExp(r'-mode\s*\(\s*system\s*\)\s*\.'), '');

      engine.loadSource('''
$socialSource
$mediatorSource

procedure send_to_user(_?).
send_to_user([T | In]) :- ground(T?) | '_output'(T?), send_to_user(In?).
send_to_user([]).

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
      final socialSource = File(socialAgentPath).readAsStringSync();
      final mediatorSource = File(uiMediatorPath).readAsStringSync()
          .replaceAll(RegExp(r'-mode\s*\(\s*system\s*\)\s*\.'), '');

      engine.loadSource('''
$socialSource
$mediatorSource

procedure send_to_user(_?).
send_to_user([T | In]) :- ground(T?) | '_output'(T?), send_to_user(In?).
send_to_user([]).

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
      final socialSource = File(socialAgentPath).readAsStringSync();
      final mediatorSource = File(uiMediatorPath).readAsStringSync()
          .replaceAll(RegExp(r'-mode\s*\(\s*system\s*\)\s*\.'), '');

      engine.loadSource('''
$socialSource
$mediatorSource

procedure send_to_user(_?).
send_to_user([T | In]) :- ground(T?) | '_output'(T?), send_to_user(In?).
send_to_user([]).

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
  });
}
