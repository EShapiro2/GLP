/// A restart under a new version of the agent's program (IGLP 61f3283,
/// madglp-spec.tex, Termination and Restart: "A restart may load a new
/// version of the agent's program, as when the application is updated.  The
/// local state is then restored under the new version when every procedure
/// called by a goal of the resolvent is defined in the new version with the
/// same type identity; otherwise the update is refused, and the agent
/// restarts under the version it terminated under"; Implementation Notes,
/// "Persistence": the store keeps "the agent's key pair, the compiled program
/// it runs, the resolvent ...").
///
/// The agent's program is one module file, which each version replaces, as
/// an update of an application replaces its program.  Its goal of serve/2
/// waits on the person's stream in the resolvent across every restart, and
/// at each restart the runtime grants refresh, on which serve/2 draws again
/// what it last drew (the paragraph after Definition "Restart"), so the
/// drawing says which version's code ran.
@Timeout.none
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/stored_program.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/codec.dart' show bytesEqual;

/// The first version: the person says go, and the agent says went; refresh
/// draws what the agent last drew.
const String _v1 = r'''
Act ::= go ; refresh.
Shown ::= went ; drawn(Constant).

exported procedure agent(Channel(Stream(Act), Stream(Shown))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- serve(In?, none).

procedure serve(Stream(Act)?, Constant?).
serve([go | In], _) :- send_to_user([went]), serve(In?, gone).
serve([refresh | In], gone) :- send_to_user([drawn(gone)]), serve(In?, gone).
serve([refresh | In], none) :- send_to_user([drawn(none)]), serve(In?, none).
serve([], _).
''';

/// A new version that keeps serve/2 and its type, and draws otherwise.
const String _v2 = r'''
Act ::= go ; refresh.
Shown ::= went ; drawn(Constant).

exported procedure agent(Channel(Stream(Act), Stream(Shown))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- serve(In?, none).

procedure serve(Stream(Act)?, Constant?).
serve([go | In], _) :- send_to_user([went]), serve(In?, gone).
serve([refresh | In], gone) :- send_to_user([drawn(again)]), serve(In?, gone).
serve([refresh | In], none) :- send_to_user([drawn(none)]), serve(In?, none).
serve([], _).
''';

/// A new version that changes the type of serve/2, a procedure a goal of the
/// resolvent calls: its second argument a State, no longer any Constant.
const String _v3 = r'''
Act ::= go ; refresh.
Shown ::= went ; drawn(Constant).
State ::= none ; gone.

exported procedure agent(Channel(Stream(Act), Stream(Shown))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- serve(In?, none).

procedure serve(Stream(Act)?, State?).
serve([go | In], _) :- send_to_user([went]), serve(In?, gone).
serve([refresh | In], gone) :- send_to_user([drawn(three)]), serve(In?, gone).
serve([refresh | In], none) :- send_to_user([drawn(none)]), serve(In?, none).
serve([], _).
''';

/// A new version that changes the type of agent/2, which no goal of the
/// resolvent calls once the agent has started, and keeps serve/2's.
const String _v4 = r'''
Act ::= go ; refresh.
Shown ::= went ; drawn(Constant) ; more.

exported procedure agent(Channel(Stream(Act), Stream(Shown))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- serve(In?, none).

procedure serve(Stream(Act)?, Constant?).
serve([go | In], _) :- send_to_user([went]), serve(In?, gone).
serve([refresh | In], gone) :- send_to_user([drawn(four)]), serve(In?, gone).
serve([refresh | In], none) :- send_to_user([drawn(none)]), serve(In?, none).
serve([], _).
''';

void main() {
  late String root;
  late Directory dir;
  late String file;

  setUpAll(() {
    // Under the root, so the program's scope holds the root self.glp
    // (agent_runtime_test.dart).
    dir = Directory('../programs/tests').createTempSync('glp_agent_update_');
    file = '${dir.path}/app.glp';
    root = File('../programs/self.glp').absolute.path;
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  /// One launch of the application with [version] its program, over [store]:
  /// the agent, and what it drew.
  Future<(AgentRuntime, List<String>)> launch(
      String version, AgentStateStore store) async {
    File(file).writeAsStringSync(version);
    final drawn = <String>[];
    final agent = AgentRuntime(
      agentId: 'u',
      program: file,
      rootSelfGlpPath: root,
      stateStore: store,
    );
    agent.onOutput = (line) {
      if (line.startsWith('< ') || line.startsWith('[ERROR]')) {
        drawn.add(line);
      }
    };
    await agent.initialize();
    return (agent, drawn);
  }

  Future<Uint8List> keptIdentity(AgentStateStore store) async =>
      StoredProgram.fromBytes((await store.loadProgram())!).identity;

  test(
      'an update that keeps the types of the procedures the resolvent\'s goals '
      'call restores the state under the new version, and refresh draws it '
      'with the new code; one that changes such a type is refused, and the '
      'agent runs the version it terminated under', () async {
    final store = MemoryAgentStateStore();

    // The first start, under the first version: the store keeps it.
    final (a1, out1) = await launch(_v1, store);
    expect(a1.firstStart, isTrue);
    expect(store.programSaves, 1,
        reason: 'the store keeps the compiled program the agent runs');
    final first = await keptIdentity(store);
    await a1.injectUserInput(ConstTerm('go'));
    expect(out1, ['< went']);

    // The application is updated, keeping serve/2 and its type: the state is
    // restored under the new version, and refresh draws with its code.
    final (a2, out2) = await launch(_v2, store);
    expect(a2.firstStart, isFalse);
    expect(a2.updated, isTrue);
    expect(a2.updateRefusal, isNull);
    expect(out2, ['< drawn(again)'],
        reason: 'the refresh is taken by the new version\'s code, from the '
            'restored state');
    expect(store.programSaves, 2,
        reason: 'the store keeps the new version, once the state restored '
            'under it is saved');
    final second = await keptIdentity(store);
    expect(bytesEqual(second, first), isFalse);
    await a2.injectUserInput(ConstTerm('refresh'));
    expect(out2, ['< drawn(again)', '< drawn(again)']);

    // An update that changes serve/2's type is refused: the agent restarts
    // under the version it terminated under, which the store keeps.
    final (a3, out3) = await launch(_v3, store);
    expect(a3.updated, isFalse);
    expect(a3.updateRefusal, isNotNull);
    expect(a3.updateRefusal!.reasons.join('; '),
        allOf(contains('serve/2'), contains('another type identity')));
    expect(out3, ['< drawn(again)'],
        reason: 'the refresh is taken by the second version\'s code');
    expect(store.programSaves, 2, reason: 'the store keeps the old version');
    expect(bytesEqual(await keptIdentity(store), second), isTrue);
    await a3.injectUserInput(ConstTerm('refresh'));
    expect(out3, ['< drawn(again)', '< drawn(again)'],
        reason: 'the agent runs the second version');

    // The state the refused restart saved is the second version's: the next
    // start under the third version refuses it again, and one under the
    // second version restores it as it was saved.
    final (a4, out4) = await launch(_v3, store);
    expect(a4.updateRefusal, isNotNull);
    expect(out4, ['< drawn(again)']);
    final (a5, out5) = await launch(_v2, store);
    expect(a5.updated, isFalse);
    expect(a5.updateRefusal, isNull);
    expect(out5, ['< drawn(again)']);

    // An update that changes agent/2's type, which no goal of the resolvent
    // calls, and keeps serve/2's, restores.
    final (a6, out6) = await launch(_v4, store);
    expect(a6.updated, isTrue);
    expect(a6.updateRefusal, isNull);
    expect(out6, ['< drawn(four)']);
    await a6.injectUserInput(ConstTerm('go'));
    expect(out6, ['< drawn(four)', '< went']);
  });

  test('a file store keeps the program beside the state and the key pair, and '
      'a restart under the program the state was saved under finds it',
      () async {
    final path = '${dir.path}/agents/u.state';
    final (a1, _) = await launch(_v1, FileAgentStateStore(path));
    expect(a1.firstStart, isTrue);
    expect(File('$path.program').existsSync(), isTrue);
    expect(File('$path.key').existsSync(), isTrue);
    final (a2, out2) = await launch(_v2, FileAgentStateStore(path));
    expect(a2.updated, isTrue);
    expect(out2, ['< drawn(none)']);
  });

  test('a saved state of code the store keeps no program of is refused, and '
      'the restart throws', () async {
    final store = MemoryAgentStateStore();
    await launch(_v1, store);
    // A store that has lost its program: the state's code is not the new
    // version's, and the store keeps nothing it could run.
    final lost = MemoryAgentStateStore()..saveKeyPair((await store.loadKeyPair())!);
    await lost.save((await store.load())!);
    File(file).writeAsStringSync(_v2);
    final agent = AgentRuntime(
        agentId: 'u', program: file, rootSelfGlpPath: root, stateStore: lost);
    await expectLater(agent.initialize(), throwsStateError);
  });
}
