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

import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show Application;
import 'package:glp_runtime/runtime/body_kernels.dart' show formatTerm;
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

/// The first version with serve/2's second argument a State: a type changed,
/// a goal of the resolvent calling serve/2, and the code the same.
const String _t2 = r'''
Act ::= go ; refresh.
Shown ::= went ; drawn(Constant).
State ::= none ; gone.

exported procedure agent(Channel(Stream(Act), Stream(Shown))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- serve(In?, none).

procedure serve(Stream(Act)?, State?).
serve([go | In], _) :- send_to_user([went]), serve(In?, gone).
serve([refresh | In], gone) :- send_to_user([drawn(gone)]), serve(In?, gone).
serve([refresh | In], none) :- send_to_user([drawn(none)]), serve(In?, none).
serve([], _).
''';

/// The first version with Shown gaining an alternative: agent/2's type
/// changed, which no goal of the resolvent calls, serve/2's kept, and the
/// code the same.
const String _t3 = r'''
Act ::= go ; refresh.
Shown ::= went ; drawn(Constant) ; more.

exported procedure agent(Channel(Stream(Act), Stream(Shown))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- serve(In?, none).

procedure serve(Stream(Act)?, Constant?).
serve([go | In], _) :- send_to_user([went]), serve(In?, gone).
serve([refresh | In], gone) :- send_to_user([drawn(gone)]), serve(In?, gone).
serve([refresh | In], none) :- send_to_user([drawn(none)]), serve(In?, none).
serve([], _).
''';

/// A version whose person's read has the agent read note.txt in its
/// program's directory with load_file/2 and show its text.
const String _l1 = r'''
Act ::= read ; refresh.
Shown ::= note(String).

exported procedure agent(Channel(Stream(Act), Stream(Shown))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- serve(In?, one).

procedure serve(Stream(Act)?, Constant?).
serve([read | In], S) :- load_file("note.txt", C), show(C?), serve(In?, S?).
serve([refresh | In], S) :- serve(In?, S?).
serve([], _).

procedure show(Content?).
show(C) :- string(C?) | send_to_user([note(C?)]).
''';

/// A new version that changes serve/2's type, which the restart refuses, and
/// its code, which reads another file.
const String _l2 = r'''
Act ::= read ; refresh.
Shown ::= note(String).
Mark ::= one ; two.

exported procedure agent(Channel(Stream(Act), Stream(Shown))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- serve(In?, one).

procedure serve(Stream(Act)?, Mark?).
serve([read | In], S) :- load_file("note2.txt", C), show(C?), serve(In?, S?).
serve([refresh | In], S) :- serve(In?, S?).
serve([], _).

procedure show(Content?).
show(C) :- string(C?) | send_to_user([note(C?)]).
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
      String version, AgentStateStore store, {Application? installed}) async {
    File(file).writeAsStringSync(version);
    final drawn = <String>[];
    final agent = AgentRuntime(
      agentId: 'u',
      program: file,
      rootSelfGlpPath: root,
      stateStore: store,
      installedApp: installed,
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
    // The app goes to the background, which saves the state (IGLP d70ec2e,
    // Implementation Notes, "Persistence"), before each termination below.
    expect(await a1.save(), isTrue);

    // The application is updated, keeping serve/2 and its type: the state is
    // restored under the new version, and refresh draws with its code.
    final (a2, out2) = await launch(_v2, store);
    expect(a2.firstStart, isFalse);
    expect(a2.updated, isTrue);
    expect(a2.updateRefusal, isNull);
    expect(out2, ['< drawn(again)'],
        reason: 'the refresh is taken by the new version\'s code, from the '
            'restored state');
    expect(store.programSaves, 1,
        reason: 'the new version is kept only once a state restored under it '
            'is saved');
    await a2.injectUserInput(ConstTerm('refresh'));
    expect(out2, ['< drawn(again)', '< drawn(again)']);
    expect(await a2.save(), isTrue);
    expect(store.programSaves, 2,
        reason: 'the store keeps the new version with the first save under it');
    final second = await keptIdentity(store);
    expect(bytesEqual(second, first), isFalse);

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
    expect(await a3.save(), isTrue);
    expect(store.programSaves, 2);

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

  test('self_app/2: the app whose program the agent runs is the installed '
      'app, and after a refused update the app the store keeps with the '
      'program it runs; the installed app is the one the host gives',
      () async {
    // GLP-Spec cce13ce, appendix-guards, "Networking seam": "self_app(R, I)
    // assigns R app(D, N, V) for the app whose program the agent runs, the
    // app the networking layer attests to the agent's peers, and I the same
    // for the app installed on the device; the two differ after an update the
    // runtime refused".  IGLP 59c40e2, Implementation Notes, "The contract":
    // the version the layer attests is "the one the runtime gives it, that
    // of the program the agent runs".
    Application app(int v) =>
        Application(developer: 'TEAM', name: 'app.grassroots', version: v);
    final store = MemoryAgentStateStore();

    final (a1, _) = await launch(_v1, store, installed: app(1));
    expect(a1.runningApp, app(1));
    expect(StoredProgram.fromBytes((await store.loadProgram())!).app, app(1),
        reason: 'the store keeps the app with the program');
    expect(await a1.save(), isTrue);

    // An update the runtime takes: the program the agent runs is the
    // installed app's.
    final (a2, _) = await launch(_v2, store, installed: app(2));
    expect(a2.updated, isTrue);
    expect(a2.runningApp, app(2));
    expect(await a2.save(), isTrue);
    expect(StoredProgram.fromBytes((await store.loadProgram())!).app, app(2));

    // An update the runtime refuses: the agent runs the program the store
    // keeps, version 2, while version 3 is installed.
    final (a3, _) = await launch(_v3, store, installed: app(3));
    expect(a3.updateRefusal, isNotNull);
    expect(a3.runningApp, app(2));
    expect(a3.runtime!.selfApps!.installed, app(3));
    expect(StoredProgram.fromBytes((await store.loadProgram())!).app, app(2),
        reason: 'the store keeps the version the agent runs');
  });

  test('a new version that changes a type and not the code is an update, '
      'compared by type identity: refused where a goal of the resolvent calls '
      'the procedure whose type changed, taken where none does', () async {
    // IGLP, Implementation Notes, "An update": each goal's procedure "must be
    // defined there with the same type identity".  Until 2026-11-09 the
    // runtime compared the code alone, so a version of the same code took the
    // path of the program the state was saved under, and its restore threw.
    final store = MemoryAgentStateStore();
    final (a1, out1) = await launch(_v1, store);
    await a1.injectUserInput(ConstTerm('go'));
    expect(out1, ['< went']);
    expect(await a1.save(), isTrue);
    final first = await keptIdentity(store);

    final (a2, out2) = await launch(_t2, store);
    expect(a2.updateRefusal, isNotNull);
    expect(a2.updateRefusal!.reasons.join('; '),
        allOf(contains('serve/2'), contains('another type identity')));
    expect(out2, ['< drawn(gone)'],
        reason: 'the version the agent terminated under draws');
    expect(await a2.save(), isTrue);
    expect(bytesEqual(await keptIdentity(store), first), isTrue);

    final (a3, out3) = await launch(_t3, store);
    expect(a3.updateRefusal, isNull);
    expect(a3.updated, isTrue);
    expect(out3, ['< drawn(gone)']);
    await a3.injectUserInput(ConstTerm('refresh'));
    expect(out3, ['< drawn(gone)', '< drawn(gone)']);
  });

  test('the store keeps the files of the program\'s directory with the '
      'program, and after a refused update load_file/2 reads those, not the '
      'new version\'s', () async {
    // IGLP 96cd44a, Implementation Notes, "Persistence": the store keeps "the
    // compiled program it runs with the files of its directory, which
    // load_file/2 reads"; "An update": "otherwise the agent runs the compiled
    // program the store holds, and load_file/2 reads the files the store
    // holds with it".
    final note = File('${dir.path}/note.txt');
    final store = MemoryAgentStateStore();
    note.writeAsStringSync('first');
    final (a1, out1) = await launch(_l1, store);
    await a1.injectUserInput(ConstTerm('read'));
    expect(out1, ['< note(first)']);
    expect(await a1.save(), isTrue);
    final kept = StoredProgram.fromBytes((await store.loadProgram())!);
    expect(utf8.decode(kept.files['note.txt']!), 'first');
    expect(kept.files.keys, contains('app.glp'));

    // The new version is installed with its own note.txt; the update is
    // refused, and the agent reads the note kept with the program it runs.
    note.writeAsStringSync('second');
    final (a2, out2) = await launch(_l2, store);
    expect(a2.updateRefusal, isNotNull);
    await a2.injectUserInput(ConstTerm('read'));
    expect(out2, ['< note(first)']);
    expect(await a2.save(), isTrue);

    // A restart under the version the store keeps, which is no update, reads
    // the directory.
    final (a3, out3) = await launch(_l1, store);
    expect(a3.updated, isFalse);
    expect(a3.updateRefusal, isNull);
    await a3.injectUserInput(ConstTerm('read'));
    expect(out3, ['< note(second)']);
    note.deleteSync();
  });

  test('self_app(R, I) assigns app(D, N, V) of the two apps the runtime '
      'holds, and none where the host gave none', () async {
    final engine = GlpEngine(rootSelfGlpPath: root);
    var result = await engine.runGoal('self_app(R, I)');
    expect('${result.bindings['R']}', contains('none'));
    expect('${result.bindings['I']}', contains('none'));

    engine.runtime.selfApps = (
      running: const Application(
          developer: 'TEAM', name: 'app.grassroots', version: 2),
      installed: const Application(
          developer: 'TEAM', name: 'app.grassroots', version: 3),
    );
    result = await engine.runGoal('self_app(R, I)');
    expect(formatTerm(result.bindings['R']!), "app('TEAM', 'app.grassroots', 2)");
    expect(formatTerm(result.bindings['I']!), "app('TEAM', 'app.grassroots', 3)");
  });

  test('a file store keeps the program beside the state and the key pair, and '
      'a restart under the program the state was saved under finds it',
      () async {
    final path = '${dir.path}/agents/u.state';
    final (a1, _) = await launch(_v1, FileAgentStateStore(path));
    expect(a1.firstStart, isTrue);
    expect(File('$path.program').existsSync(), isTrue);
    expect(File('$path.key').existsSync(), isTrue);
    expect(await a1.save(), isTrue);
    final (a2, out2) = await launch(_v2, FileAgentStateStore(path));
    expect(a2.updated, isTrue);
    expect(out2, ['< drawn(none)']);
  });

  test('a saved state of code the store keeps no program of cannot be read, '
      'and is lost: reported, and the agent starts afresh under its key pair',
      () async {
    // IGLP d70ec2e, Implementation Notes, "Persistence": "A saved state the
    // runtime cannot read ... is a loss of the state: the runtime reports it
    // and starts the agent afresh, under the key pair the store keeps."
    // Until 2026-11-10 the restart threw.
    final store = MemoryAgentStateStore();
    final (a1, _) = await launch(_v1, store);
    await a1.injectUserInput(ConstTerm('go'));
    expect(await a1.save(), isTrue);
    // A store that has lost its program: the state's code is not the new
    // version's, and the store keeps nothing it could run.
    final lost = MemoryAgentStateStore()..saveKeyPair((await store.loadKeyPair())!);
    await lost.save((await store.load())!);
    final (a2, out2) = await launch(_v2, lost);
    expect(a2.firstStart, isTrue);
    expect(out2, [contains('saved state cannot be read and is lost')]);
    expect((await lost.loadKeyPair())!.pub, (await store.loadKeyPair())!.pub);
    await a2.injectUserInput(ConstTerm('refresh'));
    expect(out2.last, '< drawn(none)', reason: 'afresh, nothing went');
  });
}
