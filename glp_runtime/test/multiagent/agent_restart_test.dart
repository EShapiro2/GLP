/// An agent's local state survives the termination of its application (IGLP
/// d70ec2e, Definition "Restart": "the local state s_p of p at restart is its
/// local state at termination"; Implementation Notes, "Persistence": the
/// runtime keeps it by a store, "saving it when the application goes to the
/// background, which the platform announces before it may kill the
/// application, and at no other time"; "A message is handed to the layer, and
/// a delivered message acknowledged to the layer, in the cycle that makes or
/// applies it, and a restart resumes from the state the store holds").  The
/// host's call is [AgentRuntime.save], made here where the phone goes to the
/// background.
///
/// "A madGLP run therefore continues across a restart ...: a message the
/// layer delivers after a restart finds the entry its localization created, a
/// pending value is still pending, and a Send class that was enabled is
/// enabled still."  Two agents, `a` and `b`, each saving its state in a store
/// of its own: `a` cold-calls `b` with a reader, and once its person acts
/// assigns the reader's writer, so the value leaves `a` and is pending there
/// until `b` acknowledges it.  Both applications terminate while the layer
/// holds the value undelivered; both restart from their stores; the layer
/// delivers the value, which finds `b`'s entry and is applied, and `b`'s
/// acknowledgement releases `a`'s pending value.
///
/// The layer here is the test's: a queue of the messages sent, each with an
/// id, which survives the agents' termination as GNA's queue does (GNA,
/// "Networking Assumptions", Fair message delivery: "the queue survives the
/// termination of the application").
@Timeout.none
library;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show PubKey;
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/terms.dart';

/// `a` sends `b` a reader, and assigns its writer `hello` when its person
/// says `go`; `b` tells its person what the reader becomes.  Each is its
/// agent's one program, started from agent(ch(_?,_),ch(_?,_)), every agent's
/// initial goal (GLP-Spec maglp.tex, Definition "Multiagent GLP"; IGLP,
/// Implementation Notes, "Boot").
const String _playA = r'''
RsMsg ::= value(Constant).

exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(RsMsg)), Stream(NetMsg(RsMsg)))?).
agent(ch(In, []), ch(_, [])) :- send_to_net([msg(b, value(X?))]), relay(In?, X).

procedure relay(Stream(Constant)?, Constant).
relay([go | _], hello).
relay([], none).
''';

const String _playB = r'''
RsMsg ::= value(Constant).
RsOut ::= got(Constant).

exported procedure agent(Channel(Stream(Constant), Stream(RsOut))?, Channel(Stream(NetMsg(RsMsg)), Stream(NetMsg(RsMsg)))?).
agent(ch(_, []), ch(N, [])) :- b_in(N?).

procedure b_in(Stream(NetMsg(RsMsg))?).
b_in([msg(_, value(V)) | _]) :- show(V?).
b_in([]).

procedure show(Constant?).
show(hello) :- send_to_user([got(hello)]).
''';

/// `c` cold-calls `b` and leaves sender/1 waiting on when_idle, which
/// cold-calls `b` again when it passes, and waiter/1, which waits on
/// sender/1's assignment and then on when_idle (when_idle_test.dart).
const String _idle = r'''
Go ::= go.

exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(_, []), ch(_, [])) :- send_to_net([msg(b, ping)]), sender(X), waiter(X?).

procedure sender(Go).
sender(go) :- when_idle | send_to_net([msg(b, pong)]), send_to_user([sender]).

procedure waiter(Go?).
waiter(go) :- when_idle | send_to_user([waiter]).
''';

/// A message the layer holds: its sender, its recipient, its payload and the
/// id the layer gave it.
typedef _Held = ({String from, String to, Uint8List payload, String id});

void main() {
  late String root;
  late Directory dir;
  late String playA;
  late String playB;
  late String idle;

  setUpAll(() {
    // Under the root, so the program's scope holds the root self.glp
    // (agent_runtime_test.dart).
    dir = Directory('../programs/tests').createTempSync('glp_agent_restart_');
    playA = '${dir.path}/play_a.glp';
    File(playA).writeAsStringSync(_playA);
    playB = '${dir.path}/play_b.glp';
    File(playB).writeAsStringSync(_playB);
    idle = '${dir.path}/idle.glp';
    File(idle).writeAsStringSync(_idle);
    root = File('../programs/self.glp').absolute.path;
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  /// The layer's queue, the messages each agent was told are delivered, and
  /// each agent's transcript.
  final queue = <_Held>[];
  final delivered = <String, List<String>>{};
  final out = <String, List<String>>{};
  var nextId = 0;

  setUp(() {
    queue.clear();
    delivered.clear();
    out.clear();
    nextId = 0;
  });

  AgentRuntime host(String id, String program, AgentStateStore store,
      ({PubKey pub, Uint8List priv}) kp) {
    final agent = AgentRuntime(
      agentId: id,
      program: program,
      rootSelfGlpPath: root,
      keyPair: kp,
      stateStore: store,
    );
    out.putIfAbsent(id, () => []);
    delivered.putIfAbsent(id, () => []);
    agent.onOutput = out[id]!.add;
    agent.onDelivered = delivered[id]!.add;
    agent.onSendMadMessage = (to, payload) async => queue.add((
          from: id,
          to: to,
          payload: Uint8List.fromList(payload),
          id: 'm${nextId++}',
        ));
    return agent;
  }

  /// Deliver every message the layer holds for [agent], named [name].
  Future<List<_Held>> deliverTo(String name, AgentRuntime agent) async {
    final mine = queue.where((m) => m.to == name).toList();
    queue.removeWhere((m) => m.to == name);
    for (final m in mine) {
      await agent.onMadMessageReceived(m.from, m.payload, messageId: m.id);
    }
    return mine;
  }

  test('an agent with an open link and a pending value is terminated and '
      'restarted; the message the layer delivers after restart finds its entry '
      'and is applied, and the pending value is acknowledged', () async {
    final kpA = generateKeyPair();
    final kpB = generateKeyPair();
    // a keeps its state in a file the application names, b in the memory
    // of the process that outlives it.
    final storeA = FileAgentStateStore('${dir.path}/a.state');
    final storeB = MemoryAgentStateStore();

    final a1 = host('a', playA, storeA, kpA);
    final b1 = host('b', playB, storeB, kpB);
    await a1.initialize();
    await b1.initialize();
    expect(File('${dir.path}/a.state').existsSync(), isFalse,
        reason: 'nothing is saved until the host calls save: the state is '
            'saved when the app goes to the background, and at no other time');
    expect(queue.map((m) => '${m.from}>${m.to}'), ['a>b'],
        reason: 'the cold call waits in the layer');

    // b applies the cold call: its localization creates the entry of a's
    // reader, _r(a, 1), and b's goal suspends on it.
    final coldCall = await deliverTo('b', b1);
    expect(b1.ctx!.wp.findByRemote('a', 1), isNotNull);
    expect(delivered['b'], [coldCall.single.id],
        reason: 'the layer is told the cold call is delivered in the cycle '
            'that applied it');

    // a's person acts: the writer is assigned, the value leaves a, and is
    // pending there until acknowledged.
    await a1.injectUserInput(ConstTerm('go'));
    expect(a1.ctx!.hasPendingReaderValue('a', 1), isTrue);
    final value = queue.where((m) => m.from == 'a' && m.to == 'b').toList();
    expect(value, hasLength(1), reason: 'the value waits in the layer');

    // Both applications go to the background, which saves each agent's
    // state, and terminate.  The layer keeps its queue.
    expect(await a1.save(), isTrue);
    expect(await b1.save(), isTrue);
    expect(File('${dir.path}/a.state').existsSync(), isTrue);
    a1.dispose();
    b1.dispose();
    final restartedFrom = out['b']!.length;

    // Both restart, each from its store, and go on from the state saved: no
    // entry goal is posted again.
    final a2 = host('a', playA, FileAgentStateStore('${dir.path}/a.state'),
        kpA);
    final b2 = host('b', playB, storeB, kpB);
    await a2.initialize();
    await b2.initialize();
    expect(out['a']!.where((l) => l.startsWith('[GOAL] Started')),
        hasLength(1),
        reason: 'the restarted agent posts no entry goal');
    expect(a2.ctx!.hasPendingReaderValue('a', 1), isTrue,
        reason: 'a pending value is still pending');
    expect(b2.ctx!.wp.findByRemote('a', 1), isNotNull,
        reason: 'the entry its localization created survives');

    // The layer delivers the value after the restart: it finds b's entry and
    // is applied, waking the goal suspended on the reader.
    final delivery = await deliverTo('b', b2);
    expect(delivery.single.id, value.single.id);
    expect(out['b']!.sublist(restartedFrom), contains('< got(hello)'));
    expect(b2.ctx!.wp.findByRemote('a', 1), isNull,
        reason: 'the entry is removed when its value is applied');
    expect(delivered['b'], contains(value.single.id));

    // b's acknowledgement reaches a, and releases its pending value.
    expect(queue.map((m) => '${m.from}>${m.to}'), ['b>a']);
    await deliverTo('a', a2);
    expect(a2.ctx!.hasPendingReaderValue('a', 1), isFalse,
        reason: 'the pending value is acknowledged');
  });

  test('a delivered message is acknowledged in the cycle that applies it, '
      'and nothing is saved in a cycle', () async {
    // IGLP d70ec2e, Implementation Notes, "Persistence": "A message is handed
    // to the layer, and a delivered message acknowledged to the layer, in the
    // cycle that makes or applies it"; the state is saved "when the
    // application goes to the background ... and at no other time".  Until
    // 2026-11-10 a message was acknowledged once a state holding it applied
    // was saved, and every cycle saved.
    final storeB = MemoryAgentStateStore();
    final a = host('a', playA, MemoryAgentStateStore(), generateKeyPair());
    final b = host('b', playB, storeB, generateKeyPair());
    await a.initialize();
    await b.initialize();

    final coldCall = await deliverTo('b', b);
    expect(b.ctx!.wp.findByRemote('a', 1), isNotNull);
    expect(delivered['b'], [coldCall.single.id]);
    expect(storeB.saves, 0, reason: 'no cycle saves');
  });

  test('a save asked for during a cycle is taken after it: the state saved '
      'is one between cycles', () async {
    final storeB = MemoryAgentStateStore();
    final kpB = generateKeyPair();
    final a = host('a', playA, MemoryAgentStateStore(), generateKeyPair());
    final b = host('b', playB, storeB, kpB);
    await a.initialize();
    await b.initialize();

    final coldCall = queue.single;
    queue.clear();
    final cycle =
        b.onMadMessageReceived('a', coldCall.payload, messageId: coldCall.id);
    final saved = b.save();
    await cycle;
    expect(await saved, isTrue);
    expect(storeB.saves, 1);

    // The state saved holds the cold call applied.
    final b2 = host('b', playB, storeB, kpB);
    await b2.initialize();
    expect(b2.ctx!.wp.findByRemote('a', 1), isNotNull);
    expect(b2.ctx!.wp.serializerAppendedCount, 1);
  });

  test('the host saves at the announcement of termination, between cycles, '
      'every Send of the last cycle handed over: a restart from it hands '
      'nothing over again and posts no entry goal; a crash after a cycle not '
      'saved loses that cycle', () async {
    // IGLP d70ec2e, Termination and Restart: "A crash loses what the agent
    // did since, and the agent restarts from the state last saved".
    final kpA = generateKeyPair();
    final storeA = MemoryAgentStateStore();
    final a1 = host('a', playA, storeA, kpA);
    final b = host('b', playB, MemoryAgentStateStore(), generateKeyPair());
    await a1.initialize();
    await b.initialize();
    // b applies the cold call, and its localization creates the entry of
    // a's reader, _r(a, 1).
    await deliverTo('b', b);
    expect(queue, isEmpty);
    expect(storeA.saves, 0);

    // a's person acts: the cycle assigns the writer and hands the value's
    // message to the layer, at once.
    await a1.injectUserInput(ConstTerm('go'));
    expect(queue.map((m) => '${m.from}>${m.to}'), ['a>b']);
    expect(a1.ctx!.mp.sendableLength, 0,
        reason: 'the cycle handed its Send over');

    // The app goes to the background, which saves, and terminates.
    expect(await a1.save(), isTrue);
    expect(storeA.saves, 1);
    a1.dispose();

    // a restarts from the state the store holds: the value is pending there,
    // nothing is handed over again, and no entry goal is posted.
    final a2 = host('a', playA, storeA, kpA);
    await a2.initialize();
    expect(queue.map((m) => '${m.from}>${m.to}'), ['a>b'],
        reason: 'the message is handed over once');
    expect(a2.ctx!.mp.sendableLength, 0);
    expect(a2.ctx!.hasPendingReaderValue('a', 1), isTrue,
        reason: 'the value is pending until b acknowledges it');
    expect(out['a']!.where((l) => l.startsWith('[GOAL] Started')),
        hasLength(1),
        reason: 'the restarted agent posts no entry goal');

    // The value reaches b and is applied; b's acknowledgement releases a's
    // pending value, and nothing more leaves a.
    await deliverTo('b', b);
    expect(out['b'], contains('< got(hello)'));
    expect(queue.map((m) => '${m.from}>${m.to}'), ['b>a']);
    await deliverTo('a', a2);
    expect(a2.ctx!.hasPendingReaderValue('a', 1), isFalse);
    expect(queue, isEmpty);

    // a crashes, with no save since: the acknowledgement's cycle is lost, and
    // a restarts from the state last saved, the value pending still.
    a2.dispose();
    final a3 = host('a', playA, storeA, kpA);
    await a3.initialize();
    expect(a3.ctx!.hasPendingReaderValue('a', 1), isTrue,
        reason: 'the state last saved');
    expect(queue, isEmpty, reason: 'that state holds nothing undelivered');
  });

  test('over a store, a goal waiting on when_idle is re-tried after the '
      'hand-over, as with none, and no run is saved', () async {
    /// `c` over [store], its outputs and its Sends recorded in the order
    /// they happen, each Send with the number of saves kept before it.
    Future<List<String>> run(MemoryAgentStateStore store) async {
      final events = <String>[];
      final c = AgentRuntime(
        agentId: 'c',
        program: idle,
        rootSelfGlpPath: root,
        stateStore: store,
      );
      c.onOutput = (line) {
        if (line.startsWith('< ')) events.add('out ${line.substring(2)}');
      };
      c.onSendMadMessage =
          (to, payload) async => events.add('send $to after ${store.saves}');
      await c.initialize();
      return events;
    }

    // The boot's cycle: the cold call is handed over; sender/1 passes and
    // cold-calls b again, which is handed over; waiter/1 then passes.
    final store = MemoryAgentStateStore();
    expect(await run(store), [
      'send b after 0',
      'out sender',
      'send b after 0',
      'out waiter',
    ]);
    expect(store.saves, 0);
  });

  test('a save that fails leaves the store with the state saved before it, '
      'reported; the agent runs on, and the next save keeps its state',
      () async {
    // IGLP d70ec2e, Implementation Notes, "Persistence": "If the save fails,
    // the store keeps the state saved before it, and the runtime reports the
    // failure."
    final storeB = MemoryAgentStateStore();
    final kpB = generateKeyPair();
    final a = host('a', playA, MemoryAgentStateStore(), generateKeyPair());
    final b = host('b', playB, storeB, kpB);
    final logs = <String>[];
    b.onLog = (_, m) => logs.add(m);
    await a.initialize();
    await b.initialize();
    expect(await b.save(), isTrue, reason: 'the state before the cold call');
    expect(storeB.saves, 1);

    await deliverTo('b', b);
    expect(delivered['b'], hasLength(1),
        reason: 'acknowledged in the cycle, whatever the store');
    expect(b.ctx!.wp.findByRemote('a', 1), isNotNull);

    storeB.failNextSaves = 1;
    expect(await b.save(), isFalse);
    expect(logs.where((l) => l.startsWith('[ERROR]')), hasLength(1));
    expect(storeB.saves, 1, reason: 'the store keeps the state saved before');

    // A restart now resumes from the state the store holds, which has not
    // applied the cold call.
    final early = host('b', playB, storeB, kpB);
    await early.initialize();
    expect(early.ctx!.wp.findByRemote('a', 1), isNull);
    early.dispose();

    // The agent runs on, and the next save keeps its state.
    await a.injectUserInput(ConstTerm('go'));
    await deliverTo('b', b);
    expect(out['b'], contains('< got(hello)'));
    expect(await b.save(), isTrue);
    expect(storeB.saves, 2);
    expect(logs.where((l) => l.startsWith('[ERROR]')), hasLength(1));

    final b2 = host('b', playB, storeB, kpB);
    await b2.initialize();
    expect(b2.ctx!.wp.serializerAppendedCount, 1,
        reason: 'the state saved holds the cold call applied');
  });

  test('a store that refuses every save: each save asked for is reported, '
      'the agent runs on, and the event loop stays free', () async {
    // IGLP, Implementation Notes, "Event-driven execution": the host event
    // loop stays live, "so timers, I/O, and the UI stay live".
    final storeB = MemoryAgentStateStore();
    final a = host('a', playA, MemoryAgentStateStore(), generateKeyPair());
    final b = host('b', playB, storeB, generateKeyPair());
    final logs = <String>[];
    b.onLog = (_, m) => logs.add(m);
    await a.initialize();
    await b.initialize();
    storeB.failNextSaves = 1 << 30;

    final fired = Completer<void>();
    Timer(Duration.zero, fired.complete);
    final saving = b.save();
    await fired.future;
    expect(await saving, isFalse);
    expect(await b.save(), isFalse);
    expect(logs.where((l) => l.startsWith('[ERROR]')), hasLength(2),
        reason: 'each save asked for is reported once');

    await deliverTo('b', b);
    await a.injectUserInput(ConstTerm('go'));
    await deliverTo('b', b);
    expect(out['b'], contains('< got(hello)'),
        reason: 'the agent runs on its state in memory');
    expect(delivered['b'], hasLength(2));
    expect(storeB.saves, 0);
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('a disposed agent takes no event and its save keeps nothing, and the '
      'next launch from its store is the one agent that writes it', () async {
    // IGLP, Implementation Notes, "Event-driven execution": "shutdown is the
    // embedding application's"; Definition "Restart" and "Persistence": one
    // agent and one store per launch.  Cloud #6, Code_inbox 2026-11-08 21:13
    // UTC: dispose did nothing, and two launches in one process wrote one
    // state file.
    final storeA = MemoryAgentStateStore();
    final kpA = generateKeyPair();
    final a = host('a', playA, storeA, kpA);
    final b = host('b', playB, MemoryAgentStateStore(), generateKeyPair());
    await a.initialize();
    await b.initialize();
    expect(await a.save(), isTrue);
    final logs = <String>[];
    a.onLog = (_, m) => logs.add(m);
    final saves = storeA.saves;
    final sent = queue.length;

    a.dispose();
    expect(a.disposed, isTrue);
    await a.injectUserInput(ConstTerm('go'));
    expect(await a.save(), isFalse, reason: 'no save after the dispose');
    expect(storeA.saves, saves);
    expect(queue.length, sent, reason: 'nothing handed to the layer');
    expect(out['a']!.where((l) => l == '> go'), isEmpty,
        reason: 'the channel to the person is closed');
    expect(logs.where((l) => l.contains('is not taken')), isNotEmpty);

    // The next launch from the same store goes on from it, and saves in it.
    final a2 = host('a', playA, storeA, kpA);
    await a2.initialize();
    expect(a2.firstStart, isFalse);
    await a2.injectUserInput(ConstTerm('go'));
    expect(await a2.save(), isTrue);
    expect(storeA.saves, greaterThan(saves));
    expect(a.disposed, isTrue);
  });

  test('a saved state is refused under another key', () async {
    final store = MemoryAgentStateStore();
    final a = host('a', playA, store, generateKeyPair());
    await a.initialize();
    expect(await a.save(), isTrue);
    final other = host('a', playA, store, generateKeyPair());
    await expectLater(other.initialize(), throwsStateError);
  });

  test('a saved state the runtime cannot read is a loss of the state: '
      'reported, and the agent starts afresh from the program, under the key '
      'pair the store keeps; it does not throw', () async {
    // IGLP d70ec2e, Implementation Notes, "Persistence": "A saved state the
    // runtime cannot read, as one written under an earlier format of the
    // store, is a loss of the state: the runtime reports it and starts the
    // agent afresh, under the key pair the store keeps."  Seen on the
    // directory server, whose state file was written under store version 1
    // (GLP #14, Code_inbox 2026-11-10 08:11 UTC, item 3).
    final store = MemoryAgentStateStore();
    AgentRuntime launch() {
      final agent = AgentRuntime(
          agentId: 'a', program: playA, rootSelfGlpPath: root, stateStore: store);
      agent.onOutput = out.putIfAbsent('a', () => []).add;
      agent.onSendMadMessage = (to, payload) async => queue.add((
            from: 'a',
            to: to,
            payload: Uint8List.fromList(payload),
            id: 'm${nextId++}',
          ));
      return agent;
    }

    final a1 = launch();
    await a1.initialize();
    expect(await a1.save(), isTrue);
    a1.dispose();
    final kept = (await store.loadKeyPair())!;

    // The state as an earlier format of the store wrote it: the magic, the
    // version byte 1, and what this runtime does not read.
    final old = Uint8List.fromList((await store.load())!);
    old[4] = 1;
    await store.save(old);

    final logs = <String>[];
    final a2 = launch();
    a2.onLog = (_, m) => logs.add(m);
    await a2.initialize();
    expect(a2.firstStart, isTrue, reason: 'the agent starts afresh');
    expect(logs.where((l) => l.startsWith('[ERROR] the saved state cannot be '
        'read, and is lost')), hasLength(1));
    expect(out['a']!.where((l) => l.startsWith('[GOAL] Started')),
        hasLength(2), reason: 'the entry goal is posted again');
    expect(queue.map((m) => '${m.from}>${m.to}'), ['a>b', 'a>b'],
        reason: 'the fresh start cold-calls b again');
    expect((await store.loadKeyPair())!.pub, kept.pub,
        reason: 'under the key pair the store keeps');
    // The agent runs, and its next save replaces the lost state.
    await a2.injectUserInput(ConstTerm('go'));
    expect(await a2.save(), isTrue);
    final a3 = launch();
    await a3.initialize();
    expect(a3.firstStart, isFalse);
    expect(a3.ctx!.hasPendingReaderValue('a', 1), isTrue);
  });
}
