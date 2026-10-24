/// An agent's local state survives the termination of its application (IGLP,
/// Definition "Restart": "the local state s_p of p at restart is its local
/// state at termination"; Implementation Notes, "Persistence": "a message is
/// handed to the layer, and a delivered message acknowledged to the layer,
/// only from a state the store holds, and a restart resumes from the state
/// the store holds").
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
/// says `go`; `b` tells its person what the reader becomes.
const String _play = r'''
procedure a_init(_?, _?, _?).
a_init(_, In, _) :- send_to_net([msg(b, value(X?))]), relay(In?, X).

procedure relay(_?, _).
relay([go | _], hello).

procedure b_init(_?, _?).
b_init(_, [msg(_, value(V)) | _]) :- show(V?).

procedure show(_?).
show(hello) :- send_to_user([got(hello)]).
''';

/// `c` cold-calls `b` and leaves sender/1 waiting on when_idle, which
/// cold-calls `b` again when it passes, and waiter/1, which waits on
/// sender/1's assignment and then on when_idle (when_idle_test.dart).
const String _idle = r'''
Go ::= go.

procedure c_init(_?, _?).
c_init(_, _) :- send_to_net([msg(b, ping)]), sender(X), waiter(X?).

procedure sender(Go).
sender(go) :- when_idle | send_to_net([msg(b, pong)]), send_to_user([sender]).

procedure waiter(Go?).
waiter(go) :- when_idle | send_to_user([waiter]).
''';

/// A message the layer holds: its sender, its recipient, its payload and the
/// id the layer gave it.
typedef _Held = ({String from, String to, Uint8List payload, String id});

/// A store whose saves wait until the test lets them complete.
class _GatedStore extends MemoryAgentStateStore {
  Completer<void>? gate;

  @override
  Future<void> save(Uint8List state) async {
    final g = gate;
    if (g != null) await g.future;
    await super.save(state);
  }
}

/// A store whose next save, once [terminate] is set, keeps the state and then never completes: the application terminates after the save and before anything that follows it in the cycle.
class _TerminatingStore extends MemoryAgentStateStore {
  bool terminate = false;

  @override
  Future<void> save(Uint8List state) async {
    await super.save(state);
    if (terminate) {
      terminate = false;
      await Completer<void>().future;
    }
  }
}

/// A store whose saves of the numbers in [failing], counted from 1, fail.
class _FailingStore extends MemoryAgentStateStore {
  final Set<int> failing;
  int attempts = 0;

  _FailingStore(this.failing);

  @override
  Future<void> save(Uint8List state) async {
    attempts++;
    if (failing.contains(attempts)) {
      throw const FileSystemException('the state was not kept');
    }
    await super.save(state);
  }
}

void main() {
  late String root;
  late Directory dir;
  late String play;
  late String idle;

  setUpAll(() {
    // Under the root, so the program's scope holds the root self.glp
    // (agent_runtime_test.dart).
    dir = Directory('../programs/tests').createTempSync('glp_agent_restart_');
    play = '${dir.path}/play.glp';
    File(play).writeAsStringSync(_play);
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

  AgentRuntime host(String id, String goal, AgentStateStore store,
      ({PubKey pub, Uint8List priv}) kp) {
    final agent = AgentRuntime(
      agentId: id,
      program: play,
      rootSelfGlpPath: root,
      goalLabel: goal,
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

    final a1 = host('a', 'a_init/3', storeA, kpA);
    final b1 = host('b', 'b_init/2', storeB, kpB);
    await a1.initialize();
    await b1.initialize();
    expect(File('${dir.path}/a.state').existsSync(), isTrue,
        reason: 'the initial start is a cycle, and its end saves the state');
    expect(queue.map((m) => '${m.from}>${m.to}'), ['a>b'],
        reason: 'the cold call waits in the layer');

    // b applies the cold call: its localization creates the entry of a's
    // reader, _r(a, 1), and b's goal suspends on it.
    final coldCall = await deliverTo('b', b1);
    expect(b1.ctx!.wp.findByRemote('a', 1), isNotNull);
    expect(delivered['b'], [coldCall.single.id],
        reason: 'the layer is told the cold call is delivered once the cycle '
            'that applied it is saved');

    // a's person acts: the writer is assigned, the value leaves a, and is
    // pending there until acknowledged.
    await a1.injectUserInput(ConstTerm('go'));
    expect(a1.ctx!.hasPendingReaderValue('a', 1), isTrue);
    final value = queue.where((m) => m.from == 'a' && m.to == 'b').toList();
    expect(value, hasLength(1), reason: 'the value waits in the layer');

    // Both applications terminate.  The layer keeps its queue.
    final restartedFrom = out['b']!.length;

    // Both restart, each from its store, and go on from the state saved at
    // the end of its last cycle: no entry goal is posted again.
    final a2 = host('a', 'a_init/3', FileAgentStateStore('${dir.path}/a.state'),
        kpA);
    final b2 = host('b', 'b_init/2', storeB, kpB);
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

  test('the layer is told a message is delivered only once the cycle that '
      'applied it is saved', () async {
    final storeB = _GatedStore();
    final a = host('a', 'a_init/3', MemoryAgentStateStore(), generateKeyPair());
    final b = host('b', 'b_init/2', storeB, generateKeyPair());
    await a.initialize();
    await b.initialize();

    storeB.gate = Completer<void>();
    final coldCall = queue.single;
    queue.clear();
    final cycle =
        b.onMadMessageReceived('a', coldCall.payload, messageId: coldCall.id);
    // The cycle has applied the message and waits on its save.
    await Future<void>.delayed(Duration.zero);
    await Future<void>.delayed(Duration.zero);
    expect(b.ctx!.wp.findByRemote('a', 1), isNotNull);
    expect(delivered['b'], isEmpty,
        reason: 'the cycle that applied the message is not yet saved');

    storeB.gate!.complete();
    await cycle;
    expect(delivered['b'], [coldCall.id]);
  });

  test('an agent terminated after its cycle\'s save and before the hand-over '
      'finds the message undelivered at restart and hands it over, once',
      () async {
    final kpA = generateKeyPair();
    final storeA = _TerminatingStore();
    final a1 = host('a', 'a_init/3', storeA, kpA);
    final b = host('b', 'b_init/2', MemoryAgentStateStore(), generateKeyPair());
    await a1.initialize();
    await b.initialize();
    // b applies the cold call, and its localization creates the entry of
    // a's reader, _r(a, 1).
    await deliverTo('b', b);
    expect(queue, isEmpty);

    // a's person acts: the cycle assigns the writer, and the value's message
    // is in M_p, undelivered.  The save keeps the state, and the application
    // terminates before the hand-over.
    storeA.terminate = true;
    final saves = storeA.saves;
    unawaited(a1.injectUserInput(ConstTerm('go')));
    while (storeA.saves == saves) {
      await Future<void>.delayed(Duration.zero);
    }
    await Future<void>.delayed(Duration.zero);
    expect(queue, isEmpty,
        reason: 'a message is handed to the layer only from a state the '
            'store holds, after the save, and the application terminated '
            'before the hand-over');
    expect(a1.ctx!.mp.sendableLength, 1,
        reason: 'the state saved holds the message undelivered');

    // a restarts from the state the store holds, and hands the message over.
    final a2 = host('a', 'a_init/3', storeA, kpA);
    await a2.initialize();
    expect(queue.map((m) => '${m.from}>${m.to}'), ['a>b'],
        reason: 'the restart hands the undelivered message over, once');
    expect(a2.ctx!.mp.sendableLength, 0);
    expect(a2.ctx!.hasPendingReaderValue('a', 1), isTrue,
        reason: 'the value is pending until b acknowledges it');

    // The state saved after the hand-over holds the message delivered: a
    // restart from it hands nothing over again.
    final a3 = host('a', 'a_init/3', storeA, kpA);
    await a3.initialize();
    expect(queue.map((m) => '${m.from}>${m.to}'), ['a>b'],
        reason: 'the message is handed over once');
    expect(out['a']!.where((l) => l.startsWith('[GOAL] Started')),
        hasLength(1),
        reason: 'the restarted agent posts no entry goal');

    // The value reaches b and is applied; b's acknowledgement releases a's
    // pending value, and nothing more leaves a.
    await deliverTo('b', b);
    expect(out['b'], contains('< got(hello)'));
    expect(queue.map((m) => '${m.from}>${m.to}'), ['b>a']);
    await deliverTo('a', a3);
    expect(a3.ctx!.hasPendingReaderValue('a', 1), isFalse);
    expect(queue, isEmpty);
  });

  test('over a store, a goal waiting on when_idle is re-tried after the '
      'hand-over, each run saved before its messages are handed over; a save '
      'that fails after a hand-over hands nothing over twice', () async {
    /// `c` over [store], its outputs and its Sends recorded in the order
    /// they happen, each Send with the number of saves kept before it.
    Future<List<String>> run(MemoryAgentStateStore store) async {
      final events = <String>[];
      final c = AgentRuntime(
        agentId: 'c',
        program: idle,
        rootSelfGlpPath: root,
        goalLabel: 'c_init/2',
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

    // The boot's cycle: the cold call is saved and handed over; sender/1
    // passes and cold-calls b again, which is saved and handed over; waiter/1
    // then passes, and its run is saved.
    final store = MemoryAgentStateStore();
    expect(await run(store), [
      'send b after 1',
      'out sender',
      'send b after 2',
      'out waiter',
    ]);
    expect(store.saves, 3);

    // The save of the run after the first hand-over fails: the agent goes
    // back to the state saved before it, which holds the cold call
    // undelivered, takes the cold call out of M_p, since it was handed over,
    // and makes the run again.  Each message is handed over once.
    final failing = _FailingStore({2});
    expect(await run(failing), [
      'send b after 1',
      'out sender',
      'out sender',
      'send b after 2',
      'out waiter',
    ]);
  });

  test('a cycle whose save fails is not complete, and is run again from the '
      'state saved before it', () async {
    final storeB = MemoryAgentStateStore();
    final kpB = generateKeyPair();
    final a = host('a', 'a_init/3', MemoryAgentStateStore(), generateKeyPair());
    final b = host('b', 'b_init/2', storeB, kpB);
    final logs = <String>[];
    b.onLog = (_, m) => logs.add(m);
    await a.initialize();
    await b.initialize();
    final savesBefore = storeB.saves;

    storeB.failNextSaves = 1;
    await deliverTo('b', b);
    expect(logs.where((l) => l.startsWith('SAVE FAILED')), hasLength(1));
    expect(storeB.saves, savesBefore + 1,
        reason: 'the cycle run again is saved');
    expect(delivered['b'], hasLength(1),
        reason: 'the layer is told once, after the save');
    expect(b.ctx!.wp.findByRemote('a', 1), isNotNull);
    expect(b.ctx!.wp.serializerAppendedCount, 1,
        reason: 'the cold call is appended once, the cycle having been run '
            'again from the state saved before it');

    // The state saved is the state after the cycle: a restart from it holds
    // the entry.
    final b2 = host('b', 'b_init/2', storeB, kpB);
    await b2.initialize();
    expect(b2.ctx!.wp.findByRemote('a', 1), isNotNull);
  });

  test('a saved state is refused under another key', () async {
    final store = MemoryAgentStateStore();
    final a = host('a', 'a_init/3', store, generateKeyPair());
    await a.initialize();
    final other = host('a', 'a_init/3', store, generateKeyPair());
    await expectLater(other.initialize(), throwsStateError);
  });
}
