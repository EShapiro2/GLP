/// Epochs (IGLP db0a117, madglp-spec.tex, Definitions "Epoch", "Global
/// Variable Name", "Requests and Acknowledgements", "madGLP Local State" and
/// the paragraph after it, "Retire", "madGLP Send Transaction", "Suspension,
/// Termination, Crash", "Restart", "Restart after a Crash"; the Persistence
/// items of appendix-implementation-notes.tex).  An agent p is in an epoch
/// e_p, 1 at its first start and one more at every restart after a crash;
/// every global name carries its anchor's epoch, `_w(p, e, i)`, `_r(p, e,
/// i)`, and every message its sender's; each link end records the other
/// end's epoch; applying a message from p sets E_q(p), and q retires every
/// link end whose other end is at p and whose recorded epoch of p is below
/// it; traffic for an end the receiver lost is answered gone(G), which
/// retires the sender's end.
///
/// The running mark (Persistence, When): set at every start and every
/// resumption, cleared by the save; a start that finds it set is a restart
/// after a crash, which opens the next epoch, retires every link end but the
/// serializer entry, and saves at once; a start that finds it clear keeps
/// the epoch and the links.
///
/// The host's calls are [AgentRuntime.save] at the platform's announcement
/// of termination and [AgentRuntime.resume] at the resumption; a termination
/// with no save is a crash.  Two agents, `a` and `b`: `a` cold-calls `b`
/// with a reader, and sends `b` a ping when its person says so.
@Timeout.none
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show PubKey;
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/codec.dart' show wireMsgKindGone;
import 'package:glp_runtime/wire/payload_codec.dart';

/// `a` sends `b` a reader at its start, assigns its writer `hello` when its
/// person says `go`, and cold-calls `b` with `ping` when its person says
/// `ping`; the `refresh` the runtime grants at a restart it passes over.
const String _playA = r'''
RsMsg ::= value(Constant) ; ping.

exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(Stream(NetMsg(RsMsg)), Stream(NetMsg(RsMsg)))?).
agent(ch(In, []), ch(_, [])) :- send_to_net([msg(b, value(X?))]), relay(In?, X).

procedure relay(Stream(Constant)?, Constant).
relay([go | _], hello).
relay([ping | In], X?) :- send_to_net([msg(b, ping)]), relay(In?, X).
relay([refresh | In], X?) :- relay(In?, X).
relay([], none).
''';

/// `b` tells its person what the reader becomes, `got(hello)`, or that it
/// is widowed: its writer, held by the link end `b` kept for it, occurs
/// nowhere once that end is retired (the GLP paper 07835e1,
/// appendix-guards.tex, the paragraph on link ends after the guard table).
const String _playB = r'''
RsMsg ::= value(Constant) ; ping.
RsOut ::= got(Constant) ; widowed.

exported procedure agent(Channel(Stream(Constant), Stream(RsOut))?, Channel(Stream(NetMsg(RsMsg)), Stream(NetMsg(RsMsg)))?).
agent(ch(_, []), ch(N, [])) :- b_in(N?).

procedure b_in(Stream(NetMsg(RsMsg))?).
b_in([msg(_, value(V)) | _]) :- show(V?).
b_in([msg(_, ping) | In]) :- b_in(In?).
b_in([]).

procedure show(Constant?).
show(hello) :- send_to_user([got(hello)]).
show(V) :- widowed_reader(V?) | send_to_user([widowed]).
''';

/// A message the layer holds: its sender, its recipient, its payload and the
/// id the layer gave it.
typedef _Held = ({String from, String to, Uint8List payload, String id});

void main() {
  late String root;
  late Directory dir;
  late String playA;
  late String playB;

  setUpAll(() {
    dir = Directory('../programs/tests').createTempSync('glp_epochs_');
    playA = '${dir.path}/play_a.glp';
    File(playA).writeAsStringSync(_playA);
    playB = '${dir.path}/play_b.glp';
    File(playB).writeAsStringSync(_playB);
    root = File('../programs/self.glp').absolute.path;
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  final queue = <_Held>[];
  final out = <String, List<String>>{};
  var nextId = 0;

  setUp(() {
    queue.clear();
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
    agent.onOutput = out[id]!.add;
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

  /// The application of [agent] goes to the background, which saves, and
  /// comes back, which sets the running mark again; then it terminates with
  /// no save: a crash.
  Future<void> crash(AgentRuntime agent) async {
    expect(await agent.save(), isTrue);
    await agent.resume();
    agent.dispose();
  }

  final rA1 = GlobalName.reader('a', 1); // _r(a, 1, 1)

  test('a start that finds the running mark set is a restart after a crash: '
      'the next epoch opens, every link end but the serializer entry is '
      'retired, and the state is saved at once; a second crash opens the '
      'next', () async {
    final storeA = MemoryAgentStateStore();
    final kpA = generateKeyPair();
    final a1 = host('a', playA, storeA, kpA);
    final b = host('b', playB, MemoryAgentStateStore(), generateKeyPair());
    await a1.initialize();
    expect(a1.ctx!.epoch, 1, reason: 'the first start opens epoch 1');
    expect(await storeA.loadRunning(), isTrue,
        reason: 'the mark is set at the start');
    await b.initialize();
    await deliverTo('b', b);
    expect(a1.ctx!.globalSendRegistry.pendingCount, 1,
        reason: 'the global_send goal of the reader sent is the link end');
    expect(await a1.save(), isTrue);
    expect(await storeA.loadRunning(), isFalse,
        reason: 'the save clears the mark');
    await a1.resume();
    expect(await storeA.loadRunning(), isTrue,
        reason: 'the resumption sets it again');
    final saves = storeA.saves;
    a1.dispose(); // no save: a crash

    final a2 = host('a', playA, storeA, kpA);
    await a2.initialize();
    expect(a2.restartedAfterCrash, isTrue);
    expect(a2.ctx!.epoch, 2);
    expect(a2.ctx!.globalSendRegistry.pendingCount, 0,
        reason: 'every link end retired');
    expect(a2.ctx!.hasRetired(rA1), isTrue);
    expect(a2.ctx!.wp.serializerWriterAddr, isNotNull,
        reason: 'but the serializer entry');
    expect(storeA.saves, saves + 1, reason: 'the state is saved at once');
    expect(out['a'], contains(startsWith('[INIT] The agent restarts after a '
        'crash, in epoch 2')));
    expect(out['a']!.where((l) => l.startsWith('[GOAL] Started')),
        hasLength(1), reason: 'no entry goal is posted again');

    // The person acts: the writer is assigned, but its reader's link end is
    // gone, so nothing leaves a.
    await a2.injectUserInput(ConstTerm('go'));
    expect(queue.where((m) => m.from == 'a'), isEmpty);
    expect(a2.ctx!.hasPendingReaderValue('a', 1), isFalse);

    // A restart from the state the crash restart saved, with no save since,
    // is a crash again: epoch 3.
    a2.dispose();
    final a3 = host('a', playA, storeA, kpA);
    await a3.initialize();
    expect(a3.restartedAfterCrash, isTrue);
    expect(a3.ctx!.epoch, 3);
  });

  test('a start that finds the running mark clear keeps the epoch and the '
      'links, as a graceful restart does', () async {
    final storeA = MemoryAgentStateStore();
    final kpA = generateKeyPair();
    final a1 = host('a', playA, storeA, kpA);
    final b = host('b', playB, MemoryAgentStateStore(), generateKeyPair());
    await a1.initialize();
    await b.initialize();
    await deliverTo('b', b);
    expect(await a1.save(), isTrue);
    a1.dispose();

    final a2 = host('a', playA, storeA, kpA);
    await a2.initialize();
    expect(a2.restartedAfterCrash, isFalse);
    expect(a2.ctx!.epoch, 1);
    expect(a2.ctx!.globalSendRegistry.pendingCount, 1,
        reason: 'the link end stands');
    await a2.injectUserInput(ConstTerm('go'));
    final value = await deliverTo('b', b);
    expect(value, hasLength(1));
    expect(PayloadCodec.senderEpochOf(value.single.payload), 1);
    expect(out['b'], contains('< got(hello)'));
    expect(b.ctx!.knownEpoch('a'), 1);
  });

  test('the names and the messages carry the epoch: the reader sent in '
      'epoch 1 is _r(a, 1, 1), and a message sent after a crash restart '
      'carries epoch 2', () async {
    final storeA = MemoryAgentStateStore();
    final kpA = generateKeyPair();
    final a1 = host('a', playA, storeA, kpA);
    await a1.initialize();
    final coldCall = queue.single;
    expect(PayloadCodec.senderEpochOf(coldCall.payload), 1);
    final (:name, :value, :serial, :senderEpoch) =
        PayloadCodec.deserializeValuePayload(coldCall.payload);
    expect('$name', '_w(b, 0, 0)', reason: 'the serializer name has epoch 0');
    expect(serial, isNotNull);
    expect(senderEpoch, 1);
    // The reader, and the serializer tail of the cold call, _w(b, 0, 0).
    final names = extractGlobalNames(value);
    expect(names.map((n) => '$n'), ['_r(a, 1, 1)', '_w(b, 0, 0)']);
    expect(names.map((n) => n.epoch), [1, 0]);

    await crash(a1);
    final a2 = host('a', playA, storeA, kpA);
    await a2.initialize();
    queue.clear();
    await a2.injectUserInput(ConstTerm('ping'));
    final ping = queue.single;
    expect(PayloadCodec.senderEpochOf(ping.payload), 2);
    expect(PayloadCodec.deserializeValuePayload(ping.payload).serial,
        isNotNull, reason: 'a cold call carries a serial');
  });

  test('the peer learns the epoch from the first message of the new one and '
      'retires its ends at the restarted agent; widowed_reader(X?) then succeeds on '
      'its waiting reader', () async {
    final storeA = MemoryAgentStateStore();
    final kpA = generateKeyPair();
    final a1 = host('a', playA, storeA, kpA);
    final b = host('b', playB, MemoryAgentStateStore(), generateKeyPair());
    await a1.initialize();
    await b.initialize();
    await deliverTo('b', b);
    expect(b.ctx!.knownEpoch('a'), 1);
    expect(b.ctx!.wp.findByRemote(rA1), isNotNull,
        reason: 'b holds the end of the link, the entry of _r(a, 1, 1)');
    expect(out['b'], isNot(contains('< widowed')),
        reason: 'the reader\'s writer occurs in the link end: show/1 waits');

    await crash(a1);
    final a2 = host('a', playA, storeA, kpA);
    await a2.initialize();
    expect(a2.ctx!.epoch, 2);
    expect(out['b'], isNot(contains('< widowed')),
        reason: 'b has heard nothing yet');

    // a's first message in epoch 2 reaches b.
    await a2.injectUserInput(ConstTerm('ping'));
    await deliverTo('b', b);
    expect(b.ctx!.knownEpoch('a'), 2);
    expect(b.ctx!.wp.findByRemote(rA1), isNull,
        reason: 'the end at a, recorded in epoch 1, is retired');
    expect(b.ctx!.hasRetired(rA1), isTrue);
    expect(out['b'], contains('< widowed'),
        reason: 'the writer paired to the waiting reader occurs nowhere');
    expect(out['b'], isNot(contains('< got(hello)')));
  });

  test('a value for an end the receiver lost is answered gone(G), which '
      'retires the sender\'s end: its pending value is dropped', () async {
    final storeB = MemoryAgentStateStore();
    final kpB = generateKeyPair();
    final a = host('a', playA, MemoryAgentStateStore(), generateKeyPair());
    final b1 = host('b', playB, storeB, kpB);
    await a.initialize();
    await b1.initialize();
    await deliverTo('b', b1);
    expect(b1.ctx!.wp.findByRemote(rA1), isNotNull);

    // b crashes and restarts: its end of the link is retired.
    await crash(b1);
    final b2 = host('b', playB, storeB, kpB);
    await b2.initialize();
    expect(b2.ctx!.epoch, 2);
    expect(b2.ctx!.wp.findByRemote(rA1), isNull);
    expect(b2.ctx!.hasRetired(rA1), isTrue);

    // a, which has heard nothing of it, assigns the writer: the value leaves
    // a and is pending there.
    await a.injectUserInput(ConstTerm('go'));
    expect(a.ctx!.hasPendingReaderValue('a', 1), isTrue);
    expect(queue.map((m) => '${m.from}>${m.to}'), ['a>b']);

    // b answers gone(_r(a, 1, 1)), and applies nothing.
    await deliverTo('b', b2);
    expect(out['b'], isNot(contains('< got(hello)')));
    final gone = queue.where((m) => m.from == 'b' && m.to == 'a').toList();
    expect(gone, hasLength(1));
    final link = PayloadCodec.decodeLinkPayload(gone.single.payload);
    expect(link.kind, wireMsgKindGone);
    expect(link.name, rA1);
    expect(link.senderEpoch, 2);

    // The gone retires a's end: the pending value goes, and a has learnt
    // b's epoch.
    await deliverTo('a', a);
    expect(a.ctx!.hasPendingReaderValue('a', 1), isFalse);
    expect(a.ctx!.hasRetired(rA1), isTrue);
    expect(a.ctx!.knownEpoch('b'), 2);
    expect(queue, isEmpty, reason: 'a gone is answered with nothing');
  });

  test('a serializer message is a repeat only by its (sender, epoch, '
      'serial): the same serial of another epoch of the sender is a new '
      'message', () async {
    final a = host('a', playA, MemoryAgentStateStore(), generateKeyPair());
    final b = host('b', playB, MemoryAgentStateStore(), generateKeyPair());
    await a.initialize();
    await b.initialize();
    await a.injectUserInput(ConstTerm('ping'));
    final ping = queue.last;
    expect(PayloadCodec.deserializeValuePayload(ping.payload).serial,
        isNotNull);
    queue.clear();
    final serial = PayloadCodec.deserializeValuePayload(ping.payload).serial!;

    // The ping of epoch 1 is applied once.
    await b.onMadMessageReceived('a', ping.payload, messageId: 'p1');
    expect(b.ctx!.wp.serializerAppendedCount, 1);
    expect(b.ctx!.wp.serializerHasAppended('a', 1, serial), isTrue);
    await b.onMadMessageReceived('a', ping.payload, messageId: 'p1-again');
    expect(b.ctx!.wp.serializerAppendedCount, 1, reason: 'a repeat');

    // The same message under epoch 2 of a: another triple, applied.
    final inEpoch2 = PayloadCodec.withSenderEpoch(ping.payload, 2);
    await b.onMadMessageReceived('a', inEpoch2, messageId: 'p2');
    expect(b.ctx!.wp.serializerAppendedCount, 2);
    expect(b.ctx!.wp.serializerHasAppended('a', 2, serial), isTrue);
    expect(b.ctx!.knownEpoch('a'), 2);

    // The message of epoch 1 arriving again, after epoch 2 is known, is
    // still the repeat it is, by its triple.
    await b.onMadMessageReceived('a', ping.payload, messageId: 'p1-late');
    expect(b.ctx!.wp.serializerAppendedCount, 2);
    expect(b.ctx!.wp.serializerHasAppended('a', 3, serial), isFalse);
  });
}
