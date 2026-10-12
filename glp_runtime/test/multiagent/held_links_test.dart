/// Held links: a link end whose other end is at an agent this runtime did not
/// exchange the name with (madGLP).
///
/// Normative source: the IGLP paper — Definition Held Link and its four cases,
/// Definition authorise_link Predicate, Remark Authorised Runs, Definition
/// madGLP Send ("enabled when (m, q) ∈ M_p is unsent and not held"), madGLP
/// Receive request and writer cases, and "Held links" under Networking Seam in
/// the Implementation Notes appendix.
///
/// The test is structural and local: the anchor named in the global name, or
/// the agent an entry was exported to, against the agent that sent the traffic.
/// Nothing outside the agent is consulted — no reachability, no trust state.

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell, CellTag;

typedef _Msg = ({String from, String to, List<int> payload});

class _Net {
  final Map<String, MadContext> agents = {};
  final Map<String, GlpRuntime> runtimes = {};
  final Map<String, HeapCell> netIn = {};
  final List<_Msg> queue = [];
  final List<_Msg> deliveryLog = [];

  /// The messages the network delivers twice, as fair delivery allows:
  /// "every message placed in a communication channel is eventually delivered
  /// to its recipient, possibly more than once" (Remark Fair Message
  /// Delivery).  None by default.
  bool Function(_Msg m)? twice;

  /// Add an agent, booting its permanent index-0 serializer entry — every
  /// agent has one (Definition Index-0 Serializer), and it is where both
  /// cold-calls and `pending_link` reports are delivered.
  MadContext add(String id) {
    final rt = GlpRuntime();
    final ctx = MadContext(agentId: id, runtime: rt);
    ctx.onMessageReady = (dest, msg) =>
        queue.add((from: id, to: dest, payload: msg.payload));
    agents[id] = ctx;
    runtimes[id] = rt;
    final (w, _) = rt.heap.allocateVariable();
    ctx.wp.initializeSerializerEntry(w);
    netIn[id] = w;
    return ctx;
  }

  void flushAll() {
    for (final ctx in agents.values) {
      ctx.flushMessages();
    }
  }

  /// Flush and deliver until quiescent. Held messages are not eligible for
  /// Send, so this terminates with them still in M_p.
  void settle() {
    while (true) {
      flushAll();
      if (queue.isEmpty) break;
      while (queue.isNotEmpty) {
        final m = queue.removeAt(0);
        deliveryLog.add(m);
        agents[m.to]!.handleIncomingPayload(payload: m.payload, fromAgent: m.from);
        if (twice?.call(m) ?? false) {
          deliveryLog.add(m);
          agents[m.to]!
              .handleIncomingPayload(payload: m.payload, fromAgent: m.from);
        }
      }
    }
  }

  /// Deliver, in order, the queued messages [pick] selects, and leave the rest
  /// queued: delivery is unordered (Networking Seam, "The contract").
  void deliverWhere(bool Function(_Msg m) pick) {
    flushAll();
    final chosen = queue.where(pick).toList();
    queue.removeWhere(pick);
    for (final m in chosen) {
      deliveryLog.add(m);
      agents[m.to]!.handleIncomingPayload(payload: m.payload, fromAgent: m.from);
    }
  }

  int answerAll(bool authorise) {
    var n = 0;
    for (final ctx in agents.values) {
      for (final (gn, _) in ctx.reportedLinks.toList()) {
        ctx.answerLink(gn, authorise);
        n++;
      }
    }
    return n;
  }

  /// Settle, authorising every reported link, until nothing is left held.
  void settleAuthorising() {
    for (var i = 0; i < 20; i++) {
      settle();
      if (reportedCount == 0) return;
      answerAll(true);
    }
    throw StateError('links kept being reported after 20 rounds');
  }

  int get reportedCount =>
      agents.values.fold(0, (s, c) => s + c.reportedLinkCount);
  int get heldCount => agents.values.fold(0, (s, c) => s + c.mp.heldCount);
}

/// The `pending_link(G, S)` reports on [id]'s network input stream, in order,
/// as the program sees them: `G` in its ground presentation, `S` the sender.
List<(Term, String)> _reports(_Net net, String id) {
  final rt = net.runtimes[id]!;
  final out = <(Term, String)>[];
  Object? cell = rt.heap.derefAddr(net.netIn[id]!);
  while (cell is StructTerm && cell.functor == '.') {
    final head = cell.args[0];
    if (head is StructTerm &&
        head.functor == 'pending_link' &&
        head.args.length == 2) {
      out.add((head.args[0], (head.args[1] as ConstTerm).value as String));
    }
    final tail = cell.args[1];
    if (tail is! VarRef) break;
    cell = rt.heap.derefAddr(tail.addr);
  }
  return out;
}

/// Assert [term] is the ground presentation of the named link: the 2-ary
/// structure `'_r'(P, I)` or `'_w'(P, I)`, functor giving polarity, first
/// argument the anchor.
void _expectGroundName(Term term, String anchor, int index, bool isWriter) {
  expect(term, isA<StructTerm>());
  final s = term as StructTerm;
  expect(s.functor, isWriter ? '_w' : '_r');
  expect(s.args.length, 2);
  expect((s.args[0] as ConstTerm).value, anchor);
  expect((s.args[1] as ConstTerm).value, index);
}

/// Build the three-agent forwarded-reader setup: charlie anchors a reader and
/// exports it to bob, who forwards it to alice in a cold-call. Returns the
/// anchored reader name and charlie's writer.
({GlobalName rn, HeapCell restW}) _forwardedReader(_Net net) {
  final ctxBob = net.agents['bob']!;
  final ctxCharlie = net.agents['charlie']!;
  final bobRt = net.runtimes['bob']!;
  final charlieRt = net.runtimes['charlie']!;

  final (restW, restR) = charlieRt.heap.allocateVariable();
  final gres = globalize(
    variables: [TermVar.reader(restR, writerAddr: restW)],
    localAgent: 'charlie',
    remoteAgent: 'bob',
    table: ctxCharlie.wp,
    records: ctxCharlie.up,
  );
  ctxCharlie.registerGlobalSendSpawns(gres.spawns);
  final lres = localize(
    globalNames: gres.globalNames,
    localAgent: 'bob',
    fromAgent: 'charlie',
    table: ctxBob.wp,
    records: ctxBob.up,
    freshAddrAllocator: () => bobRt.heap.allocateVariable(),
  );
  ctxBob.send(
      StructTerm('m', [VarRef(lres.freshPairs[0].readerAddr)]),
      true, 'alice', 0, 'alice');
  return (rn: gres.globalNames[0], restW: restW);
}

/// Build the forwarded-writer setup: p anchors a writer and exports it to q,
/// who forwards it to r. Returns r's imported writer address.
HeapCell _forwardedWriter(_Net net) {
  final pRt = net.runtimes['p']!;
  final qRt = net.runtimes['q']!;
  final rRt = net.runtimes['r']!;
  final (xW, _) = pRt.heap.allocateVariable();
  net.agents['p']!.send(StructTerm('m', [VarRef(xW)]), true, 'q', 0, 'q');
  net.settle();
  final yq = ((net.runtimes['q']!.heap.derefAddr(net.netIn['q']!) as StructTerm)
              .args[0] as StructTerm)
          .args[0] as VarRef;
  net.agents['q']!.send(StructTerm('n', [VarRef(yq.addr)]), true, 'r', 0, 'r');
  net.settle();
  final zr = ((rRt.heap.derefAddr(net.netIn['r']!) as StructTerm).args[0]
          as StructTerm)
      .args[0] as VarRef;
  qRt.hashCode; // keep qRt referenced for clarity of the three-agent shape
  return zr.addr;
}

/// The end the first element of [id]'s network input stream carried: the
/// argument of the cold call `m(...)`.
VarRef _carried(_Net net, String id) =>
    ((net.runtimes[id]!.heap.derefAddr(net.netIn[id]!) as StructTerm).args[0]
            as StructTerm)
        .args[0] as VarRef;

/// Build the reader forwarded twice: charlie anchors a reader and exports it
/// to bob, who forwards it to alice, who forwards it to dave, each in a cold
/// call.  Alice and dave each localize the forwarded name and request it of
/// charlie (Definition Localize, case 2); each holder's program authorises its
/// own request (Held Links, case 1), so both requests are on their way to
/// charlie, neither yet delivered.  Dave is the current holder, alice a past
/// one.  Returns the reader name and charlie's writer.
({GlobalName rn, HeapCell restW}) _forwardedTwice(_Net net) {
  final res = _forwardedReader(net);
  net.deliverWhere((m) => m.to == 'alice');
  net.agents['alice']!.send(
      StructTerm('m', [_carried(net, 'alice')]), true, 'dave', 0, 'dave');
  net.deliverWhere((m) => m.to == 'dave');
  expect(net.agents['alice']!.wp.findByRemote('charlie', res.rn.index), isNull,
      reason: 'alice forwarded the name under its original anchor');
  expect(net.agents['dave']!.hasReportedLink('charlie', res.rn.index, false),
      isTrue);
  net.agents['alice']!.answerLink(res.rn, true);
  net.agents['dave']!.answerLink(res.rn, true);
  net.flushAll();
  expect(
      net.queue
          .where((m) => m.to == 'charlie' && m.payload[0] == 1 /* request */)
          .map((m) => m.from)
          .toSet(),
      {'alice', 'dave'});
  return res;
}

/// After [_forwardedTwice] and the run: the value reached dave, the current
/// holder, who acknowledged it, closing the link; alice and bob, past holders,
/// discarded what reached them, holding nothing early.
void _expectDaveHasIt(_Net net, GlobalName rn, String why) {
  expect(net.runtimes['dave']!.heap.derefAddr(_carried(net, 'dave').addr),
      isA<ConstTerm>().having((c) => c.value, 'value', 'v'),
      reason: 'the current holder receives the value ($why)');
  expect(net.agents['dave']!.wp.findByRemote('charlie', rn.index), isNull);
  expect(net.agents['charlie']!.pendingReaderValueCount, 0,
      reason: 'the current holder acknowledged: the link is closed ($why)');
  expect(net.agents['charlie']!.recordedRequestPairCount, 0,
      reason: "a closed link's pairs are released ($why)");
  for (final past in ['alice', 'bob']) {
    expect(net.agents[past]!.earlyAssignmentCount, 0,
        reason: 'a past holder discards the value ($why)');
  }
  expect(net.reportedCount, 0);
  expect(net.heldCount, 0);
}

void main() {
  group('Held links (madGLP)', () {
    test('(1) at the holder of a forwarded reader, the request is held', () {
      final net = _Net();
      net.add('alice');
      net.add('bob');
      net.add('charlie');
      final (rn: rn, restW: _) = _forwardedReader(net);
      net.settle();

      // The request exists but did not go out, and nothing reached the anchor.
      expect(net.agents['alice']!.mp.heldCount, 1);
      expect(net.deliveryLog.where((m) => m.to == 'charlie'), isEmpty,
          reason: 'a held request is not eligible for Send');
      expect(net.agents['charlie']!.reportedLinkCount, 0);

      // Exactly one report, naming the anchor and the agent that passed the
      // end — bob, not charlie.
      final reports = _reports(net, 'alice');
      expect(reports.length, 1);
      _expectGroundName(reports.single.$1, 'charlie', rn.index, false);
      expect(reports.single.$2, 'bob',
          reason: 'the sender is who passed the end, not the anchor');
    });

    test('(2) at the holder of a forwarded writer, the value is held', () {
      final net = _Net();
      net.add('p');
      net.add('q');
      net.add('r');
      final zr = _forwardedWriter(net);
      final mark = net.deliveryLog.length;

      net.runtimes['r']!.heap.bindVariable(zr, ConstTerm(42));
      net.settle();

      expect(net.deliveryLog.length, mark, reason: 'nothing went out');
      expect(net.agents['r']!.mp.heldCount, 1);
      final reports = _reports(net, 'r');
      expect(reports.length, 1);
      _expectGroundName(reports.single.$1, 'p', 1, true);
      expect(reports.single.$2, 'q',
          reason: 'the writer came from q, though it is anchored at p');
    });

    test('(3) at the anchor, every incoming request is held', () {
      final net = _Net();
      net.add('alice');
      net.add('bob');
      net.add('charlie');
      final (rn: rn, restW: restW) = _forwardedReader(net);
      net.settle();
      net.answerAll(true); // alice releases her request
      net.settle();

      // Charlie holds it: no holder recorded, nothing sent back.
      expect(net.agents['charlie']!.reportedLinkCount, 1);
      expect(net.agents['charlie']!.heldRequestCount, 1);
      expect(
          net.agents['charlie']!.globalSendRegistry
              .getGoalFor(restW)!
              .destination,
          'bob',
          reason: 'the holder is not recorded until the request is authorised');

      final reports = _reports(net, 'charlie');
      expect(reports.length, 1);
      _expectGroundName(reports.single.$1, 'charlie', rn.index, false);
      expect(reports.single.$2, 'alice',
          reason: 'the requester, who is not the agent charlie exported to');
    });

    test('(4) at the anchor, an assignment from an unexpected sender is held',
        () {
      final net = _Net();
      net.add('p');
      net.add('q');
      net.add('r');
      final zr = _forwardedWriter(net);

      net.runtimes['r']!.heap.bindVariable(zr, ConstTerm(42));
      net.settle();
      net.answerAll(true); // r releases the value; it reaches p
      net.settle();

      // p exported the name to q, so an assignment from r is held.
      expect(net.agents['p']!.heldAssignmentCount, 1);
      expect(net.agents['p']!.wp.lookupByIndex(1), isNotNull,
          reason: 'the entry survives until the assignment is applied');
      final reports = _reports(net, 'p');
      expect(reports.length, 1);
      _expectGroundName(reports.single.$1, 'p', 1, true);
      expect(reports.single.$2, 'r',
          reason: 'the agent that actually sent it, not the one p exported to');
    });

    // §Held Links: "a request or assignment arriving for a link already held
    // is dropped, so a held link is reported once."
    test('(3) a request delivered twice is held and reported once', () {
      final net = _Net();
      net.add('alice');
      net.add('bob');
      net.add('charlie');
      net.twice = (m) => m.to == 'charlie';
      final (rn: rn, restW: restW) = _forwardedReader(net);
      net.settle();
      net.answerAll(true); // alice releases her request
      net.settle();

      final requests = net.deliveryLog.where((m) =>
          m.to == 'charlie' && m.payload[0] == 1 /* request */);
      expect(requests.length, 2, reason: 'the request arrived twice');
      expect(net.agents['charlie']!.heldRequestCount, 1);
      expect(net.agents['charlie']!.reportedLinkCount, 1);
      final reports = _reports(net, 'charlie');
      expect(reports.length, 1);
      _expectGroundName(reports.single.$1, 'charlie', rn.index, false);
      expect(reports.single.$2, 'alice');

      // Authorised once, the link completes at the requester.
      net.answerAll(true);
      net.runtimes['charlie']!.heap.bindVariable(restW, ConstTerm('done'));
      net.agents['charlie']!.onWriterBound(restW, ConstTerm('done'));
      net.settleAuthorising();
      expect(net.agents['charlie']!.pendingReaderValueCount, 0);
      expect(_reports(net, 'charlie').length, 1);
    });

    test('(4) an assignment delivered twice is held and reported once', () {
      final net = _Net();
      net.add('p');
      net.add('q');
      net.add('r');
      final zr = _forwardedWriter(net);
      net.twice = (m) => m.to == 'p';
      final pXr = net.agents['p']!.wp.lookupByIndex(1)!.writerAddr;

      net.runtimes['r']!.heap.bindVariable(zr, ConstTerm(42));
      net.settle();
      net.answerAll(true); // r releases the value; it reaches p twice
      net.settle();

      expect(
          net.deliveryLog.where((m) => m.from == 'r' && m.to == 'p').length, 2,
          reason: 'the assignment arrived twice');
      expect(net.agents['p']!.heldAssignmentCount, 1);
      expect(net.agents['p']!.reportedLinkCount, 1);
      final reports = _reports(net, 'p');
      expect(reports.length, 1);
      _expectGroundName(reports.single.$1, 'p', 1, true);
      expect(reports.single.$2, 'r');

      // Authorised once, it is applied once.
      net.answerAll(true);
      net.settle();
      expect((net.runtimes['p']!.heap.derefAddr(pXr) as ConstTerm).value, 42);
      expect(net.agents['p']!.heldAssignmentCount, 0);
      expect(net.agents['p']!.wp.lookupByIndex(1), isNull);
      expect(_reports(net, 'p').length, 1);
    });

    test('authorise releases exactly what was held; the link then completes',
        () {
      final net = _Net();
      net.add('p');
      net.add('q');
      net.add('r');
      final pRt = net.runtimes['p']!;
      final (xW, xR) = pRt.heap.allocateVariable();
      net.agents['p']!.send(StructTerm('m', [VarRef(xW)]), true, 'q', 0, 'q');
      net.settle();
      final yq = ((net.runtimes['q']!.heap.derefAddr(net.netIn['q']!)
                  as StructTerm)
              .args[0] as StructTerm)
          .args[0] as VarRef;
      net.agents['q']!
          .send(StructTerm('n', [VarRef(yq.addr)]), true, 'r', 0, 'r');
      net.settle();
      final zr = ((net.runtimes['r']!.heap.derefAddr(net.netIn['r']!)
                  as StructTerm)
              .args[0] as StructTerm)
          .args[0] as VarRef;

      net.runtimes['r']!.heap.bindVariable(zr.addr, ConstTerm('through'));
      net.settleAuthorising();

      // The value arrived exactly once and both ends are clean.
      expect((pRt.heap.derefAddr(xR) as ConstTerm).value, 'through');
      expect(net.heldCount, 0);
      expect(net.reportedCount, 0);
      expect(net.agents['r']!.up.count, 0,
          reason: 'the record is released when the value is finally sent');
      expect(net.agents['p']!.wp.lookupByIndex(1), isNull);
      expect(net.agents['q']!.up.count, 0);
      expect(net.agents['q']!.globalSendRegistry.pendingCount, 0);
    });

    test('a forwarded link is held at both ends: two reports, two answers, '
        'one value', () {
      final net = _Net();
      net.add('p');
      net.add('q');
      net.add('r');
      final pRt = net.runtimes['p']!;
      final (xW, xR) = pRt.heap.allocateVariable();
      net.agents['p']!.send(StructTerm('m', [VarRef(xW)]), true, 'q', 0, 'q');
      net.settle();
      final yq = ((net.runtimes['q']!.heap.derefAddr(net.netIn['q']!)
                  as StructTerm)
              .args[0] as StructTerm)
          .args[0] as VarRef;
      net.agents['q']!
          .send(StructTerm('n', [VarRef(yq.addr)]), true, 'r', 0, 'r');
      net.settle();
      final zr = ((net.runtimes['r']!.heap.derefAddr(net.netIn['r']!)
                  as StructTerm)
              .args[0] as StructTerm)
          .args[0] as VarRef;

      final mark = net.deliveryLog.length;
      net.runtimes['r']!.heap.bindVariable(zr.addr, ConstTerm('once'));

      // First hold: at r, the holder of the forwarded writer.
      net.settle();
      expect(_reports(net, 'r').length, 1);
      expect(_reports(net, 'p'), isEmpty);
      net.answerAll(true);

      // Second hold: at p, the anchor, once the value reaches it.
      net.settle();
      expect(_reports(net, 'p').length, 1);
      net.answerAll(true);
      net.settle();

      expect((pRt.heap.derefAddr(xR) as ConstTerm).value, 'once');
      final values = net.deliveryLog
          .sublist(mark)
          .where((m) => m.from == 'r' && m.to == 'p');
      expect(values.length, 1, reason: 'two holds, still one value on the wire');
    });

    test('refuse drops the link, in every one of the four cases', () {
      // Case 1 — the holder refuses its own request.
      var net = _Net();
      net.add('alice');
      net.add('bob');
      net.add('charlie');
      var res = _forwardedReader(net);
      net.settle();
      net.answerAll(false);
      net.settle();
      expect(net.agents['alice']!.mp.heldCount, 0);
      expect(net.deliveryLog.where((m) => m.to == 'charlie'), isEmpty,
          reason: 'a refused request never goes out');
      expect(net.agents['alice']!.wp.findByRemote('charlie', res.rn.index),
          isNull,
          reason: "the refused link's entry is released");

      // Case 3 — the anchor refuses an incoming request.
      net = _Net();
      net.add('alice');
      net.add('bob');
      net.add('charlie');
      res = _forwardedReader(net);
      net.settle();
      net.answerAll(true); // alice authorises, charlie now holds
      net.settle();
      expect(net.agents['charlie']!.reportedLinkCount, 1);
      net.agents['charlie']!.answerLink(res.rn, false);
      expect(net.agents['charlie']!.heldRequestCount, 0);
      expect(net.agents['charlie']!.globalSendRegistry.hasGoalForLink(res.rn),
          isFalse, reason: "the refused link's global_send goal is released");

      // Case 2 — the holder of a forwarded writer refuses its value.
      net = _Net();
      net.add('p');
      net.add('q');
      net.add('r');
      var zr = _forwardedWriter(net);
      var mark = net.deliveryLog.length;
      net.runtimes['r']!.heap.bindVariable(zr, ConstTerm(1));
      net.settle();
      net.answerAll(false);
      net.settle();
      expect(net.deliveryLog.length, mark, reason: 'no value goes out');
      expect(net.agents['r']!.mp.heldCount, 0);
      expect(net.agents['r']!.up.count, 0, reason: 'the record is released');
      expect(net.agents['r']!.globalSendRegistry.pendingCount, 0,
          reason: 'the goal is released');

      // Case 4 — the anchor refuses an assignment from an unexpected sender.
      net = _Net();
      net.add('p');
      net.add('q');
      net.add('r');
      zr = _forwardedWriter(net);
      final pXr = net.agents['p']!.wp.lookupByIndex(1)!.writerAddr;
      net.runtimes['r']!.heap.bindVariable(zr, ConstTerm(7));
      net.settle();
      net.answerAll(true); // r authorises; p now holds
      net.settle();
      expect(net.agents['p']!.heldAssignmentCount, 1);
      net.agents['p']!.answerLink(GlobalName.writer('p', 1), false);
      expect(net.agents['p']!.heldAssignmentCount, 0);
      expect(net.agents['p']!.wp.lookupByIndex(1), isNull,
          reason: "the refused link's entry is released");
      expect(net.runtimes['p']!.heap.derefAddr(pXr), isNot(isA<ConstTerm>()),
          reason: 'a refused assignment is discarded, never applied');
    });

    test('unheld traffic is unaffected: the direct path reports nothing', () {
      final net = _Net();
      net.add('p');
      net.add('q');
      final pRt = net.runtimes['p']!;

      // p exports a writer to q and a reader to q — no forwarding anywhere.
      final (xW, xR) = pRt.heap.allocateVariable();
      net.agents['p']!
          .send(StructTerm('m', [VarRef(xW), VarRef(xR)]), true, 'q', 0, 'q');
      net.settle();
      final cell = (net.runtimes['q']!.heap.derefAddr(net.netIn['q']!)
              as StructTerm)
          .args[0] as StructTerm;
      net.runtimes['q']!.heap
          .bindVariable((cell.args[0] as VarRef).addr, ConstTerm('direct'));
      net.settle();

      expect(net.reportedCount, 0, reason: 'nothing was forwarded');
      expect(net.heldCount, 0);
      expect(_reports(net, 'p'), isEmpty);
      expect(_reports(net, 'q'), isEmpty);
      expect(
          (net.runtimes['q']!.heap.derefAddr((cell.args[1] as VarRef).addr)
                  as ConstTerm)
              .value,
          'direct');
    });
  });

  // IGLP 6998d52, §app:requests-acks: "On receiving req(_r(p, i)) from
  // requester s, the anchor p records s among the link's holders; if the value
  // is pending, p addresses it to s as well. Delivery is unordered, so when a
  // reader is forwarded twice the two holders' requests reach p in either
  // order and p cannot tell the current holder from the past one: it sends the
  // value to each, a past holder discards it (below), and the current holder
  // acknowledges. A request repeating a (link, requester) pair already
  // recorded --- the link held, authorised or closed --- is dropped."  §Held
  // Links: "A request from a requester already recorded for the link ... [is]
  // dropped, so each party is reported once; a request from a further
  // requester --- the reader forwarded again --- is held and reported in its
  // turn, the anchor's program asked once for each party that comes to hold
  // the end."
  group('Requests from several holders (madGLP)', () {
    test(
        "two holders' requests in both arrival orders, the value produced "
        'before, between and after them: neither order loses the value', () {
      for (final first in ['alice', 'dave']) {
        for (final produce in ['before', 'between', 'after']) {
          final why = '$first first, value produced $produce';
          final second = first == 'alice' ? 'dave' : 'alice';
          final net = _Net();
          for (final id in ['alice', 'bob', 'charlie', 'dave']) {
            net.add(id);
          }
          final (rn: rn, restW: restW) = _forwardedTwice(net);
          final charlie = net.agents['charlie']!;
          void value() =>
              net.runtimes['charlie']!.heap.bindVariable(restW, ConstTerm('v'));
          void request(String who) =>
              net.deliverWhere((m) => m.from == who && m.to == 'charlie');

          if (produce == 'before') value();
          request(first);
          expect(charlie.reportedLinkCount, 1, reason: why);
          charlie.answerLink(rn, true);
          if (produce == 'between') value();
          request(second);
          expect(charlie.reportedLinkCount, 1, reason: why);
          charlie.answerLink(rn, true);
          if (produce == 'after') value();
          net.settle();

          _expectDaveHasIt(net, rn, why);
          final reports = _reports(net, 'charlie');
          expect([for (final r in reports) r.$2], [first, second],
              reason: 'each party reported once ($why)');
          for (final r in reports) {
            _expectGroundName(r.$1, 'charlie', rn.index, false);
          }
        }
      }
    });

    test('a request repeated while held, once authorised, and once the link '
        'is closed, is dropped', () {
      final net = _Net();
      net.add('alice');
      net.add('bob');
      net.add('charlie');
      final (rn: rn, restW: restW) = _forwardedReader(net);
      net.settle();
      net.answerAll(true); // alice releases her request
      net.settle();
      final charlie = net.agents['charlie']!;
      final req = net.deliveryLog.singleWhere(
          (m) => m.to == 'charlie' && m.payload[0] == 1 /* request */);
      void repeat() => charlie.handleIncomingPayload(
          payload: req.payload, fromAgent: req.from);

      // Held: reported once, held once.
      expect(charlie.heldRequestCount, 1);
      repeat();
      expect(charlie.heldRequestCount, 1, reason: 'held: the repeat dropped');
      expect(charlie.reportedLinkCount, 1);
      expect(_reports(net, 'charlie').length, 1);

      // Authorised: the requester is recorded; the repeat is not held or
      // reported again, and nothing is sent.
      charlie.answerLink(rn, true);
      expect(charlie.heldRequestCount, 0);
      repeat();
      expect(charlie.heldRequestCount, 0,
          reason: 'authorised: the repeat dropped, not held again');
      expect(charlie.reportedLinkCount, 0);
      expect(_reports(net, 'charlie').length, 1,
          reason: 'authorised: not reported again');
      expect(charlie.mp.totalLength, 0, reason: 'authorised: nothing sent');
      expect(charlie.globalSendRegistry.getGoalFor(restW)!.requesters,
          ['alice']);

      // Closed: the value reaches alice, who acknowledges it.
      net.runtimes['charlie']!.heap.bindVariable(restW, ConstTerm('v'));
      net.settle();
      expect(
          (net.runtimes['alice']!.heap
                  .derefAddr(_carried(net, 'alice').addr) as ConstTerm)
              .value,
          'v');
      expect(charlie.pendingReaderValueCount, 0);
      final mark = net.deliveryLog.length;
      repeat();
      net.settle();
      expect(charlie.heldRequestCount, 0, reason: 'closed: the repeat dropped');
      expect(charlie.reportedLinkCount, 0);
      expect(_reports(net, 'charlie').length, 1);
      expect(net.deliveryLog.length, mark, reason: 'closed: nothing sent');
      expect(charlie.recordedRequestPairCount, 0);
    });

    test("a reader forwarded twice, its two holders' requests overlapping: "
        'the second held and reported in its turn, the value to each', () {
      for (final first in ['alice', 'dave']) {
        for (final produce in ['before', 'after']) {
          final why = '$first first, value produced $produce';
          final second = first == 'alice' ? 'dave' : 'alice';
          final net = _Net();
          for (final id in ['alice', 'bob', 'charlie', 'dave']) {
            net.add(id);
          }
          final (rn: rn, restW: restW) = _forwardedTwice(net);
          final charlie = net.agents['charlie']!;
          void value() =>
              net.runtimes['charlie']!.heap.bindVariable(restW, ConstTerm('v'));

          if (produce == 'before') value();
          // Both requests reach charlie before his program answers either.
          net.deliverWhere((m) => m.from == first && m.to == 'charlie');
          net.deliverWhere((m) => m.from == second && m.to == 'charlie');
          expect(charlie.heldRequestCount, 2, reason: why);
          expect(charlie.reportedLinkCount, 1, reason: why);
          expect([for (final r in _reports(net, 'charlie')) r.$2], [first],
              reason: 'the second is held, not yet reported ($why)');

          // The first answered, the second is reported in its turn.
          charlie.answerLink(rn, true);
          expect(charlie.heldRequestCount, 1, reason: why);
          expect([for (final r in _reports(net, 'charlie')) r.$2],
              [first, second],
              reason: 'reported in its turn ($why)');
          expect(
              charlie.reportedLinks.single.$2, second, reason: why);
          charlie.answerLink(rn, true);
          expect(charlie.heldRequestCount, 0, reason: why);

          if (produce == 'after') value();
          net.settle();
          _expectDaveHasIt(net, rn, why);
          expect(_reports(net, 'charlie').length, 2,
              reason: 'the program asked once per party ($why)');
        }
      }
    });
  });
}
