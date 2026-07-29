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

typedef _Msg = ({String from, String to, List<int> payload});

class _Net {
  final Map<String, MadContext> agents = {};
  final Map<String, GlpRuntime> runtimes = {};
  final Map<String, int> netIn = {};
  final List<_Msg> queue = [];
  final List<_Msg> deliveryLog = [];

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
      }
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
({GlobalName rn, int restW}) _forwardedReader(_Net net) {
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
int _forwardedWriter(_Net net) {
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
}
