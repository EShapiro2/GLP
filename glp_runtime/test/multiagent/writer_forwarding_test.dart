/// Writer forwarding: a re-exported imported writer travels under its
/// original name (madGLP).
///
/// Normative source: the IGLP paper — Definition Globalize case 4 (an imported
/// writer is emitted as `_w(o, k)`, its record and `global_send` goal removed,
/// no entry created), Definition Localize case 1 (the record is created
/// alongside the spawned goal), Definition Imported-Writer Records, madGLP
/// Receive Writer case ("the sender need not be q, the name having been
/// forwarded"), and Lemma Globalize-Localize part 5 ("the value q produces
/// reaches o's entry in one message, without request or acknowledgement").
///
/// A writer's link, like a reader's, has one anchor for life. Re-exporting the
/// writer moves the sending end; every intermediary leaves the link, keeping no
/// record and no goal, and relays nothing.

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/wire/codec.dart'
    show wireMsgKindValue, wireMsgKindRequest, wireMsgKindAcknowledgement;

/// One delivered or queued message in the harness network.
typedef _Msg = ({String from, String to, List<int> payload});

/// A test network of MadContexts exchanging opaque payload bytes.
class _Net {
  final Map<String, MadContext> agents = {};
  final Map<String, GlpRuntime> runtimes = {};
  final List<_Msg> queue = [];

  /// Every delivery performed, in order — for asserting who talked to whom.
  final List<_Msg> deliveryLog = [];

  MadContext add(String id) {
    final rt = GlpRuntime();
    final ctx = MadContext(agentId: id, runtime: rt);
    ctx.onMessageReady = (dest, msg) =>
        queue.add((from: id, to: dest, payload: msg.payload));
    agents[id] = ctx;
    runtimes[id] = rt;
    return ctx;
  }

  /// Create the permanent index-0 serializer entry; returns the network-input
  /// writer address (the root of the agent's network input stream).
  int bootSerializer(String id) {
    final (netIn, _) = runtimes[id]!.heap.allocateVariable();
    agents[id]!.wp.initializeSerializerEntry(netIn);
    return netIn;
  }

  void flushAll() {
    for (final ctx in agents.values) {
      ctx.flushMessages();
    }
  }

  /// Flush and deliver until the network is quiescent. Held messages are not
  /// eligible for Send, so they stay in M_p and settling terminates with them
  /// still there.
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

  /// Answer every link reported and not yet answered, at every agent, as the
  /// program would through `authorise_link/2`. Returns the number answered.
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

  /// Total links reported and awaiting an answer, across all agents.
  int get reportedCount =>
      agents.values.fold(0, (s, c) => s + c.reportedLinkCount);

  /// Total messages held in M_p, across all agents.
  int get heldCount => agents.values.fold(0, (s, c) => s + c.mp.heldCount);
}

/// The n-th element of the list rooted at heap address [addr].
Term _streamElement(GlpRuntime rt, int addr, int n) {
  Object? cell = rt.heap.derefAddr(addr);
  for (var i = 0; i < n; i++) {
    cell = rt.heap.derefAddr(((cell as StructTerm).args[1] as VarRef).addr);
  }
  return (cell as StructTerm).args[0];
}

/// The variable carried by the n-th cold-call message on [netIn].
int _importedVar(GlpRuntime rt, int netIn, int n) =>
    ((_streamElement(rt, netIn, n) as StructTerm).args[0] as VarRef).addr;

/// An agent holds nothing of a link it has left: no imported-writer record, no
/// `global_send` goal, and no global writers table entry.
void _expectClean(MadContext ctx) {
  expect(ctx.up.count, 0, reason: '${ctx.agentId} retains an imported-writer record');
  expect(ctx.globalSendRegistry.pendingCount, 0,
      reason: '${ctx.agentId} retains a global_send goal');
  expect(ctx.wp.globalizeEntryCount, 0);
  expect(ctx.wp.localizeEntryCount, 0);
}

void main() {
  group('Writer forwarding (madGLP Globalize case 4)', () {
    test(
        '(a) re-exported once: the value reaches the anchor in one message, '
        'with no request and no acknowledgement, and the intermediary is clean',
        () {
      final net = _Net();
      final ctxP = net.add('p'); // the anchor
      final ctxQ = net.add('q'); // the intermediary
      final ctxR = net.add('r'); // the final holder
      final pRt = net.runtimes['p']!;
      final qRt = net.runtimes['q']!;
      final rRt = net.runtimes['r']!;
      net.bootSerializer('p');
      final qNetIn = net.bootSerializer('q');
      final rNetIn = net.bootSerializer('r');

      // p exports the writer of its pair (X_p, X_p?) to q: entry (X_p, q) at
      // index 1, global name _w(p, 1) — p is the link's anchor.
      final (xW, xR) = pRt.heap.allocateVariable();
      ctxP.send(StructTerm('m', [VarRef(xW)]), true, 'q', 0, 'q');
      net.settle();

      // q localized _w(p,1): a writer Y_q with the record (Y_q, p, 1) and the
      // goal global_send(Y_q?, _w(p,1), p) (Localize case 1).
      final yq = _importedVar(qRt, qNetIn, 0);
      expect(ctxQ.up.count, 1);
      final rec = ctxQ.up.lookup(yq)!;
      expect(rec.anchor, 'p');
      expect(rec.index, 1);
      expect(ctxQ.globalSendRegistry.getGoalFor(yq)!.globalName,
          GlobalName.writer('p', 1));
      expect(ctxQ.wp.globalizeEntryCount, 0,
          reason: 'localizing a writer name creates no entry');

      // q re-exports Y_q to r: the original name _w(p,1) is emitted, and q's
      // record and goal are removed — q leaves the link (Globalize case 4).
      ctxQ.send(StructTerm('n', [VarRef(yq)]), true, 'r', 0, 'r');
      _expectClean(ctxQ);
      net.settle();

      // r localized the forwarded name: a fresh writer with the same anchor,
      // and a goal aimed at p — not at q, the sender of the term.
      final zr = _importedVar(rRt, rNetIn, 0);
      expect(ctxR.up.lookup(zr)!.anchor, 'p');
      expect(ctxR.up.lookup(zr)!.index, 1);
      expect(ctxR.globalSendRegistry.getGoalFor(zr)!.destination, 'p');
      expect(ctxR.mp.totalLength, 0,
          reason: 'a forwarded writer needs no request');

      // r assigns the writer. r's end was forwarded, so the value it produces
      // is held (Held Link, case 2) and nothing crosses the wire yet.
      final mark = net.deliveryLog.length;
      rRt.heap.bindVariable(zr, ConstTerm(42));
      net.settle();

      expect(net.deliveryLog.length, mark,
          reason: 'a held value must not go out before authorisation');
      expect(ctxR.mp.heldCount, 1);
      expect(ctxR.hasReportedLink('p', 1, true), isTrue);
      expect((pRt.heap.derefAddr(xR) as Object?) is ConstTerm, isFalse,
          reason: 'and must not be applied at the anchor either');

      // r's program authorises: the value is released and goes to the anchor,
      // where it is held again (case 4) — a forwarded link is held at both
      // ends — until p's program authorises in turn.
      net.answerAll(true);
      net.settle();
      expect(ctxP.hasReportedLink('p', 1, true), isTrue,
          reason: 'the anchor holds an assignment from an agent it did not '
              'export the name to');
      expect((pRt.heap.derefAddr(xR) as Object?) is ConstTerm, isFalse);
      net.answerAll(true);
      net.settle();

      final tail = net.deliveryLog.sublist(mark);
      expect(tail.length, 1, reason: 'one message, however far the writer went');
      expect(tail.single.from, 'r');
      expect(tail.single.to, 'p', reason: '_w(p,1) names its destination');
      expect(tail.single.payload[0], wireMsgKindValue,
          reason: 'no request, no acknowledgement');

      // p applied it to X_p and closed its entry; q was never touched.
      expect((pRt.heap.derefAddr(xR) as ConstTerm).value, 42);
      expect(ctxP.wp.lookupByIndex(1), isNull);
      expect(tail.every((m) => m.from != 'q' && m.to != 'q'), isTrue);
      _expectClean(ctxQ);
      _expectClean(ctxR);
      expect(ctxP.pendingReaderValueCount, 0);
    });

    test(
        '(b) re-exported twice: still one message to the anchor, and both '
        'intermediaries keep no record and no goal', () {
      final net = _Net();
      final ctxP = net.add('p'); // the anchor
      final ctxQ = net.add('q'); // first intermediary
      final ctxR = net.add('r'); // second intermediary
      final ctxS = net.add('s'); // the final holder
      final pRt = net.runtimes['p']!;
      final qRt = net.runtimes['q']!;
      final rRt = net.runtimes['r']!;
      final sRt = net.runtimes['s']!;
      net.bootSerializer('p');
      final qNetIn = net.bootSerializer('q');
      final rNetIn = net.bootSerializer('r');
      final sNetIn = net.bootSerializer('s');

      final (xW, xR) = pRt.heap.allocateVariable();
      ctxP.send(StructTerm('m', [VarRef(xW)]), true, 'q', 0, 'q');
      net.settle();

      // q → r
      final yq = _importedVar(qRt, qNetIn, 0);
      ctxQ.send(StructTerm('n', [VarRef(yq)]), true, 'r', 0, 'r');
      net.settle();

      // r → s: r localized the forwarded name and re-exports it in turn.
      final zr = _importedVar(rRt, rNetIn, 0);
      expect(ctxR.up.lookup(zr)!.anchor, 'p');
      ctxR.send(StructTerm('o', [VarRef(zr)]), true, 's', 0, 's');
      net.settle();

      // s holds the sending end, still anchored at p after two forwardings.
      final ws = _importedVar(sRt, sNetIn, 0);
      expect(ctxS.up.lookup(ws)!.anchor, 'p');
      expect(ctxS.up.lookup(ws)!.index, 1);
      expect(ctxS.globalSendRegistry.getGoalFor(ws)!.destination, 'p');

      final mark = net.deliveryLog.length;
      sRt.heap.bindVariable(ws, ConstTerm('through'));
      net.settle();

      // Held at s (case 2) — nothing out — then held again at the anchor
      // (case 4) once released. Two forwardings, still two holds, not three:
      // the link has two ends however far it travelled.
      expect(net.deliveryLog.length, mark,
          reason: 'a held value must not go out before authorisation');
      expect(ctxS.mp.heldCount, 1);
      expect(ctxS.hasReportedLink('p', 1, true), isTrue);
      net.answerAll(true);
      net.settle();
      expect(ctxP.hasReportedLink('p', 1, true), isTrue);
      net.answerAll(true);
      net.settle();

      final tail = net.deliveryLog.sublist(mark);
      expect(tail.length, 1);
      expect(tail.single.from, 's');
      expect(tail.single.to, 'p');
      expect(tail.single.payload[0], wireMsgKindValue);

      expect((pRt.heap.derefAddr(xR) as ConstTerm).value, 'through');
      _expectClean(ctxQ);
      _expectClean(ctxR);
      _expectClean(ctxS);

      // Over the whole run nobody sent a request or an acknowledgement for
      // this link — a writer name needs neither.
      expect(
          net.deliveryLog.every((m) =>
              m.payload[0] != wireMsgKindRequest &&
              m.payload[0] != wireMsgKindAcknowledgement),
          isTrue);
    });

    test(
        '(c) both ends exported to the same agent: unchanged — the assignment '
        'flows back through the exporter\'s local pair', () {
      // Lemma Both Ends Exported. p sends [X, X?] to q: q gets two independent
      // pairs, and the value q writes returns to q through p.
      final net = _Net();
      final ctxP = net.add('p');
      final ctxQ = net.add('q');
      final pRt = net.runtimes['p']!;
      final qRt = net.runtimes['q']!;
      net.bootSerializer('p');
      final qNetIn = net.bootSerializer('q');

      final (xW, xR) = pRt.heap.allocateVariable();
      ctxP.send(StructTerm('m', [VarRef(xW), VarRef(xR)]), true, 'q', 0, 'q');
      net.settle();

      // q's term is m(Y_q, Z_q?): a writer with a record, and a reader with an
      // entry — q cannot tell they are connected.
      final cell = _streamElement(qRt, qNetIn, 0) as StructTerm;
      final yq = (cell.args[0] as VarRef).addr;
      final zqReader = (cell.args[1] as VarRef).addr;
      expect(ctxQ.up.count, 1);
      expect(ctxQ.up.lookup(yq)!.anchor, 'p');
      expect(ctxQ.wp.localizeEntryCount, 1,
          reason: 'the exported reader gave q an entry');

      // q assigns Y_q: the value goes to p, p's local pair carries it to X?,
      // and p's global_send returns it to q as Z_q?.
      qRt.heap.bindVariable(yq, ConstTerm('round'));
      net.settle();

      expect((pRt.heap.derefAddr(xR) as ConstTerm).value, 'round');
      expect((qRt.heap.derefAddr(zqReader) as ConstTerm).value, 'round');

      // Both links are closed: no records, no goals, no entries, nothing
      // pending. The reader-name value was acknowledged; the writer-name one
      // was not — and needed no acknowledgement.
      _expectClean(ctxP);
      _expectClean(ctxQ);
      expect(ctxP.pendingReaderValueCount, 0);
    });
  });
}
