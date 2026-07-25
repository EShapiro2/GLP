/// Anchored links with request/acknowledge transfer (madGLP v2).
///
/// Normative source: the IGLP paper — Definition Globalize (forwarding case),
/// Definition Localize (request rule), §app:requests-acks (pending values,
/// acknowledgements, stale drop), and appendix-madglp-trace Example 2, closing
/// paragraph "The stream continues directly" (the acceptance scenario).
///
/// A link's global name — its anchor — is fixed for life: an unbound imported
/// reader is forwarded under its original name; the new holder requests the
/// value from the anchor; a sent reader-name value stays pending at its sender
/// until acknowledged; a request re-addresses it; stale traffic is dropped
/// silently; no epochs.

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';

/// One delivered or queued message in the harness network.
typedef _Msg = ({String from, String to, List<int> payload});

/// A test network of MadContexts exchanging opaque payload bytes. Messages
/// flushed from an agent's M_p queue here; the test controls delivery order.
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

  void _deliver(_Msg m) {
    deliveryLog.add(m);
    agents[m.to]!.handleIncomingPayload(payload: m.payload, fromAgent: m.from);
  }

  /// Deliver (in order) every queued message matching [where]; others stay
  /// queued. Returns the number delivered.
  int deliverWhere(bool Function(_Msg m) where) {
    var n = 0;
    var i = 0;
    while (i < queue.length) {
      if (where(queue[i])) {
        _deliver(queue.removeAt(i));
        n++;
      } else {
        i++;
      }
    }
    return n;
  }

  /// Flush and deliver until the network is quiescent.
  void settle() {
    while (true) {
      flushAll();
      if (queue.isEmpty) break;
      while (queue.isNotEmpty) {
        _deliver(queue.removeAt(0));
      }
    }
  }
}

/// The n-th element of the list rooted at heap address [addr] (a writer or
/// reader), following bound tails; throws if the spine is not bound that far.
Term _streamElement(GlpRuntime rt, int addr, int n) {
  Object? cell = rt.heap.derefAddr(addr);
  for (var i = 0; i < n; i++) {
    cell = rt.heap.derefAddr(((cell as StructTerm).args[1] as VarRef).addr);
  }
  return (cell as StructTerm).args[0];
}

void main() {
  group('Anchored links (madGLP v2)', () {
    test(
        '(a) acceptance: introduction relays one value, req redirects the '
        'link, the stream continues directly, the introducer ends clean', () {
      final net = _Net();
      final ctxAlice = net.add('alice');
      final ctxBob = net.add('bob');
      final ctxCharlie = net.add('charlie');
      final aliceRt = net.runtimes['alice']!;
      final bobRt = net.runtimes['bob']!;
      final charlieRt = net.runtimes['charlie']!;
      final aliceNetIn = net.bootSerializer('alice');
      net.bootSerializer('bob');
      final charlieNetIn = net.bootSerializer('charlie');

      // Bob's pair (X_b, X_b?); he introduces Alice↔Charlie by sending the
      // reader to Alice and the writer to Charlie, each in a cold-call.
      final (xbW, xbR) = bobRt.heap.allocateVariable();
      ctxBob.send(StructTerm('m', [VarRef(xbR)]), true, 'alice', 0, 'alice');
      ctxBob.send(StructTerm('n', [VarRef(xbW)]), true, 'charlie', 0,
          'charlie');
      net.settle();

      // Alice's imported reader X_a? and Charlie's imported writer X_c.
      final xaReader =
          ((_streamElement(aliceRt, aliceNetIn, 0) as StructTerm).args[0]
                  as VarRef)
              .addr;
      final xcWriter =
          ((_streamElement(charlieRt, charlieNetIn, 0) as StructTerm).args[0]
                  as VarRef)
              .addr;

      // First stream element: Charlie assigns X_c := [hello | Rest?]. The
      // value flows Charlie→Bob→Alice; forwarding the tail's reader emits the
      // original name _r(charlie, k); Alice's req reaches Charlie.
      final (restW, _) = charlieRt.heap.allocateVariable();
      final restReader = charlieRt.heap.pairedReaderAddr(restW);
      charlieRt.heap.bindVariable(
          xcWriter, StructTerm('.', [ConstTerm('hello'), VarRef(restReader)]));
      net.settle();

      // Alice received the first element through Bob.
      expect((_streamElement(aliceRt, xaReader, 0) as ConstTerm).value,
          'hello');

      // Bob ends with empty table, no goals, and no pending values.
      expect(ctxBob.wp.globalizeEntryCount, 0);
      expect(ctxBob.wp.localizeEntryCount, 0);
      expect(ctxBob.globalSendRegistry.pendingCount, 0);
      expect(ctxBob.pendingReaderValueCount, 0);

      // Charlie recorded Alice as the tail link's holder (Alice's request).
      expect(ctxCharlie.globalSendRegistry.getGoalFor(restW)!.destination,
          'alice');

      // The next element flows Charlie→Alice directly: no message touches Bob.
      final mark = net.deliveryLog.length;
      final (rest2W, _) = charlieRt.heap.allocateVariable();
      final rest2Reader = charlieRt.heap.pairedReaderAddr(rest2W);
      charlieRt.heap.bindVariable(
          restW, StructTerm('.', [ConstTerm('world'), VarRef(rest2Reader)]));
      net.settle();

      final tail = net.deliveryLog.sublist(mark);
      expect(tail, isNotEmpty);
      expect(tail.every((m) => m.from != 'bob' && m.to != 'bob'), isTrue,
          reason: 'the introducer relays one value and drops out');
      expect((_streamElement(aliceRt, xaReader, 1) as ConstTerm).value,
          'world');

      // The direct value was acknowledged: nothing pends at Charlie.
      expect(ctxCharlie.pendingReaderValueCount, 0);
    });

    test('(b) a reader-name value pends until its ack releases it', () {
      final net = _Net();
      final ctxP = net.add('p');
      final ctxQ = net.add('q');
      final pRt = net.runtimes['p']!;
      final qRt = net.runtimes['q']!;

      // p globalizes reader X? for q; q localizes from the anchor (no req).
      final (xW, xR) = pRt.heap.allocateVariable();
      final gres = globalize(
        variables: [TermVar.reader(xR, writerAddr: xW)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
      );
      ctxP.registerGlobalSendSpawns(gres.spawns);
      final rn = gres.globalNames[0]; // _r(p, i)
      final lres = localize(
        globalNames: gres.globalNames,
        localAgent: 'q',
        fromAgent: 'p',
        table: ctxQ.wp,
        freshAddrAllocator: () => qRt.heap.allocateVariable(),
      );
      expect(lres.requests, isEmpty); // sender is the anchor
      final zW = lres.freshPairs[0].writerAddr;

      // p assigns X := 42: the value is sent and retained pending.
      pRt.heap.bindVariable(xW, ConstTerm(42));
      net.flushAll();
      expect(ctxP.hasPendingReaderValue('p', rn.index), isTrue);

      // Deliver the value to q: q applies it, removes the entry, and acks —
      // the value still pends at p until the ack arrives.
      net.deliverWhere((m) => m.to == 'q');
      expect((qRt.heap.derefAddr(zW) as ConstTerm).value, 42);
      expect(ctxQ.wp.findByRemote('p', rn.index), isNull);
      expect(ctxP.hasPendingReaderValue('p', rn.index), isTrue);

      // Deliver the ack: the pending value is released — the link is closed.
      net.flushAll();
      net.deliverWhere((m) => m.to == 'p');
      expect(ctxP.hasPendingReaderValue('p', rn.index), isFalse);
      expect(ctxP.pendingReaderValueCount, 0);

      // Stale traffic for the closed link is dropped silently.
      expect(() => ctxP.handleAck(rn, 'q'), returnsNormally);
      expect(() => ctxP.handleRequest(rn, 'q'), returnsNormally);
    });

    test(
        '(c) a value reaching a past holder is dropped; the redirected value '
        'reaches the current holder exactly once', () {
      final net = _Net();
      final ctxAlice = net.add('alice');
      final ctxBob = net.add('bob');
      final ctxCharlie = net.add('charlie');
      final aliceRt = net.runtimes['alice']!;
      final bobRt = net.runtimes['bob']!;
      final charlieRt = net.runtimes['charlie']!;
      final aliceNetIn = net.bootSerializer('alice');

      // Charlie (the anchor) exports reader Rest? to Bob.
      final (restW, restR) = charlieRt.heap.allocateVariable();
      final gres = globalize(
        variables: [TermVar.reader(restR, writerAddr: restW)],
        localAgent: 'charlie',
        remoteAgent: 'bob',
        table: ctxCharlie.wp,
      );
      ctxCharlie.registerGlobalSendSpawns(gres.spawns);
      final rn = gres.globalNames[0]; // _r(charlie, k)
      final lres = localize(
        globalNames: gres.globalNames,
        localAgent: 'bob',
        fromAgent: 'charlie',
        table: ctxBob.wp,
        freshAddrAllocator: () => bobRt.heap.allocateVariable(),
      );
      final restBReader = lres.freshPairs[0].readerAddr;

      // Charlie produces the value; it is sent towards Bob — hold delivery.
      charlieRt.heap.bindVariable(restW, ConstTerm('payload'));
      net.flushAll();
      expect(ctxCharlie.hasPendingReaderValue('charlie', rn.index), isTrue);

      // Before the value arrives, Bob forwards the reader to Alice in a
      // cold-call: the original name is emitted, Bob's entry removed.
      ctxBob.send(StructTerm('m', [VarRef(restBReader)]), true, 'alice', 0,
          'alice');
      expect(ctxBob.wp.localizeEntryCount, 0);
      net.flushAll();
      // Deliver the carrier to Alice: she localizes the forwarded name and
      // queues req(_r(charlie, k)) to Charlie.
      net.deliverWhere((m) => m.from == 'bob' && m.to == 'alice');
      net.flushAll();

      // The in-flight value now reaches the past holder: dropped silently —
      // no application, no ack.
      net.deliverWhere((m) => m.from == 'charlie' && m.to == 'bob');
      expect(ctxBob.wp.localizeEntryCount, 0);
      expect(ctxBob.mp.totalLength, 0, reason: 'a dropped value is not acked');
      expect(ctxCharlie.hasPendingReaderValue('charlie', rn.index), isTrue);

      // Alice's request redirects the pending value to her.
      net.deliverWhere((m) => m.from == 'alice' && m.to == 'charlie');
      net.flushAll();
      net.deliverWhere((m) => m.from == 'charlie' && m.to == 'alice');

      // Applied exactly once at the current holder.
      final zA =
          ((_streamElement(aliceRt, aliceNetIn, 0) as StructTerm).args[0]
                  as VarRef)
              .addr;
      expect((aliceRt.heap.derefAddr(zA) as ConstTerm).value, 'payload');
      expect(ctxAlice.wp.findByRemote('charlie', rn.index), isNull);

      // Alice's ack closes the link at Charlie.
      net.settle();
      expect(ctxCharlie.pendingReaderValueCount, 0);
    });

    test(
        '(d) request before value: the anchor records the holder and the '
        'value is sent there on production', () {
      final net = _Net();
      final ctxBob = net.add('bob');
      final ctxCharlie = net.add('charlie');
      final aliceRt = net.add('alice').runtime;
      final ctxAlice = net.agents['alice']!;
      final bobRt = net.runtimes['bob']!;
      final charlieRt = net.runtimes['charlie']!;
      final aliceNetIn = net.bootSerializer('alice');

      // Charlie (the anchor) exports reader Rest? to Bob; Bob localizes.
      final (restW, restR) = charlieRt.heap.allocateVariable();
      final gres = globalize(
        variables: [TermVar.reader(restR, writerAddr: restW)],
        localAgent: 'charlie',
        remoteAgent: 'bob',
        table: ctxCharlie.wp,
      );
      ctxCharlie.registerGlobalSendSpawns(gres.spawns);
      final rn = gres.globalNames[0]; // _r(charlie, k)
      final lres = localize(
        globalNames: gres.globalNames,
        localAgent: 'bob',
        fromAgent: 'charlie',
        table: ctxBob.wp,
        freshAddrAllocator: () => bobRt.heap.allocateVariable(),
      );
      final restBReader = lres.freshPairs[0].readerAddr;

      // Bob forwards the reader to Alice before any value exists; Alice's
      // request reaches Charlie, who records her as the link's holder.
      ctxBob.send(StructTerm('m', [VarRef(restBReader)]), true, 'alice', 0,
          'alice');
      net.settle();
      expect(ctxCharlie.globalSendRegistry.getGoalFor(restW)!.destination,
          'alice');

      // On production the value goes to the recorded holder directly.
      final mark = net.deliveryLog.length;
      charlieRt.heap.bindVariable(restW, ConstTerm('late'));
      net.settle();

      final tail = net.deliveryLog.sublist(mark);
      expect(tail.every((m) => m.from != 'bob' && m.to != 'bob'), isTrue,
          reason: 'the value never touches the past holder');
      final zA =
          ((_streamElement(aliceRt, aliceNetIn, 0) as StructTerm).args[0]
                  as VarRef)
              .addr;
      expect((aliceRt.heap.derefAddr(zA) as ConstTerm).value, 'late');
      expect(ctxAlice.wp.findByRemote('charlie', rn.index), isNull);
      expect(ctxCharlie.pendingReaderValueCount, 0);
    });
  });
}
