/// A message delivered twice leaves the run unchanged (madGLP).
///
/// Normative source: the IGLP paper (35d7501) — Remark Fair Message Delivery:
/// "every message placed in a communication channel is eventually delivered
/// to its recipient, possibly more than once ... A repeat is harmless
/// everywhere but at the serializer: a value finds its entry removed, a
/// request or acknowledgement for a closed link is dropped, a request or
/// assignment for a link already held is dropped; the serializer appends a
/// cold call once, by its serial"; Definition Index-0 Serializer; Remark
/// Cold-Call via Serializer; Definition madGLP Receive Transaction; §Held
/// Links.  GLP's task of 2026-10-09 19:01 UTC: "A test: every message kind
/// delivered twice, the run unchanged".
///
/// Each run is made twice, once with every message delivered once and once
/// with every message delivered twice, and the two end in the same state:
/// each agent's network input stream, the values its variables received, and
/// its tables, records, goals, outgoing messages and held, pending, early and
/// reported traffic.
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;

typedef _Msg = ({String from, String to, List<int> payload});

class _Net {
  final bool twice;
  final Map<String, MadContext> agents = {};
  final Map<String, GlpRuntime> runtimes = {};
  final Map<String, HeapCell> netIn = {};
  final List<_Msg> queue = [];
  final List<_Msg> deliveryLog = [];

  /// A network that delivers every message once, or, where [twice], every
  /// message twice in a row.
  _Net({required this.twice});

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

  void settle() {
    while (true) {
      for (final ctx in agents.values) {
        ctx.flushMessages();
      }
      if (queue.isEmpty) break;
      while (queue.isNotEmpty) {
        final m = queue.removeAt(0);
        for (var k = 0; k < (twice ? 2 : 1); k++) {
          deliveryLog.add(m);
          agents[m.to]!
              .handleIncomingPayload(payload: m.payload, fromAgent: m.from);
        }
      }
    }
  }

  /// Settle, authorising every reported link, until nothing is reported.
  void settleAuthorising() {
    for (var i = 0; i < 20; i++) {
      settle();
      var answered = 0;
      for (final ctx in agents.values) {
        for (final (gn, _) in ctx.reportedLinks.toList()) {
          ctx.answerLink(gn, true);
          answered++;
        }
      }
      if (answered == 0) return;
    }
    throw StateError('links kept being reported after 20 rounds');
  }

  /// The elements of [id]'s network input stream, in order.
  List<Term> stream(String id) {
    final rt = runtimes[id]!;
    final out = <Term>[];
    Object? cell = rt.heap.derefAddr(netIn[id]!);
    while (cell is StructTerm && cell.functor == '.') {
      out.add(cell.args[0]);
      final tail = cell.args[1];
      if (tail is! VarRef) break;
      cell = rt.heap.derefAddr(tail.addr);
    }
    return out;
  }

  /// [t] at [id] with every bound variable replaced by its value, and an
  /// unbound one written by its polarity and cell.
  String show(String id, Object? t) {
    final heap = runtimes[id]!.heap;
    if (t is VarRef) {
      final v = heap.derefAddr(t.addr);
      if (v is VarRef) {
        return '${heap.isReader(t.addr) ? 'R' : 'W'}@${t.addr}';
      }
      return show(id, v);
    }
    if (t is StructTerm) {
      return '${t.functor}(${t.args.map((a) => show(id, a)).join(', ')})';
    }
    if (t is ConstTerm) return '${t.value}';
    return '$t';
  }

  /// The state the run ends in, agent by agent.
  Map<String, Object> state() => {
        for (final MapEntry(key: id, value: ctx) in agents.entries)
          id: {
            'stream': [for (final e in stream(id)) show(id, e)],
            'nextIndex': ctx.wp.nextIndex,
            'globalizeEntries': ctx.wp.globalizeEntryCount,
            'localizeEntries': ctx.wp.localizeEntryCount,
            'coldCallsAppended': ctx.wp.serializerAppendedCount,
            'records': ctx.up.count,
            'goals': ctx.globalSendRegistry.pendingCount,
            'outgoing': ctx.mp.totalLength,
            'heldOutgoing': ctx.mp.heldCount,
            'pending': ctx.pendingReaderValueCount,
            'heldAssignments': ctx.heldAssignmentCount,
            'heldRequests': ctx.heldRequestCount,
            'early': ctx.earlyAssignmentCount,
            'reported': ctx.reportedLinkCount,
          }.toString(),
      };

  /// The kinds of the messages delivered, by sender and recipient: value,
  /// serializer (a value to `_w(q,0)`), request, acknowledgement.
  List<String> kinds() => [
        for (final m in deliveryLog)
          '${m.from}>${m.to}:${switch (m.payload[0]) {
            0 => PayloadCodec.deserializeValuePayload(m.payload).serial != null
                ? 'serializer'
                : 'value',
            1 => 'request',
            2 => 'acknowledgement',
            _ => 'unknown',
          }}'
      ];
}

/// The run [scenario] makes, once with single and once with double delivery:
/// the state each ends in, the kinds of message each delivered, and what
/// [scenario] observed in each.
({
  Map<String, Object> once,
  Map<String, Object> twice,
  List<String> kindsOnce,
  List<String> kindsTwice,
  Object? seenOnce,
  Object? seenTwice,
}) _both(Object? Function(_Net net) scenario) {
  final a = _Net(twice: false);
  final seenOnce = scenario(a);
  final b = _Net(twice: true);
  final seenTwice = scenario(b);
  return (
    once: a.state(),
    twice: b.state(),
    kindsOnce: a.kinds(),
    kindsTwice: b.kinds(),
    seenOnce: seenOnce,
    seenTwice: seenTwice,
  );
}

/// Every message of the single run, each delivered twice.
List<String> _doubled(List<String> kinds) => [for (final k in kinds) ...[k, k]];

void _expectUnchanged(
    ({
      Map<String, Object> once,
      Map<String, Object> twice,
      List<String> kindsOnce,
      List<String> kindsTwice,
      Object? seenOnce,
      Object? seenTwice,
    }) r,
    List<String> kinds) {
  for (final k in kinds) {
    expect(r.kindsOnce.any((m) => m.endsWith(':$k')), isTrue,
        reason: 'the run carries a $k');
  }
  expect(r.kindsTwice, _doubled(r.kindsOnce),
      reason: 'every message delivered twice, and nothing more sent');
  expect(r.twice, r.once);
  expect(r.seenTwice, r.seenOnce);
}

/// The head of the first element of [id]'s stream: `m(...)`'s [i]th argument.
VarRef _arg(_Net net, String id, int element, int i) =>
    (net.stream(id)[element] as StructTerm).args[i] as VarRef;

void main() {
  group('every message kind delivered twice, the run unchanged', () {
    test('a serializer message, a value each way and an acknowledgement', () {
      final r = _both((net) {
        final alice = net.add('alice');
        net.add('bob');
        final aRt = net.runtimes['alice']!;
        final bRt = net.runtimes['bob']!;
        // alice cold-calls bob with a reader and a writer of her own.
        final (xW, xR) = aRt.heap.allocateVariable();
        final (yW, yR) = aRt.heap.allocateVariable();
        alice.send(StructTerm('m', [VarRef(xR), VarRef(yW)]), true, 'bob', 0,
            'bob');
        net.settle();
        // alice assigns X: a value to bob on _r(alice,1), acknowledged.
        aRt.heap.bindVariable(xW, ConstTerm('hello'));
        net.settle();
        // bob assigns the writer he received: a value to alice on _w(alice,2).
        bRt.heap.bindVariable(_arg(net, 'bob', 0, 1).addr, ConstTerm('world'));
        net.settle();
        return [
          net.show('alice', VarRef(yR)),
          net.show('bob', _arg(net, 'bob', 0, 0)),
        ];
      });
      expect(r.seenOnce, ['world', 'hello']);
      _expectUnchanged(r, ['serializer', 'value', 'acknowledgement']);
    });

    test('a forwarded reader: the held request, the redirected value and its '
        'acknowledgement', () {
      final r = _both((net) {
        final bob = net.add('bob');
        final charlie = net.add('charlie');
        net.add('alice');
        final bobRt = net.runtimes['bob']!;
        final charlieRt = net.runtimes['charlie']!;
        // charlie anchors a reader and exports it to bob, who forwards it to
        // alice in a cold call: alice requests it of charlie (Localize case 2).
        final (restW, restR) = charlieRt.heap.allocateVariable();
        final gres = globalize(
          variables: [TermVar.reader(restR, writerAddr: restW)],
          localAgent: 'charlie',
          remoteAgent: 'bob',
          table: charlie.wp,
          records: charlie.up,
        );
        charlie.registerGlobalSendSpawns(gres.spawns);
        final lres = localize(
          globalNames: gres.globalNames,
          localAgent: 'bob',
          fromAgent: 'charlie',
          table: bob.wp,
          records: bob.up,
          freshAddrAllocator: () => bobRt.heap.allocateVariable(),
        );
        bob.send(StructTerm('m', [VarRef(lres.freshPairs[0].readerAddr)]),
            true, 'alice', 0, 'alice');
        // alice's request is held at alice, then at charlie; both authorise.
        net.settleAuthorising();
        charlieRt.heap.bindVariable(restW, ConstTerm('through'));
        net.settleAuthorising();
        return net.show('alice', _arg(net, 'alice', 0, 0));
      });
      expect(r.seenOnce, 'through');
      _expectUnchanged(r, ['serializer', 'request', 'value', 'acknowledgement']);
    });

    test('a forwarded writer: the held assignment at the anchor', () {
      final r = _both((net) {
        final p = net.add('p');
        net.add('q');
        net.add('r');
        final pRt = net.runtimes['p']!;
        // p exports a writer to q, who forwards it to r; r assigns it, and the
        // value is held at r and then at p (Held Links, cases 2 and 4).
        final (xW, xR) = pRt.heap.allocateVariable();
        p.send(StructTerm('m', [VarRef(xW)]), true, 'q', 0, 'q');
        net.settle();
        net.agents['q']!.send(
            StructTerm('n', [_arg(net, 'q', 0, 0)]), true, 'r', 0, 'r');
        net.settle();
        net.runtimes['r']!.heap
            .bindVariable(_arg(net, 'r', 0, 0).addr, ConstTerm(42));
        net.settleAuthorising();
        return net.show('p', VarRef(xR));
      });
      expect(r.seenOnce, '42');
      _expectUnchanged(r, ['serializer', 'value']);
    });
  });

  test('a fresh cold call after a duplicate is still delivered', () {
    final r = _both((net) {
      final alice = net.add('alice');
      net.add('bob');
      alice.send(ConstTerm('one'), true, 'bob', 0, 'bob');
      net.settle();
      alice.send(ConstTerm('two'), true, 'bob', 0, 'bob');
      net.settle();
      alice.send(ConstTerm('two'), true, 'bob', 0, 'bob');
      net.settle();
      return [for (final e in net.stream('bob')) net.show('bob', e)];
    });
    expect(r.seenOnce, ['one', 'two', 'two'],
        reason: 'the same term sent again is a new cold call');
    _expectUnchanged(r, ['serializer']);
  });
}
