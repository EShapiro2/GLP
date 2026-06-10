/// Reverse-order delivery test (seam spec v0.2 §7.3 / Issue 7).
///
/// Wires two MadContexts (alice, bob) through one SimulationRouter with the real
/// serialize-on-send / deserialize-on-deliver seam glue. alice cold-calls bob
/// with a term carrying a reader, then binds the reader (firing the global_send
/// for the `_r(alice, i) := T` assignment). The pair is held so both messages
/// queue in the router, then released in REVERSE order (assignment before its
/// carrier). The Issue 7 hold mechanism must make bob reach the same outcome as
/// in-order delivery.

import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';

/// One alice→bob cold-call carrying a reader, with the reply value [reply].
/// Runs the pair through [router] under the given delivery regime and returns
/// bob's network-input writer address plus bob's runtime for inspection.
({GlpRuntime bobRt, int bobNetIn}) _runColdCall(
  SimulationRouter router, {
  required bool reverse,
  required String reply,
}) {
  final aliceRt = GlpRuntime();
  final bobRt = GlpRuntime();
  final ctxAlice = MadContext(agentId: 'alice', runtime: aliceRt);
  final ctxBob = MadContext(agentId: 'bob', runtime: bobRt);

  // bob's permanent index-0 serializer entry (boot step).
  final (bobNetIn, _) = bobRt.heap.allocateVariable();
  ctxBob.wp.initializeSerializerEntry(bobNetIn);

  // Seam glue: alice's outbound payloads route through the router; the router
  // delivers to bob, where they are deserialized and handed to handleMadAssignment.
  ctxAlice.onMessageReady = (destId, msg) {
    router.routeSend('alice', destId, Uint8List.fromList(msg.payload));
  };
  router.onDeliver = (toId, fromPk, payload, messageId, t) {
    final (gn, value) = PayloadSerializer('bob').deserializeGlobalSendPayload(
      payload,
      (isReader) {
        final (w, r) = bobRt.heap.allocateVariable();
        return isReader ? r : w;
      },
    );
    ctxBob.handleMadAssignment(
      globalName: gn,
      value: value,
      fromAgent: router.directory.idOf(fromPk)!,
    );
  };

  // For the reverse case, hold the pair so both messages queue in the router;
  // for the in-order case, leave delivery immediate (carrier then assignment).
  if (reverse) router.holdDelivery('alice', 'bob');

  // 1) Carrier: cold-call to bob's serializer (index 0) carrying reader W?.
  final (writerW, readerW) = aliceRt.heap.allocateVariable();
  ctxAlice.send(
    StructTerm('msg', [VarRef(readerW)]),
    true, // isWriter: destination serializer _w(bob, 0)
    'bob',
    0,
    'bob',
  );
  ctxAlice.flushMessages(); // → router (held)

  // 2) Assignment: alice binds W, firing global_send(_r(alice,1) := reply).
  aliceRt.heap.bindVariable(writerW, ConstTerm(reply));
  ctxAlice.onWriterBound(writerW, ConstTerm(reply));
  ctxAlice.flushMessages(); // → router (held)

  // Release in reverse order (assignment before its carrier).
  if (reverse) router.releaseDelivery('alice', 'bob');

  return (bobRt: bobRt, bobNetIn: bobNetIn);
}

/// Build a 2-agent router (alice, bob), both Open, recording nothing.
SimulationRouter _router() {
  final r = SimulationRouter();
  for (final id in ['alice', 'bob']) {
    r.register(id, generateKeyPair().pub);
    r.setTrustLevel(id, TrustLevel.open);
  }
  r.onConnectivity = (_, __, ___, ____) {};
  return r;
}

/// Extract the reply value bob received on its network-input stream:
/// netIn := [ msg(Z?) | N'? ] with Z bound to the reply.
Object _deliveredReply(GlpRuntime bobRt, int bobNetIn) {
  final cell = bobRt.heap.derefAddr(bobNetIn);
  expect(cell, isA<StructTerm>());
  final head = (cell as StructTerm).args[0]; // msg(Z?)
  expect(head, isA<StructTerm>());
  final inner = (head as StructTerm).args[0]; // Z?
  expect(inner, isA<VarRef>());
  return bobRt.heap.derefAddr((inner as VarRef).addr);
}

void main() {
  group('Reverse-order delivery (§7.3 / Issue 7)', () {
    test('in-order delivery: bob receives the reply', () {
      final r = _router();
      final out = _runColdCall(r, reverse: false, reply: 'pong');
      final delivered = _deliveredReply(out.bobRt, out.bobNetIn);
      expect(delivered, isA<ConstTerm>());
      expect((delivered as ConstTerm).value, 'pong');
    });

    test('reverse-order delivery: same outcome via the hold mechanism', () {
      final r = _router();
      // Assignment is released before its carrier; Issue 7 holds it until the
      // carrier's localize() creates the entry, then delivers it.
      final out = _runColdCall(r, reverse: true, reply: 'pong');
      final delivered = _deliveredReply(out.bobRt, out.bobNetIn);
      expect(delivered, isA<ConstTerm>());
      expect((delivered as ConstTerm).value, 'pong');
    });
  });
}
