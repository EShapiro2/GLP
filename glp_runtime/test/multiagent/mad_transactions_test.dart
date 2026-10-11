/// Tests for madGLP transaction handling
///
/// Validates end-to-end message flow using the push-based model.
///
/// See: IGLP Definitions madGLP Reduce, Send and Receive Transaction
/// (app:madglp-local-states-transitions).

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/multiagent/global_send.dart';

void main() {
  group('Receive Transaction', () {
    test('_w(p,i) message: finds GlobalizeEntry by index, binds writer', () {
      // Given: Agent p has a GlobalizeEntry (X, q) at index 1 from globalizing writer X
      // When: p receives message _w(p,1) := 42 from q
      // Then: X is bound to 42, entry removed

      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'p', runtime: runtime);

      // Create a local pair that will receive the assignment
      final (writerAddr, readerAddr) = runtime.heap.allocateVariable();

      // Add GlobalizeEntry: this simulates having globalized writer X
      // addGlobalizeEntry returns the allocated index (starts at 1)
      final index = ctx.wp.addGlobalizeEntry(writerAddr, 'q');

      // Receive the assignment message
      final globalName = GlobalName.writer('p', index);
      ctx.handleMadAssignment(
        globalName: globalName,
        value: ConstTerm(42),
        fromAgent: 'q',
      );

      // Verify writer was bound
      final derefed = runtime.heap.derefAddr(writerAddr);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 42);

      // Verify entry was removed
      expect(ctx.wp.lookupByIndex(index), isNull);
    });

    test('_r(p,i) message: finds LocalizeEntry, binds writer', () {
      // Given: Agent q has a LocalizeEntry (Z_q, p, 3) from localizing _r(p, 3)
      // When: q receives message _r(p,3) := 42
      // Then: Z_q is bound to 42, entry removed

      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'q', runtime: runtime);

      // Create a local pair (Z_q, Z_q?) where Z_q is the writer
      final (writerAddr, readerAddr) = runtime.heap.allocateVariable();

      // Add LocalizeEntry: this simulates having localized _r(p, 3)
      ctx.wp.addLocalizeEntry(writerAddr, 'p', 3);

      // Receive the assignment message from p
      final globalName = GlobalName.reader('p', 3);
      ctx.handleMadAssignment(
        globalName: globalName,
        value: ConstTerm(42),
        fromAgent: 'p',
      );

      // Verify writer was bound
      final derefed = runtime.heap.derefAddr(writerAddr);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 42);

      // Verify entry was removed
      expect(ctx.wp.findByRemote('p', 3), isNull);
    });

    test('receive localizes nested variables', () {
      // Given: Agent p receives _w(p,1) message with nested global names
      // When: value contains _r(q,2) (a reader global name)
      // Then: Fresh pair created, entry added for _r

      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'p', runtime: runtime);

      // Setup: p has GlobalizeEntry for receiving main message _w(p,1)
      final (writerAddr, _) = runtime.heap.allocateVariable();
      final index = ctx.wp.addGlobalizeEntry(writerAddr, 'q');

      // The value contains a nested global name _r(q,2) (a reader from q)
      // Under corrected definitions, localizing _r creates a LocalizeEntry
      final nestedGlobalNames = [GlobalName.reader('q', 2)];

      ctx.handleMadAssignmentWithGlobalNames(
        globalName: GlobalName.writer('p', index),
        value: ConstTerm('placeholder'), // Will be replaced
        nestedGlobalNames: nestedGlobalNames,
        fromAgent: 'q',
      );

      // Verify LocalizeEntry was created for nested _r(q,2)
      expect(ctx.wp.findByRemote('q', 2), isNotNull);
    });

    test('writer value with no GlobalizeEntry is dropped silently', () {
      // Spec §app:requests-acks (stale drop): a value matching no entry and no
      // pending value is dropped. A writer-name value always finds its fixed
      // anchor entry, so a miss is provably stale — indices are never reused.
      // This replaces the pre-anchored-links behavior (throw StateError).
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'p', runtime: runtime);

      // No entry exists at index 5 — the value is dropped, nothing thrown.
      expect(
        () => ctx.handleMadAssignment(
          globalName: GlobalName.writer('p', 5),
          value: ConstTerm(42),
          fromAgent: 'q',
        ),
        returnsNormally,
      );
      expect(ctx.wp.globalizeEntryCount, 0);
    });

    test('early _r assignment is held, then delivered when localize creates the entry', () {
      // Issue 7 / spec §8.3 (Early Messages): a `_r(p, i) := T` arriving before
      // its LocalizeEntry exists is HELD, not dropped or thrown, and delivered
      // when localize() creates the entry. This overrides the prior behavior
      // (throw StateError). Only the `_w` case still throws on a missing
      // GlobalizeEntry (see the test above) — `_w` entries always exist first.
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'q', runtime: runtime);

      // Network-input serializer entry at index 0 (boot step).
      final (netW, _) = runtime.heap.allocateVariable();
      ctx.wp.initializeSerializerEntry(netW);

      // 1) Early assignment for (p, 5) arrives before any entry: must not throw,
      //    must create no entry, and must bind nothing yet (held).
      expect(
        () => ctx.handleMadAssignment(
          globalName: GlobalName.reader('p', 5),
          value: ConstTerm(42),
          fromAgent: 'p',
        ),
        returnsNormally,
      );
      expect(ctx.wp.findByRemote('p', 5), isNull);

      // 2) Carrier cold-call (serializer, index 0) whose content is _r(p, 5):
      //    localize() creates LocalizeEntry (Z_q, p, 5) and immediately delivers
      //    the held assignment, binding Z_q to 42.
      final content = StructTerm('_r', [ConstTerm('p'), ConstTerm(5)]);
      final carrier = StructTerm(
          '.', [content, StructTerm('_w', [ConstTerm('q'), ConstTerm(0)])]);
      ctx.handleMadAssignment(
        globalName: GlobalName.writer('q', 0),
        value: carrier,
        fromAgent: 'p',
        serial: 1,
      );

      // The held value reached the localized reader on the network-input stream:
      // netW := [ Z_q? | N'_q? ] with Z_q bound to 42.
      final cell = runtime.heap.derefAddr(netW);
      expect(cell, isA<StructTerm>());
      final head = (cell as StructTerm).args[0];
      expect(head, isA<VarRef>());
      final delivered = runtime.heap.derefAddr((head as VarRef).addr);
      expect(delivered, isA<ConstTerm>());
      expect((delivered as ConstTerm).value, 42);

      // Entry was created by localize then removed by delivery.
      expect(ctx.wp.findByRemote('p', 5), isNull);
    });

    // Definition madGLP Receive Transaction, Serializer case: "If (s, i) ∈
    // D_q, m is a repeat: it is dropped and nothing changes.  Otherwise ...
    // The entry at index 0 in W'_q becomes (N'_q, D_q ∪ {(s, i)})."
    test('a serializer message is appended once, by its sender and serial',
        () {
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'q', runtime: runtime);
      final (netW, _) = runtime.heap.allocateVariable();
      ctx.wp.initializeSerializerEntry(netW);

      Term cell(Term content) => StructTerm(
          '.', [content, StructTerm('_w', [ConstTerm('q'), ConstTerm(0)])]);
      void receive(Term content, String from, int serial) =>
          ctx.handleMadAssignment(
            globalName: GlobalName.writer('q', 0),
            value: cell(content),
            fromAgent: from,
            serial: serial,
          );
      List<Object?> stream() {
        final out = <Object?>[];
        Object? c = runtime.heap.derefAddr(netW);
        while (c is StructTerm && c.functor == '.') {
          final head = c.args[0];
          out.add(head is ConstTerm ? head.value : head);
          final tail = c.args[1];
          if (tail is! VarRef) break;
          c = runtime.heap.derefAddr(tail.addr);
        }
        return out;
      }

      // A cold call carrying a reader: localized, appended, its pair in D_q.
      receive(StructTerm('_r', [ConstTerm('p'), ConstTerm(1)]), 'p', 2);
      expect(ctx.wp.localizeEntryCount, 1);
      final writerAfterFirst = ctx.wp.serializerWriterAddr;
      expect(ctx.wp.serializerHasAppended('p', 2), isTrue);

      // The same message again: dropped and nothing changes — no second
      // entry, no second element, the entry's writer where it was.
      receive(StructTerm('_r', [ConstTerm('p'), ConstTerm(1)]), 'p', 2);
      expect(ctx.wp.localizeEntryCount, 1);
      expect(ctx.wp.serializerWriterAddr, writerAfterFirst);
      expect(ctx.wp.serializerAppendedCount, 1);

      // A fresh serial from the same sender, and the same serial from
      // another, are new cold calls and are appended.
      receive(ConstTerm('second'), 'p', 3);
      receive(ConstTerm('third'), 'r', 2);
      receive(ConstTerm('third'), 'r', 2);
      final s = stream();
      expect(s.length, 3);
      expect(s.sublist(1), ['second', 'third']);
      expect(ctx.wp.serializerAppendedCount, 3);
    });

    // Remark Fair Message Delivery: "A repeat is harmless everywhere but at the
    // serializer: a value finds its entry removed"; §app:requests-acks: "A
    // value ... matching no entry and no pending value is dropped".  A repeat
    // is not early (app:in-networking, Early messages), so it is not held.
    test('a reader-name value delivered twice is applied once and the repeat '
        'dropped, not held as early', () {
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'q', runtime: runtime);
      final (writerAddr, _) = runtime.heap.allocateVariable();
      ctx.wp.addLocalizeEntry(writerAddr, 'p', 3);

      for (var k = 0; k < 2; k++) {
        ctx.handleMadAssignment(
          globalName: GlobalName.reader('p', 3),
          value: ConstTerm(42),
          fromAgent: 'p',
        );
      }
      expect((runtime.heap.derefAddr(writerAddr) as ConstTerm).value, 42);
      expect(ctx.wp.findByRemote('p', 3), isNull);
      expect(ctx.earlyAssignmentCount, 0,
          reason: 'the repeat found its entry removed: dropped, not early');
      expect(ctx.mp.totalLength, 1, reason: 'one acknowledgement, not two');

      // An early value — its entry not yet made — is still held.
      ctx.handleMadAssignment(
        globalName: GlobalName.reader('p', 4),
        value: ConstTerm(43),
        fromAgent: 'p',
      );
      expect(ctx.earlyAssignmentCount, 1);
    });

    test('a writer-name value delivered twice is applied once', () {
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'p', runtime: runtime);
      final (writerAddr, _) = runtime.heap.allocateVariable();
      final index = ctx.wp.addGlobalizeEntry(writerAddr, 'q');
      for (final v in [42, 43]) {
        ctx.handleMadAssignment(
          globalName: GlobalName.writer('p', index),
          value: ConstTerm(v),
          fromAgent: 'q',
        );
      }
      expect((runtime.heap.derefAddr(writerAddr) as ConstTerm).value, 42);
      expect(ctx.wp.lookupByIndex(index), isNull);
      expect(ctx.heldAssignmentCount, 0);
      expect(ctx.reportedLinkCount, 0);
    });

    test('a serializer message without a serial is refused, and a serial on '
        'another message', () {
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'q', runtime: runtime);
      final (netW, _) = runtime.heap.allocateVariable();
      ctx.wp.initializeSerializerEntry(netW);
      expect(
          () => ctx.handleMadAssignment(
              globalName: GlobalName.writer('q', 0),
              value: ConstTerm('x'),
              fromAgent: 'p'),
          throwsArgumentError);
      expect(
          () => ctx.handleMadAssignment(
              globalName: GlobalName.writer('q', 4),
              value: ConstTerm('x'),
              fromAgent: 'p',
              serial: 1),
          throwsArgumentError);
      expect(runtime.heap.derefAddr(netW), isNot(isA<StructTerm>()));
    });
  });

  group('Send Transaction', () {
    test('flushMessages sends queued messages', () {
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'p', runtime: runtime);

      // Add a message to the queue
      ctx.mp.add(OutboundMessage(
        destination: 'q',
        type: MessageType.assignment,
        payload: [1, 2, 3],
      ));

      final sent = <(String, OutboundMessage)>[];
      ctx.onMessageReady = (dest, msg) {
        sent.add((dest, msg));
      };

      final count = ctx.flushMessages();
      expect(count, 1);
      expect(sent.length, 1);
      expect(sent[0].$1, 'q');
      expect(sent[0].$2.type, MessageType.assignment);
    });
  });

  group('Direct Communication Scenario', () {
    test('p sends X to q, p assigns X := 1, q receives value', () {
      // Corrected definitions:
      // Globalize writer X at p → entry (X, q) at index i, no spawn
      // Localize _w(p,i) at q → fresh pair (Y_q, Y_q?), spawn gs, use Y_q (writer)
      // q assigns Y_q → gs fires → sends _w(p,i) := T to p → p binds X
      //
      // Wait — this is the REVERSE direction. "p sends X to q" means q gets the
      // writer and can assign it, sending the value back to p.
      //
      // For the "p assigns, q receives" direction, p must send X? (reader):
      // Globalize reader X? at p → spawn gs(X?, _r(p,i), q), no entry
      // Localize _r(p,i) at q → fresh pair (Z_q, Z_q?), entry (Z_q, p, i), use Z_q? (reader)
      // p assigns X → gs fires → sends _r(p,i) := T to q → q binds Z_q

      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates variable (X, X?)
      final (writerXp, readerXp) = runtimeP.heap.allocateVariable();

      // Globalize X? (reader) at p for q:
      // - spawns global_send(X?, _r(p,1), q)
      // - no entry
      final globalizeResult = globalize(
        variables: [TermVar.reader(readerXp, writerAddr: writerXp)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
        records: ctxP.up,
      );

      expect(globalizeResult.spawns.length, 1);
      expect(ctxP.wp.globalizeEntryCount, 0); // No entry for reader

      // Register the global_send goal at p
      ctxP.registerGlobalSendSpawns(globalizeResult.spawns);

      // Localize _r(p,1) at q:
      // - creates fresh pair (Z_q, Z_q?)
      // - adds entry (Z_q, p, 1)
      // - returns Z_q? (reader)
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        fromAgent: 'p',
        table: ctxQ.wp,
        records: ctxQ.up,
        freshAddrAllocator: () => runtimeQ.heap.allocateVariable(),
      );

      expect(localizeResult.useReader[0], true); // q gets reader
      final writerZq = localizeResult.freshPairs[0].writerAddr;

      // Setup message routing: p -> q
      ctxP.onMessageReady = (dest, msg) {
        if (dest == 'q') {
          // q receives the message _r(p,1) := 1
          ctxQ.handleMadAssignment(
            globalName: globalizeResult.globalNames[0],
            value: ConstTerm(1),
            fromAgent: 'p',
          );
        }
      };

      // p assigns X := 1
      runtimeP.heap.bindVariable(writerXp, ConstTerm(1));

      // This should trigger the global_send goal
      ctxP.onWriterBound(writerXp, ConstTerm(1));
      ctxP.flushMessages();

      // Verify q received the value (Z_q bound to 1)
      final derefed = runtimeQ.heap.derefAddr(writerZq);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 1);
    });
  });

  group('Return Value Scenario', () {
    test('p sends V? to q, q assigns V := result, p receives result', () {
      // Corrected definitions:
      // Globalize writer V at p → entry (V, q) at index i, no spawn
      // Localize _w(p,i) at q → fresh pair (Y_q, Y_q?), spawn gs, use Y_q (writer)
      // q assigns Y_q := 42 → gs fires → sends _w(p,i) := 42 to p → p binds V

      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates variable (V, V?)
      final (writerVp, readerVp) = runtimeP.heap.allocateVariable();

      // Globalize V (writer) at p for q:
      // - creates entry (V, q) at index 1
      // - no spawn
      final globalizeResult = globalize(
        variables: [TermVar.writer(writerVp, readerAddr: readerVp)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
        records: ctxP.up,
      );

      expect(ctxP.wp.lookupByIndex(1), isNotNull); // Entry created at index 1
      expect(globalizeResult.spawns, isEmpty); // No spawn for writer

      // Localize _w(p,1) at q:
      // - creates fresh pair (Y_q, Y_q?)
      // - spawns global_send(Y_q?, _w(p,1), p)
      // - returns Y_q (writer)
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        fromAgent: 'p',
        table: ctxQ.wp,
        records: ctxQ.up,
        freshAddrAllocator: () => runtimeQ.heap.allocateVariable(),
      );

      expect(localizeResult.useReader[0], false); // q gets writer
      final writerYq = localizeResult.freshPairs[0].writerAddr;

      // Register the global_send goal at q
      ctxQ.registerGlobalSendSpawns(localizeResult.spawns);

      // Setup message routing: q -> p
      ctxQ.onMessageReady = (dest, msg) {
        if (dest == 'p') {
          // p receives the return value _w(p,1) := 42
          ctxP.handleMadAssignment(
            globalName: globalizeResult.globalNames[0], // _w(p,1)
            value: ConstTerm(42),
            fromAgent: 'q',
          );
        }
      };

      // q assigns Y_q := 42 (the result)
      runtimeQ.heap.bindVariable(writerYq, ConstTerm(42));

      // This should trigger the global_send goal at q
      ctxQ.onWriterBound(writerYq, ConstTerm(42));
      ctxQ.flushMessages();

      // Verify p received the result (V is now bound to 42)
      final derefed = runtimeP.heap.derefAddr(writerVp);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 42);
    });
  });
}
