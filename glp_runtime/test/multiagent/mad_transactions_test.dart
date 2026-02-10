/// Tests for madGLP transaction handling
///
/// Validates end-to-end message flow using the push-based model.
///
/// See: madGLP-spec.md Sections 8.1-8.4

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

    test('receive for non-existent GlobalizeEntry throws', () {
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'p', runtime: runtime);

      // No entry exists at index 5
      expect(
        () => ctx.handleMadAssignment(
          globalName: GlobalName.writer('p', 5),
          value: ConstTerm(42),
          fromAgent: 'q',
        ),
        throwsStateError,
      );
    });

    test('receive for non-existent LocalizeEntry throws', () {
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'q', runtime: runtime);

      // No entry exists for _r(p, 5)
      expect(
        () => ctx.handleMadAssignment(
          globalName: GlobalName.reader('p', 5),
          value: ConstTerm(42),
          fromAgent: 'p',
        ),
        throwsStateError,
      );
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
        table: ctxQ.wp,
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
        table: ctxQ.wp,
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
