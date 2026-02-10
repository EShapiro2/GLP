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
    test('_w(p,i) message: finds LocalizeEntry, binds writer', () {
      // Given: Agent q has a LocalizeEntry (X_q, p, 0) from localizing _w(p,0)
      // When: q receives message _w(p,0) := 42
      // Then: X_q is bound to 42, entry removed

      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'q', runtime: runtime);

      // Create a local pair that will receive the assignment
      final (writerAddr, readerAddr) = runtime.heap.allocateVariable();

      // Add LocalizeEntry: this simulates having localized _w(p, 0)
      ctx.wp.addLocalizeEntry(writerAddr, 'p', 0);

      // Receive the assignment message
      final globalName = GlobalName.writer('p', 0);
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
      expect(ctx.wp.findByRemote('p', 0), isNull);
    });

    test('_r(p,i) message: finds GlobalizeEntry by index, binds writer', () {
      // Given: Agent p has a GlobalizeEntry (X, q) at index 0 from globalizing Y?
      // When: p receives message _r(p,0) := 42 from q
      // Then: X is bound to 42, entry removed

      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'p', runtime: runtime);

      // Create a local pair (X, X?) where X is the writer
      final (writerAddr, readerAddr) = runtime.heap.allocateVariable();

      // Add GlobalizeEntry: this simulates having globalized X?
      // When we globalized X?, we created entry (X, q) at some index
      ctx.wp.addGlobalizeEntry(writerAddr, 'q');
      // The entry is at index 0

      // Receive the assignment message from q
      final globalName = GlobalName.reader('p', 0);
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
      expect(ctx.wp.lookupByIndex(0), isNull);
    });

    test('receive localizes nested variables', () {
      // Given: Agent q receives message with nested global names
      // When: value contains _w(p,1) (another global name)
      // Then: Fresh pair created, entry added, spawns registered

      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'q', runtime: runtime);

      // Setup: q has LocalizeEntry for receiving main message
      final (writerAddr, _) = runtime.heap.allocateVariable();
      ctx.wp.addLocalizeEntry(writerAddr, 'p', 0);

      // The value contains a nested global name _w(p,1) (another writer from p)
      // This would normally be serialized/deserialized, but for unit test
      // we simulate by passing nested global names directly
      final nestedGlobalNames = [GlobalName.writer('p', 1)];

      ctx.handleMadAssignmentWithGlobalNames(
        globalName: GlobalName.writer('p', 0),
        value: ConstTerm('placeholder'), // Will be replaced
        nestedGlobalNames: nestedGlobalNames,
        fromAgent: 'p',
      );

      // Verify LocalizeEntry was created for nested _w(p,1)
      expect(ctx.wp.findByRemote('p', 1), isNotNull);
    });

    test('receive for non-existent LocalizeEntry throws', () {
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'q', runtime: runtime);

      // No entry exists for _w(p, 5)
      expect(
        () => ctx.handleMadAssignment(
          globalName: GlobalName.writer('p', 5),
          value: ConstTerm(42),
          fromAgent: 'p',
        ),
        throwsStateError,
      );
    });

    test('receive for non-existent GlobalizeEntry throws', () {
      final runtime = GlpRuntime();
      final ctx = MadContext(agentId: 'p', runtime: runtime);

      // No entry exists at index 5
      expect(
        () => ctx.handleMadAssignment(
          globalName: GlobalName.reader('p', 5),
          value: ConstTerm(42),
          fromAgent: 'q',
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
      // Setup two agents
      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates variable X
      final (writerXp, readerXp) = runtimeP.heap.allocateVariable();

      // Network transaction: p globalizes X, q localizes
      // Globalize X (writer) at p for q:
      // - spawns global_send(X?, _w(p,0), q)
      // - no entry
      final globalizeResult = globalize(
        variables: [TermVar.writer(writerXp, readerAddr: readerXp)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
      );

      // Register the global_send goal at p
      ctxP.registerGlobalSendSpawns(globalizeResult.spawns);

      // Localize _w(p,0) at q:
      // - creates fresh pair (X_q, X_q?)
      // - adds entry (X_q, p, 0)
      // - returns X_q? (reader)
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        table: ctxQ.wp,
        freshAddrAllocator: () => runtimeQ.heap.allocateVariable(),
      );

      expect(localizeResult.useReader[0], true); // q gets reader
      final writerXq = localizeResult.freshPairs[0].writerAddr;

      // Setup message routing
      ctxP.onMessageReady = (dest, msg) {
        if (dest == 'q') {
          // q receives the message
          ctxQ.handleMadAssignment(
            globalName: globalizeResult.globalNames[0],
            value: ConstTerm(1), // The value that was sent
            fromAgent: 'p',
          );
        }
      };

      // p assigns X := 1
      runtimeP.heap.bindVariable(writerXp, ConstTerm(1));

      // This should trigger the global_send goal
      ctxP.onWriterBound(writerXp, ConstTerm(1));
      ctxP.flushMessages();

      // Verify q received the value
      final derefed = runtimeQ.heap.derefAddr(writerXq);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 1);
    });
  });

  group('Return Value Scenario', () {
    test('p sends V? to q, q assigns V := result, p receives result', () {
      // Setup two agents
      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates variable V (with V?)
      final (writerVp, readerVp) = runtimeP.heap.allocateVariable();

      // Network transaction: p globalizes V?, q localizes
      // Globalize V? (reader) at p for q:
      // - creates entry (V, q) at index 0 where V is the WRITER address
      // - no spawn
      // TermVar.reader carries both addresses; writerAddr is stored in the entry
      final globalizeResult = globalize(
        variables: [TermVar.reader(readerVp, writerAddr: writerVp)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
      );

      expect(ctxP.wp.lookupByIndex(0), isNotNull); // Entry created

      // Localize _r(p,0) at q:
      // - creates fresh pair (V_q, V_q?)
      // - returns V_q (writer)
      // - spawns global_send(V_q?, _r(p,0), p)
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        table: ctxQ.wp,
        freshAddrAllocator: () => runtimeQ.heap.allocateVariable(),
      );

      expect(localizeResult.useReader[0], false); // q gets writer
      final writerVq = localizeResult.freshPairs[0].writerAddr;

      // Register the global_send goal at q
      ctxQ.registerGlobalSendSpawns(localizeResult.spawns);

      // Setup message routing: q -> p
      ctxQ.onMessageReady = (dest, msg) {
        if (dest == 'p') {
          // p receives the return value
          ctxP.handleMadAssignment(
            globalName: globalizeResult.globalNames[0], // _r(p,0)
            value: ConstTerm(42), // The result
            fromAgent: 'q',
          );
        }
      };

      // q assigns V_q := 42 (the result)
      runtimeQ.heap.bindVariable(writerVq, ConstTerm(42));

      // This should trigger the global_send goal at q
      ctxQ.onWriterBound(writerVq, ConstTerm(42));
      ctxQ.flushMessages();

      // Verify p received the result (V is now bound)
      final derefed = runtimeP.heap.derefAddr(writerVp);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 42);
    });
  });
}
