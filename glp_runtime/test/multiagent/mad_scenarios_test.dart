/// End-to-End madGLP Scenario Tests
///
/// Validates the complete madGLP implementation with realistic multi-agent
/// scenarios from the spec Section 10.
///
/// See: madGLP-spec.md Sections 5.4, 10.1-10.3

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/mad_context.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/multiagent/global_send.dart';

void main() {
  group('Section 10.1: Direct Communication (Client-Monitor)', () {
    test('p sends stream X to q, p assigns X := [add|Xs1], q receives', () {
      // Corrected definitions:
      // p sends Xs? (reader) to q. p assigns Xs, gs fires, q receives.
      // Globalize reader Xs? → spawn gs(Xs?, _r(p,1), q), no entry
      // Localize _r(p,1) at q → entry (Z_q, p, 1), use Z_q? (reader)

      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates stream variable (Xs, Xs?)
      final (writerXs, readerXs) = runtimeP.heap.allocateVariable();
      // p also creates Xs1 for the tail
      final (writerXs1, readerXs1) = runtimeP.heap.allocateVariable();

      // Globalize Xs? (reader) at p for q:
      // - spawns global_send(Xs?, _r(p,1), q)
      // - no entry
      final globalizeResult = globalize(
        variables: [TermVar.reader(readerXs, writerAddr: writerXs)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
      );
      expect(globalizeResult.spawns.length, 1); // spawn for reader
      expect(ctxP.wp.globalizeEntryCount, 0); // no entry
      ctxP.registerGlobalSendSpawns(globalizeResult.spawns);

      // q localizes _r(p,1)
      // - entry (Z_q, p, 1)
      // - returns Z_q? (reader)
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        table: ctxQ.wp,
        freshAddrAllocator: () => runtimeQ.heap.allocateVariable(),
      );
      expect(localizeResult.useReader[0], true); // q gets reader
      final writerZq = localizeResult.freshPairs[0].writerAddr;

      // Message routing: p -> q
      ctxP.onMessageReady = (dest, msg) {
        if (dest == 'q') {
          // q receives _r(p,1) := [add|Xs1]
          ctxQ.handleMadAssignment(
            globalName: globalizeResult.globalNames[0], // _r(p,1)
            value: StructTerm('.', [ConstTerm('add'), VarRef(readerXs1)]),
            fromAgent: 'p',
          );
        }
      };

      // p assigns Xs := [add|Xs1] (represented as cons cell)
      final streamValue = StructTerm('.', [ConstTerm('add'), VarRef(readerXs1)]);
      runtimeP.heap.bindVariable(writerXs, streamValue);

      // Trigger the global_send goal
      ctxP.onWriterBound(writerXs, streamValue);
      ctxP.flushMessages();

      // Verify q received [add|...]
      final derefed = runtimeQ.heap.derefAddr(writerZq);
      expect(derefed, isA<StructTerm>());
      final list = derefed as StructTerm;
      expect(list.functor, '.');
      expect(list.args[0], isA<ConstTerm>());
      expect((list.args[0] as ConstTerm).value, 'add');
    });
  });

  group('Section 10.2: Return Value Scenario', () {
    test('p sends [value(V?)|...] to q, q assigns V_q := Sum, p receives Sum', () {
      // Corrected definitions:
      // p sends V (writer) to q. q assigns V_q, gs fires, p receives.
      // Globalize writer V → entry (V, q) at index 1, no spawn
      // Localize _w(p,1) at q → spawn gs(V_q?, _w(p,1), p), use V_q (writer)

      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates V (for return value)
      final (writerV, readerV) = runtimeP.heap.allocateVariable();

      // p globalizes V (writer) - creates entry, no spawn
      final globalizeResult = globalize(
        variables: [TermVar.writer(writerV, readerAddr: readerV)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
      );

      expect(ctxP.wp.lookupByIndex(1), isNotNull); // Entry at index 1
      expect(globalizeResult.spawns, isEmpty); // No spawn for writer

      // q localizes _w(p,1) - gets writer, spawns global_send
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        table: ctxQ.wp,
        freshAddrAllocator: () => runtimeQ.heap.allocateVariable(),
      );
      ctxQ.registerGlobalSendSpawns(localizeResult.spawns);

      expect(localizeResult.useReader[0], false); // q gets writer
      final writerVq = localizeResult.freshPairs[0].writerAddr;

      // Message routing: q -> p
      ctxQ.onMessageReady = (dest, msg) {
        if (dest == 'p') {
          ctxP.handleMadAssignment(
            globalName: globalizeResult.globalNames[0], // _w(p,1)
            value: ConstTerm(100), // Sum = 100
            fromAgent: 'q',
          );
        }
      };

      // q assigns V_q := 100 (the sum)
      runtimeQ.heap.bindVariable(writerVq, ConstTerm(100));

      // Trigger the global_send goal at q
      ctxQ.onWriterBound(writerVq, ConstTerm(100));
      ctxQ.flushMessages();

      // Verify p received the return value
      final derefed = runtimeP.heap.derefAddr(writerV);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 100);
    });
  });

  group('Section 10.3: Friend-Mediated Introduction', () {
    test('Bob forwards X from Alice to Charlie, Charlie assigns, Alice receives', () {
      // Corrected scenario per spec Section 10.3:
      // - Bob creates (X, X?)
      // - Bob sends X? (reader) to Alice → gs at Bob, entry at Alice
      // - Bob sends X (writer) to Charlie → entry at Bob, gs at Charlie
      // - Charlie assigns X_c := T
      // - Charlie's gs fires → sends _w(bob,1) := T to Bob
      // - Bob binds X, X? becomes known
      // - Bob's gs fires → sends _r(bob,2) := T to Alice
      // - Alice binds Z, Z? becomes known

      final runtimeAlice = GlpRuntime();
      final runtimeBob = GlpRuntime();
      final runtimeCharlie = GlpRuntime();
      final ctxAlice = MadContext(agentId: 'alice', runtime: runtimeAlice);
      final ctxBob = MadContext(agentId: 'bob', runtime: runtimeBob);
      final ctxCharlie = MadContext(agentId: 'charlie', runtime: runtimeCharlie);

      // Bob creates X
      final (writerXBob, readerXBob) = runtimeBob.heap.allocateVariable();

      // === Bob -> Alice: Bob globalizes X? (reader) ===
      // Corrected: reader → spawn gs(X?, _r(bob,1), alice), no entry
      final bobToAliceGlobal = globalize(
        variables: [TermVar.reader(readerXBob, writerAddr: writerXBob)],
        localAgent: 'bob',
        remoteAgent: 'alice',
        table: ctxBob.wp,
      );
      expect(bobToAliceGlobal.spawns.length, 1); // gs for reader
      expect(ctxBob.wp.globalizeEntryCount, 0); // no entry
      ctxBob.registerGlobalSendSpawns(bobToAliceGlobal.spawns);

      // Alice localizes _r(bob,1) → entry (Z_a, bob, 1), gets Z_a? (reader)
      final aliceFromBob = localize(
        globalNames: bobToAliceGlobal.globalNames,
        localAgent: 'alice',
        table: ctxAlice.wp,
        freshAddrAllocator: () => runtimeAlice.heap.allocateVariable(),
      );
      expect(aliceFromBob.useReader[0], true); // Alice gets reader
      expect(aliceFromBob.spawns, isEmpty); // no spawn for _r
      final writerZAlice = aliceFromBob.freshPairs[0].writerAddr;

      // === Bob -> Charlie: Bob globalizes X (writer) ===
      // Corrected: writer → entry (X, charlie) at index 2, no spawn
      final bobToCharlieGlobal = globalize(
        variables: [TermVar.writer(writerXBob, readerAddr: readerXBob)],
        localAgent: 'bob',
        remoteAgent: 'charlie',
        table: ctxBob.wp,
      );
      expect(bobToCharlieGlobal.spawns, isEmpty); // No spawn for writer
      expect(ctxBob.wp.globalizeEntryCount, 1); // entry at index 2

      // Charlie localizes _w(bob,2) → spawn gs(Y_c?, _w(bob,2), bob), gets Y_c (writer)
      final charlieFromBob = localize(
        globalNames: bobToCharlieGlobal.globalNames,
        localAgent: 'charlie',
        table: ctxCharlie.wp,
        freshAddrAllocator: () => runtimeCharlie.heap.allocateVariable(),
      );
      ctxCharlie.registerGlobalSendSpawns(charlieFromBob.spawns);

      expect(charlieFromBob.useReader[0], false); // Charlie gets writer
      expect(charlieFromBob.spawns.length, 1); // Spawn for _w(bob,2)
      final writerYCharlie = charlieFromBob.freshPairs[0].writerAddr;

      // === Message routing ===
      // Charlie -> Bob: _w(bob,2) := T
      // Bob -> Alice: _r(bob,1) := T (after Bob's gs fires)

      ctxCharlie.onMessageReady = (dest, msg) {
        if (dest == 'bob') {
          // Bob receives _w(bob,2) := T → GlobalizeEntry lookup
          ctxBob.handleMadAssignment(
            globalName: bobToCharlieGlobal.globalNames[0], // _w(bob,2)
            value: ConstTerm('hello_from_charlie'),
            fromAgent: 'charlie',
          );
          // handleMadAssignment binds writerXBob
          // Now Bob's global_send(X?, _r(bob,1), alice) should fire
          ctxBob.onWriterBound(writerXBob, ConstTerm('hello_from_charlie'));
          ctxBob.flushMessages();
        }
      };

      ctxBob.onMessageReady = (dest, msg) {
        if (dest == 'alice') {
          // Alice receives _r(bob,1) := T → LocalizeEntry search
          ctxAlice.handleMadAssignment(
            globalName: bobToAliceGlobal.globalNames[0], // _r(bob,1)
            value: ConstTerm('hello_from_charlie'),
            fromAgent: 'bob',
          );
        }
      };

      // Charlie assigns Y_c := 'hello_from_charlie'
      runtimeCharlie.heap.bindVariable(writerYCharlie, ConstTerm('hello_from_charlie'));
      ctxCharlie.onWriterBound(writerYCharlie, ConstTerm('hello_from_charlie'));
      ctxCharlie.flushMessages();

      // Verify Alice received the value
      final derefed = runtimeAlice.heap.derefAddr(writerZAlice);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 'hello_from_charlie');
    });
  });

  group('Section 5.4: Both Ends Exported', () {
    test('p exports [X, X?] to q, q assigns Y_q := T, T flows back to p', () {
      // Corrected definitions:
      // Globalize X (writer): entry (X, q) at index 1, no spawn
      // Globalize X? (reader): spawn gs(X?, _r(p,2), q), no entry
      //
      // Localize _w(p,1) at q: spawn gs(Y_q?, _w(p,1), p), use Y_q (writer)
      // Localize _r(p,2) at q: entry (Z_q, p, 2), use Z_q? (reader)
      //
      // q's term: [Y_q, Z_q?]
      // q assigns Y_q → gs fires → _w(p,1) := T to p → p binds X
      // X? becomes known → p's gs fires → _r(p,2) := T to q → q binds Z_q

      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates X (and X?)
      final (writerX, readerX) = runtimeP.heap.allocateVariable();

      // p globalizes both X and X?
      // Writer X: entry (X, q) at index 1
      // Reader X?: spawn gs(X?, _r(p,2), q)
      final globalizeResult = globalize(
        variables: [TermVar.writer(writerX, readerAddr: readerX), TermVar.reader(readerX, writerAddr: writerX)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
      );
      ctxP.registerGlobalSendSpawns(globalizeResult.spawns);

      expect(globalizeResult.globalNames.length, 2);
      expect(globalizeResult.globalNames[0], GlobalName.writer('p', 1)); // _w(p,1)
      expect(globalizeResult.globalNames[1], GlobalName.reader('p', 2)); // _r(p,2)
      expect(globalizeResult.spawns.length, 1); // One spawn for reader X?
      expect(ctxP.wp.globalizeEntryCount, 1); // One entry for writer X

      // q localizes [_w(p,1), _r(p,2)]
      // _w(p,1): spawn gs(Y_q?, _w(p,1), p), use Y_q (writer)
      // _r(p,2): entry (Z_q, p, 2), use Z_q? (reader)
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        table: ctxQ.wp,
        freshAddrAllocator: () => runtimeQ.heap.allocateVariable(),
      );
      ctxQ.registerGlobalSendSpawns(localizeResult.spawns);

      expect(localizeResult.useReader[0], false); // Y_q (writer) for _w
      expect(localizeResult.useReader[1], true);  // Z_q? (reader) for _r
      expect(localizeResult.spawns.length, 1);    // One spawn for _w(p,1)

      final writerYq = localizeResult.freshPairs[0].writerAddr; // For _w(p,1)
      final writerZq = localizeResult.freshPairs[1].writerAddr; // For _r(p,2)

      // Message routing:
      // q -> p: _w(p,1) := T (q assigns Y_q, gs fires)
      // p -> q: _r(p,2) := T (p's gs fires after X? becomes known)

      ctxQ.onMessageReady = (dest, msg) {
        if (dest == 'p') {
          // p receives _w(p,1) := T → GlobalizeEntry at index 1
          ctxP.handleMadAssignment(
            globalName: globalizeResult.globalNames[0], // _w(p,1)
            value: ConstTerm('value_from_q'),
            fromAgent: 'q',
          );
          // This binds X at p. X? becomes known.
          // p's gs(X?, _r(p,2), q) should fire.
          ctxP.onWriterBound(writerX, ConstTerm('value_from_q'));
          ctxP.flushMessages();
        }
      };

      ctxP.onMessageReady = (dest, msg) {
        if (dest == 'q') {
          // q receives _r(p,2) := T → LocalizeEntry search for (p, 2)
          ctxQ.handleMadAssignment(
            globalName: globalizeResult.globalNames[1], // _r(p,2)
            value: ConstTerm('value_from_q'),
            fromAgent: 'p',
          );
        }
      };

      // q assigns Y_q := 'value_from_q'
      runtimeQ.heap.bindVariable(writerYq, ConstTerm('value_from_q'));
      ctxQ.onWriterBound(writerYq, ConstTerm('value_from_q'));
      ctxQ.flushMessages();

      // Verify p received the value (X is now bound)
      final derefedP = runtimeP.heap.derefAddr(writerX);
      expect(derefedP, isA<ConstTerm>());
      expect((derefedP as ConstTerm).value, 'value_from_q');

      // Verify q also received the value via the forwarded _r link (Z_q bound)
      final derefedQ = runtimeQ.heap.derefAddr(writerZq);
      expect(derefedQ, isA<ConstTerm>());
      expect((derefedQ as ConstTerm).value, 'value_from_q');
    });
  });
}
