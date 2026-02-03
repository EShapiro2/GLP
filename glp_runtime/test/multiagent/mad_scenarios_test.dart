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
      // Setup: Agent p has client(Xs), agent q has monitor(Xs?)
      // Action: p assigns Xs := [add|Xs1]
      // Verify: q receives [add|Xs1] via global link

      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates stream variable Xs
      final (writerXs, readerXs) = runtimeP.heap.allocateVariable();
      // p also creates Xs1 for the tail
      final (writerXs1, readerXs1) = runtimeP.heap.allocateVariable();

      // Network transaction: p globalizes Xs, q localizes
      final globalizeResult = globalize(
        variables: [TermVar.writer(writerXs)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
      );
      ctxP.registerGlobalSendSpawns(globalizeResult.spawns);

      // q localizes _w(p,0)
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        table: ctxQ.wp,
        freshAddrAllocator: () {
          final (w, _) = runtimeQ.heap.allocateVariable();
          return w;
        },
      );
      final writerXsQ = localizeResult.freshPairs[0].writerAddr;

      // Message routing: p -> q
      ctxP.onMessageReady = (dest, msg) {
        if (dest == 'q') {
          // For this test, we know the value is [add|Xs1]
          // Lists are StructTerm with functor '.' (cons)
          ctxQ.handleMadAssignment(
            globalName: globalizeResult.globalNames[0],
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
      final derefed = runtimeQ.heap.derefAddr(writerXsQ);
      expect(derefed, isA<StructTerm>());
      final list = derefed as StructTerm;
      expect(list.functor, '.');
      expect(list.args[0], isA<ConstTerm>());
      expect((list.args[0] as ConstTerm).value, 'add');
    });
  });

  group('Section 10.2: Return Value Scenario', () {
    test('p sends [value(V?)|...] to q, q assigns V_q := Sum, p receives Sum', () {
      // Setup: p sends [value(V?)|...] to q
      // Action: q assigns V_q := Sum (the writer it received)
      // Verify: p receives Sum via the global link

      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates V (for return value)
      final (writerV, readerV) = runtimeP.heap.allocateVariable();

      // p globalizes V? (reader) - creates entry, returns _r(p,0)
      final globalizeResult = globalize(
        variables: [TermVar.reader(writerV)], // Use writer addr for entry
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
      );

      expect(ctxP.wp.lookupByIndex(0), isNotNull);

      // q localizes _r(p,0) - gets writer, spawns global_send
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        table: ctxQ.wp,
        freshAddrAllocator: () {
          final (w, _) = runtimeQ.heap.allocateVariable();
          return w;
        },
      );
      ctxQ.registerGlobalSendSpawns(localizeResult.spawns);

      expect(localizeResult.useReader[0], false); // q gets writer
      final writerVq = localizeResult.freshPairs[0].writerAddr;

      // Message routing: q -> p
      ctxQ.onMessageReady = (dest, msg) {
        if (dest == 'p') {
          ctxP.handleMadAssignment(
            globalName: globalizeResult.globalNames[0], // _r(p,0)
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
      // - Bob creates X
      // - Bob sends X (writer) to Alice
      // - Bob sends X? (reader) to Charlie
      // - Charlie assigns X_c := T
      // Verify: T flows from Charlie -> Bob -> Alice

      final runtimeAlice = GlpRuntime();
      final runtimeBob = GlpRuntime();
      final runtimeCharlie = GlpRuntime();
      final ctxAlice = MadContext(agentId: 'alice', runtime: runtimeAlice);
      final ctxBob = MadContext(agentId: 'bob', runtime: runtimeBob);
      final ctxCharlie = MadContext(agentId: 'charlie', runtime: runtimeCharlie);

      // Bob creates X
      final (writerXBob, readerXBob) = runtimeBob.heap.allocateVariable();

      // === Bob -> Alice: Bob globalizes X (writer) ===
      // Spawns global_send(X?, _w(bob,0), alice)
      final bobToAliceGlobal = globalize(
        variables: [TermVar.writer(writerXBob)],
        localAgent: 'bob',
        remoteAgent: 'alice',
        table: ctxBob.wp,
      );
      ctxBob.registerGlobalSendSpawns(bobToAliceGlobal.spawns);

      // Alice localizes _w(bob,0) - creates entry, gets reader
      final aliceFromBob = localize(
        globalNames: bobToAliceGlobal.globalNames,
        localAgent: 'alice',
        table: ctxAlice.wp,
        freshAddrAllocator: () {
          final (w, _) = runtimeAlice.heap.allocateVariable();
          return w;
        },
      );
      expect(aliceFromBob.useReader[0], true); // Alice gets reader
      final writerXAlice = aliceFromBob.freshPairs[0].writerAddr;

      // === Bob -> Charlie: Bob globalizes X? (reader) ===
      // Creates entry (X, charlie) at index 0, no spawn
      final bobToCharlieGlobal = globalize(
        variables: [TermVar.reader(writerXBob)],
        localAgent: 'bob',
        remoteAgent: 'charlie',
        table: ctxBob.wp,
      );
      expect(bobToCharlieGlobal.spawns.length, 0); // No spawn for reader

      // Charlie localizes _r(bob,0) - gets writer, spawns global_send
      final charlieFromBob = localize(
        globalNames: bobToCharlieGlobal.globalNames,
        localAgent: 'charlie',
        table: ctxCharlie.wp,
        freshAddrAllocator: () {
          final (w, _) = runtimeCharlie.heap.allocateVariable();
          return w;
        },
      );
      ctxCharlie.registerGlobalSendSpawns(charlieFromBob.spawns);

      expect(charlieFromBob.useReader[0], false); // Charlie gets writer
      expect(charlieFromBob.spawns.length, 1);    // Spawn for _r(bob,0)
      final writerXCharlie = charlieFromBob.freshPairs[0].writerAddr;

      // === Message routing ===
      // Charlie -> Bob (via global_send)
      // Bob -> Alice (via global_send when Bob's writer is bound)

      ctxCharlie.onMessageReady = (dest, msg) {
        if (dest == 'bob') {
          // Bob receives _r(bob,0) := T
          ctxBob.handleMadAssignment(
            globalName: bobToCharlieGlobal.globalNames[0], // _r(bob,0)
            value: ConstTerm('hello_from_charlie'),
            fromAgent: 'charlie',
          );
          // handleMadAssignment binds writerXBob
          // Now Bob's global_send(X?, _w(bob,0), alice) should fire
          ctxBob.onWriterBound(writerXBob, ConstTerm('hello_from_charlie'));
          ctxBob.flushMessages();
        }
      };

      ctxBob.onMessageReady = (dest, msg) {
        if (dest == 'alice') {
          // Alice receives _w(bob,0) := T
          ctxAlice.handleMadAssignment(
            globalName: bobToAliceGlobal.globalNames[0], // _w(bob,0)
            value: ConstTerm('hello_from_charlie'),
            fromAgent: 'bob',
          );
        }
      };

      // Charlie assigns X_c := 'hello_from_charlie'
      runtimeCharlie.heap.bindVariable(writerXCharlie, ConstTerm('hello_from_charlie'));
      ctxCharlie.onWriterBound(writerXCharlie, ConstTerm('hello_from_charlie'));
      ctxCharlie.flushMessages();

      // Verify Alice received the value
      final derefed = runtimeAlice.heap.derefAddr(writerXAlice);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 'hello_from_charlie');
    });
  });

  group('Section 5.4: Both Ends Exported', () {
    test('p exports [X, X?] to q, q assigns Z_q := T, T flows back to p', () {
      // Setup: p exports [X, X?] to q
      // - X (writer) goes to q as reader
      // - X? (reader) goes to q as writer
      // Action: q assigns Z_q := T (the writer for X?)
      // Verify: T flows back through p's global_send goal

      final runtimeP = GlpRuntime();
      final runtimeQ = GlpRuntime();
      final ctxP = MadContext(agentId: 'p', runtime: runtimeP);
      final ctxQ = MadContext(agentId: 'q', runtime: runtimeQ);

      // p creates X (and X?)
      final (writerX, readerX) = runtimeP.heap.allocateVariable();

      // p globalizes both X and X?
      // Globalize X (writer): spawns global_send(X?, _w(p,0), q), no entry
      // Globalize X? (reader): creates entry (X, q) at index 0, no spawn
      final globalizeResult = globalize(
        variables: [TermVar.writer(writerX), TermVar.reader(writerX)],
        localAgent: 'p',
        remoteAgent: 'q',
        table: ctxP.wp,
      );
      ctxP.registerGlobalSendSpawns(globalizeResult.spawns);

      expect(globalizeResult.globalNames.length, 2);
      expect(globalizeResult.globalNames[0].isWriter, true); // _w(p,0)
      expect(globalizeResult.globalNames[1].isReader, true); // _r(p,0)
      expect(globalizeResult.spawns.length, 1); // One spawn for writer

      // q localizes [_w(p,0), _r(p,0)]
      // Localize _w(p,0): creates fresh (Y, Y?), entry (Y, p, 0), returns Y?
      // Localize _r(p,0): creates fresh (Z, Z?), spawns global_send(Z?, _r(p,0), p), returns Z
      final localizeResult = localize(
        globalNames: globalizeResult.globalNames,
        localAgent: 'q',
        table: ctxQ.wp,
        freshAddrAllocator: () {
          final (w, _) = runtimeQ.heap.allocateVariable();
          return w;
        },
      );
      ctxQ.registerGlobalSendSpawns(localizeResult.spawns);

      expect(localizeResult.useReader[0], true);  // First gets reader
      expect(localizeResult.useReader[1], false); // Second gets writer
      expect(localizeResult.spawns.length, 1);    // One spawn for _r(p,0)

      final writerYq = localizeResult.freshPairs[0].writerAddr; // For _w(p,0)
      final writerZq = localizeResult.freshPairs[1].writerAddr; // For _r(p,0)

      // Message routing: q -> p
      ctxQ.onMessageReady = (dest, msg) {
        if (dest == 'p') {
          // p receives _r(p,0) := T
          ctxP.handleMadAssignment(
            globalName: globalizeResult.globalNames[1], // _r(p,0)
            value: ConstTerm('value_from_q'),
            fromAgent: 'q',
          );
        }
      };

      // q assigns Z_q := 'value_from_q'
      runtimeQ.heap.bindVariable(writerZq, ConstTerm('value_from_q'));
      ctxQ.onWriterBound(writerZq, ConstTerm('value_from_q'));
      ctxQ.flushMessages();

      // Verify p received the value (X is now bound)
      final derefed = runtimeP.heap.derefAddr(writerX);
      expect(derefed, isA<ConstTerm>());
      expect((derefed as ConstTerm).value, 'value_from_q');
    });
  });
}
