/// Unit tests for IrmaContext
/// 
/// Tests integration of V_p/M_p with GLP runtime
/// Updated for VarKey composite key architecture
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/irma_context.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';

void main() {
  group('IrmaContext - Initialization', () {
    test('creates context with agent ID', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      expect(ctx.agentId, 'alice');
      expect(ctx.vp.agentId, 'alice');
      expect(ctx.mp.isEmpty, isTrue);
    });
    
    test('multiple agents have independent contexts', () {
      final rtAlice = GlpRuntime();
      final rtBob = GlpRuntime();
      
      final alice = IrmaContext(agentId: 'alice', runtime: rtAlice);
      final bob = IrmaContext(agentId: 'bob', runtime: rtBob);
      
      // Add variable to alice's V_p
      alice.vp.add(VarKey(100, false), VariableEntry(
        varId: 100,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      // Bob's V_p should be unaffected
      expect(alice.vp.contains(VarKey(100, false)), isTrue);
      expect(bob.vp.contains(VarKey(100, false)), isFalse);
    });
  });
  
  group('IrmaContext - processReaderBindings', () {
    test('queues assignment when created reader has requester', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice created reader 100, bob requested it
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
        requester: 'bob', // bob requested
      ));
      
      // Reduce assigns reader 100 = hello
      final sigmaHatReader = {100: ConstTerm('hello')};
      ctx.processReaderBindings(sigmaHatReader);
      
      // Should queue message to bob
      expect(ctx.mp.countFor('bob'), 1);
      
      final msg = ctx.mp.poll('bob');
      expect(msg, isNotNull);
      expect(msg!.type, MessageType.assignment);
    });
    
    test('no message when created reader has no requester', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice created reader 100, no one requested yet
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
        // No requester (default null)
      ));
      
      final sigmaHatReader = {100: ConstTerm('hello')};
      ctx.processReaderBindings(sigmaHatReader);
      
      // No message should be queued
      expect(ctx.mp.isEmpty, isTrue);
    });
    
    test('updates state for imported reader that got assigned', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice imported reader 100 from bob, not yet requested
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        // requestSent defaults to false
      ));

      final sigmaHatReader = {100: ConstTerm('hello')};
      ctx.processReaderBindings(sigmaHatReader);

      // requestSent should be updated to true
      expect(ctx.vp.lookup(VarKey(100, true))!.requestSent, isTrue);
    });
    
    test('ignores variables not in V_p', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Variable 999 not in V_p
      final sigmaHatReader = {999: ConstTerm('hello')};
      ctx.processReaderBindings(sigmaHatReader);
      
      // No error, no messages
      expect(ctx.mp.isEmpty, isTrue);
    });
  });
  
  group('IrmaContext - processAbandonedReaders', () {
    test('abandons readers that disappear without binding', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice imported reader 100 from bob
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Reader 100 was in goal, not assigned, not in body
      ctx.processAbandonedReaders(
        readersInGoal: {100},
        assignedReaders: {},
        readersInBody: {},
      );
      
      // Should queue abandon message to bob
      expect(ctx.mp.countFor('bob'), 1);
      final msg = ctx.mp.poll('bob');
      expect(msg!.type, MessageType.abandon);
      
      // Should remove from V_p
      expect(ctx.vp.contains(VarKey(100, true)), isFalse);
    });
    
    test('does not abandon readers that were assigned', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Reader 100 was assigned
      ctx.processAbandonedReaders(
        readersInGoal: {100},
        assignedReaders: {100},
        readersInBody: {},
      );
      
      // No abandon message
      expect(ctx.mp.isEmpty, isTrue);
      // Still in V_p
      expect(ctx.vp.contains(VarKey(100, true)), isTrue);
    });
    
    test('does not abandon readers that appear in body', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Reader 100 appears in body
      ctx.processAbandonedReaders(
        readersInGoal: {100},
        assignedReaders: {},
        readersInBody: {100},
      );
      
      // No abandon message
      expect(ctx.mp.isEmpty, isTrue);
    });
  });
  
  group('IrmaContext - processSuspension', () {
    test('sends read requests for blocking imported readers', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice imported reader 100 from bob, not yet requested
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        // requestSent defaults to false
      ));

      // Goal suspends on reader 100
      ctx.processSuspension({100});

      // Should queue read request to bob
      expect(ctx.mp.countFor('bob'), 1);
      final msg = ctx.mp.poll('bob');
      expect(msg!.type, MessageType.readRequest);

      // requestSent should be updated to true
      expect(ctx.vp.lookup(VarKey(100, true))!.requestSent, isTrue);
    });
    
    test('does not send duplicate request', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice imported reader 100 from bob, already requested
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        requestSent: true, // Already requested
      ));
      
      ctx.processSuspension({100});
      
      // No new message (idempotent)
      expect(ctx.mp.isEmpty, isTrue);
    });
  });
  
  group('IrmaContext - processFailure', () {
    test('abandons all readers in failed goal', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice imported readers 100 and 101 from bob
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      ctx.vp.add(VarKey(101, true), VariableEntry(
        varId: 101,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Goal with readers 100 and 101 fails
      ctx.processFailure({100, 101});
      
      // Both should send abandon messages
      expect(ctx.mp.countFor('bob'), 2);
      
      // Both removed from V_p
      expect(ctx.vp.contains(VarKey(100, true)), isFalse);
      expect(ctx.vp.contains(VarKey(101, true)), isFalse);
    });
  });
  
  group('IrmaContext - flushMessages', () {
    test('delivers all messages via callback', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Queue messages to multiple destinations
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      ctx.vp.add(VarKey(101, true), VariableEntry(
        varId: 101,
        isReader: true,
        creator: 'charlie',
        role: VariableRole.importedReader,
      ));
      
      ctx.processFailure({100, 101});
      
      expect(ctx.mp.totalLength, 2);
      
      // Set up delivery callback
      final delivered = <(String, OutboundMessage)>[];
      ctx.onMessageReady = (dest, msg) => delivered.add((dest, msg));
      
      // Flush
      final count = ctx.flushMessages();
      
      expect(count, 2);
      expect(ctx.mp.isEmpty, isTrue);
      expect(delivered.length, 2);
    });
    
    test('returns 0 when no callback set', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      ctx.processFailure({100});
      
      // No callback
      final count = ctx.flushMessages();
      
      expect(count, 0);
      // Messages still in queue
      expect(ctx.mp.totalLength, 1);
    });
  });
  
  group('IrmaContext - importTerm', () {
    test('adds variables from term to V_p', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Receive term with variable from bob
      final term = StructTerm('msg', [
        ConstTerm('hello'),
        VarRef(201),
      ]);
      
      ctx.importTerm(term, 'bob');
      
      // Variable 200 should be in V_p as reader
      expect(ctx.vp.contains(VarKey(200, true)), isTrue);
      final entry = ctx.vp.lookup(VarKey(200, true));
      expect(entry!.creator, 'bob');
      expect(entry.role, VariableRole.importedReader);
    });
    
    test('does not duplicate existing variables', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Variable 200 already in V_p
      ctx.vp.add(VarKey(200, true), VariableEntry(
        varId: 200,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        requestSent: true, // Already requested
      ));

      final term = VarRef(201);
      ctx.importTerm(term, 'bob');

      // requestSent should be unchanged
      expect(ctx.vp.lookup(VarKey(200, true))!.requestSent, isTrue);
    });
  });
  
  group('IrmaContext - handleAssignment', () {
    test('applies assignment to heap and reactivates goals', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate variable
      final (varId, _) = rt.heap.allocateVariable();
      
      // Add to V_p as imported reader
      ctx.vp.add(VarKey(varId, true), VariableEntry(
        varId: varId,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Receive assignment (from bob, the creator)
      ctx.handleAssignment('bob', varId, ConstTerm('hello'));
      
      // Variable should be bound
      expect(rt.heap.isFullyBound(varId), isTrue);
      expect((rt.heap.getValue(varId) as ConstTerm).value, 'hello');
      
      // Should be removed from V_p
      expect(ctx.vp.contains(VarKey(varId, true)), isFalse);
    });
  });
  
  group('IrmaContext - handleReadRequest', () {
    test('records requester when reader not yet requested', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice created reader 100
      ctx.vp.add(VarKey(100, true), VariableEntry(
        varId: 100,
        isReader: true,
        creator: 'alice',
        role: VariableRole.createdReader,
        // requester defaults to null
      ));

      // Bob requests the reader
      ctx.handleReadRequest(100, 'bob');

      // requester should record bob
      expect(ctx.vp.lookup(VarKey(100, true))!.requester, 'bob');
    });
  });
  
  group('IrmaContext - handleAbandon', () {
    test('removes writer from V_p', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice has writer 100
      ctx.vp.add(VarKey(100, false), VariableEntry(
        varId: 100,
        isReader: false,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      // Receive abandon notification
      ctx.handleAbandon(100);
      
      // Writer should be removed from V_p
      expect(ctx.vp.contains(VarKey(100, false)), isFalse);
    });
  });
  
  group('IrmaContext - Introduction Protocol', () {
    test('scenario: alice imports writer from bob and binds it', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate a variable to represent the imported writer
      final (writerId, _) = rt.heap.allocateVariable();
      
      // Alice imports writer CA from bob (created by bob)
      ctx.registerImportedWriter(writerId, 'bob');
      
      // Verify V_p entry
      expect(ctx.vp.contains(VarKey(writerId, false)), isTrue);
      final entry = ctx.vp.lookup(VarKey(writerId, false));
      expect(entry!.role, VariableRole.importedWriter);
      expect(entry.creator, 'bob');
      
      // When Alice binds the imported writer, it should notify bob
      rt.heap.bindVariable(writerId, ConstTerm('hello'));
      
      // Should queue message to bob (the creator)
      expect(ctx.mp.countFor('bob'), 1);
      final msg = ctx.mp.poll('bob');
      expect(msg, isNotNull);
      expect(msg!.type, MessageType.assignment);
    });
    
    test('scenario: bob receives assignment and forwards to charlie', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'bob', runtime: rt);
      
      // Bob created reader CA? and exported it
      final (readerId, _) = rt.heap.allocateVariable();
      ctx.registerCreatedReader(readerId);
      
      // Charlie requests the reader
      ctx.handleReadRequest(readerId, 'charlie');
      expect(ctx.vp.lookup(VarKey(readerId, true))!.requester, 'charlie');
      
      // Alice sends assignment to bob (the creator)
      ctx.handleAssignment('bob', readerId, ConstTerm('hello from alice'));
      
      // Bob should forward to charlie
      expect(ctx.mp.countFor('charlie'), 1);
      final msg = ctx.mp.poll('charlie');
      expect(msg, isNotNull);
      expect(msg!.type, MessageType.assignment);
    });
    
    test('scenario: value arrives before request', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'bob', runtime: rt);
      
      // Bob created reader CA?
      final (readerId, _) = rt.heap.allocateVariable();
      ctx.registerCreatedReader(readerId);
      
      // Alice sends value BEFORE Charlie requests
      ctx.handleAssignment('bob', readerId, ConstTerm('early value'));
      
      // Value should be stored
      expect(ctx.vp.lookup(VarKey(readerId, true))!.boundValue, isA<Term>());
      expect(ctx.mp.isEmpty, isTrue); // No message yet
      
      // Now Charlie requests
      ctx.handleReadRequest(readerId, 'charlie');
      
      // Bob should reply immediately with stored value
      expect(ctx.mp.countFor('charlie'), 1);
      final msg = ctx.mp.poll('charlie');
      expect(msg, isNotNull);
      expect(msg!.type, MessageType.assignment);
    });
    
    test('importTerm handles writers correctly', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);

      // Allocate actual variables in the heap
      // allocateVariable() returns (writerAddr, readerAddr) tuple
      final (readerVarId, readerAddr) = rt.heap.allocateVariable();  // For the reader
      final (writerVarId, _) = rt.heap.allocateVariable();            // For the writer

      // Receive a term containing both writer and reader from bob
      final term = StructTerm('ch', [
        VarRef(readerAddr),    // AC? - reader at odd address
        VarRef(writerVarId),   // CA - writer at even address
      ]);

      ctx.importTerm(term, 'bob');

      // Reader should be imported reader (varId is the writer address)
      expect(ctx.vp.lookup(VarKey(readerVarId, true))!.role, VariableRole.importedReader);
      expect(ctx.vp.lookup(VarKey(readerVarId, true))!.creator, 'bob');

      // Writer should be imported writer
      expect(ctx.vp.lookup(VarKey(writerVarId, false))!.role, VariableRole.importedWriter);
      expect(ctx.vp.lookup(VarKey(writerVarId, false))!.creator, 'bob');
    });
  });

  group('IrmaContext - Heap Callback Integration', () {
    test('registerWriter sets up binding callback', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate a variable in the heap
      final (varId, _) = rt.heap.allocateVariable();
      
      // Register as writer in V_p with callback
      ctx.registerWriter(varId);
      
      // Variable should be in V_p
      expect(ctx.vp.contains(VarKey(varId, false)), isTrue);
      expect(ctx.vp.lookup(VarKey(varId, false))!.role, VariableRole.createdWriter);
    });
    
    test('binding callback queues message when requester exists', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate a variable in the heap
      final (varId, _) = rt.heap.allocateVariable();
      
      // Register as writer
      ctx.registerWriter(varId);
      
      // Simulate bob requesting the value
      ctx.vp.updateRequester(VarKey(varId, false), 'bob');
      
      // Now bind the variable (triggers onBind callback)
      rt.heap.bindVariable(varId, ConstTerm('hello'));
      
      // Should have queued an assignment message to bob
      expect(ctx.mp.countFor('bob'), 1);
      final msg = ctx.mp.poll('bob');
      expect(msg!.type, MessageType.assignment);
    });
    
    test('binding callback does nothing when no requester', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate a variable in the heap
      final (varId, _) = rt.heap.allocateVariable();
      
      // Register as writer (no requester yet)
      ctx.registerWriter(varId);
      
      // Bind the variable
      rt.heap.bindVariable(varId, ConstTerm('hello'));
      
      // No message should be queued (no requester)
      expect(ctx.mp.isEmpty, isTrue);
    });
    
    test('registerCreatedReader sets up binding callback', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate a variable in the heap
      final (varId, _) = rt.heap.allocateVariable();
      
      // Register as created reader
      ctx.registerCreatedReader(varId);
      
      // Variable should be in V_p
      expect(ctx.vp.contains(VarKey(varId, true)), isTrue);
      expect(ctx.vp.lookup(VarKey(varId, true))!.role, VariableRole.createdReader);
    });
    
    test('created reader binding callback queues message when requester exists', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate a variable in the heap
      final (varId, _) = rt.heap.allocateVariable();
      
      // Register as created reader
      ctx.registerCreatedReader(varId);
      
      // Simulate charlie requesting the value
      ctx.vp.updateRequester(VarKey(varId, true), 'charlie');
      
      // Bind the variable (triggers onBind callback)
      rt.heap.bindVariable(varId, ConstTerm('world'));
      
      // Should have queued an assignment message to charlie
      expect(ctx.mp.countFor('charlie'), 1);
      final msg = ctx.mp.poll('charlie');
      expect(msg!.type, MessageType.assignment);
    });
  });
}
