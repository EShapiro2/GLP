/// Unit tests for IrmaContext
/// 
/// Tests integration of V_p/M_p with GLP runtime
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
      alice.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'alice',
        role: VariableRole.writer,
      ));
      
      // Bob's V_p should be unaffected
      expect(alice.vp.contains(100), isTrue);
      expect(bob.vp.contains(100), isFalse);
    });
  });
  
  group('IrmaContext - processReaderBindings', () {
    test('queues assignment when created reader has requester', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice created reader 100, bob requested it
      ctx.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'alice',
        role: VariableRole.createdReader,
        state: 'bob', // bob requested
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
      ctx.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'alice',
        role: VariableRole.createdReader,
        state: null, // No requester
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
      ctx.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
        state: null,
      ));
      
      final sigmaHatReader = {100: ConstTerm('hello')};
      ctx.processReaderBindings(sigmaHatReader);
      
      // State should be updated to creator
      expect(ctx.vp.lookup(100)!.state, 'bob');
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
      ctx.vp.add(100, VariableEntry(
        varId: 100,
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
      expect(ctx.vp.contains(100), isFalse);
    });
    
    test('does not abandon readers that were assigned', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      ctx.vp.add(100, VariableEntry(
        varId: 100,
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
      expect(ctx.vp.contains(100), isTrue);
    });
    
    test('does not abandon readers that appear in body', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      ctx.vp.add(100, VariableEntry(
        varId: 100,
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
      ctx.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
        state: null,
      ));
      
      // Goal suspends on reader 100
      ctx.processSuspension({100});
      
      // Should queue read request to bob
      expect(ctx.mp.countFor('bob'), 1);
      final msg = ctx.mp.poll('bob');
      expect(msg!.type, MessageType.readRequest);
      
      // State should be updated
      expect(ctx.vp.lookup(100)!.state, 'bob');
    });
    
    test('does not send duplicate request', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice imported reader 100 from bob, already requested
      ctx.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
        state: 'bob', // Already requested
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
      ctx.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      ctx.vp.add(101, VariableEntry(
        varId: 101,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Goal with readers 100 and 101 fails
      ctx.processFailure({100, 101});
      
      // Both should send abandon messages
      expect(ctx.mp.countFor('bob'), 2);
      
      // Both removed from V_p
      expect(ctx.vp.contains(100), isFalse);
      expect(ctx.vp.contains(101), isFalse);
    });
  });
  
  group('IrmaContext - flushMessages', () {
    test('delivers all messages via callback', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Queue messages to multiple destinations
      ctx.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      ctx.vp.add(101, VariableEntry(
        varId: 101,
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
      
      ctx.vp.add(100, VariableEntry(
        varId: 100,
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
        VarRef(200, isReader: true),
      ]);
      
      ctx.importTerm(term, 'bob');
      
      // Variable 200 should be in V_p
      expect(ctx.vp.contains(200), isTrue);
      final entry = ctx.vp.lookup(200);
      expect(entry!.creator, 'bob');
      expect(entry.role, VariableRole.importedReader);
    });
    
    test('does not duplicate existing variables', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Variable 200 already in V_p
      ctx.vp.add(200, VariableEntry(
        varId: 200,
        creator: 'bob',
        role: VariableRole.importedReader,
        state: 'bob', // Already requested
      ));
      
      final term = VarRef(200, isReader: true);
      ctx.importTerm(term, 'bob');
      
      // State should be unchanged
      expect(ctx.vp.lookup(200)!.state, 'bob');
    });
  });
  
  group('IrmaContext - handleAssignment', () {
    test('applies assignment to heap and reactivates goals', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate variable
      final varId = rt.heap.allocateVariable();
      
      // Add to V_p as imported reader
      ctx.vp.add(varId, VariableEntry(
        varId: varId,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Receive assignment
      ctx.handleAssignment(varId, ConstTerm('hello'));
      
      // Variable should be bound
      expect(rt.heap.isFullyBound(varId), isTrue);
      expect((rt.heap.getValue(varId) as ConstTerm).value, 'hello');
      
      // Should be removed from V_p
      expect(ctx.vp.contains(varId), isFalse);
    });
  });
  
  group('IrmaContext - handleReadRequest', () {
    test('records requester when reader not yet requested', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice created reader 100
      ctx.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'alice',
        role: VariableRole.createdReader,
        state: null,
      ));
      
      // Bob requests the reader
      ctx.handleReadRequest(100, 'bob');
      
      // State should record bob as requester
      expect(ctx.vp.lookup(100)!.state, 'bob');
    });
  });
  
  group('IrmaContext - handleAbandon', () {
    test('removes writer from V_p', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Alice has writer 100
      ctx.vp.add(100, VariableEntry(
        varId: 100,
        creator: 'alice',
        role: VariableRole.writer,
      ));
      
      // Receive abandon notification
      ctx.handleAbandon(100);
      
      // Writer should be removed from V_p
      expect(ctx.vp.contains(100), isFalse);
    });
  });
  
  group('IrmaContext - Heap Callback Integration', () {
    test('registerWriter sets up binding callback', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate a variable in the heap
      final varId = rt.heap.allocateVariable();
      
      // Register as writer in V_p with callback
      ctx.registerWriter(varId);
      
      // Variable should be in V_p
      expect(ctx.vp.contains(varId), isTrue);
      expect(ctx.vp.lookup(varId)!.role, VariableRole.writer);
    });
    
    test('binding callback queues message when requester exists', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate a variable in the heap
      final varId = rt.heap.allocateVariable();
      
      // Register as writer
      ctx.registerWriter(varId);
      
      // Simulate bob requesting the value
      ctx.vp.updateState(varId, 'bob');
      
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
      final varId = rt.heap.allocateVariable();
      
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
      final varId = rt.heap.allocateVariable();
      
      // Register as created reader
      ctx.registerCreatedReader(varId);
      
      // Variable should be in V_p
      expect(ctx.vp.contains(varId), isTrue);
      expect(ctx.vp.lookup(varId)!.role, VariableRole.createdReader);
    });
    
    test('created reader binding callback queues message when requester exists', () {
      final rt = GlpRuntime();
      final ctx = IrmaContext(agentId: 'alice', runtime: rt);
      
      // Allocate a variable in the heap
      final varId = rt.heap.allocateVariable();
      
      // Register as created reader
      ctx.registerCreatedReader(varId);
      
      // Simulate charlie requesting the value
      ctx.vp.updateState(varId, 'charlie');
      
      // Bind the variable (triggers onBind callback)
      rt.heap.bindVariable(varId, ConstTerm('world'));
      
      // Should have queued an assignment message to charlie
      expect(ctx.mp.countFor('charlie'), 1);
      final msg = ctx.mp.poll('charlie');
      expect(msg!.type, MessageType.assignment);
    });
  });
}
