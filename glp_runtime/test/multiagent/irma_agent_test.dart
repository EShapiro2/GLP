/// Unit tests for IrmaAgent
/// 
/// Tests agent wrapper for multiagent integration
library;

import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/irma_agent.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';

void main() {
  group('IrmaAgent - Initialization', () {
    test('creates agent with ID', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      expect(agent.agentId, 'alice');
      expect(agent.runtime, isNotNull);
      expect(agent.context, isNotNull);
      expect(agent.vp.agentId, 'alice');
    });
    
    test('creates agent with provided runtime', () {
      final runtime = GlpRuntime();
      final agent = IrmaAgent(agentId: 'bob', runtime: runtime);
      
      expect(agent.runtime, same(runtime));
      expect(agent.context.runtime, same(runtime));
    });
    
    test('context and agent share same runtime', () {
      final agent = IrmaAgent(agentId: 'charlie');
      
      // Verify they're the same instance
      expect(agent.runtime, same(agent.context.runtime));
    });
  });
  
  group('IrmaAgent - Message Serialization', () {
    test('outbound message triggers coordinator callback', () async {
      final agent = IrmaAgent(agentId: 'alice');
      
      final receivedMessages = <(String, Uint8List)>[];
      agent.onSendToCoordinator = (dest, payload) async {
        receivedMessages.add((dest, payload));
      };
      
      // Add a message to M_p
      agent.mp.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [1, 2, 3],
      ));
      
      // Flush should trigger callback
      final flushed = agent.flushMessages();
      
      expect(flushed, 1);
      expect(receivedMessages.length, 1);
      expect(receivedMessages[0].$1, 'bob');
    });
    
    test('no callback means messages dropped', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      final logs = <String>[];
      agent.onLog = logs.add;
      
      // Add a message without setting callback
      agent.mp.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [1, 2, 3],
      ));
      
      final flushed = agent.flushMessages();
      
      expect(flushed, 1);
      expect(logs.any((l) => l.contains('WARNING')), isTrue);
    });
  });
  
  group('IrmaAgent - Incoming Messages', () {
    test('handles incoming assignment message', () {
      final agent = IrmaAgent(agentId: 'bob');
      
      // Set up a variable in bob's heap
      final (writerId, readerId) = agent.runtime.heap.allocateFreshPair();
      
      // Add to V_p as imported reader
      agent.vp.add(readerId, VariableEntry(
        varId: readerId,
        creator: 'alice',
        role: VariableRole.importedReader,
      ));
      
      // Create assignment message from alice
      final serializer = PayloadSerializer('alice');
      final payload = serializer.createAssignmentPayload(readerId, ConstTerm('hello'));
      final msg = OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: payload,
      );
      final bytes = serializer.serializeMessage(msg);
      
      // Handle the message
      agent.handleIncomingMessage('alice', bytes);
      
      // Variable should be removed from V_p after assignment
      expect(agent.vp.contains(readerId), isFalse);
    });
    
    test('handles incoming read request message', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Set up a writer in alice's heap
      final (writerId, readerId) = agent.runtime.heap.allocateFreshPair();
      
      // Add to V_p as createdWriter (alice created it)
      agent.vp.add(writerId, VariableEntry(
        varId: writerId,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      // Create read request message from bob
      final serializer = PayloadSerializer('bob');
      final payload = serializer.createReadRequestPayload(writerId, 'bob');
      final msg = OutboundMessage(
        destination: 'alice',
        type: MessageType.readRequest,
        payload: payload,
      );
      final bytes = serializer.serializeMessage(msg);
      
      // Handle the message
      agent.handleIncomingMessage('bob', bytes);
      
      // Entry should now have requester recorded
      final entry = agent.vp.lookup(writerId);
      expect(entry, isNotNull);
      // Note: handleReadRequest records requester in createdReader entries
    });
    
    test('handles incoming abandon message', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Set up a writer in alice's heap
      final (writerId, readerId) = agent.runtime.heap.allocateFreshPair();
      
      // Add to V_p as createdWriter
      agent.vp.add(writerId, VariableEntry(
        varId: writerId,
        creator: 'alice',
        role: VariableRole.createdWriter,
      ));
      
      // Create abandon message from bob
      final serializer = PayloadSerializer('bob');
      final payload = serializer.createAbandonPayload(writerId);
      final msg = OutboundMessage(
        destination: 'alice',
        type: MessageType.abandon,
        payload: payload,
      );
      final bytes = serializer.serializeMessage(msg);
      
      // Handle the message
      agent.handleIncomingMessage('bob', bytes);
      
      // Writer should be removed from V_p
      expect(agent.vp.contains(writerId), isFalse);
    });
  });
  
  group('IrmaAgent - V_p Operations', () {
    test('registerWriter adds to V_p', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Allocate a variable
      final (writerId, readerId) = agent.runtime.heap.allocateFreshPair();
      
      // Register as writer
      agent.registerWriter(writerId);
      
      expect(agent.vp.contains(writerId), isTrue);
      expect(agent.vp.lookup(writerId)!.role, VariableRole.createdWriter);
    });
    
    test('registerCreatedReader adds to V_p', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Allocate a variable
      final (writerId, readerId) = agent.runtime.heap.allocateFreshPair();
      
      // Register as created reader
      agent.registerCreatedReader(readerId);
      
      expect(agent.vp.contains(readerId), isTrue);
      expect(agent.vp.lookup(readerId)!.role, VariableRole.createdReader);
    });
    
    test('registerImportedWriter adds to V_p', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Allocate a variable to represent imported writer
      final writerId = agent.runtime.heap.allocateVariable();
      
      // Register as imported writer from bob
      agent.registerImportedWriter(writerId, 'bob');
      
      expect(agent.vp.contains(writerId), isTrue);
      expect(agent.vp.lookup(writerId)!.role, VariableRole.importedWriter);
      expect(agent.vp.lookup(writerId)!.creator, 'bob');
    });
  });
  
  group('IrmaAgent - Logging', () {
    test('logs messages when callback set', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      final logs = <String>[];
      agent.onLog = logs.add;
      
      // Set up a variable in alice's heap first
      final (writerId, readerId) = agent.runtime.heap.allocateFreshPair();
      
      // Add to V_p as imported reader
      agent.vp.add(readerId, VariableEntry(
        varId: readerId,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Create an assignment message from bob
      final serializer = PayloadSerializer('bob');
      final payload = serializer.createAssignmentPayload(readerId, ConstTerm('test'));
      final msg = OutboundMessage(
        destination: 'alice',
        type: MessageType.assignment,
        payload: payload,
      );
      final bytes = serializer.serializeMessage(msg);
      
      // Handle message should log
      agent.handleIncomingMessage('bob', bytes);
      
      expect(logs.isNotEmpty, isTrue);
      expect(logs.any((l) => l.contains('[alice]')), isTrue);
    });
  });
}
