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
      final (writerId, readerId) = agent.runtime.heap.allocateVariable();
      
      // Add to V_p as imported reader
      final readerKey = VarKey(readerId, true);
      agent.vp.add(readerKey, VariableEntry(
        varId: readerId,
        isReader: true,
        creator: 'alice',
        role: VariableRole.importedReader,
      ));
      
      // Create assignment message from alice
      final serializer = PayloadSerializer('alice');
      // Use V2 API with isReader callback (won't be called for ConstTerm values)
      final payload = serializer.createAssignmentPayloadV2(
        readerId, ConstTerm('hello'), (_) => false);
      final msg = OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: payload,
      );
      final bytes = serializer.serializeMessage(msg);
      
      // Handle the message
      agent.handleIncomingMessage('alice', bytes);
      
      // Variable should be removed from V_p after assignment
      expect(agent.vp.contains(readerKey), isFalse);
    });
    
    test('handles incoming read request message', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Set up a writer in alice's heap
      final (writerId, readerId) = agent.runtime.heap.allocateVariable();
      
      // Add to V_p as createdWriter (alice created it)
      final writerKey = VarKey(writerId, false);
      agent.vp.add(writerKey, VariableEntry(
        varId: writerId,
        isReader: false,
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
      final entry = agent.vp.lookup(writerKey);
      expect(entry, isNotNull);
      // Note: handleReadRequest records requester in createdWriter entries when no reader entry exists
    });
    
    test('handles incoming abandon message', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Set up a writer in alice's heap
      final (writerId, readerId) = agent.runtime.heap.allocateVariable();
      
      // Add to V_p as createdWriter
      final writerKey = VarKey(writerId, false);
      agent.vp.add(writerKey, VariableEntry(
        varId: writerId,
        isReader: false,
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
      expect(agent.vp.contains(writerKey), isFalse);
    });
  });
  
  group('IrmaAgent - V_p Operations', () {
    test('registerWriter adds to V_p', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Allocate a variable
      final (writerId, readerId) = agent.runtime.heap.allocateVariable();
      
      // Register as writer
      agent.registerWriter(writerId);
      
      final writerKey = VarKey(writerId, false);
      expect(agent.vp.contains(writerKey), isTrue);
      expect(agent.vp.lookup(writerKey)!.role, VariableRole.createdWriter);
    });
    
    test('registerCreatedReader adds to V_p', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Allocate a variable
      final (writerId, readerId) = agent.runtime.heap.allocateVariable();
      
      // Register as created reader
      agent.registerCreatedReader(readerId);
      
      final readerKey = VarKey(readerId, true);
      expect(agent.vp.contains(readerKey), isTrue);
      expect(agent.vp.lookup(readerKey)!.role, VariableRole.createdReader);
    });
    
    test('registerImportedWriter adds to V_p', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      // Allocate a variable to represent imported writer
      final (writerId, _) = agent.runtime.heap.allocateVariable();
      
      // Register as imported writer from bob
      agent.registerImportedWriter(writerId, 'bob');
      
      final writerKey = VarKey(writerId, false);
      expect(agent.vp.contains(writerKey), isTrue);
      expect(agent.vp.lookup(writerKey)!.role, VariableRole.importedWriter);
      expect(agent.vp.lookup(writerKey)!.creator, 'bob');
    });
  });
  
  group('IrmaAgent - Logging', () {
    test('logs messages when callback set', () {
      final agent = IrmaAgent(agentId: 'alice');
      
      final logs = <String>[];
      agent.onLog = logs.add;
      
      // Set up a variable in alice's heap first
      final (writerId, readerId) = agent.runtime.heap.allocateVariable();
      
      // Add to V_p as imported reader
      final readerKey = VarKey(readerId, true);
      agent.vp.add(readerKey, VariableEntry(
        varId: readerId,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
      ));
      
      // Create an assignment message from bob
      final serializer = PayloadSerializer('bob');
      // Use V2 API with isReader callback (won't be called for ConstTerm values)
      final payload = serializer.createAssignmentPayloadV2(
        readerId, ConstTerm('test'), (_) => false);
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
