/// Unit tests for PayloadSerializer
///
/// Tests global variable ID encoding and message serialization
/// from irmaGLP-spec.md v3.0
///
/// NOTE: Tests for term serialization using V2 API (serializeTermWithCallbacks,
/// deserializeAgentMessagePayload, etc.) need to be added. The deprecated
/// serializeTerm/deserializeTerm methods have been removed.
library;

import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';

void main() {
  group('GlobalVarId', () {
    test('encodes to creator:localId format', () {
      final id = GlobalVarId('alice', 1042);
      expect(id.encode(), 'alice:1042');
    });

    test('decodes from creator:localId format', () {
      final id = GlobalVarId.decode('alice:1042');
      expect(id.creator, 'alice');
      expect(id.localId, 1042);
    });

    test('round-trip encoding/decoding', () {
      final original = GlobalVarId('bob', 999);
      final encoded = original.encode();
      final decoded = GlobalVarId.decode(encoded);

      expect(decoded.creator, original.creator);
      expect(decoded.localId, original.localId);
    });

    test('handles multi-character agent names', () {
      final id = GlobalVarId('charlie_agent_123', 42);
      final encoded = id.encode();
      final decoded = GlobalVarId.decode(encoded);

      expect(decoded.creator, 'charlie_agent_123');
      expect(decoded.localId, 42);
    });

    test('equality comparison', () {
      final id1 = GlobalVarId('alice', 42);
      final id2 = GlobalVarId('alice', 42);
      final id3 = GlobalVarId('alice', 43);
      final id4 = GlobalVarId('bob', 42);

      expect(id1, equals(id2));
      expect(id1, isNot(equals(id3)));
      expect(id1, isNot(equals(id4)));
    });

    test('throws on invalid format', () {
      expect(() => GlobalVarId.decode('invalid'), throwsFormatException);
      expect(() => GlobalVarId.decode('alice:notanumber'), throwsFormatException);
      expect(() => GlobalVarId.decode('alice:bob:42'), throwsFormatException);
    });
  });

  group('PayloadSerializer - Message Serialization', () {
    late PayloadSerializer serializer;

    setUp(() {
      serializer = PayloadSerializer('alice');
    });

    test('serializes assignment message', () {
      final msg = OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [1, 2, 3, 4],
      );

      final bytes = serializer.serializeMessage(msg);
      final deserialized = serializer.deserializeMessage(bytes);

      expect(deserialized.destination, 'bob');
      expect(deserialized.type, MessageType.assignment);
      expect(deserialized.payload, [1, 2, 3, 4]);
    });

    test('serializes readRequest message', () {
      final msg = OutboundMessage(
        destination: 'alice',
        type: MessageType.readRequest,
        payload: [10, 20, 30],
      );

      final bytes = serializer.serializeMessage(msg);
      final deserialized = serializer.deserializeMessage(bytes);

      expect(deserialized.destination, 'alice');
      expect(deserialized.type, MessageType.readRequest);
      expect(deserialized.payload, [10, 20, 30]);
    });

    test('serializes abandon message', () {
      final msg = OutboundMessage(
        destination: 'charlie',
        type: MessageType.abandon,
        payload: [99],
      );

      final bytes = serializer.serializeMessage(msg);
      final deserialized = serializer.deserializeMessage(bytes);

      expect(deserialized.destination, 'charlie');
      expect(deserialized.type, MessageType.abandon);
      expect(deserialized.payload, [99]);
    });

    test('handles empty payload', () {
      final msg = OutboundMessage(
        destination: 'alice',
        type: MessageType.assignment,
        payload: [],
      );

      final bytes = serializer.serializeMessage(msg);
      final deserialized = serializer.deserializeMessage(bytes);

      expect(deserialized.payload, isEmpty);
    });

    test('handles large payload', () {
      final largePayload = List.generate(10000, (i) => i % 256);
      final msg = OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: largePayload,
      );

      final bytes = serializer.serializeMessage(msg);
      final deserialized = serializer.deserializeMessage(bytes);

      expect(deserialized.payload, largePayload);
    });

    test('all message types round-trip correctly', () {
      final types = [
        MessageType.assignment,
        MessageType.readRequest,
        MessageType.abandon,
      ];

      for (final type in types) {
        final msg = OutboundMessage(
          destination: 'agent_$type',
          type: type,
          payload: [type.index, type.index + 1],
        );

        final bytes = serializer.serializeMessage(msg);
        final deserialized = serializer.deserializeMessage(bytes);

        expect(deserialized.destination, msg.destination);
        expect(deserialized.type, msg.type);
        expect(deserialized.payload, msg.payload);
      }
    });
  });

  // TODO: Add tests for V2 serialization API:
  // - serializeTermWithCallbacks with isReader callback
  // - createAssignmentPayloadV2 with isReader callback
  // - deserializeAssignmentPayload with allocator callbacks
  // - deserializeAgentMessagePayload with allocator callbacks
  // - deserializeAgentMessagePayloadWithMapping with allocator callbacks
}
