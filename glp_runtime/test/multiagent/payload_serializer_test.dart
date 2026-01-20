/// Unit tests for PayloadSerializer
/// 
/// Tests global variable ID encoding and term/message serialization
/// from irmaGLP-spec.md v1.1
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/terms.dart';
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
  
  group('PayloadSerializer - Constants', () {
    late PayloadSerializer serializer;
    
    setUp(() {
      serializer = PayloadSerializer('alice');
    });
    
    test('serializes and deserializes nil', () {
      final term = ConstTerm(null);
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<ConstTerm>());
      expect((deserialized as ConstTerm).value, isNull);
    });
    
    test('serializes and deserializes integer', () {
      final term = ConstTerm(42);
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<ConstTerm>());
      expect((deserialized as ConstTerm).value, 42);
    });
    
    test('serializes and deserializes negative integer', () {
      final term = ConstTerm(-999);
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<ConstTerm>());
      expect((deserialized as ConstTerm).value, -999);
    });
    
    test('serializes and deserializes double', () {
      final term = ConstTerm(3.14159);
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<ConstTerm>());
      expect((deserialized as ConstTerm).value, closeTo(3.14159, 0.00001));
    });
    
    test('serializes and deserializes string', () {
      final term = ConstTerm('hello world');
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<ConstTerm>());
      expect((deserialized as ConstTerm).value, 'hello world');
    });
    
    test('serializes and deserializes boolean', () {
      final term1 = ConstTerm(true);
      final bytes1 = serializer.serializeTerm(term1, 'alice');
      final (deserialized1, _) = serializer.deserializeTerm(bytes1, 0);
      expect((deserialized1 as ConstTerm).value, true);
      
      final term2 = ConstTerm(false);
      final bytes2 = serializer.serializeTerm(term2, 'alice');
      final (deserialized2, _) = serializer.deserializeTerm(bytes2, 0);
      expect((deserialized2 as ConstTerm).value, false);
    });
    
    test('serializes and deserializes atom constant', () {
      final term = ConstTerm('ping');
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<ConstTerm>());
      expect((deserialized as ConstTerm).value, 'ping');
    });
  });
  
  group('PayloadSerializer - Variables', () {
    late PayloadSerializer serializer;
    
    setUp(() {
      serializer = PayloadSerializer('alice');
    });
    
    test('serializes writer with global ID', () {
      final term = VarRef(42);
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<VarRef>());
      final varRef = deserialized as VarRef;
      expect(varRef.varId, 42);
      expect(varRef.isReader, false);
    });
    
    test('serializes reader with global ID', () {
      final term = VarRef(101);
      final bytes = serializer.serializeTerm(term, 'bob');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<VarRef>());
      final varRef = deserialized as VarRef;
      expect(varRef.varId, 100);
      expect(varRef.isReader, true);
    });
    
    test('preserves isReader flag', () {
      final writer = VarRef(50);
      final reader = VarRef(51);
      
      final writerBytes = serializer.serializeTerm(writer, 'alice');
      final readerBytes = serializer.serializeTerm(reader, 'alice');
      
      final (deserializedWriter, _) = serializer.deserializeTerm(writerBytes, 0);
      final (deserializedReader, _) = serializer.deserializeTerm(readerBytes, 0);
      
      expect((deserializedWriter as VarRef).isReader, false);
      expect((deserializedReader as VarRef).isReader, true);
    });
  });
  
  group('PayloadSerializer - Structures', () {
    late PayloadSerializer serializer;
    
    setUp(() {
      serializer = PayloadSerializer('alice');
    });
    
    test('serializes simple structure', () {
      final term = StructTerm('msg', [
        ConstTerm('alice'),
        ConstTerm('bob'),
        ConstTerm('hello'),
      ]);
      
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<StructTerm>());
      final struct = deserialized as StructTerm;
      expect(struct.functor, 'msg');
      expect(struct.args.length, 3);
      expect((struct.args[0] as ConstTerm).value, 'alice');
      expect((struct.args[1] as ConstTerm).value, 'bob');
      expect((struct.args[2] as ConstTerm).value, 'hello');
    });
    
    test('serializes structure with variables', () {
      final term = StructTerm('ch', [
        VarRef(10), // Writer
        VarRef(11),  // Reader
      ]);
      
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<StructTerm>());
      final struct = deserialized as StructTerm;
      expect(struct.functor, 'ch');
      expect(struct.args.length, 2);
      expect((struct.args[0] as VarRef).isReader, false);
      expect((struct.args[1] as VarRef).isReader, true);
    });
    
    test('serializes nested structures', () {
      final term = StructTerm('outer', [
        StructTerm('inner', [
          ConstTerm(1),
          ConstTerm(2),
        ]),
        ConstTerm(3),
      ]);
      
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<StructTerm>());
      final outer = deserialized as StructTerm;
      expect(outer.functor, 'outer');
      expect(outer.args.length, 2);
      
      final inner = outer.args[0] as StructTerm;
      expect(inner.functor, 'inner');
      expect(inner.args.length, 2);
      expect((inner.args[0] as ConstTerm).value, 1);
      expect((inner.args[1] as ConstTerm).value, 2);
      expect((outer.args[1] as ConstTerm).value, 3);
    });
    
    test('serializes list as dot structures', () {
      // [1, 2, 3] represented as .(1, .(2, .(3, nil)))
      final term = StructTerm('.', [
        ConstTerm(1),
        StructTerm('.', [
          ConstTerm(2),
          StructTerm('.', [
            ConstTerm(3),
            ConstTerm(null), // nil
          ]),
        ]),
      ]);
      
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<StructTerm>());
      final list = deserialized as StructTerm;
      expect(list.functor, '.');
      expect(list.args.length, 2);
      expect((list.args[0] as ConstTerm).value, 1);
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
  });
  
  group('PayloadSerializer - Round-Trip Tests', () {
    late PayloadSerializer serializer;
    
    setUp(() {
      serializer = PayloadSerializer('alice');
    });
    
    test('constant round-trip preserves value', () {
      final constants = [
        ConstTerm(null),
        ConstTerm(42),
        ConstTerm(-100),
        ConstTerm(3.14),
        ConstTerm('hello'),
        ConstTerm(true),
        ConstTerm(false),
      ];
      
      for (final term in constants) {
        final bytes = serializer.serializeTerm(term, 'alice');
        final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
        
        expect(deserialized, isA<ConstTerm>());
        final constTerm = deserialized as ConstTerm;
        
        if (term.value is double) {
          expect(constTerm.value, closeTo(term.value as double, 0.00001));
        } else {
          expect(constTerm.value, term.value);
        }
      }
    });
    
    test('variable round-trip preserves ID and isReader', () {
      final writer = VarRef(42);
      final reader = VarRef(101);
      
      final writerBytes = serializer.serializeTerm(writer, 'alice');
      final readerBytes = serializer.serializeTerm(reader, 'bob');
      
      final (deserializedWriter, _) = serializer.deserializeTerm(writerBytes, 0);
      final (deserializedReader, _) = serializer.deserializeTerm(readerBytes, 0);
      
      expect(deserializedWriter, isA<VarRef>());
      expect((deserializedWriter as VarRef).varId, 42);
      expect(deserializedWriter.isReader, false);
      
      expect(deserializedReader, isA<VarRef>());
      expect((deserializedReader as VarRef).varId, 100);
      expect(deserializedReader.isReader, true);
    });
    
    test('structure round-trip preserves functor and args', () {
      final term = StructTerm('msg', [
        ConstTerm('alice'),
        ConstTerm('bob'),
        ConstTerm('ping'),
      ]);
      
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<StructTerm>());
      final struct = deserialized as StructTerm;
      expect(struct.functor, 'msg');
      expect(struct.args.length, 3);
      expect((struct.args[0] as ConstTerm).value, 'alice');
      expect((struct.args[1] as ConstTerm).value, 'bob');
      expect((struct.args[2] as ConstTerm).value, 'ping');
    });
    
    test('complex nested structure round-trip', () {
      final term = StructTerm('intro', [
        ConstTerm('charlie'),
        StructTerm('ch', [
          VarRef(50),   // Writer at addr 50, varId=50
          VarRef(51),   // Reader at addr 51, varId=50 (same pair)
        ]),
      ]);

      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);

      expect(deserialized, isA<StructTerm>());
      final intro = deserialized as StructTerm;
      expect(intro.functor, 'intro');
      expect(intro.args.length, 2);
      expect((intro.args[0] as ConstTerm).value, 'charlie');

      final ch = intro.args[1] as StructTerm;
      expect(ch.functor, 'ch');
      expect(ch.args.length, 2);
      expect((ch.args[0] as VarRef).varId, 50);
      expect((ch.args[0] as VarRef).isReader, false);
      expect((ch.args[1] as VarRef).varId, 50);  // Same varId as writer
      expect((ch.args[1] as VarRef).isReader, true);
    });
    
    test('message round-trip preserves all fields', () {
      final original = OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [1, 2, 3, 4, 5],
      );
      
      final bytes = serializer.serializeMessage(original);
      final deserialized = serializer.deserializeMessage(bytes);
      
      expect(deserialized.destination, original.destination);
      expect(deserialized.type, original.type);
      expect(deserialized.payload, original.payload);
    });
  });
  
  group('PayloadSerializer - Edge Cases', () {
    late PayloadSerializer serializer;
    
    setUp(() {
      serializer = PayloadSerializer('alice');
    });
    
    test('handles empty structure', () {
      final term = StructTerm('empty', []);
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<StructTerm>());
      final struct = deserialized as StructTerm;
      expect(struct.functor, 'empty');
      expect(struct.args, isEmpty);
    });
    
    test('handles very long functor names', () {
      final longFunctor = 'a' * 1000;
      final term = StructTerm(longFunctor, [ConstTerm(1)]);
      
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<StructTerm>());
      expect((deserialized as StructTerm).functor, longFunctor);
    });
    
    test('handles unicode strings', () {
      final term = ConstTerm('Hello 世界 🌍');
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect((deserialized as ConstTerm).value, 'Hello 世界 🌍');
    });
    
    test('handles deeply nested structures', () {
      // Build: f(f(f(f(f(1)))))
      Term term = ConstTerm(1);
      for (int i = 0; i < 10; i++) {
        term = StructTerm('f', [term]);
      }
      
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      // Verify nesting depth
      Term current = deserialized;
      int depth = 0;
      while (current is StructTerm) {
        expect(current.functor, 'f');
        depth++;
        current = current.args[0];
      }
      expect(depth, 10);
      expect((current as ConstTerm).value, 1);
    });
  });
  
  group('PayloadSerializer - No Information Loss', () {
    late PayloadSerializer serializer;
    
    setUp(() {
      serializer = PayloadSerializer('alice');
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
    
    test('complex term with all types round-trips', () {
      final term = StructTerm('complex', [
        ConstTerm(42),
        ConstTerm('string'),
        ConstTerm(3.14),
        ConstTerm(true),
        ConstTerm(null),
        VarRef(10),
        VarRef(21),
        StructTerm('nested', [
          ConstTerm('inner'),
          VarRef(31),
        ]),
      ]);
      
      final bytes = serializer.serializeTerm(term, 'alice');
      final (deserialized, _) = serializer.deserializeTerm(bytes, 0);
      
      expect(deserialized, isA<StructTerm>());
      final complex = deserialized as StructTerm;
      expect(complex.functor, 'complex');
      expect(complex.args.length, 8);
      
      // Verify each arg type and value
      expect((complex.args[0] as ConstTerm).value, 42);
      expect((complex.args[1] as ConstTerm).value, 'string');
      expect((complex.args[2] as ConstTerm).value, closeTo(3.14, 0.01));
      expect((complex.args[3] as ConstTerm).value, true);
      expect((complex.args[4] as ConstTerm).value, null);
      expect((complex.args[5] as VarRef).varId, 10);
      expect((complex.args[6] as VarRef).varId, 20);
      
      final nested = complex.args[7] as StructTerm;
      expect(nested.functor, 'nested');
      expect((nested.args[0] as ConstTerm).value, 'inner');
      expect((nested.args[1] as VarRef).varId, 30);
    });
  });
}
