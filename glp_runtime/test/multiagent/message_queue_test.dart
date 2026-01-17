/// Unit tests for MessageQueue (M_p)
/// 
/// Tests the FIFO ordering and at-most-once delivery semantics
/// from irmaGLP-spec.md v1.1
library;

import 'dart:typed_data';
import 'package:test/test.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';

void main() {
  group('OutboundMessage', () {
    test('creates message with all fields', () {
      final payload = Uint8List.fromList([1, 2, 3, 4]);
      final msg = OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: payload,
      );
      
      expect(msg.destination, 'bob');
      expect(msg.type, MessageType.assignment);
      expect(msg.payload, payload);
    });
    
    test('toString includes destination and type', () {
      final msg = OutboundMessage(
        destination: 'alice',
        type: MessageType.readRequest,
        payload: [1, 2, 3],
      );
      
      final str = msg.toString();
      expect(str, contains('alice'));
      expect(str, contains('readRequest'));
      expect(str, contains('3 bytes'));
    });
  });
  
  group('MessageQueue - Basic Operations', () {
    test('creates empty queue', () {
      final mq = MessageQueue();
      
      expect(mq.isEmpty, isTrue);
      expect(mq.totalLength, 0);
      expect(mq.destinations, isEmpty);
    });
    
    test('add and poll message', () {
      final mq = MessageQueue();
      final msg = OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [1, 2, 3],
      );
      
      mq.add(msg);
      
      expect(mq.isEmpty, isFalse);
      expect(mq.totalLength, 1);
      expect(mq.countFor('bob'), 1);
      
      final polled = mq.poll('bob');
      expect(polled, isNotNull);
      expect(polled!.destination, 'bob');
      expect(polled.type, MessageType.assignment);
      
      expect(mq.isEmpty, isTrue);
    });
    
    test('poll on empty queue returns null', () {
      final mq = MessageQueue();
      
      expect(mq.poll('alice'), isNull);
    });
    
    test('poll on unknown destination returns null', () {
      final mq = MessageQueue();
      mq.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [1],
      ));
      
      expect(mq.poll('alice'), isNull);
    });
    
    test('peek does not remove message', () {
      final mq = MessageQueue();
      final msg = OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [1, 2, 3],
      );
      
      mq.add(msg);
      
      final peeked1 = mq.peek('bob');
      expect(peeked1, isNotNull);
      expect(mq.countFor('bob'), 1); // Still there
      
      final peeked2 = mq.peek('bob');
      expect(peeked2, isNotNull);
      expect(mq.countFor('bob'), 1); // Still there
      
      final polled = mq.poll('bob');
      expect(polled, isNotNull);
      expect(mq.isEmpty, isTrue);
    });
  });
  
  group('MessageQueue - FIFO Ordering Per Destination', () {
    test('messages to same destination are FIFO', () {
      final mq = MessageQueue();
      
      // Add three messages to bob
      mq.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [1], // First
      ));
      mq.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.readRequest,
        payload: [2], // Second
      ));
      mq.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.abandon,
        payload: [3], // Third
      ));
      
      expect(mq.countFor('bob'), 3);
      
      // Poll in FIFO order
      final first = mq.poll('bob');
      expect(first!.payload, [1]);
      
      final second = mq.poll('bob');
      expect(second!.payload, [2]);
      
      final third = mq.poll('bob');
      expect(third!.payload, [3]);
      
      expect(mq.isEmpty, isTrue);
    });
    
    test('FIFO maintained across multiple add/poll cycles', () {
      final mq = MessageQueue();
      
      // Add, poll, add, poll
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [1]));
      expect(mq.poll('bob')!.payload, [1]);
      
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [2]));
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [3]));
      
      expect(mq.poll('bob')!.payload, [2]);
      expect(mq.poll('bob')!.payload, [3]);
    });
  });
  
  group('MessageQueue - Multiple Destinations', () {
    test('messages to different destinations are independent', () {
      final mq = MessageQueue();
      
      // Add messages to alice and bob
      mq.add(OutboundMessage(destination: 'alice', type: MessageType.assignment, payload: [1]));
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [2]));
      mq.add(OutboundMessage(destination: 'alice', type: MessageType.assignment, payload: [3]));
      
      expect(mq.countFor('alice'), 2);
      expect(mq.countFor('bob'), 1);
      expect(mq.totalLength, 3);
      expect(mq.destinations.toSet(), {'alice', 'bob'});
      
      // Poll alice messages
      expect(mq.poll('alice')!.payload, [1]);
      expect(mq.poll('alice')!.payload, [3]);
      expect(mq.countFor('alice'), 0);
      
      // Bob message still there
      expect(mq.countFor('bob'), 1);
      expect(mq.poll('bob')!.payload, [2]);
    });
    
    test('destinations list updates correctly', () {
      final mq = MessageQueue();
      
      expect(mq.destinations, isEmpty);
      
      mq.add(OutboundMessage(destination: 'alice', type: MessageType.assignment, payload: [1]));
      expect(mq.destinations, ['alice']);
      
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [2]));
      expect(mq.destinations.toSet(), {'alice', 'bob'});
      
      mq.poll('alice');
      expect(mq.destinations, ['bob']);
      
      mq.poll('bob');
      expect(mq.destinations, isEmpty);
    });
  });
  
  group('MessageQueue - At-Most-Once Delivery', () {
    test('poll removes message from queue', () {
      final mq = MessageQueue();
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [1]));
      
      final first = mq.poll('bob');
      expect(first, isNotNull);
      
      // Second poll returns null - message already delivered
      final second = mq.poll('bob');
      expect(second, isNull);
    });
    
    test('each message delivered exactly once', () {
      final mq = MessageQueue();
      
      // Add 3 messages
      for (int i = 0; i < 3; i++) {
        mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [i]));
      }
      
      // Poll 3 times - each returns a message
      final msg1 = mq.poll('bob');
      final msg2 = mq.poll('bob');
      final msg3 = mq.poll('bob');
      
      expect(msg1, isNotNull);
      expect(msg2, isNotNull);
      expect(msg3, isNotNull);
      
      // Fourth poll returns null
      expect(mq.poll('bob'), isNull);
    });
  });
  
  group('MessageQueue - Helper Methods', () {
    test('countFor returns correct count', () {
      final mq = MessageQueue();
      
      expect(mq.countFor('alice'), 0);
      
      mq.add(OutboundMessage(destination: 'alice', type: MessageType.assignment, payload: [1]));
      expect(mq.countFor('alice'), 1);
      
      mq.add(OutboundMessage(destination: 'alice', type: MessageType.assignment, payload: [2]));
      expect(mq.countFor('alice'), 2);
      
      mq.poll('alice');
      expect(mq.countFor('alice'), 1);
    });
    
    test('peekAll returns all messages without removing', () {
      final mq = MessageQueue();
      
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [1]));
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [2]));
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [3]));
      
      final all = mq.peekAll('bob');
      expect(all.length, 3);
      expect(all[0].payload, [1]);
      expect(all[1].payload, [2]);
      expect(all[2].payload, [3]);
      
      // Messages still in queue
      expect(mq.countFor('bob'), 3);
    });
    
    test('pollAll returns and removes all messages', () {
      final mq = MessageQueue();
      
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [1]));
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [2]));
      
      final all = mq.pollAll('bob');
      expect(all.length, 2);
      expect(all[0].payload, [1]);
      expect(all[1].payload, [2]);
      
      // Queue now empty
      expect(mq.countFor('bob'), 0);
      expect(mq.isEmpty, isTrue);
    });
    
    test('clear removes all messages', () {
      final mq = MessageQueue();
      
      mq.add(OutboundMessage(destination: 'alice', type: MessageType.assignment, payload: [1]));
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [2]));
      
      expect(mq.totalLength, 2);
      
      mq.clear();
      
      expect(mq.isEmpty, isTrue);
      expect(mq.totalLength, 0);
      expect(mq.destinations, isEmpty);
    });
    
    test('clearFor removes messages for specific destination', () {
      final mq = MessageQueue();
      
      mq.add(OutboundMessage(destination: 'alice', type: MessageType.assignment, payload: [1]));
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [2]));
      
      mq.clearFor('alice');
      
      expect(mq.countFor('alice'), 0);
      expect(mq.countFor('bob'), 1);
    });
  });
  
  group('MessageQueue - Message Types', () {
    test('handles all three message types', () {
      final mq = MessageQueue();
      
      mq.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [1],
      ));
      mq.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.readRequest,
        payload: [2],
      ));
      mq.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.abandon,
        payload: [3],
      ));
      
      final msg1 = mq.poll('bob');
      expect(msg1!.type, MessageType.assignment);
      
      final msg2 = mq.poll('bob');
      expect(msg2!.type, MessageType.readRequest);
      
      final msg3 = mq.poll('bob');
      expect(msg3!.type, MessageType.abandon);
    });
  });
  
  group('MessageQueue - Edge Cases', () {
    test('handles empty payload', () {
      final mq = MessageQueue();
      mq.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: [],
      ));
      
      final msg = mq.poll('bob');
      expect(msg, isNotNull);
      expect(msg!.payload, isEmpty);
    });
    
    test('handles large payload', () {
      final mq = MessageQueue();
      final largePayload = List.generate(10000, (i) => i % 256);
      
      mq.add(OutboundMessage(
        destination: 'bob',
        type: MessageType.assignment,
        payload: largePayload,
      ));
      
      final msg = mq.poll('bob');
      expect(msg, isNotNull);
      expect(msg!.payload.length, 10000);
    });
    
    test('toString provides readable output', () {
      final mq = MessageQueue();
      
      final emptyStr = mq.toString();
      expect(emptyStr, contains('empty'));
      
      mq.add(OutboundMessage(destination: 'alice', type: MessageType.assignment, payload: [1]));
      mq.add(OutboundMessage(destination: 'bob', type: MessageType.assignment, payload: [2]));
      
      final str = mq.toString();
      expect(str, contains('alice'));
      expect(str, contains('bob'));
      expect(str, contains('1 message'));
    });
  });
}
