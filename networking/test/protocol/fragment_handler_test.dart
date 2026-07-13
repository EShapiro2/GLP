import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';

/// Fragmentation for the shared message transport (spec §Message Transport):
/// self-contained fragments (messageId + index + count), reassembled in any
/// arrival order, delivered whole.
void main() {
  const maxChunk = 270;
  const messageId = '00000000-0000-4000-8000-000000000001';

  group('FragmentHandler', () {
    late FragmentHandler handler;

    setUp(() {
      handler = FragmentHandler();
    });
    tearDown(() {
      handler.dispose();
    });

    group('needsFragmentation', () {
      test('false at or below one chunk', () {
        expect(handler.needsFragmentation(Uint8List(maxChunk),
            maxChunk: maxChunk), isFalse);
      });
      test('true above one chunk', () {
        expect(handler.needsFragmentation(Uint8List(maxChunk + 1),
            maxChunk: maxChunk), isTrue);
      });
    });

    group('fragment', () {
      test('splits into ceil(len/maxChunk) self-contained fragments', () {
        final payload = Uint8List.fromList(
            List.generate(1000, (i) => i % 256));
        final result =
            handler.fragment(payload: payload, messageId: messageId, maxChunk: maxChunk);
        expect(result.fragments.length, (1000 / maxChunk).ceil());
        expect(result.messageId, messageId);
        for (final f in result.fragments) {
          expect(f.type, PacketType.fragment);
          final decoded = FragmentHandler.decodeFragment(f.payload);
          expect(decoded.messageId, messageId);
          expect(decoded.count, result.fragments.length);
        }
      });

      test('fragment indices run 0..count-1 in order', () {
        final payload = Uint8List(1000);
        final result =
            handler.fragment(payload: payload, messageId: messageId, maxChunk: maxChunk);
        for (var i = 0; i < result.fragments.length; i++) {
          expect(
              FragmentHandler.decodeFragment(result.fragments[i].payload).index,
              i);
        }
      });

      test('a one-chunk payload still produces a single fragment', () {
        final result = handler.fragment(
            payload: Uint8List(10), messageId: messageId, maxChunk: maxChunk);
        expect(result.fragments.length, 1);
        expect(
            FragmentHandler.decodeFragment(result.fragments.first.payload).count,
            1);
      });

      test('rejects a non-UUID messageId', () {
        expect(
          () => handler.fragment(
              payload: Uint8List(1000), messageId: 'short', maxChunk: maxChunk),
          throwsArgumentError,
        );
      });
    });

    group('reassembly', () {
      test('reassembles in-order fragments to the original payload', () {
        final payload = Uint8List.fromList(
            List.generate(1000, (i) => (i * 7) % 256));
        final result =
            handler.fragment(payload: payload, messageId: messageId, maxChunk: maxChunk);
        ReassembledMessage? done;
        for (final f in result.fragments) {
          done = handler.addFragment(FragmentHandler.decodeFragment(f.payload));
        }
        expect(done, isNotNull);
        expect(done!.messageId, messageId);
        expect(done.payload, payload);
      });

      test('reassembles fragments arriving in reverse order', () {
        final payload = Uint8List.fromList(
            List.generate(1000, (i) => (i * 13) % 256));
        final result =
            handler.fragment(payload: payload, messageId: messageId, maxChunk: maxChunk);
        final decoded = [
          for (final f in result.fragments)
            FragmentHandler.decodeFragment(f.payload)
        ].reversed.toList();
        ReassembledMessage? done;
        for (final frag in decoded) {
          done = handler.addFragment(frag);
        }
        expect(done, isNotNull);
        expect(done!.payload, payload);
      });

      test('a partial set does not deliver', () {
        final payload = Uint8List(1000);
        final result =
            handler.fragment(payload: payload, messageId: messageId, maxChunk: maxChunk);
        final decoded = [
          for (final f in result.fragments)
            FragmentHandler.decodeFragment(f.payload)
        ];
        for (var i = 0; i < decoded.length - 1; i++) {
          expect(handler.addFragment(decoded[i]), isNull);
        }
      });

      test('a duplicate fragment does not double-count or misdeliver', () {
        final payload = Uint8List.fromList(
            List.generate(400, (i) => i % 256));
        final result =
            handler.fragment(payload: payload, messageId: messageId, maxChunk: maxChunk);
        final decoded = [
          for (final f in result.fragments)
            FragmentHandler.decodeFragment(f.payload)
        ];
        expect(handler.addFragment(decoded[0]), isNull);
        expect(handler.addFragment(decoded[0]), isNull); // duplicate
        final done = handler.addFragment(decoded[1]);
        expect(done, isNotNull);
        expect(done!.payload, payload);
      });
    });

    group('fragment ack codec', () {
      test('round-trips messageId and index', () {
        final bytes =
            FragmentHandler.encodeFragmentAck(messageId: messageId, index: 42);
        final (id, index) = FragmentHandler.decodeFragmentAck(bytes);
        expect(id, messageId);
        expect(index, 42);
      });
    });
  });
}
