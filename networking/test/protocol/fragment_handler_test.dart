import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking/src/models/packet.dart';

void main() {
  group('FragmentHandler', () {
    late FragmentHandler handler;

    setUp(() {
      handler = FragmentHandler();
    });

    tearDown(() {
      handler.dispose();
    });

    group('needsFragmentation', () {
      test('returns false for small payload', () {
        final small = Uint8List(100);
        expect(handler.needsFragmentation(small), isFalse);
      });

      test('returns false for payload at threshold', () {
        final atThreshold = Uint8List(FragmentHandler.fragmentThreshold);
        expect(handler.needsFragmentation(atThreshold), isFalse);
      });

      test('returns true for payload above threshold', () {
        final large = Uint8List(FragmentHandler.fragmentThreshold + 1);
        expect(handler.needsFragmentation(large), isTrue);
      });
    });

    group('fragment', () {
      test('throws for payload that does not need fragmentation', () {
        final small = Uint8List(100);
        expect(
          () => handler.fragment(payload: small),
          throwsArgumentError,
        );
      });

      test('creates correct number of fragments for large payload', () {
        final payload = Uint8List(1000);
        for (var i = 0; i < payload.length; i++) {
          payload[i] = i % 256;
        }

        final result = handler.fragment(payload: payload);

        final expectedFragments =
            (1000 / FragmentHandler.maxFragmentPayload).ceil();
        expect(result.fragments.length, equals(expectedFragments));
        expect(result.messageId, isNotEmpty);
      });

      test('first fragment has fragmentStart type', () {
        final payload = Uint8List(1000);
        final result = handler.fragment(payload: payload);

        expect(result.fragments.first.type, equals(PacketType.fragmentStart));
      });

      test('last fragment has fragmentEnd type', () {
        final payload = Uint8List(1000);
        final result = handler.fragment(payload: payload);

        expect(result.fragments.last.type, equals(PacketType.fragmentEnd));
      });

      test('middle fragments have fragmentContinue type', () {
        // Need at least 3 fragments: payload > 2 * maxFragmentPayload
        final payload = Uint8List(FragmentHandler.maxFragmentPayload * 3);
        final result = handler.fragment(payload: payload);

        expect(result.fragments.length, greaterThanOrEqualTo(3));
        for (var i = 1; i < result.fragments.length - 1; i++) {
          expect(result.fragments[i].type, equals(PacketType.fragmentContinue));
        }
      });

      test('generates unique message IDs', () {
        final payload = Uint8List(1000);
        final result1 = handler.fragment(payload: payload);
        final result2 = handler.fragment(payload: payload);

        expect(result1.messageId, isNot(equals(result2.messageId)));
      });
    });

    group('processFragment - reassembly', () {
      test('reassembles complete message from in-order fragments', () {
        final payload = Uint8List(1000);
        for (var i = 0; i < payload.length; i++) {
          payload[i] = i % 256;
        }

        final fragmented = handler.fragment(payload: payload);

        // Feed fragments to a separate handler (simulating the receiver)
        final receiver = FragmentHandler();
        addTearDown(receiver.dispose);

        ReassembledMessage? result;
        for (final fragment in fragmented.fragments) {
          result = receiver.processFragment(fragment);
        }

        expect(result, isNotNull);
        expect(result!.payload, equals(payload));
      });

      test('reassembles complete message from out-of-order fragments', () {
        final payload = Uint8List(1500);
        for (var i = 0; i < payload.length; i++) {
          payload[i] = i % 256;
        }

        final fragmented = handler.fragment(payload: payload);

        final receiver = FragmentHandler();
        addTearDown(receiver.dispose);

        // Send start first (required to create reassembly state)
        receiver.processFragment(fragmented.fragments.first);

        // Send end before middle fragments
        final lastFragment = fragmented.fragments.last;

        // Send middle fragments
        for (var i = 1; i < fragmented.fragments.length - 1; i++) {
          final result = receiver.processFragment(fragmented.fragments[i]);
          expect(result, isNull);
        }

        // Send end last - should trigger reassembly
        final result = receiver.processFragment(lastFragment);
        expect(result, isNotNull);
        expect(result!.payload, equals(payload));
      });

      test('handles duplicate fragments idempotently', () {
        final payload = Uint8List(1000);
        for (var i = 0; i < payload.length; i++) {
          payload[i] = i % 256;
        }

        final fragmented = handler.fragment(payload: payload);

        final receiver = FragmentHandler();
        addTearDown(receiver.dispose);

        // Send first fragment twice
        receiver.processFragment(fragmented.fragments.first);
        receiver.processFragment(fragmented.fragments.first);

        // Send remaining fragments
        ReassembledMessage? result;
        for (var i = 1; i < fragmented.fragments.length; i++) {
          result = receiver.processFragment(fragmented.fragments[i]);
        }

        expect(result, isNotNull);
        expect(result!.payload, equals(payload));
      });

      test('returns null for incomplete fragments', () {
        final payload = Uint8List(1000);
        final fragmented = handler.fragment(payload: payload);

        final receiver = FragmentHandler();
        addTearDown(receiver.dispose);

        // Only send first fragment
        final result = receiver.processFragment(fragmented.fragments.first);
        expect(result, isNull);
      });

      test('returns null for continue fragment without start', () {
        final payload = Uint8List(1500);
        final fragmented = handler.fragment(payload: payload);

        final receiver = FragmentHandler();
        addTearDown(receiver.dispose);

        // Skip start, send continue directly
        expect(fragmented.fragments.length, greaterThan(2));
        final result = receiver.processFragment(fragmented.fragments[1]);
        expect(result, isNull);
      });

      test('returns null for end fragment without start', () {
        final payload = Uint8List(1000);
        final fragmented = handler.fragment(payload: payload);

        final receiver = FragmentHandler();
        addTearDown(receiver.dispose);

        // Only send last fragment (no start)
        final result = receiver.processFragment(fragmented.fragments.last);
        expect(result, isNull);
      });

      test('throws for non-fragment packet', () {
        final packet = GrassrootsPacket(
          type: PacketType.message,
          payload: Uint8List(10),
        );

        expect(
          () => handler.processFragment(packet),
          throwsArgumentError,
        );
      });
    });

    group('round-trip', () {
      test('fragment and reassemble preserves payload exactly', () {
        // Test with various payload sizes above threshold
        for (final size in [501, 900, 1000, 2000, 5000]) {
          final payload = Uint8List(size);
          for (var i = 0; i < size; i++) {
            payload[i] = i % 256;
          }

          final fragmented = handler.fragment(payload: payload);

          final receiver = FragmentHandler();
          ReassembledMessage? result;
          for (final fragment in fragmented.fragments) {
            result = receiver.processFragment(fragment);
          }
          receiver.dispose();

          expect(result, isNotNull, reason: 'Failed for size=$size');
          expect(result!.payload.length, equals(size),
              reason: 'Length mismatch for size=$size');
          expect(result.payload, equals(payload),
              reason: 'Content mismatch for size=$size');
        }
      });

      test('multiple concurrent reassemblies', () {
        final receiver = FragmentHandler();
        addTearDown(receiver.dispose);

        // Fragment two different messages
        final payload1 = Uint8List(1000);
        for (var i = 0; i < 1000; i++) {
          payload1[i] = i % 256;
        }

        final payload2 = Uint8List(1200);
        for (var i = 0; i < 1200; i++) {
          payload2[i] = (i * 7) % 256;
        }

        final fragmented1 = handler.fragment(payload: payload1);
        final fragmented2 = handler.fragment(payload: payload2);

        // Interleave fragments: start1, start2, continue1, continue2, end1, end2
        receiver.processFragment(fragmented1.fragments[0]);
        receiver.processFragment(fragmented2.fragments[0]);

        // Send middle fragments of both
        for (var i = 1; i < fragmented1.fragments.length - 1; i++) {
          receiver.processFragment(fragmented1.fragments[i]);
        }
        for (var i = 1; i < fragmented2.fragments.length - 1; i++) {
          receiver.processFragment(fragmented2.fragments[i]);
        }

        // End fragments
        final result1 = receiver.processFragment(fragmented1.fragments.last);
        final result2 = receiver.processFragment(fragmented2.fragments.last);

        expect(result1?.payload, equals(payload1));
        expect(result2?.payload, equals(payload2));
      });
    });

    group('dispose', () {
      test('cleans up without errors', () {
        final h = FragmentHandler();
        // Fragment something to populate internal state
        final payload = Uint8List(1000);
        h.fragment(payload: payload);
        expect(() => h.dispose(), returnsNormally);
      });

      test('double dispose is safe', () {
        final h = FragmentHandler();
        h.dispose();
        expect(() => h.dispose(), returnsNormally);
      });
    });
  });
}
