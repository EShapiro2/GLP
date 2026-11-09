import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/src/transport/hole_punch_service.dart';

void main() {
  group('PunchPacket', () {
    test('is the GRPU magic alone', () {
      final bytes = PunchPacket.serialize();

      // Magic: "GRPU" (0x47, 0x52, 0x50, 0x55)
      expect(bytes, orderedEquals([0x47, 0x52, 0x50, 0x55]));
      expect(PunchPacket.packetSize, equals(4));
    });

    test('isPunch accepts the serialized packet', () {
      expect(PunchPacket.isPunch(PunchPacket.serialize()), isTrue);
    });

    test('isPunch rejects wrong magic', () {
      expect(PunchPacket.isPunch(Uint8List.fromList([0xFF, 0x52, 0x50, 0x55])),
          isFalse);
    });

    test('isPunch rejects the magic followed by anything', () {
      final withKey = Uint8List(36)
        ..setRange(0, 4, [0x47, 0x52, 0x50, 0x55]);
      expect(PunchPacket.isPunch(withKey), isFalse);
    });

    test('isPunch rejects too-short and empty data', () {
      expect(PunchPacket.isPunch(Uint8List(3)), isFalse);
      expect(PunchPacket.isPunch(Uint8List(0)), isFalse);
    });
  });

  group('HolePunchService', () {
    late RawDatagramSocket socket;
    late HolePunchService service;

    setUp(() async {
      socket = await RawDatagramSocket.bind(InternetAddress.loopbackIPv6, 0);
      service = HolePunchService(socket: socket);
    });

    tearDown(() {
      service.dispose();
      socket.close();
    });

    test('punch sends packets to target address', () async {
      // Set up a receiver to count packets
      final receiver =
          await RawDatagramSocket.bind(InternetAddress.loopbackIPv6, 0);
      final receivedPackets = <Datagram>[];

      receiver.listen((event) {
        if (event == RawSocketEvent.read) {
          final dg = receiver.receive();
          if (dg != null) receivedPackets.add(dg);
        }
      });

      await service.punch(
        InternetAddress.loopbackIPv6,
        receiver.port,
        duration: const Duration(milliseconds: 500),
        interval: const Duration(milliseconds: 100),
      );

      receiver.close();

      // Should have sent ~5 packets (500ms / 100ms)
      expect(receivedPackets.length, greaterThanOrEqualTo(3));
      expect(receivedPackets.length, lessThanOrEqualTo(7));

      // Each is the bare GRPU magic, with no key in it
      for (final dg in receivedPackets) {
        expect(dg.data, orderedEquals([0x47, 0x52, 0x50, 0x55]));
      }
    });

    test('punch completes after duration', () async {
      final receiver =
          await RawDatagramSocket.bind(InternetAddress.loopbackIPv6, 0);

      final stopwatch = Stopwatch()..start();
      await service.punch(
        InternetAddress.loopbackIPv6,
        receiver.port,
        duration: const Duration(milliseconds: 300),
        interval: const Duration(milliseconds: 50),
      );
      stopwatch.stop();

      receiver.close();

      expect(stopwatch.elapsedMilliseconds, greaterThanOrEqualTo(250));
      expect(stopwatch.elapsedMilliseconds, lessThan(600));
    });

    test('punch can be cancelled via dispose', () async {
      final receiver =
          await RawDatagramSocket.bind(InternetAddress.loopbackIPv6, 0);
      final firstPacket = Completer<void>();
      receiver.listen((event) {
        if (event == RawSocketEvent.read &&
            receiver.receive() != null &&
            !firstPacket.isCompleted) {
          firstPacket.complete();
        }
      });

      // Start a long punch
      final future = service.punch(
        InternetAddress.loopbackIPv6,
        receiver.port,
        duration: const Duration(seconds: 10),
        interval: const Duration(milliseconds: 50),
      );

      // Cancel once the punch is under way: its first packet has arrived.
      await firstPacket.future;
      service.dispose();

      // Should complete quickly after dispose
      await future.timeout(
        const Duration(milliseconds: 500),
        onTimeout: () => fail('punch did not cancel after dispose'),
      );

      receiver.close();
    });
  });
}
