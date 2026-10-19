import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/src/transport/hole_punch_service.dart';

void main() {
  group('PunchPacket', () {
    test('has correct magic bytes', () {
      final pubkey = Uint8List(32);
      for (int i = 0; i < 32; i++) pubkey[i] = i;

      final packet = PunchPacket.create(pubkey);
      final bytes = packet.serialize();

      // Magic: "GRPU" (0x47, 0x52, 0x50, 0x55)
      expect(bytes[0], equals(0x47));
      expect(bytes[1], equals(0x52));
      expect(bytes[2], equals(0x50));
      expect(bytes[3], equals(0x55));
    });

    test('contains sender pubkey', () {
      final pubkey = Uint8List(32);
      for (int i = 0; i < 32; i++) pubkey[i] = i + 10;

      final packet = PunchPacket.create(pubkey);
      final bytes = packet.serialize();

      // Pubkey starts at offset 4
      expect(bytes.sublist(4, 36), equals(pubkey));
    });

    test('total size is 36 bytes (4 magic + 32 pubkey)', () {
      final pubkey = Uint8List(32);
      final packet = PunchPacket.create(pubkey);
      expect(packet.serialize().length, equals(36));
    });

    test('deserialize valid packet', () {
      final pubkey = Uint8List(32);
      for (int i = 0; i < 32; i++) pubkey[i] = 0xAB;

      final original = PunchPacket.create(pubkey);
      final parsed = PunchPacket.tryParse(original.serialize());

      expect(parsed, isNotNull);
      expect(parsed!.senderPubkey, equals(pubkey));
    });

    test('tryParse returns null for wrong magic', () {
      final bytes = Uint8List(36);
      bytes[0] = 0xFF; // wrong magic
      expect(PunchPacket.tryParse(bytes), isNull);
    });

    test('tryParse returns null for too-short data', () {
      expect(PunchPacket.tryParse(Uint8List(10)), isNull);
    });

    test('tryParse returns null for empty data', () {
      expect(PunchPacket.tryParse(Uint8List(0)), isNull);
    });
  });

  group('HolePunchService', () {
    late RawDatagramSocket socket;
    late HolePunchService service;
    final senderPubkey = Uint8List(32)..fillRange(0, 32, 0xAA);

    setUp(() async {
      socket = await RawDatagramSocket.bind(InternetAddress.loopbackIPv6, 0);
      service = HolePunchService(socket: socket, senderPubkey: senderPubkey);
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

      // Each should be a valid punch packet
      for (final dg in receivedPackets) {
        final parsed = PunchPacket.tryParse(dg.data);
        expect(parsed, isNotNull);
        expect(parsed!.senderPubkey, equals(senderPubkey));
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

      // Start a long punch
      final future = service.punch(
        InternetAddress.loopbackIPv6,
        receiver.port,
        duration: const Duration(seconds: 10),
        interval: const Duration(milliseconds: 50),
      );

      // Cancel after 200ms
      await Future.delayed(const Duration(milliseconds: 200));
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
