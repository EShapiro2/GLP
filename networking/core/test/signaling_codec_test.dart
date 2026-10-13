import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/signaling/signaling_codec.dart';

/// The signaling codec: what an agent's layer and a rendezvous server's
/// exchange --- the reflected address, and the invite registration and
/// redemption of a WAN cold call (GNA §4.4) --- and nothing else.  The retired
/// message types stay retired on the wire: the friend-driven RV_LIST and
/// FRIEND_LIST (carried over from the Flutter package's signaling test), and
/// the transport's reconnection protocol, PUNCH_INITIATE, PUNCH_READY,
/// RECONNECT and AVAILABLE, retired 2026-10-13 for the GLP rendezvous agent
/// (GNA §4.2, §6).
void main() {
  const codec = SignalingCodec();

  test('ADDR_REFLECT round-trips', () {
    final decoded =
        codec.decode(codec.encode(AddrReflectMessage(ip: '2400::12', port: 60123)));
    expect(decoded, isA<AddrReflectMessage>());
    final reflect = decoded as AddrReflectMessage;
    expect(reflect.ip, '2400::12');
    expect(reflect.port, 60123);
  });

  for (final (name, byte) in [
    ('PUNCH_INITIATE', 0x05),
    ('PUNCH_READY', 0x06),
    ('RECONNECT', 0x08),
    ('AVAILABLE', 0x09),
    ('RV_LIST', 0x0a),
    ('FRIEND_LIST', 0x0b),
  ]) {
    test('rejects the retired $name wire byte '
        '(0x${byte.toRadixString(16).padLeft(2, '0')})', () {
      expect(
        () => codec.decode(Uint8List.fromList([byte, ...List.filled(64, 0)])),
        throwsArgumentError,
      );
    });
  }
}
