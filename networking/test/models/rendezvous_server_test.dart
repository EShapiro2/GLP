import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/src/models/rendezvous_server.dart';

/// A rendezvous server as the layer is instantiated with it (GNA
/// §Instantiation: "Rendezvous servers: list of (ip:port, PubKey)").  Until
/// 2026-10-12 the servers were settings, added after construction once one
/// answered a probe; the acceptance of a probe's reply before the server was
/// stored went with that.
void main() {
  final key = Uint8List.fromList(List.filled(32, 0xaa));

  group('RendezvousServer', () {
    test('keeps its address and its key', () {
      final server = RendezvousServer('[2001:db8::10]:9516', key);

      expect(server.address, '[2001:db8::10]:9516');
      expect(server.publicKey, key);
      expect(server.pubkeyHex, 'aa' * 32);
    });

    test('refuses an unparseable address', () {
      expect(() => RendezvousServer('rendezvous.example', key),
          throwsArgumentError);
      expect(() => RendezvousServer('2001:db8::10:9516', key),
          throwsArgumentError,
          reason: 'an IPv6 address is bracketed');
    });

    test('refuses a key that is not 32 bytes', () {
      expect(() => RendezvousServer('198.51.100.20:9514', Uint8List(31)),
          throwsArgumentError);
    });

    test('is equal by address and key', () {
      expect(RendezvousServer('198.51.100.20:9514', key),
          RendezvousServer('198.51.100.20:9514', Uint8List.fromList(key)));
      expect(RendezvousServer('198.51.100.20:9514', key),
          isNot(RendezvousServer('198.51.100.21:9514', key)));
    });
  });
}
