import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// The three layer functions the runtime seam needs and the layer did not
/// expose: `sign` and `verify` (GLP Networking API §System Predicates — the
/// runtime realizes Secure GLP's `sign/2` and `verify/3` over them, the layer
/// never learning about modules), and `punchUdp` (§System Predicates, backing
/// `punch_udp/1`).
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  // GrassrootsNetwork's constructor subscribes to connectivity changes;
  // stub the plugin channels so construction works in a test isolate.
  setUp(() {
    final messenger =
        TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger;
    messenger.setMockMethodCallHandler(
      const MethodChannel('dev.fluttercommunity.plus/connectivity'),
      (call) async => ['wifi'],
    );
    messenger.setMockStreamHandler(
      const EventChannel('dev.fluttercommunity.plus/connectivity_status'),
      MockStreamHandler.inline(onListen: (arguments, events) {}),
    );
  });

  Future<GrassrootsIdentity> identity(String nickname) async {
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPair(),
      nickname: nickname,
    );
  }

  GrassrootsNetwork networkFor(GrassrootsIdentity self) => GrassrootsNetwork(
        identity: self,
        store: Store<AppState>(appReducer, initialState: const AppState()),
        sodium: sodium,
      );

  final message = Uint8List.fromList('attest(alice, bob)'.codeUnits);

  test('sign produces a signature verify accepts under the signer key',
      () async {
    final self = await identity('Self');
    final network = networkFor(self);

    final signature = network.sign(message);

    // Ed25519 detached signatures are 64 bytes.
    expect(signature.length, equals(64));
    expect(
      network.verify(Uint8List.fromList(self.publicKey), message, signature),
      isTrue,
    );
  });

  test('verify rejects a signature by another key', () async {
    final self = await identity('Self');
    final other = await identity('Other');
    final network = networkFor(self);

    final signature = network.sign(message);

    expect(
      network.verify(Uint8List.fromList(other.publicKey), message, signature),
      isFalse,
    );
  });

  test('verify rejects a signature over a different message', () async {
    final self = await identity('Self');
    final network = networkFor(self);

    final signature = network.sign(message);
    final tampered = Uint8List.fromList('attest(alice, carol)'.codeUnits);

    expect(
      network.verify(Uint8List.fromList(self.publicKey), tampered, signature),
      isFalse,
    );
  });

  test('verify returns false on malformed input rather than throwing',
      () async {
    final self = await identity('Self');
    final network = networkFor(self);

    // A signature of the wrong length, and a key of the wrong length: both
    // are the same answer to the caller as a bad signature.
    expect(
      network.verify(
        Uint8List.fromList(self.publicKey),
        message,
        Uint8List.fromList(const [1, 2, 3]),
      ),
      isFalse,
    );
    expect(
      network.verify(
        Uint8List.fromList(const [1, 2, 3]),
        message,
        network.sign(message),
      ),
      isFalse,
    );
  });

  test('sign and verify are synchronous — no Future is returned', () async {
    final self = await identity('Self');
    final network = networkFor(self);

    // The seam declares both synchronous; a Future here would break it.
    final Uint8List signature = network.sign(message);
    final bool ok =
        network.verify(Uint8List.fromList(self.publicKey), message, signature);

    expect(signature, isA<Uint8List>());
    expect(ok, isTrue);
  });

  test('punchUdp rejects an unparseable address', () async {
    final self = await identity('Self');
    final network = networkFor(self);

    expect(() => network.punchUdp('not-an-address'), throwsArgumentError);
    expect(() => network.punchUdp(''), throwsArgumentError);
    expect(() => network.punchUdp('203.0.113.5'), throwsArgumentError);
    // Unbracketed IPv6 is not an address the layer accepts anywhere.
    expect(() => network.punchUdp('2001:db8::1:4242'), throwsArgumentError);
  });

  test('punchUdp accepts a well-formed address and returns at once', () async {
    final self = await identity('Self');
    final network = networkFor(self);

    // No UDP transport is started here, so there is no hole-punch service for
    // either family: the punch is a no-op. What the call must not do is throw
    // or block — it is fire-and-forget, and a NAT mapping reports nothing.
    expect(() => network.punchUdp('203.0.113.5:4242'), returnsNormally);
    expect(() => network.punchUdp('[2001:db8::1]:4242'), returnsNormally);
  });
}
