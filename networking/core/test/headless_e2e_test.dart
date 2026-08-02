import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// End-to-end proof of the headless embedding (GPW cross-project request,
/// Answer 2): a headless initiator pushes a payload to a headless service
/// with `putPeerAddress` + `send`; under Closed trust the registered key
/// delivers and an unregistered key cannot complete a session.
///
/// Needs a native libsodium (`brew install libsodium` / `apt-get install
/// libsodium23`, or LIBSODIUM_PATH) — the suite self-skips when absent.
void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping headless e2e: $e';
    }
  });

  Future<GrassrootsIdentity> identityFromSeed(int fill, String name) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
      nickname: name,
    );
  }

  test('registered initiator delivers under Closed trust; stranger cannot '
      'complete a session', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final service = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(0x22, 'service'),
      sodium: sodium!,
    );
    final pusher = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(0x11, 'pusher'),
      sodium: sodium!,
    );
    final stranger = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(0x33, 'stranger'),
      sodium: sodium!,
    );
    addTearDown(() async {
      await pusher.dispose();
      await stranger.dispose();
      await service.dispose();
    });

    // Service side: pusher registered, stranger not. Unsolicited inbound IP
    // contact is governed by no trust level — the known-peer set is the whole
    // of what the service will answer.
    service.putKnownPeer(pusher.identity.publicKey);

    final delivered = <(Uint8List sender, String payload)>[];
    final serviceConnected = <String>[];
    service.onMessageReceived = (messageId, sender, payload, transport) {
      delivered.add((sender, utf8.decode(payload)));
    };
    service.onPeerConnected = (pk, transport, _) {
      serviceConnected.add(_hex(pk));
    };

    expect(await service.start(), isTrue);
    expect(await pusher.start(), isTrue);
    expect(await stranger.start(), isTrue);
    final address = '127.0.0.1:${service.boundPort}';

    // The spec sequence: putPeerAddress supplies key and address; send
    // dials, establishes the session, delivers.
    final pusherLinked = Completer<void>();
    pusher.onPeerConnected = (pk, transport, _) {
      expect(_hex(pk), _hex(service.identity.publicKey));
      expect(transport, MessageTransport.udp);
      if (!pusherLinked.isCompleted) pusherLinked.complete();
    };
    pusher.putPeerAddress(service.identity.publicKey, address);
    final messageId = await pusher.send(
      service.identity.publicKey,
      Uint8List.fromList(utf8.encode('gpw/area-push/1 payload')),
    );
    expect(messageId, isNotNull);

    await pusherLinked.future.timeout(const Duration(seconds: 15));
    // Delivery follows the session; poll briefly for the router to surface it.
    final deadline = DateTime.now().add(const Duration(seconds: 10));
    while (delivered.isEmpty && DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 100));
    }
    expect(delivered, hasLength(1));
    expect(_hex(delivered.single.$1), _hex(pusher.identity.publicKey));
    expect(delivered.single.$2, 'gpw/area-push/1 payload');
    expect(serviceConnected, contains(_hex(pusher.identity.publicKey)));
    expect(service.isPeerReachable(pusher.identity.publicKey), isTrue);

    // Stranger: same sequence, unregistered key — the service drops its
    // ANNOUNCE and refuses its handshake; no session, no delivery.
    var strangerLinked = false;
    stranger.onPeerConnected = (_, __, ___) => strangerLinked = true;
    stranger.putPeerAddress(service.identity.publicKey, address);
    final strangerMessage = await stranger.send(
      service.identity.publicKey,
      Uint8List.fromList(utf8.encode('stranger payload')),
    );
    expect(strangerMessage, isNotNull); // queued, never delivered
    await Future<void>.delayed(const Duration(seconds: 3));
    expect(strangerLinked, isFalse,
        reason: 'unregistered key must not complete a session under Closed');
    expect(
      service.isPeerReachable(stranger.identity.publicKey),
      isFalse,
    );
    expect(delivered, hasLength(1),
        reason: 'stranger payload must not be delivered');

    // Revocation: removeKnownPeer withdraws the pusher.
    service.removeKnownPeer(pusher.identity.publicKey);
    expect(service.isPeerReachable(pusher.identity.publicKey), isFalse);
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('a pre-registered initiator is admitted', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final service = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(0x44, 'registering-service'),
      sodium: sodium!,
    );
    final caller = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(0x55, 'caller'),
      sodium: sodium!,
    );
    addTearDown(() async {
      await caller.dispose();
      await service.dispose();
    });
    // No trust level governs unsolicited inbound IP contact, so a caller the
    // service must answer is registered first — there is no level that would
    // admit it otherwise.
    service.putKnownPeer(caller.identity.publicKey);
    final delivered = Completer<String>();
    service.onMessageReceived = (messageId, sender, payload, transport) {
      if (!delivered.isCompleted) delivered.complete(utf8.decode(payload));
    };
    expect(await service.start(), isTrue);
    expect(await caller.start(), isTrue);
    caller.putPeerAddress(
      service.identity.publicKey,
      '127.0.0.1:${service.boundPort}',
    );
    await caller.send(
      service.identity.publicKey,
      Uint8List.fromList(utf8.encode('registered call')),
    );
    expect(
      await delivered.future.timeout(const Duration(seconds: 15)),
      'registered call',
    );
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('file-backed known peers persist across instances', () async {
    final dir = await Directory.systemTemp.createTemp('known-peers');
    addTearDown(() => dir.delete(recursive: true));
    final path = '${dir.path}/peers.json';
    FileKnownPeersStore(path).save({'aa' * 32: '203.0.113.9:9600'});
    final restored = FileKnownPeersStore(path).load();
    expect(restored, {'aa' * 32: '203.0.113.9:9600'});
  });
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
