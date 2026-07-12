import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:gpw_name_service/gpw_name_service.dart';

import 'service_test.dart' show signed, manifestBody, zone;

/// GPW Stage 4 end to end, in process: the name service's bindings drive the
/// layer's known peers (registration rule); a phone-side push over a real
/// authenticated loopback session lands, verified, in the mirror; a
/// stranger's cannot complete a session.  Self-skips without libsodium.
void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping push e2e: $e';
    }
  });

  test('bindings-driven push channel: registered phone lands a verified '
      'area; stranger refused', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final tmp = Directory.systemTemp.createTempSync('gpw_e2e');
    addTearDown(() => tmp.deleteSync(recursive: true));

    // The person: one key for signing and for the layer (the key is the
    // person).
    final person = await SigningKey.generate();
    final personIdentity = await GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(
          Uint8List.fromList(await person.seed())),
      nickname: 'phone',
    );

    // The mirroring service agent.
    final serviceNet = HeadlessGrassrootsNetwork(
      identity: await GrassrootsIdentity.generate(nickname: 'gpw-mirror'),
      sodium: sodium!,
    );
    serviceNet.setTrustLevel(ColdCallTrustLevel.closed);
    final mirrors = Directory('${tmp.path}/mirrors')..createSync();
    final store = NameStore(tmp.path);
    final writer = MirrorWriter(zone: zone, store: store, mirrorsDir: mirrors);
    final service = NameService(
      zone: zone,
      store: store,
      zoneWriter: FakeZoneWriter(),
      serverKey: await SigningKey.generate(),
      registrar: LayerRegistrar(serviceNet),
    );
    serviceNet.onMessageReceived = (messageId, senderPk, payload, _) async {
      final result = await writer.applyPush(b64url(senderPk), payload);
      await serviceNet.send(senderPk,
          Uint8List.fromList(utf8.encode(jsonEncode(result.toJson()))));
    };
    expect(await serviceNet.start(), isTrue);
    final address = '127.0.0.1:${serviceNet.boundPort}';

    // The deposit registers the phone's key with the layer (registration
    // rule) — no direct putKnownPeer anywhere in this test.
    final deposit =
        await service.deposit('me', await signed(person, manifestBody(person)));
    expect(deposit.status, 201);

    // Phone side: signed area, putPeerAddress + send, wait for the ack.
    final area = Directory('${tmp.path}/area')..createSync();
    File('${area.path}/index.html').writeAsStringSync('<p>pushed v1</p>');
    await signArea(area, person, 'me.$zone', 1);

    final phone = HeadlessGrassrootsNetwork(
        identity: personIdentity, sodium: sodium!);
    addTearDown(() async {
      await phone.dispose();
      await serviceNet.dispose();
    });
    final ack = Completer<Map>();
    phone.onMessageReceived = (messageId, senderPk, payload, _) {
      final m = jsonDecode(utf8.decode(payload)) as Map;
      if (m['format'] == 'gpw/push-ack/1' && !ack.isCompleted) {
        ack.complete(m);
      }
    };
    expect(await phone.start(), isTrue);
    phone.putPeerAddress(serviceNet.identity.publicKey, address);
    await phone.send(serviceNet.identity.publicKey,
        Uint8List.fromList(buildAreaPush(area, 'me.$zone')));

    final result = await ack.future.timeout(const Duration(seconds: 20));
    expect(result['accepted'], isTrue);
    expect(result['areaEpoch'], 1);

    // The pushed area landed and verifies clean.
    expect(File('${mirrors.path}/me.$zone/index.html').readAsStringSync(),
        '<p>pushed v1</p>');
    Future<List<int>?> fetch(String path) async {
      final f = File('${mirrors.path}/me.$zone$path');
      return f.existsSync() ? f.readAsBytesSync() : null;
    }

    expect(await verifyArea(fetch, person.publicKeyB64), isEmpty);

    // A stranger cannot complete a session, so nothing reaches the writer.
    final stranger = HeadlessGrassrootsNetwork(
        identity: await GrassrootsIdentity.generate(nickname: 'stranger'),
        sodium: sodium!);
    addTearDown(() => stranger.dispose());
    var strangerLinked = false;
    stranger.onPeerConnected = (_, __) => strangerLinked = true;
    expect(await stranger.start(), isTrue);
    stranger.putPeerAddress(serviceNet.identity.publicKey, address);
    await stranger.send(serviceNet.identity.publicKey,
        Uint8List.fromList(utf8.encode('{"format":"gpw/area-push/1"}')));
    await Future<void>.delayed(const Duration(seconds: 3));
    expect(strangerLinked, isFalse);
  }, timeout: const Timeout(Duration(minutes: 2)));
}
