/// The wire hookup of the push channel (GPW Stage 4): GPW's registrar and
/// mirror writer wired to GLP-Networking-API's headless server profile
/// (`HeadlessGrassrootsNetwork`, package `grassroots_networking_core`).
/// GPW consumes the public API only: construction with the service identity,
/// Closed trust, `putKnownPeer`/`removeKnownPeer` driven by the bindings,
/// `onMessageReceived` feeding the verified mirror write, `send` returning
/// the gpw/push-ack/1.
library;

import 'dart:convert';
import 'dart:typed_data';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_core/src/headless/identity_file.dart';

import 'crypto.dart';
import 'push.dart';

/// The bindings-driven known-peer registry, backed by the layer.
class LayerRegistrar implements PushRegistrar {
  LayerRegistrar(this.network);

  final HeadlessGrassrootsNetwork network;

  @override
  void register(String publicKeyB64) =>
      network.putKnownPeer(unb64url(publicKeyB64));

  @override
  void unregister(String publicKeyB64) =>
      network.removeKnownPeer(unb64url(publicKeyB64));
}

/// The running push service: the layer under Closed trust, receiving pushes
/// into [writer] and acking each.
class PushService {
  PushService._(this.network, this.writer);

  final HeadlessGrassrootsNetwork network;
  final MirrorWriter writer;

  String get publicKeyB64 => b64url(network.identity.publicKey);

  static Future<PushService> start({
    required String identityPath,
    required int port,
    required String publicAddress,
    required MirrorWriter writer,
    String? knownPeersPath,
  }) async {
    final sodium = await initHeadlessSodium();
    final identity =
        await loadOrCreateIdentity(identityPath, nickname: 'gpw-mirror');
    final network = HeadlessGrassrootsNetwork(
      identity: identity,
      sodium: sodium,
      listenPort: port,
      staticPublicAddress: publicAddress,
      knownPeersStore: knownPeersPath == null
          ? null
          : FileKnownPeersStore(knownPeersPath),
    );
    network.setTrustLevel(ColdCallTrustLevel.closed);
    final service = PushService._(network, writer);
    network.onMessageReceived =
        (messageId, senderPk, payload, transport) async {
      final result =
          await writer.applyPush(b64url(senderPk), payload);
      await network.send(
          senderPk, Uint8List.fromList(utf8.encode(jsonEncode(result.toJson()))));
    };
    if (!await network.start()) {
      throw StateError('push service: failed to bind UDP port $port');
    }
    return service;
  }

  Future<void> dispose() => network.dispose();
}
