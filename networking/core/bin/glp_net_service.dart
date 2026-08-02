/// Headless service entrypoint for the Grassroots networking layer
/// (spec `GLP_Networking_API`; GPW cross-project request, Answer 2).
///
/// Embeds [HeadlessGrassrootsNetwork] as a receiving service: binds a fixed
/// UDP port, runs at Closed trust by default (unregistered keys cannot
/// complete a session), registers known peers from the command line and a
/// persisted file, and prints every delivered payload. A consumer such as
/// GPW's mirroring service embeds the same class and replaces the print with
/// its registrar (`onMessageReceived`).
///
/// Compile: `dart compile exe bin/glp_net_service.dart -o glp_net_service`
///
/// Usage:
///   glp_net_service --port 9600 --identity service-identity.json \
///     [--public-address 203.0.113.7:9600] \
///     [--known-peer <pubkey-hex>]... [--known-peers-file peers.json] \
///     [--trust closed|open]
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:args/args.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_core/src/headless/identity_file.dart';

Future<void> main(List<String> argv) async {
  final parser = ArgParser()
    ..addOption('port', defaultsTo: '9600', help: 'UDP port to bind')
    ..addOption('identity',
        defaultsTo: 'service-identity.json',
        help: 'Path to identity file (created on first run)')
    ..addOption('public-address',
        help: 'The service\'s static public ip:port (reported by '
            'getPublicAddress)')
    ..addMultiOption('known-peer',
        help: 'Public key (64 hex chars) this service will answer; '
            'repeatable. Unsolicited contact from any other key is refused')
    ..addOption('known-peers-file',
        help: 'JSON file persisting the known-peer set across restarts '
            '(default: in-memory only)')
    ..addFlag('help', abbr: 'h', negatable: false);

  final ArgResults args;
  try {
    args = parser.parse(argv);
  } on FormatException catch (e) {
    stderr.writeln(e.message);
    stderr.writeln(parser.usage);
    exitCode = 64;
    return;
  }
  if (args['help'] as bool) {
    print(parser.usage);
    return;
  }

  final sodium = await initHeadlessSodium();
  final identity = await loadOrCreateIdentity(
    args['identity'] as String,
    nickname: 'service',
  );
  print('Service identity: ${_hex(identity.publicKey)}');

  final knownPeersFile = args['known-peers-file'] as String?;
  final network = HeadlessGrassrootsNetwork(
    identity: identity,
    sodium: sodium,
    listenPort: int.parse(args['port'] as String),
    staticPublicAddress: args['public-address'] as String?,
    knownPeersStore: knownPeersFile != null
        ? FileKnownPeersStore(knownPeersFile)
        : MemoryKnownPeersStore(),
  );

  for (final hex in args['known-peer'] as List<String>) {
    network.putKnownPeer(_bytes(hex));
    print('Known peer registered: ${hex.substring(0, 8)}…');
  }

  network.onPeerConnected = (pk, transport, _) =>
      print('Peer connected (${transport.name}): ${_hex(pk)}');
  network.onPeerDisconnected = (pk, transport) =>
      print('Peer disconnected (${transport.name}): ${_hex(pk)}');
  network.onMessageReceived = (messageId, sender, payload, transport) {
    String rendering;
    try {
      rendering = utf8.decode(payload);
    } catch (_) {
      rendering = '<${payload.length} bytes>';
    }
    print('Message $messageId from ${_hex(sender).substring(0, 8)}…: '
        '$rendering');
  };

  if (!await network.start()) {
    stderr.writeln('Failed to bind UDP port ${args['port']}');
    exitCode = 1;
    return;
  }
  print('Listening on UDP port ${network.boundPort} '
      '(trust: ${args['trust']})');

  // Run until terminated.
  final done = Completer<void>();
  ProcessSignal.sigint.watch().listen((_) async {
    print('Shutting down…');
    await network.dispose();
    if (!done.isCompleted) done.complete();
  });
  await done.future;
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

Uint8List _bytes(String hex) {
  final out = Uint8List(hex.length ~/ 2);
  for (var i = 0; i < out.length; i++) {
    out[i] = int.parse(hex.substring(2 * i, 2 * i + 2), radix: 16);
  }
  return out;
}
