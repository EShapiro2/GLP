/// Headless reference initiator — the phone side of the GPW push, per the
/// cross-project answer: configure the service's `(publicKey, ip:port)`,
/// call `putPeerAddress` (which also supplies the key), then `send`.
///
/// Compile: `dart compile exe bin/glp_net_push.dart -o glp_net_push`
///
/// Usage:
///   glp_net_push --peer <service-pubkey-hex> --address <ip:port> \
///     --message 'payload' --outbound-queue-file pusher-queue.json \
///     [--identity pusher-identity.json] [--timeout 15]
///
/// The fair-delivery queue is kept in the file `--outbound-queue-file` names,
/// so that it survives the termination of the pusher (GNA §2.11): a message
/// not acknowledged before the pusher ends is replayed by the next run with
/// the same identity and file.
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
    ..addOption('peer', help: 'Service public key (64 hex chars)')
    ..addOption('address', help: 'Service ip:port')
    ..addOption('message', help: 'UTF-8 payload to push')
    ..addOption('identity',
        defaultsTo: 'pusher-identity.json',
        help: 'Path to identity file (created on first run)')
    ..addOption('outbound-queue-file',
        help: 'JSON file persisting the fair-delivery queue across runs, so '
            'that it survives the termination of the pusher (GNA §2.11)')
    ..addOption('timeout',
        defaultsTo: '15', help: 'Seconds to wait for the session')
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
  if (args['help'] as bool ||
      args['peer'] == null ||
      args['address'] == null ||
      args['message'] == null ||
      args['outbound-queue-file'] == null) {
    print(parser.usage);
    if (args['help'] != true) exitCode = 64;
    return;
  }

  final sodium = await initHeadlessSodium();
  final identity = await loadOrCreateIdentity(args['identity'] as String);
  print('Pusher identity: ${_hex(identity.publicKey)}');

  final network = HeadlessGrassrootsNetwork(
    identity: identity,
    rendezvousServers: const [],
    sodium: sodium,
    outboundQueueStore:
        FileOutboundQueueStore(args['outbound-queue-file'] as String),
  );
  final servicePk = _bytes(args['peer'] as String);
  final connected = Completer<void>();
  network.onPeerConnected = (pk, underlay, _) {
    print('Service reachable (${underlay.name})');
    if (!connected.isCompleted) connected.complete();
  };

  if (!await network.start()) {
    stderr.writeln('Failed to bind a UDP socket');
    exitCode = 1;
    return;
  }

  // The spec sequence: wan.putPeerAddress supplies key and address; send dials,
  // establishes the Noise session with the service's key as the expected
  // static, and delivers.
  network.wan.putPeerAddress(servicePk, args['address'] as String);
  final messageId = await network.send(
    servicePk,
    Uint8List.fromList(utf8.encode(args['message'] as String)),
  );
  print('Message id: $messageId');

  final timeout = Duration(seconds: int.parse(args['timeout'] as String));
  try {
    await connected.future.timeout(timeout);
    // Give the delivered message a moment to flush before teardown.
    await Future<void>.delayed(const Duration(milliseconds: 500));
    print('Push delivered over authenticated session.');
  } on TimeoutException {
    stderr.writeln('No authenticated session within ${timeout.inSeconds}s '
        '(service down, wrong address, or key not registered under Closed '
        'trust).');
    exitCode = 1;
  } finally {
    await network.dispose();
  }
  exit(exitCode);
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
