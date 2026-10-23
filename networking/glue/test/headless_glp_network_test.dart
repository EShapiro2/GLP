import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:sodium/sodium_sumo.dart';
import 'package:flutter_test/flutter_test.dart';

import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// The adapter over the headless layer: the runtime's `GlpNetwork`
/// implemented by [HeadlessGlpNetwork] over [HeadlessGrassrootsNetwork] (GNA's
/// task of 2026-10-04 16:07 UTC, items 1 and 4; IGLP, Implementation Notes,
/// "Networking Seam", One interface).
///
/// Two groups.  The first holds the glue to passing straight through: each
/// function the layer has reaches it, each callback the layer fires reaches
/// the runtime with its arguments as the layer gave them, and each function
/// of the API the headless layer does not have throws [UnsupportedError].  The
/// second runs two madGLP runtimes, each hosted by the runtime's own host,
/// `AgentRuntime`, over its own headless layer through the adapter, in two
/// processes: a cold call crosses from the first to the second, and an
/// assignment crosses back.
///
/// Needs a native libsodium, as `headless_e2e_test.dart` does; self-skips
/// when absent.
void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping the adapter: $e';
    }
  });

  Future<GrassrootsIdentity> identityFromSeed(Uint8List seed) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519().newKeyPairFromSeed(seed),
      );

  Uint8List seedOf(int fill) => Uint8List.fromList(List.filled(32, fill));

  group('HeadlessGlpNetwork passes straight through', () {
    late HeadlessGrassrootsNetwork layer;
    late HeadlessGlpNetwork network;
    late glp.PubKey peer;

    setUp(() async {
      if (sodiumUnavailable != null) return;
      layer = HeadlessGrassrootsNetwork(
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: await identityFromSeed(seedOf(0x21)),
        rendezvousServers: const [],
        sodium: sodium!,
        serverAddress: ServerAddress(
            listenPort: 9600, publicAddress: '203.0.113.7:9600'),
      );
      network = HeadlessGlpNetwork(layer);
      peer = glp.PubKey.fromList(
          (await identityFromSeed(seedOf(0x22))).publicKey);
    });

    tearDown(() async {
      if (sodiumUnavailable != null) return;
      await layer.dispose();
    });

    test('identity, known peers, address, reachability', () {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      expect(network.getIdentity(),
          glp.PubKey.fromList(layer.identity.publicKey));

      network.putKnownPeer(peer);
      expect(layer.store.state.knownPeers.known.containsKey(peer.hex), isTrue);
      network.putPeerAddress(peer, '127.0.0.1:9601');
      expect(layer.store.state.knownPeers.known[peer.hex], '127.0.0.1:9601');
      expect(() => network.putPeerAddress(peer, 'nowhere'),
          throwsArgumentError,
          reason: "the layer's refusal of an unparseable address, unchanged");
      network.removeKnownPeer(peer);
      expect(layer.store.state.knownPeers.known.containsKey(peer.hex), isFalse);

      expect(network.getPublicAddress(), '203.0.113.7:9600');
      expect(network.isPeerReachable(peer), isFalse);
    });

    test('callbacks carry what the layer reports, in GNA\'s order', () {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final events = <String>[];
      const attested = IosApplicationIdentity('TEAM.app.grassroots');
      Object? identitySeen;
      network.onPeerConnected = (pk, underlay, identity) {
        identitySeen = identity;
        events.add('connected ${pk.hex} $underlay');
      };
      network.onPeerDisconnected =
          (pk, underlay) => events.add('disconnected ${pk.hex} $underlay');
      network.onMessageReceived = (sender, payload, messageId, underlay) =>
          events.add('received ${sender.hex} $payload $messageId $underlay');

      layer.onPeerConnected!(peer.bytes, Underlay.wan, attested);
      layer.onMessageReceived!(
          peer.bytes, Uint8List.fromList([1, 2, 3]), 'm-1', Underlay.wan);
      layer.onPeerDisconnected!(peer.bytes, Underlay.wan);

      expect(events, [
        'connected ${peer.hex} ${glp.Underlay.wan}',
        'received ${peer.hex} [1, 2, 3] m-1 ${glp.Underlay.wan}',
        'disconnected ${peer.hex} ${glp.Underlay.wan}',
      ]);
      expect(identical(identitySeen, attested), isTrue,
          reason: 'the attested application identity, as the layer reports it');

      network.onPeerConnected = null;
      network.onPeerDisconnected = null;
      network.onMessageReceived = null;
      expect(layer.onPeerConnected, isNull);
      expect(layer.onPeerDisconnected, isNull);
      expect(layer.onMessageReceived, isNull);
    });

    test(
        'the WAN functions and the cryptography pass straight through (GNA, '
        '2026-10-11, Task C; until then they threw UnsupportedError)',
        () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final message = Uint8List.fromList('attest(alice, bob)'.codeUnits);
      final signature = network.sign(message);
      expect(signature, layer.sign(message));
      expect(network.verify(network.getIdentity(), message, signature), isTrue);
      expect(network.verify(peer, message, signature), isFalse);

      expect(network.peerUnderlays(peer), isEmpty);
      expect(network.observedPeerAddress(peer), isNull,
          reason: 'the peer is not connected');
      expect(network.networkIdentity(), layer.wan.networkIdentity());
      expect(() => network.punchUdp('nowhere'), throwsArgumentError,
          reason: "the layer's refusal of an unparseable address, unchanged");

      final events = <String>[];
      network.onConnectivityStatusChanged =
          (oldAddress, newAddress) => events.add('address $oldAddress $newAddress');
      network.onNetworkChanged =
          (oldId, newId) => events.add('network $oldId $newId');
      layer.wan.onConnectivityStatusChanged!(null, '203.0.113.7:9600');
      layer.wan.onNetworkChanged!(null, 'n-1');
      expect(events, ['address null 203.0.113.7:9600', 'network null n-1']);
    });

    test(
        "send awaits the layer's save of the queue, and a save that fails "
        "fails the glue's send (GNA §2.11)", () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final store = MemoryOutboundQueueStore();
      final saving = HeadlessGrassrootsNetwork(
        identity: await identityFromSeed(seedOf(0x23)),
        rendezvousServers: const [],
        sodium: sodium!,
        outboundQueueStore: store,
      );
      addTearDown(saving.dispose);
      await HeadlessGlpNetwork(saving).send(peer, Uint8List.fromList([1]));
      expect(
          (await store.load(_hex(saving.identity.publicKey)))
              .map((e) => e.recipientHex),
          [peer.hex],
          reason: 'the message is saved once the glue\'s send completes');

      final failing = HeadlessGrassrootsNetwork(
        identity: await identityFromSeed(seedOf(0x24)),
        rendezvousServers: const [],
        sodium: sodium!,
        outboundQueueStore: _FailingQueueStore(),
      );
      addTearDown(failing.dispose);
      await expectLater(
          HeadlessGlpNetwork(failing).send(peer, Uint8List.fromList([2])),
          throwsA(isA<FileSystemException>()),
          reason: 'a save that fails reaches the caller of the glue\'s send');
    });

    test(
        'no proximity underlay and no location service: getPeers is empty, '
        'setTrustLevel and the places are refused as unsupported', () {
      // GNA, 2026-10-11, the adapter's Q1; Section "Instantiation", Location
      // service: "none on a headless server".
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      expect(network.getPeers(), isEmpty);
      // The layer's pan and lan sub-objects are null (GNA §2.2).
      expect(layer.pan, isNull);
      expect(layer.lan, isNull);
      expect(
          () => network.setTrustLevel(
              glp.ProximityUnderlay.pan, glp.TrustLevel.open),
          throwsUnsupportedError);
      expect(
          () => network.setTrustLevel(
              glp.ProximityUnderlay.lan, glp.TrustLevel.closed),
          throwsUnsupportedError);
      expect(() => network.declarePlace('home', 50), throwsUnsupportedError);
      expect(() => network.removePlace('home'), throwsUnsupportedError);
    });
  });

  group('two runtimes over two headless layers in two processes', () {
    // The agents' one program: the caller cold-calls the callee, naming it by
    // the key its host passes, with a writer; the callee assigns the writer,
    // and the assignment crosses back.  Written under the root, programs/
    // (TGLP modules.tex, "Scope construction": "A program lies at or below the
    // root"), as glp_runtime's agent_runtime_test.dart writes its play.
    const play = r'''
procedure caller_init(_?, Key?, _?).
caller_init(_, Callee, _) :-
    send_to_net([msg(Callee?, ask(Answer))]),
    answered(Answer?).

procedure answered(_?).
answered(forty_two) :- send_to_user([answered(forty_two)]).

procedure callee_init(_?, _?).
callee_init(_, [msg(Me, ask(A?)) | _]) :-
    send_to_user([called(Me?)]),
    answer(A).

procedure answer(_).
answer(forty_two).
''';

    late Directory dir;
    late String program;
    final rootSelfGlpPath = File('../../programs/self.glp').absolute.path;

    setUpAll(() {
      dir = Directory('../../programs/tests').createTempSync('glp_adapter_');
      program = File('${dir.path}/play.glp').absolute.path;
      File(program).writeAsStringSync(play);
    });
    tearDownAll(() => dir.deleteSync(recursive: true));

    test('a cold call and an assignment cross between them through the adapter',
        () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final callerSeed = seedOf(0x5a);
      final calleeSeed = seedOf(0x6b);
      final callerIdentity = await identityFromSeed(callerSeed);
      final caller = glp.PubKey.fromList(callerIdentity.publicKey);
      final callerPriv = Uint8List.fromList(callerIdentity.privateKey);
      final callee =
          glp.PubKey.fromList((await identityFromSeed(calleeSeed)).publicKey);

      // The second process: the callee over its own headless layer.
      final child = await Process.start(_dartExecutable(), [
        'run',
        'test/helpers/glp_adapter_agent.dart',
        _hex(calleeSeed),
        rootSelfGlpPath,
        program,
        'callee_init/2',
        caller.hex,
      ]);
      addTearDown(() async {
        await child.stdin.close();
        await child.exitCode.timeout(const Duration(seconds: 20),
            onTimeout: () {
          child.kill();
          return -1;
        });
      });
      final childLines = <String>[];
      final childLine = StreamController<String>.broadcast();
      child.stdout
          .transform(utf8.decoder)
          .transform(const LineSplitter())
          .listen((l) {
        childLines.add(l);
        childLine.add(l);
      });
      final childErr = StringBuffer();
      child.stderr.transform(utf8.decoder).listen(childErr.write);
      Future<String> fromChild(bool Function(String) test) {
        for (final l in childLines) {
          if (test(l)) return Future.value(l);
        }
        return childLine.stream.firstWhere(test).timeout(
            const Duration(seconds: 90),
            onTimeout: () => throw TimeoutException(
                'the callee process said ${childLines.join(' | ')}; '
                'stderr: $childErr'));
      }

      final ready = (await fromChild((l) => l.startsWith('READY '))).split(' ');
      expect(ready[2], callee.hex, reason: 'the callee is named by its key');
      final calleeAddress = '127.0.0.1:${ready[1]}';

      // This process: the caller over its own headless layer.
      final layer = HeadlessGrassrootsNetwork(
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: callerIdentity,
        rendezvousServers: const [],
        sodium: sodium!,
      );
      addTearDown(layer.dispose);
      final network = HeadlessGlpNetwork(layer);
      final host = AgentRuntime(
        agentId: 'caller',
        program: program,
        rootSelfGlpPath: rootSelfGlpPath,
        goalLabel: 'caller_init/3',
        extraArgs: [callee.hex],
        keyPair: (pub: caller, priv: callerPriv),
        network: network,
      );
      addTearDown(host.dispose);
      final transcript = <String>[];
      host.onOutput = transcript.add;
      final connected = <String>[];
      host.onPeerConnected = (pk, underlay, attested) =>
          connected.add('${pk.hex} $underlay $attested');
      expect(host.name, caller.hex);
      expect(await layer.start(), isTrue);
      // The callee's key and address, through the adapter: what peer_declare/1
      // and address_declare/2 give the layer, which the runtime does not yet
      // have as predicates.
      network.putKnownPeer(callee);
      network.putPeerAddress(callee, calleeAddress);

      // The host binds the adapter as it boots; each delivery is recorded
      // too, then handed to the host.
      await host.initialize();
      final receivedFrom = <String>[];
      final deliver = network.onMessageReceived!;
      network.onMessageReceived = (sender, payload, messageId, underlay) {
        receivedFrom.add(sender.hex);
        deliver(sender, payload, messageId, underlay);
      };

      // The cold call crosses: the callee's program receives it on its
      // network input, addressed to its own name.
      final called = await fromChild((l) => l.startsWith('OUT called('));
      expect(called, contains(callee.hex));
      expect(childLines,
          contains(startsWith('RECEIVED ${caller.hex} wan')),
          reason: "the callee's adapter delivered the caller's message");

      // The assignment crosses back: the caller's writer, assigned by the
      // callee, reaches the caller's program.
      final answered =
          await _line(transcript, (l) => l.startsWith('< answered('));
      expect(answered, contains('forty_two'));
      expect(receivedFrom, contains(callee.hex),
          reason: 'the assignment arrived through the adapter');

      // Each side's adapter reported the other connected over the WAN, the
      // headless profile's one underlay, unattested (GNA, Rendezvous Server,
      // "Unattested": a headless server has no attestation to offer).
      expect(connected, contains('${callee.hex} ${glp.Underlay.wan} null'));
      await fromChild((l) => l == 'CONNECTED ${caller.hex} wan null');
    }, timeout: const Timeout(Duration(minutes: 3)));
  });
}

/// A store of the queue whose every save fails, as a full disc's does.
class _FailingQueueStore implements OutboundQueueStore {
  @override
  Future<List<OutboundQueueEntry>> load(String ownerHex) async => [];

  @override
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries) async =>
      throw const FileSystemException('the disc is full');
}

/// The first line of [lines] that [test] accepts, waiting up to 90 seconds
/// for it to come.
Future<String> _line(List<String> lines, bool Function(String) test) async {
  final deadline = DateTime.now().add(const Duration(seconds: 90));
  while (true) {
    for (final l in lines) {
      if (test(l)) return l;
    }
    if (DateTime.now().isAfter(deadline)) {
      throw TimeoutException('no line came: ${lines.join(' | ')}');
    }
    await Future<void>.delayed(const Duration(milliseconds: 50));
  }
}

/// The Dart that runs the second process.  The test runs under
/// `flutter test`, in flutter_tester, whose executable is not Dart's: the
/// Flutter SDK's own Dart is used, and failing that the Dart on the path.
String _dartExecutable() {
  final root = Platform.environment['FLUTTER_ROOT'];
  if (root != null) {
    final dart = File('$root/bin/dart');
    if (dart.existsSync()) return dart.path;
  }
  return 'dart';
}

String _hex(List<int> bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
