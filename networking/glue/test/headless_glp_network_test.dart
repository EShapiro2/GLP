import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/runtime/terms.dart' as rt;
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
      const attested =
        Application(developer: 'TEAM', name: 'app.grassroots', version: 1);
      glp.Application? applicationSeen;
      network.onPeerConnected = (pk, underlay, application) {
        applicationSeen = application;
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
      expect(applicationSeen,
          const glp.Application(
              developer: 'TEAM', name: 'app.grassroots', version: 1),
          reason: 'the attested application, its developer, its name and its '
              'version number, as the runtime holds it (GNA §2.3)');

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
      // declare(rendezvous(P, A)) and remove(rendezvous(P)) reach the WAN
      // sub-object's putRendezvousServer and removeRendezvousServer (GNA
      // 776016c, Section 4.2): the layer's refusal of an unparseable address
      // comes back unchanged, and a key not declared does nothing.
      expect(() => network.putRendezvousServer(peer, 'nowhere'),
          throwsArgumentError,
          reason: "the layer's refusal of an unparseable address, unchanged");
      expect(() => network.removeRendezvousServer(peer), returnsNormally);
      // acknowledge(messageId) (GNA, Section 2.9) reaches the layer's: a
      // message not kept does nothing.
      expect(() => network.acknowledge('m-1'), returnsNormally);

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
        'a trust declaration finds no sub-object and does nothing, and the '
        'places are refused as unsupported', () {
      // GNA §2.2: "a call on an underlay the layer does not run finds no
      // sub-object"; §2.7: a headless server "refuses declarePlace and
      // removePlace".
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
          returnsNormally);
      expect(
          () => network.setTrustLevel(
              glp.ProximityUnderlay.lan, glp.TrustLevel.closed),
          returnsNormally);
      expect(() => network.declarePlace('home', 50), throwsUnsupportedError);
      expect(() => network.removePlace('home'), throwsUnsupportedError);
    });

    test(
        'declare(trust(lan, open)) on the layer, which runs no LAN, returns, '
        'and the program runs on', () async {
      // GNA §2.2: a trust declaration for an underlay the layer does not run
      // finds no sub-object, and does nothing (GNA task O (f)).
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final out = <String>[];
      final engine = GlpEngine(
          rootSelfGlpPath: File('../../programs/self.glp').absolute.path);
      engine.enableMadGLP(agentId: 'alice');
      engine.runtime.outputCallback = out.add;
      engine.madContext!.network = network;
      engine.loadSource('''
procedure go.
go :- declare(trust(lan, open)), declared.
procedure declared.
declared :- send_to_user([running]).
''');
      final result = await engine.runGoal('go');
      expect(result.succeeded, isTrue,
          reason: 'the declaration returns, and aborts nothing');
      expect(out, ['running'], reason: 'the program runs on');
    });
  });

  test(
      "a message the layer kept is delivered again through the glue at "
      "restart, and the glue's acknowledge(messageId) is the layer's (GNA "
      "§2.9, §2.11)", () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final self = await identityFromSeed(seedOf(0x23));
    final sender = await identityFromSeed(seedOf(0x24));
    final owner = glp.PubKey.fromList(self.publicKey).hex;
    final senderKey = glp.PubKey.fromList(sender.publicKey);
    // What a layer that delivered the message and was not told it is applied
    // left behind.
    final store = MemoryOutboundQueueStore();
    await store.saveDelivered(owner, [
      DeliveredQueueEntry(
        senderHex: senderKey.hex,
        messageId: 'm-kept',
        payload: Uint8List.fromList([4, 2]),
        underlay: Underlay.wan,
      ),
    ]);
    final layer = HeadlessGrassrootsNetwork(
      outboundQueueStore: store,
      identity: self,
      sodium: sodium!,
    );
    final network = HeadlessGlpNetwork(layer);
    final delivered = Completer<(glp.PubKey, List<int>, String, glp.Underlay)>();
    network.onMessageReceived = (s, payload, id, u) =>
        delivered.complete((s, payload, id, u));
    final (s, payload, id, u) = await delivered.future;
    expect((s, id, u), (senderKey, 'm-kept', glp.Underlay.wan));
    expect(payload, [4, 2]);

    network.acknowledge(id);
    await layer.dispose();
    expect(await store.loadDelivered(owner), isEmpty,
        reason: 'acknowledged through the glue, it is kept no longer');
  }, timeout: Timeout.none);

  test(
      'declare(rendezvous(K, A)) gives the layer a server, which it dials, '
      'and remove(rendezvous(K)) ends its session with it (GNA §4.2, §5)',
      () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    // A UDP port nothing listens on, until the server binds it.
    final probe = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    final port = probe.port;
    probe.close();
    final serverId = await identityFromSeed(seedOf(0x25));
    final server = HeadlessGrassrootsNetwork(
      identity: serverId,
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '127.0.0.1:$port'),
      rendezvousServer: true,
      sodium: sodium!,
      outboundQueueStore: MemoryOutboundQueueStore(),
    );
    addTearDown(server.dispose);
    expect(await server.start(), isTrue);

    final layer = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(seedOf(0x26)),
      sodium: sodium!,
      outboundQueueStore: MemoryOutboundQueueStore(),
    );
    addTearDown(layer.dispose);
    expect(await layer.start(), isTrue);
    final network = HeadlessGlpNetwork(layer);
    final serverKey = glp.PubKey.fromList(serverId.publicKey);
    final connected = Completer<glp.Underlay>();
    final disconnected = Completer<glp.Underlay>();
    network.onPeerConnected = (pk, underlay, _) {
      if (pk == serverKey && !connected.isCompleted) {
        connected.complete(underlay);
      }
    };
    network.onPeerDisconnected = (pk, underlay) {
      if (pk == serverKey && !disconnected.isCompleted) {
        disconnected.complete(underlay);
      }
    };

    final out = <String>[];
    final engine = GlpEngine(
        rootSelfGlpPath: File('../../programs/self.glp').absolute.path);
    engine.enableMadGLP(agentId: 'alice');
    engine.runtime.outputCallback = out.add;
    engine.madContext!.network = network;
    engine.loadSource('''
procedure declared.
declared :- declare(rendezvous('${serverKey.hex}', '127.0.0.1:$port')).
procedure removed.
removed :- remove(rendezvous('${serverKey.hex}')).
''');

    expect((await engine.runGoal('declared')).succeeded, isTrue);
    expect(await connected.future, glp.Underlay.wan,
        reason: 'the declared server is dialled, and its session comes up');
    expect(network.isPeerReachable(serverKey), isTrue);

    expect((await engine.runGoal('removed')).succeeded, isTrue);
    expect(await disconnected.future, glp.Underlay.wan,
        reason: "the removed server's session ends");
    expect(network.isPeerReachable(serverKey), isFalse);
  }, timeout: Timeout.none);

  test(
      'declare(reachable(S)) gives S connected(K) when the layer has a session '
      'with K over the WAN and lost(K) when it ends; remove(reachable) closes '
      'S (GLP-Spec cb9df43, "Networking seam"; IGLP 0c77e47)', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    // A WAN session made and ended: the layer dials a declared rendezvous
    // server and ends its session with it when the server is removed, as the
    // test above has it.
    final probe = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    final port = probe.port;
    probe.close();
    final serverId = await identityFromSeed(seedOf(0x27));
    final server = HeadlessGrassrootsNetwork(
      identity: serverId,
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '127.0.0.1:$port'),
      rendezvousServer: true,
      sodium: sodium!,
      outboundQueueStore: MemoryOutboundQueueStore(),
    );
    addTearDown(server.dispose);
    expect(await server.start(), isTrue);

    final layer = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(seedOf(0x28)),
      sodium: sodium!,
      outboundQueueStore: MemoryOutboundQueueStore(),
    );
    addTearDown(layer.dispose);
    expect(await layer.start(), isTrue);
    final network = HeadlessGlpNetwork(layer);
    final serverKey = glp.PubKey.fromList(serverId.publicKey);

    final out = <String>[];
    final engine = GlpEngine(
        rootSelfGlpPath: File('../../programs/self.glp').absolute.path);
    engine.enableMadGLP(agentId: 'alice');
    engine.runtime.outputCallback = out.add;
    final ctx = engine.madContext!;
    ctx.network = network;
    // What the layer reports reaches the runtime, as the agent's host binds
    // it (AgentRuntime, isolate_manager.dart).
    final connected = Completer<void>();
    final disconnected = Completer<void>();
    network.onPeerConnected = (pk, underlay, application) {
      ctx.peerConnected(pk, underlay, application);
      if (pk == serverKey && underlay == glp.Underlay.wan &&
          !connected.isCompleted) {
        connected.complete();
      }
    };
    network.onPeerDisconnected = (pk, underlay) {
      ctx.peerDisconnected(pk, underlay);
      if (pk == serverKey && underlay == glp.Underlay.wan &&
          !disconnected.isCompleted) {
        disconnected.complete();
      }
    };
    engine.loadSource('''
procedure watched.
watched :- declare(reachable(S)), watch(S?).
procedure watch(Stream(Reach)?).
watch([R | S]) :- ground(R?) | send_to_user([R?]), watch(S?).
watch([]) :- send_to_user([closed]).
procedure declared.
declared :- declare(rendezvous('${serverKey.hex}', '127.0.0.1:$port')).
procedure removed.
removed :- remove(rendezvous('${serverKey.hex}')).
procedure ended.
ended :- remove(reachable).
procedure tick.
tick.
''');

    await engine.runGoal('watched');
    expect(out, isEmpty, reason: 'no session over the WAN yet');
    expect((await engine.runGoal('declared')).succeeded, isTrue);
    await connected.future;
    await engine.runGoal('tick');
    expect(out, [contains('connected(')], reason: 'the session comes up');
    expect(out.single, contains(serverKey.hex));

    // The watch suspended on S is part of the drain, so a run may end
    // suspended; it does not fail.
    expect((await engine.runGoal('removed')).failed, isFalse);
    await disconnected.future;
    await engine.runGoal('tick');
    expect(out.length, 2);
    expect(out[1], allOf(contains('lost('), contains(serverKey.hex)),
        reason: 'the session ends');

    expect((await engine.runGoal('ended')).failed, isFalse);
    expect(out.last, 'closed', reason: 'remove(reachable) closes the stream');
  }, timeout: Timeout.none);

  group('two runtimes over two headless layers in two processes', () {
    // The agents' one program: each agent started from agent(ch(_?,_),
    // ch(_?,_)), every agent's initial goal (IGLP, Implementation Notes,
    // "Boot"); the caller's person names the callee by its key, call(Key), and
    // the caller cold-calls it with a writer; the callee's person tells its
    // agent to answer, and the callee assigns the writer, and the assignment
    // crosses back.  Written under the root, programs/
    // (TGLP modules.tex, "Scope construction": "A program lies at or below the
    // root"), as glp_runtime's agent_runtime_test.dart writes its play.
    const play = r'''
PlayIn  ::= call(Key) ; answer.
PlayOut ::= answered(Constant) ; called(Key).
PlayMsg ::= ask(Constant?).

exported procedure agent(Channel(Stream(PlayIn), Stream(PlayOut))?, Channel(Stream(NetMsg(PlayMsg)), Stream(NetMsg(PlayMsg)))?).
agent(ch(In, []), ch(N, [])) :- start(In?, N?).

procedure start(Stream(PlayIn)?, Stream(NetMsg(PlayMsg))?).
start([call(Callee) | _], _) :- ground(Callee?) |
    send_to_net([msg(Callee?, ask(Answer))]),
    answered(Answer?).
start([answer | _], [msg(Me, ask(A?)) | _]) :-
    send_to_user([called(Me?)]),
    answer(A).
start([], _).

procedure answered(Constant?).
answered(forty_two) :- send_to_user([answered(forty_two)]).

procedure answer(Constant).
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
        'answer',
        caller.hex,
      ]);
      addTearDown(() async {
        // Its stdin closed, the child exits.
        await child.stdin.close();
        await child.exitCode;
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
      addTearDown(() => printOnFailure('the callee process said '
          '${childLines.join(' | ')}; stderr: $childErr'));
      Future<String> fromChild(bool Function(String) test) {
        for (final l in childLines) {
          if (test(l)) return Future.value(l);
        }
        return childLine.stream.firstWhere(test);
      }

      final ready = (await fromChild((l) => l.startsWith('READY '))).split(' ');
      expect(ready[2], callee.hex, reason: 'the callee is named by its key');
      final calleeAddress = '127.0.0.1:${ready[1]}';

      // This process: the caller over its own headless layer.
      final layer = HeadlessGrassrootsNetwork(
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: callerIdentity,
        sodium: sodium!,
      );
      addTearDown(layer.dispose);
      final network = HeadlessGlpNetwork(layer);
      final host = AgentRuntime(
        agentId: 'caller',
        program: program,
        rootSelfGlpPath: rootSelfGlpPath,
        keyPair: (pub: caller, priv: callerPriv),
        network: network,
      );
      addTearDown(host.dispose);
      final transcript = <String>[];
      final transcriptLine = StreamController<String>.broadcast();
      host.onOutput = (l) {
        transcript.add(l);
        transcriptLine.add(l);
      };
      final connected = <String>[];
      host.onPeerConnected = (pk, underlay, attested) =>
          connected.add('${pk.hex} $underlay $attested');
      expect(host.name, caller.hex);
      expect(await layer.start(), isTrue);
      // The callee's key and address, through the adapter: what
      // declare(peer(P)) and declare(address(P, A)) give the layer.
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
      // The caller's person names the callee by its key.
      await host.injectUserInput(
          rt.StructTerm('call', [rt.ConstTerm(callee.hex)]));

      // The cold call crosses: the callee's program receives it on its
      // network input, addressed to its own name.
      final called = await fromChild((l) => l.startsWith('OUT called('));
      expect(called, contains(callee.hex));
      expect(childLines,
          contains(startsWith('RECEIVED ${caller.hex} wan')),
          reason: "the callee's adapter delivered the caller's message");

      // The assignment crosses back: the caller's writer, assigned by the
      // callee, reaches the caller's program.
      final answered = await _line(
          transcript, transcriptLine.stream, (l) => l.startsWith('< answered('));
      expect(answered, contains('forty_two'));
      expect(receivedFrom, contains(callee.hex),
          reason: 'the assignment arrived through the adapter');

      // Each side's adapter reported the other connected over the WAN, the
      // headless profile's one underlay, unattested (GNA, Rendezvous Server,
      // "Unattested": a headless server has no attestation to offer).
      expect(connected, contains('${callee.hex} ${glp.Underlay.wan} null'));
      await fromChild((l) => l == 'CONNECTED ${caller.hex} wan null');
    }, timeout: Timeout.none);
  });
}

/// A store of the queue whose every save fails, as a full disc's does.
class _FailingQueueStore implements OutboundQueueStore {
  @override
  Future<List<OutboundQueueEntry>> load(String ownerHex) async => [];

  @override
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries) async =>
      throw const FileSystemException('the disc is full');

  @override
  Future<List<DeliveredQueueEntry>> loadDelivered(String ownerHex) async => [];

  @override
  Future<void> saveDelivered(
          String ownerHex, List<DeliveredQueueEntry> entries) async =>
      throw const FileSystemException('the disc is full');
}

/// The first line of [lines], or of the lines still to come on [more], that
/// [test] accepts.
Future<String> _line(
    List<String> lines, Stream<String> more, bool Function(String) test) {
  for (final l in lines) {
    if (test(l)) return Future.value(l);
  }
  return more.firstWhere(test);
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
