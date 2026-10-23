import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:sodium/sodium_sumo.dart';
import 'package:flutter_test/flutter_test.dart';

import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// The rendezvous server (GNA Section 6): `bin/rendezvous_server.dart`, built
/// to a native executable and run as a process of its own --- the headless
/// layer marked as a rendezvous server, with the GLP rendezvous agent,
/// `programs/examples/rendezvous/rv_agent.glp`, over it through the adapter.
///
/// Two friends, each a madGLP agent over its own headless layer in this
/// process, each declaring the server to its layer (GNA Section 4.2,
/// "Rendezvous servers"), hold each other's signature over the pair `attest(PkA, PkB)`
/// (GNA Section 6, "Reconnection").  The one sends the server a reconnection
/// request naming the other and carrying the other's signature, the other an
/// availability notice carrying the first's; the agent verifies each
/// signature as the other party's by `signature/2`, reads each one's address
/// from the layer by `peer_address/2`, and relays to each the address it
/// observes for the other, whereupon each declares it (`address_declare/2`)
/// and punches toward it (`punch_udp/1`).  A request whose signature is the
/// requester's own is refused.  And, item 3 of GNA's task of 2026-10-09,
/// the same after one friend changes address, the two reconnecting (the
/// second test, whose comment says what the loopback can and cannot show).
///
/// The friends' program is the smallest stand-in for the social graph's part
/// (GSG's): it mints the friendship signature on request and sends the
/// request, and the test hands each signature to the other friend and says
/// when to send, where the social graph exchanges the signatures on
/// befriending and sends on an address change or a lost path.
///
/// The executable is built with `dart build cli`: `dart compile exe` refuses
/// the glue package, whose Flutter dependencies carry a build hook
/// (`objective_c`).
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
      sodiumUnavailable = 'libsodium unavailable, skipping the server: $e';
    }
  });

  // The friends' program: the stand-in for the social graph's part.  Written
  // under the root, programs/ (TGLP modules.tex, "Scope construction": "A
  // program lies at or below the root"), as the adapter's own test writes its
  // play.
  //
  //   attest(F)               sign the pair of this agent's key and F, the
  //                           smaller first, and report attestation(F, Sig,
  //                           Graph), Graph the program identity signature/2
  //                           reports for it
  //   reconnect(Rv, F, Sig)   send the server Rv a reconnection request
  //                           naming F and carrying Sig, F's signature
  //   available(Rv, F, Sig)   send Rv an availability notice for F, carrying
  //                           Sig, F's signature
  //
  // On the server's reply: punch(Addr), declare Addr as F's address, punch
  // toward it and report punched(F, Addr); rejected, report rejected(F).
  const friendProgram = r'''
FriendAct ::= attest(Key)
            ; reconnect(Key, Key, SignedTerm)
            ; available(Key, Key, SignedTerm).
FriendActs ::= [] ; [FriendAct | FriendActs].
SignalReply ::= punch(String) ; rejected.

procedure friend_init(Key?, FriendActs?, _?).
friend_init(Me, UserIn, _) :- acts(Me?, UserIn?).

procedure acts(Key?, FriendActs?).
acts(Me, [attest(F) | In]) :- ground(Me?) |
    self_key(K),
    attest_pair(K?, F?),
    acts(Me?, In?).
acts(Me, [reconnect(Rv, F, Sig) | In]) :- ground(Me?), ground(F?) |
    send_to_net([msg(Rv?, reconnect(Me?, F?, Sig?, Reply))]),
    on_reply(F?, Reply?),
    acts(Me?, In?).
acts(Me, [available(Rv, F, Sig) | In]) :- ground(Me?), ground(F?) |
    send_to_net([msg(Rv?, available(Me?, F?, Sig?, Reply))]),
    on_reply(F?, Reply?),
    acts(Me?, In?).
acts(_, []).

procedure attest_pair(Key?, Key?).
attest_pair(K, F) :- ground(K?), ground(F?), K? @< F? |
    sign(attest(K?, F?), K?, Sig),
    report(F?, Sig?).
attest_pair(K, F) :- ground(K?), ground(F?), F? @< K? |
    sign(attest(F?, K?), K?, Sig),
    report(F?, Sig?).

procedure report(Key?, SignedTerm?).
report(F, Sig) :- ground(Sig?) |
    signature(Sig?, S),
    report_graph(F?, Sig?, S?).

procedure report_graph(Key?, SignedTerm?, Signature?).
report_graph(F, Sig, signed(_, Graph, _)) :-
    send_to_user([attestation(F?, Sig?, Graph?)]).
report_graph(F, _, unsigned) :- send_to_user([unsigned(F?)]).

procedure on_reply(Key?, SignalReply?).
on_reply(F, punch(Addr)) :- ground(F?), ground(Addr?) |
    address_declare(F?, Addr?),
    punch_udp(Addr?),
    send_to_user([punched(F?, Addr?)]).
on_reply(F, rejected) :- send_to_user([rejected(F?)]).
''';

  late Directory dir;
  late String friendPath;
  late String serverExecutable;
  final glpRoot = Directory('../../programs').absolute.path;
  final rootSelfGlpPath = '$glpRoot/self.glp';
  final rvAgentPath = '$glpRoot/examples/rendezvous/rv_agent.glp';

  setUpAll(() async {
    dir = Directory('../../programs/tests').createTempSync('glp_rendezvous_');
    friendPath = File('${dir.path}/friend.glp').absolute.path;
    File(friendPath).writeAsStringSync(friendProgram);

    // The entrypoint, built to a native executable.
    final out = Directory.systemTemp.createTempSync('rendezvous_server_');
    addTearDown(() => out.deleteSync(recursive: true));
    final build = await Process.run(_dartExecutable(), [
      'build',
      'cli',
      '-t',
      'bin/rendezvous_server.dart',
      '-o',
      out.path,
    ]);
    if (build.exitCode != 0) {
      fail('dart build cli failed: ${build.stdout}\n${build.stderr}');
    }
    serverExecutable = '${out.path}/bundle/bin/rendezvous_server';
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  Future<GrassrootsIdentity> identityFromSeed(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      );

  /// The server, a process of its own, and two friends, each a madGLP agent
  /// over its own headless layer, which is declared the server, holding each
  /// other's
  /// signature over their pair; each friend's layer started.
  Future<_World> standUp(int rvFill, int aFill, int bFill) async {
    // The server's port, and its key pair as an identity file.
    final probe = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    final rvPort = probe.port;
    probe.close();
    final rvSeed = Uint8List.fromList(List.filled(32, rvFill));
    final rvIdentityFile = File('${dir.path}/rv-$rvFill.json')
      ..writeAsStringSync(jsonEncode({'seed': _hex(rvSeed)}));
    final rvKey = glp.PubKey.fromList((await GrassrootsIdentity.create(
            keyPair: await Ed25519().newKeyPairFromSeed(rvSeed)))
        .publicKey);

    // The two friends, each a madGLP agent over its own headless layer
    // declaring the server.
    Future<_Friend> friend(int fill, String id) async {
      final identity = await identityFromSeed(fill);
      final layer = HeadlessGrassrootsNetwork(
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: identity,
        sodium: sodium!,
      );
      layer.wan.putRendezvousServer(rvKey.bytes, '127.0.0.1:$rvPort');
      addTearDown(layer.dispose);
      final key = glp.PubKey.fromList(identity.publicKey);
      final agent = AgentRuntime(
        agentId: id,
        program: friendPath,
        rootSelfGlpPath: rootSelfGlpPath,
        goalLabel: 'friend_init/3',
        keyPair: (pub: key, priv: Uint8List.fromList(identity.privateKey)),
        network: HeadlessGlpNetwork(layer),
      );
      addTearDown(agent.dispose);
      final f = _Friend(key, layer, agent);
      agent.onUserOutput = f.said.add;
      agent.onOutput = f.transcript.add;
      await agent.initialize();
      return f;
    }

    final a = await friend(aFill, 'a');
    final b = await friend(bFill, 'b');

    // Each signs the pair and the signatures change hands, as on befriending.
    await a.agent.injectUserInput(_struct('attest', [_const(b.key.hex)]));
    await b.agent.injectUserInput(_struct('attest', [_const(a.key.hex)]));
    final fromA = await a.saidOne('attestation');
    final fromB = await b.saidOne('attestation');
    final sigA = fromA.args[1];
    final sigB = fromB.args[1];
    final graph = _value(fromA.args[2]);
    expect(_value(fromB.args[2]), graph,
        reason: 'both signed under the one program, the social graph');

    // The server, a process of its own.
    final server = await Process.start(serverExecutable, [
      '--identity',
      rvIdentityFile.path,
      '--port',
      '$rvPort',
      '--public-address',
      '127.0.0.1:$rvPort',
      '--outbound-queue-file',
      '${dir.path}/rv-$rvFill-queue.json',
      '--graph',
      graph,
      '--program',
      rvAgentPath,
      '--root-self',
      rootSelfGlpPath,
    ]);
    addTearDown(() async {
      server.kill(ProcessSignal.sigterm);
      await server.exitCode.timeout(const Duration(seconds: 20),
          onTimeout: () {
        server.kill(ProcessSignal.sigkill);
        return -1;
      });
    });
    final serverLines = <String>[];
    final serverErr = StringBuffer();
    server.stdout
        .transform(utf8.decoder)
        .transform(const LineSplitter())
        .listen(serverLines.add);
    server.stderr.transform(utf8.decoder).listen(serverErr.write);
    final ready = (await _line(serverLines, (l) => l.startsWith('READY '),
            () => 'stderr: $serverErr'))
        .split(' ');
    expect(ready, ['READY', '$rvPort', rvKey.hex],
        reason: 'the server listens on its port, named by its key');

    // The friends' layers start, and each holds a session with the server.
    expect(await a.layer.start(), isTrue);
    expect(await b.layer.start(), isTrue);
    return (
      rvKey: rvKey,
      a: a,
      b: b,
      sigA: sigA,
      sigB: sigB,
      serverLines: serverLines,
    );
  }

  test(
      'two friends, each over its own headless layer, are matched by the GLP '
      'rendezvous agent on the server and told the address it observes for '
      'the other; a request carrying its own signature is refused', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final (:rvKey, :a, :b, :sigA, :sigB, :serverLines) =
        await standUp(0x7e, 0x3a, 0x4b);

    // A requests reconnection to B with B's signature; B notices A with A's.
    await a.agent.injectUserInput(
        _struct('reconnect', [_const(rvKey.hex), _const(b.key.hex), sigB]));
    await b.agent.injectUserInput(
        _struct('available', [_const(rvKey.hex), _const(a.key.hex), sigA]));

    // Each is told the address the server observes for the other: the port
    // the other's layer is bound to, on the loopback.
    final punchedA = await a.saidOne('punched');
    final punchedB = await b.saidOne('punched');
    final toB = _value(punchedA.args[1]);
    final toA = _value(punchedB.args[1]);
    expect(_value(punchedA.args[0]), b.key.hex);
    expect(_loopback(toB, b.layer.boundPort!), isTrue, reason: toB);
    expect(_value(punchedB.args[0]), a.key.hex);
    expect(_loopback(toA, a.layer.boundPort!), isTrue, reason: toA);

    // Each declared the address it was told to its layer (address_declare/2,
    // putPeerAddress): the other is a known peer, recorded there at it.
    expect(a.layer.store.state.knownPeers.known[b.key.hex], toB);
    expect(b.layer.store.state.knownPeers.known[a.key.hex], toA);

    // A request carrying the requester's own signature, not the other
    // party's, is refused: the server verifies the signer by signature/2.
    await b.agent.injectUserInput(
        _struct('reconnect', [_const(rvKey.hex), _const(a.key.hex), sigB]));
    final rejected = await b.saidOne('rejected');
    expect(_value(rejected.args[0]), a.key.hex);

    // Each friend's session with the server is reported by the server.
    expect(serverLines, contains('CONNECTED ${a.key.hex}'));
    expect(serverLines, contains('CONNECTED ${b.key.hex}'));
  }, timeout: const Timeout(Duration(minutes: 4)));

  // GNA's task of 2026-10-09, item 3: two headless agents behind distinct
  // addresses, one changing address, reconnect through the GLP rendezvous
  // agent, the friends' signed attest(PkA, PkB) verified and both punched.
  //
  // On the loopback the two addresses are two ports of 127.0.0.1, and the
  // change is simulated by stopping the mover's layer and starting it again
  // on a new port, as headless_liveness_test.dart does.  What the loopback
  // shows: after the move, the rendezvous agent matches the pair, verifies
  // both signatures, observes the mover at its new address and relays it to
  // the friend, the friend declares it and punches toward it, and the two
  // are reachable again with the friend observing the mover there.  What it
  // cannot show: that the rendezvous path is necessary.  There is no NAT on
  // the loopback, so the mover's own heartbeat sweep, dialing the friend at
  // its unchanged address, would reconnect the pair without it (GNA §4.2:
  // "the endpoint that moved reconnects"); the rendezvous agent is what
  // lets two agents both behind NAT do so, and no test here stands a NAT up.
  test(
      'an agent changes address and it and its friend reconnect through the '
      'GLP rendezvous agent, each signature verified and both punched',
      () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final (:rvKey, :a, :b, :sigA, :sigB, :serverLines) =
        await standUp(0x7d, 0x3c, 0x4d);

    // The friends know each other at their addresses, as the social graph
    // tells their layers (peer_declare/1, address_declare/2), and connect.
    a.layer.wan.putPeerAddress(b.key.bytes, '127.0.0.1:${b.layer.boundPort}');
    b.layer.wan.putPeerAddress(a.key.bytes, '127.0.0.1:${a.layer.boundPort}');
    expect(
        await _until(() =>
            a.layer.isPeerReachable(b.key.bytes) &&
            b.layer.isPeerReachable(a.key.bytes)),
        isTrue,
        reason: 'the heartbeat sweep connects the two');

    // A changes address: its layer stops and starts on a new port, its
    // sessions gone, its known peers and its server kept.
    final oldPort = a.layer.boundPort;
    await a.layer.stop();
    expect(await a.layer.start(), isTrue);
    final newPort = a.layer.boundPort!;
    expect(newPort, isNot(oldPort));

    // A asks the server to reconnect it to B with B's signature; B, which
    // A's move cost its path, tells the server it is available to A with
    // A's.
    await a.agent.injectUserInput(
        _struct('reconnect', [_const(rvKey.hex), _const(b.key.hex), sigB]));
    await b.agent.injectUserInput(
        _struct('available', [_const(rvKey.hex), _const(a.key.hex), sigA]));

    // Both admitted, so both signatures verified; each told the address the
    // server observes for the other: B's unchanged, and A's new.
    final punchedA = await a.saidOne('punched');
    final punchedB = await b.saidOne('punched');
    final toB = _value(punchedA.args[1]);
    final toA = _value(punchedB.args[1]);
    expect(_loopback(toB, b.layer.boundPort!), isTrue, reason: toB);
    expect(_loopback(toA, newPort), isTrue,
        reason: 'B is told A\'s new address: $toA');
    expect(a.said.whereType<rt.StructTerm>().map((t) => t.functor),
        isNot(contains('rejected')));
    expect(b.said.whereType<rt.StructTerm>().map((t) => t.functor),
        isNot(contains('rejected')));

    // B declared A's new address (address_declare/2), and the two are
    // reachable again, B observing A there.
    expect(b.layer.store.state.knownPeers.known[a.key.hex], toA);
    expect(
        await _until(() =>
            a.layer.isPeerReachable(b.key.bytes) &&
            b.layer.isPeerReachable(a.key.bytes)),
        isTrue,
        reason: 'reconnected');
    expect(_loopback(b.layer.wan.observedPeerAddress(a.key.bytes)!, newPort),
        isTrue);
    expect(serverLines, contains('CONNECTED ${a.key.hex}'));
  }, timeout: const Timeout(Duration(minutes: 4)));
}

/// Whether [predicate] holds within 40 seconds, four heartbeat intervals.
Future<bool> _until(bool Function() predicate) async {
  final deadline = DateTime.now().add(const Duration(seconds: 40));
  while (!predicate() && DateTime.now().isBefore(deadline)) {
    await Future<void>.delayed(const Duration(milliseconds: 100));
  }
  return predicate();
}

/// What [standUp] stands up.
typedef _World = ({
  glp.PubKey rvKey,
  _Friend a,
  _Friend b,
  rt.Term sigA,
  rt.Term sigB,
  List<String> serverLines,
});

/// One friend: its key, its layer, its agent, and what the agent said to its
/// person.
class _Friend {
  _Friend(this.key, this.layer, this.agent);
  final glp.PubKey key;
  final HeadlessGrassrootsNetwork layer;
  final AgentRuntime agent;
  final List<rt.Term> said = [];
  final List<String> transcript = [];

  /// The first element this agent sent its person whose functor is
  /// [functor], waiting up to 90 seconds for it.
  Future<rt.StructTerm> saidOne(String functor) async {
    final deadline = DateTime.now().add(const Duration(seconds: 90));
    while (true) {
      for (final t in said) {
        if (t is rt.StructTerm && t.functor == functor) return t;
      }
      if (DateTime.now().isAfter(deadline)) {
        throw TimeoutException('${agent.agentId} said no $functor: '
            '${transcript.join(' | ')}');
      }
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
  }
}

rt.Term _const(String value) => rt.ConstTerm(value);

rt.Term _struct(String functor, List<rt.Term> args) =>
    rt.StructTerm(functor, args);

/// The text of a constant the agent sent.
String _value(rt.Term t) => (t as rt.ConstTerm).value.toString();

/// The first line of [lines] that [test] accepts, waiting up to 90 seconds
/// for it to come.
Future<String> _line(List<String> lines, bool Function(String) test,
    String Function() context) async {
  final deadline = DateTime.now().add(const Duration(seconds: 90));
  while (true) {
    for (final l in lines) {
      if (test(l)) return l;
    }
    if (DateTime.now().isAfter(deadline)) {
      throw TimeoutException('no line came: ${lines.join(' | ')}; '
          '${context()}');
    }
    await Future<void>.delayed(const Duration(milliseconds: 50));
  }
}

/// The Dart that builds the server.  The test runs under `flutter test`, in
/// flutter_tester, whose executable is not Dart's: the Flutter SDK's own
/// Dart is used, and failing that the Dart on the path.
String _dartExecutable() {
  final root = Platform.environment['FLUTTER_ROOT'];
  if (root != null) {
    final dart = File('$root/bin/dart');
    if (dart.existsSync()) return dart.path;
  }
  return 'dart';
}

/// Whether [address] is the loopback at [port].  The server's layer listens
/// on a dual-stack socket, which reports an IPv4 source in its IPv4-mapped
/// IPv6 form; the layer names it in its IPv4 form (`observedAddressString`).
bool _loopback(String address, int port) => address == '127.0.0.1:$port';

String _hex(List<int> bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
