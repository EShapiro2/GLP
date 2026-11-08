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
/// observes for the other, whereupon each declares it (`declare(address(P, A))`)
/// and punches toward it (`punch_udp/1`).  A request whose signature is the
/// requester's own is refused.  The server is told nothing at boot and
/// admits by key, reading no source identity (GSA, "Distribution and
/// Activation", "Reconnection": "The rendezvous agent checks keys, so an
/// update of the Grassroots App leaves the signatures friends hold good"):
/// two friends whose signatures were made under two different programs, as
/// under two versions of the Grassroots App, are matched all the same (the
/// third test).  And, item 3 of GNA's task of 2026-10-09,
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
  //   invite(Rv, RvAddr, Serial, Expiry)
  //                           sign invitation(Me, Rv, RvAddr, Serial, Expiry),
  //                           register it with Rv and report link(L)
  //   register_again(Rv, L)   register the link L with Rv once more
  //   redeem_link(Rv, L)      sign redeem(Me, L) and redeem it at Rv
  //
  // On the answer to a registration: punch(K, Addr), K the recipient who
  // redeemed it, declare Addr as K's address, punch toward it and report
  // invitee_punched(K, Addr); rejected, report register_rejected.  On the
  // answer to a redemption: punch(K, Addr), K the inviter, declare K a peer at
  // Addr, punch and report inviter_punched(K, Addr); rejected, report
  // redeem_rejected.
  //
  // On the server's reply: punch(K, Addr), K being F, the friend named,
  // declare Addr as F's address, punch toward it and report punched(F, Addr)
  // (GNA Section 6: "relays to each the key and the address of the other");
  // punch naming another key, report other(F, K); rejected, report
  // rejected(F).
  const friendProgram = r'''
FriendAct ::= attest(Key)
            ; reconnect(Key, Key, SignedTerm)
            ; available(Key, Key, SignedTerm)
            ; invite(Key, String, Integer, Integer)
            ; register_again(Key, SignedTerm)
            ; redeem_link(Key, SignedTerm).
FriendActs ::= [] ; [FriendAct | FriendActs].
SignalReply ::= punch(Key, String) ; rejected.

exported procedure agent(Channel(FriendActs, Stream(Constant))?, Channel(Stream(NetMsg(Constant)), Stream(NetMsg(Constant)))?).
agent(ch(In, []), ch(_, [])) :- self_key(Me), acts(Me?, In?).

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
acts(Me, [invite(Rv, RvAddr, Serial, Expiry) | In]) :- ground(Me?) |
    sign(invitation(Me?, Rv?, RvAddr?, Serial?, Expiry?), Me?, L),
    registered(Me?, Rv?, L?),
    acts(Me?, In?).
acts(Me, [register_again(Rv, L) | In]) :- ground(Me?) |
    send_to_net([msg(Rv?, register(Me?, L?, Reply))]),
    on_registered(Reply?),
    acts(Me?, In?).
acts(Me, [redeem_link(Rv, L) | In]) :- ground(Me?) |
    sign(redeem(Me?, L?), Me?, S),
    send_to_net([msg(Rv?, redeem(Me?, S?, Reply))]),
    on_redeemed(Reply?),
    acts(Me?, In?).
acts(_, []).

procedure registered(Key?, Key?, SignedTerm?).
registered(Me, Rv, L) :- ground(L?) |
    send_to_net([msg(Rv?, register(Me?, L?, Reply))]),
    send_to_user([link(L?)]),
    on_registered(Reply?).

procedure on_registered(SignalReply?).
on_registered(punch(K, Addr)) :- ground(K?), ground(Addr?) |
    declare(address(K?, Addr?)),
    punch_udp(Addr?),
    send_to_user([invitee_punched(K?, Addr?)]).
on_registered(rejected) :- send_to_user([register_rejected]).

procedure on_redeemed(SignalReply?).
on_redeemed(punch(K, Addr)) :- ground(K?), ground(Addr?) |
    declare(peer(K?)),
    declare(address(K?, Addr?)),
    punch_udp(Addr?),
    send_to_user([inviter_punched(K?, Addr?)]).
on_redeemed(rejected) :- send_to_user([redeem_rejected]).

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
on_reply(F, punch(K, Addr)) :- ground(F?), K? =?= F?, ground(Addr?) |
    declare(address(K?, Addr?)),
    punch_udp(Addr?),
    send_to_user([punched(K?, Addr?)]).
on_reply(F, punch(K, _)) :- otherwise | send_to_user([other(F?, K?)]).
on_reply(F, rejected) :- send_to_user([rejected(F?)]).
''';

  // A scripted directory (GNA Section 7, "The messages"; GDS:2): on
  // list(A, Signed, Supply, Reply), where signature/2 reports Signed signed by
  // A over listing(A, Email, Name, Visibility), it answers verify, sends no
  // mail, asks on Supply for two links, more(2), and no more, and reports listed(A, Email,
  // Name, Visibility); a listing not so signed it answers rejected, closing
  // Supply, and reports unsigned; on supply(A, Links) it reports supplied(A,
  // Links); unlist it answers verify.
  const directoryProgram = r'''
SupplyAsk ::= more(Integer).
ListReply ::= verify ; rejected.
Visibility ::= listed ; unlisted.
DirName ::= String ; none.
DirMsg ::= list(Key, SignedTerm, Stream(SupplyAsk)?, ListReply?)
         ; supply(Key, List(SignedTerm))
         ; unlist(Key, SignedTerm, ListReply?).
DirNet ::= [] ; [NetMsg(DirMsg) | DirNet].
DirReport ::= listed(Key, String, DirName, Visibility) ; unsigned ; supplied(Key, List(SignedTerm)).

exported procedure agent(Channel(Stream(Constant), Stream(Constant))?, Channel(DirNet, Stream(NetMsg(Constant)))?).
agent(ch(_, []), ch(N, [])) :- dir(N?).

procedure dir(DirNet?).
dir([msg(_, list(A, S, Supply?, Reply?)) | Ms]) :- ground(A?), ground(S?) |
    signature(S?, Sig),
    on_listing(A?, Sig?, Supply, Reply),
    dir(Ms?).
dir([msg(_, supply(A, Links)) | Ms]) :- ground(A?), ground(Links?) |
    report(supplied(A?, Links?)),
    dir(Ms?).
dir([msg(_, unlist(_, _, R?)) | Ms]) :- answered(R), dir(Ms?).
dir([]).

procedure on_listing(Key?, Signature?, Stream(SupplyAsk), ListReply).
on_listing(A, signed(K, _, listing(A1, E, N, V)), [more(2)], verify) :-
    ground(A?), K? =?= A?, A1? =?= A?, ground(E?), ground(N?), ground(V?) |
    report(listed(A?, E?, N?, V?)).
on_listing(_, _, [], rejected) :- otherwise | report(unsigned).

procedure answered(ListReply).
answered(verify).

procedure report(DirReport?).
report(R) :- ground(R?) | send_to_user([R?]).
''';

  // A later version of the friends' program: the same, with one procedure
  // more, so that its source identity differs.
  const laterFriendProgram = '''$friendProgram
procedure friend_version(Integer).
friend_version(2).
''';

  late Directory dir;
  late String friendPath;
  late String laterFriendPath;
  late String directoryPath;
  late String serverExecutable;
  final glpRoot = Directory('../../programs').absolute.path;
  final rootSelfGlpPath = '$glpRoot/self.glp';
  final rvAgentPath = '$glpRoot/examples/rendezvous/rv_agent.glp';
  final coreDir = '$glpRoot/social/graph/core';

  setUpAll(() async {
    dir = Directory('../../programs/tests').createTempSync('glp_rendezvous_');
    friendPath = File('${dir.path}/friend.glp').absolute.path;
    File(friendPath).writeAsStringSync(friendProgram);
    final later = Directory('${dir.path}/later')..createSync();
    laterFriendPath = File('${later.path}/friend.glp').absolute.path;
    File(laterFriendPath).writeAsStringSync(laterFriendProgram);
    final directory = Directory('${dir.path}/directory')..createSync();
    directoryPath = File('${directory.path}/directory.glp').absolute.path;
    File(directoryPath).writeAsStringSync(directoryProgram);

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

  /// A phone running the Grassroots App's operating system,
  /// programs/social/graph/core, each a madGLP agent over its own headless
  /// layer, not yet started and given no first act.
  Future<_Friend> osPhone(int fill, String id) async {
    final identity = await identityFromSeed(fill);
    final layer = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: identity,
      sodium: sodium!,
    );
    addTearDown(layer.dispose);
    final key = glp.PubKey.fromList(identity.publicKey);
    final agent = AgentRuntime(
      agentId: id,
      program: coreDir,
      rootSelfGlpPath: rootSelfGlpPath,
      keyPair: (pub: key, priv: Uint8List.fromList(identity.privateKey)),
      network: HeadlessGlpNetwork(layer),
    );
    addTearDown(agent.dispose);
    final f = _Friend(key, layer, agent);
    agent.onUserOutput = f.say;
    agent.onOutput = f.transcript.add;
    await agent.initialize();
    return f;
  }

  /// The server's port, its key, and its key pair as an identity file.
  Future<_Server> serverIdentity(int rvFill) async {
    final probe = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    final rvPort = probe.port;
    probe.close();
    final rvSeed = Uint8List.fromList(List.filled(32, rvFill));
    final rvIdentityFile = File('${dir.path}/rv-$rvFill.json')
      ..writeAsStringSync(jsonEncode({'seed': _hex(rvSeed)}));
    final rvKey = glp.PubKey.fromList((await GrassrootsIdentity.create(
            keyPair: await Ed25519().newKeyPairFromSeed(rvSeed)))
        .publicKey);
    return (
      rvKey: rvKey,
      rvPort: rvPort,
      identityPath: rvIdentityFile.path,
      rvFill: rvFill
    );
  }

  /// The servers launched, by the fill of their seed, each with its last run.
  final launched = <int, ({_Server srv, _Run run})>{};

  /// The server, a process of its own, started and listening, its state kept
  /// in a file of its own across a restart; the process and what it says.
  Future<_Run> startServer(_Server srv) async {
    final (:rvKey, :rvPort, :identityPath, :rvFill) = srv;
    final server = await Process.start(serverExecutable, [
      '--identity',
      identityPath,
      '--port',
      '$rvPort',
      '--public-address',
      '127.0.0.1:$rvPort',
      '--outbound-queue-file',
      '${dir.path}/rv-$rvFill-queue.json',
      '--state-file',
      '${dir.path}/rv-$rvFill-state',
      '--program',
      rvAgentPath,
      '--root-self',
      rootSelfGlpPath,
    ]);
    addTearDown(() async {
      server.kill(ProcessSignal.sigterm);
      await server.exitCode;
    });
    final serverLines = <String>[];
    final serverLine = StreamController<String>.broadcast();
    final serverErr = StringBuffer();
    server.stdout
        .transform(utf8.decoder)
        .transform(const LineSplitter())
        .listen((l) {
      serverLines.add(l);
      serverLine.add(l);
    });
    server.stderr.transform(utf8.decoder).listen(serverErr.write);
    addTearDown(() => printOnFailure('the server said '
        '${serverLines.join(' | ')}; stderr: $serverErr'));
    final ready = (await _line(
            serverLines, serverLine.stream, (l) => l.startsWith('READY ')))
        .split(' ');
    expect(ready, ['READY', '$rvPort', rvKey.hex],
        reason: 'the server listens on its port, named by its key');
    final run = (process: server, lines: serverLines, more: serverLine.stream);
    launched[rvFill] = (srv: srv, run: run);
    return run;
  }

  /// The server, a process of its own, started and listening; what it says,
  /// line by line.
  Future<List<String>> launch(_Server srv) async =>
      (await startServer(srv)).lines;

  /// The server, a process of its own, and two friends, each a madGLP agent
  /// over its own headless layer, which is declared the server, holding each
  /// other's signature over their pair; each friend's layer started.  B runs
  /// [bProgram], by default the friends' program, A's.
  Future<_World> standUp(int rvFill, int aFill, int bFill,
      {String? bProgram}) async {
    final srv = await serverIdentity(rvFill);
    final (:rvKey, :rvPort, identityPath: _, rvFill: _) = srv;

    // The two friends, each a madGLP agent over its own headless layer
    // declaring the server.
    Future<_Friend> friend(int fill, String id, String program) async {
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
        program: program,
        rootSelfGlpPath: rootSelfGlpPath,
        keyPair: (pub: key, priv: Uint8List.fromList(identity.privateKey)),
        network: HeadlessGlpNetwork(layer),
      );
      addTearDown(agent.dispose);
      final f = _Friend(key, layer, agent);
      agent.onUserOutput = f.say;
      agent.onOutput = f.transcript.add;
      await agent.initialize();
      return f;
    }

    final a = await friend(aFill, 'a', friendPath);
    final b = await friend(bFill, 'b', bProgram ?? friendPath);

    // Each signs the pair and the signatures change hands, as on befriending.
    await a.agent.injectUserInput(_struct('attest', [_const(b.key.hex)]));
    await b.agent.injectUserInput(_struct('attest', [_const(a.key.hex)]));
    final fromA = await a.saidOne('attestation');
    final fromB = await b.saidOne('attestation');
    final sigA = fromA.args[1];
    final sigB = fromB.args[1];
    final graphA = _value(fromA.args[2]);
    final graphB = _value(fromB.args[2]);
    if (bProgram == null) {
      expect(graphB, graphA, reason: 'both signed under the one program');
    } else {
      expect(graphB, isNot(graphA),
          reason: 'B signed under another program, a later version');
    }

    // The server, a process of its own.
    final serverLines = await launch(srv);

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

    // Each declared the address it was told to its layer (declare(address(P, A)),
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
  }, timeout: Timeout.none);

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
    // tells their layers (declare(peer(P)), declare(address(P, A))), and connect.
    a.layer.wan.putPeerAddress(b.key.bytes, '127.0.0.1:${b.layer.boundPort}');
    b.layer.wan.putPeerAddress(a.key.bytes, '127.0.0.1:${a.layer.boundPort}');
    // The heartbeat sweep connects the two.
    await _untilBothReachable(a.layer, b.layer);

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

    // B declared A's new address (declare(address(P, A))), and the two are
    // reachable again, B observing A there.
    expect(b.layer.store.state.knownPeers.known[a.key.hex], toA);
    // Reconnected.
    await _untilBothReachable(a.layer, b.layer);
    expect(_loopback(b.layer.wan.observedPeerAddress(a.key.bytes)!, newPort),
        isTrue);
    expect(serverLines, contains('CONNECTED ${a.key.hex}'));
  }, timeout: Timeout.none);

  // GSA, "Distribution and Activation", "Reconnection": the rendezvous agent
  // checks that each request is signed by the key it names and that the two
  // signatures are the two friends' over their pair, and checks keys and no
  // program identity.  B runs a later version of the friends' program, so
  // the two signatures were made under two programs; both requests are
  // admitted, and each friend is told the address of the other.
  test(
      'two friends whose signatures over their pair were made under two '
      'different programs, as under two versions of the Grassroots App, are '
      'matched by their keys', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final (:rvKey, :a, :b, :sigA, :sigB, serverLines: _) =
        await standUp(0x7c, 0x3e, 0x4f, bProgram: laterFriendPath);

    await a.agent.injectUserInput(
        _struct('reconnect', [_const(rvKey.hex), _const(b.key.hex), sigB]));
    await b.agent.injectUserInput(
        _struct('available', [_const(rvKey.hex), _const(a.key.hex), sigA]));

    final punchedA = await a.saidOne('punched');
    final punchedB = await b.saidOne('punched');
    expect(_value(punchedA.args[0]), b.key.hex);
    expect(_loopback(_value(punchedA.args[1]), b.layer.boundPort!), isTrue,
        reason: _value(punchedA.args[1]));
    expect(_value(punchedB.args[0]), a.key.hex);
    expect(_loopback(_value(punchedB.args[1]), a.layer.boundPort!), isTrue,
        reason: _value(punchedB.args[1]));
    expect(a.said.whereType<rt.StructTerm>().map((t) => t.functor),
        isNot(contains('rejected')));
    expect(b.said.whereType<rt.StructTerm>().map((t) => t.functor),
        isNot(contains('rejected')));
  }, timeout: Timeout.none);

  // GRV:2 and GRV:3 over the WAN on today's core (GNA Section 6, "Invitation
  // links"; GSA paper, Section 4.2, "Legacy online connections", and Appendix
  // "The Operating System"): two phones, each the Grassroots App's operating
  // system, programs/social/graph/core, started by its first act
  // link_phone(Me, [], Servers, none) (core's self.glp: the operating system on
  // the layer, os_phone/8, with a person who answers yes on each card and
  // whose acts are home's requests on the person's grants).  Alice's names
  // the server, which her operating system declares to its layer; Bob's names
  // none.  Alice invites by link: her operating system makes the link, signs
  // it, registers it with the server and tells her it, invitation(Link), as
  // text.  Bob opens the text: his operating system verifies the inviter's
  // signature and the expiry, declares Alice a peer and the server the link
  // names, and redeems it, signed by him; the server answers each with the
  // other's key and address, each declares the other at it and punches, and
  // Bob's cold call is a request to connect, which Alice's person answers
  // yes, so the two are friends: each is told connected(other).
  test(
      'a link made by invite_link on one phone running the operating system '
      'and opened on another is redeemed at the server, both punch, and the '
      'two befriend', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final srv = await serverIdentity(0x7b);
    final rvAddress = '127.0.0.1:${srv.rvPort}';

    final alice = await osPhone(0x3d, 'alice');
    final bob = await osPhone(0x4e, 'bob');
    final serverLines = await launch(srv);
    expect(await alice.layer.start(), isTrue);
    expect(await bob.layer.start(), isTrue);

    // The first acts: Alice's names the server, Bob's none.
    await alice.agent.injectUserInput(_struct('link_phone', [
      _const(alice.key.hex),
      _nil,
      _list([
        _struct('rendezvous', [_const(srv.rvKey.hex), _const(rvAddress)])
      ]),
      _const('none'),
    ]));
    await bob.agent.injectUserInput(_struct(
        'link_phone', [_const(bob.key.hex), _nil, _nil, _const('none')]));

    // Alice invites by link, and is told the link, as text.
    await alice.agent.injectUserInput(_grant(_const('invite_link')));
    final invitation = await alice.toldOne('invitation');
    final link = invitation.args[0];
    expect(_value(link), matches(RegExp(r'^[0-9a-f]+$')),
        reason: 'a signed term is text, the hex of the signed content');

    // Bob opens it.
    await bob.agent.injectUserInput(_grant(_struct('open_link', [link])));

    // Each is told the other a friend: the link redeemed, the two punched,
    // Bob's cold call a request to connect, and Alice's person's yes.
    final toAlice = await alice.toldOne('connected');
    final toBob = await bob.toldOne('connected');
    expect(_value(toAlice.args[0]), bob.key.hex);
    expect(_value(toBob.args[0]), alice.key.hex);

    // Each declared the other at the address the server observes for it.
    expect(alice.layer.store.state.knownPeers.known[bob.key.hex],
        startsWith('127.0.0.1:'));
    expect(bob.layer.store.state.knownPeers.known[alice.key.hex],
        startsWith('127.0.0.1:'));
    // Bob declared the server the link names, and both held a session with
    // it.
    expect(serverLines, contains('CONNECTED ${alice.key.hex}'));
    expect(serverLines, contains('CONNECTED ${bob.key.hex}'));
    for (final p in [alice, bob]) {
      expect(p.told.map((c) => c.functor), isNot(contains('rejected')),
          reason: '${p.agent.agentId} was told ${p.told}');
    }
  }, timeout: Timeout.none);

  // IGLP, Definition "Restart" ("the local state s_p of p at restart is its
  // local state at termination"), for the rendezvous agent (GNA Section 6,
  // "Invitation links"): A registers a link with the server; the server
  // refuses the same serial registered again, an answer that leaves the
  // server only from a state its store holds (IGLP, Implementation Notes,
  // "Persistence"), so the registration is saved.  The server stops and
  // starts again from its state file, at its address and under its key; once
  // both friends' layers hold a session with it again, B redeems the link,
  // which the restarted server finds registered and unused: each is told the
  // other's key and address.
  test(
      'a registration survives a restart of the server: the server, '
      'restarted from its state file, redeems the link registered before',
      () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final (:rvKey, :a, :b, sigA: _, sigB: _, serverLines: _) =
        await standUp(0x79, 0x35, 0x46);
    final (:srv, :run) = launched[0x79]!;
    final rvAddress = '127.0.0.1:${srv.rvPort}';
    final expiry = DateTime.now().millisecondsSinceEpoch + 3600 * 1000;

    await a.agent.injectUserInput(_struct('invite',
        [_const(rvKey.hex), _const(rvAddress), rt.ConstTerm(1), rt.ConstTerm(expiry)]));
    final link = (await a.saidOne('link')).args[0];
    await a.agent.injectUserInput(
        _struct('register_again', [_const(rvKey.hex), link]));
    expect(await a.saidAtom('register_rejected'), isNotNull,
        reason: 'the serial is registered, and the state that holds it saved');

    // The server stops, and starts again from its state file.
    run.process.kill(ProcessSignal.sigterm);
    await run.process.exitCode;
    final again = await startServer(srv);
    await _line(again.lines, again.more, (l) => l == 'CONNECTED ${a.key.hex}');
    await _line(again.lines, again.more, (l) => l == 'CONNECTED ${b.key.hex}');

    await b.agent.injectUserInput(
        _struct('redeem_link', [_const(rvKey.hex), link]));
    final toB = await b.saidOne('inviter_punched');
    final toA = await a.saidOne('invitee_punched');
    expect(_value(toB.args[0]), a.key.hex);
    expect(_loopback(_value(toB.args[1]), a.layer.boundPort!), isTrue,
        reason: _value(toB.args[1]));
    expect(_value(toA.args[0]), b.key.hex);
    expect(_loopback(_value(toA.args[1]), b.layer.boundPort!), isTrue,
        reason: _value(toA.args[1]));
  }, timeout: Timeout.none);

  // GDS:2 and GDS:4 on the app's side over the WAN (GNA Section 7, "The
  // messages"): a scripted directory, a madGLP agent over a headless layer
  // marked to answer any key at an address of its own, as a rendezvous
  // server's is ("Hosting": "A directory is hosted as a rendezvous server
  // is"), and two phones running the operating system.  Alice's first act
  // names the server and the directory.  Alice lists herself, List with her
  // email address and name: her operating system declares the directory,
  // signs listing(A, Email, Name, listed) and sends list(A, Signed, Supply,
  // Reply); the directory answers verify, which she is told, verify(Email),
  // and asks for two links, more(2); her operating system makes two links,
  // registers each with the server and supplies them, supply(A, [L1, L2]).
  // Bob, who looked her up, opens a link the directory holds of hers: it is
  // redeemed at the server, and the two befriend.
  test(
      'a person lists herself with a scripted directory, is told verify, '
      'supplies the two links it asks for, and one of them, opened on another '
      'phone, is redeemed and the two befriend', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final srv = await serverIdentity(0x7a);
    final rvAddress = '127.0.0.1:${srv.rvPort}';

    // The directory: its port, its layer marked to answer any key, and its
    // scripted program.
    final probe = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    final dirPort = probe.port;
    probe.close();
    final dirAddress = '127.0.0.1:$dirPort';
    final dirIdentity = await identityFromSeed(0x6c);
    final dirLayer = HeadlessGrassrootsNetwork(
      identity: dirIdentity,
      serverAddress:
          ServerAddress(listenPort: dirPort, publicAddress: dirAddress),
      rendezvousServer: true,
      sodium: sodium!,
      outboundQueueStore: MemoryOutboundQueueStore(),
    );
    addTearDown(dirLayer.dispose);
    final dirKey = glp.PubKey.fromList(dirIdentity.publicKey);
    final dirAgent = AgentRuntime(
      agentId: 'directory',
      program: directoryPath,
      rootSelfGlpPath: rootSelfGlpPath,
      keyPair: (pub: dirKey, priv: Uint8List.fromList(dirIdentity.privateKey)),
      network: HeadlessGlpNetwork(dirLayer),
    );
    addTearDown(dirAgent.dispose);
    final directory = _Friend(dirKey, dirLayer, dirAgent);
    dirAgent.onUserOutput = directory.say;
    dirAgent.onOutput = directory.transcript.add;
    await dirAgent.initialize();

    final alice = await osPhone(0x3f, 'alice');
    final bob = await osPhone(0x4f, 'bob');
    await launch(srv);
    expect(await dirLayer.start(), isTrue);
    expect(await alice.layer.start(), isTrue);
    expect(await bob.layer.start(), isTrue);

    await alice.agent.injectUserInput(_struct('link_phone', [
      _const(alice.key.hex),
      _nil,
      _list([
        _struct('rendezvous', [_const(srv.rvKey.hex), _const(rvAddress)])
      ]),
      _struct('directory', [_const(dirKey.hex), _const(dirAddress)]),
    ]));
    await bob.agent.injectUserInput(_struct(
        'link_phone', [_const(bob.key.hex), _nil, _nil, _const('none')]));

    // Alice lists herself, by her email address and her name.
    await alice.agent.injectUserInput(_grant(
        _struct('list', [_const('alice@example.org'), _const('Alice')])));

    // The directory holds her signed listing and answers verify, which she is
    // told.
    final listed = await directory.saidOne('listed');
    expect(_value(listed.args[0]), alice.key.hex);
    expect(_value(listed.args[1]), 'alice@example.org');
    expect(_value(listed.args[2]), 'Alice');
    expect(_value(listed.args[3]), 'listed');
    final verify = await alice.toldOne('verify');
    expect(_value(verify.args[0]), 'alice@example.org');

    // It asked for two links, and she supplied two.
    final supplied = await directory.saidOne('supplied');
    expect(_value(supplied.args[0]), alice.key.hex);
    final links = _items(supplied.args[1]);
    expect(links, hasLength(2));

    // Bob, who looked her up, opens one of them: it is redeemed, and the two
    // befriend.
    await bob.agent.injectUserInput(_grant(_struct('open_link', [links[0]])));
    final toAlice = await alice.toldOne('connected');
    final toBob = await bob.toldOne('connected');
    expect(_value(toAlice.args[0]), bob.key.hex);
    expect(_value(toBob.args[0]), alice.key.hex);
    for (final p in [alice, bob]) {
      expect(p.told.map((c) => c.functor), isNot(contains('rejected')),
          reason: '${p.agent.agentId} was told ${p.told}');
    }
  }, timeout: Timeout.none);
}

/// The server's port, its key, its identity file and the fill of its seed.
/// A run of the server: its process, what it has said and what it says next.
typedef _Run = ({Process process, List<String> lines, Stream<String> more});

typedef _Server = ({
  glp.PubKey rvKey,
  int rvPort,
  String identityPath,
  int rvFill,
});

/// Completes once [a] and [b] each hold the other reachable: checked now and
/// after every reachability event of either layer, whose callbacks are
/// wrapped while it waits, each calling the one set before it first, and
/// restored after.
Future<void> _untilBothReachable(
    HeadlessGrassrootsNetwork a, HeadlessGrassrootsNetwork b) async {
  bool holds() =>
      a.isPeerReachable(b.identity.publicKey) &&
      b.isPeerReachable(a.identity.publicKey);
  if (holds()) return;
  final done = Completer<void>();
  void check() {
    if (!done.isCompleted && holds()) done.complete();
  }

  final restore = <void Function()>[];
  for (final layer in [a, b]) {
    final connected = layer.onPeerConnected;
    final disconnected = layer.onPeerDisconnected;
    layer.onPeerConnected = (pk, underlay, application) {
      connected?.call(pk, underlay, application);
      check();
    };
    layer.onPeerDisconnected = (pk, underlay) {
      disconnected?.call(pk, underlay);
      check();
    };
    restore.add(() {
      layer.onPeerConnected = connected;
      layer.onPeerDisconnected = disconnected;
    });
  }
  await done.future;
  for (final undo in restore) {
    undo();
  }
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
  final StreamController<rt.Term> _said = StreamController.broadcast();
  final List<String> transcript = [];

  /// What the agent sends its person.
  void say(rt.Term t) {
    said.add(t);
    _said.add(t);
  }

  /// What the operating system told its person: the content of each notice,
  /// msg(agent, person, Content), that it sent.
  Iterable<rt.StructTerm> get told => said
      .whereType<rt.StructTerm>()
      .where((t) => t.functor == 'msg' && t.args.length == 3)
      .map((t) => t.args[2])
      .whereType<rt.StructTerm>();

  /// The content of the first notice msg(agent, person, Content) this agent
  /// sent its person whose functor is [functor], once it has sent it.
  Future<rt.StructTerm> toldOne(String functor) async {
    rt.StructTerm? content(rt.Term t) {
      if (t is rt.StructTerm && t.functor == 'msg' && t.args.length == 3) {
        final c = t.args[2];
        if (c is rt.StructTerm && c.functor == functor) return c;
      }
      return null;
    }

    for (final t in said) {
      final c = content(t);
      if (c != null) return c;
    }
    printOnFailure('${agent.agentId} waits to be told $functor');
    return content(await _said.stream.firstWhere((t) => content(t) != null))!;
  }

  /// The first element this agent sent its person whose functor is
  /// [functor], once it has sent it.
  /// The first constant [name] this agent sent its person, once it has sent
  /// it.
  Future<rt.Term> saidAtom(String name) async {
    bool named(rt.Term t) => t is rt.ConstTerm && t.value == name;
    for (final t in said) {
      if (named(t)) return t;
    }
    printOnFailure('${agent.agentId} waits for $name');
    return _said.stream.firstWhere(named);
  }

  Future<rt.StructTerm> saidOne(String functor) async {
    bool named(rt.Term t) => t is rt.StructTerm && t.functor == functor;
    for (final t in said) {
      if (named(t)) return t as rt.StructTerm;
    }
    printOnFailure('${agent.agentId} waits for $functor');
    return await _said.stream.firstWhere(named) as rt.StructTerm;
  }
}

rt.Term _const(String value) => rt.ConstTerm(value);

rt.Term _struct(String functor, List<rt.Term> args) =>
    rt.StructTerm(functor, args);

/// The empty list.
final rt.Term _nil = rt.ConstTerm(rt.nil);

/// The list of [items].
rt.Term _list(List<rt.Term> items) =>
    items.reversed.fold(_nil, (tail, head) => rt.StructTerm('.', [head, tail]));

/// The elements of the list term [list].
List<rt.Term> _items(rt.Term list) {
  final items = <rt.Term>[];
  var t = list;
  while (t is rt.StructTerm && t.functor == '.' && t.args.length == 2) {
    items.add(t.args[0]);
    t = t.args[1];
  }
  return items;
}

/// A grant of the person's request [request] on home's request menu, as the
/// person's grants carry it, input(N, Path, Request): link_phone reads the
/// request (core's link_acts/3).
rt.Term _grant(rt.Term request) =>
    rt.StructTerm('input', [rt.ConstTerm(1), _nil, request]);

/// The text of a constant the agent sent.
String _value(rt.Term t) => (t as rt.ConstTerm).value.toString();

/// The first line of [lines], or of the lines still to come on [more], that
/// [test] accepts.
Future<String> _line(
    List<String> lines, Stream<String> more, bool Function(String) test) {
  for (final l in lines) {
    if (test(l)) return Future.value(l);
  }
  return more.firstWhere(test);
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
