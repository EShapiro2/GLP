/// The runtime's own host over GNA's adapter: two `AgentRuntime`s, each over
/// its own headless layer (`HeadlessGrassrootsNetwork`) through the adapter
/// (`HeadlessGlpNetwork`), the networks given to the hosts (GLP #3 Cowork,
/// 2026-10-11 12:59 UTC, "01:05"; IGLP, Implementation Notes, "Networking
/// Seam", One interface: "transport integration is a backend swap").  Each
/// agent is named by its key (IGLP, Remark "Agent Names").
///
/// A cold call crosses from the caller to the callee over UDP on the loopback,
/// and an assignment crosses back, as they cross in GNA's adapter test
/// (networking/glue/test/headless_glp_network_test.dart), which hosts its
/// agents through `AgentRuntime` too since GNA's Task E (1739a459).
///
/// Needs a native libsodium, as GNA's headless tests do; self-skips when it
/// is absent.
library;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart' show Ed25519;
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_glue/headless_glp_network.dart';
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:sodium/sodium_sumo.dart';

import 'programs_dir.dart';

/// GNA's adapter test's play: the caller cold-calls the callee, naming it by
/// the key its host passes, with a writer; the callee assigns the writer.
const String _play = r'''
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

void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;
  late Directory dir;
  late String play;
  late String root;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping: $e';
    }
    // Under the root, programs/ (TGLP modules.tex, "Scope construction").
    dir = Directory('${programsDir()}/tests')
        .createTempSync('glp_headless_host_');
    play = '${dir.path}/play.glp';
    File(play).writeAsStringSync(_play);
    root = '${programsDir()}/self.glp';
  });
  tearDownAll(() => dir.deleteSync(recursive: true));

  Future<GrassrootsIdentity> identityOf(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
        nickname: 'agent',
      );

  /// The key pair the host holds for the person: the layer's own.
  ({glp.PubKey pub, Uint8List priv}) keyPairOf(GrassrootsIdentity id) => (
        pub: glp.PubKey.fromList(id.publicKey),
        priv: Uint8List.fromList(id.privateKey),
      );

  Future<String> line(List<String> lines, bool Function(String) test) async {
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

  test('AgentRuntime over the headless adapter: a cold call and an assignment '
      'cross between two agents named by their keys', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final callerId = await identityOf(0x5a);
    final calleeId = await identityOf(0x6b);
    final callerKp = keyPairOf(callerId);
    final calleeKp = keyPairOf(calleeId);

    // The callee's layer, which answers no unsolicited contact from a key
    // outside its known-peer set (GNA, "Cold Calls", Trust levels).
    final calleeLayer = HeadlessGrassrootsNetwork(
      identity: calleeId,
      rendezvousServers: const [],
      sodium: sodium!,
    );
    addTearDown(calleeLayer.dispose);
    final calleeNet = HeadlessGlpNetwork(calleeLayer);
    calleeNet.putKnownPeer(callerKp.pub);

    // The caller's layer, given the callee's key and address: what
    // peer_declare/1 and address_declare/2 give it.
    final callerLayer = HeadlessGrassrootsNetwork(
      identity: callerId,
      rendezvousServers: const [],
      sodium: sodium!,
    );
    addTearDown(callerLayer.dispose);
    final callerNet = HeadlessGlpNetwork(callerLayer);

    final callee = AgentRuntime(
      agentId: 'callee',
      program: play,
      rootSelfGlpPath: root,
      goalLabel: 'callee_init/2',
      keyPair: calleeKp,
      network: calleeNet,
    );
    final caller = AgentRuntime(
      agentId: 'caller',
      program: play,
      rootSelfGlpPath: root,
      goalLabel: 'caller_init/3',
      extraArgs: [calleeKp.pub.hex],
      keyPair: callerKp,
      network: callerNet,
    );
    final calleeOut = <String>[];
    final callerOut = <String>[];
    callee.onOutput = calleeOut.add;
    caller.onOutput = callerOut.add;
    final connected = <String>[];
    caller.onPeerConnected =
        (pk, underlay, identity) => connected.add('${pk.hex} $underlay');

    expect(caller.name, callerKp.pub.hex);
    expect(callee.name, calleeKp.pub.hex);

    // The callee is bound to its layer before the layer starts, so that no
    // message the layer delivers finds no agent; the caller's layer starts,
    // and is given the callee's key and address, before the caller runs and
    // sends.
    await callee.initialize();
    expect(await calleeLayer.start(), isTrue);
    expect(await callerLayer.start(), isTrue);
    callerNet.putKnownPeer(calleeKp.pub);
    callerNet.putPeerAddress(calleeKp.pub, '127.0.0.1:${calleeLayer.boundPort}');
    await caller.initialize();

    // The cold call crosses: the callee's program receives it on its network
    // input, addressed to its own name, its key.
    final called = await line(calleeOut, (l) => l.startsWith('< called('));
    expect(called, contains(calleeKp.pub.hex));

    // The assignment crosses back.
    final answered = await line(callerOut, (l) => l.startsWith('< answered('));
    expect(answered, contains('forty_two'));

    // The caller's host surfaced the layer's report of the callee, over the
    // WAN, the headless profile's one underlay.
    expect(connected, contains('${calleeKp.pub.hex} ${glp.Underlay.wan}'));
  }, timeout: const Timeout(Duration(minutes: 3)));
}
