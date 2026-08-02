// IP end-to-end conformance agent (GLP Networking API paper).
//
// One agent role per invocation, selected with --dart-define=E2E_ROLE=<role>:
//
//   A   — phase 1 initiator on simulator 1: mints the peer link, observes
//         redemption, exchanges messages, verifies heartbeat stability and
//         silent-death teardown, then observes the stranger C (Closed trust).
//   B   — phase 1 responder on simulator 2: redeems the link, exchanges
//         messages, then goes silent (the host kills the app).
//   C   — stranger on simulator 2: knows A's address+key out-of-band, is
//         unknown to A, and attempts contact without an invite.
//   A2  — phase 2 dialer on simulator 1: putPeerAddress-driven direct dial,
//         no rendezvous server running.
//   B2  — phase 2 listener on simulator 2.
//
// Coordination runs over the host blackboard (tool/e2e_blackboard.dart);
// simulators share the host network, so 127.0.0.1 reaches the host. The
// rendezvous server is bootstrap_anchor on the host.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:integration_test/integration_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

const role = String.fromEnvironment('E2E_ROLE');
const bbUrl =
    String.fromEnvironment('E2E_BB', defaultValue: 'http://127.0.0.1:8787');
const anchorAddr = String.fromEnvironment('E2E_ANCHOR_ADDR',
    defaultValue: '127.0.0.1:9516');
const anchorPk = String.fromEnvironment('E2E_ANCHOR_PK');
const runId = String.fromEnvironment('E2E_RUN', defaultValue: 'r0');

// ===== Blackboard client =====

final _http = HttpClient();

Future<void> bbPut(String key, String value) async {
  final req = await _http.putUrl(Uri.parse('$bbUrl/$runId-$key'));
  req.write(value);
  final res = await req.close();
  await res.drain<void>();
}

Future<String> bbWait(String key, {int timeoutSeconds = 900}) async {
  final deadline = DateTime.now().add(Duration(seconds: timeoutSeconds));
  while (true) {
    final remaining = deadline.difference(DateTime.now()).inSeconds;
    if (remaining <= 0) {
      throw TimeoutException('blackboard key "$key" never appeared');
    }
    final req = await _http
        .getUrl(Uri.parse('$bbUrl/wait/$runId-$key?t=${remaining.clamp(1, 60)}'));
    final res = await req.close();
    final body = await utf8.decodeStream(res);
    if (res.statusCode == 200) return body;
  }
}

// ===== Event journal =====

final _journal = <String>[];
final _startedAt = DateTime.now();

/// Live feed rendered on the device screen so the run is watchable in
/// Simulator.app.
final _feed = ValueNotifier<List<String>>(const []);

void note(String line) {
  final t = DateTime.now().difference(_startedAt);
  final stamped = '[+${t.inSeconds}.${(t.inMilliseconds % 1000) ~/ 100}s] $line';
  _journal.add(stamped);
  _feed.value = [..._feed.value, stamped];
  debugPrint('[e2e:$role] $stamped');
}

Future<void> postReport({required bool pass, String? failure}) async {
  await bbPut(
    'report_$role',
    jsonEncode({
      'role': role,
      'pass': pass,
      if (failure != null) 'failure': failure,
      'journal': _journal,
    }),
  );
}

// ===== Agent scaffolding =====

String hexOf(List<int> pubkey) =>
    pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

Uint8List pubkeyFromHex(String hex) => Uint8List.fromList(List.generate(
    hex.length ~/ 2, (i) => int.parse(hex.substring(i * 2, i * 2 + 2), radix: 16)));

class Agent {
  Agent(this.identity, this.store, this.network);

  final GrassrootsIdentity identity;
  final Store<AppState> store;
  final GrassrootsNetwork network;

  /// (senderHex, utf8 payload or '<binary n bytes>') in arrival order.
  final messages = <(String, Uint8List)>[];
  final connects = <(String, MessageTransport, DateTime)>[];
  final disconnects = <(String, MessageTransport, DateTime)>[];
  final redemptions = <String>[];

  static Future<Agent> spawn(String nickname) async {
    final sodium = await SodiumSumoInit.init();
    final identity = await GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPair(),
      nickname: nickname,
    );
    final store = Store<AppState>(
      appReducer,
      // Simulators have no usable BLE; IP-only run. Closed trust is the
      // default (spec: until set, the level is Closed) — left untouched.
      initialState: const AppState(
        settings: SettingsState(bluetoothEnabled: false),
      ),
    );
    final network = GrassrootsNetwork(
      identity: identity,
      store: store,
      sodium: sodium,
    );
    final agent = Agent(identity, store, network);
    network.onMessageReceived = (messageId, sender, payload, transport) {
      agent.messages.add((hexOf(sender), payload));
      note('onMessageReceived from ${hexOf(sender).substring(0, 8)} '
          '(${payload.length} bytes, $transport)');
    };
    network.onPeerConnected = (pubkey, transport, attestedBinaryHash) {
      agent.connects.add((hexOf(pubkey), transport, DateTime.now()));
      note('onPeerConnected ${hexOf(pubkey).substring(0, 8)} via $transport');
    };
    network.onPeerDisconnected = (pubkey, transport) {
      agent.disconnects.add((hexOf(pubkey), transport, DateTime.now()));
      note('onPeerDisconnected ${hexOf(pubkey).substring(0, 8)} via $transport');
    };
    network.onPeerLinkRedeemed = (redeemer) {
      agent.redemptions.add(hexOf(redeemer));
      note('onPeerLinkRedeemed by ${hexOf(redeemer).substring(0, 8)}');
    };

    final ok = await network.initialize();
    if (!ok) throw StateError('GrassrootsNetwork.initialize() failed');
    note('initialized; identity ${hexOf(identity.publicKey).substring(0, 8)}');
    return agent;
  }

  /// The loopback address a peer on the same host can dial: the loopback of
  /// the family the discovered public address belongs to, with our real
  /// bound port.
  Future<String> loopbackAddress({int timeoutSeconds = 90}) async {
    final deadline = DateTime.now().add(Duration(seconds: timeoutSeconds));
    while (DateTime.now().isBefore(deadline)) {
      final publicAddr = network.getPublicAddress();
      if (publicAddr != null && publicAddr.isNotEmpty) {
        final portIndex = publicAddr.lastIndexOf(':');
        final port = publicAddr.substring(portIndex + 1);
        final isV6 = publicAddr.startsWith('[');
        return isV6 ? '[::1]:$port' : '127.0.0.1:$port';
      }
      await Future.delayed(const Duration(milliseconds: 500));
    }
    throw TimeoutException('public address never discovered');
  }

  Future<void> waitFor(
    bool Function() predicate,
    String what, {
    int timeoutSeconds = 120,
  }) async {
    final deadline = DateTime.now().add(Duration(seconds: timeoutSeconds));
    while (!predicate()) {
      if (DateTime.now().isAfter(deadline)) {
        throw TimeoutException('timed out waiting for $what');
      }
      await Future.delayed(const Duration(milliseconds: 250));
    }
    note('observed: $what');
  }

  bool udpConnectedTo(String peerHex) =>
      connects.any((c) => c.$1 == peerHex && c.$2 == MessageTransport.udp);
  bool udpDisconnectedFrom(String peerHex) =>
      disconnects.any((d) => d.$1 == peerHex && d.$2 == MessageTransport.udp);
  bool receivedFrom(String peerHex, bool Function(Uint8List) match) =>
      messages.any((m) => m.$1 == peerHex && match(m.$2));
}

/// Payload above the 320-byte fragment threshold, with a verifiable pattern.
Uint8List bigPayload(int length) =>
    Uint8List.fromList(List.generate(length, (i) => (i * 7 + 3) % 256));

bool isBigPayload(Uint8List p, int length) {
  if (p.length != length) return false;
  for (var i = 0; i < p.length; i++) {
    if (p[i] != (i * 7 + 3) % 256) return false;
  }
  return true;
}

// ===== Roles =====

Future<void> runA() async {
  final agent = await Agent.spawn('A');
  final myHex = hexOf(agent.identity.publicKey);
  await bbPut('pk_A', myHex);

  // Item 1: two distinct agents — compare identities across the blackboard.
  final bHex = await bbWait('pk_B');
  expect(bHex, isNot(equals(myHex)), reason: 'A and B must be distinct');
  expect(myHex.length, equals(64));
  note('item1: distinct identities OK (A=${myHex.substring(0, 8)}, '
      'B=${bHex.substring(0, 8)})');

  // Item 2: rendezvous + peer link.
  final rvOk = await agent.network.addRendezvousServer(
    address: anchorAddr,
    pubkeyHex: anchorPk,
  );
  expect(rvOk, isTrue, reason: 'anchor must respond');
  note('rendezvous server configured');

  final link = await agent.network.generatePeerLink();
  note('peer link minted (${link.length} chars)');
  await bbPut('peer_link', link);

  await agent.waitFor(() => agent.redemptions.contains(bHex),
      'peer link redeemed by B', timeoutSeconds: 180);
  // The redemption decision is GLP's: accept B by supplying the key.
  agent.network.putKnownPeer(pubkeyFromHex(bHex));
  note('accepted redeemer: putKnownPeer(B)');

  await agent.waitFor(() => agent.udpConnectedTo(bHex),
      'onPeerConnected(B, IP)', timeoutSeconds: 180);
  expect(agent.network.isPeerReachable(pubkeyFromHex(bHex)), isTrue);
  note('item2: session established, Transport IP');

  // Item 3: messages both directions (B sent first; then we reply and send
  // a payload above the BLE fragment threshold).
  await agent.waitFor(
      () => agent.receivedFrom(bHex, (p) => utf8.decode(p) == 'hello-from-B'),
      'hello-from-B');
  final smallId = await agent.network
      .send(pubkeyFromHex(bHex), Uint8List.fromList(utf8.encode('hello-from-A')));
  expect(smallId, isNotNull);
  final bigId =
      await agent.network.send(pubkeyFromHex(bHex), bigPayload(2000));
  expect(bigId, isNotNull);
  note('item3: sent hello-from-A and 2000-byte payload');
  expect(await bbWait('b_item3_ok'), equals('ok'));
  note('item3: B confirmed both arrivals');

  // Item 4a: idle stability across several 10-s heartbeat intervals.
  final disconnectsBefore = agent.disconnects.length;
  note('item4a: idling 35 s (3.5 heartbeat intervals)...');
  await Future.delayed(const Duration(seconds: 35));
  expect(agent.network.isPeerReachable(pubkeyFromHex(bHex)), isTrue,
      reason: 'idle pair must stay connected across heartbeat intervals');
  expect(agent.disconnects.length, equals(disconnectsBefore),
      reason: 'no disconnects while idle');
  note('item4a: still reachable after 35 s idle');
  await bbPut('a_idle_ok', 'ok');

  // Item 4b: B is killed by the host; expect teardown after two silent
  // 10-s intervals (plus sweep granularity).
  await bbWait('b_killed');
  final killedAt = DateTime.now();
  note('item4b: B killed, watching for teardown...');
  await agent.waitFor(() => agent.udpDisconnectedFrom(bHex),
      'onPeerDisconnected(B, IP)', timeoutSeconds: 90);
  final teardown = DateTime.now().difference(killedAt).inSeconds;
  expect(agent.network.isPeerReachable(pubkeyFromHex(bHex)), isFalse);
  note('item4b: teardown fired ${teardown}s after kill');
  await bbPut('a_teardown_seconds', '$teardown');

  // Item 6: the stranger C. Publish where A can be dialed; C knows A
  // out-of-band, A does not know C. Observe whether C gets through.
  final myLoopback = await agent.loopbackAddress();
  await bbPut('a_addr', myLoopback);
  final cHex = await bbWait('pk_C');
  await bbWait('c_done', timeoutSeconds: 240);
  await Future.delayed(const Duration(seconds: 10)); // grace for stragglers
  final cConnected = agent.udpConnectedTo(cHex);
  final cDelivered = agent.messages.any((m) => m.$1 == cHex);
  note('item6: stranger connected=$cConnected delivered=$cDelivered');
  await bbPut('a_item6',
      jsonEncode({'connected': cConnected, 'delivered': cDelivered}));
  expect(cConnected || cDelivered, isFalse,
      reason: 'Closed trust: a third agent unknown to A must not connect');
}

Future<void> runB() async {
  final agent = await Agent.spawn('B');
  final myHex = hexOf(agent.identity.publicKey);
  // Redemption dials the rendezvous server named in the link, which needs
  // our local address candidates — wait for address discovery first.
  await agent.loopbackAddress();
  note('address discovery done');
  await bbPut('pk_B', myHex);
  final aHex = await bbWait('pk_A');

  // Item 2: redeem the peer link via the rendezvous server named in it.
  final link = await bbWait('peer_link', timeoutSeconds: 180);
  final inviterPk = await agent.network.consumePeerLink(link);
  expect(hexOf(inviterPk), equals(aHex),
      reason: 'link must name A as the inviter');
  note('item2: link redeemed; inviter is A');

  // GLP-level acceptance: supply A\'s key; configure the same rendezvous
  // server so reconnect discovery can pair with A\'s AVAILABLE.
  agent.network.putKnownPeer(inviterPk);
  final rvOk = await agent.network.addRendezvousServer(
    address: anchorAddr,
    pubkeyHex: anchorPk,
  );
  expect(rvOk, isTrue);

  // Item 3 driver: sending with no dial-book address forces the
  // rendezvous-coordinated path; the message drains once the session is up.
  final helloId = await agent.network.send(
      inviterPk, Uint8List.fromList(utf8.encode('hello-from-B')));
  note('sent hello-from-B (id=$helloId)');

  await agent.waitFor(() => agent.udpConnectedTo(aHex),
      'onPeerConnected(A, IP)', timeoutSeconds: 180);
  note('item2: session established, Transport IP');

  await agent.waitFor(
      () => agent.receivedFrom(aHex, (p) => utf8.decode(p) == 'hello-from-A'),
      'hello-from-A');
  await agent.waitFor(() => agent.receivedFrom(aHex, (p) => isBigPayload(p, 2000)),
      '2000-byte payload intact');
  note('item3: both directions verified, large payload intact');
  await bbPut('b_item3_ok', 'ok');

  // Item 4a: match A\'s idle window.
  await bbWait('a_idle_ok');
  expect(agent.network.isPeerReachable(pubkeyFromHex(aHex)), isTrue);
  note('item4a: still reachable after idle window');

  // Item 4b: hand ourselves to the executioner. Post the report FIRST —
  // the host kills this app without warning.
  await postReport(pass: true);
  await bbPut('b_ready_to_die', 'ok');
  note('awaiting termination...');
  await Future.delayed(const Duration(minutes: 10)); // killed long before
}

Future<void> runC() async {
  final agent = await Agent.spawn('C');
  final myHex = hexOf(agent.identity.publicKey);
  final aHex = await bbWait('pk_A');
  final aAddr = await bbWait('a_addr');
  await bbPut('pk_C', myHex);

  // The stranger knows A\'s key and address out-of-band — no invite, no
  // rendezvous server, unknown to A.
  final aPk = pubkeyFromHex(aHex);
  agent.network.putPeerAddress(aPk, aAddr);
  note('item6: dialing A at $aAddr without an invite...');
  final sendId = await agent.network
      .send(aPk, Uint8List.fromList(utf8.encode('stranger-hello')));
  note('item6: send returned ${sendId == null ? 'null' : 'id'} '
      '(delivery is judged on A\'s side)');
  await Future.delayed(const Duration(seconds: 20));
  final connected = agent.udpConnectedTo(aHex);
  note('item6: stranger-side connected=$connected');
  await bbPut('c_result', jsonEncode({'connected': connected}));
  await bbPut('c_done', 'ok');
}

Future<void> runA2() async {
  final agent = await Agent.spawn('A2');
  final myHex = hexOf(agent.identity.publicKey);
  await bbPut('p2_pk_A', myHex);
  final bHex = await bbWait('p2_pk_B');
  final bAddr = await bbWait('p2_b_addr');

  // Item 5: no rendezvous server is running; the dial book alone must carry
  // the connection.
  final bPk = pubkeyFromHex(bHex);
  agent.network.putPeerAddress(bPk, bAddr);
  note('item5: direct-dialing B at $bAddr (no server)...');
  final id = await agent.network
      .send(bPk, Uint8List.fromList(utf8.encode('direct-dial-hello')));
  expect(id, isNotNull);

  await agent.waitFor(() => agent.udpConnectedTo(bHex),
      'onPeerConnected(B, IP) via direct dial', timeoutSeconds: 120);
  expect(await bbWait('p2_b_ok'), equals('ok'));
  note('item5: direct dial delivered without the server');

  // Item 4 (silence variant): the host SIGSTOPs B2 — its socket stays alive
  // (no ICMP), so the peer is genuinely silent and only the heartbeat sweep
  // (two silent 10-s intervals) can tear it down.
  await bbWait('p2_b_stopped');
  final stoppedAt = DateTime.now();
  note('item4-silence: B2 frozen; watching for silence-driven teardown...');
  await agent.waitFor(() => agent.udpDisconnectedFrom(bHex),
      'onPeerDisconnected(B2, IP) by silence sweep', timeoutSeconds: 120);
  final teardown = DateTime.now().difference(stoppedAt).inSeconds;
  expect(agent.network.isPeerReachable(pubkeyFromHex(bHex)), isFalse);
  note('item4-silence: teardown fired ${teardown}s after freeze');
  await bbPut('p2_silence_teardown_seconds', '$teardown');
}

Future<void> runB2() async {
  final agent = await Agent.spawn('B2');
  final myHex = hexOf(agent.identity.publicKey);
  final aHex = await bbWait('p2_pk_A');
  // Make A2 known so its inbound contact is expected GLP-supplied traffic —
  // item 5 tests the dial book, not trust gating.
  agent.network.putKnownPeer(pubkeyFromHex(aHex));
  final myLoopback = await agent.loopbackAddress();
  await bbPut('p2_b_addr', myLoopback);
  await bbPut('p2_pk_B', myHex);

  await agent.waitFor(
      () => agent.receivedFrom(
          aHex, (p) => utf8.decode(p) == 'direct-dial-hello'),
      'direct-dial-hello', timeoutSeconds: 120);
  await agent.waitFor(() => agent.udpConnectedTo(aHex),
      'onPeerConnected(A2, IP)');
  note('item5: received direct dial + connect');
  // Report before the host freezes this process for the silence test.
  await postReport(pass: true);
  await bbPut('p2_b_ok', 'ok');
  await bbPut('p2_b_ready_to_freeze', 'ok');
  note('awaiting freeze...');
  await Future.delayed(const Duration(minutes: 10)); // frozen long before
}

/// Full-screen live journal so the agent's steps are observable on the
/// simulator screen while the protocol runs underneath.
Widget _viewer() {
  return MaterialApp(
    debugShowCheckedModeBanner: false,
    home: Scaffold(
      backgroundColor: Colors.black,
      body: SafeArea(
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Padding(
              padding: const EdgeInsets.fromLTRB(16, 12, 16, 4),
              child: Text(
                'GLP e2e — agent $role',
                style: const TextStyle(
                  color: Color(0xFF7CFC8A),
                  fontSize: 26,
                  fontWeight: FontWeight.bold,
                ),
              ),
            ),
            const Divider(color: Colors.white24, height: 8),
            Expanded(
              child: ValueListenableBuilder<List<String>>(
                valueListenable: _feed,
                builder: (context, lines, _) => ListView.builder(
                  reverse: true,
                  padding: const EdgeInsets.symmetric(
                      horizontal: 12, vertical: 8),
                  itemCount: lines.length,
                  itemBuilder: (context, i) {
                    final line = lines[lines.length - 1 - i];
                    final milestone = line.contains('item') ||
                        line.contains('onPeer') ||
                        line.contains('onMessage') ||
                        line.contains('Redeemed') ||
                        line.contains('minted');
                    return Padding(
                      padding: const EdgeInsets.symmetric(vertical: 3),
                      child: Text(
                        line,
                        style: TextStyle(
                          fontFamily: 'Menlo',
                          fontSize: 14,
                          height: 1.25,
                          color: milestone
                              ? const Color(0xFF7CFC8A)
                              : Colors.white70,
                          fontWeight: milestone
                              ? FontWeight.bold
                              : FontWeight.normal,
                        ),
                      ),
                    );
                  },
                ),
              ),
            ),
          ],
        ),
      ),
    ),
  );
}

void main() {
  final binding = IntegrationTestWidgetsFlutterBinding.ensureInitialized();
  binding.framePolicy = LiveTestWidgetsFlutterBindingFramePolicy.fullyLive;

  testWidgets('e2e agent $role', (tester) async {
    expect(role, isNotEmpty, reason: 'pass --dart-define=E2E_ROLE=...');
    expect(anchorPk, isNotEmpty,
        reason: 'pass --dart-define=E2E_ANCHOR_PK=<hex>');
    await tester.pumpWidget(_viewer());
    await tester.pump();
    note('agent $role starting');
    try {
      switch (role) {
        case 'A':
          await runA();
        case 'B':
          await runB();
        case 'C':
          await runC();
        case 'A2':
          await runA2();
        case 'B2':
          await runB2();
        default:
          fail('unknown role: $role');
      }
      await postReport(pass: true);
    } catch (e) {
      note('FAILURE: $e');
      await postReport(pass: false, failure: '$e');
      rethrow;
    }
  }, timeout: const Timeout(Duration(minutes: 25)));
}
