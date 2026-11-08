/// Part 1 of the live suite (GSA #2's task for GNA, Code_inbox.md 2026-11-08
/// 12:51 UTC): the rendezvous server at 162.35.126.126:9516, tested by two
/// agents off the VPS, each a madGLP agent over its own headless layer with a
/// fresh key pair, declaring the live server to its layer (GNA Section 6,
/// "Rendezvous Server"; Section 4.2, "Rendezvous servers").
///
/// The agents run `programs/tests/live/rv_live_client.glp`, which sends what
/// it is told and reports every reply; this program judges each reply
/// against GNA Section 6 (2ce8e10) and prints one line per check:
///
///   RESULT <id> PASS|FAIL|PENDING|INFO <what>
///
/// The model is networking/glue/test/rendezvous_server_test.dart, which runs
/// the same over the loopback with the server as a local process.
///
/// What one site can prove: the two agents run on one machine behind one
/// NAT, so the server observes both at the one public address, on two ports.
/// That the server relays the address it observes, and that each agent
/// declares it and punches, is proved; whether the punch opens a direct path
/// between two NATs is not, since a path between the two here, if one forms,
/// forms by the NAT's hairpin or not at all; it is reported as INFO.
///
/// Usage:
///   dart run bin/live_rv.dart --server-key <hex> --server-address ip:port
///       [--registration] [--glp-root <GLP>/programs]
/// --registration sends register/redeem: only where the running agent has
/// them (run_live.sh checks this on the VPS first), since a request the
/// running agent has no clause for leaves its loop waiting, and it would
/// then serve nobody.
library;

import 'dart:async';
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

import 'package:args/args.dart';
import 'package:cryptography/cryptography.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_glue/headless_glp_network.dart';

int _fails = 0;

void result(String id, String verdict, String what) {
  if (verdict == 'FAIL') _fails++;
  stdout.writeln('RESULT $id $verdict $what');
}

Future<void> main(List<String> argv) async {
  final parser = ArgParser()
    ..addOption('server-key', mandatory: true)
    ..addOption('server-address', mandatory: true)
    ..addOption('glp-root', defaultsTo: '../../programs')
    ..addFlag('registration', defaultsTo: false)
    ..addOption('wait', defaultsTo: '25', help: 'seconds per reply');
  final args = parser.parse(argv);
  final rvHex = args['server-key'] as String;
  final rvAddr = args['server-address'] as String;
  final glpRoot = Directory(args['glp-root'] as String).absolute.path;
  final wait = Duration(seconds: int.parse(args['wait'] as String));
  final rvKey = glp.PubKey.fromList(_unhex(rvHex));

  final sodium = await initHeadlessSodium();
  final rng = Random.secure();

  Future<_Agent> agent(String id) async {
    final seed = Uint8List.fromList(List.generate(32, (_) => rng.nextInt(256)));
    final identity = await GrassrootsIdentity.create(
        keyPair: await Ed25519().newKeyPairFromSeed(seed));
    final layer = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: identity,
      sodium: sodium,
    );
    layer.wan.putRendezvousServer(rvKey.bytes, rvAddr);
    final key = glp.PubKey.fromList(identity.publicKey);
    final runtime = AgentRuntime(
      agentId: id,
      program: '$glpRoot/tests/live/rv_live_client.glp',
      rootSelfGlpPath: '$glpRoot/self.glp',
      keyPair: (pub: key, priv: Uint8List.fromList(identity.privateKey)),
      network: HeadlessGlpNetwork(layer),
    );
    final a = _Agent(id, key, layer, runtime);
    runtime.onUserOutput = a.say;
    await runtime.initialize();
    return a;
  }

  final a = await agent('alice');
  final b = await agent('bob');
  stdout.writeln('INFO alice ${a.key.hex}');
  stdout.writeln('INFO bob   ${b.key.hex}');

  // 1.0  Each agent opens a session with the live server, which its layer
  // reports reachable, and learns its public address from it.
  if (!await a.layer.start() || !await b.layer.start()) {
    result('1.0-start', 'FAIL', 'a layer could not bind a UDP port');
    exit(1);
  }
  final aUp = await _until(() => a.layer.isPeerReachable(rvKey.bytes), wait);
  final bUp = await _until(() => b.layer.isPeerReachable(rvKey.bytes), wait);
  if (!aUp || !bUp) {
    result('1.0-session', 'FAIL',
        'no session with the server at $rvAddr within ${wait.inSeconds}s '
        '(alice ${aUp ? 'up' : 'down'}, bob ${bUp ? 'up' : 'down'})');
    await _down([a, b]);
    exit(1);
  }
  result('1.0-session', 'PASS',
      'both agents hold a session with the server at $rvAddr, key $rvHex');
  await _until(() => a.layer.wan.getPublicAddress() != null &&
      b.layer.wan.getPublicAddress() != null, wait);
  final pubA = a.layer.wan.getPublicAddress();
  final pubB = b.layer.wan.getPublicAddress();
  result('1.0-observed', pubA != null && pubB != null ? 'PASS' : 'FAIL',
      'the server observes alice at $pubA and bob at $pubB '
      '(GNA Section 6, "Address observation")');

  // Each signs the pair; the signatures change hands, as on befriending.
  await a.act('attest', [_c(b.key.hex)]);
  await b.act('attest', [_c(a.key.hex)]);
  final sigA = (await a.first((t) => _is(t, 'attestation'), wait))?.args[1];
  final sigB = (await b.first((t) => _is(t, 'attestation'), wait))?.args[1];
  if (sigA == null || sigB == null) {
    result('1.1-attest', 'FAIL', 'the agents did not sign their pair');
    await _down([a, b]);
    exit(1);
  }

  // 1.1  Reconnection (GRV:4) at the protocol level: alice asks for bob with
  // bob's signature, bob is available to alice with alice's; the server
  // verifies each, relays each the key and the address of the other, each
  // declares it and punches.  What triggers the request, the layer's
  // reachable stream (connected(K), lost(K)), is not yet in the seam.
  var tag = 0;
  final tRec = ++tag, tAv = ++tag;
  await a.act('reconnect', [_i(tRec), _c(rvHex), _c(b.key.hex), sigB]);
  await b.act('available', [_i(tAv), _c(rvHex), _c(a.key.hex), sigA]);
  final rA = await a.reply(tRec, wait);
  final rB = await b.reply(tAv, wait);
  _judgePunch('1.1-reconnect', 'reconnection', rA, rB, a, b, pubA, pubB);
  result('1.1-trigger', 'PENDING',
      'reconnection on a lost path waits on the seam stream reachable(Events), '
      'connected(K) and lost(K) over the WAN (GNA 2ce8e10, asked of GLP, '
      'Blocking: yes); tested here as the request and the relay alone');
  if (rA != null && rB != null && _is(rA, 'punch') && _is(rB, 'punch')) {
    final direct = await _until(
        () => a.layer.isPeerReachable(b.key.bytes) &&
            b.layer.isPeerReachable(a.key.bytes),
        const Duration(seconds: 20));
    result('1.1-path', 'INFO',
        direct
            ? 'alice and bob reach each other after the punch; both are behind '
                'this one NAT, so the path is the hairpin or the host, and it '
                'proves nothing about two NATs'
            : 'alice and bob do not reach each other within 20s after the '
                'punch; both are behind this one NAT, whose hairpin is not '
                'given, so this is no finding about the server');
  }

  // 1.2  A request carrying the requester's own signature is refused.
  final tBad = ++tag;
  await b.act('reconnect', [_i(tBad), _c(rvHex), _c(a.key.hex), sigB]);
  final rBad = await b.reply(tBad, wait);
  result('1.2-reconnect-own-signature',
      rBad != null && _is(rBad, 'rejected') ? 'PASS' : 'FAIL',
      'a reconnection carrying the requester\'s own signature: ${_show(rBad)}');

  if (!(args['registration'] as bool)) {
    for (final id in [
      '1.3-register-redeem',
      '1.4-refused-used',
      '1.5-refused-expired',
      '1.6-refused-unregistered',
      '1.7-refused-wrongly-signed',
      '1.8-used-serial-dropped',
    ]) {
      result(id, 'PENDING',
          'not sent: the agent running on the VPS has no register/redeem '
          '(see run_live.sh, the deployed-agent check)');
    }
    await _down([a, b]);
    exit(_fails == 0 ? 0 : 1);
  }

  final serialBase = 1000 + rng.nextInt(1 << 20);
  const hour = 3600000;

  Future<rt.Term?> link(_Agent who, int serial, int offset,
      {_Agent? forgingFor}) async {
    final t = ++tag;
    if (forgingFor == null) {
      await who.act('make_link',
          [_i(t), _c(rvHex), _c(rvAddr), _i(serial), _i(offset)]);
    } else {
      await who.act('forge_link', [
        _i(t), _c(forgingFor.key.hex), _c(rvHex), _c(rvAddr), _i(serial),
        _i(offset)
      ]);
    }
    return (await who.first(
            (x) => _is(x, 'link') && _int((x as rt.StructTerm).args[0]) == t, wait))
        ?.args[1];
  }

  Future<int> register(rt.Term l) async {
    final t = ++tag;
    await a.act('register', [_i(t), _c(rvHex), l]);
    return t;
  }

  Future<rt.Term?> redeem(rt.Term l, {bool asOther = false}) async {
    final t = ++tag;
    if (asOther) {
      // Signed by bob, naming alice as the recipient.
      await b.act('redeem_as', [_i(t), _c(rvHex), _c(a.key.hex), l]);
    } else {
      await b.act('redeem', [_i(t), _c(rvHex), _c(a.key.hex), l]);
    }
    return b.reply(t, wait);
  }

  // A registration is answered only at its redemption, or refused at once:
  // admitted is no refusal within a few seconds.
  Future<bool> admitted(int t) async =>
      await a.reply(t, const Duration(seconds: 4)) == null;

  // 1.3  Registration and redemption (GRV:2, GRV:3).
  final s1 = serialBase;
  final l1 = await link(a, s1, hour);
  final tReg1 = await register(l1!);
  final adm1 = await admitted(tReg1);
  result('1.3-register', adm1 ? 'PASS' : 'FAIL',
      'alice registers serial $s1: ${adm1 ? 'admitted' : _show(a.replyNow(tReg1))}');
  final rRed1 = await redeem(l1);
  final rReg1 = await a.reply(tReg1, wait);
  _judgePunch('1.3-redeem', 'redemption', rReg1, rRed1, a, b, pubA, pubB);

  // 1.4  The link redeemed again, and its serial registered again: refused.
  final rRed1b = await redeem(l1);
  result('1.4-refused-used-redeem',
      rRed1b != null && _is(rRed1b, 'rejected') ? 'PASS' : 'FAIL',
      'a used link redeemed again: ${_show(rRed1b)}');
  final tReg1b = await register(l1);
  final rReg1b = await a.reply(tReg1b, const Duration(seconds: 6));
  result('1.4-refused-used-register',
      rReg1b != null && _is(rReg1b, 'rejected') ? 'PASS' : 'FAIL',
      'a used serial registered again before its expiry: ${_show(rReg1b)}');

  // 1.5  An expired link, registered: its redemption refused.
  final l2 = await link(a, serialBase + 1, -1000);
  await register(l2!);
  final rRed2 = await redeem(l2);
  result('1.5-refused-expired',
      rRed2 != null && _is(rRed2, 'rejected') ? 'PASS' : 'FAIL',
      'an expired link redeemed: ${_show(rRed2)}');

  // 1.6  A link never registered: refused.
  final l3 = await link(a, serialBase + 2, hour);
  final rRed3 = await redeem(l3!);
  result('1.6-refused-unregistered',
      rRed3 != null && _is(rRed3, 'rejected') ? 'PASS' : 'FAIL',
      'an unregistered link redeemed: ${_show(rRed3)}');

  // 1.7  Wrongly signed: a link naming alice as inviter signed by bob; and a
  // redemption naming alice as recipient signed by bob.  Then the serial,
  // registered and untouched by the refusals, redeems.
  final s4 = serialBase + 3;
  final l4 = await link(a, s4, hour);
  final tReg4 = await register(l4!);
  final forged = await link(b, s4, hour, forgingFor: a);
  final rForged = await redeem(forged!);
  result('1.7-refused-forged-link',
      rForged != null && _is(rForged, 'rejected') ? 'PASS' : 'FAIL',
      'a link naming alice, signed by bob: ${_show(rForged)}');
  final rAsOther = await redeem(l4, asOther: true);
  result('1.7-refused-wrong-redeemer',
      rAsOther != null && _is(rAsOther, 'rejected') ? 'PASS' : 'FAIL',
      'a redemption naming alice as its recipient, signed by bob: '
      '${_show(rAsOther)}');
  final rRed4 = await redeem(l4);
  final rReg4 = await a.reply(tReg4, wait);
  result('1.7-refusals-change-nothing',
      rRed4 != null && _is(rRed4, 'punch') && rReg4 != null && _is(rReg4, 'punch')
          ? 'PASS'
          : 'FAIL',
      'after the refusals, serial $s4 redeems: ${_show(rRed4)} / ${_show(rReg4)}');

  // 1.8  A used serial is dropped at its link's expiry: a link expiring in
  // 8 s is redeemed; its serial is refused again before the expiry, and
  // admitted again after it, and the new link redeems.
  final s5 = serialBase + 4;
  final l5 = await link(a, s5, 8000);
  final made5 = DateTime.now();
  final tReg5 = await register(l5!);
  final rRed5 = await redeem(l5);
  await a.reply(tReg5, wait);
  final l5b = await link(a, s5, hour);
  final tReg5b = await register(l5b!);
  final before = await a.reply(tReg5b, const Duration(seconds: 3));
  final early = DateTime.now().difference(made5) < const Duration(seconds: 8);
  result('1.8-used-kept-until-expiry',
      rRed5 != null && _is(rRed5, 'punch') && before != null &&
              _is(before, 'rejected') && early
          ? 'PASS'
          : 'FAIL',
      'serial $s5 used, re-registered before its expiry: ${_show(before)}'
      '${early ? '' : ' (the expiry passed before the check; inconclusive)'}');
  final left = const Duration(seconds: 9) - DateTime.now().difference(made5);
  if (left > Duration.zero) await Future.delayed(left);
  final tReg5c = await register(l5b);
  final adm5c = await admitted(tReg5c);
  final rRed5c = adm5c ? await redeem(l5b) : null;
  result('1.8-used-serial-dropped',
      adm5c && rRed5c != null && _is(rRed5c, 'punch') ? 'PASS' : 'FAIL',
      'serial $s5 after its link\'s expiry: registration '
      '${adm5c ? 'admitted' : _show(a.replyNow(tReg5c))}, redemption ${_show(rRed5c)}');

  await _down([a, b]);
  exit(_fails == 0 ? 0 : 1);
}

/// Judges a relay against GNA Section 6 (2ce8e10): each of the two told
/// punch(Key, Address), the other's key and the address the server observes
/// for it.  [rX] is [x]'s reply, [rY] [y]'s.
void _judgePunch(String id, String what, rt.Term? rX, rt.Term? rY, _Agent x,
    _Agent y, String? pubX, String? pubY) {
  if (rX == null || rY == null || !_is(rX, 'punch') || !_is(rY, 'punch')) {
    result('$id-relay', 'FAIL',
        'the $what was not relayed to both: ${x.id} ${_show(rX)}, '
        '${y.id} ${_show(rY)}');
    return;
  }
  final ax = _str((rX as rt.StructTerm).args.last);
  final ay = _str((rY as rt.StructTerm).args.last);
  result('$id-address', ax == pubY && ay == pubX ? 'PASS' : 'FAIL',
      '${x.id} told $ax (bob observed at $pubY), ${y.id} told $ay '
      '(observed at $pubX); each declared it and punched');
  final keyed = rX.args.length == 2 && rY.args.length == 2;
  result(
      '$id-key',
      keyed &&
              _str(rX.args.first) == y.key.hex &&
              _str(rY.args.first) == x.key.hex
          ? 'PASS'
          : 'FAIL',
      keyed
          ? 'each told the other\'s key'
          : 'the relay is ${_show(rX)}: the address alone, where GNA Section 6 '
              '(2ce8e10) answers punch(Key, Address); rv_agent.glp on main '
              'answers punch(Address) still (Cloud #4\'s point 1, '
              '2026-11-08 10:52 UTC, approved in GNA 2ce8e10)');
}

class _Agent {
  _Agent(this.id, this.key, this.layer, this.runtime);
  final String id;
  final glp.PubKey key;
  final HeadlessGrassrootsNetwork layer;
  final AgentRuntime runtime;
  final List<rt.Term> said = [];
  final _said = StreamController<rt.Term>.broadcast();

  void say(rt.Term t) {
    said.add(t);
    _said.add(t);
  }

  Future<void> act(String f, List<rt.Term> args) =>
      runtime.injectUserInput(rt.StructTerm(f, args));

  Future<rt.StructTerm?> first(bool Function(rt.Term) p, Duration d) async {
    for (final t in said) {
      if (p(t)) return t as rt.StructTerm;
    }
    try {
      return await _said.stream.firstWhere(p).timeout(d) as rt.StructTerm;
    } on TimeoutException {
      return null;
    }
  }

  rt.Term? replyNow(int tag) {
    for (final t in said) {
      if (_is(t, 'reply') && _int((t as rt.StructTerm).args[0]) == tag) {
        return t.args[1];
      }
    }
    return null;
  }

  Future<rt.Term?> reply(int tag, Duration d) async =>
      (await first((t) => _is(t, 'reply') && _int((t as rt.StructTerm).args[0]) == tag, d))
          ?.args[1];
}

Future<bool> _until(bool Function() p, Duration d) async {
  final end = DateTime.now().add(d);
  while (!p()) {
    if (DateTime.now().isAfter(end)) return false;
    await Future.delayed(const Duration(milliseconds: 250));
  }
  return true;
}

Future<void> _down(List<_Agent> as) async {
  for (final a in as) {
    a.runtime.dispose();
    await a.layer.dispose();
  }
}

bool _is(rt.Term? t, String f) =>
    (t is rt.StructTerm && t.functor == f) ||
    (t is rt.ConstTerm && t.value == f);
String _str(rt.Term t) => (t as rt.ConstTerm).value.toString();
int _int(rt.Term t) => (t as rt.ConstTerm).value as int;
rt.Term _c(String s) => rt.ConstTerm(s);
rt.Term _i(int n) => rt.ConstTerm(n);

String _show(rt.Term? t) {
  if (t == null) return 'no reply';
  if (t is rt.ConstTerm) return '${t.value}';
  if (t is rt.StructTerm) return '${t.functor}(${t.args.map(_show).join(', ')})';
  return t.toString();
}

Uint8List _unhex(String h) => Uint8List.fromList(
    [for (var i = 0; i < h.length; i += 2) int.parse(h.substring(i, i + 2), radix: 16)]);
