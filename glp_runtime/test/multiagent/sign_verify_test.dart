/// Tests for the sign/2 body kernel and the valid_attestation/4 guard.
///
/// Covers networking-seam-spec.md §4 and §7 test 6: an agent signs
/// attest(PkA, PkB); a clause guarded by valid_attestation/4 is selected on the
/// valid signature; on a tampered or malformed signature the guard fails and the
/// `otherwise` clause is selected; sign suspends until its input is ground and
/// resumes on binding; a signature produced by one agent verifies at another
/// agent.
///
/// sign/verify are real Ed25519 (ed25519_edwards) via GlpNetwork; keys and
/// signatures are lowercase-hex string constants.

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';

/// Build a madGLP engine for an agent holding [kp], with [outputLines]
/// capturing `_output`, and a SimulationNetworkClient bound as its GlpNetwork.
GlpEngine _agent(
    String id, ({PubKey pub, Uint8List priv}) kp, List<String> outputLines) {
  final engine = GlpEngine(
      rootSelfGlpPath: File('../programs/self.glp').absolute.path)
    ..strictTypes = false;
  engine.enableMadGLP(agentId: id);
  engine.runtime.outputCallback = (line) => outputLines.add(line);

  final dir = NetworkDirectory()..register(id, kp.pub);
  final client = SimulationNetworkClient(
    selfId: id,
    directory: dir,
    sendToRouter: (_, __) {},
  );
  client.putIdentity(kp.pub, kp.priv);
  engine.madContext!.network = client;
  return engine;
}

void main() {
  group('sign/2 + valid_attestation/4 guard (§4, §7.6)', () {
    test('round-trip: valid signature selects the guarded clause', () async {
      final out = <String>[];
      final a = generateKeyPair();
      final b = generateKeyPair();
      final engine = _agent('alice', a, out);
      engine.loadSource('''
-mode(system).
procedure check(_?).
check(Sig) :-
    valid_attestation('${a.pub.hex}', '${a.pub.hex}', '${b.pub.hex}', Sig?) |
    '_output'(verified).
check(_) :- otherwise | '_output'(rejected).
procedure round_trip.
round_trip :- sign(attest('${a.pub.hex}', '${b.pub.hex}'), Sig), check(Sig?).
''');
      final result = await engine.runGoal('round_trip');
      expect(result.succeeded, isTrue);
      expect(out, ['verified']);
    });

    test('sign emits a 128-char lowercase-hex signature', () async {
      final out = <String>[];
      final a = generateKeyPair();
      final b = generateKeyPair();
      final engine = _agent('alice', a, out);
      engine.loadSource('''
-mode(system).
procedure emit(_?).
emit(Sig) :- ground(Sig?) | '_output'(Sig?).
procedure do_sign.
do_sign :- sign(attest('${a.pub.hex}', '${b.pub.hex}'), Sig), emit(Sig?).
''');
      final result = await engine.runGoal('do_sign');
      expect(result.succeeded, isTrue);
      expect(out.length, 1);
      expect(out[0], matches(RegExp(r'^[0-9a-f]{128}$')));
    });

    test('cross-agent: alice signs, bob\'s guard verifies → selected', () async {
      final a = generateKeyPair();
      final b = generateKeyPair();

      // alice signs and emits the signature.
      final aliceOut = <String>[];
      final alice = _agent('alice', a, aliceOut);
      alice.loadSource('''
-mode(system).
procedure emit(_?).
emit(Sig) :- ground(Sig?) | '_output'(Sig?).
procedure do_sign.
do_sign :- sign(attest('${a.pub.hex}', '${b.pub.hex}'), Sig), emit(Sig?).
''');
      await alice.runGoal('do_sign');
      final sig = aliceOut.single;

      // bob (different key) verifies alice's signature via the guard.
      final bobOut = <String>[];
      final bob = _agent('bob', b, bobOut);
      bob.loadSource('''
-mode(system).
procedure check.
check :-
    valid_attestation('${a.pub.hex}', '${a.pub.hex}', '${b.pub.hex}', '$sig') |
    '_output'(verified).
check :- otherwise | '_output'(rejected).
''');
      final result = await bob.runGoal('check');
      expect(result.succeeded, isTrue);
      expect(bobOut, ['verified']);
    });

    test('tampered signature → guard fails, otherwise clause selected',
        () async {
      final out = <String>[];
      final a = generateKeyPair();
      final b = generateKeyPair();
      final engine = _agent('alice', a, out);
      final zeros = '0' * 128; // well-formed hex, not a valid signature
      engine.loadSource('''
-mode(system).
procedure check.
check :-
    valid_attestation('${a.pub.hex}', '${a.pub.hex}', '${b.pub.hex}', '$zeros') |
    '_output'(verified).
check :- otherwise | '_output'(rejected).
''');
      final result = await engine.runGoal('check');
      expect(result.succeeded, isTrue);
      expect(out, ['rejected']);
    });

    test('malformed hex signature → guard fails, otherwise clause selected',
        () async {
      final out = <String>[];
      final a = generateKeyPair();
      final b = generateKeyPair();
      final engine = _agent('alice', a, out);
      engine.loadSource('''
-mode(system).
procedure check.
check :-
    valid_attestation('${a.pub.hex}', '${a.pub.hex}', '${b.pub.hex}', 'not_valid_hex') |
    '_output'(verified).
check :- otherwise | '_output'(rejected).
''');
      final result = await engine.runGoal('check');
      expect(result.succeeded, isTrue);
      expect(out, ['rejected']);
    });

    test('sign suspends until its input is ground, then resumes', () async {
      final out = <String>[];
      final a = generateKeyPair();
      final engine = _agent('alice', a, out);
      // attest(A?, ...) is non-ground until A is bound by the later `=`.
      // If sign resumes on binding, the signature is produced and emitted.
      engine.loadSource('''
-mode(system).
procedure emit(_?).
emit(Sig) :- ground(Sig?) | '_output'(signed).
procedure test_suspend.
test_suspend :- sign(attest(A?, '${a.pub.hex}'), Sig), emit(Sig?), A = '${a.pub.hex}'.
''');
      final result = await engine.runGoal('test_suspend');
      expect(result.succeeded, isTrue);
      expect(out, ['signed']);
    });
  });
}
