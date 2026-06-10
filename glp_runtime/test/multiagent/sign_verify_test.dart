/// Tests for the sign/2 and verify_attestation/4 system-predicate kernels.
///
/// Covers networking-seam-spec.md v0.4 §4 and §7 test 6: an agent signs
/// attest(PkA, PkB); verify_attestation binds true; a tampered/invalid
/// signature binds false; sign suspends until its input is ground and resumes
/// on binding; a signature produced by one agent verifies at another agent.
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
  group('sign/2 + verify_attestation/4 (§4, §7.6)', () {
    test('round-trip: sign attest(PkA,PkB) then verify → true', () async {
      final out = <String>[];
      final a = generateKeyPair();
      final b = generateKeyPair();
      final engine = _agent('alice', a, out);
      engine.loadSource('''
-mode(system).
procedure ev(_?).
ev(Ok) :- ground(Ok?) | '_output'(verified(Ok?)).
procedure verify_step(_?).
verify_step(Sig) :- ground(Sig?) |
    verify_attestation('${a.pub.hex}', '${b.pub.hex}', Sig?, Ok),
    ev(Ok?).
procedure round_trip.
round_trip :- sign(attest('${a.pub.hex}', '${b.pub.hex}'), Sig), verify_step(Sig?).
''');
      final result = await engine.runGoal('round_trip');
      expect(result.succeeded, isTrue);
      expect(out, ['verified(true)']);
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

    test('cross-agent: alice signs, bob verifies in a separate run → true',
        () async {
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

      // bob (different key) verifies alice's signature.
      final bobOut = <String>[];
      final bob = _agent('bob', b, bobOut);
      bob.loadSource('''
-mode(system).
procedure ev(_?).
ev(Ok) :- ground(Ok?) | '_output'(verified(Ok?)).
procedure do_verify.
do_verify :- verify_attestation('${a.pub.hex}', '${b.pub.hex}', '$sig', Ok), ev(Ok?).
''');
      final result = await bob.runGoal('do_verify');
      expect(result.succeeded, isTrue);
      expect(bobOut, ['verified(true)']);
    });

    test('tampered/invalid signature → false (never goal failure)', () async {
      final out = <String>[];
      final a = generateKeyPair();
      final b = generateKeyPair();
      final engine = _agent('alice', a, out);
      final zeros = '0' * 128; // well-formed hex, not a valid signature
      engine.loadSource('''
-mode(system).
procedure ev(_?).
ev(Ok) :- ground(Ok?) | '_output'(verified(Ok?)).
procedure do_verify.
do_verify :- verify_attestation('${a.pub.hex}', '${b.pub.hex}', '$zeros', Ok), ev(Ok?).
''');
      final result = await engine.runGoal('do_verify');
      expect(result.succeeded, isTrue); // data, not failure
      expect(out, ['verified(false)']);
    });

    test('malformed hex signature → false', () async {
      final out = <String>[];
      final a = generateKeyPair();
      final b = generateKeyPair();
      final engine = _agent('alice', a, out);
      engine.loadSource('''
-mode(system).
procedure ev(_?).
ev(Ok) :- ground(Ok?) | '_output'(verified(Ok?)).
procedure do_verify.
do_verify :- verify_attestation('${a.pub.hex}', '${b.pub.hex}', 'not_valid_hex', Ok), ev(Ok?).
''');
      final result = await engine.runGoal('do_verify');
      expect(result.succeeded, isTrue);
      expect(out, ['verified(false)']);
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
