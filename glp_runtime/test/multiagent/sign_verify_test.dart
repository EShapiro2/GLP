/// Tests for the signature kernels — self_key/1, sign/3, signed/4, signed/2
/// (GLP-Spec appendix-guards, "Identity and signature"; IGLP code format
/// §Offer and Handshake Messages, "Signed content") — and the
/// valid_attestation/4 guard of the networking seam.
///
/// An agent signs attest(PkA, PkB) under its own key; signed/4 gives back the
/// signer, the source identity of the signing module and the term; a tampered
/// signed term does not hold; sign signs under no key but the person's; sign
/// suspends until its input is ground and resumes on binding; a signed term
/// produced by one agent is read at another. The guard is fed a raw Ed25519
/// signature made in Dart over the canonical bytes, which is what it checks.
///
/// Keys and signed terms are lowercase-hex string constants. The runtime holds
/// the person's identity from construction; the networking layer is given the
/// same pair.
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/artefact.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;

String _hex(List<int> b) =>
    b.map((x) => x.toRadixString(16).padLeft(2, '0')).join();

/// A madGLP engine for agent [id] whose person is [identity], with a
/// SimulationNetworkClient bound as its GlpNetwork under the same pair, and
/// send_to_user/1 captured into [out].
GlpEngine _agent(String id, PersonIdentity identity, List<String> out) {
  final engine = GlpEngine(rootSelfGlpPath: _rootSelf, identity: identity)
    ..strictTypes = false;
  engine.enableMadGLP(agentId: id);
  engine.runtime.outputCallback = (line) => out.add(line);

  final dir = NetworkDirectory()..register(id, identity.pub);
  final client = SimulationNetworkClient(
    selfId: id,
    directory: dir,
    sendToRouter: (_, __) {},
  );
  client.putIdentity(identity.pub, identity.priv);
  engine.madContext!.network = client;
  return engine;
}

/// Load [source] as a program of its own, from a file: a goal then carries a
/// module value, and sign/3 has a source identity to put into the signed term.
Directory _load(GlpEngine engine, String source) {
  final dir = Directory.systemTemp.createTempSync('glp_sign_');
  final f = File('${dir.path}/probe.glp')..writeAsStringSync(source);
  engine.loadFile(f.path);
  return dir;
}

void main() {
  group('sign/3, signed/4, signed/2, self_key/1', () {
    test('round-trip: signed/4 gives the signer, the module identity and the '
        'term', () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, '''
procedure round_trip.
round_trip :- self_key(K), sign(attest(pka, pkb), K?, S),
    signed(S?, K1, H, T), send_to_user([K1?, H?, T?]).
''');
      try {
        final result = await engine.runGoal('round_trip');
        expect(result.succeeded, isTrue);
        expect(out.length, 3);
        expect(out[0], a.pub.hex);
        final hM = (engine.appModule!.artefact as Artefact).hM;
        expect(out[1], _hex(hM),
            reason: 'the module identity is the signing module\'s h(M)');
        expect(out[2], contains('attest'));
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('signed/2 gives the signer alone, and it is self_key\'s answer',
        () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, '''
procedure who.
who :- self_key(K), sign(hello, K?, S), signed(S?, K1), send_to_user([K1?]).
''');
      try {
        final result = await engine.runGoal('who');
        expect(result.succeeded, isTrue);
        expect(out, [a.pub.hex]);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('sign signs under no key but the person\'s', () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final b = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, '''
procedure under_other.
under_other :- sign(hello, '${b.pub.hex}', S), send_to_user([S?]).
''');
      try {
        final result = await engine.runGoal('under_other');
        expect(result.succeeded, isFalse);
        expect(out, isEmpty);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('a tampered signed term does not hold: signed/4 fails', () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, '''
procedure make.
make :- self_key(K), sign(hello, K?, S), send_to_user([S?]).
procedure check(_?).
check(S) :- signed(S?, K, _, _), send_to_user([K?]).
''');
      try {
        await engine.runGoal('make');
        final signed = out.single;
        out.clear();
        // Flip one hex digit of the signature (the bytes after the 32-byte
        // key and its 1-byte length: offset 2 + 64 hex chars).
        final at = 2 + 64 + 4;
        final tampered = signed.replaceRange(
            at, at + 1, signed[at] == '0' ? '1' : '0');
        final result = await engine.runGoal("check('$tampered')");
        expect(result.succeeded, isFalse);
        expect(out, isEmpty);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('cross-agent: alice signs, bob reads the signer', () async {
      final a = PersonIdentity.generate();
      final b = PersonIdentity.generate();

      final aliceOut = <String>[];
      final alice = _agent('alice', a, aliceOut);
      final aliceDir = _load(alice, '''
procedure make.
make :- self_key(K), sign(attest(pka, pkb), K?, S), send_to_user([S?]).
''');
      final bobOut = <String>[];
      final bob = _agent('bob', b, bobOut);
      final bobDir = _load(bob, '''
procedure check(_?).
check(S) :- signed(S?, K, _, T), send_to_user([K?, T?]).
''');
      try {
        await alice.runGoal('make');
        final signed = aliceOut.single;
        final result = await bob.runGoal("check('$signed')");
        expect(result.succeeded, isTrue);
        expect(bobOut.length, 2);
        expect(bobOut[0], a.pub.hex);
        expect(bobOut[1], contains('attest'));
      } finally {
        aliceDir.deleteSync(recursive: true);
        bobDir.deleteSync(recursive: true);
      }
    });

    test('sign suspends until its input is ground, then resumes', () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      // attest(A?, ...) is non-ground until A is bound by the later `=`.
      // If sign resumes on binding, the signed term is produced and emitted.
      final dir = _load(engine, '''
procedure emit(_?).
emit(S) :- ground(S?) | send_to_user([signed]).
procedure test_suspend.
test_suspend :- self_key(K), sign(attest(A?, pkb), K?, S), emit(S?), A = pka.
''');
      try {
        final result = await engine.runGoal('test_suspend');
        expect(result.succeeded, isTrue);
        expect(out, ['signed']);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });
  });

  group('valid_attestation/4 guard (networking seam)', () {
    /// A raw Ed25519 signature by [engine]'s layer over the canonical bytes of
    /// attest(PkA, PkB) — what the guard checks.
    String attestation(GlpEngine engine, String pkA, String pkB) {
      final ctx = engine.madContext!;
      final canonical = ctx.canonicalSerialize(
          StructTerm('attest', [ConstTerm(pkA), ConstTerm(pkB)]));
      return _hex(ctx.network!.sign(Uint8List.fromList(canonical)));
    }

    test('a valid signature selects the guarded clause', () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final b = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final sig = attestation(engine, a.pub.hex, b.pub.hex);
      final dir = _load(engine, '''
procedure check.
check :-
    valid_attestation('${a.pub.hex}', '${a.pub.hex}', '${b.pub.hex}', '$sig') |
    send_to_user([verified]).
check :- otherwise | send_to_user([rejected]).
''');
      try {
        final result = await engine.runGoal('check');
        expect(result.succeeded, isTrue);
        expect(out, ['verified']);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('cross-agent: alice attests, bob\'s guard verifies', () async {
      final a = PersonIdentity.generate();
      final b = PersonIdentity.generate();
      final alice = _agent('alice', a, <String>[]);
      final sig = attestation(alice, a.pub.hex, b.pub.hex);
      final bobOut = <String>[];
      final bob = _agent('bob', b, bobOut);
      final dir = _load(bob, '''
procedure check.
check :-
    valid_attestation('${a.pub.hex}', '${a.pub.hex}', '${b.pub.hex}', '$sig') |
    send_to_user([verified]).
check :- otherwise | send_to_user([rejected]).
''');
      try {
        final result = await bob.runGoal('check');
        expect(result.succeeded, isTrue);
        expect(bobOut, ['verified']);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('tampered signature → guard fails, otherwise clause selected',
        () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final b = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final zeros = '0' * 128; // well-formed hex, not a valid signature
      final dir = _load(engine, '''
procedure check.
check :-
    valid_attestation('${a.pub.hex}', '${a.pub.hex}', '${b.pub.hex}', '$zeros') |
    send_to_user([verified]).
check :- otherwise | send_to_user([rejected]).
''');
      try {
        final result = await engine.runGoal('check');
        expect(result.succeeded, isTrue);
        expect(out, ['rejected']);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('malformed hex signature → guard fails, otherwise clause selected',
        () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final b = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, '''
procedure check.
check :-
    valid_attestation('${a.pub.hex}', '${a.pub.hex}', '${b.pub.hex}', 'not_valid_hex') |
    send_to_user([verified]).
check :- otherwise | send_to_user([rejected]).
''');
      try {
        final result = await engine.runGoal('check');
        expect(result.succeeded, isTrue);
        expect(out, ['rejected']);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });
  });
}
