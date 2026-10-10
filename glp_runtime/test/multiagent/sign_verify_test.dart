/// Tests for the signature kernels — self_key/1, sign/3, signature/2
/// (GLP-Spec appendix-guards, "Identity and signature"; IGLP code format
/// §Offer and Handshake Messages, "Signed content").
///
/// An agent signs attest(PkA, PkB) under its own key; signature/2 gives back
/// signed(K, H, T) — the signer, the source identity of the signing module and
/// the term — where the signed term verifies, and the constant unsigned
/// otherwise, a value the caller matches and not a failure: a tampered
/// signature, altered content, a hex string that is no signed term and a term
/// that is no string each answer unsigned, and the kernel aborts only on a
/// malformed call. sign signs under no key but the person's; sign suspends
/// until its input is ground and resumes on binding; a signed term produced by
/// one agent is read at another.
///
/// valid_attestation/4, a guard that held of a raw Ed25519 signature over
/// attest(PkA, PkB), is gone from the root and the runtime (GLP, 2026-09-20
/// 11:53 UTC): the catalogue's guard table does not carry it and signature/2
/// does its work, so a clause guarded by it is refused.
///
/// Keys and signed terms are lowercase-hex string constants. The runtime holds
/// the person's identity from construction; the networking layer is given the
/// same pair.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/root_scope.dart'
    show builtinProcedures;
import 'package:glp_runtime/bytecode/runner.dart' show runtimeGuards;
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/artefact.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;

String _hex(List<int> b) =>
    b.map((x) => x.toRadixString(16).padLeft(2, '0')).join();

/// A madGLP engine for agent [id] whose person is [identity], with a
/// SimulationNetworkClient bound as its GlpNetwork under the same pair, and
/// send_to_user/1 captured into [out].
GlpEngine _agent(String id, PersonIdentity identity, List<String> out) {
  final engine = GlpEngine(rootSelfGlpPath: _rootSelf, identity: identity);
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
  // Under the root, programs/ (TGLP modules.tex, "Scope construction": "A
  // program lies at or below the root"): a program in the system's temporary
  // directory, outside it, is refused since 2026-10-04.
  final dir = Directory('../programs/tests').createTempSync('glp_sign_');
  final f = File('${dir.path}/probe.glp')..writeAsStringSync(source);
  engine.loadFile(f.path);
  return dir;
}

/// A program that signs `hello` and emits the signed term, and one that takes
/// any term through signature/2 and emits the signer and the term where it is
/// a verified signed term, and `unsigned` otherwise.
const String _makeAndCheck = '''
procedure make.
make :- self_key(K), sign(hello, K?, S), send_to_user([S?]).
procedure check(_?).
check(S) :- signature(S?, Sig), report(Sig?).
procedure report(Signature?).
report(signed(K, _, T)) :- send_to_user([K?, T?]).
report(unsigned) :- send_to_user([unsigned]).
''';

/// [signed] with the hex digit at [at] flipped.
String _flip(String signed, int at) =>
    signed.replaceRange(at, at + 1, signed[at] == '0' ? '1' : '0');

void main() {
  group('sign/3, signature/2, self_key/1', () {
    test('round-trip: signature/2 gives signed(K, H, T) — the signer, the '
        'module identity and the term', () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, '''
procedure round_trip.
round_trip :- self_key(K), sign(attest(pka, pkb), K?, S), signature(S?, Sig),
    report(Sig?).
procedure report(Signature?).
report(signed(K, H, T)) :- send_to_user([K?, H?, T?]).
report(unsigned) :- send_to_user([unsigned]).
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

    test('matching signed(K, _, _) gives the signer alone, and it is '
        'self_key\'s answer', () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, '''
procedure who.
who :- self_key(K), sign(hello, K?, S), signature(S?, Sig), signer(Sig?).
procedure signer(Signature?).
signer(signed(K, _, _)) :- send_to_user([K?]).
signer(unsigned) :- send_to_user([unsigned]).
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

    test('a tampered signature answers unsigned: a value, not a failure',
        () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, _makeAndCheck);
      try {
        await engine.runGoal('make');
        final signed = out.single;
        out.clear();
        // Flip one hex digit of the signature (the bytes after the 32-byte
        // key and its 1-byte length: offset 2 + 64 hex chars).
        final tampered = _flip(signed, 2 + 64 + 4);
        final result = await engine.runGoal("check('$tampered')");
        expect(result.succeeded, isTrue);
        expect(out, ['unsigned']);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('altered content answers unsigned: the signature no longer covers it',
        () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, _makeAndCheck);
      try {
        await engine.runGoal('make');
        final signed = out.single;
        out.clear();
        // Flip the last hex digit: inside e(sig(HSrc, hello)), the signed
        // content, which still decodes as a term.
        final forged = _flip(signed, signed.length - 1);
        final result = await engine.runGoal("check('$forged')");
        expect(result.succeeded, isTrue);
        expect(out, ['unsigned']);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('a term that is no signed term answers unsigned: a hex string, a '
        'string that is not hex, a compound, a number', () async {
      final out = <String>[];
      final a = PersonIdentity.generate();
      final engine = _agent('alice', a, out);
      final dir = _load(engine, _makeAndCheck);
      try {
        for (final goal in [
          "check('00ff')",
          'check(hello)',
          'check(foo(bar))',
          'check(42)',
        ]) {
          out.clear();
          final result = await engine.runGoal(goal);
          expect(result.succeeded, isTrue, reason: goal);
          expect(out, ['unsigned'], reason: goal);
        }
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('the kernel aborts only on a malformed call: the wrong arity, or a '
        'first argument that is not known', () {
      final a = PersonIdentity.generate();
      final rt = _agent('alice', a, <String>[]).runtime;
      expect(signatureKernel(rt, []), BodyKernelResult.abort);
      expect(signatureKernel(rt, [ConstTerm('00ff')]), BodyKernelResult.abort);
      final (_, unknown) = rt.heap.allocateVariable();
      final (w1, _) = rt.heap.allocateVariable();
      expect(signatureKernel(rt, [VarRef(unknown), VarRef(w1)]),
          BodyKernelResult.abort);
      // A known argument that is no signed term is not a malformed call: the
      // kernel succeeds and assigns unsigned.
      final (w2, r2) = rt.heap.allocateVariable();
      expect(signatureKernel(rt, [ConstTerm('00ff'), VarRef(w2)]),
          BodyKernelResult.success);
      final v = rt.heap.getValue(r2);
      expect(v, isA<ConstTerm>());
      expect((v as ConstTerm).value, 'unsigned');
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
      final bobDir = _load(bob, _makeAndCheck);
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

  // valid_attestation/4 is no guard (GLP, 2026-09-20 11:53 UTC): the
  // catalogue's guard table (GLP-Spec appendix-guards.tex) does not carry it,
  // and signature/2 does its work, above --- a program takes the signed term
  // apart and refuses a forgery without failing.  Until 2026-10-10 the root
  // declared it, the checker listed it, the analyzer grounded its four inputs
  // and the runner evaluated it.
  group('valid_attestation/4 is no guard', () {
    test('the root declares it nowhere, and neither the checker nor the '
        'runtime has it', () {
      final root =
          Parser(Lexer(File(_rootSelf).readAsStringSync()).tokenize())
              .parseModule();
      expect(
          root.procDeclarations.where((d) => d.name == 'valid_attestation'),
          isEmpty);
      expect(builtinProcedures, isNot(contains('valid_attestation/4')));
      expect(runtimeGuards, isNot(contains('valid_attestation/4')));
    });

    test('a clause guarded by it is refused at load', () {
      final a = PersonIdentity.generate();
      final b = PersonIdentity.generate();
      final engine = _agent('alice', a, <String>[]);
      final sig = '0' * 128;
      final dir = Directory('../programs/tests').createTempSync('glp_sign_');
      try {
        final f = File('${dir.path}/probe.glp')
          ..writeAsStringSync('''
procedure check.
check :-
    valid_attestation('${a.pub.hex}', '${a.pub.hex}', '${b.pub.hex}', '$sig') |
    send_to_user([verified]).
check :- otherwise | send_to_user([rejected]).
''');
        expect(() => engine.loadFile(f.path),
            throwsA(predicate((e) => '$e'.contains('valid_attestation'),
                'a refusal naming valid_attestation')));
      } finally {
        dir.deleteSync(recursive: true);
      }
    });
  });
}
