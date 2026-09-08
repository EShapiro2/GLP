/// Module artefact + loader (code format §Program Artefact, §Loader), at the
/// byte-format level.
///
/// Body layout (header/interface/symbol-table/code) followed by the
/// certificate; whole-artefact byte round-trip; the compiled identity as
/// SHA-256 of the body; the loader's verification of the certificate — body
/// hash, signature, source identity against the offer — its export aliasing,
/// and its dedup by compiled identity. The compile→write→read→run-identical
/// integration over real projects is in artefact_roundtrip_test.dart.
library;

import 'dart:typed_data';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart'
    show interfaceTypeIdentityTables;
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:test/test.dart';

Uint8List _hm() => Uint8List.fromList(List<int>.generate(32, (i) => i));

/// The compiling person of these tests.
final PersonIdentity _compiler = PersonIdentity.generate();

Artefact _sample({PersonIdentity? signer}) => Artefact(
      isaVersion: '2.16.3',
      hM: _hm(),
      moduleName: 'demo',
      typeDefsText: 'Stream(X) ::= [] ; [X | Stream(X)].',
      exports: const [
        ArtefactExport('foo', 1, 'exported procedure foo(Integer?).'),
      ],
      symbols: [
        // compiled foo/1 — guards on a builtin, spawns bar/1 and a kernel.
        ArtefactSymbol.compiled('foo', 1, <Object>[
          ClauseTry(),
          Guard('number', 1),
          Commit(),
          Spawn('bar/1', 1),
          Spawn('_add/3', 3),
          Proceed(),
        ]),
        // compiled bar/1
        ArtefactSymbol.compiled('bar', 1, <Object>[
          ClauseTry(),
          Commit(),
          Proceed(),
        ]),
        // codeless: a builtin guard and a body kernel, bound by name at load.
        ArtefactSymbol.codeless('number', 1),
        ArtefactSymbol.codeless('_add', 3),
      ],
      signer: signer ?? _compiler,
    );

void main() {
  group('artefact byte format', () {
    test('header begins with magic GLPW and code-format version 2', () {
      final b = _sample().toBytes();
      expect(b.sublist(0, 4), artefactMagic); // 'GLPW'
      expect(b[4], wireFormatVersion); // 2 (message kind byte, certificate)
      expect(wireFormatVersion, 2);
    });

    test('neither identity is in the header: the body is followed by the '
        'certificate, which carries both', () {
      final a = _sample();
      final body = a.bodyBytes();
      final whole = a.toBytes();
      expect(whole.sublist(0, body.length), body);
      expect(Artefact.bodyLength(whole), body.length);
      final cert = a.certificate;
      expect(cert.hSrc, _hm());
      expect(cert.hBin, Artefact.compiledIdentityOfBody(body));
      expect(cert.agent, _compiler.pub.bytes);
      expect(cert.isRefused, isFalse);
    });

    test('whole-artefact byte round-trip is stable', () {
      final a = _sample();
      final b1 = a.toBytes();
      final a2 = Artefact.fromBytes(b1);
      final b2 = a2.toBytes();
      expect(b2, b1);
    });

    test('parsed fields, symbol kinds and the certificate survive the '
        'round-trip', () {
      final a2 = Artefact.fromBytes(_sample().toBytes());
      expect(a2.isaVersion, '2.16.3');
      expect(a2.moduleName, 'demo');
      expect(a2.hM, _hm());
      expect(a2.certificate.agent, _compiler.pub.bytes);
      expect(a2.certificate.verifies(), isTrue);
      expect(a2.typeDefsText, contains('Stream(X)'));
      expect(a2.exports.single,
          const ArtefactExport('foo', 1, 'exported procedure foo(Integer?).'));
      expect(a2.symbols.map((s) => s.signature),
          ['foo/1', 'bar/1', 'number/1', '_add/3']);
      expect(a2.symbols.map((s) => s.compiled), [true, true, false, false]);
      // Compiled bodies decoded back.
      expect(a2.symbols[0].ops.length, 6);
      expect(a2.symbols[2].ops, isEmpty); // codeless
    });

    test('bad magic is rejected', () {
      final b = _sample().toBytes();
      b[0] = 0x00;
      expect(() => Artefact.fromBytes(b), throwsA(isA<WireFormatException>()));
    });
  });

  group('the two identities and the certificate', () {
    test('the compiled identity is SHA-256 of the body, deterministic and '
        '32 bytes', () {
      final b = _sample().toBytes();
      final id1 = Artefact.compiledIdentityOf(b);
      final id2 = Artefact.compiledIdentityOf(b);
      expect(id1.length, 32);
      expect(id1, id2);
      expect(id1, Artefact.fromBytes(b).compiledIdentity);
    });

    test('the certificate signs e(ids(HSrc, HBin)) under the compiler\'s key',
        () {
      final cert = _sample().certificate;
      final content = Certificate.signedContent(cert.hSrc, cert.hBin);
      expect(PersonIdentity.verify(_compiler.pub, content, cert.signature),
          isTrue);
      // The functor ids is fixed by the code format: term tag 3, "ids", arity 2.
      expect(content.sublist(0, 6), [3, 3, 0x69, 0x64, 0x73, 2]);
    });

    test('two compilers compiling one source produce two certificates over '
        'one pair of identities', () {
      final other = PersonIdentity.generate();
      final c1 = _sample().certificate;
      final c2 = _sample(signer: other).certificate;
      expect(c1.hSrc, c2.hSrc);
      expect(c1.hBin, c2.hBin);
      expect(c1.agent, isNot(c2.agent));
      expect(c1.verifies(), isTrue);
      expect(c2.verifies(), isTrue);
    });

    test('a module no one certifies carries its identities under no '
        'signature', () {
      final a = Artefact(
        isaVersion: '2.16.3',
        hM: _hm(),
        moduleName: 'demo',
        typeDefsText: '',
        exports: const [],
        symbols: const [],
      );
      final cert = Artefact.fromBytes(a.toBytes()).certificate;
      expect(cert.isRefused, isTrue);
      expect(cert.hSrc, _hm());
      expect(cert.hBin, Artefact.compiledIdentityOfBody(a.bodyBytes()));
      expect(cert.verifies(), isFalse);
    });
  });

  group('toProgram', () {
    test('compiled symbols become entry labels; codeless do not', () {
      final prog = _sample().toProgram();
      expect(prog.labels.containsKey('foo/1'), isTrue);
      expect(prog.labels.containsKey('bar/1'), isTrue);
      // codeless kernel/guard names are runtime-resolved, not program labels.
      expect(prog.labels.containsKey('number/1'), isFalse);
      expect(prog.labels.containsKey('_add/3'), isFalse);
    });
  });

  group('loader (§Loader)', () {
    test('loads with a verifying certificate and the offered h(M); aliases '
        'exports', () {
      final bytes = _sample().toBytes();
      final m = ArtefactLoader().load(bytes, offeredHM: _hm());
      expect(m.exportAliases, {'foo/1'});
      expect(m.hM, _hm());
      expect(m.compiledIdentity, Artefact.compiledIdentityOf(bytes));
      expect(m.program.labels.containsKey('foo/1'), isTrue);
    });

    test('rejects a body that does not hash to the certificate\'s compiled '
        'identity', () {
      final bytes = _sample().toBytes();
      // Flip a byte of the module name inside the body.
      final tampered = Uint8List.fromList(bytes);
      final at = Artefact.bodyLength(bytes) - 1;
      tampered[at] ^= 0x01;
      expect(() => ArtefactLoader().load(tampered, offeredHM: _hm()),
          throwsA(isA<WireFormatException>()));
    });

    test('rejects a certificate whose signature does not verify', () {
      final a = _sample();
      final forged = Certificate(
        agent: a.certificate.agent,
        hSrc: a.certificate.hSrc,
        hBin: a.certificate.hBin,
        signature: Uint8List(64),
      );
      final w = WireWriter();
      for (final b in a.bodyBytes()) {
        w.u8(b);
      }
      forged.write(w);
      expect(() => ArtefactLoader().load(w.toBytes(), offeredHM: _hm()),
          throwsA(isA<WireFormatException>()));
    });

    test('rejects a module that carries no certificate', () {
      final a = Artefact(
        isaVersion: '2.16.3',
        hM: _hm(),
        moduleName: 'demo',
        typeDefsText: '',
        exports: const [],
        symbols: const [],
      );
      expect(() => ArtefactLoader().load(a.toBytes(), offeredHM: _hm()),
          throwsA(isA<WireFormatException>()));
    });

    test('rejects an h(M) that does not match the offer', () {
      final bytes = _sample().toBytes();
      final wrongHM = Uint8List(32);
      expect(() => ArtefactLoader().load(bytes, offeredHM: wrongHM),
          throwsA(isA<WireFormatException>()));
    });

    test('refuses an unsupported ISA version', () {
      final bytes = _sample().toBytes();
      expect(
          () => ArtefactLoader().load(bytes,
              offeredHM: _hm(), supportedIsaVersions: {'9.9.9'}),
          throwsA(isA<WireFormatException>()));
    });

    test('caches/dedups by compiled identity', () {
      final bytes = _sample().toBytes();
      final loader = ArtefactLoader();
      final m1 = loader.load(bytes, offeredHM: _hm());
      final m2 = loader.load(bytes, offeredHM: _hm());
      expect(identical(m1, m2), isTrue);
    });

    // Step 2: the exported type-identity table is derived from the interface
    // text the artefact carries, not shipped beside it.
    test('derives the exported type-identity table from the interface text',
        () {
      final bytes = _sample().toBytes();
      final m = ArtefactLoader().load(bytes, offeredHM: _hm());
      expect(m.exportedTypes.exported.keys, {'foo/1'});
      expect(m.exportedTypes.exported['foo/1'], isNotEmpty);
      // An interface section says nothing about the rest of the module's scope.
      expect(m.exportedTypes.declared, isEmpty);
      expect(m.exportedTypes.unresolved, isEmpty);
    });

    test('the derived table equals the one derived from the text directly', () {
      final art = _sample();
      final bytes = art.toBytes();
      final m = ArtefactLoader().load(bytes, offeredHM: _hm());
      final direct = interfaceTypeIdentityTables(
        typeDefsText: art.typeDefsText,
        exportDeclarationTexts: art.exports.map((e) => e.declarationText),
      );
      expect(m.exportedTypes.exported, direct.exported);
    });

    test('interface text that does not parse is a failsafe refusal', () {
      final broken = Artefact(
        isaVersion: '2.16.3',
        hM: _hm(),
        moduleName: 'demo',
        typeDefsText: 'Stream(X) ::= [] ; [X | Stream(X)].',
        exports: const [ArtefactExport('foo', 1, 'foo(X, Y?) :- true | true.')],
        symbols: [
          ArtefactSymbol.compiled('foo', 1, <Object>[ClauseTry(), Commit(), Proceed()]),
        ],
        signer: _compiler,
      );
      expect(() => ArtefactLoader().load(broken.toBytes(), offeredHM: _hm()),
          throwsA(isA<WireFormatException>()));
    });
  });
}
