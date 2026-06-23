/// S4 — module artefact + loader (§§5, 7), byte-format level.
///
/// Header/interface/symbol-table/code-section layout; whole-artefact byte
/// round-trip; SHA-256 artefact identity; loader verification, export aliasing,
/// and dedup by identity. The compile→write→read→run-identical integration over
/// real projects is in artefact_roundtrip_test.dart.
library;

import 'dart:typed_data';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:test/test.dart';

Uint8List _hm() => Uint8List.fromList(List<int>.generate(32, (i) => i));

Artefact _sample() => Artefact(
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
    );

void main() {
  group('artefact byte format', () {
    test('header begins with magic GLPW and wire version 1', () {
      final b = _sample().toBytes();
      expect(b.sublist(0, 4), artefactMagic); // 'GLPW'
      expect(b[4], wireFormatVersion); // 1
    });

    test('whole-artefact byte round-trip is stable', () {
      final a = _sample();
      final b1 = a.toBytes();
      final a2 = Artefact.fromBytes(b1);
      final b2 = a2.toBytes();
      expect(b2, b1);
    });

    test('parsed fields and symbol kinds survive the round-trip', () {
      final a2 = Artefact.fromBytes(_sample().toBytes());
      expect(a2.isaVersion, '2.16.3');
      expect(a2.moduleName, 'demo');
      expect(a2.hM, _hm());
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

  group('artefact identity', () {
    test('SHA-256 of the bytes, deterministic and 32 bytes', () {
      final b = _sample().toBytes();
      final id1 = Artefact.identityOf(b);
      final id2 = Artefact.identityOf(b);
      expect(id1.length, 32);
      expect(id1, id2);
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

  group('loader (§7)', () {
    test('loads with matching identity and h(M); aliases exports', () {
      final bytes = _sample().toBytes();
      final id = Artefact.identityOf(bytes);
      final m = ArtefactLoader().load(bytes,
          offeredHM: _hm(), certifiedArtefactId: id);
      expect(m.exportAliases, {'foo/1'});
      expect(m.hM, _hm());
      expect(m.artefactId, id);
      expect(m.program.labels.containsKey('foo/1'), isTrue);
    });

    test('rejects a wrong certified artefact identity', () {
      final bytes = _sample().toBytes();
      final wrong = Uint8List(32); // zeros
      expect(
          () => ArtefactLoader()
              .load(bytes, offeredHM: _hm(), certifiedArtefactId: wrong),
          throwsA(isA<WireFormatException>()));
    });

    test('rejects an h(M) that does not match the offer', () {
      final bytes = _sample().toBytes();
      final id = Artefact.identityOf(bytes);
      final wrongHM = Uint8List(32);
      expect(
          () => ArtefactLoader()
              .load(bytes, offeredHM: wrongHM, certifiedArtefactId: id),
          throwsA(isA<WireFormatException>()));
    });

    test('refuses an unsupported ISA version', () {
      final bytes = _sample().toBytes();
      final id = Artefact.identityOf(bytes);
      expect(
          () => ArtefactLoader().load(bytes,
              offeredHM: _hm(),
              certifiedArtefactId: id,
              supportedIsaVersions: {'9.9.9'}),
          throwsA(isA<WireFormatException>()));
    });

    test('caches/dedups by artefact identity', () {
      final bytes = _sample().toBytes();
      final id = Artefact.identityOf(bytes);
      final loader = ArtefactLoader();
      final m1 =
          loader.load(bytes, offeredHM: _hm(), certifiedArtefactId: id);
      final m2 =
          loader.load(bytes, offeredHM: _hm(), certifiedArtefactId: id);
      expect(identical(m1, m2), isTrue);
    });
  });
}
