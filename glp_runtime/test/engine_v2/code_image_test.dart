/// B2 — the loaded code image parses §cf artefact bytes for direct execution
/// without decoding bodies to Op objects. Cross-checked against `Artefact`.
library;

import 'dart:typed_data';
import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/opcodes_v2.dart' as bcv2;
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/instruction_codec.dart' show decodeCode;
import 'package:glp_runtime/engine_v2/code_image.dart';

void main() {
  // A tiny flattened program: two compiled procedures (p/2, r/1) and one
  // codeless target (helper/1, spawned but undefined → a kernel/guard bound by
  // name at load).
  final ops = <Object>[
    Label('p/2'),
    bcv2.GetVariable(0, 0, isReader: false),
    bcv2.GetVariable(1, 1, isReader: false),
    Spawn('helper/1', 1),
    Proceed(),
    Label('r/1'),
    Proceed(),
  ];
  final hM = Uint8List.fromList(List<int>.generate(32, (i) => i));
  final artefact = Artefact.fromCompiled(
    ops: ops,
    hM: hM,
    moduleName: 'demo',
    isaVersion: 'glp-isa-1',
    typeDefsText: 'Foo ::= a ; b.',
    exports: const [ArtefactExport('p', 2, 'procedure p(_?, _).')],
  );
  final bytes = artefact.toBytes();

  test('header / interface fields are preserved', () {
    final img = CodeImage.fromArtefactBytes(bytes);
    expect(img.isaVersion, 'glp-isa-1');
    expect(img.moduleName, 'demo');
    expect(img.typeDefsText, 'Foo ::= a ; b.');
    expect(img.hM, hM);
    expect(img.exports.single.name, 'p');
    expect(img.exports.single.arity, 2);
    expect(img.exportAliases, {'p/2'});
  });

  test('symbol table matches Artefact: order, names, kinds', () {
    final img = CodeImage.fromArtefactBytes(bytes);
    final art = Artefact.fromBytes(bytes);
    expect(img.symbols.length, art.symbols.length);
    for (var i = 0; i < img.symbols.length; i++) {
      expect(img.symbols[i].signature, art.symbols[i].signature);
      expect(img.symbols[i].compiled, art.symbols[i].compiled);
    }
    // Compiled in program order, then codeless: p/2, r/1, helper/1.
    expect(img.symbols.map((s) => s.signature).toList(),
        ['p/2', 'r/1', 'helper/1']);
    expect(img.symbols[2].compiled, isFalse);
  });

  test('entry offsets address the right body bytes (vs decodeCode)', () {
    final img = CodeImage.fromArtefactBytes(bytes);
    final art = Artefact.fromBytes(bytes);
    String procNameOf(int i) =>
        '${art.symbols[i].name}/${art.symbols[i].arity}';

    // p/2 starts at 0; r/1 follows; helper/1 is codeless (no offset).
    expect(img.entryOffsetOf('p/2'), 0);
    final pEnd = img.entryEndOf('p/2')!;
    expect(img.entryOffsetOf('r/1'), pEnd);
    expect(img.entryOffsetOf('helper/1'), isNull);

    // Decode each compiled symbol's byte slice and confirm it equals the body
    // the artefact decoded — i.e. the image's (offset,length) range is correct.
    for (final s in img.symbols.where((s) => s.compiled)) {
      final slice = Uint8List.sublistView(
          img.code, s.codeOffset, s.codeOffset + s.codeLength);
      final decoded = decodeCode(Uint8List.fromList(slice),
          procNameOf: procNameOf);
      final artSym =
          art.symbols.firstWhere((a) => a.signature == s.signature);
      expect(decoded.length, artSym.ops.length,
          reason: 'body length for ${s.signature}');
      expect(decoded.map((o) => o.runtimeType).toList(),
          artSym.ops.map((o) => o.runtimeType).toList(),
          reason: 'body opcodes for ${s.signature}');
    }
  });

  test('proc-index resolution round-trips signatures', () {
    final img = CodeImage.fromArtefactBytes(bytes);
    for (var i = 0; i < img.symbols.length; i++) {
      expect(img.symbolIndexOf(img.symbols[i].signature), i);
    }
    expect(img.symbolIndexOf('nonesuch/9'), isNull);
  });

  test('rejects bad magic', () {
    final bad = Uint8List.fromList(bytes);
    bad[0] = 0;
    expect(() => CodeImage.fromArtefactBytes(bad),
        throwsA(isA<Object>()));
  });
}
