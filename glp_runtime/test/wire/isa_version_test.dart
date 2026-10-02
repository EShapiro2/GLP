/// The instruction-set version of the code format.
///
/// Specification: IGLP, Code Format appendix at 8c1d5e2
/// (sections/code-format-fragment.tex), "Format Versioning": the header
/// carries the instruction-set version, a string, and "A loader refuses an
/// artefact whose code-format version or instruction-set version it does not
/// support"; "an older runtime rejects a newer instruction-set version at
/// adoption".  The opcode table has no 0x54: `spawn_rated`, the instruction of
/// sGLP's engine extension, left the code format with the extension at
/// 8c1d5e2, and with it `glp-isa-2`, the version that carried it.  This
/// runtime writes and loads `glp-isa-1`, the version before the extension, and
/// refuses an artefact at `glp-isa-2`.
library;

import 'dart:typed_data';

import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine_v2/code_image.dart';
import 'package:glp_runtime/engine_v2/interp.dart' show codeImageFromProgram;
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/instruction_codec.dart';
import 'package:test/test.dart';

Uint8List _hm() => Uint8List.fromList(List<int>.generate(32, (i) => 7 * i));

final PersonIdentity _compiler = PersonIdentity.generate();

const String _source = '''
procedure p(Integer?).
p(X) :- q(X?).
procedure q(Integer?).
q(_).
''';

/// The certified artefact of [_source] at instruction-set version [isa].
Uint8List _artefact(String isa) => Artefact.fromCompiled(
      ops: GlpCompiler().compile(_source).ops.cast<Object>(),
      hM: _hm(),
      moduleName: 'p',
      isaVersion: isa,
      signer: _compiler,
    ).toBytes();

Matcher _refused(String isa) => throwsA(isA<WireFormatException>()
    .having((e) => e.message, 'message', 'unsupported ISA version: $isa'));

void main() {
  test('this implementation writes glp-isa-1 and loads glp-isa-1 alone', () {
    expect(glpIsaVersion, 'glp-isa-1');
    expect(runtimeIsaVersions, {'glp-isa-1'});
    final bytes = _artefact(glpIsaVersion);
    expect(Artefact.fromBytes(bytes).isaVersion, 'glp-isa-1');
    expect(ArtefactLoader().load(bytes, offeredHM: _hm()).artefact.isaVersion,
        'glp-isa-1');
    expect(CodeImage.fromArtefactBytes(bytes).isaVersion, 'glp-isa-1');
    // The engine's own image of a compiled program is at the version too.
    expect(codeImageFromProgram(GlpCompiler().compile(_source)).isaVersion,
        'glp-isa-1');
  });

  group('an artefact at glp-isa-2 is refused', () {
    test('by the loader, at adoption', () {
      expect(() => ArtefactLoader().load(_artefact('glp-isa-2'),
          offeredHM: _hm()), _refused('glp-isa-2'));
    });

    test('by the code image a runtime runs', () {
      expect(() => CodeImage.fromArtefactBytes(_artefact('glp-isa-2')),
          _refused('glp-isa-2'));
    });

    test('whatever the loader has loaded before', () {
      // A loader told it supports glp-isa-2 takes the artefact and caches
      // it; at this runtime's versions the same loader refuses it all the
      // same, the check standing before the cache.
      final loader = ArtefactLoader();
      final bytes = _artefact('glp-isa-2');
      expect(
          loader.load(bytes,
              offeredHM: _hm(), supportedIsaVersions: const {'glp-isa-2'}),
          isNotNull);
      expect(() => loader.load(bytes, offeredHM: _hm()), _refused('glp-isa-2'));
    });
  });

  test('0x54 is no opcode: an instruction carrying it is refused, not run',
      () {
    final r = WireReader(Uint8List.fromList([0x54, 0x00, 0x01]));
    expect(
        () => decodeInstruction(r,
            procNameOf: (i) => 'p/1', ctargetLabelOf: (i) => '#$i'),
        throwsA(isA<WireFormatException>().having(
            (e) => e.message, 'message', 'unknown opcode: 0x54')));
  });
}
