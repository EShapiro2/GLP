/// The instruction-set version of the code format.
///
/// Specification: IGLP, Code Format appendix at eadadcd
/// (sections/code-format-fragment.tex), "Format Versioning": the header
/// carries the instruction-set version, a string, and "A loader refuses an
/// artefact whose code-format version or instruction-set version it does not
/// support"; "an older runtime rejects a newer instruction-set version at
/// adoption"; and "Removing an operand from an assigned opcode, or an opcode,
/// is an instruction-set version change after which the runtime refuses the
/// versions before it; the current version is glp-isa-3".  The opcode table
/// has no 0x54: `spawn_rated`, the instruction of sGLP's engine extension,
/// left the code format with the extension at 8c1d5e2, and with it
/// `glp-isa-2`, the version that carried it.  This runtime writes and loads
/// `glp-isa-3`, and refuses an artefact at `glp-isa-1` or `glp-isa-2`.
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
  test('this implementation writes glp-isa-3 and loads glp-isa-3 alone', () {
    expect(glpIsaVersion, 'glp-isa-3');
    expect(runtimeIsaVersions, {'glp-isa-3'});
    final bytes = _artefact(glpIsaVersion);
    expect(Artefact.fromBytes(bytes).isaVersion, 'glp-isa-3');
    expect(ArtefactLoader().load(bytes, offeredHM: _hm()).artefact.isaVersion,
        'glp-isa-3');
    expect(CodeImage.fromArtefactBytes(bytes).isaVersion, 'glp-isa-3');
    // The engine's own image of a compiled program is at the version too.
    expect(codeImageFromProgram(GlpCompiler().compile(_source)).isaVersion,
        'glp-isa-3');
  });

  for (final isa in ['glp-isa-1', 'glp-isa-2']) {
    group('an artefact at $isa, a version before the current one, is refused',
        () {
      test('by the loader, at adoption', () {
        expect(() => ArtefactLoader().load(_artefact(isa), offeredHM: _hm()),
            _refused(isa));
      });

      test('by the code image a runtime runs', () {
        expect(() => CodeImage.fromArtefactBytes(_artefact(isa)),
            _refused(isa));
      });

      test('whatever the loader has loaded before', () {
        // A loader told it supports the version takes the artefact and
        // caches it; at this runtime's versions the same loader refuses it
        // all the same, the check standing before the cache.
        final loader = ArtefactLoader();
        final bytes = _artefact(isa);
        expect(
            loader.load(bytes,
                offeredHM: _hm(), supportedIsaVersions: {isa}),
            isNotNull);
        expect(() => loader.load(bytes, offeredHM: _hm()), _refused(isa));
      });
    });
  }

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
