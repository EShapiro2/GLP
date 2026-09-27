/// The instruction-set version of the code format.
///
/// Specification: IGLP, Code Format appendix at 877691b
/// (sections/code-format-fragment.tex): `0x54 spawn_rated` (proc, arity,
/// rate) is the instruction of the stochastic extension of GLP, which a
/// runtime not offering the extension never emits, and its arrival is an
/// instruction-set version change carried in the artefact header; the loader
/// refuses an unsupported instruction-set version, "an older runtime rejects
/// a newer instruction-set version at adoption", and "a newer runtime runs
/// older artefacts unchanged".
library;

import 'dart:typed_data';

import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine_v2/code_image.dart';
import 'package:glp_runtime/engine_v2/interp.dart' show codeImageFromProgram;
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:test/test.dart';

Uint8List _hm() => Uint8List.fromList(List<int>.generate(32, (i) => 7 * i));

final PersonIdentity _compiler = PersonIdentity.generate();

/// A program with a rated goal, compiled: it holds a spawn_rated.
List<Object> _ratedOps() => GlpCompiler()
    .compile('''
procedure p(Integer?).
p(X) :- q(X?) @ 1/week.
procedure q(Integer?).
q(_).
''')
    .ops
    .cast<Object>();

/// The certified artefact of [_ratedOps] at instruction-set version [isa].
Uint8List _artefact(String isa) => Artefact.fromCompiled(
      ops: _ratedOps(),
      hM: _hm(),
      moduleName: 'rated',
      isaVersion: isa,
      signer: _compiler,
    ).toBytes();

/// A runtime at the instruction-set version before spawn_rated.
const Set<String> _oldRuntime = {'glp-isa-1'};

void main() {
  test('this implementation writes glp-isa-2, the version with spawn_rated, '
      'and loads it and every earlier version', () {
    expect(glpIsaVersion, 'glp-isa-2');
    expect(runtimeIsaVersions, {'glp-isa-1', 'glp-isa-2'});
    expect(_ratedOps().whereType<SpawnRated>(), hasLength(1));
    final a = Artefact.fromBytes(_artefact(glpIsaVersion));
    expect(a.isaVersion, glpIsaVersion);
    // The engine's own image of a compiled program is at the version too.
    final img = codeImageFromProgram(GlpCompiler().compile('''
procedure p(Integer?).
p(X) :- q(X?) @ 1/week.
procedure q(Integer?).
q(_).
'''));
    expect(img.isaVersion, glpIsaVersion);
  });

  group('an artefact at the new version is refused by a runtime at the old',
      () {
    test('by the loader, at adoption', () {
      expect(
          () => ArtefactLoader().load(_artefact(glpIsaVersion),
              offeredHM: _hm(), supportedIsaVersions: _oldRuntime),
          throwsA(isA<WireFormatException>().having((e) => e.message,
              'message', 'unsupported ISA version: glp-isa-2')));
    });

    test('by the code image a runtime runs', () {
      expect(
          () => CodeImage.fromArtefactBytes(_artefact(glpIsaVersion),
              supportedIsaVersions: _oldRuntime),
          throwsA(isA<WireFormatException>().having((e) => e.message,
              'message', 'unsupported ISA version: glp-isa-2')));
    });

    test('whatever the loader has loaded before', () {
      final loader = ArtefactLoader();
      final bytes = _artefact(glpIsaVersion);
      expect(loader.load(bytes, offeredHM: _hm()), isNotNull);
      expect(
          () => loader.load(bytes,
              offeredHM: _hm(), supportedIsaVersions: _oldRuntime),
          throwsA(isA<WireFormatException>()));
    });
  });

  test('a runtime at the new version runs an artefact at the old unchanged, '
      'and refuses one newer than its own', () {
    final old = Artefact(
      isaVersion: 'glp-isa-1',
      hM: _hm(),
      moduleName: 'old',
      typeDefsText: '',
      exports: const [],
      symbols: [
        ArtefactSymbol.compiled('q', 1, <Object>[ClauseTry(), Commit(), Proceed()]),
      ],
      signer: _compiler,
    ).toBytes();
    expect(ArtefactLoader().load(old, offeredHM: _hm()).artefact.isaVersion,
        'glp-isa-1');
    expect(CodeImage.fromArtefactBytes(old).isaVersion, 'glp-isa-1');

    final newer = _artefact('glp-isa-3');
    expect(() => ArtefactLoader().load(newer, offeredHM: _hm()),
        throwsA(isA<WireFormatException>()));
    expect(() => CodeImage.fromArtefactBytes(newer),
        throwsA(isA<WireFormatException>()));
  });
}
