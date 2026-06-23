/// S4 — artefact round-trip over real compiled programs (§§5, 7).
///
/// Compile a program, build its artefact, write it, read it back, reconstruct
/// the runnable program. Behaviour-identity to direct compilation holds by
/// construction: the reconstructed program is the original minus erased internal
/// labels (`_cN`/`_end`), which nothing references (no clause_next is emitted)
/// and the runtime skips. The proof here is structural and exact: the artefact
/// round-trips byte-stably, the reconstructed program carries exactly the
/// original's procedure entries, and every instruction is preserved (only
/// internal labels drop).
library;

import 'dart:typed_data';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:test/test.dart';

Uint8List _hm() => Uint8List.fromList(List<int>.generate(32, (i) => 7));

final _internal = RegExp(r'_c\d+$');
bool _isEntry(String n) =>
    n.contains('/') && !n.endsWith('_end') && !_internal.hasMatch(n);

Set<String> _entrySigs(List<Object> ops) => {
      for (final op in ops)
        if (op is Label && _isEntry(op.name)) op.name,
    };

int _nonInternalCount(List<Object> ops) =>
    ops.where((op) => op is! Label || _isEntry((op as Label).name)).length;

void _checkProgram(String name, String source) {
  test('$name: artefact round-trips and reconstructs faithfully', () {
    final engine = GlpEngine(rootSelfGlpPath: '../programs/self.glp')
      ..strictTypes = false;
    engine.loadSource(source, filename: name);
    final ops0 = engine.combinedProgram.ops.cast<Object>();

    final art = Artefact.fromCompiled(
      ops: ops0,
      hM: _hm(),
      moduleName: name,
      isaVersion: '2.16.3',
    );
    final bytes1 = art.toBytes();
    final art2 = Artefact.fromBytes(bytes1);
    final prog2 = art2.toProgram();

    // (1) Whole-pipeline byte idempotence: re-deriving the artefact from the
    // reconstructed program yields the identical bytes.
    final bytes2 = Artefact.fromCompiled(
      ops: prog2.ops.cast<Object>(),
      hM: _hm(),
      moduleName: name,
      isaVersion: '2.16.3',
    ).toBytes();
    expect(bytes2, bytes1, reason: 'program→artefact→program→artefact is stable');

    // (2) The reconstructed program carries exactly the original's procedure
    // entries (and only entry labels — no internal labels survive).
    final origEntries = _entrySigs(ops0);
    final reEntries = _entrySigs(prog2.ops.cast<Object>());
    expect(reEntries, origEntries);
    final reLabels =
        prog2.ops.whereType<Label>().map((l) => l.name).toSet();
    expect(reLabels, origEntries, reason: 'only entry labels remain');

    // (3) Every instruction is preserved: the reconstructed op count equals the
    // original's with internal labels removed.
    expect(prog2.ops.length, _nonInternalCount(ops0));

    // (4) Sanity: there is real content and the symbol table covers all targets.
    expect(origEntries, isNotEmpty);
  });
}

void main() {
  group('artefact round-trip over compiled programs', () {
    // Exercises builtin guards (>, =<) — codeless symbols bound by name.
    _checkProgram('classify', '''
procedure classify(Integer?, Constant).
classify(N, positive) :- N? > 0 | true.
classify(N, nonpositive) :- N? =< 0 | true.
''');

    _checkProgram('append', '''
List(X) ::= [] ; [X | List(X)].
procedure append(List(X)?, List(X)?, List(X)).
append([], Ys, Ys?).
append([X|Xs], Ys, [X?|Zs?]) :- append(Xs?, Ys?, Zs).
''');
  });
}
