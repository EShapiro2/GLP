/// The byte runner's clause navigation: [ByteRunner.nextClauseByte], the
/// next clause boundary after a byte offset, where a clause that fails or
/// suspends sends the run; and [ByteRunner.procNameForPc], the procedure a
/// goal's entry offset names.
///
/// Until 2026-10-02 both walked a list from its start each time they were
/// asked: the next boundary was found by walking every clause boundary of the
/// linked program up to the offset, so a clause try cost in the number of
/// clauses linked before it, and a goal of `:=/2`, which starts some three
/// hundred boundaries in and has some forty clauses, cost thousands of steps
/// to suspend --- half the time of a year of sGLP's 100-agent social graph.
/// The boundary is now found by binary search and the name by a table.  These
/// tests hold both to the walk's answer at every byte offset of a linked
/// program that carries the whole root scope.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/instruction_codec.dart';
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

/// A procedure of many clauses beside the root scope's, and calls of `:=/2`.
const String _source = r'''
procedure colour(Integer?, Integer).
colour(1, 10).
colour(2, 20).
colour(3, 30).
colour(4, 40).
colour(5, 50).
colour(6, 60).
colour(7, 70).
colour(8, 80).

procedure sum(Integer?, Integer?, Integer).
sum(0, A, A?).
sum(N, A, S?) :- N? > 0 | colour(N?, C), A1 := A? + C?, N1 := N? - 1, sum(N1?, A1?, S).
''';

void main() {
  final engine = GlpEngine(rootSelfGlpPath: _root)..loadSource(_source);
  final image = codeImageFromProgram(engine.combinedProgram);
  final runner = ByteRunner(image);

  // The clause boundaries, by a decode of the code section of its own.
  final boundaries = <int>[];
  final r = WireReader(image.code);
  while (!r.atEnd) {
    final off = r.offset;
    final opcode = image.code[off];
    decodeInstruction(r,
        procNameOf: (i) => image.symbolAt(i).signature,
        ctargetLabelOf: (i) => '#$i');
    if (opcode == Opcode.clauseTry ||
        opcode == Opcode.clauseNext ||
        opcode == Opcode.noMoreClauses) {
      boundaries.add(off);
    }
  }

  test('the program carries the root scope: over a hundred clause boundaries',
      () {
    expect(boundaries.length, greaterThan(100));
  });

  test(
      'the next clause boundary after every byte offset is the first '
      'boundary past it, as a walk from the start finds it, or the code end',
      () {
    int walk(int from) {
      for (final b in boundaries) {
        if (b > from) return b;
      }
      return image.code.length;
    }

    for (var off = -1; off <= image.code.length; off++) {
      expect(runner.nextClauseByte(off), walk(off), reason: 'offset $off');
    }
  });

  test(
      'the procedure an entry offset names is the first compiled symbol '
      'there, as a walk of the symbol table finds it, and none elsewhere', () {
    String? walk(int pc) {
      for (final s in image.symbols) {
        if (s.compiled && s.codeOffset == pc) return s.signature;
      }
      return null;
    }

    for (var pc = 0; pc <= image.code.length; pc++) {
      expect(runner.procNameForPc(pc), walk(pc), reason: 'offset $pc');
    }
  });

  test('a run through many clauses and :=/2 computes as before', () async {
    final r = await engine.runGoal('sum(8, 0, S)');
    expect('${r.bindings['S']}', 'Const(360)');
  });
}
