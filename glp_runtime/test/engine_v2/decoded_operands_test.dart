/// The byte runner decodes an instruction's operands at its first execution
/// and keeps them by the instruction's byte offset ([ByteRunner]); until
/// 2026-10-02 every execution decoded them again from the bytes.  The bytes
/// are what run, ship and are hashed; what is kept is what a decode of them
/// gives.  These tests hold the kept operands to the decoder's reading of the
/// bytes at every instruction of a linked program that carries the root scope,
/// and runs that execute the same instructions many times to what the
/// language computes.
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/code_image.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/instruction_codec.dart';
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

/// Constants of each kind in heads and bodies, guards and arithmetic, each
/// executed many times by the recursion.
const String _source = r'''
procedure tag(Integer?, Constant).
tag(0, zero).
tag(1, "one").
tag(2, 2.5).
tag(N, many) :- N? > 2 | true.

procedure walk(Integer?, Stream(Constant)).
walk(0, []).
walk(N, [T?|Ts?]) :- N? > 0 | M := N? mod 4, tag(M?, T), N1 := N? - 1, walk(N1?, Ts).
''';

/// The operands the decoder reads at an instruction, in the form the runner
/// keeps them: its clen operands in order, its polarity, and its constant,
/// functor or guard name.  Spawn and requeue name their callee by its symbol
/// index, the decoder by its signature.
({int a, int b, bool pol, Object? k}) _operands(Op op, CodeImage image) {
  return switch (op) {
    HeadConstant(:final value, :final argSlot) =>
      (a: argSlot, b: 0, pol: false, k: value),
    PutConstant(:final value, :final argSlot) =>
      (a: argSlot, b: 0, pol: false, k: value),
    PutBoundConst(:final value, :final argSlot) =>
      (a: argSlot, b: 0, pol: false, k: value),
    UnifyConstant(:final value) => (a: 0, b: 0, pol: false, k: value),
    SetConstant(:final value) => (a: 0, b: 0, pol: false, k: value),
    HeadStructure(:final functor, :final arity, :final argSlot) =>
      (a: arity, b: argSlot, pol: false, k: functor),
    PutStructure(:final functor, :final arity, :final argSlot) =>
      (a: arity, b: argSlot, pol: false, k: functor),
    UnifyStructure(:final functor, :final arity) =>
      (a: arity, b: 0, pol: false, k: functor),
    HeadNil(:final argSlot) => (a: argSlot, b: 0, pol: false, k: null),
    HeadList(:final argSlot) => (a: argSlot, b: 0, pol: false, k: null),
    PutNil(:final argSlot) => (a: argSlot, b: 0, pol: false, k: null),
    PutList(:final argSlot) => (a: argSlot, b: 0, pol: false, k: null),
    PutBoundNil(:final argSlot) => (a: argSlot, b: 0, pol: false, k: null),
    UnifyVoid(:final count) => (a: count, b: 0, pol: false, k: null),
    Push(:final regIndex) => (a: regIndex, b: 0, pol: false, k: null),
    Pop(:final regIndex) => (a: regIndex, b: 0, pol: false, k: null),
    Allocate(:final slots) => (a: slots, b: 0, pol: false, k: null),
    Ground(:final varIndex) => (a: varIndex, b: 0, pol: false, k: null),
    Known(:final varIndex) => (a: varIndex, b: 0, pol: false, k: null),
    Unknown(:final varIndex) => (a: varIndex, b: 0, pol: false, k: null),
    NoReaders(:final varIndex) => (a: varIndex, b: 0, pol: false, k: null),
    GroundEqual(:final leftVarIndex, :final rightVarIndex) =>
      (a: leftVarIndex, b: rightVarIndex, pol: false, k: null),
    HeadVariable(:final varIndex, :final isReader) =>
      (a: varIndex, b: 0, pol: isReader, k: null),
    UnifyVariable(:final varIndex, :final isReader) =>
      (a: varIndex, b: 0, pol: isReader, k: null),
    SetVariable(:final varIndex, :final isReader) =>
      (a: varIndex, b: 0, pol: isReader, k: null),
    GetVariable(:final varIndex, :final argSlot, :final isReader) =>
      (a: varIndex, b: argSlot, pol: isReader, k: null),
    GetValue(:final varIndex, :final argSlot, :final isReader) =>
      (a: varIndex, b: argSlot, pol: isReader, k: null),
    PutVariable(:final varIndex, :final argSlot, :final isReader) =>
      (a: varIndex, b: argSlot, pol: isReader, k: null),
    Guard(:final procedureLabel, :final arity) =>
      (a: arity, b: 0, pol: false, k: procedureLabel),
    Spawn(:final procedureLabel, :final arity) => (
        a: image.symbolIndexOf(procedureLabel)!,
        b: arity,
        pol: false,
        k: null
      ),
    Requeue(:final procedureLabel, :final arity) => (
        a: image.symbolIndexOf(procedureLabel)!,
        b: arity,
        pol: false,
        k: null
      ),
    _ => (a: 0, b: 0, pol: false, k: null),
  };
}

void main() {
  final engine = GlpEngine(rootSelfGlpPath: _root)..loadSource(_source);
  final image = codeImageFromProgram(engine.combinedProgram);
  final runner = ByteRunner(image);

  test(
      'the operands kept for every instruction of a linked program are those '
      'the decoder reads from its bytes, and so is the byte after them', () {
    final r = WireReader(image.code);
    var n = 0;
    while (!r.atEnd) {
      final off = r.offset;
      final op = decodeInstruction(r,
          procNameOf: (i) => image.symbolAt(i).signature,
          ctargetLabelOf: (i) => '#$i') as Op;
      final kept = runner.decodedOperandsAt(off);
      final want = _operands(op, image);
      expect(kept.op, image.code[off], reason: 'opcode at $off');
      expect(kept.next, r.offset, reason: 'next instruction after $off');
      expect(kept.a, want.a, reason: 'operand a at $off ($op)');
      expect(kept.b, want.b, reason: 'operand b at $off ($op)');
      expect(kept.pol, want.pol, reason: 'polarity at $off ($op)');
      expect(kept.k, want.k, reason: 'constant or name at $off ($op)');
      n++;
    }
    expect(n, greaterThan(1000));
  });

  test(
      'runs that execute the same instructions many times compute what the '
      'language does', () async {
    final once = await engine.runGoal('walk(2, Xs)');
    expect(once.status, ExecutionStatus.succeeded);
    final many = await engine.runGoal('walk(400, Ys)');
    expect(many.status, ExecutionStatus.succeeded);
    Term? cell = many.bindings['Ys'];
    final seen = <String>[];
    while (cell is StructTerm && cell.functor == '.') {
      seen.add('${engine.runtime.heap.dereference(cell.args[0])}');
      cell = engine.runtime.heap.dereference(cell.args[1]);
    }
    expect(seen.length, 400);
    // 400, 399, ... mod 4: 0 zero, 3 many, 2 2.5, 1 "one", repeating, the
    // string constant held with its quotes.
    expect(seen.take(4).toList(),
        ['Const(zero)', 'Const(many)', 'Const(2.5)', 'Const("one")']);
    expect(seen.toSet().length, 4);
  });
}
