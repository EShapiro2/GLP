/// B3b — the byte interpreter (`ByteRunner` over a `CodeImage`) executes the
/// §cf code bytes directly and produces outcomes IDENTICAL to the object runner
/// (`BytecodeRunner` over `BytecodeProgram`) for the same program.
///
/// Each case builds one ops list, runs it BOTH ways from the same initial state,
/// and asserts the `RunResult`, the reduction (commit happened), and any heap
/// binding agree. The ops list is shared: `BC.prog(ops)` for the object path,
/// `Artefact.fromCompiled(ops:)` → `CodeImage` for the byte path. This is the
/// verification gate of the plan ("run a fact then a one-clause program, verify
/// identical outcome vs the object loop"), extended to clause selection (the
/// byte `nextClause` scan) and suspension (Si→U through `no_more_clauses`).
library;

import 'dart:typed_data';
import 'package:test/test.dart';

import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/engine_v2/code_image.dart';
import 'package:glp_runtime/engine_v2/interp.dart';

/// Outcome of a single goal run, plus the runtime so callers can inspect heap.
class _Run {
  final RunResult result;
  final bool reduced;
  final GlpRuntime rt;
  _Run(this.result, this.reduced, this.rt);
}

/// Set up a fresh runtime's goal args and return the CallEnv. Receives the
/// runtime so it can allocate heap variables; records nothing else (addresses
/// allocate identically in both runs, so values are compared, not addresses).
typedef _Setup = CallEnv Function(GlpRuntime rt);

const _hM = <int>[
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, //
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31
];

_Run _runObject(List<Op> ops, String entry, _Setup setup) {
  final rt = GlpRuntime();
  final env = setup(rt);
  var reduced = false;
  final prog = BC.prog(ops);
  final cx = RunnerContext(
    rt: rt,
    goalId: 100,
    kappa: prog.labels[entry]!,
    env: env,
    goalHead: entry,
    onReduction: (_, __, ___) => reduced = true,
  );
  final res = BytecodeRunner(prog).runWithStatus(cx);
  return _Run(res, reduced, rt);
}

_Run _runByte(List<Op> ops, String entry, _Setup setup) {
  final rt = GlpRuntime();
  final env = setup(rt);
  var reduced = false;
  final artefact = Artefact.fromCompiled(
    ops: ops,
    hM: Uint8List.fromList(_hM),
    moduleName: 't',
    isaVersion: 'glp-isa-1',
    typeDefsText: '',
    exports: const [],
  );
  final img = CodeImage.fromArtefactBytes(artefact.toBytes());
  final cx = RunnerContext(
    rt: rt,
    goalId: 100,
    kappa: img.entryOffsetOf(entry)!,
    env: env,
    goalHead: entry,
    onReduction: (_, __, ___) => reduced = true,
  );
  final res = ByteRunner(img).runWithStatus(cx);
  return _Run(res, reduced, rt);
}

void main() {
  test('fact p/0 — both runners terminate with a reduction', () {
    final ops = <Op>[
      BC.L('p/0'),
      BC.TRY(),
      BC.COMMIT(),
      BC.PROCEED(),
    ];
    CallEnv setup(GlpRuntime rt) => CallEnv();

    final obj = _runObject(ops, 'p/0', setup);
    final byte = _runByte(ops, 'p/0', setup);

    expect(obj.result, RunResult.terminated);
    expect(byte.result, obj.result);
    expect(obj.reduced, isTrue);
    expect(byte.reduced, obj.reduced);
  });

  test('q/1 head_constant binds the goal writer — same binding both ways', () {
    // q(X) :- true.  with HeadConstant binding X to 'foo'.
    final ops = <Op>[
      BC.L('q/1'),
      BC.TRY(),
      BC.headConst('foo', 0),
      BC.COMMIT(),
      BC.PROCEED(),
    ];

    int writerAddr = -1;
    CallEnv setup(GlpRuntime rt) {
      final (w, _) = rt.heap.allocateVariable();
      writerAddr = w; // identical allocation order → same addr in both runs
      return CallEnv(args: {0: VarRef(w)});
    }

    final obj = _runObject(ops, 'q/1', setup);
    final byte = _runByte(ops, 'q/1', setup);

    expect(obj.result, RunResult.terminated);
    expect(byte.result, obj.result);
    expect(obj.reduced && byte.reduced, isTrue);

    // The writer is bound to 'foo' in both runtimes.
    expect(obj.rt.heap.isWriterBound(writerAddr), isTrue);
    expect(byte.rt.heap.isWriterBound(writerAddr), isTrue);
    final objVal = obj.rt.heap.valueOfWriter(writerAddr);
    final byteVal = byte.rt.heap.valueOfWriter(writerAddr);
    expect(objVal, isA<ConstTerm>());
    expect((objVal as ConstTerm).value, 'foo');
    expect(byteVal, isA<ConstTerm>());
    expect((byteVal as ConstTerm).value, 'foo');
  });

  test('r/1 two clauses: first fails, second commits — byte nextClause scan', () {
    // r(a) :- true.   r(b) :- true.   goal r(B) with B bound to 'b'.
    // Clause 1 (HeadConstant 'a') mismatches a bound reader 'b' → nextClause;
    // the byte loop must scan to clause 2's clause_try, which matches.
    final ops = <Op>[
      BC.L('r/1'),
      BC.TRY(),
      BC.headConst('a', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.TRY(),
      BC.headConst('b', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.NO_MORE_CLAUSES(),
    ];

    CallEnv setup(GlpRuntime rt) {
      final (w, rdr) = rt.heap.allocateVariable();
      rt.heap.bindWriterConst(w, 'b');
      return CallEnv(args: {0: VarRef(rdr)});
    }

    final obj = _runObject(ops, 'r/1', setup);
    final byte = _runByte(ops, 'r/1', setup);

    expect(obj.result, RunResult.terminated);
    expect(byte.result, obj.result);
    // Reduction fired → clause 2 committed (not fell through to no_more_clauses).
    expect(obj.reduced, isTrue);
    expect(byte.reduced, obj.reduced);
  });

  test('r/1 unbound reader arg: no clause matches → suspended both ways', () {
    // Same two-clause r/1, but the arg is an UNBOUND reader: both clauses
    // suspend (Si), no_more_clauses sees U non-empty → RunResult.suspended.
    final ops = <Op>[
      BC.L('r/1'),
      BC.TRY(),
      BC.headConst('a', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.TRY(),
      BC.headConst('b', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.NO_MORE_CLAUSES(),
    ];

    CallEnv setup(GlpRuntime rt) {
      final (_, rdr) = rt.heap.allocateVariable(); // unbound reader
      return CallEnv(args: {0: VarRef(rdr)});
    }

    final obj = _runObject(ops, 'r/1', setup);
    final byte = _runByte(ops, 'r/1', setup);

    expect(obj.result, RunResult.suspended);
    expect(byte.result, obj.result);
    expect(obj.reduced, isFalse);
    expect(byte.reduced, isFalse);
  });

  test('spawn chain main/0 → p(foo) reduces twice — byte Spawn via CodeImage',
      () {
    // main :- p(foo).   p(foo) :- true.
    // The body of main puts 'foo' in arg 0 and spawns p/1; the spawned p/1
    // head-matches 'foo'. Exercises Spawn's proc-index → entry-byte-offset
    // resolution and a minimal goal-queue drive. Two reductions iff the arg
    // threaded through correctly.
    final ops = <Op>[
      BC.L('main/0'),
      BC.TRY(),
      BC.COMMIT(),
      BC.putConst('foo', 0),
      BC.spawn('p/1', 1),
      BC.PROCEED(),
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('foo', 0),
      BC.COMMIT(),
      BC.PROCEED(),
    ];

    // Minimal goal-queue drive: pop each goal, run it, count reductions.
    int driveObject() {
      final rt = GlpRuntime();
      final prog = BC.prog(ops);
      final runner = BytecodeRunner(prog);
      rt.setGoalEnv(1, CallEnv());
      rt.gq.enqueue(GoalRef(1, prog.labels['main/0']!));
      return _driveAll(runner.runWithStatus, rt);
    }

    int driveByte() {
      final rt = GlpRuntime();
      final artefact = Artefact.fromCompiled(
        ops: ops,
        hM: Uint8List.fromList(_hM),
        moduleName: 't',
        isaVersion: 'glp-isa-1',
        typeDefsText: '',
        exports: const [],
      );
      final img = CodeImage.fromArtefactBytes(artefact.toBytes());
      final runner = ByteRunner(img);
      rt.setGoalEnv(1, CallEnv());
      rt.gq.enqueue(GoalRef(1, img.entryOffsetOf('main/0')!));
      return _driveAll(runner.runWithStatus, rt);
    }

    final objReductions = driveObject();
    final byteReductions = driveByte();
    expect(objReductions, 2);
    expect(byteReductions, objReductions);
  });
}

/// Run every queued goal to completion, counting reductions. Shared by the
/// object and byte runs (only [runOne] — the runner's `runWithStatus` — differs).
int _driveAll(RunResult Function(RunnerContext) runOne, GlpRuntime rt) {
  var reductions = 0;
  while (!rt.gq.isEmpty) {
    final g = rt.gq.dequeue()!;
    final env = rt.getGoalEnv(g.id) ?? CallEnv();
    var reduced = false;
    final cx = RunnerContext(
      rt: rt,
      goalId: g.id,
      kappa: g.pc,
      env: env,
      goalHead: 'g',
      onReduction: (_, __, ___) => reduced = true,
    );
    runOne(cx);
    if (reduced) reductions++;
  }
  return reductions;
}
