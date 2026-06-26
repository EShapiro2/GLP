/// Direct bytecode execution — the byte interpreter (B3b/B3c).
///
/// Goal (IGLP `app:code-format`): execute the §cf code-format byte string
/// DIRECTLY. [ByteRunner] is the FCP-style fetch/decode/dispatch loop over a
/// [CodeImage]'s `code` section — `switch (code[pc])`, operands read inline from
/// the byte stream, a byte-offset program counter — calling the SAME per-opcode
/// executors as the object runner (`OpExecutors` in `runner.dart`). There is no
/// second in-memory instruction form: the executed bytes are the shipped, hashed
/// bytes.
///
/// The semantics live once, in `OpExecutors`. This file is only the byte driver:
/// it decodes operands (mirroring `wire/instruction_codec.dart` `decodeInstruction`
/// case-for-case), calls `execX`, and maps the returned [StepOutcome] to a
/// byte-offset PC — `advance` → the byte after the operands; `nextClause` → a
/// scan to the next clause-control opcode within the procedure; `suspended`/
/// `proceed`/`halt` → a [RunResult]. The loop-divergent control ops
/// (`ClauseTry`/`Commit`/`NoMoreClauses`/`Proceed`/`Spawn`/`Requeue`) are handled
/// here, resolving `proc` indices to entry byte offsets via the [CodeImage]
/// symbol table.
///
/// Contract parity with `BytecodeRunner`: `runWithStatus(RunnerContext) →
/// RunResult`, with `cx.kappa` the goal's entry — a BYTE OFFSET here (an
/// instruction index in the object runner). The scheduler/engine drives goals
/// identically; only the per-goal run entry differs.
library;

import 'dart:io' show Platform;
import 'dart:typed_data';

import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/engine_v2/code_image.dart';
import 'package:glp_runtime/engine_v2/step_outcome.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/wire/artefact.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/instruction_codec.dart';

/// Sandbox flag (B4): when the environment sets `GLP_BYTE_INTERP=1`, the engine
/// drives goals through the byte interpreter ([ByteRunner]) instead of the
/// object loop ([BytecodeRunner]). Off by default → no behaviour change.
bool get byteInterpEnabled => Platform.environment['GLP_BYTE_INTERP'] == '1';

/// Flatten an object [BytecodeProgram] to a §cf artefact and reload it as a
/// [CodeImage] for direct byte execution — the bridge that lets the engine run
/// the same compiled program through the byte loop behind the sandbox flag.
/// (B6 makes the compiler emit the artefact directly, retiring this round-trip.)
CodeImage codeImageFromProgram(BytecodeProgram prog,
    {String moduleName = 'main'}) {
  final artefact = Artefact.fromCompiled(
    ops: prog.ops.cast<Object>(),
    hM: Uint8List(32),
    moduleName: moduleName,
    isaVersion: 'glp-isa-1',
    typeDefsText: '',
    exports: const [],
  );
  return CodeImage.fromArtefactBytes(artefact.toBytes());
}

/// Executes a [CodeImage] directly over its code bytes, reusing the object
/// runner's [OpExecutors] for every per-opcode semantic.
class ByteRunner with OpExecutors implements GoalRunner {
  final CodeImage image;

  /// Byte offsets of every clause-control opcode (`clause_try`/`clause_next`/
  /// `no_more_clauses`) in the code section, ascending — the byte analogue of
  /// the object runner scanning `prog.ops` for the next clause boundary. Built
  /// once by a length-aware decode pass over the whole code section.
  late final List<int> _controlOffsets = _scanControlOffsets();

  ByteRunner(this.image);

  List<int> _scanControlOffsets() {
    final offs = <int>[];
    final r = WireReader(image.code);
    while (!r.atEnd) {
      final off = r.offset;
      final opcode = image.code[off];
      // Decode-to-advance; the returned Op is discarded, we only need the
      // reader to step past this instruction's operands. Use the real symbol
      // resolver: the `guard` decode derives a bare name by stripping `/arity`,
      // which needs a valid `name/arity` signature (a placeholder would throw).
      decodeInstruction(r,
          procNameOf: (i) => image.symbolAt(i).signature,
          ctargetLabelOf: (i) => '#$i');
      if (opcode == Opcode.clauseTry ||
          opcode == Opcode.clauseNext ||
          opcode == Opcode.noMoreClauses) {
        offs.add(off);
      }
    }
    return offs;
  }

  /// First clause-control opcode strictly after [fromStart], or the code end if
  /// none — the byte analogue of `BytecodeRunner._findNextClauseTry`.
  int _nextClauseByte(int fromStart) {
    for (final co in _controlOffsets) {
      if (co > fromStart) return co;
    }
    return image.code.length;
  }

  /// Byte-loop routing for `StepOutcome.nextClause`: soft-fail the clause (merge
  /// Si into U, clear clause state — the public-API form of the object runner's
  /// `_softFailToNextClause`) and return the next clause's byte offset.
  int _applyNextClauseByte(RunnerContext cx, int opStart) {
    cx.U.addAll(cx.Si);
    cx.clearClause();
    return _nextClauseByte(opStart);
  }

  void run(RunnerContext cx) {
    runWithStatus(cx);
  }

  @override
  String? procNameForPc(int pc) {
    for (final s in image.symbols) {
      if (s.compiled && s.codeOffset == pc) return s.signature;
    }
    return null;
  }

  RunResult runWithStatus(RunnerContext cx) {
    final code = image.code;
    var pc = cx.kappa; // byte offset of the goal's entry instruction

    bool pol(WireReader r) {
      final p = r.u8();
      if (p != 0 && p != 1) throw WireFormatException('polarity not 0/1: $p');
      return p == 1;
    }

    bool neg(WireReader r) {
      final n = r.u8();
      if (n != 0 && n != 1) throw WireFormatException('negated not 0/1: $n');
      return n == 1;
    }

    Object? constant(WireReader r) => valueOfWireConst(decodeConstantPayload(r));

    while (pc < code.length) {
      if (cx.reductionBudget != null &&
          cx.reductionsUsed >= cx.reductionBudget!) {
        return RunResult.outOfReductions;
      }
      cx.reductionsUsed++;

      final opStart = pc;
      final r = WireReader(Uint8List.sublistView(code, pc));
      final opcode = r.u8();
      // Byte offset of the next instruction (after this opcode's operands).
      int after() => opStart + r.offset;

      switch (opcode) {
        // ===== Clause control =====
        case Opcode.clauseTry:
          execClauseTry(cx); // advance
          pc = after();
          continue;

        case Opcode.clauseNext:
          // Dead in current codegen (no ClauseNext emitted); the byte-offset
          // ctarget jump is a later slice. Reaching it signals stale codegen.
          throw StateError(
              'clause_next byte execution not yet implemented (B3c slice)');

        case Opcode.noMoreClauses:
          return execNoMoreClauses(cx).kind == StepKind.suspended
              ? RunResult.suspended
              : RunResult.terminated;

        case Opcode.commit:
          if (execCommit(cx).kind == StepKind.nextClause) {
            pc = _applyNextClauseByte(cx, opStart);
            continue;
          }
          pc = after();
          continue;

        case Opcode.proceed:
          execProceed(cx); // fires the reduction callback
          return RunResult.terminated;

        case Opcode.halt:
          execHalt();
          return RunResult.terminated;

        case Opcode.nop:
          pc = after();
          continue;

        case Opcode.otherwise:
          if (execOtherwise(cx).kind == StepKind.nextClause) {
            pc = _applyNextClauseByte(cx, opStart);
            continue;
          }
          pc = after();
          continue;

        // ===== Structure traversal control =====
        case Opcode.push:
          execPush(cx, r.clen());
          pc = after();
          continue;

        case Opcode.pop:
          execPop(cx, r.clen());
          pc = after();
          continue;

        // ===== HEAD matching =====
        case Opcode.headConstant:
          {
            final v = constant(r);
            final o = execHeadConstant(cx, v, r.clen());
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.headNil:
          {
            final o = execHeadNil(cx, r.clen());
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.headStructure:
          {
            final f = r.string();
            final o = execHeadStructure(cx, f, r.clen(), r.clen());
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.headList:
          {
            final o = execHeadList(cx, r.clen());
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.headVariable:
          {
            final isReader = pol(r);
            final o = execHeadVariable(cx, r.clen(), isReader);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.getVariable:
          {
            final isReader = pol(r);
            final varIndex = r.clen();
            final o = execGetVariable(cx, varIndex, r.clen(), isReader);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.getValue:
          {
            final isReader = pol(r);
            final varIndex = r.clen();
            final o = execGetValue(cx, varIndex, r.clen(), isReader);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        // ===== Structure subterm matching =====
        case Opcode.unifyVariable:
          {
            final isReader = pol(r);
            final o = execUnifyVariable(cx, r.clen(), isReader);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.unifyConstant:
          {
            final o = execUnifyConstant(cx, constant(r));
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.unifyVoid:
          execUnifyVoid(cx, r.clen());
          pc = after();
          continue;

        case Opcode.unifyStructure:
          {
            final f = r.string();
            final o = execUnifyStructure(cx, f, r.clen());
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        // ===== BODY argument setup =====
        case Opcode.putVariable:
          {
            final isReader = pol(r);
            final varIndex = r.clen();
            execPutVariable(cx, varIndex, r.clen(), isReader);
            pc = after();
            continue;
          }

        case Opcode.putConstant:
          {
            final v = constant(r);
            execPutConstant(cx, v, r.clen());
            pc = after();
            continue;
          }

        case Opcode.putNil:
          execPutNil(cx, r.clen());
          pc = after();
          continue;

        case Opcode.putList:
          execPutList(cx, r.clen());
          pc = after();
          continue;

        case Opcode.putStructure:
          {
            final f = r.string();
            execPutStructure(cx, f, r.clen(), r.clen());
            pc = after();
            continue;
          }

        case Opcode.putBoundConst:
          {
            final v = constant(r);
            execPutBoundConst(cx, v, r.clen());
            pc = after();
            continue;
          }

        case Opcode.putBoundNil:
          execPutBoundNil(cx, r.clen());
          pc = after();
          continue;

        case Opcode.setVariable:
          {
            final isReader = pol(r);
            execSetVariable(cx, r.clen(), isReader);
            pc = after();
            continue;
          }

        case Opcode.setConstant:
          execSetConstant(cx, constant(r));
          pc = after();
          continue;

        case Opcode.allocate:
          {
            final slots = r.clen();
            execAllocate(cx, slots, after()); // continuation = next instr byte
            pc = after();
            continue;
          }

        case Opcode.deallocate:
          execDeallocate(cx);
          pc = after();
          continue;

        // ===== Guards =====
        case Opcode.guard:
          {
            final sig = image.symbolAt(r.clen()).signature;
            final arity = r.clen();
            final negated = neg(r);
            final name = sig.substring(0, sig.lastIndexOf('/'));
            if (execGuard(cx, name, arity, negated).kind ==
                StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.ground:
          {
            final varIndex = r.clen();
            final negated = neg(r);
            if (execGround(cx, varIndex, negated).kind ==
                StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.known:
          {
            final varIndex = r.clen();
            final negated = neg(r);
            if (execKnown(cx, varIndex, negated).kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.unknown:
          {
            final o = execUnknown(cx, r.clen());
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.noReaders:
          {
            final varIndex = r.clen();
            final negated = neg(r);
            if (execNoReaders(cx, varIndex, negated).kind ==
                StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        case Opcode.groundEqual:
          {
            final l = r.clen();
            final rr = r.clen();
            final negated = neg(r);
            if (execGroundEqual(cx, l, rr, negated).kind ==
                StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after();
            continue;
          }

        // ===== Goal spawning / control =====
        case Opcode.spawn:
          {
            final procIndex = r.clen();
            final arity = r.clen();
            final result = _spawn(cx, procIndex, arity);
            if (result != null) return result; // terminal (kernel abort / error)
            pc = after();
            continue;
          }

        case Opcode.requeue:
          {
            final procIndex = r.clen();
            final arity = r.clen();
            final (result, nextPc) = _requeue(cx, procIndex, arity);
            if (result != null) return result;
            if (nextPc != null) {
              pc = nextPc;
              continue;
            }
            pc = after();
            continue;
          }

        default:
          throw WireFormatException(
              'unknown opcode 0x${opcode.toRadixString(16)} at byte $opStart');
      }
    }
    return RunResult.terminated;
  }

  /// Spawn a body goal. Mirrors the object runner's `Spawn` arm but resolves the
  /// `proc` symbol index to a CALLEE ENTRY BYTE OFFSET via [CodeImage], and
  /// enqueues a `GoalRef` carrying that byte offset. Returns a [RunResult] only
  /// on a terminal outcome (kernel abort or unresolved symbol); otherwise null
  /// (advance).
  RunResult? _spawn(RunnerContext cx, int procIndex, int arity) {
    if (!cx.inBody) return null;

    final symbol = image.symbolAt(procIndex);

    if (!symbol.compiled) {
      // Codeless symbol — a runtime body kernel bound by name (same fallback as
      // the object Spawn handler).
      final kernel = cx.rt.bodyKernels.lookup(symbol.name, arity);
      if (kernel != null) {
        final args = <Object?>[];
        for (var i = 0; i < arity; i++) {
          args.add(cx.argSlots[i]);
        }
        final result = kernel(cx.rt, args);
        if (result == BodyKernelResult.abort) {
          print('ERROR: Body kernel ${symbol.name}/$arity aborted');
          return RunResult.terminated;
        }
        cx.argSlots.clear();
        return null;
      }
      print('ERROR: Spawn could not find procedure: ${symbol.signature}');
      return RunResult.terminated;
    }

    // Compiled callee — enqueue a new goal at its entry byte offset.
    final newEnv = CallEnv(args: Map<int, Term>.from(cx.argSlots));
    final newGoalId = cx.rt.nextGoalId++;
    final newGoalRef = GoalRef(newGoalId, symbol.codeOffset);

    // Format the spawned goal for the reduction trace.
    final args = <String>[];
    for (var i = 0; i < 10; i++) {
      final term = newEnv.arg(i);
      if (term == null) break;
      args.add(cx.termFormatter != null
          ? cx.termFormatter!(term)
          : term.toString());
    }
    cx.spawnedGoals.add(
        args.isEmpty ? symbol.name : '${symbol.name}(${args.join(', ')})');

    cx.rt.setGoalEnv(newGoalId, newEnv);

    // Inherit the parent's program key so the scheduler routes the child to the
    // same (byte) runner (mirrors the object Spawn handler).
    final parentProgram = cx.rt.getGoalProgram(cx.goalId);
    if (parentProgram != null) {
      cx.rt.setGoalProgram(newGoalId, parentProgram);
    }

    cx.rt.gq.enqueue(newGoalRef);

    if (cx.rt.infrastructureGoalIds.contains(cx.goalId)) {
      cx.rt.infrastructureGoalIds.add(newGoalId);
    }

    cx.argSlots.clear();
    return null;
  }

  /// Tail call (`Requeue`). Mirrors the object runner's `Requeue` arm but jumps
  /// to the callee's entry BYTE OFFSET. Returns `(result, nextPc)`: a non-null
  /// `result` is terminal (`yielded` on a fairness yield, `terminated` on an
  /// unresolved symbol); a non-null `nextPc` is the byte offset to continue the
  /// in-line tail call from; both null means advance.
  (RunResult?, int?) _requeue(RunnerContext cx, int procIndex, int arity) {
    if (!cx.inBody) return (null, null);

    final symbol = image.symbolAt(procIndex);
    if (!symbol.compiled) {
      print('ERROR: Requeue could not find procedure: ${symbol.signature}');
      return (RunResult.terminated, null);
    }
    final entryByte = symbol.codeOffset;

    // Format the requeued goal for the reduction trace.
    final args = <String>[];
    for (var i = 0; i < 10; i++) {
      final term = cx.argSlots[i];
      if (term == null) break;
      args.add(cx.termFormatter != null
          ? cx.termFormatter!(term)
          : term.toString());
    }
    final newHeadGoalStr =
        args.isEmpty ? symbol.name : '${symbol.name}(${args.join(', ')})';
    cx.spawnedGoals.add(newHeadGoalStr);

    if (cx.onReduction != null && cx.goalHead != null) {
      final body = cx.spawnedGoals.join(', ');
      cx.onReduction!(cx.goalId, cx.reformatHead(), body);
    }

    cx.env.update(Map<int, Term>.from(cx.argSlots));
    cx.argSlots.clear();
    cx.spawnedGoals.clear();
    cx.goalHead = newHeadGoalStr;

    // Reset clause state for the new procedure.
    cx.sigmaHat.clear();
    cx.U.clear();
    cx.clauseVars.clear();
    cx.inBody = false;
    cx.mode = UnifyMode.read;
    cx.S = 0;
    cx.currentStructure = null;

    cx.kappa = entryByte;

    // Tail-recursion fairness (ISA §9.2): spend the tail budget; on exhaustion,
    // re-enqueue at the entry byte offset and yield.
    if (cx.rt.tailReduce(cx.goalId)) {
      cx.rt.gq.enqueue(GoalRef(cx.goalId, entryByte));
      return (RunResult.yielded, null);
    }

    return (null, entryByte);
  }
}
