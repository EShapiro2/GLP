/// Direct bytecode execution — the byte interpreter (B3b/B3c).
///
/// Goal (IGLP `app:code-format`): execute the §cf code-format byte string
/// DIRECTLY. [ByteRunner] is the FCP-style fetch/decode/dispatch loop over a
/// [CodeImage]'s `code` section — `switch (code[pc])`, operands read inline from
/// the byte stream, a byte-offset program counter — calling the per-opcode
/// executors (`OpExecutors` in `runner.dart`). There is no
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
/// Run contract: `runWithStatus(RunnerContext) → RunResult`, with `cx.kappa`
/// the goal's entry as a BYTE OFFSET into the code section. The scheduler/engine
/// drives every goal through this runner.
library;

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
    isaVersion: glpIsaVersion,
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
  /// none: the next clause boundary in the code section, found by binary
  /// search in [_controlOffsets], which is ascending.  Until 2026-10-02 the
  /// list was walked from its start at every clause that failed or suspended,
  /// so a clause try cost in the number of clauses linked before it: the
  /// linked sGLP program has 429 boundaries, `:=/2` starts at the 315th and
  /// has some forty clauses, and the walk was half the run's time.
  int nextClauseByte(int fromStart) {
    final offs = _controlOffsets;
    var lo = 0, hi = offs.length;
    while (lo < hi) {
      final mid = (lo + hi) >> 1;
      if (offs[mid] > fromStart) {
        hi = mid;
      } else {
        lo = mid + 1;
      }
    }
    return lo < offs.length ? offs[lo] : image.code.length;
  }

  /// Byte-loop routing for `StepOutcome.nextClause`: leave the clause (clear its
  /// state) and return the next clause's byte offset.  A clause that SUSPENDED
  /// --- the commit added readers to U ([SuspensionSet.touched]) ---
  /// also gives U its own suspension set Si; one that FAILED gives nothing,
  /// whatever it suspended on before failing: "The writer mgu is the union of
  /// all writer assignments if no fail was encountered and the suspension set
  /// is empty" (GLP-Spec appendix-term-matching.tex, Definition "Term
  /// Matching"), so a fail anywhere is a fail.  Until 2026-10-02 Si was merged
  /// on every next clause, and a goal whose every clause failed suspended if
  /// one of them had met an unbound reader first (GLP 2026-10-01 23:58 UTC
  /// item 4).
  int _applyNextClauseByte(RunnerContext cx, int opStart) {
    if (cx.U.touched) cx.U.addAll(cx.Si);
    cx.clearClause();
    return nextClauseByte(opStart);
  }

  void run(RunnerContext cx) {
    runWithStatus(cx);
  }

  /// The signature of the first compiled symbol at each entry offset, made
  /// once: [procNameForPc] is asked for every goal the scheduler takes, and
  /// walked the symbol table each time until 2026-10-02.
  late final Map<int, String> _procNameByPc = () {
    final m = <int, String>{};
    for (final s in image.symbols) {
      if (s.compiled) m.putIfAbsent(s.codeOffset, () => s.signature);
    }
    return m;
  }();

  @override
  String? procNameForPc(int pc) => _procNameByPc[pc];

  /// The decoded operands of each instruction executed, by its byte offset:
  /// an instruction's operands are decoded at its first execution and kept,
  /// the bytes being what is run, shipped and hashed.  Until 2026-10-02 each
  /// execution decoded them again, a reader over a view of the code made
  /// for it and a functor's or constant's bytes decoded afresh.
  late final List<_Ins?> _decoded =
      List<_Ins?>.filled(image.code.length, null);

  /// Decode the instruction at [pc], as the loop below once did at each
  /// execution: the opcode, its operands in order, and the byte after them.
  _Ins _decodeAt(int pc) {
    final code = image.code;
    final r = WireReader(Uint8List.sublistView(code, pc));
    final opcode = r.u8();

    bool pol() {
      final p = r.u8();
      if (p != 0 && p != 1) throw WireFormatException('polarity not 0/1: $p');
      return p == 1;
    }

    Object? constant() => valueOfWireConst(decodeConstantPayload(r));

    switch (opcode) {
      case Opcode.clauseTry:
      case Opcode.clauseNext:
      case Opcode.noMoreClauses:
      case Opcode.commit:
      case Opcode.proceed:
      case Opcode.halt:
      case Opcode.nop:
      case Opcode.otherwise:
      case Opcode.deallocate:
        return _Ins(opcode, pc + r.offset);
      case Opcode.push:
      case Opcode.pop:
      case Opcode.headNil:
      case Opcode.headList:
      case Opcode.unifyVoid:
      case Opcode.putNil:
      case Opcode.putList:
      case Opcode.putBoundNil:
      case Opcode.allocate:
      case Opcode.ground:
      case Opcode.known:
      case Opcode.unknown:
      case Opcode.noReaders:
        final a = r.clen();
        return _Ins(opcode, pc + r.offset, a: a);
      case Opcode.headConstant:
      case Opcode.putConstant:
      case Opcode.putBoundConst:
        final k = constant();
        final a = r.clen();
        return _Ins(opcode, pc + r.offset, k: k, a: a);
      case Opcode.unifyConstant:
      case Opcode.setConstant:
        final k = constant();
        return _Ins(opcode, pc + r.offset, k: k);
      case Opcode.headStructure:
      case Opcode.putStructure:
        final f = r.string();
        final a = r.clen();
        final b = r.clen();
        return _Ins(opcode, pc + r.offset, k: f, a: a, b: b);
      case Opcode.unifyStructure:
        final f = r.string();
        final a = r.clen();
        return _Ins(opcode, pc + r.offset, k: f, a: a);
      case Opcode.headVariable:
      case Opcode.unifyVariable:
      case Opcode.setVariable:
        final p = pol();
        final a = r.clen();
        return _Ins(opcode, pc + r.offset, pol: p, a: a);
      case Opcode.getVariable:
      case Opcode.getValue:
      case Opcode.putVariable:
        final p = pol();
        final a = r.clen();
        final b = r.clen();
        return _Ins(opcode, pc + r.offset, pol: p, a: a, b: b);
      case Opcode.guard:
        final sig = image.symbolAt(r.clen()).signature;
        final arity = r.clen();
        final name = sig.substring(0, sig.lastIndexOf('/'));
        return _Ins(opcode, pc + r.offset, k: name, a: arity);
      case Opcode.groundEqual:
      case Opcode.spawn:
      case Opcode.requeue:
        final a = r.clen();
        final b = r.clen();
        return _Ins(opcode, pc + r.offset, a: a, b: b);
      default:
        throw WireFormatException(
            'unknown opcode 0x${opcode.toRadixString(16)} at byte $pc');
    }
  }

  /// The operands kept for the instruction at [pc], decoded now if it has not
  /// run yet: its opcode, the byte after its operands, its clen operands in
  /// order, its polarity, and its constant, functor or guard name.
  ({int op, int next, int a, int b, bool pol, Object? k}) decodedOperandsAt(
      int pc) {
    final ins = _decoded[pc] ??= _decodeAt(pc);
    return (op: ins.op, next: ins.next, a: ins.a, b: ins.b, pol: ins.pol, k: ins.k);
  }

  /// A constant operand as the instruction carries it: a byte string copied,
  /// as each decode once made it afresh, the rest shared, being immutable.
  static Object? _k(Object? k) => k is Uint8List ? Uint8List.fromList(k) : k;

  RunResult runWithStatus(RunnerContext cx) {
    final code = image.code;
    final decoded = _decoded;
    var pc = cx.kappa; // byte offset of the goal's entry instruction

    while (pc < code.length) {
      final opStart = pc;
      final ins = decoded[pc] ??= _decodeAt(pc);
      // Byte offset of the next instruction (after this opcode's operands).
      final after = ins.next;

      switch (ins.op) {
        // ===== Clause control =====
        case Opcode.clauseTry:
          execClauseTry(cx); // advance
          pc = after;
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
          pc = after;
          continue;

        case Opcode.proceed:
          execProceed(cx); // fires the reduction callback
          return RunResult.terminated;

        case Opcode.halt:
          execHalt();
          return RunResult.terminated;

        case Opcode.nop:
          pc = after;
          continue;

        case Opcode.otherwise:
          if (execOtherwise(cx).kind == StepKind.nextClause) {
            pc = _applyNextClauseByte(cx, opStart);
            continue;
          }
          pc = after;
          continue;

        // ===== Structure traversal control =====
        case Opcode.push:
          execPush(cx, ins.a);
          pc = after;
          continue;

        case Opcode.pop:
          execPop(cx, ins.a);
          pc = after;
          continue;

        // ===== HEAD matching =====
        case Opcode.headConstant:
          {
            final o = execHeadConstant(cx, _k(ins.k), ins.a);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.headNil:
          {
            final o = execHeadNil(cx, ins.a);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.headStructure:
          {
            final o =
                execHeadStructure(cx, ins.k as String, ins.a, ins.b);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.headList:
          {
            final o = execHeadList(cx, ins.a);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.headVariable:
          {
            final o = execHeadVariable(cx, ins.a, ins.pol);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.getVariable:
          {
            final o = execGetVariable(cx, ins.a, ins.b, ins.pol);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.getValue:
          {
            final o = execGetValue(cx, ins.a, ins.b, ins.pol);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        // ===== Structure subterm matching =====
        case Opcode.unifyVariable:
          {
            final o = execUnifyVariable(cx, ins.a, ins.pol);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.unifyConstant:
          {
            final o = execUnifyConstant(cx, _k(ins.k));
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.unifyVoid:
          {
            // A goal writer at a head `_` fails ([OpExecutors.execUnifyVoid]).
            final o = execUnifyVoid(cx, ins.a);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.unifyStructure:
          {
            final o = execUnifyStructure(cx, ins.k as String, ins.a);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        // ===== BODY argument setup =====
        case Opcode.putVariable:
          execPutVariable(cx, ins.a, ins.b, ins.pol);
          pc = after;
          continue;

        case Opcode.putConstant:
          execPutConstant(cx, _k(ins.k), ins.a);
          pc = after;
          continue;

        case Opcode.putNil:
          execPutNil(cx, ins.a);
          pc = after;
          continue;

        case Opcode.putList:
          execPutList(cx, ins.a);
          pc = after;
          continue;

        case Opcode.putStructure:
          execPutStructure(cx, ins.k as String, ins.a, ins.b);
          pc = after;
          continue;

        case Opcode.putBoundConst:
          execPutBoundConst(cx, _k(ins.k), ins.a);
          pc = after;
          continue;

        case Opcode.putBoundNil:
          execPutBoundNil(cx, ins.a);
          pc = after;
          continue;

        case Opcode.setVariable:
          execSetVariable(cx, ins.a, ins.pol);
          pc = after;
          continue;

        case Opcode.setConstant:
          execSetConstant(cx, _k(ins.k));
          pc = after;
          continue;

        case Opcode.allocate:
          execAllocate(cx, ins.a, after); // continuation = next instr byte
          pc = after;
          continue;

        case Opcode.deallocate:
          execDeallocate(cx);
          pc = after;
          continue;

        // ===== Guards =====
        case Opcode.guard:
          {
            if (execGuard(cx, ins.k as String, ins.a).kind ==
                StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.ground:
          {
            if (execGround(cx, ins.a).kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.known:
          {
            if (execKnown(cx, ins.a).kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.unknown:
          {
            final o = execUnknown(cx, ins.a);
            if (o.kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.noReaders:
          {
            if (execNoReaders(cx, ins.a).kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        case Opcode.groundEqual:
          {
            if (execGroundEqual(cx, ins.a, ins.b).kind == StepKind.nextClause) {
              pc = _applyNextClauseByte(cx, opStart);
              continue;
            }
            pc = after;
            continue;
          }

        // ===== Goal spawning / control =====
        case Opcode.spawn:
          {
            final result = _spawn(cx, ins.a, ins.b);
            if (result != null) return result; // terminal (kernel abort / error)
            pc = after;
            continue;
          }

        case Opcode.requeue:
          {
            final (result, nextPc) = _requeue(cx, ins.a, ins.b);
            if (result != null) return result;
            if (nextPc != null) {
              pc = nextPc;
              continue;
            }
            pc = after;
            continue;
          }

        default:
          throw WireFormatException(
              'unknown opcode 0x${ins.op.toRadixString(16)} at byte $opStart');
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
        // Expose the calling goal to the kernel (self_module and friends): the
        // (rt, args) signature carries no goal handle.
        cx.rt.currentGoalId = cx.goalId;
        final result = kernel(cx.rt, args);
        if (result == BodyKernelResult.abort) {
          print('ERROR: Body kernel ${symbol.name}/$arity aborted');
          return RunResult.terminated;
        }
        if (result == BodyKernelResult.fail) {
          // The predicate does not hold of its arguments: the goal fails and
          // joins F, exactly as a spawned goal with no procedure does below.
          final call = _callText(symbol.name, arity, cx);
          cx.rt.failedGoals.add(call);
          print('ERROR: goal failed: $call');
          cx.argSlots.clear();
          return null;
        }
        cx.argSlots.clear();
        return null;
      }
      // No kernel of that name. A root-scope procedure --- merge/3, send/3, the
      // clauses of programs/self.glp --- is not in a module's artefact: the
      // root scope is ambient at every runtime and excluded from h(M), so an
      // activated module (run/2, run/3, a module read from a file) reaches it
      // through the runtime's root runner, registered by the engine as
      // `__root__`. The goal is spawned there, its PC in the root's code.
      if (_spawnInRoot(cx, symbol.name, arity)) {
        cx.argSlots.clear();
        return null;
      }
      // Neither, so the spawned body goal has no procedure and
      // FAILS. It does not end the parent's run: IGLP gives a reduction exactly
      // three outcomes — succeeds, suspends with a suspension set, or fails —
      // and the dGLP and madGLP Reduce transactions each put a failed goal in F
      // and continue with the remainder of the queue. No transaction ends a
      // computation. So the parent goes on spawning the rest of its body and
      // proceeds, and this goal joins F.
      //
      // The diagnostic carries the call's arguments, not the signature alone.
      // No := domain error arrives here: a body kernel whose precondition
      // fails aborts (GLP-Spec appendix-guards), so '_div', '_idiv', '_mod',
      // '_sqrt', '_ln', '_log10', '_asin' and '_acos' abort themselves, and
      // the root self.glp sends nothing to the undefined abort/1, as its
      // domain-error clauses did until 2026-10-02.
      final call = _callText(symbol.name, arity, cx);
      cx.rt.failedGoals.add(call);
      print('ERROR: no procedure for goal, failed: $call');
      cx.argSlots.clear();
      return null;
    }

    // Compiled callee — enqueue a new goal at its entry byte offset.
    final newEnv = CallEnv(args: Map<int, Term>.from(cx.argSlots));
    final newGoalId = cx.rt.nextGoalId++;
    final newGoalRef = GoalRef(newGoalId, symbol.codeOffset);

    // Format the spawned goal for the reduction trace, where the run keeps one.
    if (cx.tracing) {
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
    }

    cx.rt.setGoalEnv(newGoalId, newEnv);

    // Inherit the parent's program key so the scheduler routes the child to the
    // same (byte) runner (mirrors the object Spawn handler).
    final parentProgram = cx.rt.getGoalProgram(cx.goalId);
    if (parentProgram != null) {
      cx.rt.setGoalProgram(newGoalId, parentProgram);
    }
    // The invariant: a spawned goal runs its parent's module (its PC indexes
    // into that module's code). Inherit the module value.
    cx.rt.setGoalModule(newGoalId, cx.rt.getGoalModule(cx.goalId));

    cx.rt.gq.enqueue(newGoalRef);

    cx.argSlots.clear();
    return null;
  }

  /// Spawn `name/arity` as a goal of the runtime's root runner (`__root__`),
  /// where the root self.glp's procedures are compiled, with this goal's
  /// argument slots as its arguments and this goal's module value inherited.
  /// True where the root runner has the procedure; false where it has not or
  /// no root runner is registered, and nothing was spawned.
  bool _spawnInRoot(RunnerContext cx, String name, int arity) {
    final root = cx.rt.runners['__root__'];
    if (root is! ByteRunner) return false;
    final sig = '$name/$arity';
    final entry = root.image.entryOffsetOf(sig);
    if (entry == null) return false;

    final newEnv = CallEnv(args: Map<int, Term>.from(cx.argSlots));
    final newGoalId = cx.rt.nextGoalId++;

    if (cx.tracing) {
      final args = <String>[];
      for (var i = 0; i < 10; i++) {
        final term = newEnv.arg(i);
        if (term == null) break;
        args.add(cx.termFormatter != null
            ? cx.termFormatter!(term)
            : term.toString());
      }
      cx.spawnedGoals.add(args.isEmpty ? name : '$name(${args.join(', ')})');
    }

    cx.rt.setGoalEnv(newGoalId, newEnv);
    cx.rt.setGoalProgram(newGoalId, '__root__');
    cx.rt.setGoalModule(newGoalId, cx.rt.getGoalModule(cx.goalId));
    cx.rt.gq.enqueue(GoalRef(newGoalId, entry));
    return true;
  }

  /// A goal's call text — `name(arg, ...)`, or `name/arity` when it has no
  /// arguments — formatted from the argument slots with the context's formatter,
  /// the same way a spawned goal is formatted for the reduction trace.
  String _callText(String name, int arity, RunnerContext cx) {
    final args = <String>[];
    for (var i = 0; i < arity; i++) {
      final t = cx.argSlots[i];
      if (t == null) continue;
      args.add(
          cx.termFormatter != null ? cx.termFormatter!(t) : t.toString());
    }
    return args.isEmpty ? '$name/$arity' : '$name(${args.join(', ')})';
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
      // A tail call to a root-scope procedure from an activated module: the
      // callee's code is in the root runner, not this image, so the tail call
      // becomes a spawn there and this goal proceeds.
      if (_spawnInRoot(cx, symbol.name, arity)) {
        cx.argSlots.clear();
        execProceed(cx);
        return (RunResult.terminated, null);
      }
      print('ERROR: Requeue could not find procedure: ${symbol.signature}');
      return (RunResult.terminated, null);
    }
    final entryByte = symbol.codeOffset;

    // Format the requeued goal for the reduction trace, where the run keeps
    // one; the reduction is recorded either way.
    final tracing = cx.tracing;
    String? newHeadGoalStr;
    if (tracing) {
      final args = <String>[];
      for (var i = 0; i < 10; i++) {
        final term = cx.argSlots[i];
        if (term == null) break;
        args.add(cx.termFormatter != null
            ? cx.termFormatter!(term)
            : term.toString());
      }
      newHeadGoalStr =
          args.isEmpty ? symbol.name : '${symbol.name}(${args.join(', ')})';
      cx.spawnedGoals.add(newHeadGoalStr);

      final body = cx.spawnedGoals.join(', ');
      cx.onReduction!(cx.goalId, cx.reformatHead(), body);
    }
    cx.reduced = true;

    cx.env.update(Map<int, Term>.from(cx.argSlots));
    cx.argSlots.clear();
    cx.spawnedGoals.clear();
    if (tracing) cx.goalHead = newHeadGoalStr;

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


/// An instruction's decoded operands ([ByteRunner._decodeAt]): its opcode,
/// the byte offset after its operands, and the operands by kind --- clen
/// operands in [a] and [b] in their order, a polarity in [pol], a constant, a
/// functor or a guard's name in [k].
final class _Ins {
  final int op;
  final int next;
  final int a;
  final int b;
  final bool pol;
  final Object? k;
  const _Ins(this.op, this.next,
      {this.a = 0, this.b = 0, this.pol = false, this.k});
}
