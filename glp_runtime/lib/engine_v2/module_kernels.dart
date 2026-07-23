// Module-as-value: the consumer kernel `run/2`.
//
// `run(Goal, Module)` launches Goal as a fresh initial goal on the Module value,
// with its program counter into that module's code — the START primitive, the
// inverse of `self_module`. A friend runs an adopted module by `run`-ning its
// boot goal.
//
// This is per-goal program selection — the same seam the engine's own initial
// goal uses (rt.setGoalProgram + the Scheduler's rt.runners fallback) — NOT the
// retired cross-module dispatch routing: `run` starts a new
// goal tree on a module; it routes no cross-unit call.
//
// It lives here, not in runtime/body_kernels.dart, so it can reach engine_v2's
// CodeImage/ByteRunner without an import cycle. `GlpEngine` registers it via
// [registerModuleKernels].

import '../runtime/runtime.dart';
import '../runtime/body_kernels.dart' show BodyKernelResult;
import '../runtime/terms.dart';
import '../runtime/machine_state.dart' show GoalRef;
import '../bytecode/runner.dart' show BytecodeProgram, CallEnv;
import 'code_image.dart' show CodeImage;
import 'interp.dart' show ByteRunner, codeImageFromProgram;

/// Register the engine_v2-dependent module kernels onto [rt].
/// Called from the `GlpEngine` constructor, after the runtime is built.
void registerModuleKernels(GlpRuntime rt) {
  rt.bodyKernels.register('run', 2, runKernel);
}

/// Follow a top-level VarRef to its bound value (shallow dereference).
Object? _deref(GlpRuntime rt, Object? term) {
  if (term is VarRef) {
    return rt.heap.getValue(term.addr);
  }
  return term;
}

/// `run(Goal, Module)`/2 — module-as-value, consumer half. See the library note.
BodyKernelResult runKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] run/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }

  final module = _deref(rt, args[1]);
  if (module is! ModuleTerm) {
    print('[ABORT] run/2: second argument is not a module value');
    return BodyKernelResult.abort;
  }
  final program = module.bytecode;
  if (program is! BytecodeProgram) {
    print('[ABORT] run/2: module carries no bytecode program');
    return BodyKernelResult.abort;
  }

  // The boot goal: a term boot(A, ...) — or a bare constant for arity 0.
  final goal = _deref(rt, args[0]);
  final String functor;
  final List<Term> bootArgs;
  if (goal is StructTerm) {
    functor = goal.functor;
    bootArgs = goal.args;
  } else if (goal is ConstTerm) {
    functor = goal.value.toString();
    bootArgs = const [];
  } else {
    print('[ABORT] run/2: first argument is not a goal');
    return BodyKernelResult.abort;
  }

  final CodeImage image = codeImageFromProgram(program, moduleName: module.name);
  final sig = '$functor/${bootArgs.length}';
  final entry = image.entryOffsetOf(sig);
  if (entry == null) {
    // Resolution, not type-checking: the module exposes no such entry point.
    print('[ABORT] run/2: $sig has no entry point in module ${module.name}');
    return BodyKernelResult.abort;
  }

  // One ByteRunner per distinct module program, keyed by program identity, so
  // the Scheduler routes this goal (and its children, which inherit the key) to
  // the module's code via rt.runners — its documented per-goal-program fallback.
  final key = 'module:${identityHashCode(program)}';
  rt.runners.putIfAbsent(key, () => ByteRunner(image));

  final slots = <int, Term>{};
  for (var i = 0; i < bootArgs.length; i++) {
    slots[i] = bootArgs[i];
  }

  final newGoalId = rt.nextGoalId++;
  rt.setGoalEnv(newGoalId, CallEnv(args: slots));
  rt.setGoalProgram(newGoalId, key);
  rt.setGoalModule(newGoalId, module);
  rt.gq.enqueue(GoalRef(newGoalId, entry));
  return BodyKernelResult.success;
}
