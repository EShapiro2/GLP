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

import 'dart:typed_data';

import '../runtime/runtime.dart';
import '../runtime/body_kernels.dart' show BodyKernelResult;
import '../runtime/terms.dart';
import '../runtime/machine_state.dart' show GoalRef;
import '../wire/artefact.dart' show Artefact;
import '../bytecode/runner.dart' show CallEnv;
import 'code_image.dart' show CodeImage;
import 'interp.dart' show ByteRunner;

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

/// Lowercase hex of a hash — used to key a module's runner by its source
/// identity h(M), so equal modules share one runner.
String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

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
  final artefact = module.artefact;
  if (artefact is! Artefact) {
    print('[ABORT] run/2: module carries no artefact');
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

  // One ByteRunner per distinct module, keyed by its source identity h(M), so
  // the Scheduler routes this goal (and its children, which inherit the key) to
  // the module's code via rt.runners — its documented per-goal-program fallback.
  // Equal modules share a runner, so the image is decoded once per module.
  final key = 'module:${_hex(artefact.hM)}';
  final cached = rt.runners[key];
  final CodeImage image = cached is ByteRunner
      ? cached.image
      : CodeImage.fromArtefactBytes(artefact.toBytes());

  final sig = '$functor/${bootArgs.length}';
  final entry = image.entryOffsetOf(sig);
  if (entry == null) {
    // Resolution, not type-checking: the module exposes no such entry point.
    print('[ABORT] run/2: $sig has no entry point in module ${module.name}');
    return BodyKernelResult.abort;
  }

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
