// Module-as-value: the consumer kernel `_run`/2 — the kernel behind the
// user-facing `run` system predicate.
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
import '../analysis/type_checker/type_identity.dart'
    show TypeIdentityTables, interfaceTypeIdentityTables;
import 'code_image.dart' show CodeImage;
import 'interp.dart' show ByteRunner;

/// Register the engine_v2-dependent module kernels onto [rt].
/// Called from the `GlpEngine` constructor, after the runtime is built.
///
/// `_run`/2 is the untyped activation; `_run`/3 activates under a required
/// type identity and `_find_type`/2 obtains one (GLP-Spec catalogue, "Dynamic
/// activation"; TGLP §Dynamic Activation).
void registerModuleKernels(GlpRuntime rt) {
  rt.bodyKernels.register('_run', 2, runKernel);
  rt.bodyKernels.register('_run', 3, runTypedKernel);
  rt.bodyKernels.register('_find_type', 2, findTypeKernel);
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

/// `_run(Goal, Module)`/2 — module-as-value, consumer half. See the library note.
BodyKernelResult runKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] _run/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  return _activate(rt, '_run/2', args[0], args[1]);
}

/// `_run(Goal, Type, Module)`/3 — activation under a required type identity
/// (GLP-Spec catalogue, "Dynamic activation"): activates Module and posts Goal
/// to it where Type equals the type identity Module's exports record for Goal's
/// predicate, and errs otherwise. The exported table is derived from the
/// artefact's interface text on first use (TGLP Implementation Notes,
/// "Deriving the exported table when the artefact is read") and cached on the
/// module value; the comparison is of hashes, and no term is type-checked at
/// run time (TGLP §Dynamic Activation).
BodyKernelResult runTypedKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    print('[ABORT] _run/3: expected 3 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final type = _deref(rt, args[1]);
  final String? required = type is ConstTerm && type.value is String
      ? type.value as String
      : (type is String ? type : null);
  if (required == null) {
    print('[ABORT] _run/3: second argument is not a type identity '
        '(a ground string), got $type');
    return BodyKernelResult.abort;
  }
  return _activate(rt, '_run/3', args[0], args[2], requiredType: required);
}

/// The type identity of a signature in a module's exported table, deriving and
/// caching the table on the module value.
String? _exportedIdentity(ModuleTerm module, Artefact artefact, String sig) {
  var tables = module.exportedTypesCache;
  if (tables is! TypeIdentityTables) {
    tables = interfaceTypeIdentityTables(
      typeDefsText: artefact.typeDefsText,
      exportDeclarationTexts: artefact.exports.map((e) => e.declarationText),
    );
    module.exportedTypesCache = tables;
  }
  return tables.exported[sig];
}

/// Activate [moduleArg] and post [goalArg] to it — the START primitive shared by
/// `_run`/2 and `_run`/3. Under [requiredType], the goal's predicate must be
/// exported at exactly that type identity.
BodyKernelResult _activate(
    GlpRuntime rt, String kernel, Object? goalArg, Object? moduleArg,
    {String? requiredType}) {
  final module = _deref(rt, moduleArg);
  if (module is! ModuleTerm) {
    print('[ABORT] $kernel: module argument is not a module value');
    return BodyKernelResult.abort;
  }
  final artefact = module.artefact;
  if (artefact is! Artefact) {
    print('[ABORT] $kernel: module carries no artefact');
    return BodyKernelResult.abort;
  }

  // The boot goal: a term boot(A, ...) — or a bare constant for arity 0.
  final goal = _deref(rt, goalArg);
  final String functor;
  final List<Term> bootArgs;
  if (goal is StructTerm) {
    functor = goal.functor;
    bootArgs = goal.args;
  } else if (goal is ConstTerm) {
    functor = goal.value.toString();
    bootArgs = const [];
  } else {
    print('[ABORT] $kernel: first argument is not a goal');
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
  // The goal's predicate must be exported by the module, else an error (GLP
  // paper, appendix "Guards, Body Kernels, and System Predicates", Dynamic
  // activation). Export check, not entry-point resolution: a compiled but
  // non-exported internal procedure is not runnable from outside.
  if (!image.exportAliases.contains(sig)) {
    print('[ABORT] $kernel: $sig is not exported by module ${module.name}');
    return BodyKernelResult.abort;
  }
  final entry = image.entryOffsetOf(sig);
  if (entry == null) {
    print(
        '[ABORT] $kernel: exported $sig has no entry point in module ${module.name}');
    return BodyKernelResult.abort;
  }

  if (requiredType != null) {
    final recorded = _exportedIdentity(module, artefact, sig);
    if (recorded == null) {
      print('[ABORT] $kernel: the exports of module ${module.name} record no '
          'type identity for $sig');
      return BodyKernelResult.abort;
    }
    if (recorded != requiredType) {
      print('[ABORT] $kernel: type identity mismatch for $sig in module '
          '${module.name}: required $requiredType, exports record $recorded');
      return BodyKernelResult.abort;
    }
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

/// `_find_type(P/N, Type)`/2 — the type identity of the declaration of `P/N`
/// in the caller's scope (GLP-Spec catalogue, "Dynamic activation"): the
/// declared table of the module the calling goal points to, keyed as the
/// compiled module carries the procedure (TGLP Implementation Notes, "The
/// tables"). The linker resolves `P/N` in the calling module's scope before
/// this runs, as it resolves a call. An error where `P/N` is not declared
/// there, and where it is parameterised, which has no identity.
BodyKernelResult findTypeKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    print('[ABORT] _find_type/2: expected 2 arguments, got ${args.length}');
    return BodyKernelResult.abort;
  }
  final ref = _deref(rt, args[0]);
  String? name;
  int? arity;
  if (ref is StructTerm && ref.functor == '/' && ref.args.length == 2) {
    final n = _deref(rt, ref.args[0]);
    final a = _deref(rt, ref.args[1]);
    if (n is ConstTerm && n.value is String) name = n.value as String;
    if (a is ConstTerm && a.value is int) arity = a.value as int;
  }
  if (name == null || arity == null) {
    print('[ABORT] _find_type/2: first argument is not a procedure reference '
        'P/N, got $ref');
    return BodyKernelResult.abort;
  }
  final key = '$name/$arity';

  final goalId = rt.currentGoalId;
  final module = goalId == null ? null : rt.getGoalModule(goalId);
  final tables = module is ModuleTerm ? module.declaredTypes : null;
  if (tables is! TypeIdentityTables) {
    print('[ABORT] _find_type/2: the calling goal\'s module carries no '
        'declared table, so $key is not declared in the caller\'s scope');
    return BodyKernelResult.abort;
  }
  final identity = tables.declared[key];
  if (identity == null) {
    if (tables.parametric.contains(key)) {
      print('[ABORT] _find_type/2: $key is a parameterised declaration and '
          'has no type identity');
    } else {
      print('[ABORT] _find_type/2: $key is not declared in the caller\'s '
          'scope');
    }
    return BodyKernelResult.abort;
  }
  return _bind(rt, args[1], ConstTerm(identity));
}

/// Bind the writer at [outputArg] to [value], enqueueing the goals the binding
/// wakes — the module kernels' copy of body_kernels.dart's binding idiom.
BodyKernelResult _bind(GlpRuntime rt, Object? outputArg, Term value) {
  if (outputArg is VarRef && rt.heap.isWriter(outputArg.addr)) {
    for (final act in rt.heap.bindVariable(outputArg.addr, value)) {
      rt.gq.enqueue(act);
    }
    return BodyKernelResult.success;
  }
  print('[ABORT] _find_type/2: output argument is not a writer');
  return BodyKernelResult.abort;
}
