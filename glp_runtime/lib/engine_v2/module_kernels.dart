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

import '../runtime/runtime.dart';
import '../runtime/body_kernels.dart'
    show BodyKernelResult, bindResult, bytesToHex, kernelAbort;
import '../runtime/terms.dart';
import '../runtime/machine_state.dart' show GoalRef;
import '../wire/artefact.dart' show Artefact;
import '../bytecode/runner.dart' show CallEnv;
import '../analysis/type_checker/type_identity.dart'
    show
        OpaqueInstanceRefusal,
        TypeIdentityTables,
        interfaceTypeIdentityTables;
import 'code_image.dart' show CodeImage;
import 'interp.dart' show ByteRunner;

/// Register the engine_v2-dependent module kernels onto [rt].
/// Called from the `GlpEngine` constructor, after the runtime is built.
///
/// `_run`/2 is the untyped activation; `_run`/3 activates under a required
/// type identity and `_find_type`/2 obtains one; `_opaque_type`/3 obtains the
/// type a module records for its entry and `_find_type`/3 the identity of an
/// opaque instance at it (GLP-Spec catalogue, "Dynamic activation"; TGLP
/// §Dynamic Activation).
void registerModuleKernels(GlpRuntime rt) {
  rt.bodyKernels.register('_run', 2, runKernel);
  rt.bodyKernels.register('_run', 3, runTypedKernel);
  rt.bodyKernels.register('_find_type', 2, findTypeKernel);
  rt.bodyKernels.register('_find_type', 3, findTypeOpaqueKernel);
  rt.bodyKernels.register('_opaque_type', 3, opaqueTypeKernel);
}

/// Follow a top-level VarRef to its bound value (shallow dereference).
Object? _deref(GlpRuntime rt, Object? term) {
  if (term is VarRef) {
    return rt.heap.getValue(term.addr);
  }
  return term;
}

/// `_run(Goal, Module)`/2 — module-as-value, consumer half. See the library note.
BodyKernelResult runKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 2) {
    return kernelAbort('[ABORT] _run/2: expected 2 arguments, got ${args.length}');
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
    return kernelAbort('[ABORT] _run/3: expected 3 arguments, got ${args.length}');
  }
  final type = _deref(rt, args[1]);
  final String? required = type is ConstTerm && type.value is String
      ? type.value as String
      : (type is String ? type : null);
  if (required == null) {
    return kernelAbort('[ABORT] _run/3: second argument is not a type identity '
        '(a ground string), got $type');
  }
  return _activate(rt, '_run/3', args[0], args[2], requiredType: required);
}

/// A module's exported tables, derived from its artefact's interface text and
/// cached on the module value.
TypeIdentityTables _exportedTables(ModuleTerm module, Artefact artefact) {
  var tables = module.exportedTypesCache;
  if (tables is! TypeIdentityTables) {
    tables = interfaceTypeIdentityTables(
      typeDefsText: artefact.typeDefsText,
      exportDeclarationTexts: artefact.exports.map((e) => e.declarationText),
    );
    module.exportedTypesCache = tables;
  }
  return tables;
}

/// Activate [moduleArg] and post [goalArg] to it — the START primitive shared by
/// `_run`/2 and `_run`/3. Under [requiredType], the goal's predicate must be
/// exported at exactly that type identity.
BodyKernelResult _activate(
    GlpRuntime rt, String kernel, Object? goalArg, Object? moduleArg,
    {String? requiredType}) {
  final module = _deref(rt, moduleArg);
  if (module is! ModuleTerm) {
    return kernelAbort('[ABORT] $kernel: module argument is not a module value');
  }
  final artefact = module.artefact;
  if (artefact is! Artefact) {
    return kernelAbort('[ABORT] $kernel: module carries no artefact');
  }

  // The certificate is verified before the code runs: the loader "computes
  // SHA-256 of the body and verifies it equals the compiled identity in the
  // certificate; verifies the certificate's signature under the key the
  // certificate carries" (IGLP code-format-fragment.tex, Loader, step 1), and
  // the machine "activates no program whose certificate does not verify" (GSG
  // s6-security.tex, G1).  Activation is run/2 and run/3, an adopter's of a
  // module it received included, which it runs by running its boot goal; the
  // engine's local load of a program is not an activation and is not refused
  // here (GLP #3 Cowork, 2026-10-02 15:46 UTC, B6).  A module refused a
  // certificate --- it calls an OS-privileged predicate --- carries none, and
  // is refused with the forged and the mismatched.  Until 2026-10-02 nothing
  // here looked at the certificate.
  final refusal = artefact.certificateRefusal;
  if (refusal != null) {
    return kernelAbort('[ABORT] $kernel: module ${module.name} is not activated: $refusal');
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
    return kernelAbort('[ABORT] $kernel: first argument is not a goal');
  }

  // One ByteRunner per distinct compiled module, keyed by its compiled
  // identity, the SHA-256 of the artefact's body: "artefacts are cached and
  // deduplicated by compiled identity" (IGLP code-format-fragment.tex, Loader,
  // step 4).  The Scheduler routes this goal (and its children, which inherit
  // the key) to the module's code via rt.runners.  Until 2026-10-02 the key was
  // the source identity h(M), so two compilations of one source --- two
  // compilers, two instruction-set versions --- shared the first one's code.
  final key = 'module:${bytesToHex(artefact.compiledIdentity)}';
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
    return kernelAbort('[ABORT] $kernel: $sig is not exported by module ${module.name}');
  }
  final entry = image.entryOffsetOf(sig);
  if (entry == null) {
    return kernelAbort(
        '[ABORT] $kernel: exported $sig has no entry point in module ${module.name}');
  }

  // The exports record for an entry its own identity, and, for a guest's
  // entry declared as the instance of a parameterised declaration D at its
  // type T, the identity of D<iota(T)> beside it (TGLP modules.tex, Dynamic
  // Activation, "Opaque types": "run activates M where the two are equal").
  // The required identity is to equal one of them.
  if (requiredType != null) {
    final tables = _exportedTables(module, artefact);
    final recorded = tables.exported[sig];
    final opaque = tables.opaque[sig]?.entry;
    if (recorded == null && opaque == null) {
      return kernelAbort('[ABORT] $kernel: the exports of module ${module.name} record no '
          'type identity for $sig');
    }
    if (recorded != requiredType && opaque != requiredType) {
      return kernelAbort('[ABORT] $kernel: type identity mismatch for $sig in module '
          '${module.name}: required $requiredType, exports record '
          '${[recorded, opaque].whereType<String>().join(' and ')}');
    }
  }

  rt.runners.putIfAbsent(key, () => ByteRunner(image));

  // A goal's argument registers hold variables (IGLP app:in-heap: a register
  // holds a cell, so a CallEnv argument is a VarRef): a ground argument of
  // the posted goal --- run(entry([], Out), T, M) --- is put in a fresh
  // variable bound to it, its reader in the register, as the engine sets up
  // the arguments of an initial goal (glp_engine.dart, _setupArgument).  Until
  // 2026-10-13 the term itself was put there, and the goal's first reduction
  // failed the runner's assertion.
  final slots = <int, Term>{};
  for (var i = 0; i < bootArgs.length; i++) {
    final arg = bootArgs[i];
    if (arg is VarRef) {
      slots[i] = arg;
      continue;
    }
    final (writer, reader) = rt.heap.allocateVariable();
    for (final act in rt.heap.bindVariable(writer, arg)) {
      rt.gq.enqueue(act);
    }
    slots[i] = VarRef(reader);
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
    return kernelAbort('[ABORT] _find_type/2: expected 2 arguments, got ${args.length}');
  }
  final key = _procedureKey(rt, args[0]);
  if (key == null) {
    return kernelAbort('[ABORT] _find_type/2: first argument is not a procedure reference '
        'P/N, got ${_deref(rt, args[0])}');
  }

  final tables = _callerTables(rt);
  if (tables is! TypeIdentityTables) {
    return kernelAbort('[ABORT] _find_type/2: the calling goal\'s module carries no '
        'declared table, so $key is not declared in the caller\'s scope');
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
  return bindResult(rt, args[1], ConstTerm(identity), kernel: '_find_type/2');
}

/// The declared table of the caller's scope (TGLP, Implementation Notes,
/// "The tables"): the module the calling goal runs carries it; a goal that
/// runs no module, one posted at the root with no program loaded, is in the
/// root's scope, and its table is the root's ([GlpRuntime.rootDeclaredTypes]).
/// "find_type assigns the type identity of a declaration in the caller's
/// scope" (TGLP modules.tex, "Dynamic Activation").  Until 2026-10-24 such a
/// goal had no table, and find_type(now/1, T) at the prompt with no program
/// loaded erred.
Object? _callerTables(GlpRuntime rt) {
  final goalId = rt.currentGoalId;
  final module = goalId == null ? null : rt.getGoalModule(goalId);
  if (module is ModuleTerm) return module.declaredTypes;
  return rt.rootDeclaredTypes?.call();
}

/// A procedure reference `P/N` as its key `p/n`, or null where [ref] is none.
String? _procedureKey(GlpRuntime rt, Object? ref) {
  final r = _deref(rt, ref);
  if (r is StructTerm && r.functor == '/' && r.args.length == 2) {
    final n = _deref(rt, r.args[0]);
    final a = _deref(rt, r.args[1]);
    if (n is ConstTerm && n.value is String && a is ConstTerm && a.value is int) {
      return '${n.value}/${a.value}';
    }
  }
  return null;
}

/// A list of type identities, ground strings, or null where [arg] is none.
List<String>? _identities(GlpRuntime rt, Object? arg) {
  // Followed through every reference: the list's cells are a term's.
  Object? follow(Object? x) {
    var y = x;
    while (y is VarRef) {
      final v = _deref(rt, y);
      if (v == null || identical(v, y)) return y;
      y = v;
    }
    return y;
  }

  final out = <String>[];
  var t = follow(arg);
  while (t is StructTerm && t.functor == '.' && t.args.length == 2) {
    final e = follow(t.args[0]);
    if (e is! ConstTerm || e.value is! String) return null;
    out.add(e.value as String);
    t = follow(t.args[1]);
  }
  if (t is ConstTerm && (t.value == nil || t.value == null)) return out;
  return null;
}

/// `_find_type(P/N, Hs, Type)`/3, `'_find_type'(_?, List(Constant)?,
/// Constant)` — the type identity of the opaque instance at `Hs` of the
/// caller's parameterised declaration of `P/N`, `Hs` a list of type
/// identities, one per type parameter of the declaration in declaration
/// order, and an error where `P/N` is not declared there or the length of
/// `Hs` is not the number of its type parameters (GLP-Spec appendix-guards,
/// the Modules kernel rows and "Dynamic activation"; TGLP modules.tex,
/// Definition "Type Identity of a Type, Opaque Instance", and after it: "each
/// takes the list, and refuses one whose length is not the number of D's
/// parameters").  The error is an abort, as a body kernel's is (GLP-Spec
/// appendix-guards: "A body kernel whose precondition fails ... aborts").
/// The caller's scope is the declared tables' of the module the calling goal
/// points to, `P/N` keyed as the compiled module carries the declaration, as
/// for `_find_type`/2.
BodyKernelResult findTypeOpaqueKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] _find_type/3: expected 3 arguments, got ${args.length}');
  }
  final key = _procedureKey(rt, args[0]);
  if (key == null) {
    return kernelAbort('[ABORT] _find_type/3: first argument is not a procedure reference '
        'P/N, got ${_deref(rt, args[0])}');
  }
  final labels = _identities(rt, args[1]);
  if (labels == null) {
    return kernelAbort('[ABORT] _find_type/3: second argument is not a list of type '
        'identities (ground strings), got ${_deref(rt, args[1])}');
  }
  final tables = _callerTables(rt);
  if (tables is! TypeIdentityTables) {
    return kernelAbort('[ABORT] _find_type/3: the calling goal\'s module carries no '
        'declared table, so $key is not declared in the caller\'s scope');
  }
  final String identity;
  try {
    identity = tables.opaqueInstanceIdentity(key, labels);
  } on OpaqueInstanceRefusal catch (e) {
    return kernelAbort('[ABORT] _find_type/3: $e');
  }
  return bindResult(rt, args[2], ConstTerm(identity), kernel: '_find_type/3');
}

/// `_opaque_type(P/N, Module, Hs)`/3, `'_opaque_type'(_?, Module?,
/// List(Constant))` — the list of the type identities of the types at which
/// `Module`'s exports record its entry `P/N` as the instance of a
/// parameterised declaration, in the order of its parameters, and an error
/// where they record none (GLP-Spec appendix-guards, the Modules kernel rows
/// and "Dynamic activation"; TGLP modules.tex, Dynamic Activation, "Opaque
/// types": "The host obtains ι(T̄) from the module value").  The record is
/// derived with the exported table from the artefact's interface text and
/// cached on the module value, as `_run`/3's is.
BodyKernelResult opaqueTypeKernel(GlpRuntime rt, List<Object?> args) {
  if (args.length != 3) {
    return kernelAbort('[ABORT] _opaque_type/3: expected 3 arguments, got ${args.length}');
  }
  final key = _procedureKey(rt, args[0]);
  if (key == null) {
    return kernelAbort('[ABORT] _opaque_type/3: first argument is not a procedure '
        'reference P/N, got ${_deref(rt, args[0])}');
  }
  final module = _deref(rt, args[1]);
  if (module is! ModuleTerm) {
    return kernelAbort('[ABORT] _opaque_type/3: second argument is not a module value');
  }
  final artefact = module.artefact;
  if (artefact is! Artefact) {
    return kernelAbort('[ABORT] _opaque_type/3: module carries no artefact');
  }
  final record = _exportedTables(module, artefact).opaque[key];
  if (record == null) {
    return kernelAbort('[ABORT] _opaque_type/3: the exports of module ${module.name} '
        'record $key as the instance of no parameterised declaration');
  }
  Term list = ConstTerm(nil);
  for (final t in record.types.reversed) {
    list = StructTerm('.', [ConstTerm(t), list]);
  }
  return bindResult(rt, args[2], list, kernel: '_opaque_type/3');
}
