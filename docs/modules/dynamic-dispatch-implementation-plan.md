# Dynamic Module Dispatch — Implementation Plan

**Date:** 2026-02-24  
**Spec:** `docs/type system/dynamic-module-dispatch.md`  
**Depends on:** Existing module infrastructure (Phase 1–3 of `module-implementation-plan.md`)

---

## 1. Goal

Implement the runtime dispatch architecture specified in `dynamic-module-dispatch.md`: the compiler generates a `_select/2` dispatch table from exported procedures, the `'_activate'` body kernel resolves goals against a module's dispatch table, and a GLP system predicate `serve/2` provides the stream-consuming loop. This replaces the current Dart-level `Dispatcher` with GLP-level dispatch, following FCP's architecture.

---

## 2. Existing Infrastructure

The following Dart infrastructure exists and will be progressively replaced or adapted:

**Keep and adapt:**

- `LoadedModule` — holds bytecode, exports, input stream. Needs: store compiled `_select/2` entry point.
- `BodyKernelRegistry` — registration infrastructure for body kernels. Needs: register `'_activate'`.
- `ImportVector` / `ServeImport` — import-side stream distribution. Keep as-is.
- `ModuleRuntime` — boot orchestration. Adapt: spawn GLP `serve` instead of Dart `Dispatcher`.
- `ModuleMessages` / `ExportMessage` — message format. Adapt: messages become GLP terms on a GLP channel, not Dart objects on a Dart stream.

**Replace:**

- `Dispatcher` — currently reads from a Dart `StreamController`, checks exports in Dart, and calls a Dart executor callback. Replaced by the GLP `serve/2` system predicate calling `'_activate'`.
- Dart `StreamController<ExportMessage>` in `LoadedModule` — replaced by a GLP channel (writer/reader pair) carrying GLP terms.

**Existing AST support (already done):**

- `RemoteGoal` AST node for `M # Goal` syntax.
- `Module.exportedSignatures` derives the export set from `ProcDecl` nodes with `exported = true`.
- `ProcDecl` carries the full moded type signature of each procedure.

---

## 3. Phases

### Phase 1: Compiler generates `_select/2`

**What:** After compiling a module's procedures, the compiler emits an additional procedure `_select/2` with one clause per exported procedure, plus an `otherwise` fallback.

**Where:** `lib/compiler/codegen.dart`, new function invoked at the end of module compilation.

**Input:** The module's list of `ProcDecl` nodes where `exported == true`.

**Output:** Bytecode for `_select/1` appended to the module's procedure table. For each exported procedure `p/n`:

```
_select(p(A1, ..., An)) :-
    true |
    p(A1, ..., An).
```

The `otherwise` fallback clause:

```
_select(_) :-
    otherwise |
    true.
```

Each clause matches on the goal term's functor and arity, then calls the corresponding exported procedure with its original arguments.

**Detail — `_select` is internal:** The `_select/1` procedure is not declared in the source. It is purely compiler-generated. It does not appear in the module's export list. The `'_activate'` body kernel knows to look for it by convention.

**Testing:** Compile a module with exported procedures, verify `_select/1` exists in the bytecode, verify it has the correct number of clauses (one per export plus one fallback).

**Files changed:**
- `lib/compiler/codegen.dart` — new `_generateSelectTable()` method
- `lib/compiler/compiler.dart` — invoke select table generation for modules with exports

### Phase 2: `'_activate'` body kernel

**What:** A new body kernel that resolves a goal term against a compiled module's `_select/1` table.

**Signature:** `'_activate'(Module?, Goal)`

**Semantics:**
1. Dereference `Module?` to obtain the module reference (a `LoadedModule` or equivalent runtime handle).
2. Look up the `_select/1` entry point in the module's compiled bytecode.
3. Spawn a goal `_select(Goal)` in the module's execution context.
4. The goal executes within the module's procedure table, resolving against the generated `_select/1` clauses.

**Where:** `lib/runtime/body_kernels.dart`, registered in `registerStandardBodyKernels`.

**Detail — module references:** A module reference is a runtime value (a heap term) that the GLP program can hold in a variable. The runtime must support a new term type or a tagged constant that refers to a `LoadedModule`. This is analogous to FCP's `module(Module)` guard — GLP needs a way to represent and pass around module handles.

**Detail — execution context:** The spawned `_select` goal runs against the *target module's* procedure table, not the caller's. The runtime must set the execution context (procedure lookup, import vector) to the target module when executing the goal.

**Testing:** Create a module with exports, obtain its handle, call `'_activate'` with a valid goal, verify the correct exported procedure runs. Call with an unknown goal, verify the fallback clause handles it.

**Files changed:**
- `lib/runtime/body_kernels.dart` — new `activateKernel` function and registration
- `lib/runtime/terms.dart` — module reference term type (if needed)
- `lib/runtime/runtime.dart` — support for switching execution context to target module

### Phase 3: `serve/2` system predicate

**What:** A GLP system predicate that provides the stream-consuming loop for procedure modules.

**Where:** New file `glp/system/serve.glp`, compiled and shipped with the runtime alongside `assign.glp`.

**Definition:**

```glp
-mode(system).

procedure serve(_, Stream?).

serve(Module, [Goal | In]) :-
    true |
    '_activate'(Module?, Goal),
    serve(Module, In?).

serve(_, []) :-
    true |
    true.
```

`serve/2` is a system predicate because it calls the `'_activate'` body kernel, which is not accessible to user programs.

**Detail — message format:** Each message on the module's input stream is a goal term (e.g., `factorial(5, F)`) — the remote procedure call sent directly, with no wrapper.

**Detail — the Module argument:** The first argument is threaded unchanged through all recursive calls. It is the module handle established at activation time.

**Testing:** Compile `serve.glp`, verify it loads. Send messages on a module's channel, verify each is dispatched. Verify the loop terminates when the stream closes.

**Files changed:**
- `glp/system/serve.glp` — new file
- `lib/runtime/system_predicates.dart` — register `serve.glp` in system predicate loading
- `lib/compiler/compiler.dart` — ensure `serve.glp` is compiled with system mode (body kernel access)

### Phase 4: Module activation via GLP

**What:** When a module is loaded, the runtime activates it by creating a GLP channel and spawning `serve(Module, In?)` on the read end. This replaces the Dart `Dispatcher`.

**Where:** `lib/runtime/module_runtime.dart`.

**Activation sequence:**
1. Load and compile the module binary.
2. Create a GLP channel: `new_channel(Ch)`, yielding a writer/reader pair.
3. Obtain the module handle (a runtime reference to the compiled module).
4. Spawn: `serve(ModuleHandle, ChannelReader?)`.
5. Register the channel writer in the module registry, keyed by module path.
6. For monitor modules (`-monitor(Name)`), spawn the programmer-written monitor procedure instead of `serve`.

**Detail — replacing Dispatcher:** The current `Dispatcher` class in `lib/runtime/dispatcher.dart` is no longer used for modules that have been activated via this mechanism. It may be retained temporarily for backward compatibility or removed.

**Detail — monitor modules:** For a module declaring `-monitor(server)`, the compiler generates `_boot/2` which calls `server(In?)`. The activation sequence spawns `_boot(ChannelReader, Attributes)` instead of `serve(Module, ChannelReader?)`.

**Testing:** Load a module, verify `serve` is spawned, send an RPC, verify it executes. Load a monitor module, verify the monitor procedure is spawned instead.

**Files changed:**
- `lib/runtime/module_runtime.dart` — new activation using GLP channel + `serve`
- `lib/runtime/module_registry.dart` — store GLP channel writers instead of Dart stream sinks
- `lib/runtime/dispatcher.dart` — deprecate or remove

### Phase 5: RPC routing via GLP channels

**What:** When a module executes `M # goal(...)`, the runtime wraps the goal as `export(goal(...), L, R)` and sends it on `M`'s GLP channel, rather than through Dart streams.

**Where:** `lib/compiler/codegen.dart` (RPC compilation), `lib/runtime/runtime.dart` (RPC execution).

**RPC compilation:** Currently `RemoteGoal` nodes are compiled to Dart-level dispatch. Change to:
1. Build the goal term from the `RemoteGoal`'s inner goal.
2. Send the goal term on the target module's channel writer: `send(GoalTerm, ChannelWriter)`.

**RPC resolution:** When the target module is not yet loaded, the runtime loads and activates it (Phase 4) before sending the message. The channel writer is obtained from the module registry.

**Detail — the send:** This uses GLP's existing `send` defined guard (in body position) to write to the channel. The message flows on a standard GLP stream, consumed by the target module's `serve` loop.

**Testing:** Cross-module call `M # factorial(5, F)`, verify the message traverses the GLP channel, verify `serve` dispatches it, verify the result is unified back to the caller.

**Files changed:**
- `lib/compiler/codegen.dart` — `RemoteGoal` compilation emits channel send
- `lib/runtime/runtime.dart` — RPC execution uses GLP channel send
- `lib/runtime/module_runtime.dart` — lazy loading on first RPC

---

## 4. Dependency Order

```
Phase 1 (compiler: _select/2)
    ↓
Phase 2 (body kernel: _activate)
    ↓
Phase 3 (system predicate: serve/2)
    ↓
Phase 4 (module activation via GLP)
    ↓
Phase 5 (RPC routing via GLP channels)
```

Each phase can be tested independently. Phase 1 is pure compiler work. Phase 2 is runtime kernel work. Phase 3 is a small GLP program. Phases 4 and 5 integrate everything and replace the Dart-level dispatch.

---

## 5. Testing Strategy

Each phase maintains zero regressions on existing tests while adding new tests.

**Phase 1 tests:** Compile modules with exports, inspect bytecode for `_select/2`, verify clause count and structure.

**Phase 2 tests:** Unit test `'_activate'` with hand-constructed modules, verify dispatch to correct procedure, verify unknown goal handling.

**Phase 3 tests:** Compile and run `serve.glp`, send messages on a test channel, verify dispatch and recursion.

**Phase 4 tests:** End-to-end module loading and activation, verify `serve` is running, send RPCs and verify execution.

**Phase 5 tests:** Cross-module call scenarios, including the CSSG application restructured into modules. Verify type-checked cross-module calls execute correctly at runtime.

---

## 6. Open Decisions

**Module reference term.** How is a module handle represented on the GLP heap? Options: (a) a new cell type `ModuleRef`, (b) a tagged constant wrapping a Dart object, (c) an opaque integer index into a runtime table. Decision needed before Phase 2.

**Backward compatibility.** Phases 1–3 are purely additive — they add new bytecode, a new body kernel, and a new system predicate without changing any existing behavior. Single-file programs and existing module tests are unaffected. Phases 4–5 introduce the new GLP-level dispatch alongside the existing Dart-level dispatch (`Dispatcher`, `StreamController`, `ExportMessage`), behind a flag. The Dart path remains the default. The GLP path must pass all existing module tests before becoming the default. Only after validation is the Dart path deprecated and removed.

---

*Version 1.0 — 2026-02-24*
