# Dynamic Module Dispatch

**Status:** Draft  
**Date:** 2026-02-24  
**Depends on:** `glp-module-system-spec.md`, `glp-predicate-taxonomy.md`, `well-typed-program.md`

---

## 1. Overview

This specification defines how GLP modules are activated and served at runtime: how a compiled module binary becomes a running service that accepts remote procedure calls on a typed channel. The design follows FCP's architecture, adapted to GLP's body kernel mechanism and moded type system.

The architecture has three components:

1. **The dispatch table** — a compiler-generated procedure `_select/2` that maps incoming goals to the module's exported procedures.

2. **The `activate` body kernel** — a runtime primitive that resolves a goal against a compiled module's dispatch table.

3. **The service loop** — a regular GLP procedure that reads goals from a channel, dispatches each via `activate`, and recurses.

Together, these implement the module server pattern: a persistent process that reads a stream of remote procedure calls and dispatches each to the appropriate exported procedure.

---

## 2. FCP Precedent

### 2.1 Architecture

FCP implements module serving through the interaction of three layers:

**Compiler layer** (`control/self.cp`). For each compilation mode (trust, failsafe, interrupt), the compiler generates a `_select/2` procedure with one clause per exported procedure. For trust/failsafe mode:

```
_select(export(p(A,B)), Controls) :-
    Controls = procedures(L, R, V) |
    p(A, B, L, R, V).
```

For interrupt mode, messages arrive as tagged tuples:

```
_select({_, p(A,B)}, Controls) :-
    Controls = procedures(S, L, R, V) |
    p(A, B, S, L, R, V).
```

A final `otherwise` clause calls `_unknown` for unrecognized goals.

**Runtime layer** (`reserved_text.cp`). The `activate/3` primitive resolves a goal against a module binary:

```
activate(Module, Arg1, Arg2) :-
    module(Module) :
        activate(Module, Arg1, Arg2);
    otherwise : Module = _ |
        unify_without_failure(Arg1, []),
        unify_without_failure(Arg2, unknown).
```

The `activate` in the head is a defined FCP procedure. The `activate` in the tell is a kernel operation that resolves `Arg1` against the module's `_select/2`, unifying `Arg2` with the controls tuple. The `module(Module)` ask-guard verifies that `Module` is a valid compiled binary.

**Service layer** (`domain_server.cp`). The `in_server` procedure provides the stream-consuming loop:

```
in_server(In, Module, ServerMode, ServiceId, Domain, Distributor) :-
    In ? Functor(CallInfo, _Scope, Goals, UCC), ... |
        layer_goals(Goals, ..., Module, CO!),
        self.
```

`layer_goals` calls `activate(Module, export(Goals), procedures(...))` to dispatch each goal, and `in_server` recurses on the stream tail via `self`.

### 2.2 Module Activation

When a module is loaded, `activate_module` in `domain_server.cp` performs a test activation to determine the module's kind:

```
activate_module(In, Module, ServiceId, Kind, Domain) :-
    test_activate(Module, export(attributes(As)), Arg2, Ok), ...
```

`test_activate` calls `activate` with `deschedule`, allowing it to inspect the result before committing. The `Arg2` unification reveals the module kind:

- `procedures(_, _, _)` → trust/failsafe module (3-arg controls)
- `procedures(_, _, _, _)` → interrupt module (4-arg controls)
- A monitor name → monitor module (programmer-written server)

The `activated` procedure then selects the appropriate service loop based on the module kind.

---

## 3. GLP Design

### 3.1 Principles

GLP's design follows FCP with these adaptations:

- **Body kernels replace tell kernels.** FCP's `activate` operates in tell position (after the `:`). GLP has no tell kernels, but body kernels serve the same role: they execute promptly after commit.

- **Typed channels replace untyped streams.** The module's input channel carries a typed stream of remote procedure calls. The type is derived from the module's export declarations.

- **The service loop is regular GLP.** No special compilation mode is needed. The loop is a standard recursive procedure reading from a channel with single-reader-single-writer discipline.

- **Load-time type verification.** When a module is loaded dynamically, the loader verifies subtype compatibility between the caller's `imported` declarations and the module's `exported` declarations, using type automata.

### 3.2 The `_select/2` Dispatch Table

The compiler generates `_select/2` for each module that has exported procedures. There is one clause per exported procedure, plus a fallback clause.

For a module exporting `factorial/2` and `fibonacci/2`:

```glp
_select(factorial(N, F), Controls) :-
    Controls = controls(L, R) |
    factorial(N, F, L, R).

_select(fibonacci(N, F), Controls) :-
    Controls = controls(L, R) |
    fibonacci(N, F, L, R).

_select(Goal, Controls) :-
    otherwise |
    Controls = controls(L, R),
    unknown(Goal, L, R).
```

The `Controls` tuple carries the termination circuit: `L` and `R` are the left and right endpoints of the circuit segment allocated to this call. The actual exported procedure is called with its original arguments plus the circuit arguments, exactly as in FCP.

The `_select/2` procedure is not visible to the programmer. It is an internal dispatch mechanism generated by the compiler from the `exported procedure` declarations.

### 3.3 The `activate` Body Kernel

`activate/3` is a body kernel that resolves a goal term against a compiled module binary's `_select/2` table.

**Signature:**

```
'_activate'(Module?, Goal, Controls)
```

**Semantics:**

- `Module?` is a reader referencing a compiled module binary.
- `Goal` is a term representing the remote procedure call (e.g., `factorial(5, F)`).
- `Controls` is unified with the controls tuple returned by `_select/2`.

**Behavior:**

- The runtime resolves `Goal` against the module's `_select/2` clauses.
- On match, the corresponding exported procedure is invoked, and `Controls` is unified with the controls tuple.
- On no match (unknown goal), the `otherwise` clause of `_select/2` handles the error.

**Preconditions:** `Module?` must be bound to a valid compiled module binary. This is ensured by the service loop's guards (the module reference is established at activation time and does not change).

**Access:** Like all body kernels, `activate` is not directly accessible to user programs. It is used by the system-level service loop and by the module activation machinery.

### 3.4 The Module Service Loop

The service loop is a regular GLP procedure that reads from the module's input channel and dispatches each goal. It is the GLP equivalent of FCP's `in_server`.

```glp
serve(Module, [export(Goal, L, R) | In]) :-
    true |
    '_activate'(Module?, Goal, controls(L, R)),
    serve(Module, In?).

serve(Module, []) :-
    true |
    true.
```

This is a system predicate — it uses the `'_activate'` body kernel and is not written by the programmer. The loop reads messages from the input stream, calls `activate` for each, and recurses on the tail.

The message format `export(Goal, L, R)` wraps the remote procedure call with termination circuit endpoints, following FCP's convention. The precise message format may vary depending on the control architecture (whether signals, suspension, and delegation are needed), but the core pattern — read, dispatch, recurse — remains the same.

### 3.5 Module Activation

When a module is loaded, the runtime activates it by:

1. **Loading the binary.** The compiled module binary is loaded from a trusted source.

2. **Probing the module kind.** The runtime performs a test activation against the `attributes` pseudo-goal to determine the module's kind and retrieve its metadata (export list, type automata, etc.).

3. **Creating the input channel.** A new channel is created for the module's input stream.

4. **Starting the service loop.** For procedure modules (those with exported procedures but no programmer-written monitor), the runtime spawns `serve(Module, In?)` where `In` is the read end of the input channel.

5. **Registering the channel.** The module's channel is registered in the domain's service directory, keyed by the module's path. Subsequent remote procedure calls to this module are routed to this channel.

For monitor modules (those with a `-monitor(Name)` declaration), step 4 differs: the runtime calls the programmer-written monitor procedure instead of the generic `serve` loop, following FCP's `_boot/2` pattern.

### 3.6 Remote Procedure Call Routing

When a module executes a cross-module call `M # p(X?, Y)`:

1. The runtime looks up `M`'s channel in the domain's service directory.
2. If `M` is not yet loaded, the runtime loads and activates it (Section 3.5).
3. The call is wrapped as `export(p(X?, Y), L, R)` and sent on `M`'s channel.
4. `M`'s service loop reads the message and dispatches via `_select/2`.
5. The exported procedure `p` executes within `M`'s context.

The termination circuit arguments `L` and `R` connect the caller's and callee's circuits, enabling the caller to detect completion or failure.

---

## 4. Typing the Dispatch

### 4.1 The Module Input Type

Each module's input stream has a type derived from its `exported procedure` declarations. For a module exporting:

```glp
exported procedure factorial(Integer?, Integer).
exported procedure fibonacci(Integer?, Integer).
```

The input stream type is:

```glp
ModuleIn ::= [export(ExportGoal, Circuit, Circuit) | ModuleIn] ; [].
ExportGoal ::= factorial(Integer, Integer) ; fibonacci(Integer, Integer).
```

The `ExportGoal` type is the disjunction of all exported procedure call patterns, with modes inverted at the module boundary: the caller's writer arguments become the callee's reader arguments and vice versa.

### 4.2 Type Checking Cross-Module Calls

When a caller invokes `M # p(X?, Y)`, the type checker:

1. Finds the caller's `imported procedure M#p(...)` declaration.
2. Type-checks the call arguments against the imported declaration's types, following the standard well-typing rules (Definition 5.7).

No access to module `M` is needed. The `imported` declaration provides all necessary type information locally.

### 4.3 Load-Time Compatibility Verification

When a module is loaded dynamically, the loader verifies that the module's actual exports are subtype-compatible with every caller's `imported` declarations.

For each imported procedure:

- The module's `exported` declaration must accept at least the inputs the caller may send (contravariance on reader arguments).
- The module's `exported` declaration must produce at most the outputs the caller expects (covariance on writer arguments).

This is the subtyping relation on procedure types induced by the moded type system (Definition 5.10 of the types paper). Type automata are compared structurally; the check is decidable and efficient.

### 4.4 Type Automata as Runtime Artifacts

For dynamic loading, compiled modules carry their type automata alongside their code. Specifically, each compiled module includes:

- The `exported` procedure declarations with their full type signatures.
- The type automata for all types transitively referenced by exported declarations.
- The `imported` procedure declarations, recording the module's own expectations of its dependencies.

This enables the load-time compatibility check without requiring source code or recompilation.

---

## 5. Monitor Modules

### 5.1 Programmer-Written Servers

A monitor module declares a monitor procedure that replaces the generic service loop:

```glp
-monitor(server).

exported procedure lookup(Key?, Value).
exported procedure store(Key?, Value?).

server([lookup(Key, Value) | In], State) :-
    ... |
    serve_lookup(Key?, Value, State, State1),
    server(In?, State1?).

server([store(Key, Value) | In], State) :-
    ... |
    serve_store(Key?, Value?, State, State1),
    server(In?, State1?).

server([], State) :-
    true |
    true.
```

The monitor procedure is the programmer's explicit stream-consuming loop. It reads from the input stream, dispatches messages, maintains state, and recurses. The compiler generates `_boot/2` which calls the monitor procedure:

```glp
_boot(In, Attributes) :-
    true |
    Attributes = [monitor, ...],
    server(In?).
```

### 5.2 Monitor vs. Procedure Modules

The distinction parallels FCP:

- **Procedure modules** have exported procedures but no monitor. The runtime provides the generic `serve` loop. The module is stateless — each call is independent.

- **Monitor modules** have a programmer-written server that maintains state across calls. The programmer controls the loop, the dispatch, and the state threading.

Both kinds are activated through `_boot/2`. The difference is whether `_boot` calls the generic `serve` or the programmer's monitor procedure.

### 5.3 Typing Monitor Input

For monitor modules, the input stream type is derived from the exported procedures, but the monitor procedure's clauses must provide coverage for all message types. The type checker verifies:

1. The monitor procedure's first argument type matches the module's input stream type.
2. Every exported goal pattern is covered by some clause of the monitor procedure.

---

## 6. The Complete Dispatch Chain

Tracing a remote procedure call from caller to callee:

1. **Caller** executes `M # factorial(5, F)`.

2. **RPC routing.** The runtime looks up `M`'s channel and sends `export(factorial(5, F), L, R)` on it.

3. **Service loop.** `M`'s `serve` (or programmer-written monitor) reads `export(factorial(5, F), L, R)` from the input stream.

4. **Dispatch.** The loop calls `'_activate'(Module?, factorial(5, F), controls(L, R))`.

5. **`_select` resolution.** The runtime resolves `factorial(5, F)` against the compiled `_select/2` table:

   ```glp
   _select(factorial(N, F), Controls) :-
       Controls = controls(L, R) |
       factorial(N, F, L, R).
   ```

6. **Procedure execution.** `factorial(5, F, L, R)` executes within the module's context, with `L` and `R` connecting to the caller's termination circuit.

7. **Recursion.** The service loop recurses on the tail of the input stream, ready for the next call.

---

## 7. Relationship to Existing Specifications

This specification extends the module system spec (`glp-module-system-spec.md`) with runtime dispatch mechanisms. The module system spec defines the language-level semantics — syntax, scoping, type checking, compatibility — and explicitly notes (Section 7, Design Principle 7) that implementation mechanisms are orthogonal. This specification fills in that orthogonal dimension.

The body kernel `'_activate'` joins the existing body kernel registry defined in `glp-predicate-taxonomy.md`, alongside arithmetic, structure, and mutual reference operations.

The `_select/2` dispatch table is a compiler artifact, generated from `exported procedure` declarations during compilation, following the pattern established in `glp-compiler-spec.md`.

Load-time type verification uses the subtyping relation defined in `subtyping.md` and the type automata representation defined in `type-automaton.md`.

---

## 8. Open Questions

1. **Controls tuple design.** The exact contents of the `controls(...)` tuple depend on the control architecture: whether GLP needs FCP's full signal/suspension/delegation machinery or a simpler termination circuit suffices. The current spec assumes the minimal form `controls(L, R)`.

2. **Message format.** The `export(Goal, L, R)` wrapper follows FCP's convention. GLP may adopt a simpler format if the full FCP control protocol (with `CallInfo`, `Scope`, and `UCC`) is not needed.

3. **Module identity and versioning.** How compiled module binaries are identified, versioned, and matched to import declarations at load time. The module system spec notes this as an open question (Section 11.1).

4. **Hot reloading.** Whether a running module can be replaced with a new version while maintaining active connections. FCP's `filter` mechanism supports this; GLP may need an equivalent.

---

*Version 1.0 — 2026-02-24*
