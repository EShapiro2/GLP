# Dynamic Module Dispatch — Implementation Instructions

**Date**: 2026-03-11
**Author**: Claude Chat
**Spec**: `docs/type system/dynamic-module-dispatch.md`
**Implementation plan**: `docs/modules/dynamic-dispatch-implementation-plan.md`
**Status**: Ready for Claude Code execution

---

## Overview

Implement dynamic module dispatch: when a GLP program calls `M # goal(...)` and module M is not statically linked, the runtime dynamically loads M, creates a typed channel, spawns a service loop, and routes the goal through the channel. Three components are needed:

1. **`_select/1` generation** — compiler emits a dispatch table for modules with exports
2. **`serve/2` system predicate** — GLP service loop that reads goals from a channel and dispatches via `_activate`
3. **RPC routing** — `M # goal(...)` sends goals on M's channel at runtime

Phases 2 and 4 (the `_activate` body kernel and `activateModule()`) are already implemented.

---

## Existing Infrastructure (already done — do NOT reimplement)

- **`_activate` body kernel** in `lib/runtime/body_kernels.dart` — registered, resolves Goal against a module's `_select/1`
- **`activateModule()`** in `lib/runtime/glp_activation.dart` — creates GLP channel, spawns serve, registers in `rt.glpChannels`
- **`GlpChannelHandle`** in `lib/runtime/glp_activation.dart` — sends goal terms on a GLP stream
- **`ModuleTerm`** in `lib/runtime/terms.dart` — wraps a `BytecodeProgram`, passed to `_activate`
- **`RemoteGoal`** AST node in `lib/compiler/ast.dart` — parser handles `M # goal(...)` syntax

---

## Phase 1: Compiler generates `_select/1`

### Spec reference

`docs/type system/dynamic-module-dispatch.md` §3.2:

> The compiler generates `_select/1` for each module that has exported procedures. There is one clause per exported procedure, plus a fallback clause.
>
> ```glp
> _select(factorial(N, F)) :- factorial(N?, F).
> _select(fibonacci(N, F)) :- fibonacci(N?, F).
> _select(_) :- otherwise | true.
> ```

### What to implement

In `lib/compiler/compiler.dart` (or `codegen.dart`), after compiling a module's procedures:

1. Check if the module has any `exported procedure` declarations (via `module.exportedSignatures`).
2. If yes, generate bytecode for `_select/1` with one clause per exported procedure, plus a fallback.
3. Each clause head-matches on the functor/arity, then calls the procedure with modes swapped (writers in the goal become readers in the call).
4. The fallback clause: `_select(_) :- otherwise | true.`
5. Add the `_select/1` label to the bytecode program's label table.

The simplest approach: construct `_select/1` as GLP source text, then compile it alongside the module's other procedures. For a module exporting `double/2` and `triple/2`:

```glp
procedure _select(_?).
_select(double(A, B)) :- double(A?, B).
_select(triple(A, B)) :- triple(A?, B).
_select(_) :- otherwise | true.
```

Note: arguments in the `_select` head are writers (receiving from the goal term). Arguments in the body call are readers (passing to the actual procedure), except output positions which remain writers. Since the `_select` clause just forwards all arguments, the simplest correct approach is:
- In the head: `_select(p(A1, A2, ..., An))` — all writers
- In the body: `p(A1?, A2?, ..., An?)` — all readers

Wait — this is wrong. The exported procedure's declaration determines which arguments are inputs and outputs. But `_select` doesn't know this at the syntactic level. The simplest correct approach:

- In the head: `_select(p(A1, A2, ..., An))` — all writers (receive from goal)
- In the body: `p(A1?, A2?, ..., An?)` — all readers (pass to procedure)

This works because the body call to `p` uses all readers, and the procedure `p` itself has the correct modes declared. The SRSW is satisfied: each Ai has one writer (head) and one reader (body).

### Test GLP files

Already created in `programs/tests/dynamic_dispatch/`:
- `math_service.glp` — 3 exports (double, triple, add_ten)
- `private_only.glp` — 0 exports (negative test)
- `single_export.glp` — 1 export

### Tests to write

Create `glp_runtime/test/dynamic_dispatch_test.dart`:

```dart
group('Phase 1: _select/1 generation', () {
  test('module with 3 exports has _select/1 label', () {
    final source = File('path/to/math_service.glp').readAsStringSync();
    final compiler = GlpCompiler();
    final program = compiler.compile(source);
    expect(program.labels.containsKey('_select/1'), isTrue);
  });

  test('module with no exports has no _select/1', () {
    final source = File('path/to/private_only.glp').readAsStringSync();
    final compiler = GlpCompiler();
    final program = compiler.compile(source);
    expect(program.labels.containsKey('_select/1'), isFalse);
  });

  test('module with 1 export has _select/1', () {
    final source = File('path/to/single_export.glp').readAsStringSync();
    final compiler = GlpCompiler();
    final program = compiler.compile(source);
    expect(program.labels.containsKey('_select/1'), isTrue);
  });
});
```

Use the GLP test files from `programs/tests/dynamic_dispatch/`. Resolve paths relative to the test file location (typically `../../programs/tests/dynamic_dispatch/`).

---

## Phase 3: `serve/2` system predicate

### Spec reference

`docs/type system/dynamic-module-dispatch.md` §3.4:

> ```glp
> serve(Module, [Goal | In]) :-
>     ground(Module?) |
>     '_activate'(Module?, Goal?),
>     serve(Module?, In?).
>
> serve(_, []) :-
>     otherwise |
>     true.
> ```

### What to implement

Add `serve/2` as an embedded source string in `glp_engine.dart`, similar to how `_madPredicatesSource` works. The source:

```glp
-mode(system).

procedure serve(_?, Stream(_)?).
serve(Module, [Goal | In]) :-
    ground(Module?) |
    '_activate'(Module?, Goal?),
    serve(Module?, In?).
serve(_, []) :-
    otherwise |
    true.
```

This needs `-mode(system)` because it calls `_activate` (a body kernel with underscore-prefixed name).

Load this during engine construction, after loading root self.glp and before registering standard predicates. Or load it lazily when the first dynamic module is activated.

**Important**: `serve/2` must be compiled into its own `BytecodeProgram` so `activateModule()` can pass it as `serveBytecode`. Currently `activateModule()` takes a `serveBytecode` parameter — this source provides it.

### Tests

```dart
group('Phase 3: serve/2', () {
  test('serve/2 compiles without errors', () {
    final serveSource = '''
      -mode(system).
      procedure serve(_?, Stream(_)?).
      serve(Module, [Goal | In]) :-
          ground(Module?) |
          '_activate'(Module?, Goal?),
          serve(Module?, In?).
      serve(_, []) :-
          otherwise |
          true.
    ''';
    final compiler = GlpCompiler();
    final program = compiler.compile(serveSource);
    expect(program.labels.containsKey('serve/2'), isTrue);
  });
});
```

---

## Phase 4+3 integration: activateModule with serve

### What to test

`activateModule()` already exists in `lib/runtime/glp_activation.dart`. It takes `serveBytecode` and `moduleBytecode`. With Phase 1 done (module has `_select/1`) and Phase 3 done (`serve/2` compiles), we can test the full activation + dispatch:

```dart
group('Phase 4: module activation + dispatch', () {
  test('activate module and dispatch goal via channel', () async {
    final engine = GlpEngine(rootSelfGlpPath: '../../programs/self.glp');
    
    // Compile the math module (should now have _select/1)
    final mathSource = File('../../programs/tests/dynamic_dispatch/math_service.glp')
        .readAsStringSync();
    final mathProgram = GlpCompiler().compile(mathSource);
    expect(mathProgram.labels.containsKey('_select/1'), isTrue);
    
    // Compile serve/2
    final serveSource = '...'; // the serve source from Phase 3
    final serveProgram = GlpCompiler().compile(serveSource);
    
    // Activate the module
    final handle = activateModule(
      rt: engine.runtime,
      serveBytecode: serveProgram,
      moduleBytecode: mathProgram,
      moduleName: 'math_service',
    );
    
    // Create a goal: double(5, F)
    // F is a fresh writer — the result will be bound to it
    final (fWriter, fReader) = engine.runtime.heap.allocateVariable();
    final goal = rt.StructTerm('double', [rt.ConstTerm(5), rt.VarRef(fWriter)]);
    
    // Send the goal on the channel
    final woken = handle.send(goal);
    for (final g in woken) {
      engine.runtime.gq.enqueue(g);
    }
    
    // Run the scheduler to execute serve → _activate → _select → double
    final combined = engine.combinedProgram;
    final runner = BytecodeRunner(combined);
    // Need runners for both serve and math module programs
    final scheduler = Scheduler(
      rt: engine.runtime,
      runners: {
        'main': runner,
        serveProgram: BytecodeRunner(serveProgram),
        mathProgram: BytecodeRunner(mathProgram),
      },
    );
    scheduler.drainWithStatus();
    
    // Check that F was bound to 10
    final fValue = engine.runtime.heap.dereference(rt.VarRef(fReader));
    expect(fValue, isA<rt.ConstTerm>());
    expect((fValue as rt.ConstTerm).value, equals(10));
  });
});
```

**Note**: The exact scheduler/runner setup may differ. Check how existing tests (e.g., `test/multiagent/` or `test/engine/`) set up the execution environment. The key requirement: the scheduler must be able to run goals from multiple `BytecodeProgram`s (serve, math module, and root self.glp for arithmetic).

---

## Phase 5: RPC routing (`M # goal(...)`)

### Spec reference

`docs/type system/dynamic-module-dispatch.md` §3.6:

> When a module executes a cross-module call `M # p(X?, Y)`:
> 1. The runtime looks up M's channel in the domain's service directory.
> 2. If M is not yet loaded, the runtime loads and activates it.
> 3. The goal term `p(X?, Y)` is sent on M's channel.

### What to implement

Currently `RemoteGoal` AST nodes (from `M # goal(...)`) are handled by the project linker for static linking. For dynamic dispatch, the compiler/runtime needs a different code path:

**Option A — Runtime RPC**: The codegen emits a special opcode or body kernel call when it encounters a `RemoteGoal` that isn't resolved by the linker. Something like:
```
'_rpc'(ModuleName?, Goal)
```
which at runtime looks up the module's channel and sends the goal.

**Option B — Compile-time channel send**: The codegen translates `M # p(X?, Y)` into code that sends `p(X?, Y)` on M's registered channel.

Option A is simpler and follows FCP more closely. Implement a `_rpc/2` body kernel:

```dart
// '_rpc'(ModuleName?, Goal) — send Goal on module's channel
BodyKernelResult rpcKernel(GlpRuntime rt, List<Object?> args) {
  final moduleNameArg = _deref(rt, args[0]);
  // extract module name string
  final moduleName = (moduleNameArg as ConstTerm).value as String;
  
  // Look up module's channel
  var channel = rt.glpChannels[moduleName];
  if (channel == null) {
    // Module not yet loaded — load and activate it
    // (requires access to file system or pre-registered module sources)
    return BodyKernelResult.abort; // for now
  }
  
  // Send the goal on the channel
  final goal = args[1] as Term;
  final woken = channel.send(goal);
  for (final g in woken) {
    rt.gq.enqueue(g);
  }
  
  return BodyKernelResult.success;
}
```

Then the codegen for `RemoteGoal` (when not statically linked) emits: `'_rpc'(module_name, goal_term)`.

### Tests

This phase is the most complex. For now, test via the Dart API:

```dart
test('RPC via channel to dynamic module', () async {
  // 1. Create engine
  // 2. Compile and activate math_service as a dynamic module
  // 3. Manually invoke _rpc or channel.send with double(5, F)
  // 4. Drain scheduler
  // 5. Verify F = 10
});
```

End-to-end REPL test (add to `test/run_all_tests.sh` Section J):

```bash
# --- J1: Dynamic module dispatch ---
echo "--- J1: Dynamic dispatch (double) ---"
j1=$($DART run "$REPL" <<HEREDOC
$GLP_DIR/programs/tests/dynamic_dispatch/math_service.glp
:activate math_service
math_service # double(5, X).
:quit
HEREDOC
2>&1)
check "Dynamic dispatch double(5)" "X = 10" "$j1"
```

**Note**: The `:activate` REPL command doesn't exist yet. It would be a new command that dynamically activates a loaded module (creates channel + spawns serve). Alternatively, the engine could auto-activate modules with exports when they're loaded via `loadFile`. Discuss this design choice.

---

## Execution Order

1. **Baseline**: Pull main, run `bash test/run_all_tests.sh` — expect 399/399. Commit.
2. **Phase 1**: Implement `_select/1` generation in the compiler. Add Dart tests. Run all tests — must be 399+new passing, 0 regressions.
3. **Phase 3**: Implement `serve/2` as embedded source. Add Dart tests. Run all tests.
4. **Integration test**: Test activation + dispatch end-to-end via Dart API.
5. **Phase 5**: Implement `_rpc/2` body kernel and codegen for `RemoteGoal`. Add tests.
6. **REPL integration**: Add `:activate` command (or auto-activation) to the REPL. Add REPL tests to Section J.
7. **Final**: Run full test suite. Commit and push.

---

## What NOT to change

- Do NOT modify existing static linking code (`project_linker.dart`).
- Do NOT modify existing module test programs or REPL tests.
- Do NOT change the `_activate` body kernel or `activateModule()` — they are done.
- Do NOT change `ModuleTerm` or `GlpChannelHandle` — they are done.

---

## Files to modify

| File | Phase | Change |
|------|-------|--------|
| `lib/compiler/compiler.dart` or `codegen.dart` | 1 | Generate `_select/1` for modules with exports |
| `lib/engine/glp_engine.dart` | 3 | Embed `serve/2` source, compile it at init |
| `lib/runtime/body_kernels.dart` | 5 | Add `_rpc/2` kernel, register it |
| `lib/compiler/codegen.dart` | 5 | Compile `RemoteGoal` to `_rpc` call (when not statically linked) |
| `bin/glp_repl.dart` | 5 | Add `:activate` command (or auto-activation) |
| `test/dynamic_dispatch_test.dart` | all | Dart tests for each phase |
| `test/run_all_tests.sh` | 5 | Section J: dynamic dispatch REPL tests |

---

## Key spec quotes

**`_select/1` (§3.2):** "Each clause matches on the goal term's functor and arity, then calls the corresponding exported procedure. No guards are needed — the dispatch is entirely in the head unification. The fallback clause silently accepts unrecognized goals."

**`serve/2` (§3.4):** "This is a system predicate — it uses the `'_activate'` body kernel and is not written by the programmer. The `ground(Module?)` guard is required by SRSW: `Module` is read twice in the body."

**`_activate` (§3.3):** "The runtime resolves `Goal` against the module's `_select/1` clauses. On match, the corresponding exported procedure is invoked with the goal's arguments."
