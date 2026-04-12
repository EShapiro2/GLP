# Dynamic Module Dispatch — Test & Debug Instructions

**Date**: 2026-03-12
**Author**: Claude Chat
**Status**: Ready for Claude Code execution

---

## Situation

The dynamic dispatch implementation is COMPLETE in the codebase but UNTESTED end-to-end. All five phases exist in code:

1. **`_select/1` generation** — `_generateSelectProcedure()` in `lib/compiler/compiler.dart`
2. **`_activate` body kernel** — `activateKernel` in `lib/runtime/body_kernels.dart`
3. **`serve/2`** — `_serveSource` embedded in `lib/engine/glp_engine.dart`, compiled at init as `_serveBytecode`
4. **`activateModule()`** — in `lib/runtime/glp_activation.dart`, creates channel, spawns serve, registers in `rt.glpChannels`
5. **Distribute/Transmit handlers** — in `lib/bytecode/runner.dart`, both check `rt.glpChannels` first and route goal terms via `GlpChannelHandle.send()`

The `claude.md` still says "Dynamic RPC and reduce/2 are currently broken." The goal is to validate, test, and fix whatever is broken.

---

## Task

1. Write Dart integration tests that exercise the full dispatch chain
2. Run them — expect some to fail
3. Debug and fix failures
4. Add REPL-level tests
5. Update `claude.md` to reflect the new status

---

## Test GLP files

Already created in `programs/tests/dynamic_dispatch/`:
- `math_service.glp` — 3 exports (double, triple, add_ten)
- `private_only.glp` — 0 exports (no `_select/1` expected)
- `single_export.glp` — 1 export (inc)

---

## Dart tests to write

Create `glp_runtime/test/dynamic_dispatch_test.dart`.

### Test 1: `_select/1` is generated for modules with exports

```dart
test('module with exports has _select/1 label', () {
  final source = File('../../programs/tests/dynamic_dispatch/math_service.glp').readAsStringSync();
  final program = GlpCompiler().compile(source);
  expect(program.labels.containsKey('_select/1'), isTrue);
});
```

### Test 2: `_select/1` is NOT generated for modules without exports

```dart
test('module without exports has no _select/1', () {
  final source = File('../../programs/tests/dynamic_dispatch/private_only.glp').readAsStringSync();
  final program = GlpCompiler().compile(source);
  expect(program.labels.containsKey('_select/1'), isFalse);
});
```

### Test 3: `serve/2` compiles

```dart
test('serve/2 compiles and has label', () {
  final engine = GlpEngine(rootSelfGlpPath: File('../../programs/self.glp').absolute.path);
  expect(engine.serveBytecode.labels.containsKey('serve/2'), isTrue);
});
```

### Test 4: Full dispatch chain — activate module, send goal, verify result

This is the critical end-to-end test. The chain is:
1. Compile `math_service.glp` (should produce bytecode with `_select/1`)
2. Call `activateModule()` with the module bytecode and `serve` bytecode
3. Create a goal term `double(5, F)` where F is a fresh writer
4. Send the goal on the module's channel via `handle.send(goal)`
5. Enqueue woken goals
6. Drain the scheduler
7. Verify F is bound to 10

```dart
test('activate module and dispatch double(5, F) → F = 10', () {
  final engine = GlpEngine(rootSelfGlpPath: File('../../programs/self.glp').absolute.path);
  
  // Compile the module
  final source = File('../../programs/tests/dynamic_dispatch/math_service.glp').readAsStringSync();
  final moduleProg = GlpCompiler().compile(source);
  expect(moduleProg.labels.containsKey('_select/1'), isTrue);
  
  // Activate the module
  final handle = activateModule(
    rt: engine.runtime,
    serveBytecode: engine.serveBytecode,
    moduleBytecode: moduleProg,
    moduleName: 'math_service',
  );
  
  // Create goal: double(5, F)
  final (fWriter, fReader) = engine.runtime.heap.allocateVariable();
  final (fiveW, fiveR) = engine.runtime.heap.allocateVariable();
  engine.runtime.heap.bindVariable(fiveW, ConstTerm(5));
  final goal = StructTerm('double', [VarRef(fiveR), VarRef(fWriter)]);
  
  // Send goal on channel
  final woken = handle.send(goal);
  for (final g in woken) {
    engine.runtime.gq.enqueue(g);
  }
  
  // Drain scheduler
  // Need to set up runners for serve, module, and root self.glp programs
  final combined = engine.combinedProgram;
  final scheduler = Scheduler(
    rt: engine.runtime,
    runners: {
      'main': BytecodeRunner(combined),
      engine.serveBytecode: BytecodeRunner(engine.serveBytecode),
      moduleProg: BytecodeRunner(moduleProg),
    },
  );
  scheduler.drainWithStatus();
  
  // Check result
  final result = engine.runtime.heap.dereference(VarRef(fReader));
  expect(result, isA<ConstTerm>());
  expect((result as ConstTerm).value, equals(10));
});
```

**Note**: The exact Scheduler setup may need adjustment. Look at how existing tests (e.g., `test/engine/glp_engine_test.dart`, `test/multiagent/`) create schedulers. The key requirement: the scheduler must find the correct `BytecodeRunner` for each goal's program key.

### Test 5: Unknown goal handled by fallback

```dart
test('unknown goal does not crash (fallback clause)', () {
  // Same setup as Test 4, but send nonexistent_proc(X) instead
  // Should not crash — the _select/1 otherwise clause handles it silently
});
```

---

## Execution order

1. **Baseline**: Pull main, run `bash test/run_all_tests.sh` — expect 399/399. Commit.
2. **Write tests**: Create `test/dynamic_dispatch_test.dart` with Tests 1–5 above.
3. **Run Tests 1–3**: These test compilation only — should pass immediately.
4. **Run Test 4**: The end-to-end test. If it fails, debug.
5. **Debug cycle**: Read error messages, trace execution, fix issues. Common problems to expect:
   - Scheduler can't find runner for serve or module program
   - `_select/1` bytecode doesn't correctly dispatch (head matching or body call wrong)
   - `_activate` kernel doesn't correctly spawn goal in module's context
   - `serve/2` doesn't correctly read from channel (ground guard on Module fails)
   - Arithmetic body kernels not available in module context (`:=` needs root self.glp)
6. **Once Test 4 passes**: Add REPL-level tests to `test/run_all_tests.sh` Section J.
7. **Update `claude.md`**: Remove the "Dynamic RPC currently broken" note.
8. **Final**: Run full test suite. Commit and push.

---

## Spec references

- Full spec: `docs/type system/dynamic-module-dispatch.md`
- Implementation plan: `docs/modules/dynamic-dispatch-implementation-plan.md`
- `_select/1` (§3.2): one clause per export, fallback with `otherwise`
- `serve/2` (§3.4): reads goals from stream, dispatches via `_activate`
- `_activate` (§3.3): resolves goal against module's `_select/1`
- Complete dispatch chain (§6): caller → channel → serve → _activate → _select → procedure

---

## What NOT to change

- Do NOT rewrite any of the existing implementation unless a test proves it's broken.
- Do NOT modify static linking code (`project_linker.dart`).
- Do NOT modify existing tests or test expectations.
- Do NOT change any `.glp` program files except the test files in `programs/tests/dynamic_dispatch/`.
