# Dynamic Module Dispatch — REPL Integration Instructions

**Date**: 2026-03-12
**Author**: Claude Chat
**Prerequisite**: Dart integration tests pass (8/8), 424 REPL tests pass
**Status**: Ready for Claude Code execution

---

## What's Done

- `_select/1` generation in compiler ✅
- `_activate` body kernel (fixed: direct dispatch, bypasses _select) ✅
- `serve/2` embedded source, compiled at engine init ✅
- `activateModule()` in glp_activation.dart ✅
- Distribute/Transmit handlers in runner.dart ✅
- 8 Dart integration tests in `test/dynamic_dispatch_test.dart` ✅

## What Remains

1. `GlpEngine.activateDynamicModule(name)` — public method to activate a loaded module
2. `:activate <name>` REPL command
3. REPL tests in `test/run_all_tests.sh` Section J

---

## Step 1: Add `activateDynamicModule` to GlpEngine

In `lib/engine/glp_engine.dart`, add a public method:

```dart
/// Activate a loaded module for dynamic dispatch.
///
/// Creates a GLP channel, spawns serve(Module, ChannelReader?),
/// and registers the channel in rt.glpChannels.
/// The module must have been loaded via loadFile() or loadSource() first.
/// The module must have exported procedures (_select/1 in its bytecode).
///
/// After activation, cross-module calls via Distribute/Transmit opcodes
/// route goals through the module's channel.
void activateDynamicModule(String moduleName) {
  // Find the module's bytecode
  final moduleInfo = _loadedModules[moduleName];
  if (moduleInfo == null) {
    throw Exception('Module "$moduleName" not loaded');
  }
  final moduleProg = moduleInfo.program;
  
  // Verify it has _select/1 (i.e., has exports)
  if (!moduleProg.labels.containsKey('_select/1')) {
    throw Exception('Module "$moduleName" has no exported procedures');
  }
  
  // Activate via the existing glp_activation.dart function
  activateModule(
    rt: _runtime,
    serveBytecode: _serveBytecode,
    moduleBytecode: moduleProg,
    moduleName: moduleName,
  );
}
```

This method uses the existing `activateModule()` from `glp_activation.dart` (already imported).

---

## Step 2: Add `:activate` REPL command

In `bin/glp_repl.dart`, add a handler for `:activate <name>` after the existing `:limit` handler:

```dart
if (trimmed.startsWith(':activate')) {
  final parts = trimmed.split(RegExp(r'\s+'));
  if (parts.length != 2) {
    print('Usage: :activate <module_name>');
    continue;
  }
  final moduleName = parts[1];
  try {
    engine.activateDynamicModule(moduleName);
    print('✓ Activated module: $moduleName');
  } catch (e) {
    print('Error activating $moduleName: $e');
  }
  continue;
}
```

Also update `_printHelp()` to include the new command.

---

## Step 3: Add REPL tests (Section J)

Add Section J to `test/run_all_tests.sh`, after Section I and before the SUMMARY.

Test GLP files are in `programs/tests/dynamic_dispatch/`:
- `math_service.glp` — module with 3 exports
- `dispatch_client.glp` — client that calls math_service via `M # goal(...)`

The test flow:
1. Load math_service.glp
2. `:activate math_service`
3. Load dispatch_client.glp 
4. Run `test_double(5, X).` — should bind X = 10
5. Run `test_triple(4, X).` — should bind X = 12

```bash
# =============================================================================
# Section J: Dynamic Module Dispatch Tests
# =============================================================================
echo "=== Section J: Dynamic Module Dispatch Tests ==="
echo ""

DD="$GLP_DIR/programs/tests/dynamic_dispatch"

# --- J1: Activate module and dispatch via client ---
echo "--- J1: Dynamic dispatch via M # goal ---"
j1=$($DART run "$REPL" <<HEREDOC
$DD/math_service.glp
:activate math_service
$DD/dispatch_client.glp
test_double(5, X).
:quit
HEREDOC
2>&1)

check "math_service activated" "Activated module" "$j1"
check "test_double(5, X) = 10" "X = 10" "$j1"

# --- J2: Triple dispatch ---
echo "--- J2: Dynamic dispatch triple ---"
j2=$($DART run "$REPL" <<HEREDOC
$DD/math_service.glp
:activate math_service
$DD/dispatch_client.glp
test_triple(4, X).
:quit
HEREDOC
2>&1)

check "test_triple(4, X) = 12" "X = 12" "$j2"

# --- J3: Add_ten dispatch ---
echo "--- J3: Dynamic dispatch add_ten ---"
j3=$($DART run "$REPL" <<HEREDOC
$DD/math_service.glp
:activate math_service
$DD/dispatch_client.glp
test_add_ten(7, X).
:quit
HEREDOC
2>&1)

check "test_add_ten(7, X) = 17" "X = 17" "$j3"

echo ""
```

**Important**: The REPL snapshot will need recompilation after modifying `glp_repl.dart` and `glp_engine.dart`. Delete `.dart_tool/repl.dill` before running tests, or the test script handles this automatically if `.dart` files are newer.

---

## Execution order

1. Add `activateDynamicModule` to `glp_engine.dart`
2. Add `:activate` command to `glp_repl.dart`
3. Test manually: load math_service, :activate, load dispatch_client, run test_double(5, X)
4. If it works, add Section J to `test/run_all_tests.sh`
5. Run full test suite
6. Update `current_plan.md` — mark complete
7. Commit and push

---

## Debugging hints

If `test_double(5, X)` fails after activation:
- The Distribute opcode in `dispatch_client`'s bytecode routes to `rt.glpChannels['math_service']`
- Check that activation registered the channel: `rt.glpChannels` should contain `'math_service'`
- The Scheduler needs runners for both serve and module bytecode programs
- The engine's `_runSingleGoal` creates a Scheduler with `combinedProgram` — this might not include serve/module runners. May need to register them in `_runtime.runners` so the scheduler finds them.

The most likely issue: the Scheduler in `_runSingleGoal` only knows about the `'main'` runner (combinedProgram). The serve and module bytecodes need their own runners registered in `_runtime.runners`. `activateModule()` in `glp_activation.dart` already does `rt.runners[serveBytecode] = BytecodeRunner(serveBytecode)`, but the module bytecode runner may also need registration.
