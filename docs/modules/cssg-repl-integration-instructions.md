# CSSG Modules: Run All Plays via REPL

**Date:** 2026-02-23
**Branch:** `claude/module-phase1`
**Discipline:** `docs/DISCIPLINE.md`

---

## Goal

Run all 7 CSSG plays from `programs/cssg_modules/` through the REPL, matching
the original `programs/typed_book/cssg/` functionality.

Two things are needed:
1. **Wire hierarchy into GlpEngine** — so `agent.glp`, `ui/mediator.glp`, and `ui/actors.glp` can see types from `self.glp` during type checking
2. **Add `fplay1`–`fplay7`** to `boot.glp` — tagged output variants for observable testing
3. **Test all plays** — both silent (play1-7) and tagged (fplay1-7)

---

## Step 1: Read relevant code

```
glp_runtime/lib/engine/glp_engine.dart        — loadFile, loadSource (the pipeline)
glp_runtime/lib/runtime/module_hierarchy.dart  — discoverSelfChain, assembleTypeScope
glp_runtime/lib/analysis/type_checker/type_checker.dart — checkModule, ancestorScope param
glp_runtime/lib/analysis/type_checker/type_environment_builder.dart — buildTypeEnvironment
programs/cssg_modules/boot.glp                 — current plays (silent only)
programs/typed_book/cssg/play_ui_sim_boot.glp  — original plays (silent + tagged)
docs/DISCIPLINE.md
```

---

## Step 2: Wire hierarchy into GlpEngine.loadSource

Currently `loadSource` type-checks each file in isolation. Modify it so that when a file
has a `self.glp` ancestor chain, that chain provides the ancestor type scope.

### In `loadFile(String path)`:

After reading the file, before calling `loadSource`, discover the self.glp chain:

```dart
import 'package:glp_runtime/runtime/module_hierarchy.dart';
```

In `loadFile`:
```dart
bool loadFile(String path) {
    final file = File(path);
    if (!file.existsSync()) {
      throw FileSystemException('File not found', path);
    }
    final source = file.readAsStringSync();
    return loadSource(source, filename: path);
}
```

### In `loadSource(String source, {String? filename})`:

When `filename` is an actual file path (not `_source_`), discover the hierarchy:

```dart
// After parsing the module, before type checking:
TypeEnvironment? ancestorScope;
if (filename != null && filename != '_source_' && File(filename).existsSync()) {
  final rootDir = _findProjectRoot(filename);
  if (rootDir != null) {
    final chain = discoverSelfChain(targetFile: filename, rootDir: rootDir);
    if (chain.isNotEmpty) {
      ancestorScope = _buildAncestorScope(chain);
    }
  }
}

// Then pass ancestorScope to checkModule:
final typeResult = checkModule(module,
    transformedProcedures: transformedAst.procedures,
    ancestorScope: ancestorScope);
```

### Helper: `_findProjectRoot`

Walk up from the file's directory to find the topmost directory containing `self.glp`:

```dart
String? _findProjectRoot(String filePath) {
  var dir = File(filePath).parent;
  String? root;
  while (true) {
    final selfGlp = File('${dir.path}/self.glp');
    if (selfGlp.existsSync()) {
      root = dir.path;
    }
    final parent = dir.parent;
    if (parent.path == dir.path) break; // filesystem root
    dir = parent;
  }
  return root;
}
```

### Helper: `_buildAncestorScope`

Build prelude + chain scope (WITHOUT the target module — `checkModule` adds that):

```dart
TypeEnvironment _buildAncestorScope(List<String> chain) {
  var env = buildPreludeEnvironment();
  for (final selfGlpPath in chain) {
    final source = File(selfGlpPath).readAsStringSync();
    final lexer = Lexer(source);
    final tokens = lexer.tokenize();
    final parser = Parser(tokens);
    final selfModule = parser.parseModule();

    final types = <String, TypeDef>{};
    for (final t in selfModule.typeDefs) {
      types[t.name] = t;
    }
    final procs = <String, ProcDecl>{};
    for (final p in selfModule.procDeclarations) {
      procs[p.qualifiedKey] = p;
    }
    env = env.merge(TypeEnvironment(types, procs));
  }
  return env;
}
```

You'll need the right imports — check what `TypeEnvironment`, `TypeDef`, `ProcDecl`, `buildPreludeEnvironment` come from. They should be in:
- `package:glp_runtime/analysis/type_checker/type_ast.dart`
- `package:glp_runtime/analysis/type_checker/type_environment_builder.dart`

### Verify `checkModule` accepts `ancestorScope`

Read `checkModule` in `type_checker.dart`. It already has an `ancestorScope` parameter (added in Phase 2). Confirm it passes through to `buildTypeEnvironment`. If the parameter doesn't exist yet, you'll need to thread it through — read the code and follow the pattern.

---

## Step 3: Add fplay1–fplay7 to boot.glp

The original `play_ui_sim_boot.glp` has both `play1`–`play7` (silent, with sink) and
`fplay1`–`fplay7` (tagged output via `send_to_user_tagged`).

Currently `programs/cssg_modules/boot.glp` only has the silent variants.

Add to `boot.glp`:

### 3a. Add `send_to_user_tagged` (untyped, same as original)

After the `sink` definition, add:

```prolog
send_to_user_tagged(Id, [T|Cmds], Notifies) :-
    ground(Id?), ground(T?) |
    '_output'(tagged(Id?, cmd(T?))),
    send_to_user_tagged(Id?, Cmds?, Notifies?).
send_to_user_tagged(Id, Cmds, [T|Notifies]) :-
    ground(Id?), ground(T?) |
    '_output'(tagged(Id?, notify(T?))),
    send_to_user_tagged(Id?, Cmds?, Notifies?).
send_to_user_tagged(_, [], []).
send_to_user_tagged(_, Cmds, []) :- sink(Cmds?).
```

### 3b. Add fplay1–fplay7

Copy each `playN` clause and create `fplayN` that replaces `sink(XDispCmd?), sink(XDispNotify?)` with `send_to_user_tagged(name, XDispCmd?, XDispNotify?)`.

The pattern for fplay1 (from original `play_ui_sim_boot.glp`): identical to play1 but
replace the two `sink` lines for each agent with one `send_to_user_tagged` line.

Do this for all 7 plays. Refer to `programs/typed_book/cssg/play_ui_sim_boot.glp` for the exact fplay code — it's a mechanical transformation.

---

## Step 4: Handle `#` dispatch

`boot.glp` uses `agent # agent(alice, ...)` syntax. At runtime, this requires the bytecode
runner to resolve RemoteGoal nodes. Two scenarios:

### If `#` dispatch works at runtime:
Great — proceed to testing.

### If `#` dispatch does NOT work (e.g., "predicate not found"):
Create `programs/cssg_modules/boot_direct.glp` — identical to boot.glp but with direct calls:
- `agent # agent(alice, ...)` → `agent(alice, ...)`
- `mediator # ui_mediator(alice, ...)` → `ui_mediator(alice, ...)`
- `actors # alice1(...)` → `alice1(...)`

Remove all `imported procedure` declarations from boot_direct.glp (not needed for direct calls).
Use boot_direct.glp for the tests while noting that `#` dispatch needs runtime work.

---

## Step 5: Create REPL test script

Create `test/cssg_modules_test.sh`:

```bash
#!/bin/bash
# CSSG Modules — REPL integration test
# Loads modular CSSG and runs all plays
# Matches original programs/typed_book/cssg/ functionality

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GLP_DIR="$SCRIPT_DIR/.."
CSSG="$GLP_DIR/programs/cssg_modules"
cd "$GLP_DIR/glp_runtime"

DART=${DART:-$(which dart 2>/dev/null || echo "dart")}
REPL="bin/glp_repl.dart"

PASS=0
FAIL=0

check() {
    local name="$1" pattern="$2" source="$3"
    if echo "$source" | grep -q "$pattern"; then
        echo "  PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "  FAIL: $name (expected: $pattern)"
        FAIL=$((FAIL + 1))
    fi
}

check_not() {
    local name="$1" pattern="$2" source="$3"
    if echo "$source" | grep -q "$pattern"; then
        echo "  FAIL: $name (should NOT match: $pattern)"
        FAIL=$((FAIL + 1))
    else
        echo "  PASS: $name"
        PASS=$((PASS + 1))
    fi
}

echo "============================================"
echo "   CSSG Modules — Play Tests                "
echo "============================================"
echo ""

# Determine which boot file to use
BOOT="$CSSG/boot.glp"
if [ -f "$CSSG/boot_direct.glp" ]; then
    BOOT="$CSSG/boot_direct.glp"
    echo "Using boot_direct.glp (direct calls, no # dispatch)"
fi

# -----------------------------------------------
# Test 1: Loading all modules succeeds
# -----------------------------------------------
echo "--- Loading modules ---"
load_result=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
:quit
HEREDOC
2>&1)

check_not "No type errors on load" "Type checking failed" "$load_result"
check_not "No load errors" "Error loading" "$load_result"

# -----------------------------------------------
# Test 2: Silent plays (play1–play7) succeed
# -----------------------------------------------
echo ""
echo "--- Silent plays (play1-play7) ---"
for play_num in 1 2 3 4 5 6 7; do
    result=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
play${play_num}.
:quit
HEREDOC
2>&1)

    if echo "$result" | grep -q "Type checking failed\|Error loading"; then
        echo "  FAIL: play${play_num} — load error"
        FAIL=$((FAIL + 1))
    else
        check "play${play_num} succeeds" "succeeds" "$result"
    fi
done

# -----------------------------------------------
# Test 3: Tagged plays (fplay1–fplay7) produce output
# -----------------------------------------------
echo ""
echo "--- Tagged plays (fplay1-fplay7) ---"

# fplay1: Both accept intro → Alice and Charlie become friends, exchange messages
fp1=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay1.
:quit
HEREDOC
2>&1)

check "fplay1 succeeds" "succeeds" "$fp1"
check "fplay1 alice connected bob" "tagged(alice.*connected(bob)" "$fp1"
check "fplay1 charlie connected alice" "tagged(charlie.*connected(alice)" "$fp1"

# fplay2: Alice accepts intro, Charlie rejects
fp2=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay2.
:quit
HEREDOC
2>&1)

check "fplay2 succeeds" "succeeds" "$fp2"
check "fplay2 alice rejected" "tagged(alice.*rejected" "$fp2"

# fplay3: Both reject intro
fp3=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay3.
:quit
HEREDOC
2>&1)

check "fplay3 succeeds" "succeeds" "$fp3"

# fplay4: CSSG all accept → Carol and Dave become friends
fp4=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay4.
:quit
HEREDOC
2>&1)

check "fplay4 succeeds" "succeeds" "$fp4"
check "fplay4 carol connected dave" "tagged(carol.*connected(dave)" "$fp4"

# fplay5: Bob rejects → Carol gets rejected
fp5=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay5.
:quit
HEREDOC
2>&1)

check "fplay5 succeeds" "succeeds" "$fp5"

# fplay6: Carol rejects → Dave gets rejected
fp6=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay6.
:quit
HEREDOC
2>&1)

check "fplay6 succeeds" "succeeds" "$fp6"

# fplay7: Dave rejects → Carol gets rejected
fp7=$($DART run "$REPL" <<HEREDOC
$CSSG/self.glp
$CSSG/agent.glp
$CSSG/ui/mediator.glp
$CSSG/ui/actors.glp
$BOOT
fplay7.
:quit
HEREDOC
2>&1)

check "fplay7 succeeds" "succeeds" "$fp7"

# -----------------------------------------------
# Summary
# -----------------------------------------------
echo ""
echo "============================================"
echo "Total: $((PASS + FAIL)) | Passed: $PASS | Failed: $FAIL"
echo "============================================"

if [ $FAIL -eq 0 ]; then
    echo "ALL CSSG MODULE TESTS PASSED!"
    exit 0
else
    echo "SOME TESTS FAILED"
    exit 1
fi
```

---

## Step 6: Run and iterate

```bash
bash test/cssg_modules_test.sh
```

If failures occur, diagnose and fix. The likely issues are:
1. `ancestorScope` not threaded correctly → type errors on load
2. `#` dispatch not working → need boot_direct.glp fallback
3. fplay output format different from expected grep patterns → adjust patterns

---

## Step 7: Full regression

After cssg_modules tests pass:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh
```

No new failures allowed.

---

## Step 8: Add to main test suite

Add `cssg_modules_test.sh` to the end of `test/run_all_tests.sh` so it runs as part of the standard suite.

---

## Step 9: Commit

```bash
git add -A
git commit -m "feat(modules): wire hierarchy into GlpEngine, add CSSG module play tests

- GlpEngine.loadSource discovers self.glp chain and passes ancestorScope to type checker
- Added fplay1-7 to boot.glp for observable tagged output
- REPL test script validates all 14 plays (7 silent + 7 tagged)
- Full regression clean"
```

---

## Rules

- Hierarchy integration MUST be backwards compatible — all 326 existing REPL tests must pass
- If `#` dispatch fails at runtime, create boot_direct.glp as fallback and note it
- The test script should be self-contained and runnable standalone
- Report exact error messages for any failures
