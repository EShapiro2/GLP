# Current Plan: Unify dGLP/madGLP Execution via GlpEngine

## Status: COMPLETED ✓

All implementation steps completed on 2026-02-01.

---

## Problem (Solved)

Previously there were multiple ways to run GLP programs:
1. **REPL** (`bin/glp_repl.dart`) - CLI, single-agent dGLP only
2. **IsolateManager** (`lib/multiagent/isolate_manager.dart`) - created its own runtime per isolate
3. **Tests** - manually constructed goals, bypassing REPL flow

This caused code duplication and different behavior between REPL and tests.

---

## Solution: GlpEngine

**One runtime implementation**: GlpEngine is the single source of truth.

- dGLP = GlpEngine in single process (REPL is thin CLI wrapper)
- madGLP = GlpEngine per isolate + message routing between isolates
- Tests use GlpEngine (not manual goal construction)

---

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    IsolateManager                        │
│  - Parses boot clause                                   │
│  - Spawns isolates                                      │
│  - Routes messages between isolates                     │
└─────────────────────────────────────────────────────────┘
        │                    │                    │
        ▼                    ▼                    ▼
┌─────────────┐      ┌─────────────┐      ┌─────────────┐
│   Isolate   │      │   Isolate   │      │   Isolate   │
│   (alice)   │      │   (bob)     │      │  (charlie)  │
│             │      │             │      │             │
│  GlpEngine  │      │  GlpEngine  │      │  GlpEngine  │
└─────────────┘      └─────────────┘      └─────────────┘
```

---

## Implementation Steps (All Completed)

### ✓ Step 1: Extract GlpEngine from REPL
Created `lib/engine/glp_engine.dart` - the embeddable core.

```dart
class GlpEngine {
  void loadFile(String path);
  void loadSource(String source);
  Future<ExecutionResult> runGoal(String goalText);
  void enableMadGLP({required String agentId});

  GlpRuntime get runtime;
  MadContext? madContext;
}
```

### ✓ Step 2: Refactor REPL to use GlpEngine
`bin/glp_repl.dart` is now a thin CLI wrapper calling engine methods.

### ✓ Step 3: Refactor IsolateManager to use GlpEngine
Each isolate creates GlpEngine, loads program, runs boot goal via engine.

### ✓ Step 4: Update Tests
- Archived `test/multiagent/actor_single_isolate_test.dart` to `test/archive/`
- Created `test/engine/glp_engine_test.dart` with 5 tests (all passing)

---

## Files Created
- `lib/engine/glp_engine.dart` - the unified execution engine
- `test/engine/glp_engine_test.dart` - GlpEngine tests

## Files Modified
- `bin/glp_repl.dart` - refactored to use GlpEngine
- `lib/multiagent/isolate_manager.dart` - refactored to use GlpEngine

## Files Archived
- `test/multiagent/actor_single_isolate_test.dart` → `test/archive/`

---

## Previous Completed Work

### Type Checker Extensions
- Added `#` to builtinGoals (SpawnGoal handling)
- Added SpawnGoal handling in `well_typed_clause.dart`
- Added `no_readers/1` to builtinProcedures and typePrelude

### Boot File Redesign
- Simplified boot clause: `agent_init(alice, _)@alice` etc.
- Changed from separate streams to merged input with OutputsList
- Fixed Channel argument order

### BootLoader Update
- Updated regex for 2-arity spawn directives

---

## Reference Files

### Boot File (main target)
- `/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_alice_bob_charlie_actor_boot.glp`

### Specs
- Isolate Boot Spec: `/Users/udi/Grassroots/GLP/docs/ma/isolate-boot-spec.md`
- Typed GLP Manual: `/Users/udi/Grassroots/GLP/docs/typed-glp-manual.md`
- Discipline: `/Users/udi/Grassroots/GLP/docs/discipline.md`
- madGLP Spec: `/Users/udi/Grassroots/GLP/docs/ma/madGLP-spec.md`

### Type Checker
- Prelude: `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/prelude.dart`
- Clause Validation: `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/clause_validation.dart`
- Well-Typed Clause: `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/well_typed_clause.dart`

### Boot Loader
- `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/boot_loader.dart`

### Working Examples
- `/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_alice_bob_typed.glp` (2-agent, well-typed)

---

## Key Technical Details

### Channel Type
```glp
Channel ::= ch(Stream, Stream?).
```
- First arg: output stream (writer)
- Second arg: input stream (reader)

### agent_init Pattern
```glp
procedure agent_init(Constant?, Channel).
agent_init(Id, ch(NetOut?, NetIn)) :-
    ground(Id?), new_channel(ch(UserIn, UserOut?), ActorCh) |
    ui_agent_actor(Id?, ActorCh?),
    merge(UserIn?, NetIn?, In),
    agent(Id?, In?, [output('_user', UserOut), output('_net', NetOut)]).
```

### OutputsList for Uniform Messaging
```glp
OutputEntry ::= output(String, Stream?).
OutputsList ::= [] ; [OutputEntry|OutputsList].
```
Contains `'_user'`, `'_net'`, and dynamic friends.

---

## Test Commands

```bash
# Type check boot file
cd /Users/udi/Grassroots/GLP/glp_runtime
echo "../programs/typed_book/social_graph/play_alice_bob_charlie_actor_boot.glp" | dart run bin/glp_repl.dart

# Run REPL tests
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh

# Run Dart unit tests
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test

# Run multiagent tests
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/
```
