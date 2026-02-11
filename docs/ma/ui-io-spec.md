# UI I/O Specification for madGLP

**Updated: 2026-02-12**

## Overview

The UI I/O layer provides ground-term-only interaction between Dart and GLP agents.
All I/O between Dart and GLP is ground terms — no GLP variables cross the boundary.

## Architecture

```
Dart (Flutter)                           GLP

UserInput (ground) ───InputInjector───► ui_mediator ───► agent/4 (_user input)
                                           │
                    ◄── _output/1 kernel ◄── send_to_user ◄── ui_mediator ◄── agent/4 (_user output)
                      (outputCallback)

NetInput ─────────InputInjector──────────────────────────► agent/4 (_net input)
                                                              │
NetOutput ◄───── OutputObserver ◄────────────────────────── agent/4 (_net output)
                  (MadRouter)
```

User output goes through the `_output/1` Dart kernel (via `outputCallback`), not through a Dart-observed stream. Network output is observed by Dart's `OutputObserver` and routed via `MadRouter`.

## Implemented Components

### 1. `_output/1` kernel (Dart)

**File**: `glp_runtime/lib/runtime/body_kernels.dart`

Prints a ground term as a line. Registered as a body kernel like `_send/3`.
Uses `rt.outputCallback` if set (for tests/Flutter), otherwise `print()`.

Formatting: atoms as-is, structs as `f(a, b)`, lists as `[a, b, c]`.

**Status**: Implemented and tested.

### 2. `outputCallback` on GlpRuntime (Dart)

**File**: `glp_runtime/lib/runtime/runtime.dart`

```dart
void Function(String)? outputCallback;
```

If set, `_output/1` kernel calls this instead of `print()`. Used by tests and Flutter UI.

**Status**: Implemented.

### 3. `send_to_user/1` (GLP)

**File**: Embedded in `glp_runtime/lib/multiagent/isolate_manager.dart` (`_madPredicatesSource`)

Reads a stream of ground terms, calls `_output/1` for each:

```glp
procedure send_to_user(Stream?).
send_to_user([T | In]) :- ground(T?) | '_output'(T?), send_to_user(In?).
send_to_user([]).
```

**Status**: Implemented and tested.

### 4. `ui_mediator/5` (GLP)

**File**: `programs/typed_book/social_graph/ui_mediator.glp`

Channel-based mediator between `agent/4` and Dart. Uses `send`/`receive` from `social_agent.glp`.

```glp
procedure ui_mediator(Constant?, Channel?, Channel?, PendingList?, Constant?).
```

Arguments:
- `Id` — agent identifier
- `AgentCh` — channel to/from agent (`_user` side of agent/4)
- `UserCh` — channel to/from Dart (ground terms only)
- `Pending` — stored writers awaiting user response
- `NextId` — next request ID counter

**Agent-to-user direction**: strips `msg(agent, '_user', ...)` wrapper. Replaces unbound writers with request IDs: `befriend(alice, Resp?)` becomes `befriend(alice, req(1))`. Stores writer for later binding.

**User-to-agent direction**: wraps user commands in `msg('_user', Id, ...)`. For `decision(Dec, From, ReqId)`, retrieves stored writer via `lookup_pending` and forwards to agent with the original writer restored.

**Status**: Implemented and tested (3 unit tests pass).

### 5. `play_ui_boot.glp` (GLP)

**File**: `programs/typed_book/social_graph/play_ui_boot.glp`

```glp
procedure agent_init(Constant?, Stream?, Stream?).
agent_init(Id, UserIn, NetIn) :-
    ground(Id?) |
    send_to_net(NetOut?),
    agent(Id?, AgentIn?, NetIn?, [output('_user', AgentToUser), output('_net', NetOut)]),
    ui_mediator(Id?, ch(AgentToUser?, AgentIn), ch(UserIn?, UserOut), [], 1),
    send_to_user(UserOut?).
```

**Status**: Written but not yet integrated with Flutter app. Has a known integration problem (see below).

### 6. Input (Dart side)

REPL-like parser for ground terms only. Dart converts user text to ground
GLP terms and writes them into a stream via `InputInjector`.

**File**: `glp_runtime/lib/runtime/external_io.dart` — `InputInjector` class

**Status**: Existing, working.

## Tests

**File**: `glp_runtime/test/multiagent/output_kernel_test.dart` — 5 tests (all pass)
- `_output/1`: prints constant, struct, list
- `send_to_user/1`: consumes ground stream; waits for stream elements to become ground

**File**: `glp_runtime/test/multiagent/ui_mediator_test.dart` — 3 tests (all pass)
- Grounds `befriend` output with request ID
- Passes ground `connected` message through
- Passes ground `received` message through

## Flutter App Integration (Completed 2026-02-12)

The Flutter app has been updated to use `AgentRuntime` from `glp_runtime`. The runtime logic (~600 lines) was extracted from `main.dart` into `agent_runtime.dart`, and the boot was updated to use `agent_init/3` from `play_ui_boot.glp`.

### How it works

1. Coordinator loads three GLP files: `social_agent.glp` + `ui_mediator.glp` + `play_ui_boot.glp`
2. Each agent window creates an `AgentRuntime` with the concatenated source
3. `AgentRuntime.initialize()` compiles the source (with `madPredicatesSource`), starts `agent_init(Id, UserIn, NetIn)`
4. Network output: `send_to_net` → `global_send` → `MadContext` → `onMessageReady` → coordinator → `MadRouter`
5. User output: `send_to_user` → `_output/1` kernel → `outputCallback` → Flutter UI
6. No `ExternalChannel` or `OutputObserver` needed — all I/O goes through GLP predicates

### Status: Needs Manual Testing

The integration is complete and the app builds, but has not been tested interactively with `flutter run -d macos`.

## File Summary

| Component | File | Status |
|-----------|------|--------|
| `AgentRuntime` | `glp_runtime/lib/multiagent/agent_runtime.dart` | ✅ Implemented |
| `_output/1` kernel | `glp_runtime/lib/runtime/body_kernels.dart` | ✅ Implemented |
| `outputCallback` | `glp_runtime/lib/runtime/runtime.dart` | ✅ Implemented |
| `madPredicatesSource` | `glp_runtime/lib/multiagent/isolate_manager.dart` | ✅ Implemented |
| `ui_mediator/5` | `programs/typed_book/social_graph/ui_mediator.glp` | ✅ Implemented |
| `play_ui_boot.glp` | `programs/typed_book/social_graph/play_ui_boot.glp` | ✅ Integrated |
| `ui_agent.glp` | `programs/typed_book/social_graph/ui_agent.glp` | ❌ Obsolete |
| Output kernel tests | `glp_runtime/test/multiagent/output_kernel_test.dart` | ✅ 5 pass |
| Mediator tests | `glp_runtime/test/multiagent/ui_mediator_test.dart` | ✅ 3 pass |
| Flutter app | `glp_multiagent/lib/main.dart` | ✅ Updated |
| External I/O | `glp_runtime/lib/runtime/external_io.dart` | ✅ Existing |
| MadRouter | `glp_multiagent/lib/mad_router.dart` | ✅ Existing |
