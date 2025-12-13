# irmaGLP Multi-Window Multiagent Implementation Plan

**Status**: Draft for Review
**Date**: December 2025
**Prerequisite**: irmaGLP Multi-Window Design Document

## Overview

This document provides a phased implementation plan for the irmaGLP multi-window multiagent simulation. Each phase builds on the previous, with clear deliverables and testing checkpoints.

## Prerequisites

Before starting implementation:

1. **Working GLPSAM**: Single-agent GLP runtime passing all tests
2. **Development environment**: Dart SDK, Flutter desktop for macOS (later phases)

## Phase Summary

| Phase | Milestone | Description | Estimated Effort |
|-------|-----------|-------------|------------------|
| 0 | GLP I/O | GLP ↔ Dart boundary specification + implementation | 5-7 days |
| 1 | Setup | Flutter desktop project, plugin integration | 1-2 days |
| 2 | M1 | Single window with GLPSAM + simple UI | 3-5 days |
| 3 | M2 | Two windows with MethodChannel communication | 3-5 days |
| 4 | M3 | Coordinator with dynamic agent spawning | 3-5 days |
| 5 | M4 | Full irmaGLP V_p/M_p implementation | 5-7 days |
| 6 | M5 | Chat application demo | 3-5 days |
| 7 | M6 | Befriending protocol end-to-end | 5-7 days |

---

## Phase 0: GLP I/O Specification

### Objective

Design, implement, and test the GLP ↔ Dart boundary - the mechanism by which Dart provides input to GLP streams and observes output from GLP streams.

**This phase must be completed and tested with a simple Dart harness (no Flutter) before any UI work begins.**

### The Problem

GLP as specified has no I/O. The agent program receives channels like `ch(UserIn, UserOut)`:

```prolog
agent(Id, ch(UserIn, UserOut), ch(NetIn, NetOut)) :-
    merge(UserIn?, NetIn?, In),
    social_graph(Id?, In?, [(user, UserOut), (net, NetOut)]).
```

But GLP does not specify:
1. Who creates `UserIn` and `UserOut` streams?
2. How does external input (user typing) become terms on `UserIn`?
3. How does the Dart world observe terms written to `UserOut`?
4. What is the synchronization model between Dart and GLPSAM?

### Tasks

#### 0.1 Design GLP I/O Specification

Define the precise mechanism for GLP ↔ Dart communication.

**Questions to answer:**
- Are external streams special stream types or regular GLP streams?
- Does Dart write to a writer variable that GLP reads?
- Does GLP write to a writer variable that Dart observes?
- How does Dart know when a new term is available on an output stream?
- How does GLPSAM know when new input is available?
- What happens if Dart writes faster than GLPSAM consumes?
- What happens if GLPSAM writes faster than Dart consumes?

**Possible approaches:**
1. **System predicates**: `read_external/2`, `write_external/2`
2. **Special stream types**: External streams with Dart callbacks
3. **Bridged variables**: Dart holds writer, GLP holds reader (and vice versa)
4. **Polling**: GLPSAM periodically checks external queue

**Deliverable:** Written specification document for GLP I/O

#### 0.2 Design Review

Review specification against:
- irmaGLP semantics (does it fit?)
- FCP/Logix precedent (how did they do I/O?)
- Implementation feasibility
- Testing approach

#### 0.3 Implement GLP I/O in GLPSAM

Add to `glp_runtime` the mechanisms defined in the specification.

Structure TBD based on design, but likely includes:
- External channel creation
- Term injection from Dart to GLP
- Term observation from GLP to Dart
- Synchronization handling

#### 0.4 Create Test Harness (No Flutter)

Simple Dart command-line program that:
1. Creates a GLPSAM
2. Creates external channels for User and Net
3. Loads a test GLP program
4. Injects input terms from Dart
5. Observes and prints output terms
6. Verifies correct behavior

```dart
// bin/io_test_harness.dart
void main() async {
  final runtime = GlpRuntime();

  // Setup external I/O (mechanism TBD)
  // ...

  // Load test program
  // ...

  // Inject input from Dart side
  print('Injecting: hello');
  // ...

  // Run GLPSAM
  await runtime.runUntilQuiescent();

  // Verify output received on Dart side
  // ...
}
```

#### 0.5 Create Test GLP Programs

```prolog
%% echo_test.glp - Simple echo for I/O testing
echo_test(ch(In, Out)) :-
    echo_loop(In?, Out?).

echo_loop([H|T], Out) :-
    Out = [echo(H?)|Out1],
    echo_loop(T?, Out1?).
echo_loop([], []).
```

```prolog
%% ping_pong_test.glp - Bidirectional I/O test
ping_pong(ch(In, Out)) :-
    Out = [ready|Out1],
    wait_ping(In?, Out1?).

wait_ping([ping|T], Out) :-
    Out = [pong|Out1],
    wait_ping(T?, Out1?).
wait_ping([quit|_], Out) :-
    Out = [goodbye].
```

### Deliverables

1. **GLP I/O Specification Document** - precise definition of GLP ↔ Dart boundary
2. **Implementation in glp_runtime** - working code
3. **Test harness** - command-line Dart program
4. **Test GLP programs** - echo, ping-pong, etc.
5. **Passing tests** - all I/O tests green

### Test Checkpoint

All tests pass with command-line harness:
1. Dart injects term → GLP receives on input stream
2. GLP writes to output stream → Dart observes term
3. Multiple terms in sequence work correctly
4. Bidirectional communication works
5. Stream termination handled correctly
6. No memory leaks on extended use

**Phase 0 must be complete before proceeding to Phase 1.**

---

## Phase 1: Flutter Project Setup

### Objective
Create Flutter desktop project with multi-window plugin configured.

### Tasks

#### 1.1 Create Flutter Project
```bash
flutter create --platforms=macos glp_multiagent
cd glp_multiagent
```

#### 1.2 Add Dependencies
```yaml
# pubspec.yaml
dependencies:
  flutter:
    sdk: flutter
  desktop_multi_window: ^0.2.3
  window_manager: ^0.4.0
  path: ^1.8.0

dev_dependencies:
  flutter_test:
    sdk: flutter
```

#### 1.3 Configure macOS Platform
- Update `macos/Runner/AppDelegate.swift` for multi-window callback
- Update `macos/Runner/MainFlutterWindow.swift` if needed
- Verify signing and entitlements

#### 1.4 Link GLP Runtime
- Add path dependency to existing `glp_runtime` package (with Phase 0 I/O)

```yaml
dependencies:
  glp_runtime:
    path: ../glp_runtime
```

### Deliverable
- Flutter project builds and runs on macOS
- Empty window appears
- `desktop_multi_window` plugin loads without errors
- GLP runtime with I/O available

### Test Checkpoint
```bash
flutter run -d macos
# Verify: Window opens, no errors in console
```

---

## Phase 2: Single Window with GLPSAM (M1)

### Objective
Run a GLPSAM inside a Flutter window with basic UI interaction, using Phase 0 I/O mechanism.

### Tasks

#### 2.1 Create AgentContext
```dart
// lib/multiagent/agent_context.dart
class AgentContext {
  final String id;
  final GlpRuntime runtime;
  final ExternalChannel userChannel;  // From Phase 0

  AgentContext(this.id) :
    runtime = GlpRuntime(),
    userChannel = runtime.createExternalChannel('user');
}
```

#### 2.2 Create Simple Agent UI
```dart
// lib/ui/simple_agent_ui.dart
class SimpleAgentUI extends StatefulWidget {
  final AgentContext context;
  // Shows:
  // - Agent ID
  // - Text input field
  // - Output log area
  // - "Send" button
}
```

#### 2.3 Wire UI to Phase 0 I/O
- Button press → inject term via Phase 0 mechanism
- Phase 0 output observation → update UI display

#### 2.4 Initialize and Run
```dart
void main() {
  final context = AgentContext('test');
  // Load echo_test.glp
  // Start goal with external channel
  runApp(MaterialApp(
    home: SimpleAgentUI(context: context),
  ));
}
```

### Deliverable
- Single window with text input
- User types message, sees echo response
- GLPSAM running inside Flutter app
- Uses Phase 0 I/O mechanism

### Test Checkpoint
1. Type "hello" → See "echo(hello)" in output
2. Type multiple messages → All echoed
3. No memory leaks on extended use

---

## Phase 3: Two Windows with MethodChannel (M2)

### Objective
Two separate windows (isolates), each with GLPSAM, communicating via MethodChannel.

### Tasks

#### 3.1 Implement Window Spawning
```dart
// lib/multiagent/window_manager.dart
class WindowManager {
  Future<WindowController> spawnWindow(String agentId) async {
    final window = await DesktopMultiWindow.createWindow(
      jsonEncode({'agentId': agentId}),
    );
    await window.setFrame(Rect.fromLTWH(x, y, 375, 667));
    await window.show();
    return window;
  }
}
```

#### 3.2 Implement Dual Entry Point
```dart
// main.dart
void main(List<String> args) {
  if (args.firstOrNull == 'multi_window') {
    // Spawned window
    final windowId = int.parse(args[1]);
    final params = jsonDecode(args[2]);
    _runAgentWindow(params['agentId']);
  } else {
    // Main window
    _runMainWindow();
  }
}
```

#### 3.3 Implement NetworkBridge
```dart
// lib/multiagent/network_bridge.dart
class NetworkBridge {
  final String agentId;
  final MethodChannel channel;

  void send(String destination, List<int> payload) {
    channel.invokeMethod('send', {
      'from': agentId,
      'to': destination,
      'payload': payload,
    });
  }

  void listen(void Function(String from, List<int> payload) onReceive) {
    channel.setMethodCallHandler((call) async {
      if (call.method == 'deliver') {
        onReceive(
          call.arguments['from'],
          call.arguments['payload'],
        );
      }
    });
  }
}
```

#### 3.4 Implement Simple Router (in Main Window)
```dart
// lib/multiagent/simple_router.dart
class SimpleRouter {
  final Map<String, MethodChannel> channels = {};

  void register(String agentId, int windowId) { ... }
  void route(Map args) { ... }
}
```

#### 3.5 Create Ping-Pong Test Between Windows

### Deliverable
- Two windows open
- Alice sends ping to Bob
- Bob receives ping, sends pong
- Alice receives pong
- Visible in both UIs

### Test Checkpoint
1. Both windows open with correct titles
2. Ping-pong completes successfully
3. Messages visible in both UIs
4. Close one window → other continues running

---

## Phase 4: Coordinator with Dynamic Spawning (M3)

### Objective
Replace hardcoded two-agent setup with coordinator GLPSAM that spawns agents dynamically.

### Tasks

#### 4.1 Create Coordinator GLPSAM
```dart
// lib/multiagent/coordinator.dart
class Coordinator {
  final GlpRuntime runtime;
  final WindowManager windowManager;
  final Map<String, WindowController> agents = {};

  void _registerSystemPredicates() {
    runtime.systemPredicates.register('spawn_agent', _spawnAgent);
    runtime.systemPredicates.register('kill_agent', _killAgent);
    runtime.systemPredicates.register('list_agents', _listAgents);
  }
}
```

#### 4.2 Create Coordinator UI
- List of active agents
- "Spawn Agent" button with name input
- "Kill Agent" button per agent
- Step/Run/Pause controls

#### 4.3 Create Coordinator GLP Program

#### 4.4 Wire Coordinator UI to GLPSAM via Phase 0 I/O

### Deliverable
- Main window is coordinator with spawn controls
- Click "Spawn Alice" → Alice window appears
- Click "Kill Alice" → Alice window closes
- Agent list updates dynamically

### Test Checkpoint
1. Spawn 5 agents → all windows visible
2. Kill 2 agents → windows close, list updates
3. Remaining agents still functional

---

## Phase 5: Full irmaGLP V_p/M_p (M4)

### Objective
Implement complete irmaGLP variable tracking and message queue as specified.

### Tasks

#### 5.1 Implement VariableTable (V_p)
Per irmaGLP specification.

#### 5.2 Implement MessageQueue (M_p)
Per irmaGLP specification.

#### 5.3 Implement Payload Serializer

#### 5.4 Implement Export Helper
Per irmaGLP specification.

#### 5.5 Implement Reactivate Helper
Per irmaGLP specification.

#### 5.6 Implement Abandon Helper
Per irmaGLP specification.

#### 5.7 Integrate with Reduce Transaction

#### 5.8 Integrate with Message Receipt

### Deliverable
- V_p tracks all non-local variables correctly
- M_p queues and flushes messages
- Export helper creates relays when needed
- Abandonment propagates correctly

### Test Checkpoint
1. Alice creates variable, sends reader to Bob
2. Alice binds variable → Bob receives value
3. Bob abandons reader before binding → Alice notified
4. Complex term with multiple variables → all tracked correctly

---

## Phase 6: Chat Application Demo (M5)

### Objective
Build simple chat UI demonstrating message exchange between agents.

### Tasks

#### 6.1 Design Chat UI
#### 6.2 Create Chat Message Widget
#### 6.3 Create Chat GLP Program
#### 6.4 Integrate Chat UI with GLPSAM via Phase 0 I/O
#### 6.5 Add Friend Management (Simplified)

### Deliverable
- Chat UI looks like simple messenger
- Alice can send "Hello" to Bob
- Bob sees message from Alice
- Multiple concurrent conversations

### Test Checkpoint
1. Two agents chat back and forth
2. Three agents: A↔B, A↔C, B↔C all work
3. Messages display in correct order

---

## Phase 7: Befriending Protocol End-to-End (M6)

### Objective
Full cold-call befriending protocol working with chat application.

### Tasks

#### 7.1 Implement Befriend UI Elements
#### 7.2 Load Full Agent GLP Programs from AofGLP
#### 7.3 Create Network Simulation
#### 7.4 Wire Befriend UI to GLPSAM
#### 7.5 Wire Channel Establishment

### Deliverable
- Alice clicks "Add Friend", enters "bob"
- Bob sees "Alice wants to connect"
- Bob clicks "Accept"
- Both see each other in friends list
- Can now chat

### Test Checkpoint
1. Cold-call befriending: A→B works
2. Rejection: A→B, B rejects, A sees rejection
3. After befriend: chat works

---

## Testing Strategy

### Unit Tests (Each Phase)
- Individual component tests
- Mock dependencies

### Integration Tests (Phase 3+)
- Two-component interaction
- Message flow verification

### End-to-End Tests (Phase 6+)
- Full scenario execution

### Manual Testing Checklist
- [ ] I/O mechanism (Phase 0)
- [ ] Window spawning/killing
- [ ] Message delivery both directions
- [ ] Variable synchronization
- [ ] Abandonment handling
- [ ] UI responsiveness
- [ ] Memory usage stable
- [ ] No deadlocks on rapid interaction

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| GLP I/O design complexity | Start with simplest approach; iterate |
| Plugin compatibility issues | Test early in Phase 1; have fallback plan |
| MethodChannel performance | Profile in Phase 3; batch messages if needed |
| GLPSAM integration complexity | Keep runtime changes minimal; adapt bridge layer |

---

## Definition of Done

### Per Phase
- All tasks completed
- Test checkpoint passed
- No critical bugs

### Project Complete (M6)
- Live demo with 3+ agents
- Cold-call befriending works
- Chat messaging works
- Stable for 10+ minute demo
- Documentation updated
