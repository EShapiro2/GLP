# Dart Runtime Spec for `@` Operator (Isolate Boot)

**Version**: 0.4 (DRAFT)
**Date**: 2026-01-28
**Status**: For review

---

## 1. Overview

The `@` operator enables GLP programs to declaratively spawn agents across Dart isolates at boot time. A GLP file with:

```prolog
procedure boot.
boot :- 
    agent_init(alice, ch(_?,_), ch(_?,_))@alice,
    agent_init(bob, ch(_?,_), ch(_?,_))@bob,
    agent_init(charlie, ch(_?,_), ch(_?,_))@charlie.

%% ... rest of program (agent_init/3, agent/3, etc.)
```

Instructs the Dart runtime to:
1. Create three isolates named `alice`, `bob`, `charlie`
2. In each isolate, run the specified goal with properly wired channels
3. Route cold-call messages between isolates

**Key design principle**: The Dart runtime handles all inter-isolate routing. There is no GLP-level network switch — cold-call messages are routed by Dart based on the destination agent ID in `msg(Target, Content)`.

---

## 2. Syntax and Restrictions

### 2.1 Syntax

```
BootDecl   ::= 'procedure' 'boot' '.'
BootClause ::= 'boot' ':-' SpawnGoal (',' SpawnGoal)* '.'
SpawnGoal  ::= Goal '@' AgentId
AgentId    ::= Atom
Goal       ::= Functor '(' AgentId ',' Channel ',' Channel ')'
Functor    ::= Atom
Channel    ::= 'ch' '(' '_?' ',' '_' ')'
```

### 2.2 Restrictions (v0.4)

1. **Boot-time only**: The `@` operator is only valid in the `boot/0` clause. It cannot appear elsewhere in the program.

2. **First clause requirement**: The GLP file must have `procedure boot.` declaration and `boot/0` clause as its first procedure when using isolate spawning.

3. **Procedure declaration required**: `procedure boot.` must precede the boot clause for the program to compile.

4. **Ground agent identifiers**: The `AgentId` in both the goal and after `@` must be ground atoms, and must match (e.g., `agent_init(alice, ...)@alice`).

5. **Goal structure**: The spawned goal must be a 3-arity procedure `p(AgentId, UICh, NetCh)` where:
   - First argument is the agent's identifier (must match the `@AgentId`)
   - Second argument is the UI channel (user interaction)
   - Third argument is the network channel (inter-agent cold-calls)
   - The procedure name `p` can be any atom (e.g., `agent_init`, `alice_agent`, `test_agent`)

6. **Anonymous channel variables**: Channel arguments must use the pattern `ch(_?,_)` — the Dart runtime creates and wires the actual variables.

---

## 3. Dart Runtime Components

### 3.1 Boot File Loader

The Dart runtime is initialized with a GLP file path. It:

1. Reads the file
2. Verifies the first procedure is `boot/0`
3. Extracts spawn directives from the boot clause body
4. Compiles the program (including boot, though boot is not executed as GLP)
5. Spawns isolates according to the directives

```dart
class BootLoader {
  /// Load a GLP file and extract boot configuration
  /// Throws if first procedure is not boot/0
  BootConfig load(String filePath);
}

class BootConfig {
  final List<SpawnDirective> directives;
  final Program program;  // Compiled GLP program
}

class SpawnDirective {
  final String agentId;      // e.g., 'alice'
  final String goalFunctor;  // e.g., 'agent_init', 'alice_agent'
  // Channels created by Dart
}
```

### 3.2 Isolate Manager

Manages isolate lifecycle and message routing.

```dart
class IsolateManager {
  final Map<String, SendPort> _agentPorts = {};
  final ReceivePort _mainPort = ReceivePort();
  
  /// Spawn all agents from boot configuration
  Future<void> boot(BootConfig config);
  
  /// Route a cold-call message to destination agent
  void routeMessage(String from, String to, NetworkMsg msg);
  
  /// Wait for all agents to complete
  Future<void> waitForCompletion({Duration? timeout});
  
  /// Shutdown all isolates
  Future<void> shutdown();
}
```

### 3.3 Agent Isolate Entry Point

Each isolate runs the specified goal with the following setup:

```dart
void agentIsolateEntry(AgentConfig config) async {
  // 1. Create runtime and IRMA context
  final runtime = GlpRuntime();
  final ctx = IrmaContext(agentId: config.agentId, runtime: runtime);
  
  // 2. Create UI channel pair (second argument)
  final (uiInWriter, uiInReader) = runtime.heap.allocateVariable();
  final (uiOutWriter, uiOutReader) = runtime.heap.allocateVariable();
  final uiCh = StructTerm('ch', [VarRef(uiInReader), VarRef(uiOutWriter)]);
  
  // 3. Create network channel pair (third argument)
  final (netInWriter, netInReader) = runtime.heap.allocateVariable();
  final (netOutWriter, netOutReader) = runtime.heap.allocateVariable();
  final netCh = StructTerm('ch', [VarRef(netInReader), VarRef(netOutWriter)]);
  
  // 4. Register IRMA network streams
  ctx.registerNetworkInput(netInWriter);
  ctx.registerNetworkOutput(netOutWriter);
  
  // 5. Wire UI channel to window
  ctx.registerUIOutput(uiOutWriter, config.uiPort);
  ctx.registerUIInput(uiInWriter);
  
  // 6. Spawn the goal (e.g., agent_init/3)
  spawnGoal(runtime, config.program, '${config.goalFunctor}/3', [
    ConstTerm(config.agentId),  // Agent's identity
    uiCh,                        // UI channel (second arg)
    netCh,                       // Network channel (third arg)
  ]);
  
  // 7. Enter message loop
  runMessageLoop(runtime, ctx, config);
}
```

---

## 4. Channel Wiring

### 4.1 Channel Structure

A channel is a pair `ch(In?, Out)` where:
- `In?` is a reader — the agent reads messages from this stream
- `Out` is a writer — the agent writes messages to this stream

### 4.2 UI Channel (UICh) — Second Argument

Connects the agent to its user interface window. **The user operating the Flutter window is the "actor"** — there are no scripted actor procedures.

| Direction | Variable | Dart Wiring |
|-----------|----------|-------------|
| Agent reads | `UIIn?` | User commands from Flutter window delivered here |
| Agent writes | `UIOut` | Agent notifications displayed in Flutter window |

**Message format (user → agent)**: `msg(user, AgentId, Command)` where Command is e.g., `connect(Target)`, `send(Target, Text)`, `decision(yes, From, Resp)`

**Message format (agent → user)**: `msg(agent, user, Notification)` where Notification is e.g., `befriend(From, Resp)`, `connected(Friend)`, `received(From, Text)`

### 4.3 Network Channel (NetCh) — Third Argument

Connects the agent to the inter-isolate message router (Dart).

| Direction | Variable | Dart Wiring |
|-----------|----------|-------------|
| Agent reads | `NetIn?` | IRMA delivers cold-call messages here via `handleNetworkMessage()` |
| Agent writes | `NetOut` | IRMA monitors via `registerNetworkOutput()`, sends to router |

**Message format**: `msg(Target, Content)` where `Target` is the destination agent's identifier.

---

## 5. Message Routing

### 5.1 Cold-Call Flow (Network Transaction)

When agent `alice` writes `msg(bob, Content)` to her `NetOut`:

1. **IRMA detects write**: `IrmaContext.flushMessages()` sees binding on monitored `NetOut`
2. **Serialize**: `PayloadSerializer` converts the message to bytes
3. **Send to router**: `onMessageReady(destination: 'bob', payload)` callback fires
4. **Router delivers**: `IsolateManager.routeMessage('alice', 'bob', msg)`
5. **Bob receives**: Bob's isolate gets `NetworkMsg` via its `ReceivePort`
6. **IRMA processes**: `ctx.handleNetworkMessage('alice', payload)` binds to Bob's `NetIn`

### 5.2 Message Types

The router handles these IRMA message types:

| Type | Description | Routing |
|------|-------------|---------|
| `agentMessage` | Cold-call content (Network Transaction) | By destination in `msg(Target, _)` |
| `assignment` | Variable binding | By creator in global variable ID |
| `readRequest` | Read request for variable | By creator in global variable ID |

---

## 6. Initialization Sequence

### 6.1 Boot Sequence

```
1. Dart runtime receives GLP file path
2. BootLoader reads file, verifies first procedure is boot/0
3. BootLoader extracts SpawnDirectives from boot clause
4. BootLoader compiles the program
5. IsolateManager spawns isolates:
   a. For each SpawnDirective:
      - Spawn isolate with AgentConfig
      - Wait for Ready message with SendPort
      - Store SendPort in routing table
6. IsolateManager sends Start to all isolates
7. IsolateManager enters routing loop
```

### 6.2 Agent Startup Sequence

```
1. Create GlpRuntime and IrmaContext
2. Allocate channel variables (UICh, NetCh)
3. Register IRMA streams
4. Wire UI channel to window
5. Build goal arguments with proper VarRef readers
6. Spawn goal on scheduler
7. Send Ready to main isolate
8. Wait for Start
9. Run scheduler, process messages
```

---

## 7. Data Structures

### 7.1 AgentConfig

Passed to isolate entry point:

```dart
class AgentConfig {
  final String agentId;
  final String goalFunctor;   // e.g., 'agent_init', 'alice_agent'
  final Program program;
  final SendPort mainPort;    // For routing messages
  final SendPort? uiPort;     // For UI events (null if headless)
}
```

### 7.2 Inter-Isolate Messages

```dart
sealed class IsolateMessage {}

/// Agent is ready, provides its SendPort
class Ready extends IsolateMessage {
  final String agentId;
  final SendPort sendPort;
}

/// Start execution
class Start extends IsolateMessage {}

/// Network message to route
class NetworkMsg extends IsolateMessage {
  final String from;
  final String to;
  final List<int> payload;
  final MessageType type;
}

/// UI event from window
class UIEvent extends IsolateMessage {
  final String agentId;
  final Term content;
}

/// Agent completed
class Done extends IsolateMessage {
  final String agentId;
  final bool success;
  final String? error;
}
```

---

## 8. Error Handling

### 8.1 Boot Errors

| Error | Condition | Action |
|-------|-----------|--------|
| `BootClauseMissing` | First procedure is not `boot/0` | Throw exception, do not start |
| `InvalidSpawnDirective` | Malformed `@` syntax | Throw exception with line number |
| `DuplicateAgentId` | Same agent ID used twice | Throw exception |
| `AgentIdMismatch` | Goal's first arg doesn't match `@AgentId` | Throw exception |

### 8.2 Runtime Errors

| Error | Condition | Action |
|-------|-----------|--------|
| Isolate exception | Unhandled error in agent | Send `Done(success: false)`, log error |
| Unknown destination | Message to non-existent agent | Log warning, drop message |
| Timeout | No progress after configured duration | Report status, allow graceful shutdown |

---

## 9. Testing

### 9.1 Headless Testing

For unit tests, UI channels can be mocked:
- `uiPort` is null or connects to a test harness
- Test harness injects UI events (simulating user actions) and captures agent output

### 9.2 Test Harness Example

```dart
test('three agent cold-call protocol', () async {
  final manager = IsolateManager();
  final config = BootLoader().load('programs/play_alice_bob_charlie_boot.glp');
  
  await manager.boot(config);
  
  // Simulate user actions via UI channel
  manager.injectUIEvent('alice', ConstTerm('msg(user, alice, connect(bob))'));
  
  await manager.waitForCompletion(timeout: Duration(seconds: 30));
  
  expect(manager.allCompleted, isTrue);
});
```

---

## 10. Example GLP File

```prolog
%% play_alice_bob_charlie_boot.glp

procedure boot.
boot :-
    agent_init(alice, ch(_?,_), ch(_?,_))@alice,
    agent_init(bob, ch(_?,_), ch(_?,_))@bob,
    agent_init(charlie, ch(_?,_), ch(_?,_))@charlie.

%% Agent initialization - wires channels and starts agent loop
procedure agent_init(_?, Channel?, Channel?).
agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
    merge(UserIn?, NetIn?, In),
    agent(Id?, In?, [friend(user, UserOut), friend(net, NetOut)]).

%% Agent main loop
procedure agent(_?, Stream?, FriendsList?).
agent(Id, [Msg|In], Fs) :-
    %% Handle messages from user and network
    ...
agent(_, [], _).

%% Supporting procedures: merge, lookup_send, etc.
...
```

**Note**: The `network3` procedure (GLP-level network switch) is not needed — Dart handles all inter-isolate routing via cold-calls.

---

## 11. Future Extensions (Out of Scope for v0.4)

The following are explicitly **not supported** in this version:

1. **Dynamic spawning**: Using `@` at runtime (not just boot)
2. **Variable agent IDs**: `agent_init(Id?, ...)@Id?` with runtime evaluation
3. **Isolate pools**: Multiple agents per isolate
4. **Remote isolates**: Network-distributed agents
5. **Non-3-arity goals**: Only 3-argument procedures are supported

These may be added in future versions as needed.

---

## Document History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2026-01-28 | Initial draft |
| 0.2 | 2026-01-28 | Restricted to boot-time only; fixed goal to `agent/3`; boot clause must be first |
| 0.3 | 2026-01-28 | Generalized goal functor: any `p/3` procedure allowed, not just `agent/3` |
| 0.4 | 2026-01-28 | Fixed channel order (UICh second, NetCh third); added `procedure boot.` requirement; clarified users are actors; updated example to match actual GLP code |
