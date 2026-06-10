# Dart Runtime Spec for `@` Operator (Isolate Boot)

**Version**: 0.6 (DRAFT)
**Date**: 2026-02-01
**Status**: Updated for madGLP

---

## 1. Overview

The `@` operator enables GLP programs to declaratively spawn agents across Dart isolates at boot time. A GLP file with:

```prolog
procedure boot.
boot :-
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob,
    agent_init(charlie, _)@charlie.

%% ... rest of program (agent_init/2, agent/4, etc.)
```

Instructs the Dart runtime to:
1. Create three isolates named `alice`, `bob`, `charlie`
2. In each isolate, create the serializer entry at index 0 and spawn the specified goal with `(agentId, netInReader)`
3. Route madGLP messages between isolates

**Key design principle**: The Dart runtime handles all inter-isolate routing. There is no GLP-level network switch — messages are routed by Dart based on the destination agent ID.

---

## 2. Syntax and Restrictions

### 2.1 Syntax

```
BootDecl   ::= 'procedure' 'boot' '.'
BootClause ::= 'boot' ':-' SpawnGoal (',' SpawnGoal)* '.'
SpawnGoal  ::= Goal '@' AgentId
AgentId    ::= Atom
Goal       ::= Functor '(' AgentId (',' ConstArg)* ',' '_' ')'
Functor    ::= Atom
ConstArg   ::= Atom | Integer
```

The last argument is always `_` (network input placeholder). Middle arguments between the agent ID and `_` are optional constants passed through to the isolate.

### 2.2 Restrictions (v0.6)

1. **Boot-time only**: The `@` operator is only valid in the `boot/0` clause. It cannot appear elsewhere in the program.

2. **First clause requirement**: The GLP file must have `procedure boot.` declaration and `boot/0` clause as its first procedure when using isolate spawning.

3. **Procedure declaration required**: `procedure boot.` must precede the boot clause for the program to compile.

4. **Ground agent identifiers**: The `AgentId` in both the goal and after `@` must be ground atoms, and must match (e.g., `agent_init(alice, ...)@alice`).

5. **Goal structure**: The spawned goal must have arity 2 or more with the following convention:
   - First argument is the agent's identifier (must match the `@AgentId`)
   - Last argument receives the network input reader (provided by Dart as `_`)
   - Middle arguments (if any) are additional constants passed through from the boot clause
   - The procedure name can be any atom (e.g., `agent_init`, `parent_init`)
   - Examples: `agent_init(alice, _)@alice` (arity 2), `child_init(carol, alice, 4, _)@carol` (arity 4)

6. **Anonymous last argument**: The last argument must be `_` — the Dart runtime creates and provides the actual network input reader.

7. **No Dart-provided channels**: The Dart runtime does NOT create or pass UI channels. The GLP boot goal is responsible for creating any channels it needs internally.

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
  BootConfig load(String source);
}

class BootConfig {
  final List<SpawnDirective> directives;
  final String source;           // Source with boot clause stripped
  List<String>? sharedSources;   // Optional shared code files
  String? projectDir;            // Optional project directory for static linking
  String rootSelfGlpPath;        // Absolute path to programs/self.glp
}

class SpawnDirective {
  final String agentId;          // e.g., 'alice'
  final String goalFunctor;      // e.g., 'agent_init', 'parent_init'
  final int goalArity;           // e.g., 2, 3, 4
  final List<String> constantArgs; // Constants between agentId and netIn
  // e.g., for parent_init(alice, carol, 4, _)@alice:
  //   agentId='alice', goalFunctor='parent_init', goalArity=4,
  //   constantArgs=['carol', '4']
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

  /// Route a madGLP message to destination agent
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
  // 1. Create runtime and madGLP context
  final engine = GlpEngine(rootSelfGlpPath: config.rootSelfGlpPath);
  engine.enableMadGLP(agentId: config.agentId);

  // 2. Load program code
  if (config.projectDir != null) {
    engine.loadProject(config.projectDir!);
    engine.loadSource(config.programSource, filename: 'program');
  } else {
    if (config.sharedSources != null) {
      for (var i = 0; i < config.sharedSources!.length; i++) {
        engine.loadSource(config.sharedSources![i], filename: 'shared_$i');
      }
    }
    engine.loadSource(config.programSource, filename: 'program');
  }

  final ctx = engine.madContext!;
  final runtime = engine.runtime;

  // 3. Initialize serializer entry at index 0 for network input
  final (netInWriter, netInReader) = runtime.heap.allocateVariable();
  ctx.wp.initializeSerializerEntry(netInWriter);

  // 4. Set up message delivery callback
  ctx.onMessageReady = (dest, msg) {
    config.mainPort.send(NetworkMsg(config.agentId, dest, msg.payload, msg.type));
  };

  // 5. Build goal arguments:
  //    arg 0 = agent ID (constant)
  //    args 1..n-2 = additional constants from boot directive
  //    arg n-1 = network input reader
  final args = <int, Term>{};
  // Arg 0: agent ID
  args[0] = boundConstant(runtime, config.agentId);
  // Middle args: constants
  for (var i = 0; i < config.goalConstantArgs.length; i++) {
    args[i + 1] = boundConstant(runtime, config.goalConstantArgs[i]);
  }
  // Last arg: network input reader
  args[config.goalArity - 1] = VarRef(netInReader);

  // 6. Spawn the goal (e.g., agent_init/2)
  spawnGoal(runtime, program, config.goalFunctor, config.goalArity, args);

  // 7. Enter event-driven message loop
  runMessageLoop(runtime, ctx, config);
}
```

Note: The Dart runtime does NOT create UI channels. Only the network input is provided via the serializer entry at index 0. The GLP boot goal (e.g., `agent_init`) is responsible for creating any channels it needs internally.

---

## 4. Channel Wiring

### 4.1 Channel Structure

A channel is a pair `ch(In?, Out)` where:
- `In?` is a reader — the agent reads messages from this stream
- `Out` is a writer — the agent writes messages to this stream

The Dart runtime provides only the **network input reader** (via the index-0 serializer entry at boot time). All other channels (UI, friend channels, etc.) are created internally by the GLP boot goal.

### 4.2 UI Agent Layer

The UI channel connects the social agent to its user interface. This is mediated by a **ui_agent** layer that has two implementations:

1. **Window implementation** (`ui_agent_window/2`): Spawns a Flutter window for human interaction
2. **Actor implementation** (`ui_agent_actor/2`): Runs a scripted GLP actor procedure for automated testing

Both implementations provide the same interface to the social agent — a channel for bidirectional communication.

#### 4.2.1 UI Agent Interface

```prolog
%% ui_agent_window(AgentId, Ch) - spawns Flutter window
%% ui_agent_actor(AgentId, Ch)  - runs actor/2 procedure

%% Both provide Ch to social agent for communication
```

#### 4.2.2 Window Implementation

```prolog
procedure ui_agent_window(_?, Channel).
ui_agent_window(Agent, Ch) :-
    '_spawn_window'(Agent, DartCh) |  %% builtin: creates window, returns channel
    ui_relay(Ch?, DartCh?).
```

The `'_spawn_window'/2` builtin:
- Creates a Flutter window titled with Agent name
- Returns a channel connected to the window's I/O
- Window displays agent output, accepts user input

#### 4.2.3 Actor Implementation

```prolog
procedure ui_agent_actor(_?, Channel).
ui_agent_actor(Agent, Ch) :-
    actor(Agent, Ch?).  %% actor/2 defined in GLP, drives the test
```

The `actor/2` procedure is pure GLP code that simulates user behavior:
- Reads messages from the social agent
- Makes decisions (accept/reject friend requests, etc.)
- Writes commands back (connect, send, etc.)

#### 4.2.4 Shared Relay with no_readers Validation

Both implementations use a relay that validates output with `no_readers`:

```prolog
procedure ui_relay(Channel?, Channel?).

%% From social agent to user: wait until no readers, then forward
ui_relay(AgentCh, UserCh) :-
    receive(Msg, AgentCh?, AgentCh1),
    no_readers(Msg?) |
    send(Msg?, UserCh?, UserCh1),
    ui_relay(AgentCh1?, UserCh1?).

%% From user to social agent: pass through
ui_relay(AgentCh, UserCh) :-
    receive(Msg, UserCh?, UserCh1) |
    send(Msg?, AgentCh?, AgentCh1),
    ui_relay(AgentCh1?, UserCh1?).
```

The `no_readers(Msg?)` guard ensures output to the user contains no reader variables (writers are allowed for interactive queries like `befriend(alice, X35)`).

#### 4.2.5 Boot Examples

The boot clause is the same for both actor and window modes:

**Boot clause (same for both modes):**
```prolog
procedure boot.
boot :-
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob,
    agent_init(charlie, _)@charlie.
```

**Actor mode `agent_init` (headless testing):**
```prolog
agent_init(Id, NetIn) :-
    ground(Id?) |
    send_to_net(NetOut?),
    agent(Id?, UserOut?, NetIn?, [output('_user', UserIn), output('_net', NetOut)]),
    actor(Id?, ch(UserIn?, UserOut)).
```

**Window mode `agent_init` (visual UI):**
```prolog
agent_init(Id, NetIn) :-
    ground(Id?) |
    send_to_net(NetOut?),
    '_spawn_window'(Id?, DartCh) |
    agent(Id?, WindowOut?, NetIn?, [output('_user', UserIn), output('_net', NetOut)]),
    ui_relay(ch(UserIn?, WindowOut), DartCh?).
```

Both modes receive only `(agentId, netIn)` from Dart. The difference is in how the UI channel is wired: actor mode creates a GLP actor procedure, window mode spawns a Flutter window.

#### 4.2.6 Message Formats

| Direction | Format | Examples |
|-----------|--------|----------|
| User → Agent | Command terms | `connect(bob)`, `send(bob, hello)`, `X35 = accept(Ch)` |
| Agent → User | Notification terms | `befriend(alice, X35)`, `connected(bob)`, `received(alice, hello)` |

Writer variables in output (like `X35` in `befriend(alice, X35)`) allow interactive queries — the user binds them to provide responses.

### 4.3 Network Channel (NetIn) — Last Argument

The network input is provided by the Dart runtime as the last argument to the boot goal.

| Direction | Variable | Source |
|-----------|----------|--------|
| Agent reads | `NetIn?` | Serializer entry at index 0; cold-call messages delivered here |
| Agent writes | `NetOut` | Created by GLP boot goal; processed by `send_to_net` |

The Dart runtime creates the serializer entry `(N_p, *)` at index 0 and passes `N_p?` (the reader) as the last argument to the boot goal. Network output is handled by the GLP `send_to_net` predicate, which uses `global_send` to send cold-call messages via the index-0 serializer mechanism.

**Message format**: `msg(Target, Content)` where `Target` is the destination agent's identifier.

---

## 5. Message Routing

### 5.1 madGLP Message Flow

madGLP uses a push-based model where messages are sent when writers are bound. See madGLP-spec.md for full details.

**Assignment Message Flow:**

1. Agent p binds writer X, X? becomes known
2. `global_send` goal fires (if watching X?)
3. Message added to M_p (message set)
4. `ctx.flushMessages()` delivers via `onMessageReady` callback
5. IsolateManager routes to destination agent
6. Destination receives, calls `ctx.handleMadAssignment()`
7. Local writer bound, entry removed from W_p

### 5.2 Message Types

The router handles these madGLP message types:

| Type | Description | Routing |
|------|-------------|---------|
| `agentMessage` | Cold-call content (Network Transaction) | By destination in `msg(Target, _)` |
| `assignment` | Variable assignment (per madGLP spec) | By global name in message |

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
1. Create GlpRuntime and MadContext
2. Allocate channel variables (UICh, NetCh)
3. Set up onMessageReady callback for message delivery
4. Build goal arguments with proper VarRef readers
5. Spawn goal on scheduler
6. Send Ready to main isolate
7. Wait for Start
8. Run scheduler, process messages via flushMessages()
```

---

## 7. Data Structures

### 7.1 AgentConfig

Passed to isolate entry point:

```dart
class AgentConfig {
  final String agentId;
  final String goalFunctor;       // e.g., 'agent_init', 'parent_init'
  final int goalArity;            // e.g., 2, 3, 4
  final List<String> goalConstantArgs; // Constants between agentId and netIn
  final String programSource;
  final List<String>? sharedSources;  // Optional shared code files
  final String? projectDir;          // Optional project dir for static linking
  final String rootSelfGlpPath;      // Absolute path to programs/self.glp
  final SendPort mainPort;           // For routing messages
  final SendPort? uiPort;            // For UI events (null if headless)
  final TraceConfig traceConfig;     // Trace configuration
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

/// Network message to route (madGLP)
class NetworkMsg extends IsolateMessage {
  final String from;
  final String to;
  final List<int> payload;
  final MessageType type;

  /// Optional global name fields for routing
  final String? globalNameAgent;
  final int? globalNameIndex;
  final bool? globalNameIsWriter;
}

/// UI event from window
class UIEvent extends IsolateMessage {
  final String agentId;
  final List<int> payload;
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
    agent_init(alice, _)@alice,
    agent_init(bob, _)@bob,
    agent_init(charlie, _)@charlie.

%% Agent initialization - creates channels internally, starts agent loop
procedure agent_init(_?, Stream(X)?).
agent_init(Id, NetIn) :-
    ground(Id?) |
    send_to_net(NetOut?),
    agent(Id?, UserOut?, NetIn?, [output('_user', UserIn), output('_net', NetOut)]),
    actor(Id?, ch(UserIn?, UserOut)).

%% Agent main loop
procedure agent(_?, Stream(X)?, OutputsList?).
agent(Id, [Msg|In], Outs) :-
    %% Handle messages from user and network
    ...
agent(_, [], _).

%% Supporting procedures: merge, lookup_send, etc.
...
```

**Note**: The Dart runtime provides only the network input reader (`NetIn?`) via the serializer. The `agent_init` procedure creates user channels and network output internally.

---

## 11. Future Extensions (Out of Scope for v0.6)

The following are explicitly **not supported** in this version:

1. **Dynamic spawning**: Using `@` at runtime (not just boot)
2. **Variable agent IDs**: `agent_init(Id?, ...)@Id?` with runtime evaluation
3. **Isolate pools**: Multiple agents per isolate
4. **Remote isolates**: Network-distributed agents

These may be added in future versions as needed.

---

## Document History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2026-01-28 | Initial draft |
| 0.2 | 2026-01-28 | Restricted to boot-time only; fixed goal to `agent/3`; boot clause must be first |
| 0.3 | 2026-01-28 | Generalized goal functor: any `p/3` procedure allowed, not just `agent/3` |
| 0.4 | 2026-01-28 | Fixed channel order (UICh second, NetCh third); added `procedure boot.` requirement; clarified users are actors; updated example to match actual GLP code |
| 0.5 | 2026-01-31 | Updated for madGLP: replaced IRMA terminology with madGLP, removed deprecated APIs (registerNetworkInput/Output, handleNetworkMessage), updated message flow to use push-based model with global_send and handleMadAssignment |
| 0.6 | 2026-02-01 | Redesigned Section 4.2: UI Agent Layer with two implementations (window vs actor). Added `ui_agent_window/2`, `ui_agent_actor/2`, `ui_relay/2` with `no_readers` validation. Added `'_spawn_window'/2` builtin. Boot examples for both modes. |
| 0.7 | 2026-04-04 | Harmonized with implementation: variable-arity goals (not 3-only), Dart provides only network input (not UI channels), updated syntax, AgentConfig, entry point, and channel wiring. |
