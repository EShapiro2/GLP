# irmaGLP Multi-Window Multiagent Simulation Design

**Status**: Draft for Review
**Date**: December 2025
**Authors**: Udi Shapiro, Claude

## 1. Overview

### 1.1 Purpose

This document specifies the design for a multiagent GLP simulation where each agent runs in its own Dart isolate with its own Flutter window, providing a realistic simulation of smartphones running GLP-based grassroots applications.

### 1.2 Goals

1. **Faithful irmaGLP implementation**: Each agent has its own GLPSAM with local heap, V_p (variable table), and M_p (message queue)
2. **Visual simulation**: Each agent appears as a separate "smartphone" window on macOS
3. **Realistic communication**: Inter-agent communication via opaque byte transport between isolates
4. **Coordinator agent**: A special GLPSAM that manages simulation, routes messages, and provides operator UI
5. **Application flexibility**: UI is application-dependent (WhatsApp-like, X-like, etc.)

### 1.3 Dart and Flutter

**Dart** is the programming language providing:
- Language syntax, types, async/await
- Dart VM for execution
- **Isolates** - memory-isolated execution units

**Flutter** is a UI framework built on top of Dart, adding:
- **FlutterEngine** - wraps a Dart isolate with rendering capabilities
- Widgets for UI components
- Platform integration (windows, input, etc.)
- **MethodChannel** - byte transport between isolates

The `desktop_multi_window` Flutter plugin creates separate FlutterEngines for each window, each running in its own isolate.

**Software Stack:**

```
┌─────────────────────┐
│   Our GLP Code      │  ← GlpRuntime, V_p, M_p (what we write)
├─────────────────────┤
│   Dart Language     │  ← Classes, async, etc.
├─────────────────────┤
│   Flutter Framework │  ← Widgets, MethodChannel
├─────────────────────┤
│   FlutterEngine     │  ← Dart isolate + renderer
├─────────────────────┤
│   macOS Window      │  ← OS-level window
└─────────────────────┘
```

**Concept Mapping:**

| Concept | What It Is | irmaGLP Equivalent |
|---------|------------|-------------------|
| Dart Isolate | Memory-isolated execution unit | Agent's isolated runtime |
| FlutterEngine | Isolate + UI rendering | Agent runtime + smartphone display |
| Window | OS window containing FlutterEngine | Smartphone screen |
| MethodChannel | Opaque byte transport between isolates | Inter-agent communication channel |

## 2. Architecture

### 2.1 System Topology

```
┌─────────────────────────────────────────────────────────────────────┐
│                      COORDINATOR WINDOW                              │
│                      (Main FlutterEngine)                            │
│  ┌───────────────────────────────────────────────────────────────┐  │
│  │                     Coordinator GLPSAM                         │  │
│  │  - Routes opaque bytes between agents                         │  │
│  │  - Spawns/destroys agent windows                              │  │
│  │  - Operator UI (step/run/pause, visualization)                │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                              │                                       │
│                    MethodChannel Hub                                 │
└──────────────────────────────┼───────────────────────────────────────┘
                               │
          ┌────────────────────┼────────────────────┐
          │                    │                    │
          ▼                    ▼                    ▼
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│  ALICE WINDOW   │  │   BOB WINDOW    │  │  CAROL WINDOW   │
│ (FlutterEngine) │  │ (FlutterEngine) │  │ (FlutterEngine) │
│                 │  │                 │  │                 │
│ ┌─────────────┐ │  │ ┌─────────────┐ │  │ ┌─────────────┐ │
│ │   GLPSAM    │ │  │ │   GLPSAM    │ │  │ │   GLPSAM    │ │
│ │  ┌───────┐  │ │  │ │  ┌───────┐  │ │  │ │  ┌───────┐  │ │
│ │  │ Heap  │  │ │  │ │  │ Heap  │  │ │  │ │  │ Heap  │  │ │
│ │  │ ROQ   │  │ │  │ │  │ ROQ   │  │ │  │ │  │ ROQ   │  │ │
│ │  │ GQ    │  │ │  │ │  │ GQ    │  │ │  │ │  │ GQ    │  │ │
│ │  └───────┘  │ │  │ │  └───────┘  │ │  │ │  └───────┘  │ │
│ │  V_p        │ │  │ │  V_p        │ │  │ │  V_p        │ │
│ │  M_p        │ │  │ │  M_p        │ │  │ │  M_p        │ │
│ └─────────────┘ │  │ └─────────────┘ │  │ └─────────────┘ │
│                 │  │                 │  │                 │
│ ┌─────────────┐ │  │ ┌─────────────┐ │  │ ┌─────────────┐ │
│ │  App UI     │ │  │ │  App UI     │ │  │ │  App UI     │ │
│ │ (phone)     │ │  │ │ (phone)     │ │  │ │ (phone)     │ │
│ └─────────────┘ │  │ └─────────────┘ │  │ └─────────────┘ │
└─────────────────┘  └─────────────────┘  └─────────────────┘
```

### 2.2 Communication Model

**The Dart layer is a dumb pipe.** It transports opaque bytes between isolates with no knowledge of structure, meaning, or content.

**What triggers communication:** When writer X in agent P is bound to term T, and reader X? resides in agent Q, then P sends bytes to Q. When Q's GLPSAM interprets these bytes, it binds X? to T.

The payload structure is defined by the GLPSAM, not the transport layer. In future, payloads will be signed and encrypted, but the transport layer will remain oblivious to content.

## 3. Component Specifications

### 3.1 AgentContext

Each agent window contains an AgentContext:

```dart
class AgentContext {
  final AgentId id;                    // e.g., 'alice'
  final GlpRuntime runtime;            // GLPSAM: heap, roq, gq
  final VariableTable vp;              // V_p: non-local variable tracking
  final MessageQueue mp;               // M_p: outbound message buffer
  final NetworkBridge networkBridge;   // MethodChannel interface

  // Streams connected to GLPSAM
  StreamController<Term> chUserIn;     // From UI to GLPSAM
  StreamController<Term> chUserOut;    // From GLPSAM to UI
  StreamController<Term> chNetIn;      // From network to GLPSAM
  StreamController<Term> chNetOut;     // From GLPSAM to network
}
```

### 3.2 VariableTable (V_p)

From irmaGLP spec - tracks variables whose paired counterparts are non-local:

```dart
class VariableTable {
  // Map: varId -> VariableEntry
  final Map<int, VariableEntry> _entries = {};
}

class VariableEntry {
  final int varId;
  final AgentId creator;      // Who created this variable
  final VariableRole role;    // writer, createdReader, importedReader
  dynamic state;              // For writers: bound value or null
                              // For readers: requester AgentId or null
}

enum VariableRole {
  writer,           // We created the writer, counterpart reader is remote
  createdReader,    // We created the reader, counterpart writer is remote
  importedReader,   // We received this reader from another agent
}
```

The variable table maintains the invariant: it contains exactly those variables whose paired counterparts are non-local. Entries are added by the `export` helper when terms leave the agent, and removed when variables become local or are abandoned.

### 3.3 MessageQueue (M_p)

Outbound messages awaiting delivery:

```dart
class MessageQueue {
  final Queue<OutboundMessage> _queue = Queue();
}

class OutboundMessage {
  final AgentId destination;
  final List<int> payload;  // Opaque bytes
}
```

### 3.4 NetworkBridge

Interface between GLPSAM and MethodChannel:

```dart
class NetworkBridge {
  final AgentId agentId;
  final MethodChannel channel;

  // Send opaque bytes to coordinator for routing
  void send(AgentId destination, List<int> payload) {
    channel.invokeMethod('route', {
      'from': agentId,
      'to': destination,
      'payload': payload,
    });
  }

  // Receive opaque bytes from coordinator
  void onReceive(List<int> payload) {
    // Pass to GLPSAM for interpretation
    runtime.processIncomingPayload(payload);
  }
}
```

### 3.5 Coordinator

The coordinator is itself a GLPSAM running a coordination program:

```dart
class Coordinator {
  final GlpRuntime runtime;           // Coordinator's own GLPSAM
  final Map<AgentId, WindowController> windows = {};
  final Map<AgentId, MethodChannel> channels = {};

  // System predicates available to coordinator GLPSAM
  void registerSystemPredicates() {
    runtime.systemPredicates.register('spawn_agent', _spawnAgent);
    runtime.systemPredicates.register('kill_agent', _killAgent);
  }

  // Route bytes from source to destination
  void route(AgentId from, AgentId to, List<int> payload) {
    channels[to]?.invokeMethod('deliver', {
      'from': from,
      'payload': payload,
    });
  }
}
```

**Coordinator GLP Program (sketch):**

```prolog
%% coordinator.glp - Simulation controller

coordinator(ch(OpIn, OpOut), Agents) :-
    merge(OpIn?, NetIn?, In),
    coordinator_loop(In?, OpOut?, Agents?, NetIn?).

coordinator_loop([Cmd|In], OpOut, Agents, NetIn) :-
    handle(Cmd?, Agents?, Agents1, OpOut?, OpOut1, NetIn?, NetIn1),
    coordinator_loop(In?, OpOut1?, Agents1?, NetIn1?).

%% Spawn new agent
handle(spawn(Id), Agents, Agents1, Out, Out1, NetIn, NetIn) :-
    atom(Id?),
    spawn_agent(Id?, Ch),    %% system predicate
    Agents1 = [(Id?, Ch?)|Agents?],
    send(spawned(Id?), Out?, Out1).
```

## 4. Payload Serialization

### 4.1 Requirements

MethodChannel transports bytes between isolates. The GLPSAM must serialize/deserialize its internal messages to bytes.

### 4.2 Design Principle

The serialization format is internal to the GLPSAM. The Dart transport layer treats payloads as opaque `List<int>`. This allows future changes (encryption, signing) without modifying the transport.

### 4.3 Variable Identity Across Agents

Variables that cross agent boundaries need globally unique identification.

**Scheme**: `creator:localId` (e.g., `alice:1042`)

When Alice sends a term containing her local variable 42 to Bob, Bob's GLPSAM records it as an imported variable from Alice.

## 5. irmaGLP Transaction Implementation

### 5.1 Reduce Transaction (Local)

Standard GLPSAM reduction with extensions for V_p and M_p:

```dart
void reduce(AgentContext ctx) {
  final result = ctx.runtime.reduceOneGoal();

  if (result.bindings.isNotEmpty) {
    for (final binding in result.bindings) {
      // Check if reader is non-local
      final entry = ctx.vp.lookup(binding.readerId);
      if (entry != null && entry.role == VariableRole.importedReader) {
        // Reader is remote - queue message
        final payload = serializeBinding(binding);
        ctx.mp.add(OutboundMessage(
          destination: entry.creator,
          payload: payload,
        ));
      }
    }
  }

  // Handle abandoned variables per irmaGLP spec
  for (final abandoned in result.abandoned) {
    _notifyAbandon(ctx, abandoned);
  }
}
```

### 5.2 Message Delivery (Cross-Agent)

When payload arrives at destination agent:

```dart
void processIncomingPayload(AgentContext ctx, List<int> payload) {
  final message = deserializePayload(payload);

  // GLPSAM interprets message and performs appropriate action
  // (bind variable, record request, handle abandon, etc.)
  ctx.runtime.applyRemoteMessage(message);
}
```

## 6. Window Management

### 6.1 Plugin: desktop_multi_window

```yaml
# pubspec.yaml
dependencies:
  desktop_multi_window: ^0.2.3
```

### 6.2 Creating Agent Windows

```dart
class AgentWindowManager {
  final Map<AgentId, WindowController> _windows = {};

  Future<WindowController> spawnAgent(AgentId id, String appType) async {
    final window = await DesktopMultiWindow.createWindow(jsonEncode({
      'agentId': id,
      'appType': appType,  // 'chat', 'social', etc.
    }));

    await window.setTitle('$id - GLP Agent');
    await window.setFrame(Rect.fromLTWH(
      _nextWindowX(), _nextWindowY(),
      375, 667,  // iPhone-ish dimensions
    ));
    await window.show();

    _windows[id] = window;
    return window;
  }

  Future<void> killAgent(AgentId id) async {
    final window = _windows.remove(id);
    await window?.close();
  }
}
```

### 6.3 Window Entry Point

Each window needs its own entry point:

```dart
// main.dart
void main(List<String> args) {
  if (args.firstOrNull == 'multi_window') {
    // This is a spawned agent window
    final windowId = int.parse(args[1]);
    final params = jsonDecode(args[2]);
    runApp(AgentApp(
      windowId: windowId,
      agentId: params['agentId'],
      appType: params['appType'],
    ));
  } else {
    // This is the coordinator window
    runApp(CoordinatorApp());
  }
}
```

## 7. UI Architecture

### 7.1 Separation of Concerns

The UI is **application-dependent**. The irmaGLP/GLPSAM layer provides:

- `chUserIn`: Stream from UI to GLPSAM (user commands)
- `chUserOut`: Stream from GLPSAM to UI (display updates)

The application layer interprets these streams.

### 7.2 UI Widget Interface

```dart
abstract class AgentAppWidget extends StatefulWidget {
  final AgentContext context;

  // Subclasses implement app-specific UI
  // Examples: ChatAppWidget, SocialAppWidget, WalletAppWidget
}
```

### 7.3 Example: Chat Application

```dart
class ChatAppWidget extends AgentAppWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text(widget.context.id)),
      body: Column(
        children: [
          // Message list (from chUserOut)
          Expanded(child: MessageListView(stream: widget.context.chUserOut)),
          // Input field (sends to chUserIn)
          ChatInputField(onSend: (text) {
            widget.context.chUserIn.add(
              StructTerm('user_input', [ConstTerm(text)])
            );
          }),
        ],
      ),
    );
  }
}
```

### 7.4 Coordinator UI

```dart
class CoordinatorWidget extends StatefulWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('GLP Simulation Controller')),
      body: Row(
        children: [
          // Agent list with spawn/kill controls
          AgentListPanel(),
          // Control panel
          ControlPanel(
            onStep: () => coordinator.step(),
            onRun: () => coordinator.run(),
            onPause: () => coordinator.pause(),
          ),
        ],
      ),
    );
  }
}
```

## 8. Initialization Sequence

### 8.1 Startup Flow

1. **Coordinator starts** (main window)
   - Creates coordinator GLPSAM
   - Loads coordinator.glp program
   - Registers system predicates
   - Displays operator UI

2. **Operator spawns agents** (via UI or script)
   - Coordinator calls `spawn_agent(alice, chat)`
   - System predicate creates new window
   - Window initializes with AgentContext
   - Loads agent.glp program
   - Starts with goal: `agent(alice, ch(UserIn, UserOut), ch(NetIn, NetOut))`

3. **Communication established**
   - MethodChannel connected between agent and coordinator
   - NetworkBridge activated
   - Agent ready for messages

### 8.2 Agent Initialization (GLP side)

```prolog
%% From agent.glp
agent(Id, ch(UserIn, UserOut), ch(NetIn, NetOut)) :-
    merge(UserIn?, NetIn?, In),
    social_graph(Id?, In?, [(user, UserOut), (net, NetOut)]).
```

The ChUser and ChNet channels are provided by the Dart runtime, bridged to the UI and NetworkBridge respectively.

## 9. Testing Strategy

### 9.1 Unit Tests

- Payload serialization: round-trip
- VariableTable: V_p operations per irmaGLP spec
- MessageQueue: M_p operations
- NetworkBridge: byte routing

### 9.2 Integration Tests

- Two-agent variable synchronization
- Abandonment propagation

### 9.3 Scenario Tests

Using the existing "plays" framework from grassroots-testing-framework.md:

```yaml
# befriend_scenario.yaml
agents:
  - alice
  - bob

script:
  - agent: alice
    action: connect(bob)
  - agent: bob
    expect: befriend(alice, _)
  - agent: bob
    action: decision(yes, alice, _)
  - agent: alice
    expect: response(accept(_))
```

## 10. Scope Limitations (Current Phase)

- **Error handling**: Restart entire simulation; failstop handling deferred
- **Persistence**: Deferred
- **Performance target**: ~20 agents (single physical screen demo)

## 11. Dependencies

```yaml
# pubspec.yaml
dependencies:
  flutter:
    sdk: flutter
  desktop_multi_window: ^0.2.3
  window_manager: ^0.4.0  # For window positioning

dev_dependencies:
  flutter_test:
    sdk: flutter
```

## 12. File Structure

```
glp_runtime/
├── lib/
│   ├── multiagent/
│   │   ├── agent_context.dart
│   │   ├── variable_table.dart      # V_p
│   │   ├── message_queue.dart       # M_p
│   │   ├── network_bridge.dart
│   │   ├── payload_serializer.dart
│   │   ├── coordinator.dart
│   │   └── window_manager.dart
│   ├── ui/
│   │   ├── agent_app.dart           # Base agent UI
│   │   ├── coordinator_app.dart
│   │   └── apps/
│   │       ├── chat_app.dart
│   │       └── social_app.dart
│   └── runtime/                      # Existing GLPSAM
│       └── ...
├── bin/
│   └── multiagent_sim.dart          # Entry point
└── glp/
    ├── coordinator.glp
    └── agent.glp
```

## 13. Milestones

1. **M1**: Single window with GLPSAM + simple UI (no multiagent yet)
2. **M2**: Two windows communicating via MethodChannel (hardcoded)
3. **M3**: Coordinator with dynamic agent spawning
4. **M4**: Full irmaGLP V_p/M_p implementation
5. **M5**: Chat application demo
6. **M6**: Befriending protocol working end-to-end

---

## Appendix A: irmaGLP Reference

See GLP paper Appendix (irmaGLP section) for formal definitions of:
- Local state (R_p, V_p, M_p)
- Reduce transaction
- Communicate transaction
- Network transaction
- Helper routines: abandon, request, export, reactivate

## Appendix B: MethodChannel Protocol

The MethodChannel carries opaque payloads. The only structure visible to the transport:

```dart
// Agent → Coordinator
channel.invokeMethod('route', {
  'from': agentId,      // Source agent
  'to': targetId,       // Destination agent
  'payload': bytes,     // Opaque List<int>
});

// Coordinator → Agent
channel.invokeMethod('deliver', {
  'from': sourceId,     // Source agent
  'payload': bytes,     // Opaque List<int>
});
```

The `payload` content is created and interpreted solely by GLPSAMs.
