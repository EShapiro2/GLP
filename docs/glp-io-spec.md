# GLP I/O Specification

**Status**: Draft for Review
**Date**: December 2025
**Phase**: 0 (Implementation Plan)
**Prerequisite**: Working GLPSAM with heap, ROQ, goal queue

## 1. Overview

### 1.1 Purpose

This document specifies the mechanism by which GLP programs communicate with the external world (Dart runtime, UI, network). It follows the FCP/Logix design principle:

> **GLP programs don't know they're talking to the external world - they just read/write streams.**

### 1.2 The Two-Channel Architecture

Every GLPSAM agent has exactly **two external channels**:

| Channel | Connects To | Purpose |
|---------|-------------|---------|
| **ChUser** | Phone UI | User input (taps, text) → agent; Agent output → display |
| **ChNet** | Network router | Cold-call messages from other agents; Outbound network messages |

From the agent's perspective, both channels are identical `ch(In, Out)` pairs. The difference is only what the Dart runtime connects them to.

### 1.3 Design Principle

No new GLP predicates are needed. External I/O uses standard GLP stream operations:
- Input: GLP reads from a stream (reader variable)
- Output: GLP writes to a stream (writer variable)

The Dart runtime bridges between GLP streams and external events.

### 1.4 Key Insight from FCP

In FCP/Logix:
- Boot gives programs channels already connected to external world
- Input is a stream that runtime extends when events arrive
- Output is a stream that runtime observes
- Programs suspend on unbound readers, resume when bound
- Clean separation: runtime handles external world, programs handle streams

## 2. Architecture

### 2.1 Agent Entry Point

Every agent starts with:

```prolog
agent(Id, ChUser, ChNet) :-
    ChUser = ch(UserIn, UserOut), ChNet = ch(NetIn, NetOut) |
    merge(UserIn?, NetIn?, In),
    social_graph(Id?, In?, [(user, UserOut), (net, NetOut)]).
```

- `ChUser = ch(UserIn, UserOut)` - bidirectional channel to UI
- `ChNet = ch(NetIn, NetOut)` - bidirectional channel to network
- Agent merges inputs, processes uniformly, routes outputs by destination

### 2.2 Stream Direction

| Direction | Who Holds Writer | Who Holds Reader | Data Flow |
|-----------|------------------|------------------|-----------|
| Input (external → GLP) | Dart runtime | GLP program | Dart extends stream, GLP reads |
| Output (GLP → external) | GLP program | Dart runtime | GLP extends stream, Dart observes |

### 2.3 Variable Ownership

```
CHUSER - User Interface Channel:

  UI Events (button tap, text input)         Display Updates
              │                                    ▲
              ▼                                    │
┌──────────────────────────┐         ┌──────────────────────────┐
│    Dart (UserIn Writer)  │         │  Dart (UserOut Reader)   │
│    injects UI events     │         │  observes display cmds   │
└──────────────────────────┘         └──────────────────────────┘
              │                                    ▲
              ▼                                    │
┌──────────────────────────┐         ┌──────────────────────────┐
│   GLP (UserIn? Reader)   │         │  GLP (UserOut Writer)    │
│   reads user commands    │         │  writes display updates  │
└──────────────────────────┘         └──────────────────────────┘


CHNET - Network Channel:

  Incoming cold-calls                        Outgoing messages
              │                                    ▲
              ▼                                    │
┌──────────────────────────┐         ┌──────────────────────────┐
│    Dart (NetIn Writer)   │         │   Dart (NetOut Reader)   │
│  injects network msgs    │         │  observes outbound msgs  │
└──────────────────────────┘         └──────────────────────────┘
              │                                    ▲
              ▼                                    │
┌──────────────────────────┐         ┌──────────────────────────┐
│    GLP (NetIn? Reader)   │         │   GLP (NetOut Writer)    │
│  reads incoming msgs     │         │  writes outbound msgs    │
└──────────────────────────┘         └──────────────────────────┘
```

### 2.4 System Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                       EXTERNAL WORLD                            │
├────────────────────────────┬────────────────────────────────────┤
│      PHONE UI              │         NETWORK                    │
│  (Flutter widgets)         │    (MethodChannel router)          │
└────────────┬───────────────┴──────────────┬─────────────────────┘
             │                              │
             ▼                              ▼
┌────────────────────────────────────────────────────────────────┐
│                    DART RUNTIME BRIDGE                         │
│  ┌─────────────────────┐      ┌─────────────────────────┐      │
│  │ UserInputInjector   │      │ NetInputInjector        │      │
│  │ UserOutputObserver  │      │ NetOutputObserver       │      │
│  └─────────────────────┘      └─────────────────────────┘      │
└────────────────────────────────┬───────────────────────────────┘
                                 │
                                 ▼
┌────────────────────────────────────────────────────────────────┐
│                          GLPSAM                                │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  Heap: variable bindings                                 │  │
│  │  ROQ: suspension/reactivation                            │  │
│  │  Goal Queue: active goals                                │  │
│  └──────────────────────────────────────────────────────────┘  │
└────────────────────────────────┬───────────────────────────────┘
                                 │
                                 ▼
┌────────────────────────────────────────────────────────────────┐
│                        GLP PROGRAM                             │
│  agent(Id, ch(UserIn, UserOut), ch(NetIn, NetOut)) :-          │
│      merge(UserIn?, NetIn?, In),                               │
│      social_graph(Id?, In?, [(user, UserOut), (net, NetOut)]). │
└────────────────────────────────────────────────────────────────┘
```

## 3. External Channel

### 3.1 Definition

An **External Channel** is a bidirectional connection between Dart and GLP:

```dart
class ExternalChannel {
  final String name;           // 'user' or 'net'

  // Input: Dart → GLP
  final int inputWriterAddr;   // Dart holds this writer (to inject terms)
  final int inputReaderAddr;   // GLP receives this reader

  // Output: GLP → Dart
  final int outputWriterAddr;  // GLP receives this writer (to produce terms)
  final int outputReaderAddr;  // Dart holds this reader (to observe)
}
```

**IMPORTANT**: Per `heap-pointer-architecture-spec.md` Section 1.1: "Heap navigation follows pointers explicitly rather than computing addresses via arithmetic. There is no implicit relationship between adjacent heap addresses."

Therefore, the channel stores BOTH writer and reader addresses explicitly. Code must NEVER compute one from the other (e.g., `writerAddr + 1`).

### 3.2 Creation

```dart
ExternalChannel createExternalChannel(HeapFCP heap, String name) {
  // allocateVariable returns (writerAddr, readerAddr) tuple
  final (inputWriterAddr, inputReaderAddr) = heap.allocateVariable();
  final (outputWriterAddr, outputReaderAddr) = heap.allocateVariable();

  return ExternalChannel(
    name: name,
    inputWriterAddr: inputWriterAddr,
    inputReaderAddr: inputReaderAddr,
    outputWriterAddr: outputWriterAddr,
    outputReaderAddr: outputReaderAddr,
  );
}
```

### 3.3 Building the Channel Term for GLP

The channel term follows the GLP convention `ch(Reader, Writer)`.

**Reference**: Per CGLP paper (Definition 5.5, madGLP Transition System), the initial configuration has:
```
A_p = [agent(p, ch(_?, _), ch(_?, _))]
```
where `_?` denotes a reader and `_` denotes a writer. Thus `ch(_?, _)` = `ch(Reader, Writer)`.

**Why ch(Reader, Writer)?**

The GLP clause HEAD pattern is:
```prolog
agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
```

Where `UserIn` (no `?`) is a writer and `UserOut?` (with `?`) is a reader. During HEAD unification:

1. **First position**: HEAD has writer `UserIn`. Per bytecode spec Section 8.1, a writer-mode position cannot receive an unbound writer (WxW violation). It CAN receive a reader - the reader term is stored in the clause variable.

2. **Second position**: HEAD has reader `UserOut?`. Per bytecode spec Section 8.2, a reader-mode position expects a writer to bind.

Therefore the channel **passed to the goal** must be `ch(Reader, Writer)` to match the HEAD pattern `ch(Writer, Reader?)`.

**CRITICAL: Heap-Only Representation**

Per `glp-bytecode-v216-complete.md`, ALL terms must be heap-allocated. The channel structure must be built on the heap, not as a Dart `StructTerm` object. The function returns a `VarRef` pointing to the heap-allocated structure.

```dart
/// Build ch(In, Out) term for GLP - HEAP ALLOCATED
///
/// Channel convention: ch(Reader, Writer)
/// - First element: Reader for input (Dart holds writer, GLP reads via this reader)
/// - Second element: Writer for output (GLP binds this writer, Dart reads via paired reader)
///
/// Returns VarRef to heap-allocated structure (NOT a StructTerm Dart object)
VarRef buildChannelTermOnHeap(HeapFCP heap, ExternalChannel channel) {
  // Allocate structure cell on heap
  final structAddr = heap.allocateStructure('ch', 2);

  // Use explicit addresses - NO arithmetic (per heap-pointer-architecture-spec.md)
  heap.setStructureArg(structAddr, 0, VarRef(channel.inputReaderAddr));   // Reader
  heap.setStructureArg(structAddr, 1, VarRef(channel.outputWriterAddr));  // Writer

  return VarRef(structAddr);
}
```

**Data Flow Summary**:

| Direction | Dart holds | Channel contains | GLP HEAD | GLP accesses |
|-----------|------------|------------------|----------|--------------|
| Input (Dart→GLP) | Writer (binds values) | Reader | `UserIn` (writer mode) | `UserIn?` (reader) |
| Output (GLP→Dart) | Reader (observes) | Writer | `UserOut?` (reader mode) | `UserOut` (paired writer, binds) |

## 4. Input: Dart → GLP

### 4.1 Mechanism

When Dart wants to send a term to GLP:

1. Dart holds the **writer** for the input stream
2. Dart constructs: `[Term | NewTail]`
3. Dart binds: `currentWriter = [Term | NewTail]`
4. Dart updates: `currentWriter = NewTail` (for next injection)
5. GLPSAM ROQ processes the binding
6. Goals suspended on the reader wake up

### 4.2 Implementation

```dart
class InputInjector {
  final Heap heap;
  final String channelName;  // 'user' or 'net'
  int _currentWriterId;

  InputInjector(this.heap, this.channelName, int initialWriterId)
    : _currentWriterId = initialWriterId;

  /// Inject a term into the input stream
  void inject(Term term) {
    // Allocate fresh variable for tail
    final tailId = heap.allocateFreshVar();

    // Build list cell: [term | tail]
    final listCell = ListPair(term, VarRef(tailId, isReader: false));

    // Bind current writer to list cell
    heap.bindVariable(_currentWriterId, listCell);

    // Advance writer to tail for next injection
    _currentWriterId = tailId;
  }

  /// Close the input stream (no more input)
  void close() {
    heap.bindVariable(_currentWriterId, EmptyList());
  }
}
```

### 4.3 GLP Side

GLP program reads normally:

```prolog
social_graph(Id, [Msg|Rest], Fs) :-
    handle_message(Id?, Msg?, Fs?, Fs1),
    social_graph(Id?, Rest?, Fs1?).
social_graph(_, [], _).
```

When `inject(term)` is called:
- If GLP goal was suspended on `In?`, it wakes up
- Goal sees `In? = [term | Rest?]`
- Goal proceeds, may suspend again on `Rest?`

## 5. Output: GLP → Dart

### 5.1 Mechanism

When GLP writes to output stream:

1. GLP holds the **writer** for output stream
2. GLP binds: `Out = [Msg | Out']`
3. GLPSAM ROQ processes binding
4. Dart's observation callback fires
5. Dart receives `Msg`, continues observing `Out'`

### 5.2 Implementation

```dart
class OutputObserver {
  final Heap heap;
  final String channelName;  // 'user' or 'net'
  final void Function(Term) onTerm;
  final void Function() onClose;
  int _currentReaderId;

  OutputObserver(this.heap, this.channelName, int initialReaderId,
                 this.onTerm, this.onClose)
    : _currentReaderId = initialReaderId {
    _observeNext();
  }

  void _observeNext() {
    // Register callback for when reader is bound
    heap.onBind(_currentReaderId, (Term value) {
      if (value is ListPair) {
        // Got [Head | Tail]
        onTerm(value.head);

        // Continue observing tail
        if (value.tail is VarRef) {
          _currentReaderId = (value.tail as VarRef).varId;
          _observeNext();
        } else if (value.tail is EmptyList) {
          onClose();
        }
      } else if (value is EmptyList) {
        onClose();
      }
    });
  }
}
```

### 5.3 Heap Extension: onBind Callback

The heap needs a mechanism to notify Dart when a variable is bound:

```dart
class Heap {
  // Existing fields...

  // Callbacks for external observation
  final Map<int, void Function(Term)> _bindCallbacks = {};

  /// Register callback for when variable is bound
  void onBind(int varId, void Function(Term) callback) {
    // If already bound, call immediately
    if (isBound(varId)) {
      callback(getValue(varId)!);
      return;
    }
    // Otherwise register for later
    _bindCallbacks[varId] = callback;
  }

  /// Called when variable is bound (existing method, extended)
  void bindVariable(int varId, Term value) {
    // Existing binding logic...
    _vars[varId]!.value = value;

    // Notify external observer if registered
    final callback = _bindCallbacks.remove(varId);
    if (callback != null) {
      callback(value);
    }

    // Process ROQ as usual...
    _processROQ(varId);
  }
}
```

## 6. Agent Initialization

### 6.1 Complete Startup Sequence

```dart
class AgentContext {
  final String id;
  final GlpRuntime runtime;

  // User channel (UI)
  final ExternalChannel userChannel;
  final InputInjector userInput;
  final OutputObserver userOutput;

  // Network channel
  final ExternalChannel netChannel;
  final InputInjector netInput;
  final OutputObserver netOutput;
}

Future<AgentContext> startAgent(String agentId, String programFile) async {
  // 1. Create runtime
  final runtime = GlpRuntime();

  // 2. Create both external channels
  final userChannel = runtime.createExternalChannel('user');
  final netChannel = runtime.createExternalChannel('net');

  // 3. Load program
  runtime.loadProgram(programFile);

  // 4. Build channel terms for GLP
  final chUserTerm = buildChannelTerm(userChannel);
  final chNetTerm = buildChannelTerm(netChannel);

  // 5. Start goal: agent(Id, ChUser, ChNet)
  runtime.startGoal('agent', [
    ConstTerm(agentId),
    chUserTerm,
    chNetTerm,
  ]);

  // 6. Create injectors for both input streams
  final userInput = InputInjector(
    runtime.heap, 'user', userChannel.inputWriterId);
  final netInput = InputInjector(
    runtime.heap, 'net', netChannel.inputWriterId);

  // 7. Create observers for both output streams
  final userOutput = OutputObserver(
    runtime.heap, 'user', userChannel.outputReaderId,
    (term) => handleUserOutput(term),  // → UI display
    () => handleUserClose(),
  );
  final netOutput = OutputObserver(
    runtime.heap, 'net', netChannel.outputReaderId,
    (term) => handleNetOutput(term),   // → network router
    () => handleNetClose(),
  );

  return AgentContext(
    id: agentId,
    runtime: runtime,
    userChannel: userChannel,
    userInput: userInput,
    userOutput: userOutput,
    netChannel: netChannel,
    netInput: netInput,
    netOutput: netOutput,
  );
}
```

### 6.2 What Dart Connects To Each Channel

| Channel | Input Injector Source | Output Observer Destination |
|---------|----------------------|----------------------------|
| **user** | UI events: button taps, text input, gestures | UI updates: display messages, update state |
| **net** | Network router: incoming cold-call messages | Network router: outbound messages to other agents |

### 6.3 GLP Program Entry

```prolog
%% agent.glp
agent(Id, ChUser, ChNet) :-
    ChUser = ch(UserIn, UserOut), ChNet = ch(NetIn, NetOut) |
    merge(UserIn?, NetIn?, In),
    social_graph(Id?, In?, [(user, UserOut), (net, NetOut)]).
```

The agent:
1. Unpacks both channels
2. Merges input streams into single `In`
3. Keeps output streams in friends list indexed by `user` and `net`
4. Routes outbound messages via `lookup_send(user, ...)` or `lookup_send(net, ...)`

## 7. Synchronization

### 7.1 Input Buffering

If Dart injects faster than GLP consumes:
- Terms queue up as list elements
- No problem - standard stream behavior
- GLP catches up when it reads

### 7.2 Output Buffering

If GLP writes faster than Dart processes:
- Dart callbacks queue in Dart's event loop
- No problem - Dart async handles this

### 7.3 GLPSAM Execution Model

GLPSAM execution must be scheduled to allow:
1. Dart to inject input (user and net)
2. GLPSAM to run reductions
3. Dart to process output (user and net)
4. Repeat

Options:
- **Cooperative**: GLPSAM runs N reductions, yields to Dart event loop
- **Async**: GLPSAM runs in microtask, Dart events interleave
- **Manual**: Explicit `step()` / `runUntilSuspended()` calls

Recommended for Phase 0: **Manual stepping** for predictable testing.

```dart
// Test harness
while (!runtime.isQuiescent) {
  runtime.step();  // One reduction
}
// Now inject input
userInput.inject(term);
// Run until suspended again
while (!runtime.isQuiescent) {
  runtime.step();
}
```

## 8. API Summary

### 8.1 Dart API

```dart
/// Create external channel (input + output streams)
ExternalChannel createExternalChannel(String name);

/// Build ch(In, Out) term for GLP
Term buildChannelTerm(ExternalChannel channel);

/// Inject term into input stream
void InputInjector.inject(Term term);

/// Close input stream
void InputInjector.close();

/// Observe output stream (callback on each term)
OutputObserver(Heap heap, String name, int readerId,
               void Function(Term) onTerm,
               void Function() onClose);
```

### 8.2 Heap Extensions

```dart
/// Register callback for variable binding
void Heap.onBind(int varId, void Function(Term) callback);
```

### 8.3 No New GLP Predicates

GLP programs use standard stream operations:
- Read: `[H|T] = In?` via pattern match
- Write: `Out = [Msg|Out']` in body

## 9. Test Cases

### 9.1 Echo Test (Single Channel)

```prolog
%% echo.glp
echo(ch(In, Out)) :-
    echo_loop(In?, Out?).

echo_loop([H|T], Out) :-
    Out = [echo(H?)|Out1],
    echo_loop(T?, Out1?).
echo_loop([], []).
```

```dart
// Test
userInput.inject(ConstTerm('hello'));
runtime.runUntilQuiescent();
// Expect: userOutput.onTerm called with echo(hello)
```

### 9.2 Two-Channel Routing Test

```prolog
%% router.glp - Route messages to appropriate output
router(ch(UserIn, UserOut), ch(NetIn, NetOut)) :-
    merge(UserIn?, NetIn?, In),
    route_loop(In?, UserOut?, NetOut?).

route_loop([msg(user, Content)|In], UserOut, NetOut) :-
    UserOut = [Content?|UserOut1],
    route_loop(In?, UserOut1?, NetOut?).
route_loop([msg(net, Content)|In], UserOut, NetOut) :-
    NetOut = [Content?|NetOut1],
    route_loop(In?, UserOut?, NetOut1?).
route_loop([], [], []).
```

```dart
// Test
userInput.inject(StructTerm('msg', [ConstTerm('net'), ConstTerm('hello')]));
runtime.runUntilQuiescent();
// Expect: netOutput.onTerm called with hello

netInput.inject(StructTerm('msg', [ConstTerm('user'), ConstTerm('world')]));
runtime.runUntilQuiescent();
// Expect: userOutput.onTerm called with world
```

### 9.3 Social Graph Initialization Test

```prolog
%% Test: agent initializes and waits for input
agent(Id, ch(UserIn, UserOut), ch(NetIn, NetOut)) :-
    merge(UserIn?, NetIn?, In),
    social_graph(Id?, In?, [(user, UserOut), (net, NetOut)]).

social_graph(Id, [Msg|In], Fs) :-
    lookup_send(user, got(Msg?), Fs?, Fs1),
    social_graph(Id?, In?, Fs1?).
```

```dart
// Test
// Agent starts, suspends waiting for input
assert(runtime.isQuiescent);

// User sends connect request
userInput.inject(StructTerm('msg', [
  ConstTerm('user'),
  ConstTerm('alice'),
  StructTerm('connect', [ConstTerm('bob')])
]));
runtime.runUntilQuiescent();

// Agent should echo back via user output
// Expect: userOutput.onTerm called with got(msg(...))
```

## 10. Implementation Checklist

### 10.1 Heap Extensions
- [ ] `onBind(varId, callback)` method
- [ ] Callback invocation in `bindVariable()`
- [ ] Callback cleanup on binding

### 10.2 External Channel
- [ ] `ExternalChannel` class
- [ ] `createExternalChannel()` factory
- [ ] `buildChannelTerm()` helper

### 10.3 Input Injection
- [ ] `InputInjector` class
- [ ] `inject(term)` method
- [ ] `close()` method
- [ ] Separate instances for user and net

### 10.4 Output Observation
- [ ] `OutputObserver` class
- [ ] Recursive observation of stream tail
- [ ] `onTerm` and `onClose` callbacks
- [ ] Separate instances for user and net

### 10.5 Agent Context
- [ ] `AgentContext` class with both channels
- [ ] `startAgent()` factory function

### 10.6 Test Harness
- [ ] Command-line test program
- [ ] Load GLP file
- [ ] Manual stepping
- [ ] Input injection for both channels
- [ ] Output printing for both channels

### 10.7 Test GLP Programs
- [ ] echo.glp (single channel)
- [ ] router.glp (two channel routing)
- [ ] agent_test.glp (social graph init)

## 11. Message Format Convention

### 11.1 Tagged Messages

Following the social graph pattern, messages are tagged with source:

```prolog
msg(Source, Destination, Content)
```

Where:
- `Source` = `user` | `net` | `<agent_id>`
- `Destination` = `user` | `net` | `<agent_id>`
- `Content` = application-specific term

### 11.2 Example Messages

```prolog
%% User requests connection
msg(user, alice, connect(bob))

%% Agent sends to network
msg(alice, bob, intro(alice, alice, Resp))

%% Network delivers to agent
msg(bob, alice, response(accept(Ch)))

%% Agent updates UI
msg(agent, user, status(connected(bob)))
```

---

## Appendix A: FCP Reference

From FCP/Logix analysis:

**Boot terminal:**
```prolog
io(Bytes, TCH) :-
    processor # [device(create(tty, Bytes)),
                 device(open(tty)),
                 terminal(channel(TCH))].
```

**Input stream:** List of integers (bytes), extended by C emulator when keyboard input arrives.

**Output channel:** Messages written by program, observed by terminal server.

**Polling:** C emulator uses `select()` on file descriptors. When data arrives, stream is extended, goals resume.

## Appendix B: Comparison with FCP

| Aspect | FCP | GLP |
|--------|-----|-----|
| External channels | tty (terminal) | user (UI), net (network) |
| Input format | Bytes (integers) | Terms |
| Output format | Channel messages | Terms |
| Creation | device(create(tty, S)) | createExternalChannel() |
| Polling | C select() | Dart event loop |
| Program view | Same - just streams | Same - just streams |

## Appendix C: Uniform Channel Treatment

Both `ChUser` and `ChNet` are treated identically by the I/O mechanism:

```dart
// Same API for both channels
final userInput = InputInjector(heap, 'user', userChannel.inputWriterId);
final netInput = InputInjector(heap, 'net', netChannel.inputWriterId);

// Same observation pattern
final userOutput = OutputObserver(heap, 'user', ...);
final netOutput = OutputObserver(heap, 'net', ...);
```

The only difference is what Dart connects to each channel:

| Channel | Dart Input Source | Dart Output Destination |
|---------|-------------------|------------------------|
| user | Flutter UI events | Flutter UI updates |
| net | MethodChannel from coordinator | MethodChannel to coordinator |

This uniform treatment means:
1. One implementation handles both channels
2. Testing can use same harness for both
3. Additional channels (if ever needed) work the same way
