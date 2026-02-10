# Multi-Agent Trace Spec

**Version**: 0.1 (DRAFT)
**Date**: 2026-02-10

---

## 1. Overview

The multi-agent trace extends the single-agent REPL trace to multi-isolate runs. Each agent isolate already uses `GlpEngine` with `scheduler.drainWithStatus(debug: debugTrace)` — the same tracing mechanism the REPL uses. The goal is to present the trace output in a structured, per-agent format that is useful for debugging multi-agent programs.

---

## 2. Current State

### 2.1 Single-Agent REPL Trace

The REPL toggles tracing with `:trace`. When enabled, the scheduler prints:

```
goal_head(args) :- body_goal1(args), body_goal2(args)    %% reduction
goal_head(args) → suspended                               %% suspension
goal_head(args) → failed                                   %% failure
```

No agent prefix. Output goes to stdout.

### 2.2 Multi-Agent Test Harness (Current)

Each agent isolate sets `engine.debugTrace = true`. The scheduler trace lines are interleaved on stdout with no agent prefix. MAD infrastructure prints its own lines with `[MAD agentId]` or `[agentId]` prefixes. The result is a mixture:

```
[agent1] Starting isolate
[agent2] Starting isolate
responder_init(agent2, X1?) → suspended
sender_init(agent1, X1?) :- send_to_net([msg(agent2, ack(X2))]), wait_response(X3?)
[MAD agent1] send: found 1 variables in term
send_to_net([msg(agent2, ack(X2))]) :- global_send(...), send_to_net([])
[MAD agent2] _handleSerializerAssignment: cold-call from agent1
responder_init(agent2, [msg(agent2, ack(X2?)) | X3?]) :- bind_done(X4)
bind_done(X4) :- true
```

Problems:
- No agent prefix on scheduler trace lines (reductions, suspensions, failures).
- MAD debug prints (`[MAD agentId]`) mixed with GLP-level trace.
- No separation between GLP-level events and infrastructure events.
- No way to filter by agent.
- No way to suppress infrastructure noise.

---

## 3. Design

### 3.1 Trace Levels

Two levels of trace output:

| Level | Content | Default |
|-------|---------|---------|
| `glp` | Reductions, suspensions, failures — the GLP computation trace | On |
| `mad` | MAD infrastructure: send, globalize, localize, message routing | Off |

The `glp` level is what the REPL shows. The `mad` level is the `[MAD agentId]` debug output.

### 3.2 Output Format

Every trace line is prefixed with the agent ID:

```
[agent1] sender_init(agent1, X1?) :- send_to_net([msg(agent2, ack(X2))]), wait_response(X3?)
[agent1] send_to_net([msg(agent2, ack(X2))]) :- global_send(...), send_to_net([])
[agent1] wait_response(X3?) → suspended
[agent2] responder_init(agent2, X1?) → suspended
[agent2] responder_init(agent2, [msg(agent2, ack(X2?)) | X3?]) :- bind_done(X4)
[agent2] bind_done(X4) :- true
```

At `mad` level, infrastructure events are also prefixed and shown:

```
[agent1] send: globalized term = msg(Const(agent2),ack(_w(Const(agent1),Const(1))))
[agent2] localize: _w(agent1, 1) → fresh pair (writer=6, reader=7)
```

### 3.3 Message Events

When a message is sent or received, a special trace line marks the event. This makes inter-agent dataflow visible:

```
[agent1] ← msg from agent2: assignment _w(agent1, 1) := done
[agent2] → msg to agent1: assignment _w(agent1, 1) := done
```

The arrow direction indicates the agent's perspective: `→` for outgoing, `←` for incoming.

### 3.4 Agent Ordering

Within a single tick, all trace lines from one agent are grouped together before moving to the next agent. Between ticks, a separator line is printed:

```
--- tick 1 ---
[agent1] sender_init(agent1, X1?) :- send_to_net([msg(agent2, ack(X2))]), wait_response(X3?)
[agent1] send_to_net([msg(agent2, ack(X2))]) :- ...
[agent1] wait_response(X3?) → suspended
[agent1] → msg to agent2: assignment _w(agent2, 0) := [msg(agent2, ack(_w(agent1, 1))) | ...]
[agent2] responder_init(agent2, X1?) → suspended
--- tick 2 ---
[agent2] ← msg from agent1: assignment _w(agent2, 0) := [msg(agent2, ack(_w(agent1, 1))) | ...]
[agent2] responder_init(agent2, [msg(agent2, ack(X2?)) | X3?]) :- bind_done(X4)
[agent2] bind_done(X4) :- true
[agent2] → msg to agent1: assignment _w(agent1, 1) := done
[agent1] ← msg from agent1: assignment _w(agent1, 1) := done
[agent1] wait_response(done) :- true
```

Note: this ordering requires each isolate to collect its trace lines and send them back to the main isolate, rather than printing directly. See Section 4.

---

## 4. Implementation

### 4.1 Trace Collection in Agent Isolate

Currently each isolate prints directly to stdout via `print()`. To enable structured trace output, each isolate collects trace lines into a buffer and sends them back to the main isolate as part of the `Status` message.

```dart
class Status extends IsolateMessage {
  final String agentId;
  final String status;
  final int goalCount;
  final List<String> traceLines;  // NEW: collected trace output
}
```

### 4.2 Scheduler Trace Callback

Replace the `print()` calls in the `onReduction` callback with a configurable trace sink. The scheduler already receives callbacks — extend them to write to a list instead of stdout:

```dart
// In agent isolate entry:
final traceLines = <String>[];

final scheduler = Scheduler(
  rt: runtime,
  runners: {'main': runner},
  traceSink: (String line) => traceLines.add('[$agentId] $line'),
);
```

The `Scheduler` adds `traceSink` as an optional parameter. When non-null, all trace output (reductions, suspensions, failures) goes through it instead of `print()`.

### 4.3 MAD Trace Sink

Similarly, `MadContext` currently uses `print('[MAD $agentId] ...')`. Replace with a configurable sink:

```dart
class MadContext {
  void Function(String)? traceSink;

  void _trace(String msg) {
    traceSink?.call(msg);
  }
}
```

The `mad`-level trace is only collected when the `mad` trace level is enabled.

### 4.4 Main Isolate Rendering

The `IsolateManager` collects trace lines from each agent per tick and renders them in agent-grouped order:

```dart
void _renderTrace(int tick, Map<String, List<String>> agentTraces) {
  print('--- tick $tick ---');
  for (final agentId in agentTraces.keys.toList()..sort()) {
    for (final line in agentTraces[agentId]!) {
      print(line);
    }
  }
}
```

### 4.5 Test Harness Integration

`runGlpTest` in `multiagent_glp_test.dart` gets trace control parameters:

```dart
Future<void> runGlpTest(
  String glpFile, {
  int maxTicks = 50,
  int tickDelayMs = 50,
  bool traceGlp = false,   // GLP-level trace (reductions, suspensions)
  bool traceMad = false,    // MAD infrastructure trace
}) async {
  ...
}
```

When both are false (default), no trace output — tests run silently. When `traceGlp` is true, the REPL-style reduction trace is shown with agent prefixes.

---

## 5. Trace Filtering

### 5.1 Per-Agent Filtering

An optional `traceAgents` parameter selects which agents to trace:

```dart
Future<void> runGlpTest(
  String glpFile, {
  ...
  Set<String>? traceAgents,  // null = all agents
}) async {
```

### 5.2 Command-Line Filtering

When running tests via `dart test`, grep-based filtering already works:

```bash
dart test test/multiagent/multiagent_glp_test.dart --name "pipeline" 2>&1 | grep "^\[agent2\]"
```

With the agent prefix on every line, this filters cleanly to one agent's trace.

---

## 6. Comparison with REPL

| Feature | REPL | Multi-Agent Trace |
|---------|------|------------------|
| Reduction trace | `head :- body` | `[agent] head :- body` |
| Suspension trace | `goal → suspended` | `[agent] goal → suspended` |
| Failure trace | `goal → failed` | `[agent] goal → failed` |
| Toggle | `:trace` command | `traceGlp` parameter |
| MAD infrastructure | N/A | `traceMad` parameter |
| Message events | N/A | `[agent] → msg to ...` / `[agent] ← msg from ...` |
| Tick boundaries | N/A | `--- tick N ---` |

The per-agent GLP trace should be identical to what the REPL would show if that agent were running alone — same format, same information, just prefixed with the agent ID.
