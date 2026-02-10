# Bug Report: send-globalise-localise

**Date**: 2026-02-10

## Minimal Program

File: `programs/typed_book/multiagent_tests/three_agent_pipeline_boot.glp`

```prolog
procedure boot.
boot :-
    producer_init(agent1, _)@agent1,
    transformer_init(agent2, _)@agent2,
    consumer_init(agent3, _)@agent3.

procedure producer_init(_?, _?).
producer_init(_, _) :-
    send_to_net([msg(agent2, data([1,2,3]))]).

procedure transformer_init(_?, _?).
transformer_init(_, [msg(_, data(Xs))|_]) :-
    ground(Xs?) |
    transform(Xs?, Ys),
    send_to_net([msg(agent3, data(Ys?))]).

procedure transform(_?, _).
transform([X|Xs], [got(X?)|Ys?]) :- transform(Xs?, Ys).
transform([], []).

procedure consumer_init(_?, _?).
consumer_init(_, [msg(_, data(Ys))|_]) :-
    ground(Ys?) |
    wrap(Ys?, Result),
    consume(Result?).

procedure wrap(_?, _).
wrap(List, done(List?)) :- ground(List?) | true.

procedure consume(_?).
consume(_) :- true.
```

## How to Run

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/multiagent_glp_test.dart --name "pipeline"
```

## How to See the Bug

```bash
dart test test/multiagent/multiagent_glp_test.dart --name "pipeline" 2>&1 | grep -E "send:.*found|send:.*glob|consumer_init|registered|_r\("
```

Output:

```
[MAD agent2] send: found 1 variables in term
[MAD agent2] send: globalized term = msg(Const(agent3),data(.(got(Const(1)),.(got(Const(2)),_r(Const(agent2),Const(1))))))
[MAD agent3] registered global_send goal: _r(agent2, 1) -> agent2
consumer_init(agent3, [msg(agent3, data([got(1), got(2) | X2])) | X3?]) → failed
```

Key lines:

1. `send: found 1 variables in term` — agent2's `_send` finds an unbound variable in the term being sent
2. `globalized term = ...._r(Const(agent2),Const(1))...` — the variable is globalized as `_r` (reader global name)
3. `registered global_send goal: _r(agent2, 1) -> agent2` — agent3 localizes `_r(agent2,1)` and creates a writer + spawns global_send back to agent2
4. `consumer_init(agent3, ...) → failed` — agent3's `ground(Ys?)` finds the unbound writer and fails

## What Happens Step by Step

### Agent2 (transformer)

Agent2's body has two concurrent goals:

```prolog
transform(Xs?, Ys),
send_to_net([msg(agent3, data(Ys?))]).
```

`transform` is recursive and takes multiple reductions. `send_to_net` races ahead. When `_send` fires, the term is `data([got(1), got(2) | X?])` — partially built. The tail `X?` is an unbound reader.

### `_send` at agent2

`_send` calls `_extractTermVarsRecursive` which walks the term. It encounters the unbound reader `X?` (a VarRef at a reader address). It records it as `TermVar.reader`.

The relevant code is in `glp_runtime/lib/multiagent/mad_context.dart` lines 154-168:

```dart
void _extractTermVarsRecursive(Term term, List<TermVar> result) {
  if (term is VarRef) {
    final isReader = runtime.heap.isReader(term.addr);
    if (isReader) {
      result.add(TermVar.reader(term.addr));
    } else {
      result.add(TermVar.writer(term.addr));
    }
  } else if (term is StructTerm) {
    for (final arg in term.args) {
      _extractTermVarsRecursive(arg, result);
    }
  }
}
```

### Globalize at agent2

`globalize()` in `glp_runtime/lib/multiagent/mad_helpers.dart` lines 163-197 receives this reader variable. Per spec 5.1 case 2, it produces `_r(agent2, 1)`.

### Localize at agent3

`localize()` in `glp_runtime/lib/multiagent/mad_helpers.dart` lines 212-255 receives `_r(agent2, 1)`. Per spec 5.2 case 2, it creates a fresh pair and puts the **writer** into agent3's term.

### Agent3

Agent3's `consumer_init` head-matches the incoming message. `Ys` gets bound to the partially-built list, which contains the localized **writer**. The guard `ground(Ys?)` traverses the term, finds the unbound writer, and definitively fails (unbound writer = definitive failure per SRSW).

The goal should have suspended (waiting for the value to arrive), but it fails because the localized variable is a writer instead of a reader.

## Complement Program: Send Writer, Receiver Writes Back

File: `programs/typed_book/multiagent_tests/writer_response_boot.glp`

```prolog
procedure boot.
boot :-
    sender_init(agent1, _)@agent1,
    responder_init(agent2, _)@agent2.

procedure sender_init(_?, _?).
sender_init(_, _) :-
    send_to_net([msg(agent2, ack(Resp))]),
    wait_response(Resp?).

procedure wait_response(_?).
wait_response(done).

procedure responder_init(_?, _?).
responder_init(_, [msg(_, ack(X?))|_]) :-
    bind_done(X).

procedure bind_done(_).
bind_done(done).
```

### How to Run

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/multiagent_glp_test.dart --name "writer response"
```

### How to See the Bug

```bash
dart test test/multiagent/multiagent_glp_test.dart --name "writer response" 2>&1 | grep -E "send:.*found|send:.*glob|responder_init|bind_done|registered|_w\("
```

Output:

```
[MAD agent1] send: found 1 variables in term
[MAD agent1] send: registering global_send goal for _w(agent1, 1)
[MAD agent1] send: globalized term = msg(Const(agent2),ack(_w(Const(agent1),Const(1))))
responder_init(agent2, [msg(agent2, ack(X2?)) | X3?]) :- bind_done(X4)
bind_done(X4) :- true
```

### What Happens

1. Agent1 sends `ack(Resp)` where Resp is a **writer**. Globalize produces `_w(agent1, 1)`.
2. Agent1 registers `global_send` goal watching `_w(agent1, 1)` — but `TermVar.pairedReaderAddr` returns the writer address itself (bug: should be the actual paired reader).
3. Agent2 localizes `_w(agent1, 1)`: creates fresh pair (writer@6, reader@7), adds `LocalizeEntry(writer=6, agent1, 1)`, substitutes Var@7 (reader) into the term.
4. Agent2 head-matches `ack(X?)` binding X? to Var@7 (reader). `bind_done(X)` binds writer@6 to `done`.
5. **Missing**: No `onBind` callback on writer@6 at agent2. The `LocalizeEntry` in the table is designed for **receiving** `_w(agent1, 1) := T` from agent1, not for detecting local writes that need to go back.
6. Agent2 completes. Agent1 suspended forever on `wait_response(Resp?)`.

### Two Independent Bugs

**Bug A — `TermVar.pairedReaderAddr`**: Returns `addr` (the writer address) instead of the actual paired reader address. The `GlobalSendSpawn` at agent1 gets `readerAddr: writerAddr` instead of `readerAddr: writerAddr+1`. This means the `onBind` callback and `GlobalSendRegistry` goal are registered on the wrong address.

Code: `glp_runtime/lib/multiagent/mad_helpers.dart` line 98:
```dart
int get pairedReaderAddr => addr;  // BUG: should use heap cross-pointer
```

**Bug B — No local-write-back mechanism at receiver**: When agent2 localizes `_w(agent1, 1)` and gets a fresh pair, there is no `onBind` callback registered on the fresh writer. When the receiving agent binds that writer locally, nothing sends the value back to agent1. The `LocalizeEntry` only handles the **incoming** direction (agent1 → agent2), not the **outgoing** direction (agent2 → agent1).

The spec says localize-`_w` creates `(Y_q, Y_q?)` and adds entry `(Y_q, p, i)` — "q will receive the assignment on this link." But in the writer_response case, it is **q** that assigns, and the assignment needs to flow **out** to p.

## Files Involved

- `glp_runtime/lib/multiagent/mad_context.dart` — `_extractTermVarsRecursive` (lines 154-168), `send` (lines 474-530)
- `glp_runtime/lib/multiagent/mad_helpers.dart` — `globalize` (lines 163-197), `localize` (lines 212-255), `TermVar.pairedReaderAddr` (line 98)
- `glp_runtime/lib/runtime/body_kernels.dart` — `sendKernel` / `_deepDeref` (lines 413-432, 692-770)
- `glp_runtime/lib/runtime/heap_fcp.dart` — `allocateVariable` (lines 85-97), `onBind` (lines 596-605)
- `programs/typed_book/multiagent_tests/three_agent_pipeline_boot.glp` — sends reader test
- `programs/typed_book/multiagent_tests/writer_response_boot.glp` — sends writer test
