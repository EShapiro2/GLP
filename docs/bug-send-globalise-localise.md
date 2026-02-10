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

**Bug B — Not a bug**: Previously described as "no local-write-back mechanism at receiver." A write-back mechanism was added and then removed. The data flow for `_w(p, i)` is strictly p→q per the spec. There is no reverse flow. If q needs to write back to p, the program must use `_r(p, i)` (export the reader, so localize spawns a `global_send` at q). The test program `writer_response_boot.glp` exports the wrong polarity — it sends a writer expecting the receiver to write back, but should send a reader for q→p flow.

## Third Program: Send Unbound Reader, Bind Later

File: `programs/typed_book/multiagent_tests/send_reader_boot.glp`

```prolog
procedure boot.
boot :-
    sender_init(agent1, _)@agent1,
    receiver_init(agent2, _?)@agent2.

procedure sender_init(_?, _?).
sender_init(_, _) :-
    send_to_net([msg(agent2, data(X?))]),
    bind_later(X).

procedure bind_later(_).
bind_later(Done?) :- wait(1000) | done(Done).

procedure done(_).
done(done).

procedure receiver_init(_?, _?).
receiver_init(_, [msg(_, data(Y))|_]) :-
    got_it(Y?).

procedure got_it(_?).
got_it(done).
```

### What Should Happen

1. Agent1 sends `data(X?)` where `X?` is an unbound reader. The `wait(1000)` guard ensures `bind_later` does not reduce before `_send` serializes the term.
2. Globalize produces `_r(agent1, 1)` for the unbound reader, with a `GlobalizeEntry(Y, agent2)` at index 1.
3. Agent2 receives the message, localizes `_r(agent1, 1)`: creates fresh pair `(Z, Z?)`, puts writer `Z` into the term, spawns `global_send(Z?, _r(agent1,1), agent1)`.
4. After 1000ms, agent1's `bind_later` fires: `done(Done)` binds the writer paired with `X?` to `done`.
5. Agent1's `onBind` fires a `global_send` that sends `_r(agent1, 1) := done` to agent2.
6. Agent2 receives the assignment, finds `GlobalizeEntry` — wait, agent2 is not the globalizer, agent1 is. So this goes to agent1's `_handleReaderAssignment` which finds the entry at index 1, binds the writer Y to `done`. But Y's reader is `X?` which was the original reader in agent1's term — so `X?` becomes known.

Actually the flow is: agent2's `global_send` fires when Z is bound. But Z is the writer in agent2's term — nothing in agent2 binds Z. The value flows from agent1 to agent2.

Correct flow:
1. Agent1 globalizes reader `X?` as `_r(agent1, 1)`, creates `GlobalizeEntry(writerOfX, agent2)` at index 1.
2. Agent2 localizes `_r(agent1, 1)`: creates fresh `(Z, Z?)`, spawns `global_send(Z?, _r(agent1,1), agent1)`. Writer `Z` goes into agent2's term.
3. Agent1 binds writer of X to `done`. The `onBind` callback on agent1 fires (for the globalize-writer path), but no — globalize-reader creates an **entry**, not a spawn. There is no `onBind` on agent1 for the globalize-reader path.
4. Instead, the `GlobalizeEntry` at agent1 index 1 has `(writerOfX, agent2)`. When writerOfX is bound, **someone** needs to detect this and send `_r(agent1, 1) := done` to agent2.

### Bug C — Not a bug (globalize-reader has no onBind by design)

Previously described as "no onBind for globalize-reader path." Per the spec (Section 5.1), when p globalizes reader Y? as `_r(p, i)`, p creates an entry (Y, q) and waits. No goal is spawned and no onBind is registered at p — this is correct. The `global_send` for `_r(p, i)` is spawned at q by `localize`, not at p.

The test program `send_reader_boot.glp` exports a reader X? expecting p to send the value when the paired writer X is bound. But per the paper, exporting a reader means q→p flow (q sends, p receives). For p→q flow (p assigns), the program should export the writer X instead.

## Fixes Applied

### Fix 1: `localize()` spawn address (Bug in `_r` path)

In `mad_helpers.dart` `localize()`, changed `GlobalSendSpawn` for `_r(p,i)` from `readerAddr: readerAddr` to `readerAddr: writerAddr`. The `readerAddr` field is used as the key for `heap.onBind()`, which is indexed by writer address.

### Fix 2: Removed write-back mechanism (Bug B — not a bug)

The write-back mechanism (`_registerWriteBackCallbacks`, `_sendWriteBack`) was added and then removed. It does not exist in GLP. The `_w(p, i)` flow is strictly p→q. Programs needing q→p flow must export the reader, producing `_r(p, i)`.

### Fix 3: No fix needed (Bug C — not a bug)

Globalize-reader correctly creates only an entry at p, with no onBind and no goal. The `global_send` is spawned at q by `localize`. The test programs need to use the correct polarity.

## Files Involved

- `glp_runtime/lib/multiagent/mad_context.dart` — `_extractTermVarsRecursive`, `send`, `registerGlobalSendSpawns`
- `glp_runtime/lib/multiagent/mad_helpers.dart` — `globalize`, `localize`, `TermVar.pairedReaderAddr`
- `glp_runtime/lib/runtime/body_kernels.dart` — `sendKernel` / `_deepDeref`
- `glp_runtime/lib/runtime/heap_fcp.dart` — `allocateVariable`, `onBind`, `bindVariable`
- `programs/typed_book/multiagent_tests/three_agent_pipeline_boot.glp` — sends partially-bound reader
- `programs/typed_book/multiagent_tests/writer_response_boot.glp` — sends writer, receiver writes back
- `programs/typed_book/multiagent_tests/send_reader_boot.glp` — sends unbound reader, binds later
