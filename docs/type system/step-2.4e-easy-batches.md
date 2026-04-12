# Step 2.4e Batch Instructions — Easy Batches

**Prerequisite**: Steps 2.4a–d are done, 390/390 tests pass.

**Rule**: After each batch, run `bash test/run_all_tests.sh`. All 390 tests must pass. Commit after each batch.

**Principle**: Every local `Stream ::= [] ; [_|Stream].` or `Channel ::= ch(Stream, Stream?).` should be removed and replaced by parameterized root types. Every `_` in type definitions should become a concrete type or type parameter. Every bare `Stream`/`Channel` in procedure declarations should become parameterized.

---

## Batch A: `programs/paper/` (5 files)

### paper/merge.glp
References root `Stream`. Change:
```
procedure merge(Stream?, Stream?, Stream).
```
→
```
procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
```

### paper/monitor.glp
`CounterStream` is already precise (uses `CounterCall`, not `_`). But it should use `Stream(CounterCall)` from root instead of a local copy. Remove local `CounterStream` definition and update proc decls:

```
CounterStream ::= [] ; [CounterCall|CounterStream].

procedure monitor(CounterStream?).
procedure monitor(Integer?, CounterStream?).
```
→
```
procedure monitor(Stream(CounterCall)?).
procedure monitor(Integer?, Stream(CounterCall)?).
```

### paper/channel.glp
Defines local `MyStream ::= [] ; [_|MyStream].` and `MyChannel ::= ch(MyStream?, MyStream).` These are imprecise local copies. But this is a paper example demonstrating channel construction, and the `my_*` procedures exist to show how channels work. Convert to use parameterized root types:

Remove `MyStream` and `MyChannel` definitions. Change:
```
MyStream ::= [] ; [_|MyStream].
MyChannel ::= ch(MyStream?, MyStream).

procedure my_new_channel(MyChannel, MyChannel).
procedure my_send(_, MyChannel?, MyChannel).
procedure my_receive(_?, MyChannel?, MyChannel).
```
→
```
procedure my_new_channel(Channel(X, Y), Channel(Y, X)).
procedure my_send(X?, Channel(Stream(X))?, Channel(Stream(X))).
procedure my_receive(X, Channel(Stream(X))?, Channel(Stream(X))).
```

### paper/dl_append.glp
Same approach — remove local types, use root:
```
MyStream ::= [] ; [_|MyStream].
MyDiffList ::= MyStream? \ MyStream.

procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
```
→
```
procedure my_dl_append(DiffList(X)?, DiffList(X)?, DiffList(X)).
```

### paper/coop_stream.glp
`CoopStream` is a genuine custom interactive type (not a `Stream(X)` instantiation — it has mode inversion in one alternative). Already precise (uses `Integer`, not `_`). No changes needed. Does not reference root `Stream`.

---

## Batch B: `programs/typed_book/recursive/list_processing/` (2 files)

### nth.glp
```
NonEmptyList ::= [_ | Stream].
procedure nth(Number?, NonEmptyList?, _).
```
→ `NonEmptyList` is `OpenStream` from root. `nth` is generic (works on any element type). Use parameterized types:
```
procedure nth(Number?, OpenStream(X)?, X).
```
Remove the local `NonEmptyList` definition.

Also fix `reduce` clauses to match — they reference `nth` with the same signature. Check that the reduce proc decl `procedure reduce(_?, _).` still works with the changed nth signature. Since reduce is a meta-procedure with `_` types, it should be fine.

### member.glp
Same approach:
```
NonEmptyList ::= [_ | Stream].
procedure member(_?, NonEmptyList?).
```
→
```
procedure member(X?, OpenStream(X)?).
```
Remove local `NonEmptyList` definition. Same consideration for `reduce`.

---

## Batch C: `programs/typed_book/meta/` (3 files)

### plain/plain_meta.glp
```
procedure merge(Stream?, Stream?, Stream).
```
→
```
procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
```
This is an external declaration (no clauses) — it declares the dependency. The reduce clauses encode merge as data; they use `_` types in the reduce declaration and are fine.

### debugging/runtime_control_meta.glp
Same merge change. Also has local types `ControlCmd`, `ControlList`, `DumpList` — these are already precise EXCEPT `DumpList ::= [] ; [_ | DumpList].` which is a list of anything. This is genuinely `Stream(_)` in spirit — a dump list can contain any goal. For tight typing, change to `Stream(_)`.

Wait — we're eliminating `_` in user type definitions. But `DumpList` genuinely contains arbitrary terms (aborted goals). The tight typing discipline says we don't use `_` in OUR code, but system builtins that accept any term are the exception. This is a meta-interpreter dumping arbitrary goals — `_` is semantically correct.

Actually the plan says: "we do not use them in our code". So even `DumpList` should avoid `_`. But what would the element type be? It's arbitrary goals. We'd need a `Goal` type. The plan's future work section says: "Meta-interpreters... cannot be precisely typed with the current system. Their conversion to parameterized types is deferred."

So for meta-interpreters: convert what we can (merge declarations), leave `_` in meta-level types (DumpList, reduce, run) as-is. Note this in the commit message.

### enhanced/snapshot_meta_cp.glp
Same merge change. `Chain ::= chain(Stream?, Stream).` uses bare root `Stream`. Since `Chain` carries arbitrary process dump data, this is another meta-interpreter `_` case. Change `Stream` to `Stream(_)`:
```
Chain ::= chain(Stream(_)?, Stream(_)).
```
Wait, but we're eliminating `_` — the plan says meta-interpreters are deferred.

Actually let me re-read the plan. Step 2.7 says: "System-level builtins in self.glp (under -mode(system)) that genuinely accept any term — such as ground(_?), =(_?, _), =?=(_?, _?) — are the only exception." And the future work section says: "Meta-interpreters: These manipulate arbitrary terms and cannot be precisely typed with the current system."

So for now: convert merge declarations, leave meta-level `_` and bare `Stream` in meta-interpreter types with a comment explaining why. When root monomorphic `Stream` is removed, `Chain ::= chain(Stream?, Stream).` will break — so we need to define a local type or use `Stream(_)`. Since `Stream(_)` expands to the same thing as old `Stream`, this is acceptable as a documented exception.

Change:
```
Chain ::= chain(Stream?, Stream).
procedure merge(Stream?, Stream?, Stream).
```
→
```
%% Chain carries arbitrary terms — meta-interpreter exception to tight typing
Chain ::= chain(Stream(_)?, Stream(_)).
procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
```

Also: `ShotList ::= [] ; [_ | ShotList].` → same issue. Change to `Stream(_)` or leave with comment. Since `ShotList` IS just `Stream(_)`, replace with `Stream(_)` reference:
Actually `ShotList` won't break because it's a local definition that doesn't reference root `Stream`. So leave it.

---

## Batch D: `programs/tests/` (3 files)

### tests/type_channel_test.glp
Defines local `Stream` and `Channel` that shadow root. Must be converted:
```
Stream ::= [] ; [_ | Stream].
Channel ::= ch(Stream?, Stream).
Response ::= accept(Channel) ; no.
procedure handle_response(Response?, Stream?, Stream).
procedure merge(Stream?, Stream?, Stream).
```

This test is about channel type checking behavior. The local `Stream` with `_` is imprecise. For a type test, we should use a concrete message type. Define a simple one:
```
Msg ::= hello ; goodbye.
MsgChannel ::= Channel(Stream(Msg), Stream(Msg)).
Response ::= accept(MsgChannel) ; no.
procedure handle_response(Response?, Stream(Msg)?, Stream(Msg)).
procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
```
Remove local `Stream` and `Channel` definitions.

### tests/typed/cssg_precise/typed_social_agent.glp
This is a big file (the monolithic CSSG agent with precise types). Has many bare `Stream` and `Channel` references. Multiple issues:
- `OutputEntry ::= output(OutputKey, Stream?).` → needs OutputStream
- `procedure merge(Stream?, Stream?, Stream).` → parameterized
- `IntroResult ::= intro_result(Constant, Channel)` → IntroChannel
- `UserInMsg ::= ... ; intro_result(Constant, Channel)` → IntroChannel
- `procedure intro_await_peer(Constant?, Channel?, IntroResult).` → IntroChannel
- `procedure add_output(OutputKey?, Stream, ...)` → OutputStream
- `PendingValue ::= ... ; channel(_)` → channel(IntroChannel)

This file needs the same treatment as cssg_modules. It's a substantial conversion — defer to Batch F (typed_book/cssg).

### test_agent_init.glp
```
procedure agent_init(_?, Channel?, Channel?).
```
Uses root `Channel`. This is a simple test of channel pattern matching. Convert to parameterized:
```
Msg ::= hello.
procedure agent_init(_?, Channel(Stream(Msg), Stream(Msg))?, Channel(Stream(Msg), Stream(Msg))?).
```
That's very verbose. Alternatively define a local alias:
```
Msg ::= hello.
TestChannel ::= Channel(Stream(Msg), Stream(Msg)).
procedure agent_init(_?, TestChannel?, TestChannel?).
```
Actually `Channel(Stream(Msg), Stream(Msg))` is odd — both sides carry the same message type. The original test just checks that `ch(Reader?, Writer)` pattern-matches correctly. We can simplify:
```
Msg ::= hello.
procedure agent_init(Constant?, Channel(Stream(Msg), Stream(Msg))?, Channel(Stream(Msg), Stream(Msg))?).
```
Or even simpler — this test doesn't really need message types, it just needs any channel. But we can't use `_` under tight typing. Use a minimal concrete type.

---

## Batch E: Stragglers

### cssg_modules/mad_boot.glp
```
procedure tee(Stream?, Stream, Stream).
```
→ Already parameterized by the boot module: `procedure tee(Stream(X)?, Stream(X), Stream(X)).`

Wait, let me check — the current file has `procedure tee(Stream?, Stream, Stream).` which references root `Stream`. Change to parameterized. The rest of mad_boot.glp has no procedure declarations (it's system mode using kernels). Check for other bare type references.

### typed_book/test_friend.glp
```
FriendEntry ::= friend(String, Stream?).
```
Uses root `Stream`. The friend entry carries a stream of... what? Looking at the procedure `lookup_send_step`, the second arg is `_?` (any message). This is a simplified test. For tight typing, define a message type:
```
FriendMsg ::= msg(Constant, Constant).
FriendEntry ::= friend(String, Stream(FriendMsg)?).
FriendsList ::= [] ; [FriendEntry|FriendsList].
procedure lookup_send_step(String?, FriendMsg?, FriendsList?, FriendsList).
```

---

## Execution order

1. Batch A (paper/) — run tests, commit
2. Batch B (recursive/) — run tests, commit
3. Batch C (meta/) — run tests, commit
4. Batch D partial (tests/type_channel_test.glp + test_agent_init.glp only, defer cssg_precise) — run tests, commit
5. Batch E (mad_boot.glp + test_friend.glp) — run tests, commit

After these 5 commits, proceed to the harder batches (separate instruction file).
