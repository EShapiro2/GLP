# Handover: the polymorphic-channel typing gap

**For:** the next Claude Code session
**Repo:** `/Users/udi/Grassroots/GLP`
**Mode:** discussion / investigation first — do NOT change the type system, the
language, or any `.glp` file without Udi's explicit approval (CLAUDE.md, DISCIPLINE §1.14).
**Date raised:** 2026-06-16

---

## 1. One-paragraph summary

A reader/writer polarity bug in `typed_ui_mediator.glp` type-checked cleanly yet
could never fire at runtime (the `befriend` receive clause suspended forever, so no
inbox card reached the Dart UI). The clause-level bug is already fixed and approved.
What remains open, and what this handover is about, is the **root question**: why did
the type checker not reject the mismatch? We established it is **not** a separate-vs-
single-project compilation artifact and **not** a checker bug in duality enforcement.
It is a **coverage gap**: the mediator types its agent-facing channel as a polymorphic
`Channel(X, Y)`, so the message protocol crossing that channel is never pinned to a
concrete type, and the duality check that the checker *does* perform passes vacuously
at the abstract level. The polarity clash only exists at the concrete instantiation,
which the mediator never names.

---

## 2. What is already resolved (do not reopen)

- **The runtime bug.** `typed_ui_mediator.glp`, the cold-call `befriend` receive clause,
  had writer/reader polarity reversed. Fixed and approved earlier this session. On disk
  now (verified):

  ```prolog
  PendingValue ::= response(Response?) ; channel(FriendChannel?) ; error.

  ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
      receive(msg(agent, '_user', befriend(From, Resp)),
              AgentCh?, AgentCh1),
      ground(From?) |
      send(befriend(From?, req(N?)), UserCh?, UserCh1),
      N1 := N? + 1,
      ui_mediator(Id?, AgentCh1?, UserCh1?,
          [pending(req(N?), response(Resp?)) | Ps?], N1?).
  ```

  (The `befriend_intro` clause had the same inversion on `Ch`/`channel(Ch)`; confirm it
  was fixed too.)

- **The Dart side.** The two-surface GSG runtime renders a card from a `befriend(alice,
  req(1))` notify and round-trips `decision`. Not in scope here.

- **Two hypotheses ruled out:**
  1. *"It escaped because modules are compiled separately, not as a project."* **False.**
     See the experiment in §4 — single-unit compilation *does* enforce producer/consumer
     duality and rejects the mismatch.
  2. *"The duality checker is buggy."* **False.** It fires correctly when the types are
     concrete.

---

## 3. The actual gap

The agent and the mediator never call each other (`M # goal(...)`); they exchange
messages over a channel wired in `play_ui_boot.glp`:

```prolog
agent(Id?, AgentIn?, NetIn?, [output('_user', AgentToUser), output('_net', NetOut)]),
ui_mediator(Id?, ch(AgentToUser?, AgentIn), ch(UserIn?, UserOut), [], 1),
```

The mediator's declaration types both its channels polymorphically:

```prolog
procedure ui_mediator(Constant?, Channel(X, Y)?, Channel(X, Y)?, PendingList?, Constant?).
```

`Channel(In, Out) ::= ch(In, Out?)` (root `self.glp`). With the agent-facing channel at
`Channel(X, Y)`, the message type is the **type variable `X`**, never instantiated to a
concrete protocol type. The agent produces concrete messages — note the agent file
declares the *same* functor at two polarities, by design:

```prolog
%% typed_social_agent.glp
AgentContent  ::= befriend(Constant, Response?) ...   %% reader  (msg(agent,'_user',...))
OutputContent ::= befriend(Constant, Response)  ...   %% writer  (the _user OUTPUT stream)
```

The mediator pattern-matches `befriend(From, Resp)` out of an `X`-typed stream. Because
`X` is abstract at the mediator's checking site, the constructor's argument polarity is
fixed only by how the mediator's own clause uses it — never reconciled against the
agent's `OutputContent.befriend(Constant, Response)`. So the duality obligation is
discharged at the `X` level (vacuously) and the concrete clash is never expressed as a
constraint. That is the whole mechanism.

The mediator additionally **hand-duplicates** the shared types (its own comment:
`%% Friend channel types (duplicated from typed_social_agent.glp)`), and the copies have
already drifted (`PendingValue.response` is `Response?` here vs `Response` in the agent).
Duplication makes drift possible; the polymorphic channel makes it invisible.

---

## 4. Evidence — the minimal experiment (reproducible)

File: `programs/tests/min_polarity_bug.glp` (already in the repo). It puts a producer
(`Stream(ProdMsg)`, `befriend(Constant, Response)`) and a consumer (`Stream(ConsMsg)?`,
`befriend(Constant, Response?)`) of one shared stream `S` in a **single unit**, with no
polymorphic channel.

Run from `/Users/udi/Grassroots/GLP/glp_runtime/`:

```bash
echo -e 'load ../programs/tests/min_polarity_bug.glp\n:quit' | dart run bin/glp_repl.dart
```

**Result: type error** (this is the point — concrete types ARE checked):

```
Type checking failed:
  Head of producer is not well-typed:
  reader requires down (consume), got up (produce)
  Variable pair (S, S?) not dual across clause:
  writer type Stream<ProdMsg> is not a subtype of Stream<ConsMsg>
```

So: concrete producer/consumer duality is enforced within one unit. The real bug
escaped only because the mediator's channel is `Channel(X, Y)` (abstract), not because
of how files are compiled. A next session should keep this file as the regression anchor:
the goal of any fix is that the *mediator* configuration becomes as checkable as this
minimal one.

---

## 5. Open question for discussion (do not implement yet)

How should the protocol crossing the agent↔mediator channel be made concrete enough that
the checker sees the duality, without losing the genericity the mediator needs to serve
multiple platforms? Candidate directions to evaluate, with trade-offs, before proposing
a spec amendment:

1. **Concrete channel element type.** Type the mediator's agent-facing channel as
   `Channel(Stream(AgentToMediatorMsg), Stream(MediatorToAgentMsg))` (or the precise
   pair) instead of `Channel(X, Y)`. Then `receive(... befriend(From, Resp) ...)` is
   checked against the shared constructor, and a polarity error is a load-time type
   error. Question: does the mediator genuinely need polymorphism here, or was
   `Channel(X, Y)` just convenience?

2. **Single source of truth for the protocol type.** Move the agent↔mediator message
   type (and `Response`, `FriendChannel`) into `programs/book/social_graph/self.glp`
   (manual §19.6 — the example literally lists `self.glp — shared types (Response,…)`
   with `agent.glp` and `mediator.glp` both seeing it). Delete the duplicated block from
   the mediator. This removes drift; combined with (1) it also makes the seam checked.
   Note: `programs/book/social_graph/` currently has **no** `self.glp`.

3. **Language/checker question (Udi to decide, do not touch без approval).** Should a
   *polymorphic channel whose messages are pattern-matched on concrete constructors* be
   a type-discipline smell the checker can warn on? This edges into language design
   (DISCIPLINE §1.14) — raise as a question, do not prototype.

The fix almost certainly combines (1)+(2). But per spec-first discipline: identify/quote
the governing spec (typed-glp-manual §19.6, §19.7, §20.3; the moded-types paper's duality
rule) and confirm with Udi which direction is sanctioned before editing any `.glp`.

---

## 6. Verify-on-disk checklist for the next session (post-compaction safe)

1. Re-read this file, then verify against disk before trusting any claim:
   - `programs/book/social_graph/typed_ui_mediator.glp` — the `ui_mediator` declaration
     (`Channel(X, Y)`), the `befriend` / `befriend_intro` clauses, the duplicated-types
     comment, the `PendingValue` polarity.
   - `programs/book/social_graph/typed_social_agent.glp` — `AgentContent` vs
     `OutputContent` befriend polarity.
   - `programs/book/social_graph/play_ui_boot.glp` — the channel wiring.
   - confirm there is still no `programs/book/social_graph/self.glp`.
2. Re-run `programs/tests/min_polarity_bug.glp` and confirm it still errors.
3. Run the baseline before proposing anything: `bash test/run_all_tests.sh >
   /private/tmp/glp-baseline.txt 2>&1`, confirm `ALL TESTS PASSED!`.
4. Do NOT modify `.glp`, the checker, or the language to "fix" this until the direction
   in §5 is agreed with Udi.

---

## 7. Context: why this matters beyond one clause

This is the implementation grounding for paper §7.4 (the generic UI / mediator-as-
contract). The paper's claim is that the mediator's GLP types ARE the UI manifest and
the boundary is type-safe. A polymorphic channel at exactly that boundary means the
agent↔mediator protocol is, today, not type-checked end to end — which is a gap between
what §7.4 asserts and what the implementation enforces. Closing it strengthens both the
code and the paper's claim. Paper repo (separate): UIVE; do not put `.glp` there.
