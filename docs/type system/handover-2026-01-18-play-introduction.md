# Handover Report: Play Introduction Test SRSW Compliance

**Date**: 2026-01-18
**Author**: Claude
**Status**: Blocked on SRSW analysis
**Previous Session**: Definition 4.5 wildcard fix (well-typed-term.md v0.7)

## Summary

Attempted to create an SRSW-compliant test harness (`play_introduction_test.glp`) for the friend-mediated introduction protocol. Multiple SRSW violations were encountered that appear to stem from fundamental GLP idioms used in the Art-of-GLP-2025 book. Work is blocked pending analysis of whether these are genuine violations or whether the SRSW checker needs adjustment.

## Current State

A partial implementation exists with type and procedure declarations at:
- `/Users/udi/Grassroots/GLP/programs/multiagent/play_alice_bob.glp`

This file contains type declarations, procedure declarations, and clause implementations based on Art-of-GLP-2025 Chapter 8 "Testing with Multiagent Plays."

## Files Referenced

### Source Documents
- **Art-of-GLP-2025**: `/Users/udi/Grassroots/Art-of-GLP-2025/chapters/social_graph.tex` — Section 8 "Testing with Multiagent Plays" contains the canonical play patterns
- **GLP-ICLP-2026 Paper**: `/Users/udi/Grassroots/GLP-ICLP-2026/glp_section_multiagent.tex` — Multiagent GLP specification
- **GLP-ICLP-2026 Appendix**: `/Users/udi/Grassroots/GLP-ICLP-2026/glp_appendix_additional_techniques.tex` — 3-way network switch, channel abstractions

### Type System Specifications
- **Type Environment**: `/Users/udi/Grassroots/GLP/docs/type system/type-environment.md` (v0.8) — Confirms conjunction syntax `(_, _)` is valid
- **Well-Typed Term**: `/Users/udi/Grassroots/GLP/docs/type system/well-typed-term.md` (v0.7) — Updated Definition 4.5 for wildcard handling

### Implementation Files
- **Test File**: `/Users/udi/Grassroots/GLP/programs/multiagent/play_alice_bob.glp` — Current work-in-progress
- **Social Agent**: `/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp` — Working typed social graph implementation

## Type Declarations in play_alice_bob.glp

```prolog
NetMsg ::= msg(_, _, _).
Channel ::= ch(NetMsg?, NetMsg).
AgentChannel ::= (_, Channel).
UserCmd ::= connect(_) | decision(_, _, _).
UserResp ::= befriend(_, _) | connected(_).
BefriendResp ::= accept(Channel) | no.
FriendEntry ::= (_, NetMsg).
FriendList ::= list(FriendEntry).
```

Note: Conjunction syntax `(_, Channel)` is confirmed valid per type-environment.md v0.8.

## Procedure Declarations in play_alice_bob.glp

```prolog
procedure network2(AgentChannel?, AgentChannel?).
procedure play_alice_bob.
procedure agent(_, Channel?, Channel?).
procedure social_graph(_, NetMsg?, FriendList?).
procedure alice_actor(UserResp?, UserCmd).
procedure alice_wait_response(UserResp?, UserCmd).
procedure bob_actor(UserResp?, UserCmd).
procedure bob_done(UserResp?, UserCmd).
procedure response_stream(_?, _, _, NetMsg).
procedure bind_response(_, _, BefriendResp, FriendList?, FriendList, NetMsg?, NetMsg).
procedure handle_accept(_, Channel?, FriendList?, FriendList, NetMsg?, NetMsg).
procedure handle_response(BefriendResp?, _, FriendList?, FriendList, NetMsg?, NetMsg).
procedure add_friend(_, NetMsg, FriendList?, FriendList).
procedure lookup_send(_, NetMsg?, FriendList?, FriendList, _).
procedure tag_stream(_, NetMsg?, NetMsg).
procedure merge(NetMsg?, NetMsg?, NetMsg).
```

## Known SRSW Issues Requiring Analysis

The following patterns from the book generate SRSW violations. Each needs analysis to determine if it is a genuine violation or a valid pattern that the checker should accept.

### Issue 1: Stream Variable Passed to Continuation

**Book pattern** (alice_actor):
```prolog
alice_actor(AgentOut?, [msg(user, alice, connect(bob))|UserIn?]) :-
    alice_wait_response(AgentOut?, UserIn).
```

**Reported violation**: Reader variable `AgentOut?` occurs 2 times without ground guard.

**Question**: Is passing a stream reader to a continuation a valid pattern? The stream is consumed incrementally across recursive calls.

### Issue 2: Guard-Based Channel Destructuring

**Book pattern** (agent):
```prolog
agent(Id, ChUser, ChNet) :-
    ChUser = ch(UserIn, UserOut), ChNet = ch(NetIn, NetOut) |
    merge(UserIn?, NetIn?, In),
    social_graph(Id?, In?, [(user, UserOut), (net, NetOut)]).
```

**Reported violation**: Writer variables `ChUser`, `ChNet`, `UserOut`, `NetOut` occur 2 times.

**Question**: Guard unification `ChUser = ch(UserIn, UserOut)` creates bindings. Should variables bound in guards be treated differently for SRSW purposes?

### Issue 3: Trigger Pattern with known/1

**Book pattern** (response_stream):
```prolog
response_stream(Resp, Target, Id, [msg(Target?, Id?, response(Resp?))]) :-
    known(Resp?) | true.
```

**Reported violation**: Reader variable `Resp?` occurs 2 times without ground guard.

**Question**: The `known(Resp?)` guard succeeds when `Resp` is bound. Does this implicitly ground `Resp?` for SRSW purposes in the body?

### Issue 4: Data Structures Containing Output Channels

**Pattern** (agent body):
```prolog
social_graph(Id?, In?, [(user, UserOut), (net, NetOut)]).
```

**Question**: `UserOut` and `NetOut` are writers that appear in the head AND inside the friends list. Is this a valid "output collection" pattern where a data structure holds output channels?

## Recommended Next Steps

1. **Test the current file** to get exact SRSW error messages:
   ```bash
   cd /Users/udi/Grassroots/GLP/glp_runtime
   echo -e '../programs/multiagent/play_alice_bob.glp\n:quit' | dart run bin/glp_repl.dart
   ```

2. **For each SRSW error**, determine:
   - Is this a genuine violation that requires clause rewriting?
   - Is this a valid GLP pattern that the SRSW checker should accept?
   - If the latter, what relaxation rule should be added?

3. **Consult SRSW specification** at `/Users/udi/Grassroots/GLP/docs/type system/` to understand current rules.

4. **Compare with social_agent.glp** which passes SRSW checking to identify working patterns.

## Key Insight from Session

The Art-of-GLP-2025 book uses idioms that may predate the strict SRSW enforcement in the current type checker. The book's `play_alice_bob` pattern establishes friendships dynamically during execution via the cold-call protocol, which involves:
- Channel creation via `new_channel` guard predicate
- Response variables bound asynchronously by remote agents
- Stream threading through actor continuations

These patterns may require either:
- Clause rewrites to satisfy current SRSW rules, or
- SRSW checker adjustments to recognize valid concurrent programming patterns

## Related Transcripts

- `/mnt/transcripts/2026-01-18-16-20-57-wildcard-type-semantics-fix.txt` — Definition 4.5 fix session
- `/mnt/transcripts/2026-01-18-16-03-57-wildcard-type-mode-propagation-fix.txt` — Earlier wildcard work
