# Handover: play_introduction.glp

**Date**: 2026-01-18  
**Project**: Multiagent GLP  
**Status**: Ready for single-agent GLP testing (type checker bypassed)

---

## Summary

Created `play_introduction.glp`, a pure GLP simulation of the friend-mediated introduction protocol with three agents (Alice, Bob, Charlie). This program should be tested on single-agent GLPSAM before deploying to the multiagent Flutter app.

---

## File Location

`/Users/udi/Grassroots/GLP/programs/multiagent/play_introduction.glp`

---

## Program Overview

The program simulates three agents with a 3-way network switch handling message routing:

**Topology:**
- Alice ↔ Bob (friends)
- Bob ↔ Charlie (friends)  
- Alice and Charlie do NOT initially know each other

**Entry Point:**
```glp
play(AliceOut, BobOut, CharlieOut, AliceIn, BobIn, CharlieIn)
```

**Arguments:**
- `AliceOut`, `BobOut`, `CharlieOut`: Output streams showing what each user sees
- `AliceIn`, `BobIn`, `CharlieIn`: Input streams for user commands to each agent

---

## Protocol Flow (Friend-Mediated Introduction)

1. Bob types: `introduce(alice, charlie)`
2. Bob sends intro messages to both Alice and Charlie via existing friend channels
3. Alice receives: `intro_offer(bob, charlie, Ch)` where `Ch = ch(CharlieToAlice?, AliceToCharlie)`
4. Charlie receives: `intro_offer(bob, alice, Ch)` where `Ch = ch(AliceToCharlie?, CharlieToAlice)`
5. Bob sees: `introduced(alice, charlie)`
6. Alice types: `accept_intro(charlie, Ch)` (using the channel from step 3)
7. Charlie types: `accept_intro(alice, Ch)` (using the channel from step 4)
8. Alice and Charlie can now communicate directly via the fresh channel

---

## Key Components

### Agent Initialization
Each agent runs `social_graph/3` processing a merged stream of user input and network messages, with a friends list for routing.

### 3-Way Network Switch
Based on GLP-ICLP-2026 Appendix. Routes messages between agents based on destination:
- 6 routing clauses (Alice↔Bob, Alice↔Charlie, Bob↔Charlie, and reverses)
- Termination clause when all channels empty

### Introduction Clauses
From `social_agent.glp`:
- `introduce(P, Q)`: Creates fresh channel pair, sends halves to P and Q
- `intro(Other, Ch)`: Received by introduced party, triggers `intro_offer` to user
- `accept_intro(Other, Ch)`: User accepts, merges new friend's stream, adds to friends list

---

## Type Checker Status

**BYPASSED** — 27 type errors reported, belonging to Typed GLP project to resolve:

1. Mode mismatch in channel structures `ch(In?, Out)`
2. Missing `=/2` procedure declaration in prelude
3. Type compatibility issues between `MsgStream` and `_?` wildcards

See `/docs/ma/typed-glp-request-2026-01-18.md` for the handoff to Typed GLP.

---

## Testing Plan

### Phase 1: Single-Agent GLPSAM Test

Run the program in GLPSAM to verify:
1. Agents initialize correctly with merged streams
2. Network switch routes messages between agents
3. Introduction protocol creates fresh channel pairs
4. Channel variables propagate correctly through the network
5. `accept_intro` properly merges new friend streams

**Test Command:**
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/glpsam.dart ../programs/multiagent/play_introduction.glp
```

**Initial Goal:**
```glp
play(AliceOut, BobOut, CharlieOut, AliceIn, BobIn, CharlieIn)
```

**Test Sequence:**
```glp
%% Step 1: Bob introduces Alice to Charlie
BobIn = [introduce(alice, charlie) | BobIn1]

%% Step 2: Observe outputs (should see intro_offers)
%% AliceOut, CharlieOut, BobOut

%% Step 3: Alice accepts
AliceIn = [accept_intro(charlie, Ch_alice) | AliceIn1]

%% Step 4: Charlie accepts  
CharlieIn = [accept_intro(alice, Ch_charlie) | CharlieIn1]

%% Step 5: Alice sends to Charlie directly
AliceIn1 = [send(charlie, hello) | AliceIn2]

%% Step 6: Charlie should receive
%% CharlieOut should show: received(alice, hello)
```

### Phase 2: Multiagent Flutter Test

After single-agent verification, test in the Flutter app with the existing coordinator infrastructure.

---

## Dependencies

- `social_agent.glp` clauses (included in this file)
- GLPSAM runtime with external I/O support
- Stdlib `unify.glp` for `=/2` (may need to be loaded)

---

## Known Issues / Open Questions

1. **Network switch termination**: Does the termination clause `network((_, ch([], _)), ...)` fire correctly when streams are open but empty?

2. **Channel variable sharing**: When Bob creates `ch(QtoP?, PtoQ)` and sends halves to Alice and Charlie, the fresh variables must unify across agents. In single-agent simulation this works via standard unification; in multiagent this requires V_p/M_p.

3. **Stream merging in accept_intro**: The `merge(In?, Tagged?, In1)` call creates a new merged stream. Need to verify this doesn't interfere with the main `social_graph` loop.

---

## Related Files

- `/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp` — Original agent program (with types)
- `/Users/udi/Grassroots/GLP/docs/ma/typed-glp-request-2026-01-18.md` — Type checker issue handoff
- `/Users/udi/Grassroots/GLP/docs/ma/HANDOVER-2026-01-17-irmaGLP-phase1-4.md` — Phase 1-4 completion handover
- `/Users/udi/Grassroots/GLP-ICLP-2026/glp_section_social_graph.tex` — Paper reference for protocol
- `/Users/udi/Grassroots/GLP-ICLP-2026/glp_appendix_additional_techniques.tex` — 3-way network switch reference

---

## Next Steps

1. Run `play_introduction.glp` in GLPSAM with test sequence
2. Debug any issues with channel propagation or stream merging
3. Once working in single-agent, proceed to multiagent Flutter test
4. Document any GLP program fixes needed
