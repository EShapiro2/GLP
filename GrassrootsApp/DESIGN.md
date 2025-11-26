# GrassrootsApp Design Document

## Overview

Implementation of the Grassroots Social Graph platform from the GLP 2025 paper,
with a testing framework based on theater-style scripts.

## Terminology

| Term       | Description                                                    | Implementation       |
|------------|----------------------------------------------------------------|----------------------|
| **Agent**  | Personal agent from the paper with user and network channels   | GLP code (social_graph, etc.) |
| **Actor**  | Simulated user that follows a script                           | GLP code connected to Agent's user channel |
| **Script** | Test scenario describing who does what when                    | Semi-structured English text |

## Architecture

```
Script (English text)
    |
    v (parsed/interpreted)
Actor (GLP) <--user channel--> Agent (GLP) <--network channel--> Network
```

Each simulated person has an Actor-Agent pair:
- The Actor plays the human role (follows the script)
- The Agent is their personal agent (runs social_graph code)
- The Network routes messages between Agents

## Scripts

Scripts use causal linking ("after seeing X") and optionally timing ("wait 2sec").

### Script 1: Cold Call Connection

```
Title: Alice and Bob become friends via cold call
Actors: Alice, Bob
Precondition: Public keys are names (alice, bob)

---

Scene 1: Alice initiates connection

Alice says to her agent: connect(bob)

  [Alice's agent sends intro(alice, alice, Resp) to Bob via network]

---

Scene 2: Bob receives and considers the offer

Bob's agent, after receiving intro from network,
  says to Bob: befriend(alice, Resp)

Bob, after seeing befriend(alice, _),
  says to his agent: decision(yes, alice, Resp)

  [Bob's agent creates channel, binds Resp to accept(ch(...))]

---

Scene 3: Alice receives acceptance

Alice's agent, after receiving response(accept(Ch)),
  establishes friend channel to Bob

---

Scene 4: Alice sends a message to Bob

Alice says to her agent: send(bob, hello)

  [Alice's agent sends msg(alice, bob, hello) via friend channel]

Bob's agent, after receiving msg(alice, bob, hello),
  says to Bob: msg(alice, hello)

Bob, after seeing msg(alice, hello): done

---

Postcondition:
  - Alice's friends list contains (bob, ...)
  - Bob's friends list contains (alice, ...)
  - Bob received hello from Alice
```

## Workplan

### Phase 1: Core Infrastructure
- [ ] Channel operations (send, receive, new_channel)
- [ ] Stream utilities (merge, tag_stream, inject)
- [ ] Lookup utilities for friends list

### Phase 2: Agent Implementation
- [ ] Agent initialization (agent/3 from paper)
- [ ] Social graph main loop (social_graph/3)
- [ ] Cold-call protocol (connect, intro, decision, response handling)
- [ ] bind_response and handle_response helpers

### Phase 3: Actor Implementation
- [ ] Actor that reads script actions from a stream
- [ ] Causal triggers ("after seeing X")
- [ ] Simple timing support

### Phase 4: Test Harness
- [ ] Network simulation for single-workstation testing
- [ ] Script parser (or manual encoding as GLP goals)
- [ ] Run Script 1 end-to-end

### Phase 5: Extensions
- [ ] Friend-mediated introductions (3-actor scenario)
- [ ] Direct messaging
- [ ] Group formation

## Files

```
GrassrootsApp/
  DESIGN.md           # This file
  glp/
    channel.glp       # Channel operations
    agent.glp         # Agent code from paper
    actor.glp         # Actor (simulated user) code
    network.glp       # Network simulation
  scripts/
    cold_call.txt     # Script 1
  test/
    test_cold_call.glp  # Test harness for Script 1
```

## References

- Paper: `/home/user/GLP/docs/main GLP 2025.tex`
- Programs 5-8: Social graph initialization and protocols
- Program 9: Channel operations
- Program 11: Stream tagging
