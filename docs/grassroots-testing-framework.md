# Grassroots Platform Testing Framework

## Overview

This document describes a testing framework for multiagent GLP systems based on theater-style scripts ("plays"). The framework enables deterministic testing of distributed protocols by simulating user behavior through Actor processes that follow scripted interactions.

## Terminology

| Term | Description | Implementation |
|------|-------------|----------------|
| **Agent** | Personal agent from the GLP paper with user and network channels | GLP code (`social_graph`, etc.) |
| **Actor** | Simulated user that follows a script | Manual GLP code per play |
| **Play** | Test scenario with script and actors | Directory with README + .glp files |
| **Script** | Human-readable description of the test scenario | Semi-structured English in README.txt |

## Architecture

```
Play (English script in README.txt)
        |
        v (manually converted to)
    Actor (GLP)
        |
   user channel
        |
        v
    Agent (GLP) <-- network channel --> Network (GLP)
```

Each person in a test has an Actor-Agent pair:
- The **Actor** plays the human role, following a deterministic script
- The **Agent** is the personal agent code from the paper
- The **Network** routes messages between agents

## Directory Structure

```
GrassrootsApp/
  glp/
    channel.glp       # Channel operations (send, receive, new_channel)
    streams.glp       # Stream utilities (merge, tag_stream, observe)
    agent.glp         # Personal agent (cold-call protocol)
    network.glp       # 2-agent network switch
  plays/
    play01_cold_call/
      README.txt      # English script
      alice.glp       # Alice's actor
      bob.glp         # Bob's actor
      main.glp        # Test harness
    play02_introduction/
      ...
```

## Script Format

Scripts are written as semi-structured English describing causal sequences:

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

## Actor Implementation

Actors are implemented as strict, deterministic GLP processes. Each step is one clause; unexpected messages cause failure (no `otherwise` guards).

Example - Alice's actor for cold-call:

```glp
% Step 1: Initiate connection to bob
alice_actor(ch(In, [msg(user, alice, connect(bob))|Out?])) :-
    alice_wait_response(In?, Out).

% Step 2: After response, send hello
alice_wait_response([msg(agent, user, response(_))|In],
                    [msg(user, alice, send(bob, hello))|Out?]) :-
    alice_done(In?, Out).

% Step 3: Done
alice_done(_, []).
```

Key principles:
- Each script step = one clause
- Unexpected messages → no clause matches → goal fails
- Channel bindings show the execution trace
- Clean termination: output streams closed with `[]`

## Test Harness

The `main.glp` file wires together channels, network, agents, and actors:

```glp
test(AliceTrace?, BobTrace?) :-
    % Create network channels
    new_channel(NetChA, ANetCh),
    new_channel(NetChB, BNetCh),

    % Create user channels
    new_channel(UserChA, AUserCh),
    new_channel(UserChB, BUserCh),

    % Observe channels for traces
    AUserCh = ch(AIn, AOut),
    observe(AIn?, AInCopy, AliceTrace),
    BUserCh = ch(BIn, BOut),
    observe(BIn?, BInCopy, BobTrace),

    % Start network switch
    network((alice, NetChA?), (bob, NetChB?)),

    % Start agents
    agent(alice, ch(AInCopy?, AOut?), ANetCh?),
    agent(bob, ch(BInCopy?, BOut?), BNetCh?),

    % Start actors
    alice_actor(UserChA?),
    bob_actor(UserChB?).
```

## Stream Observation

The `observe/3` predicate creates two copies of a stream for observation:

```glp
% SRSW-compliant observe (fixed from paper)
observe(X?, Y, Z?) :- observe(Y?, X, Z).
observe(X, X?, X?) :- ground(X) | true.
observe(Xs, [Y1?|Ys1?], [Y2?|Ys2?]) :-
    Xs? = [X|Xs1] |
    observe(X?, Y1, Y2),
    observe(Xs1?, Ys1, Ys2).
```

**Note**: The second clause uses `ground(X)` guard to relax SRSW for multiple reader occurrences of `X?`. The compiler must recognize this relaxation.

## Running Tests

```
test(AliceTrace, BobTrace).
```

Verification:
- Check that traces contain expected messages
- Verify final channel bindings
- Confirm clean termination (no suspended goals)

## Future Work

- [ ] Play 02: Friend-mediated introduction (3 actors)
- [ ] Play 03: Group formation
- [ ] Play 04: Message rejection scenarios
- [ ] Automated script-to-actor compilation
