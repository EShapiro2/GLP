# GrassrootsApp Design Document

## Overview

Implementation of the Grassroots Social Graph platform from the GLP 2025 paper,
with a testing framework based on theater-style scripts ("plays").

## Terminology

| Term       | Description                                                    | Implementation       |
|------------|----------------------------------------------------------------|----------------------|
| **Agent**  | Personal agent from the paper with user and network channels   | GLP code (social_graph, etc.) |
| **Actor**  | Simulated user that follows a script                           | Manual GLP code per play |
| **Play**   | Test scenario with script and actors                           | Directory with README + .glp files |

## Architecture

```
Play (English script in README.txt)
    |
    v (manually converted to GLP)
Actor (GLP) <--user channel--> Agent (GLP) <--network channel--> Network
```

Each simulated person has an Actor-Agent pair:
- The Actor plays the human role (follows the script exactly)
- The Agent is their personal agent (runs social_graph code)
- The Network routes messages between Agents

Actors are strict: unexpected messages cause failure (no `otherwise` fallbacks).

## Directory Structure

```
GrassrootsApp/
  DESIGN.md           # This file
  glp/
    channel.glp       # Channel operations (send, receive, new_channel)
    streams.glp       # Stream utilities (merge, tag_stream, inject, lookup)
    agent.glp         # Personal agent code from paper
    network.glp       # 2-agent network switch
  plays/
    play01_cold_call/
      README.txt      # Script in English
      alice.glp       # Alice's actor
      bob.glp         # Bob's actor
      main.glp        # Test harness
    play02_.../       # Future plays
```

## Play 01: Cold Call

Two people (Alice and Bob) establish friendship via cold call.

See `plays/play01_cold_call/README.txt` for the full script.

**Summary:**
1. Alice says: connect(bob)
2. Bob sees befriend request, says: decision(yes, alice, Resp)
3. Alice sees response, says: send(bob, hello)
4. Bob sees hello message → done

## Testing

Run with the GLP runtime:
```
cd GrassrootsApp/plays/play01_cold_call
# Load all required files and run test goal
```

Verification: trace reductions and examine final channel bindings.

## References

- Paper: `/home/user/GLP/docs/main GLP 2025.tex`
- Programs 5-8: Social graph initialization and protocols
- Program 9: Channel operations
- Program 12: Network switch
