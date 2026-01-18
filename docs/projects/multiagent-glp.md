# Multiagent GLP

## Mission

Get multiagent GLP up and running — all aspects: Dart code, GLP programs, specs.

## Current Goal

**Friend-mediated introduction**: Bob introduces Alice to Charlie, creating a direct channel between them.

## Scope

Anything needed to make multiagent work:
- `glp_runtime/lib/multiagent/` - IrmaAgent, IrmaContext, V_p, M_p, PayloadSerializer
- `glp_multiagent/` - Flutter simulation app
- `programs/multiagent/` - GLP programs for agents
- `docs/ma/` - multiagent specifications and handovers

## Current State (2026-01-17)

### Completed
- IrmaAgent wrapper with V_p/M_p management
- IrmaRouter for opaque byte payload routing
- Flutter app integration (coordinator + agent windows)
- Basic messaging: send, ping/pong
- Friend-list enforcement (agents can only message known friends)

### In Progress
- Friend-mediated introduction protocol
- Type declarations for `social_agent.glp`

### Test Status
- 139 multiagent unit tests passing
- Flutter app builds and runs
- Three-agent topology (Alice↔Bob↔Charlie) working

## Key Files

### Dart
- `glp_runtime/lib/multiagent/irma_agent.dart` - Agent wrapper
- `glp_runtime/lib/multiagent/irma_context.dart` - V_p/M_p context
- `glp_runtime/lib/multiagent/payload_serializer.dart` - Message serialization
- `glp_multiagent/lib/main.dart` - Flutter app
- `glp_multiagent/lib/irma_router.dart` - Message routing

### GLP Programs
- `programs/multiagent/social_agent.glp` - Current agent program
- `programs/multiagent/old/` - Earlier versions for reference
- `programs/book/social_graph/friend_introduction.glp` - Reference implementation

### Specs
- `docs/ma/irmaGLP-spec.md` - irmaGLP specification
- `docs/glp-io-spec-v2.md` - I/O specification

## Next Steps

1. **Type declarations** for `social_agent.glp` (may collaborate with Typed GLP)
2. **Procedure declarations** with modes
3. **Implement** `introduce(P, Q)` command
4. **Implement** `accept_intro(Other, Ch)` handling
5. **Test** in Flutter app

## Protocol: Friend-Mediated Introduction

```
1. Bob types: introduce(alice, charlie)
2. Bob creates fresh channel pair
3. Bob sends to Alice: msg(bob, alice, intro(charlie, ch(CA?, AC)))
4. Bob sends to Charlie: msg(bob, charlie, intro(alice, ch(AC?, CA)))
5. Alice receives: intro_offer(bob, charlie, Ch)
6. Charlie receives: intro_offer(bob, alice, Ch)
7. Alice types: accept_intro(charlie, Ch)
8. Charlie types: accept_intro(alice, Ch)
9. Result: Alice and Charlie can communicate directly
```

## Handover Reports

- `docs/ma/phase6-handover-2026-01-17.md` - Latest handover
- `docs/ma/HANDOVER-2026-01-17-irmaGLP-phase1-4.md` - Earlier phases

## Testing

**READ FIRST:** `docs/DISCIPLINE.md` Part II for complete testing protocol.

```bash
# Multiagent unit tests
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/ > /tmp/ma-tests.txt 2>&1

# Flutter build
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter build macos
```
