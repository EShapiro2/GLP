# irmaGLP Implementation Handover

**Date**: January 17, 2026  
**From**: Session implementing social_agent with multiple friends  
**To**: Next session implementing M4 (V_p/M_p)

## Current State

### Working Demo
- `glp_multiagent` Flutter app with Alice↔Bob↔Charlie linear topology
- Each agent in separate window, routes messages via Dart coordinator
- GLP enforces friends-only messaging with `lookup_send` status return
- External GLP file loading (no rebuild needed)

### Key Files
- **GLP Program**: `/programs/multiagent/social_agent.glp`
- **Flutter App**: `/glp_multiagent/lib/main.dart`
- **Implementation Plan**: `/docs/irmaGLP-implementation-plan.md`

### What Works
- `send(bob, ping)` → message routed → pong reply
- `send(john, hi)` → `error(unknown_recipient, john)` (friends enforcement)
- Multiple friends per agent (Bob knows Alice and Charlie)

### What's Missing for Friend Introduction
- V_p (Variable Table): No tracking of non-local variables
- M_p (Message Queue): No outbound message buffering
- Dart routes by `msg(From, To, Content)` To field, not by variable ownership
- Shared channels can't work across isolates without V_p/M_p

## Next Task: M4 Implementation

### Phase 1: V_p (Variable Table)
Create `/glp_runtime/lib/multiagent/variable_table.dart`:
- Track variables with non-local counterparts
- Three roles: writer, createdReader, importedReader
- Methods: add, remove, lookup, getByCreator

### Phase 2: M_p (Message Queue)  
Create `/glp_runtime/lib/multiagent/message_queue.dart`:
- Queue outbound messages: assignment, readRequest, abandon
- FIFO per destination
- Flush after quiescence

### Phase 3: Serialization
Create `/glp_runtime/lib/multiagent/payload_serializer.dart`:
- Terms → bytes with global var IDs (`creator:localId`)
- Round-trip for constants, variables, structs, lists

### Phase 4: Integration
- Hook V_p/M_p into `/glp_runtime/lib/runtime/runtime.dart`
- Update scheduler to process M_p
- Update multiagent app to use serialized payloads

### Phase 5: Test
Friend-mediated introduction:
1. Bob receives `introduce(alice, charlie)`
2. Bob creates shared channel, sends to both
3. Alice and Charlie accept, add each other to friends
4. Alice writes to channel, Charlie receives

## Key Documents
- `/docs/irmaGLP-implementation-plan.md` - Full implementation plan
- `/docs/irmaGLP-multiwindow-multiagent-simulation-design.md` - Architecture
- `/mnt/project/glp-io-spec-v2.md` - I/O specification
- GLP paper Appendix (irmaGLP section) - Formal definitions

## Type Checker Bugs (Unrelated but Documented)
- `/docs/type system/bug-report-2026-01-17.md` - 7 bugs blocking typed social_agent.glp

## Commands to Run Demo
```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent && flutter run -d macos
```
Click "Alice↔Bob↔Charlie" button, then type `send(bob, ping)` in Alice's window.
