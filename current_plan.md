# Current State: Social Agent Play - dGLP and madGLP

Updated: 2026-02-03

## Summary

The `agent/4` social agent code works in dGLP mode. The madGLP version now has the `'_send'/3` kernel implemented and messages are flowing between isolates. Basic boot test passes.

## Completed

### dGLP Version (`play_dglp.glp`)
- **Status: WORKING**
- Uses `agent/4` with separate `UserIn` and `NetIn` streams
- Type-checks successfully
- Runs complete 7-step scenario:
  1. Alice cold-calls Bob (Bob accepts)
  2. Alice sends "Hi Bob, this is Alice"
  3. Bob cold-calls Charlie (Charlie accepts)
  4. Charlie sends "Hi Bob, this is Charlie"
  5. Bob introduces Alice to Charlie (both accept)
  6. Alice sends "Hi Charlie, this is Alice"
  7. Charlie responds "Hi Alice, this is Charlie"
- Suspends at end (expected - actors waiting for more input)

### madGLP Version (`play_alice_bob_charlie_actor_boot.glp`)
- **Status: PARTIAL**
- `'_send'/3` kernel implemented (replaces dated `'_cold_send'/2`)
- Procedure declaration added to `prelude.dart`
- `send_to_net/1` and `global_send/3` defined in embedded predicates
- Basic boot test passes (3 agents boot successfully)
- Cold-call messages are sent and received between isolates
- **Remaining issue**: Protocol suspends after initial message exchange (response variable not binding)

### Changes Made (2026-02-03)

1. **Implemented `'_send'/3` kernel** (`body_kernels.dart:656-750`)
   - Per spec Section 11.5: handles both cold-calls (index 0) and established links (index > 0)
   - For index 0: wraps T in list `[T | _w(q,0)]` via `coldSend()`
   - For index > 0: sends T directly via `send()`

2. **Added `send()` method to MadContext** (`mad_context.dart`)
   - Handles direct sends for established links

3. **Updated GLP predicates** (`mad_predicates.glp`, `isolate_manager.dart`)
   - Removed `global_send/2` (dated)
   - Updated `send_to_net/1` to use `global_send/3` with `'_w'(Q, 0)`
   - Added `ground(Q?)` guard for SRSW compliance

4. **Added to prelude.dart**
   - Procedure declaration: `procedure _send(_?, _?, _?).`
   - Builtin list entry: `'_send/3'`

### Key Fix Applied to Both Versions
Added clause to handle `response` messages on UserIn:
```prolog
%% Response to cold-call (injected into UserIn by inject_msg)
agent(Id, [msg(From, Id1, response(Resp))|UserIn], NetIn, Outs) :-
    Id? =?= Id1? |
    handle_response(Resp?, From?, Outs?, Outs1, NetIn?, NetIn1),
    agent(Id?, UserIn?, NetIn1?, Outs1?).
```
This was needed because `inject_msg` puts cold-call responses on UserIn, not NetIn.

## Architecture

```
                    network3 (dGLP) / send_to_net (madGLP)
                   /    |    \
              NetCh  NetCh  NetCh
                |      |      |
             Alice   Bob   Charlie  (agent/4)
                |      |      |
             UserCh UserCh UserCh
                |      |      |
             Actor  Actor  Actor
```

Each agent has:
- `UserIn` stream - messages from user/actor
- `NetIn` stream - messages from network (including friend channels merged in)
- `OutputsList` - named outputs (`'_user'`, `'_net'`, plus friend names)

## Files

| File | Status | Description |
|------|--------|-------------|
| `programs/typed_book/social_graph/play_dglp.glp` | WORKING | dGLP with agent/4, network3 switch |
| `programs/typed_book/social_graph/play_alice_bob_charlie_actor_boot.glp` | PARTIAL | madGLP with agent/4, isolate boot |
| `programs/system/mad_predicates.glp` | UPDATED | send_to_net/1 and global_send/3 |
| `glp_runtime/lib/runtime/body_kernels.dart` | UPDATED | sendKernel for '_send'/3 |
| `glp_runtime/lib/multiagent/mad_context.dart` | UPDATED | added send() method |
| `glp_runtime/lib/analysis/type_checker/prelude.dart` | UPDATED | _send/3 declaration |

## How to Run

See **[docs/ma/HOW-TO-RUN.md](docs/ma/HOW-TO-RUN.md)** for detailed instructions.

Quick commands:
```bash
# dGLP (WORKING)
cd glp_runtime && echo -e 'load ../programs/typed_book/social_graph/play_dglp.glp\nplay.' | dart run bin/glp_repl.dart

# madGLP (PARTIAL - boot works, full protocol suspends)
cd glp_runtime && dart test test/multiagent/isolate_manager_test.dart
```

## Next Steps

### 1. Debug madGLP Response Variable
- Investigate why protocol suspends after cold-call
- Check localize/globalize for response channel variable
- Verify global_send goals are firing when responses are bound

### 2. Factor Out Common Agent Code
Once both versions work:
- Extract `agent/4` and helpers to a shared file
- Each version only provides boot code (`play` vs `boot`) and actors

## Notes

- The `merge` is still used when adding friend channels to NetIn (this is correct)
- The key insight: `inject_msg` puts responses on UserIn, so agent needs a clause to handle them there
- dGLP uses `network3` for routing; madGLP uses `send_to_net` kernel
- `'_send'/3` is spec-compliant per Section 11.5 - one builtin for both cold-calls and established links
