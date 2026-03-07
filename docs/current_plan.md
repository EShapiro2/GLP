# Plan: Map-Based Friend Lookup for GLP Social Graph Agent

Started: 2026-03-07
Branch: `claude/agent-friend-request-maps-HEmnr`

## Discussion Summary

### Goal
Replace O(n) list-based friend lookup in the social graph agent with O(1) map-based lookup using the existing Dart Map support.

### Key Design Decisions

1. **Dual data structure**: Friends go in a Map (O(1) lookup), system channels (`_user`, `_net`) stay in the existing OutputsList. Friends are NOT stored in both -- only in the map.

2. **Map keys are plain Constants**: Dart map keys only support constants (String, num), not structured terms like `friend(alice)`. So the map indexes friends by NAME only (e.g., `'alice'`), not by the full OutputKey.

3. **New `map_send/4` kernel needed**: There is no `=` (assignment/unification) body kernel in GLP. Writing to a stream writer retrieved from a map cannot be done in pure GLP. A new Dart kernel `map_send(Map?, Key?, Msg?, Map1)` combines: get stream from map, write `[Msg|NewTail]`, update map with NewTail.

4. **Stale-writer problem resolved**: If we stored the same stream writer in both the list and the map, the list would become stale after map-based sends. Resolution: friends go ONLY in the map; the OutputsList retains only system channels.

5. **`send_all_friends` deferred**: Broadcasting to all friends (used in GSN update_befriend/update_unfriend) requires map_keys-based iteration. This is future work -- temporarily broken for plays 12-14.

6. **Don't touch the existing list**: Keep the list infrastructure intact. Add map alongside. Replace the list later if map approach works.

### Required Merges

- **`origin/map_impl`**: Brings `map_remove/3`, `map_keys/2` kernels + prelude declarations
- **`origin/ohad3`**: Brings GSN agent with `send_all_friends/4`, `update_befriend`/`update_unfriend` messages, plays 12-14

---

## Implementation Steps

### Phase 0: Merges and Baseline
- [ ] 0.1: Run all REPL tests, commit baseline
- [ ] 0.2: Merge `origin/map_impl` (map_remove, map_keys), run tests
- [ ] 0.3: Merge `origin/ohad3` (GSN agent, plays 12-14), run tests

### Phase 1: New Dart Kernels
- [ ] 1.1: Implement `map_send/4` in `body_kernels.dart`
- [ ] 1.2: Implement `map_close_all/1` in `body_kernels.dart`
- [ ] 1.3: Register both in `prelude.dart` + protected predicates
- [ ] 1.4: Test `map_send` with standalone GLP program

### Phase 2: Modified GLP Agent
- [ ] 2.1: Change agent signature to `agent/5` (add map arg)
- [ ] 2.2: Add `map_lookup_send/4` procedure
- [ ] 2.3: Replace friend operations in agent clauses (see table below)
- [ ] 2.4: Thread map through `bind_response` and `handle_response`
- [ ] 2.5: Update boot files with `map_new` initialization
- [ ] 2.6: Comment out `send_all_friends` calls (DEFERRED)
- [ ] 2.7: Add `map_close_all` to agent termination

### Phase 3: Testing
- [ ] 3.1: Unit test for `map_send/4` kernel
- [ ] 3.2: Run plays 1-11 (should work)
- [ ] 3.3: Note plays 12-14 as deferred
- [ ] 3.4: Run full REPL test suite -- no regressions

### Phase 4: Future Work (Deferred)
- [ ] `send_all_friends` via `map_keys` iteration
- [ ] Remove OutputsList for friends entirely
- [ ] General `bind/2` body kernel
- [ ] `map_close_all/1` kernel for termination

---

## Operation Mapping

| Operation | Before (list) | After (map) |
|-----------|--------------|-------------|
| Send to friend | `lookup_send(friend(X?), Msg, Outs, Outs1)` | `map_lookup_send(X?, Msg, FMap, FMap1)` |
| Add friend | `add_output(friend(X?), FOut, Outs, Outs1)` | `map_put(FMap?, X?, FOut, FMap1)` |
| Remove friend | `remove_output(friend(X?), Outs, Outs1)` | `map_remove(FMap?, X?, FMap1)` |
| Send to child | `lookup_send(child(X?), ...)` | `map_lookup_send(X?, ...)` (same map) |
| Send to `_user`/`_net` | `lookup_send('_user', ...)` | **unchanged** (list-based) |
| Broadcast friends | `send_all_friends(...)` | **DEFERRED** |
| Close all | `close_outputs(Outs?)` | `close_outputs(Outs?)` + `map_close_all(FMap?)` |

---

## New Dart Kernels

### `map_send(Map?, Key?, Msg?, Map1)` -- body_kernels.dart
1. Get current stream writer from map at Key
2. Create cons cell `[Msg | NewTail]` (NewTail = fresh unbound variable)
3. Bind old stream writer to cons cell (activates suspended readers)
4. Update map in-place: `entries[key] = NewTail`
5. Bind Map1 to mutated map

### `map_close_all(Map?)` -- body_kernels.dart
1. Iterate all map entries
2. Bind each value (stream writer) to `[]` (terminates stream)

---

## New GLP Procedures

### `map_lookup_send/4`
```prolog
procedure map_lookup_send(Constant?, _?, _?, _).
map_lookup_send(Key, Msg, Map, Map1?) :-
    map_contains(Map?, Key?) |
    map_send(Map?, Key?, Msg?, Map1).
```

---

## Critical Files

| File | Change |
|------|--------|
| `glp_runtime/lib/runtime/body_kernels.dart` | Add `map_send/4`, `map_close_all/1` |
| `glp_runtime/lib/analysis/type_checker/prelude.dart` | Register new kernel declarations |
| `programs/typed_book/gsn/typed_social_agent.glp` | Agent/5, map threading, new procedures |
| `programs/typed_book/gsn/play_ui_sim_boot.glp` | Add `map_new` to agent init |
| `programs/typed_book/gsn/play_dglp_boot.glp` | Add `map_new` to agent init |
