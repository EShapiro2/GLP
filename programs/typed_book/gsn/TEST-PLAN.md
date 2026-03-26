# GSN Agent FLv1/FoFMap — Test Plan

## Overview

Incremental test plan for adding FLv1 struct (epoch + state) to FMap and FoFMap to the GSN agent. Each step adds one GLP function/clause and tests it before moving on.

All tests run via REPL plays in `programs/typed_book/gsn/`.

---

## Step 1: agent/6 init

**GLP change**: Add FoFMap as 6th argument to `agent`. Boot with `map_new(FMap), map_new(FoFMap)`. Update all recursive `agent` calls to thread FoFMap through.

**Files changed**: `typed_social_agent.glp`, `play_ui_sim_boot.glp` (boot file)

**Test**:
- Boot alice with empty FMap and FoFMap
- Agent suspends waiting for input (no crash, no abort)
- Expected result: `→ suspended`

**Verification**:
```
load gsn files
boot alice
→ suspended (agent alive, waiting for messages)
```

---

## Step 2: agent/6 — connect(Target) clause

**GLP change**: Modify the `msg('_user', Id, connect(Target))` clause. Before sending intro on network:
- `map_contains(FMap?, Target?)` — if active, ignore (done)
- Compute epoch: if FMap has entry, `epoch = old_epoch + 1`; else `epoch = 1`
- Pass epoch in the intro message

**Files changed**: `typed_social_agent.glp`

**Test 2a — fresh connect**:
- Alice sends `connect(bob)` with empty FMap
- Verify `msg(bob, intro(alice, Resp))` appears on network output
- Verify agent continues (not aborted)
- Expected: `→ suspended`

**Test 2b — connect to existing active friend**:
- Pre-populate alice's FMap with `flv1(bob, 1, active)`
- Alice sends `connect(bob)`
- Verify: no intro sent, agent ignores and continues
- Expected: `→ suspended`, no network output for bob

**Test 2c — reconnect after inactive**:
- Pre-populate alice's FMap with `flv1(bob, 1, inactive)`
- Alice sends `connect(bob)`
- Verify: intro sent with epoch 2
- Expected: `→ suspended`, network output contains intro

---

## Step 3: bind_response / handle_response

**GLP change**: Modify `handle_response` to:
- `map_put(FMap, From, flv1(From, Epoch, active), FMap1)`
- Thread FMap through bind_response → handle_response → agent

**Files changed**: `typed_social_agent.glp`

**Test 3a — successful accept**:
- 2-agent play: alice connects bob, bob auto-accepts
- Verify alice's FMap has `flv1(bob, 1, active)`
- Verify channel established (alice can send text to bob)
- Expected: `→ suspended`, `map_show` outputs `{bob: flv1(bob, 1, active)}`

**Test 3b — reject**:
- Alice connects bob, bob rejects
- Verify FMap unchanged (no entry for bob, or no active entry)
- Expected: `→ suspended`, FMap empty or unchanged

---

## Step 4: agent/6 — incoming cold-call clause

**GLP change**: Modify `msg(Id1, intro(From, Resp?))` clause (network cold-call). Add:
- Check FMap: if `From` exists and active → ignore (already friends)
- Check epoch: if `FMap[From].epoch >= incoming_epoch` → ignore (stale)
- On accept: `map_put(FMap, From, flv1(From, epoch, active), FMap1)`

**Files changed**: `typed_social_agent.glp`

**Test 4a — bob receives cold-call, accepts**:
- 2-agent play: alice connects bob
- Verify bob's FMap has `flv1(alice, 1, active)`
- Expected: `map_show` on bob shows `{alice: flv1(alice, 1, active)}`

**Test 4b — duplicate cold-call ignored**:
- Bob already has `flv1(alice, 1, active)` in FMap
- Another `intro(alice, Resp)` arrives
- Verify: ignored, FMap unchanged, no duplicate channel
- Expected: `→ suspended`, FMap still shows single entry

**Test 4c — stale cold-call ignored**:
- Bob has `flv1(alice, 3, inactive)` in FMap
- `intro(alice, Resp)` arrives with epoch 2
- Verify: ignored (epoch 2 < stored epoch 3)

---

## Step 5: agent/6 — unfriend(Target) clause (user-initiated)

**GLP change**: Modify `msg('_user', Id, unfriend(Target))` clause. Add:
- If FMap[Target] doesn't exist or inactive → ignore
- Send unfriend message with current epoch
- `map_put(FMap, Target, flv1(Target, epoch, inactive), FMap1)`
- `remove_output(friend(Target), ...)`
- Skip dissemination (don't send update_unfriend to other friends)

**Files changed**: `typed_social_agent.glp`

**Test 5a — unfriend active friend**:
- Alice has bob as active friend in FMap
- Alice sends `unfriend(bob)`
- Verify FMap has `flv1(bob, 1, inactive)`
- Verify output for bob removed
- Expected: `map_show` shows `{bob: flv1(bob, 1, inactive)}`

**Test 5b — unfriend non-friend (no-op)**:
- Alice has empty FMap
- Alice sends `unfriend(bob)`
- Verify: no crash, agent continues, FMap unchanged
- Expected: `→ suspended`

---

## Step 6: agent/6 — incoming unfriend clause

**GLP change**: Modify `msg(From, Id, unfriend(_))` network clause. Add:
- If FMap[From] doesn't exist → ignore
- If FMap[From].epoch > incoming_epoch → ignore (stale)
- If FMap[From] inactive at same or newer epoch → ignore
- `map_put(FMap, From, flv1(From, epoch, inactive), FMap1)`
- `remove_output(friend(From), ...)`

**Files changed**: `typed_social_agent.glp`

**Test 6a — receive unfriend from active friend**:
- Bob has `flv1(alice, 1, active)` in FMap
- Receives `unfriend(alice)` with epoch 1
- Verify FMap has `flv1(alice, 1, inactive)`
- Expected: `map_show` on bob shows inactive

**Test 6b — stale unfriend ignored**:
- Bob has `flv1(alice, 3, active)` in FMap
- Receives `unfriend(alice)` with epoch 2
- Verify: ignored, FMap unchanged
- Expected: FMap still `flv1(alice, 3, active)`

---

## Step 7: Re-befriend after unfriend (epoch increment)

**GLP change**: No new clauses — tests that Steps 2-4 handle epoch correctly when FMap has inactive entry.

**Files changed**: None (verification of existing logic)

**Test 7a — full cycle**:
- Alice connects bob (epoch 1) → both accept → both FMaps have `flv1(_, 1, active)`
- Alice unfriends bob → both FMaps have `flv1(_, 1, inactive)`
- Alice connects bob again → epoch 2
- Both accept → both FMaps have `flv1(_, 2, active)`
- Expected: `map_show` shows epoch 2, active

**Test 7b — text message after re-befriend**:
- After re-befriend at epoch 2, alice sends text to bob
- Verify message arrives
- Expected: bob receives text from alice

---

## Step 8: Stale accept/request rejection

**GLP change**: No new clauses — tests that staleness guards in Steps 3-4 work.

**Test 8a — stale accept ignored**:
- Alice has `flv1(bob, 2, active)` (already re-befriended at epoch 2)
- A delayed accept from epoch 1 arrives
- Verify: ignored, FMap unchanged at epoch 2

**Test 8b — stale request ignored**:
- Bob has `flv1(alice, 2, inactive)`
- A delayed request from epoch 1 arrives
- Verify: ignored

---

## Step 9: FoFMap — friend_list clause

**GLP change**: Add/modify `msg(From, Id, update_friend_list(Names))` clause:
- For each name in list: create FLv2 entry
- `map_get(FoFMap, From, InnerMap)` — get or create inner map
- For each friend: `map_put(InnerMap, name, flv2(name, epoch, state), InnerMap1)`
- `map_put(FoFMap, From, InnerMap1, FoFMap1)`

**Files changed**: `typed_social_agent.glp`

**Test 9a — 3-agent friend list**:
- A↔B befriend, A↔C befriend
- A sends friend_list to B (containing C)
- Verify B's FoFMap[A] contains `flv2(C, 1, active)`
- Expected: nested map visible via map_show or map_get

**Test 9b — 4-agent scenario**:
- A↔B, A↔C, A↔D
- B receives A's friend list: [C, D]
- C receives A's friend list: [B, D]
- Verify FoFMaps correct

---

## Step 10: should_update procedure

**GLP change**: Add GLP procedure:
```
procedure should_update(FLv2?, FLv2?).
```
Returns true if incoming entry should replace stored entry:
- incoming.epoch > stored.epoch → true
- incoming.epoch == stored.epoch, stored active, incoming inactive → true
- otherwise → false

**Files changed**: `typed_social_agent.glp`

**Test 10a — higher epoch updates**:
- Stored: `flv2(bob, 1, active)`, incoming: `flv2(bob, 2, active)` → update

**Test 10b — same epoch, active→inactive updates**:
- Stored: `flv2(bob, 1, active)`, incoming: `flv2(bob, 1, inactive)` → update

**Test 10c — same epoch, inactive→active does NOT update**:
- Stored: `flv2(bob, 1, inactive)`, incoming: `flv2(bob, 1, active)` → no update

**Test 10d — lower epoch does NOT update**:
- Stored: `flv2(bob, 3, active)`, incoming: `flv2(bob, 2, active)` → no update
