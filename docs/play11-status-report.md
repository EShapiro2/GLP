# Play11 Status Report — 2026-02-25

## Overview

Play11 is a 6-agent demo: 3 parents (Alice, Bob, Charlie) and 3 children (Carol, Dave, Eve). The scenario has 4 phases: (1) parents connect, (2) parents introduce children, (3) Carol creates a clique group and invites Dave and Eve, (4) all three children chat in the group.

## Current State

**fplay11 (tagged output):** Suspends. Produces 65 tagged lines then hangs. Eve is stuck waiting for Dave's group message, which never arrives.

**play11 (sink output):** Not tested with current code changes.

**play8 and earlier plays:** Were working before current changes. Not re-tested.

**Flutter app (main_cssg_groups.dart):** Builds successfully. Runs fplay11 but shows the suspension bug.

## Files Modified This Session

- `typed_ui_actors.glp` — Added play11 actors (alice11 through eve11), plus ad-hoc interleaving states for Dave and Eve (see Issue 2 below)
- `play_ui_sim_boot.glp` — Added play11 and fplay11 boot wiring
- `main_cssg_groups.dart` — New Flutter app with 6 agent panels
- `CLAUDE.md` (top-level) — Added spec-first rule for actor scripts
- `GLP/CLAUDE.md` — Strengthened spec-first section, added CSSN group spec to reference list

## Open Issues

### Issue 1: Spec vs Implementation — "all other members" vs "all members"

**Spec says** (group-glp-implementation-spec.md, lines 69–73):
> - Creator broadcasts the message to **all other members**.
> - Each member receives `group_received(GroupId, From, Content)`.

**Code does** (typed_social_agent.glp, line 665–671):
`distribute_group_msg` sends the message to every `group_member` entry matching the GroupId, with **no exclusion of the author**. The author receives their own message back as a `group_received`.

**Discrepancy:** The spec says "all other members" (excluding the author). The code sends to all members including the author. This means every sender gets an echo of their own message.

**Question for Udi:** Is the spec correct (should exclude author) or is the implementation correct (send to all, including author)? This affects what the actor scripts should expect.

### Issue 2: Ad-hoc Interleaving States Added to Dave and Eve

During debugging, before reading the spec, I added extensive interleaving states to `dave11_wait_group_joined` and `eve11_wait_group_joined` to handle `group_received` messages arriving before the actor's own `group_joined` notification. This includes:

For Dave:
- `dave11_wait_group_joined_got_eve`
- `dave11_wait_group_joined_got_carol`
- `dave11_wait_group_joined_got_both`

For Eve:
- `eve11_wait_group_joined_got_dave`
- `eve11_wait_group_joined_got_carol`
- `eve11_wait_group_joined_got_both`

**These are ugly and were written without consulting the spec.** They should be replaced with clean code once the spec questions are resolved. Some of these states may be unnecessary if we simplify what Dave and Eve expect to receive.

### Issue 3: Eve Waits for Dave's Message That Never Arrives

**Root cause:** Dave joins the group first and sends his message immediately. Eve hasn't joined yet. Per the spec, the group broadcasts to current members only — no history replay. So Eve never receives Dave's message.

Eve's `eve11_wait_group_chat` expects both `group_received(dave, ...)` and `group_received(carol, ...)`. The Dave message never arrives. Eve suspends.

**Fix (pending agreement):** Eve should only wait for Carol's reply after sending her own message. Carol always sends last (she waits for both Dave's and Eve's messages). Carol's reply is the only message guaranteed to reach all members regardless of join order.

Similarly, Dave's code was already simplified to only wait for Carol's reply (`dave11_wait_carol_reply`). Eve needs the same simplification.

### Issue 4: Dave's Simplification Already Applied, Eve's Not Yet

Dave's group chat code was replaced with a simple `dave11_wait_carol_reply` — send message, then wait only for Carol's reply, skipping everything else via `otherwise`.

Eve still has the old code expecting both Dave's and Carol's messages, plus all the ad-hoc interleaving states from Issue 2.

### Issue 5: Carol's `carol11_wait_eve_joined` Has a Similar Interleaving Fix

From the previous session: Carol has a `carol11_wait_eve_joined_got_dave` state to handle Dave's group message arriving while Carol waits for Eve to join. This fix is correct per the spec — Carol is a member, Dave is a member, Dave sends, Carol receives. The interleaving is real. This one should stay.

## What Needs to Happen (Proposed)

1. **Resolve Issue 1:** Decide whether `distribute_group_msg` should exclude the author.
2. **Fix Eve's actor:** Simplify to only wait for Carol's reply (matching Dave's fix).
3. **Clean up Dave's actor:** Remove the ad-hoc `got_eve`/`got_carol`/`got_both` states. Dave just needs `dave11_wait_group_joined` → send → `dave11_wait_carol_reply`.
4. **Test fplay11:** Should complete without suspension.
5. **Re-test play8 and earlier plays:** Verify no regressions.
6. **Rebuild Flutter app.**

## Spec Documents

- **Group spec:** `/Users/udi/Grassroots/SGLP/docs/group-glp-implementation-spec.md`
- **Agent implementation:** `/Users/udi/Grassroots/GLP/programs/typed_book/cssn/typed_social_agent.glp`
- **Actor scripts:** `/Users/udi/Grassroots/GLP/programs/typed_book/cssn/typed_ui_actors.glp`
- **Boot wiring:** `/Users/udi/Grassroots/GLP/programs/typed_book/cssn/play_ui_sim_boot.glp`
- **Flutter app:** `/Users/udi/Grassroots/GLP/glp_multiagent/lib/main_cssg_groups.dart`
