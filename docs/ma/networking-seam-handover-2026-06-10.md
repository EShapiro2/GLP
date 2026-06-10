# Networking Seam Handover Report

**Date:** 2026-06-10
**Author:** Claude (GLP Code session)
**Status:** Completed (with noted follow-ups)
**Spec:** `docs/ma/networking-seam-spec.md` v0.2; `madGLP-spec.md` §8.3; `known-issues.md` Issue 7

---

## Summary

Implemented the madGLP networking seam per seam-spec v0.2: the `GlpNetwork`
interface, the `SimulationNetwork` realization (router + client), the runtime
adapter routing the isolate stack through it, and the Issue 7 hold mechanism.
The multiagent baseline stayed green throughout; final suite `+115 ~5 -0`.

---

## Completed Work

### Issue 7 — early `_r` hold mechanism (spec §5, madGLP §8.3)
- `lib/multiagent/mad_context.dart`: added a hold table keyed `(remoteAgent,
  remoteIndex)`. `_handleReaderAssignment` now holds an assignment with no
  matching `LocalizeEntry` instead of throwing; `_deliverHeldReaderAssignments`
  delivers it after every `localize()` site creates the entry.
- `test/multiagent/mad_transactions_test.dart`: the pre-Issue-7 test asserting
  `_r`-missing-entry **throws** was rewritten to assert the new hold→deliver
  contract (the `_w`-missing-entry test still asserts a throw — only `_r` holds).

### GlpNetwork interface (spec §2)
- `lib/multiagent/glp_network.dart` (new): `Transport`, `TrustLevel`, `PubKey`
  (32-byte Ed25519, value equality + hex), `DiscoveredPeer`, and the abstract
  `GlpNetwork`. `sign`/`verify` are synchronous per v0.2.

### SimulationNetwork (spec §3)
- `lib/multiagent/simulation_network.dart` (new): `NetworkDirectory`,
  `generateKeyPair()` (synchronous `ed25519_edwards`), `SimulationRouter`
  (directory, adjacency, trust, per-pair cut/hold queues, messageId; delivery
  and connectivity via injected sinks → testable in-process and reusable by
  `IsolateManager`), and `SimulationNetworkClient` (the agent-side `GlpNetwork`,
  real Ed25519 sign/verify).

### Runtime adapter (spec §4)
- `lib/multiagent/isolate_manager.dart`: `IsolateManager` owns a
  `SimulationRouter` (all agents registered, trust Open, total adjacency).
  `AgentConfig` gained `keyPair` + `directory`. The agent isolate builds a
  `SimulationNetworkClient`; `ctx.onMessageReady → network.send`, and
  `network.onMessageReceived → handleMadAssignment`. `MessageType` removed from
  the wire (`NetworkMsg` → `RouterSend`/`Deliver`). `cut/restore/holdDelivery/
  releaseDelivery` exposed on `IsolateManager`. Behavior-preserving: total
  adjacency + Open trust ⇒ immediate delivery; wire payload bytes unchanged.

### Tests (spec §7)
- `test/multiagent/simulation_network_test.dart` (new, 15): directory, PubKey,
  delivery, bootComplete, trust (§7.4), cut/restore (§7.2), hold/reverse-release
  (§7.3 router level), client send round-trip, real Ed25519 sign/verify.
- `test/multiagent/reverse_order_delivery_test.dart` (new, 2): two `MadContext`s
  through one `SimulationRouter` with real serialize/deserialize glue; in-order
  and reverse-order cold-call both reach the same outcome (§7.3 / Issue 7).

### Dependency
- `pubspec.yaml`/`.lock`: removed `cryptography` (async-only), added
  `ed25519_edwards 0.3.1` (synchronous), per v0.2.

---

## Current State

| Suite | Result |
|-------|--------|
| `dart test test/multiagent/` | `+115 ~5 -0` (All tests passed) |

Baseline before work: `+98 ~5 -0` (the formerly pre-existing
`mad_cold_call_isolate_test` failure was already passing). +17 new tests.

Commits on `main`: Issue 7 `ae327fe8`; interface+SimulationNetwork (step 2);
adapter `6aea4c64`; reverse-order test (step 5). **Not pushed** — awaiting Udi.

---

## Follow-ups / deviations from v0.2 (for spec harmonisation)

1. **`agent_runtime.dart` not migrated to `GlpNetwork`.** §4/§9 list it as
   changed; it is the Flutter-app path, not exercised by `test/multiagent/`. Left
   on the existing `onMessageReady`/`onMadMessageReceived` path to avoid risking
   the app outside test coverage. Migrate when the app is in scope.
2. **Connectivity callbacks not plumbed cross-isolate in the live stack.**
   `onPeerConnected`/`onPeerDiscovered` fire and are tested at the router level
   in-process; the live isolate stack does not forward them to agents because no
   GLP play consumes them. Add router→client event plumbing if a play needs it.
3. **Body kernels `sign/2`, `verify_attestation/4` deferred** (already noted in
   v0.2): `GlpNetwork.sign`/`verify` are implemented; kernel wiring waits on
   their term encoding being specified.
4. **§7.5 plays reorder check**: no dedicated full-isolate test (timing-flaky).
   Covered by the order-independent seam (§7.3) + the full play suite passing
   over the reverse-capable router. A deterministic per-sender cold-call reorder
   play test is a recommended follow-up.
5. **Stale skipped placeholder**: `mad_error_handling_test.dart` "receive for
   non-existent LocalizeEntry throws" describes pre-Issue-7 behavior; left
   untouched (skipped, no assertion). Update its text when convenient.

---

## Next Steps

1. Udi: decide on push (the change is on `main`, unpushed).
2. Fold the follow-ups above back into the spec or into `known-issues.md`.
3. When the body-kernel term encoding is specified, wire `sign/2` /
   `verify_attestation/4` to `network.sign`/`verify`.
