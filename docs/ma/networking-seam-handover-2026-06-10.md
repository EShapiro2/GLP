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
| `dart test test/multiagent/` | `+115 ~5 -0` (baseline `+98 ~5 -0`; +17 new tests) |
| `dart test` (full) | `+370 ~5 -0` |
| REPL (`bash test/run_all_tests.sh`) | 491/491 passed, 0 failed |

The formerly pre-existing `mad_cold_call_isolate_test` failure was already
passing at baseline.

Commits on `main`: Issue 7 `ae327fe8`; interface+SimulationNetwork (step 2);
adapter `6aea4c64`; reverse-order test (step 5). **Not pushed** — awaiting Udi.

---

## Follow-ups / deviations from spec (resolved as of v0.3)

1. **`agent_runtime.dart` + cross-isolate connectivity callbacks deferred** —
   now tracked as `known-issues.md` **Issue 8** (deferral approved). The live
   Flutter-app path stays on the old transport; migrate when the app can be
   manually verified.
2. **Body kernels `sign/2`, `verify_attestation/4` deferred** (per v0.2/v0.3):
   `GlpNetwork.sign`/`verify` are implemented; kernel wiring waits on their term
   encoding being specified.
3. **§7.5 plays cold-call-order check: DONE by inspection — pass.** All
   test-exercised plays were inspected; no play relies on per-sender cold-call
   arrival order (each sender's cold-calls go to distinct recipients; repeated
   `connect` occurrences are comments or alternative committed-choice clauses).
   Recorded in `known-issues.md` Issue 7 Related Check. No dedicated full-isolate
   reorder test (timing-flaky); §7.3 covers the mechanism deterministically.
4. **Stale skipped placeholder reworded**: `mad_error_handling_test.dart`
   "...LocalizeEntry throws" → "early _r assignment ... is held until the entry
   exists" (kept skipped; the live behavior is tested in `mad_transactions_test`
   and `reverse_order_delivery_test`).
5. **Spec updated to v0.3**: §10 Implementation Status added; §9 corrected
   (`boot_loader.dart` unchanged, `agent_runtime.dart` deferred per Issue 8,
   §7.5 by inspection).

---

## Next Steps

1. Issue 8: migrate `agent_runtime.dart` + connectivity callbacks when the
   Flutter app can be manually verified.
2. When the body-kernel term encoding is specified, wire `sign/2` /
   `verify_attestation/4` to `network.sign`/`verify`.
