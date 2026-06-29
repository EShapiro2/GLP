# GLP over the Networking API — Where Things Stand

**Date**: 2026-06-12
**Status**: Draft for Dan's approval
**References**: GLP-Networking-API paper; `dan-networking-gap-analysis.md` v0.6; `networking-seam-spec.md` v0.6

## What runs today

The GLP multiagent runtime (madGLP) runs multi-agent programs with agents as Dart isolates, on desktop. All inter-agent traffic crosses a single Dart interface, `GlpNetwork`, written as a transcription of the API paper — Section 2 plus the BLE calls: identity install, reachability, send and receive callbacks, discovery, trust level, sign/verify. The realization beneath it today is an in-process `SimulationNetwork`: real Ed25519 keys and signatures, fair unordered delivery, queuing for not-yet-reachable agents. It omits what has no meaning in-process — Noise sessions, BLE advertising, NAT traversal — exactly as the paper's Simulation Realization appendix states. The paper's in-language predicates run for real: `sign/2` and the guard `valid_attestation/4`, over the canonical term serialization pinned in the paper; signatures are byte-compatible across realizations by definition.

## Dan's layer against the interface

`GrassrootsNetwork` is near `GlpNetwork`: same receive-callback shape, reachability, trust levels, Noise XX per medium per pair, UDX over UDP, hole punching. Differences on record in the gap analysis: no queuing (a send to an unreachable peer fails; the paper has the layer deliver when connectivity returns); rendezvous by friend signaling (Udi's ruling: the paper's dedicated rendezvous server, with the existing configured-servers support as the basis — friends on smartphones have no stable public address); static BLE suffix (the paper's rotating suffix; platform support confirmed in the paper text); ANNOUNCE carries UDP address candidates (the paper's ANNOUNCE is the key only — address sharing is GLP-level, per the IP section); no peer links. Nickname-out-of-ANNOUNCE and trust-default-Closed are already fixed on branch `glp-api-alignment` (PR pending review).

## The integration step

Nothing in madGLP changes for phones: the isolates and the runtime run on-device as they are. The swap is one object, constructed beneath `GlpNetwork` at boot: `SimulationNetwork` out, the real layer in — via an adapter class implementing `GlpNetwork` over `GrassrootsNetwork`: callback mapping, identity install, sign/verify from the installed identity, and a queue above the no-queue send to realize the paper's fair delivery. The adapter is not yet written; it is the next code artifact, and the natural first proof is two madGLP runtimes on two desktops exchanging GLP messages over the UDP path, then phones and BLE on real devices.

## Open items

On the layer side: the rendezvous revision per the ruling above; the rotating suffix; the liveness section Dan plans after 2.2 (keeping address distribution out of ANNOUNCE); the PR review. On the runtime side: the adapter (spec first); public-key identifiers in agent names, parked until real keys route between machines.

## Asked of Dan

Approve that this matches your understanding of both sides, or say what is wrong or missing. Next steps get decided on this basis.
