# Multi-Agent GLP (madGLP) Documentation

**Last updated:** 2026-10-02

This directory contains the specifications for the multi-agent GLP runtime — agents running in separate Dart isolates that communicate via message-passing with serialised payloads.  Where IGLP states a part of them, IGLP governs.  Earlier "irmaGLP" work, phase handovers and bug-investigation notes have been removed; git holds them.

## Specifications

| Document | Description |
|----------|-------------|
| [`madGLP-spec.md`](madGLP-spec.md) | Core multi-agent runtime (isolates, variable tables, message queues, globalise/localise) |
| [`agent-runtime-spec.md`](agent-runtime-spec.md) | Agent process inside an isolate |
| [`isolate-boot-spec.md`](isolate-boot-spec.md) | Multi-isolate boot orchestration (used by `mad_boot/mad_fplayN.glp`) |
| [`multi-agent-trace-spec.md`](multi-agent-trace-spec.md) | Trace format for multi-agent runs |
| [`HOW-TO-RUN.md`](HOW-TO-RUN.md) | How to run multi-agent plays |

## Implementation

Lives in `/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/`:

| File | Purpose |
|------|---------|
| `agent_runtime.dart` | Per-isolate agent runner |
| `isolate_manager.dart` | Isolate lifecycle |
| `boot_loader.dart` | Loads the boot orchestrator |
| `mad_context.dart`, `mad_helpers.dart` | Globalise / localise / variable threading |
| `global_send.dart`, `global_writers_table.dart` | Outgoing variable management |
| `imported_writer_records.dart` | Imported-writer records |
| `message_queue.dart` | Message types |
| `glp_network.dart`, `simulation_network.dart` | The networking interface and its simulation |
| `identity.dart` | The person's signing key pair |

Payloads are encoded by `glp_runtime/lib/wire/payload_codec.dart`.  Tests: `/Users/udi/Grassroots/GLP/glp_runtime/test/multiagent/`.

## Who changes what

Ownership is Coordination Appendix B, `/Users/udi/Grassroots/Coordination/sections/B-code-map.tex`; the rules for every change are `/Users/udi/Grassroots/GLP/CLAUDE.md`.
