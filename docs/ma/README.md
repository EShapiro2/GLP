# Multi-Agent GLP (madGLP) Documentation

**Last updated:** 2026-05-18

This directory contains the active specifications for the multi-agent GLP runtime — agents running in separate Dart isolates that communicate via message-passing with serialised payloads.  Earlier "irmaGLP" work, phase handovers, and bug-investigation notes have been archived (in `archive/`) or removed.

## Specifications

| Document | Description |
|----------|-------------|
| [`madGLP-spec.md`](madGLP-spec.md) | Core multi-agent runtime (isolates, variable tables, message queues, globalise/localise) |
| [`agent-runtime-spec.md`](agent-runtime-spec.md) | Agent process inside an isolate |
| [`isolate-boot-spec.md`](isolate-boot-spec.md) | Multi-isolate boot orchestration (used by `mad_boot/mad_fplayN.glp`) |
| [`multi-agent-trace-spec.md`](multi-agent-trace-spec.md) | Trace format for multi-agent runs |
| [`ui-io-spec.md`](ui-io-spec.md) | UI / I/O integration |
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
| `variable_table.dart`, `message_queue.dart`, `payload_serializer.dart` | Per-isolate state |
| `relay.glp` | GLP-side relay |
| `repl_play_runner.dart` | REPL invocation |

Tests: `/Users/udi/Grassroots/GLP/glp_runtime/test/multiagent/`.

## Scope rule for Claude

When working on multi-agent code, modify only `glp_runtime/lib/multiagent/` and `glp_runtime/test/multiagent/`.  Do not modify core GLP files without explicit discussion.  If a core-GLP bug blocks multi-agent work, STOP and report.  (See `/Users/udi/Grassroots/GLP/CLAUDE.md` — "maGLP work — scope restriction".)
