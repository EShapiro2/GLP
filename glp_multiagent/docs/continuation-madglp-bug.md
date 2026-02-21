# Continuation: madGLP introduction stall bug

**Written: 2026-02-21**
**Updated: 2026-02-21**

## Status

Spec violation in `AgentRuntime._runUntilQuiescent` identified and fixed.
The introduction stall has not yet been retested in the Flutter app.

## What was found

Comparing the headless `IsolateManager._agentIsolateEntry` (which works) with
the Flutter `AgentRuntime._runUntilQuiescent` (which stalls), we found that
`_runUntilQuiescent` violated the agent-runtime-spec in three ways:

1. **20-round cap** (`for (var round = 0; round < 20; round++)`) — the spec
   says drain all runnable goals, no cap.
2. **maxCycles: 1000** passed to `drainAsyncWithStatus` — the spec says run
   to completion.
3. **`processSuspension` call** between drain and flush — not in the spec;
   `processSuspension` is a no-op in madGLP's push model.

The headless code correctly does one `drainWithStatus()` + one `flushMessages()`.

## What was fixed

- `agent_runtime.dart`: Replaced the 20-round loop with a single
  `drainWithStatus()` + `flushMessages()`, matching the spec and the headless
  implementation.
- `mad_cold_call_isolate_test.dart`: Fixed a pre-existing bug where Alice
  globalized the reader instead of the writer (wrong per spec Section 10.2).

All tests pass: 316/316 REPL, 72/72 multiagent (5 skipped).

## Next step

Rebuild and test the Flutter app (`main_sg_mad.dart`) to see if Play 1
introductions now complete.  If they still stall, the next hypothesis is
serialization of nested channel variables inside cold-call messages.
