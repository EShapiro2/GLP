# Globalize Direction Analysis

**Date**: 2026-02-10
**Status**: Resolved — adopted corrected (paper) definitions

## The Problem

Globalize sees "writer Y in term" but there are two cases with opposite flow directions:

### Case A: Stream (p assigns the writer)

p assigns `Xs := [add|Xs1]`. The `_send` builtin globalizes the value `[add|Xs1]`.
Xs1 is a **writer** in the value. p keeps Xs1 in its resolvent and will assign it later.
Value flows p→q.

Needs: gs at p (watches Xs1?, fires when p assigns Xs1), entry at q.

**Old definition (writer → gs at globalizer) is correct.**

### Case B: Writer-response (q assigns the writer)

agent1 sends `ack(Resp)` via cold-call. The `_send` builtin globalizes `ack(Resp)`.
Resp is a **writer** in the value. agent1 keeps Resp in its resolvent but does NOT assign it.
agent2 should get a writer and assign it. Value flows q→p.

Needs: gs at q (watches Y_q?, fires when q assigns Y_q), entry at p.

**Corrected definition (writer → entry at globalizer) is correct.**

### The distinction

Both cases have a writer in the globalized term. Both have p retaining the writer in its resolvent. The difference is purely about which agent will assign the writer — a semantic question about future program behavior that globalize cannot determine from the variable alone.

## Reader case

For readers, only one case exists:

### Case C: Send-reader (p assigns the paired writer)

agent1 sends `data(X?)` via cold-call. X? is a **reader** in the value.
agent1 keeps X (writer) and will assign it. Value flows p→q.

Needs: gs at p (watches X?, fires when p assigns X), entry at q.

**Corrected definition (reader → gs at globalizer) is correct.**
**Old definition (reader → entry at globalizer) is wrong — creates deadlock.**

Note: there is no "reader response" case. If q needs to write back, q receives a writer (Case B), not a reader.

## Summary

| Variable type | Who assigns | Flow | gs at | entry at | Old def | Corrected def |
|--------------|-------------|------|-------|----------|---------|---------------|
| Writer (stream) | p (globalizer) | p→q | p | q | ✓ | ✗ |
| Writer (response) | q (localizer) | q→p | q | p | ✗ | ✓ |
| Reader | p (globalizer) | p→q | p | q | ✗ | ✓ |

Old definition is correct for writers-assigned-by-globalizer but wrong for readers.
Corrected definition is correct for readers and writers-assigned-by-localizer but wrong for writers-assigned-by-globalizer.

Neither the old nor the corrected definitions handle all cases.

## Resolution (2026-02-10)

Approach 4 was adopted: **the programmer distinguishes**. The corrected definitions from the CGLP paper appendix are correct and sufficient, because GLP's reader/writer semantics already encode the intended direction:

- **Writer in sent term** = receiver gets the writer and will assign it (value flows receiver→sender). Example: `ack(Resp)` where Resp is a writer the receiver should bind.
- **Reader in sent term** = sender keeps the paired writer and will assign it (value flows sender→receiver). Example: `data(X?)` where sender keeps X and will assign it.

Case A (stream) was a misanalysis. When p assigns `Xs := [add|Xs1]`, the nested globalize of the value `[add|Xs1]` processes Xs1. But Xs1 is the *continuation writer* — p will assign it later, and q should *receive* the value. So p should actually be sending Xs1's paired **reader** Xs1?, not the writer. The `_send` builtin (or the GLP program) must use the reader form for continuation variables. This is exactly what happens in practice: the stream protocol sends readers for continuation points.

The spec (v5.3), the paper appendix (Definition [Globalize], [Localize]), and the code implementation are now all aligned on the corrected definitions:

| Variable type | gs at | entry at | Localizer gets |
|--------------|-------|----------|----------------|
| Writer Y     | —     | globalizer p | writer (can assign, sends back) |
| Reader Y?    | globalizer p | — | reader (waits for value) |

All unit tests (globalize, localize, global_send, transactions, scenarios) pass. The remaining integration test failures (`send_reader`, `three_agent_pipeline`) are unrelated to the Globalize/Localize definitions.
