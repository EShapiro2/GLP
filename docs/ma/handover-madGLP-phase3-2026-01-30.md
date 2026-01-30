# madGLP Implementation Handover

**Date**: 2026-01-30  
**Status**: Phase 2 Complete, Phase 3 Ready to Begin  
**Previous Session**: Planning and implementation of madGLP Phases 0-2

---

## Context

We are migrating from irmaGLP (request-based multiagent communication) to madGLP (push-based communication via spawned GLP goals). This is a fundamental architectural change specified in the CGLP paper.

### Source of Truth

The authoritative specification is the **paper**, not the derived spec:

- **Paper**: `/Users/udi/Grassroots/CGLP/sections/madGLP.tex`
- **Spec** (derived from paper): `/Users/udi/Grassroots/GLP/docs/ma/madGLP-spec.md` (v4.1)
- **Implementation Plan**: `/Users/udi/Grassroots/GLP/docs/ma/madGLP-implementation-plan.md`

Per DISCIPLINE.md: Always check the paper first when questions arise about semantics.

---

## What Has Been Completed

### Phase 0: Setup (Complete)

- Baseline established: 151 passing / 23 failing multiagent tests (failures are known irmaGLP isolate issues)
- Archive created: `glp_runtime/lib/multiagent/archive-irma-2026-01-30/`
- Test stubs created: 23 new test stubs across 5 files (all skipped pending implementation)

### Phase 1: GlobalWritersTable (Complete)

- File: `glp_runtime/lib/multiagent/global_writers_table.dart`
- Implements two entry types per spec Section 3:
  - `GlobalizeEntry(writerAddr, remoteAgent)` - for entries created when globalizing a reader
  - `LocalizeEntry(writerAddr, remoteAgent, remoteIndex)` - for entries created when localizing a writer global name
- Single counter for index allocation, indices never reused
- 6 tests passing

### Phase 2: Globalize/Localize Operations (Complete)

- File: `glp_runtime/lib/multiagent/mad_helpers.dart`
- Types implemented:
  - `GlobalName` - represents `_w(p, i)` or `_r(p, i)`
  - `GlobalSendSpawn` - info needed to spawn a `global_send` goal (readerAddr, globalName, destAgent)
  - `TermVar` - simple variable representation for testing
  - `GlobalizeResult` / `LocalizeResult` - operation results
  - `FreshPair` - fresh variable pair created during localization
- Operations implemented per spec Section 5:
  - `globalize()` - transforms term for sending, returns global names and spawn info
  - `localize()` - transforms received term, creates fresh pairs, returns spawn info
- 8 tests passing (5 globalize + 3 localize)

**Current test count**: 157 passing, 17 skipped (Phase 3+ stubs), 23 failing (pre-existing)

---

## What Needs to Be Done

### Phase 3: global_send Mechanism

This is the core of madGLP. The `global_send/3` predicate is defined in the paper (Section 4) as:

```prolog
global_send(T, G, Q) :- known(T) | '_send'(T, G, Q).
```

Where:
- `T` is a reader whose value will be sent when known
- `G` is the global variable name (`_w(p,i)` or `_r(p,i)`)
- `Q` is the destination agent
- `known(T)` succeeds when T is bound to a non-variable term (standard GLP guard semantics)
- `'_send'(T, G, Q)` globalizes T and adds message `(G := T↑, Q)` to M_p

**Key insight**: `global_send` is a regular GLP goal that uses standard suspension semantics. When the guard `known(T)` fails (T is unbound), the goal suspends on T. When T becomes bound, the goal resumes and reduces.

**What needs to be implemented**:

1. A way to spawn `global_send` goals into the resolvent (from `GlobalSendSpawn` info)
2. The `'_send'/3` builtin that:
   - Globalizes the value T (which may produce more `GlobalSendSpawn`s for nested variables)
   - Registers those new spawns atomically
   - Adds the assignment message to M_p
3. Integration with existing goal queue and suspension mechanism

**Test file**: `glp_runtime/test/multiagent/global_send_test.dart` (4 tests, currently skipped)

### Phase 4: Transaction Updates

Update the transaction handling in `irma_context.dart`:

- **Reduce**: No longer generates messages directly; `global_send` goals handle outgoing messages
- **Send**: Dequeue from M_p (already exists, may need minor updates)
- **Receive**: Lookup entry, localize, bind writer, remove entry (needs rewrite for new table structure)
- **Network**: Atomic globalize at sender, localize at receiver (needs rewrite)

**Test file**: `glp_runtime/test/multiagent/mad_transactions_test.dart` (tests in 0.5, currently skipped)

### Phase 5: Cleanup

- Remove dead code: `request()`, `abandon()`, `VariableRole`, `ReadRequest`/`Abandon` messages
- Update remaining tests
- Verify no regressions

### Error Handling Tests

**Test file**: `glp_runtime/test/multiagent/mad_error_handling_test.dart` (5 tests, currently skipped)

Negative test cases to implement:
- Receive for non-existent entry throws
- Duplicate LocalizeEntry rejected
- `global_send` on already-known reader is no-op
- Removing non-existent entry is safe

---

## Key Files

### Implementation

| File | Status | Purpose |
|------|--------|---------|
| `glp_runtime/lib/multiagent/global_writers_table.dart` | Complete | W_p table structure |
| `glp_runtime/lib/multiagent/mad_helpers.dart` | Complete | Globalize/Localize operations |
| `glp_runtime/lib/multiagent/irma_context.dart` | Needs update | Transaction handling |
| `glp_runtime/lib/multiagent/irma_agent.dart` | Needs update | Agent runtime |
| `glp_runtime/lib/runtime/system_predicates_impl.dart` | Needs `'_send'` | Builtins |

### Tests

| File | Tests | Status |
|------|-------|--------|
| `global_writers_table_test.dart` | 6 | Passing |
| `globalize_test.dart` | 5 | Passing |
| `localize_test.dart` | 3 | Passing |
| `global_send_test.dart` | 4 | Skipped |
| `mad_error_handling_test.dart` | 5 | Skipped |

### Documentation

| File | Purpose |
|------|---------|
| `docs/ma/madGLP-spec.md` | Implementation spec (v4.1) |
| `docs/ma/madGLP-implementation-plan.md` | Phased plan with TDD tests |
| `CGLP/sections/madGLP.tex` | **Paper (authoritative)** |

---

## Important Clarification

In the previous session, I incorrectly used the term "callback" to describe the `global_send` mechanism. This is wrong. `global_send` is a **GLP goal** that:

1. Gets spawned into the agent's resolvent (active queue A_p)
2. Has a guard `known(T)` that causes suspension if T is unbound
3. When T becomes known (its writer is bound), the goal resumes and reduces
4. The body `'_send'(T, G, Q)` executes, globalizing T and adding a message to M_p

This is standard GLP suspension/reduction semantics, not an imperative callback mechanism. The existing runtime's guard evaluation and suspension handling should work; we just need to implement the `'_send'` builtin and wire up goal spawning.

---

## Commands

**Run multiagent tests**:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/ 2>&1 | tee /tmp/results.txt
```

**Run specific test file**:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/global_send_test.dart
```

**Read DISCIPLINE.md**:
```bash
cat /Users/udi/Grassroots/GLP/CLAUDE.md
```

---

## Next Steps for Phase 3

1. Read the paper section on `global_send` (`CGLP/sections/madGLP.tex`, Section 4)
2. Understand how goals are currently spawned and suspended in the runtime
3. Implement `global_send` goal spawning from `GlobalSendSpawn` info
4. Implement `'_send'/3` builtin
5. Enable and pass the 4 tests in `global_send_test.dart`
6. Run full test suite to verify no regressions

---

## Estimated Remaining Effort

| Phase | Estimated Time |
|-------|----------------|
| Phase 3: global_send | 4-5 hours |
| Phase 4: Transactions | 3-4 hours |
| Phase 5: Cleanup | 2-3 hours |
| Testing/Integration | 4-5 hours |

**Total**: ~15-17 hours remaining
