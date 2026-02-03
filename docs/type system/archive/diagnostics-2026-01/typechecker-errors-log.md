# Typechecker Errors Diagnostic Log

Generated: 2026-01-24
Status: 68 failing tests out of 219 (151 passing)

## Classification Legend

- **SRSW_VIOLATION**: Program violates SRSW rules (loading error). Needs source fix.
- **PROGRAM_ERROR**: Program has type errors (mode mismatch, undefined procedures, etc.). Needs source fix.
- **TYPE_CHECKER_BUG**: Program appears valid per spec, but checker rejects it. Fix the type checker.

---

## Summary

| Classification | Count |
|----------------|-------|
| SRSW_VIOLATION | 37 |
| PROGRAM_ERROR | 31 |
| TYPE_CHECKER_BUG | 0 |

**No type checker bugs were identified.** All failures are due to issues in the source programs.

---

## Detailed Classification by Directory

### streams/producers_consumers (5 failures)

| File | Classification | Main Error |
|------|----------------|------------|
| channels.glp | SRSW_VIOLATION | Multiple writers occur 2 times; variables have no reader |
| cooperative_producers.glp | SRSW_VIOLATION | Writer `Xs` occurs 2 times; `Next` has no writer |
| dynamic_merger.glp | SRSW_VIOLATION | Variables `W`, `V` have no writer |
| observers.glp | SRSW_VIOLATION | Writer `Ys` occurs 2 times; reader `Ys1?` occurs 2 times |
| parallel_table.glp | SRSW_VIOLATION | Multiple variables have no reader/writer |

### streams/buffered_communication (3 failures)

| File | Classification | Main Error |
|------|----------------|------------|
| bounded_buffer.glp | SRSW_VIOLATION | Writer variables `Buf`, `Tail`, `NTail` occur 2 times |
| bounded_buffer_original.glp | PROGRAM_ERROR | Invalid type syntax: uses `--` instead of `\` for diff-lists |
| switch2x2.glp | SRSW_VIOLATION | Reader variables occur 2 times without ground guard |

### streams/objects_monitors (6 failures)

| File | Classification | Main Error |
|------|----------------|------------|
| many_counters.glp | SRSW_VIOLATION | Multiple writers occur 2+ times; readers without ground guard |
| network_switch.glp | SRSW_VIOLATION | Variables `ChP`, `ChQ`, `ChR`, `X` have no reader |
| network_switch_3way.glp | SRSW_VIOLATION | Same pattern as network_switch |
| observed_monitor.glp | PROGRAM_ERROR | Mode mismatch: reader/writer direction errors |
| play_absolute.glp | SRSW_VIOLATION | Reader `T0?` occurs 6 times; undefined procedures |
| queue_manager.glp | SRSW_VIOLATION | Reader variables occur 2 times without ground guard |

### meta (7 failures)

| File | Classification | Main Error |
|------|----------------|------------|
| failsafe_meta.glp | PROGRAM_ERROR | Undefined procedure: `#/2` (module calls not supported) |
| abortable_meta.glp | SRSW_VIOLATION | Reader `M?` occurs 2 times; `Eq` has no reader |
| control_meta.glp | PROGRAM_ERROR | Undefined `#/2`; uncovered alternative `[]` |
| snapshot_meta.glp | PROGRAM_ERROR | Uncovered alternative `[]` for ControlList |
| termination_detection_meta.glp | PROGRAM_ERROR | Undefined procedure: `#/2` |
| tracing_meta.glp | PROGRAM_ERROR | Undefined `#/2`; mode mismatch in `replay` |
| runtime_control_meta.glp | PROGRAM_ERROR | Multiple mode mismatches; uncovered `[]` |

### modules (1 failure)

| File | Classification | Main Error |
|------|----------------|------------|
| main_module.glp | PROGRAM_ERROR | Undefined procedure: `#/2` (module calls not supported) |

### social_graph (17 failures)

| File | Classification | Main Error |
|------|----------------|------------|
| agent.glp | SRSW_VIOLATION | `ChUser`, `ChNet`, `UserOut`, `NetOut` have no readers |
| agent_full.glp | SRSW_VIOLATION | Multiple SRSW violations |
| attestation_guards.glp | SRSW_VIOLATION | Writers `Msg`, `Other` occur 2 times |
| cold_call.glp | PROGRAM_ERROR | Uncovered alternative `[]` for MsgList |
| friend_introduction.glp | PROGRAM_ERROR | No transition for `reject_intro`; uncovered `[]` |
| network.glp | SRSW_VIOLATION | Writers `P`, `Q` occur 2 times; no readers for `ChP`, `ChQ`, `Msg` |
| network2.glp | PROGRAM_ERROR | Uncovered alternatives for AgentEntry |
| network3.glp | PROGRAM_ERROR | Uncovered alternatives for AgentEntry |
| network4.glp | PROGRAM_ERROR | Uncovered alternatives for AgentEntry |
| play_4agents.glp | PROGRAM_ERROR | Undefined procedures: `lookup_send/4`, `inject_msg/5`, etc. |
| play_alice_bob.glp | PROGRAM_ERROR | Undefined procedures: `lookup_send/4`, `inject_msg/5`, etc. |
| play_cold_call.glp | SRSW_VIOLATION | Multiple writers occur 2-3 times |
| play_introduction.glp | PROGRAM_ERROR | Non-contiguous clauses for `social_graph/3` |
| plays/alice.glp | PROGRAM_ERROR | Mode mismatch: reader `In?` at output position |
| plays/bob.glp | PROGRAM_ERROR | Mode mismatch: reader `In?` at output position |
| plays/main.glp | SRSW_VIOLATION | Writers `AUserCh`, `BUserCh` occur 2 times; undefined `observe/3` |
| response_handling.glp | SRSW_VIOLATION | Reader `FOut?` occurs 2x; writers occur 2x |
| response_handling_unfolded.glp | PROGRAM_ERROR | Mode mismatch; type mismatch `Stream != MsgList?` |
| stream_security.glp | SRSW_VIOLATION | Writers `T`, `Xs` occur 2x; `Next` has no writer |
| streams.glp | SRSW_VIOLATION | Writer `X` occurs 2x; reader `Tag?` occurs 2x |

### social_networks (14 failures)

| File | Classification | Main Error |
|------|----------------|------------|
| direct_messaging.glp | SRSW_VIOLATION | Multiple writer occurrences |
| dm_simple.glp | PROGRAM_ERROR | Cannot redefine predefined type: Stream |
| feed.glp | SRSW_VIOLATION | Multiple writer occurrences |
| feed_server.glp | PROGRAM_ERROR | Cannot redefine predefined type: Stream |
| follower_mgmt.glp | PROGRAM_ERROR | Mode mismatch in recursive call |
| group_formation.glp | PROGRAM_ERROR | Non-contiguous clauses for `social_graph/3` |
| group_messaging.glp | SRSW_VIOLATION | Readers occur 2 times without ground guard |
| interlaced_streams.glp | PROGRAM_ERROR | Cannot redefine predefined type: Stream |
| play_child_safe.glp | PROGRAM_ERROR | Uncovered `[]`; mode mismatch |
| play_dm.glp | PROGRAM_ERROR | Uncovered `[]`; mode mismatch |
| play_feed.glp | PROGRAM_ERROR | Uncovered `[]`; mode mismatch |
| play_group_interlaced.glp | PROGRAM_ERROR | Type mismatch: Stream vs TipList |
| play_group_manager.glp | PROGRAM_ERROR | Uncovered `[]`; mode mismatch |
| replicate.glp | SRSW_VIOLATION | Variable `Xs` has no reader |

### constitutional_consensus (6 failures)

| File | Classification | Main Error |
|------|----------------|------------|
| consensus.glp | SRSW_VIOLATION | Writer `Wave` occurs 2 times; readers without ground guard |
| play_agents.glp | SRSW_VIOLATION | Writers occur 2 times; readers without ground guard |
| play_high_throughput.glp | SRSW_VIOLATION | Readers occur 2-4 times without ground guard |
| play_low_throughput.glp | SRSW_VIOLATION | Readers occur 2-5 times without ground guard |
| test_blocklace.glp | SRSW_VIOLATION | Writers occur 2 times; readers occur 3-6 times |
| test_waves.glp | SRSW_VIOLATION | Variables have no reader |

### cryptocurrencies (6 failures)

| File | Classification | Main Error |
|------|----------------|------------|
| gc.glp | SRSW_VIOLATION | SRSW violations + undefined procedures |
| play_mutual_credit.glp | SRSW_VIOLATION | Readers occur 2 times without ground guard |
| play_payment.glp | SRSW_VIOLATION | Readers occur 2-3 times without ground guard |
| play_redemption.glp | PROGRAM_ERROR | Uncovered alternative `[]` |
| test_balance.glp | PROGRAM_ERROR | Mode mismatch in `=` assertions |
| test_repayments.glp | PROGRAM_ERROR | Mode mismatch in `=` assertions |

---

## Common Error Patterns

### SRSW Violations (37 files)

The most common SRSW violation patterns are:

1. **Writer variable occurs multiple times** - The same writer variable appears in multiple positions (head + body, or multiple body positions)
2. **Reader occurs multiple times without ground guard** - Using a reader variable multiple times without first establishing it's ground via `ground(X?)` guard
3. **Variable has no writer** - A reader variable is used but there's no corresponding writer
4. **Variable has no reader** - A writer variable is bound but never read (guard occurrences don't count unless grounded)

### Program Errors (31 files)

1. **Undefined procedure `#/2`** (6 files) - Module call syntax not supported
2. **Cannot redefine predefined type: Stream** (3 files) - Files define their own `Stream` type
3. **Uncovered type alternatives** (10 files) - Type declarations missing `[]` or other alternatives
4. **Mode mismatch** (12 files) - Reader used where writer expected or vice versa
5. **Non-contiguous clauses** (2 files) - Clauses for same predicate separated by other predicates
6. **Invalid type syntax** (1 file) - Using `--` instead of `\` for difference lists

---

## Recommendations

### For SRSW Violations
These programs need structural changes to comply with single-reader/single-writer semantics. Options:
- Add `ground()` guards where readers are used multiple times
- Restructure to use intermediate variables
- Some patterns may need MWM (multiway merge) for legitimate multi-reader scenarios

### For Program Errors
- **`#/2` errors**: Await module system implementation, or refactor to avoid cross-module calls
- **Stream redefinition**: Remove local `Stream ::=` definitions (use prelude's)
- **Uncovered alternatives**: Add missing `[]` cases to type definitions
- **Mode mismatches**: Correct variable modes (writer vs reader positions)
- **Non-contiguous clauses**: Reorganize source to group clauses
