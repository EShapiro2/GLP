# SecureBonds — Claude Code Handover

## Task

Load and test the SecureBonds GLP code. The code implements secure coins (mint + pay) with sovereign finality using interlaced streams, plus recovery from the custodian's log copy.

## Mandatory Reading Before Any Action

1. `/Users/udi/Grassroots/GLP/CLAUDE.md`
2. `/Users/udi/Grassroots/GLP/docs/typed-glp-manual.md` (v2.11)
3. `/Users/udi/Grassroots/GLP/docs/glp-cheat-sheet.md`

## Files

All in `/Users/udi/Grassroots/GLP/programs/bonds/secure/`:

| File | Purpose |
|------|---------|
| `self.glp` | Shared types (Bond, TxRecord, SovPayload, Finality, AckMsg, SovBlock, ApprovalReq, PendingFinality) |
| `interlace.glp` | Generic interlaced streams: `collect_tips/3`, `interlace/3` |
| `custodian.glp` | Custodian mirror: reads sovereign blocks, produces acks |
| `sovereign.glp` | `tee/3`, `bind_finality/1`, `first_request/4`, `sovereign_loop/5`, `finality_binder/2`, `setup/1`, `setup_with_log/2`, `recover_counter/2` |
| `play_sovereign.glp` | Test plays: `play/0` (basic finality), `play_recover/0` (finality + recovery) |

## How to Test

### Step 1: Load as project directory and run both plays

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e 'programs/bonds/secure/\nplay.\n:quit' | dart run bin/glp_repl.dart
```

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e 'programs/bonds/secure/\nplay_recover.\n:quit' | dart run bin/glp_repl.dart
```

### Step 2: Expected results

Both `play` and `play_recover` should succeed (`→ succeeds`).

**`play`:** Submits mint + pay requests, verifies all three finality variables are bound to `finalized`.

**`play_recover`:** Same as `play`, plus: `setup_with_log` tees the sovereign stream to produce a log copy; after finality, `recover_counter` walks the log copy and returns the last counter; `check_equals` verifies the counter is 2 (two transactions: mint at 1, pay at 2). If any step fails, the play suspends.

### Step 3: If project loading fails

Try loading files individually:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e 'load programs/bonds/secure/self.glp\nload programs/bonds/secure/interlace.glp\nload programs/bonds/secure/custodian.glp\nload programs/bonds/secure/sovereign.glp\nload programs/bonds/secure/play_sovereign.glp\nplay.\nplay_recover.\n:quit' | dart run bin/glp_repl.dart
```

### Step 4: Add tests to run_all_tests.sh

Add both plays to Section J (or wherever the existing SecureBonds tests are). Both should succeed.

### Step 5: Run full test suite

```bash
cd /Users/udi/Grassroots/GLP
bash test/run_all_tests.sh
```

All tests must pass (currently 480/480).

## Recovery Architecture

`setup_with_log` adds one `tee` compared to `setup`:

```
setup_with_log wires:
  first_request  → FirstBlock
  tee            → splits [FirstBlock|SovTail] into CustodianCopy + LogCopy
  custodian      → reads CustodianCopy, produces RawAcks
  tee            → splits RawAcks into TipAcks + FinAcks
  sovereign_loop → reads RestReqs, collects tips from [TipAcks], produces SovTail + RestPendings
  finality_binder → reads FinAcks, binds [FirstPending|RestPendings]
  LogCopy        → returned as output for recovery
```

`recover_counter` walks the log copy stream after it closes (all requests processed) and returns the counter of the last block. Uses `recover_counter_next` as a helper to carry the current last-seen counter.

## Key Design Decisions

- **`setup_with_log` keeps `setup` unchanged** — existing play is unaffected
- **Log copy via `tee`** — same SRSW-safe stream duplication used for acks
- **`recover_counter` works on closed streams** — the log copy closes when sovereign_loop terminates (all requests processed), at which point tee finishes and the log is complete
- **`check_equals` uses `=?=` guard** — guard reader occurrences don't count toward SRSW (manual §3.4)

## What Comes Next (not for this session)

- Recover holdings from log (walk tx_mint/tx_pay entries, compute who holds what)
- Resume sovereign from recovered counter (new setup continuing from recovered state)
- Secure SG recovery code
- Add Redeem, Swap, Appoint transactions
- Multiple custodians
