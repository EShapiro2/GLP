# Secure Bonds — Architecture

## Overview

Sovereign transaction log with custodian finality, using interlaced streams
as the core data structure.  The sovereign of currency p maintains an
append-only stream of transaction blocks interlaced with custodian ack streams,
forming a blocklace DAG.

## Entities

- **Sovereign** of currency p: maintains the authoritative log of all p-coin transactions
- **State custodian** of p: friend of p who mirrors p's log for recovery
- **Trader** in p-coins: friend of p who holds/transacts p-coins

## Data Flow

```
Approval Requests ──→ first_request ──→ FirstBlock (empty tips)
                      sovereign_loop ──→ SovTail (blocks with tips)
                                              │
                            ┌─────────────────┘
                            ↓
                 [FirstBlock|SovTail] = SovStream
                            │
                            ↓
                        custodian ──→ RawAcks
                                        │
                                        ↓
                                       tee
                                      /   \
                               TipAcks     FinAcks
                                  │            │
                                  ↓            ↓
                          sovereign_loop   finality_binder
                          (collect_tips)   (binds reply vars)
```

Five concurrent processes:
1. **first_request** — produces first block with empty tips (bootstraps cycle)
2. **sovereign_loop** — produces subsequent blocks with tips via collect_tips
3. **custodian** — reads sovereign stream, produces acks
4. **tee** — splits ack stream into two copies (SRSW compliance)
5. **finality_binder** — reads acks, binds pending finality variables

## Files

- `self.glp` — shared types (Bond, TxRecord, SovPayload, Finality, AckMsg, SovBlock, etc.)
- `interlace.glp` — collect_tips + interlace (typed version of paper code)
- `custodian.glp` — custodian mirror
- `sovereign.glp` — tee, first_request, sovereign_loop, finality_binder, setup
- `play_sovereign.glp` — test play (mint + pay + observe finality)

## Pay (one p-coin from q to r)

1. q sends to sovereign p: `approve_pay(q, r, coin, FinalityQ?, FinalityR?)`
2. Sovereign writes `block(tx(N, tx_pay(q, r, coin)), Tips)` to stream
3. Custodian reads block, writes `ack(N)` to ack stream
4. `tee` splits ack: one copy feeds back to sovereign (tips), other to finality binder
5. Finality binder reads `ack(N)`, binds `FinalityQ` and `FinalityR` to `finalized`
6. q observes `FinalityQ?` — removes coin from local cache
7. r observes `FinalityR?` — adds coin to local cache

Two reply variables per pay, each with one writer (sovereign) and one reader (trader). SRSW preserved.

## Interlaced Streams and the Blocklace

The sovereign stream and custodian ack streams are interlaced:
each sovereign block carries tips — references to the latest observed
custodian ack blocks. This forms a blocklace DAG (Section 3 of Secure-GLP paper).

The first block has empty tips (no acks exist yet). Subsequent blocks
carry tips collected by `collect_tips`, which non-blockingly reads
the ack stream: if a new ack is available, it's taken as a tip;
if not (unknown tail), the current ack is referenced.

## Recovery

- Sovereign loses log → recovers from any custodian's copy (log is monotonic)
- Trader loses cache → after Replace, asks each friend sovereign: "do I hold any of your coins?"

## Testing

Load as a project in the GLP REPL:

```
GLP> programs/bonds/secure/
GLP> play.
```

Expected: `→ succeeds` (all finality variables bound).
If suspended: a finality variable was not bound (indicates a bug).
