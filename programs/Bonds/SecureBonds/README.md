# Secure Bonds — Architecture

## Entities

- **Sovereign** of currency p: maintains the authoritative log of all p-coin transactions
- **State custodian** of p: friend of p who mirrors p's log for recovery
- **Trader** in p-coins: friend of p who holds/transacts p-coins

## Streams

- **Sovereign stream**: `[block(TxId, TxRecord, Tips) | ...]` — written by sovereign, read by custodians. Tips = latest observed custodian ack blocks.
- **Custodian ack streams**: `[ack(TxId) | ...]` — written by custodian, read by sovereign.

## Pay (one p-coin from q to r)

1. q sends to sovereign p: `approve_pay(q, r, coin, ApprovedQ?)`
2. p sends to r: `incoming_pay(q, coin, ApprovedR?)`
3. p checks log: does q hold the coin?
4. p writes `block(N, tx_pay(q, r, coin), Tips)` to sovereign stream
5. Custodian reads block, writes `ack(N)` to ack stream
6. p reads ack, binds `ApprovedQ` and `ApprovedR` to `finalized`
7. q observes `ApprovedQ?` — removes coin from local cache
8. r observes `ApprovedR?` — adds coin to local cache

Two reply variables, each with one writer (sovereign) and one reader. SRSW preserved.

Finality = custodian has acknowledged.

## Holdings

Sovereign's log is authoritative. Traders maintain local cache, updated upon observing finality.

## Recovery

- Sovereign loses log → recovers from any custodian's copy
- Trader loses cache → after Replace, asks each friend: "do I hold any of your coins?" Each sovereign friend checks log and reports.

## Minimal constraint

All parties (payer, payee, sovereign) must be friends.
