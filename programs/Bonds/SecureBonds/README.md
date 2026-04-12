# Secure Bonds — Architecture

## Entities

- **Sovereign** of currency p: maintains the authoritative log of all p-coin transactions
- **State custodian** of p: a friend of p who mirrors p's log for recovery
- **Trader** in p-coins: any friend of p who holds/transacts p-coins

## Streams

Each sovereign p maintains:
- **Sovereign stream**: `[block(TxId, TxRecord, Tips) | ...]`
  - Written by p, read by custodians
  - Tips = list of latest observed custodian ack blocks (explicit DAG)
- **Custodian ack streams** (one per custodian): `[ack(TxId, SovBlockRef) | ...]`
  - Written by custodian, read by sovereign
  - SovBlockRef = reference to the sovereign block being acknowledged

## Transaction flow (pay q→r in p-coins)

1. q sends pay request to sovereign p (friend channel)
2. p checks log: does q hold the coins?
3. p writes `block(N, tx_pay(q,r,coins), Tips)` to sovereign stream
4. Custodian reads block, writes `ack(N, ...)` to ack stream
5. p reads ack — transaction is final
6. p notifies q (remove coins) and r (add coins) via friend channels

## Holdings

- Sovereign's log is authoritative: current holder of each p-coin is derived from the log
- Traders maintain local cache, updated upon finality notification from sovereign
- On discrepancy, sovereign's log wins

## Finality

- **Sovereign-final**: block written to sovereign stream
- **Custodian-final**: sovereign observes custodian ack — recorded as tip in next block
- Sovereign notifies payer/payee only after custodian-final

## Recovery

- Sovereign loses log → recovers from any custodian's copy
- Trader loses local cache → requests balance from sovereign

## Minimal constraint

All parties (payer, payee, sovereign) must be friends.
