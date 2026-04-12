# Secure Bonds — GLP Implementation

## Overview

Extends the bond agent (`../agent.glp`) with sovereign transaction logs,
state custodians, and finality via interlaced streams.

## Architecture

### Streams per agent p

1. **Sovereign stream** (if p is a sovereign, i.e., has issued p-coins):
   - Append-only stream of transaction blocks: `[block(TxRecord, CustodianTips) | Rest]`
   - Each block records a p-coin transaction (mint, approve payment, approve redemption, approve swap)
   - Tips point to the latest blocks in each custodian's stream (acknowledgments)

2. **Custodian streams** (for each agent q that p is a state custodian of):
   - p reads q's sovereign stream
   - p creates acknowledgment blocks in its own custodian stream
   - Each acknowledgment block has a tip pointing to the sovereign's block it acknowledges

### Finality

A transaction is **final** when the sovereign's stream contains a block whose tips
include a custodian's acknowledgment of the approval block. The sovereign communicates
finality to payer/payee only after observing this acknowledgment.

### Minimal constraint

All parties to a transaction (payer, payee, sovereign) must be friends.
Extension to diameter-2 payments (via a common friend) is future work.

### Transaction flow (Pay q->r in s-coins)

1. q sends pay request to sovereign s (via friend channel)
2. s verifies q holds the coins, creates approval block in sovereign stream
3. s sends approval block to custodians (via SharedBroadcastStream or direct)
4. Custodian creates acknowledgment block in its stream with tip to approval
5. s observes custodian acknowledgment (tip in next sovereign block)
6. s notifies q and r: transaction is final
7. q removes coins from local holdings, r adds them

### Files

- `sovereign.glp` — sovereign stream management, approval, finality
- `custodian.glp` — custodian stream, acknowledgment
- `secure_agent.glp` — extends bond agent with sovereign/custodian roles
- `boot.glp` — test scenario
