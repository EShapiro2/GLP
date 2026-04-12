# Grassroots Bonds — GLP Implementation

This repository contains the GLP (Grassroots Logic Programs) implementation of grassroots bonds, as described in:

> Ehud Shapiro. *Grassroots Bonds: A Grassroots Foundation for Market Liquidity.* 2026.

## Files

- **agent.glp** — The bond agent (`agent/6`), extending the social graph agent with holdings and serial numbers. Handles minting, balance, befriending, trade (credit, loan, payment, redemption, sale of debt), and escrow.
- **mediator.glp** — The mediator process that bridges the user interface and the agent.
- **actors.glp** — Test scenarios (`fplay1`–`fplay12`): single-agent minting, two-party trades, multi-party credit chains, redemption, and escrow.
- **boot.glp** — Boot infrastructure for launching agents with mediators and network channels.
- **play12/** — The village market scenario: six agents (Alice, Bob, Charlie, Diana, Eve, Frank) executing a sequence of trades that exercises the full protocol.

## Language

GLP is a multiagent concurrent logic programming language implemented in Dart for smartphone deployment. The GLP runtime is available at <https://github.com/EShapiro2/GLP>.
