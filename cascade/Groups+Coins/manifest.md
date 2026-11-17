# A run of the cascade on Groups+Coins

- Contract: the composition of Groups Coins (`groups_coins` to the checker), the schemas and the pragmas of the third contract they amend that all keep with those each adds, their union where they share none or all extend it (vGLP, Section 5.4, "The contract's words"; GSC, Section 8), by programs/vglp/cva/compiler/compose.sh; each its owner's two files, copied here as they stood at `bfc24df3`:
  - `Groups`: `programs/gsc/Groups.txt` and `programs/gsc/Groups.glp`; the paper `/Users/udi/Grassroots/GSC (not on this machine)` at `unknown`.
  - `Coins`: `programs/currencies/coins/Coins.txt` and `programs/currencies/coins/Coins.glp`; the paper `/Users/udi/Grassroots/GC (not on this machine)` at `unknown`.
- The tools: this repository at `bfc24df3` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-17 13:53 UTC by `cascade.sh groups coins`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `Groups.txt` | the contract Groups |
| `Groups.glp` | the act schemas of Groups, as terms |
| `Coins.txt` | the contract Coins |
| `Coins.glp` | the act schemas of Coins, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 23 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| (no run) | vGLP keeps no plays for groups_coins |

What the tools refused or lacked:
- run: vGLP keeps no plays for groups_coins
