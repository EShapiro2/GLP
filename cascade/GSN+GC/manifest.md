# A run of the cascade on GSN+GC

- Contract: the composition of GSN GC (`gsn_gc` to the checker), the union of their schemas and of their pragmas (vGLP, Section 5.4, "The contract's words"), by programs/vglp/cva/compiler/compose.sh; each its owner's two files, copied here as they stood at `4fa6e144`:
  - `GSN`: `programs/gsc/GSN.txt` and `programs/gsc/GSN.glp`; the paper `https://github.com/EShapiro2/jurix-formal-grassroots-social-contracts` at `7a27266b`.
  - `GC`: `programs/currencies/coins/GC.txt` and `programs/currencies/coins/GC.glp`; the paper `https://github.com/EShapiro2/gc-grassroots-currencies` at `cdc2f241`.
- The tools: this repository at `4fa6e144` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-09 19:34 UTC by `cascade.sh gsn gc`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSN.txt` | the contract GSN |
| `GSN.glp` | the act schemas of GSN, as terms |
| `GC.txt` | the contract GC |
| `GC.glp` | the act schemas of GC, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 18 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| (no run) | vGLP keeps no plays for gsn_gc |

What the tools refused or lacked:
- run: vGLP keeps no plays for gsn_gc
