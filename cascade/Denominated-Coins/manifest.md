# A run of the cascade on Denominated-Coins

- Contract: `Denominated-Coins` (`denominated_coins` to the checker), its owner's two files `programs/currencies/gdc/Denominated-Coins.txt` and `programs/currencies/gdc/Denominated-Coins.glp`, copied here as they stood at `763dc9f3`.
- The paper: `/Users/udi/Grassroots/GCC (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/currencies/gdc/Denominated-Coins.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/currencies/gdc/Denominated-Coins.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-24 10:23 UTC by `cascade.sh denominated_coins`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `763dc9f3`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Denominated-Coins.txt` | `programs/currencies/gdc/Denominated-Coins.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Denominated-Coins.glp` | `programs/currencies/gdc/Denominated-Coins.txt` | calculus/tools/adopt.py, copied from `programs/currencies/gdc/Denominated-Coins.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Denominated-Coins.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Denominated-Coins.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Denominated-Coins.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Denominated-Coins.glp` | programs/gsc, compile_contract/2 | the VMAT form, 12 displays, one per schema |
| `platform.vglp` | `Denominated-Coins.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(denominated_coins)
