# A run of the cascade on GC

- Contract: `GC` (`gc` to the checker), its owner's two files `programs/currencies/coins/GC.txt` and `programs/currencies/coins/GC.glp`, copied here as they stood at `50c591c0`.
- The paper: `/Users/udi/Grassroots/GC (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/currencies/coins/GC.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/currencies/coins/GC.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-16 16:47 UTC by `cascade.sh gc`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `50c591c0`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `GC.txt` | `programs/currencies/coins/GC.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `GC.glp` | `programs/currencies/coins/GC.txt` | calculus/tools/adopt.py, copied from `programs/currencies/coins/GC.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `GC.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `GC.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `GC.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `GC.glp` | programs/gsc, compile_contract/2 | the VMAT form, 11 displays, one per schema |
| `platform.vglp` | `GC.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gc)
