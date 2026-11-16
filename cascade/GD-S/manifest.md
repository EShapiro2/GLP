# A run of the cascade on GD-S

- Contract: `GD-S` (`gd_s` to the checker), its owner's two files `programs/ggga/GD-S.txt` and `programs/ggga/GD-S.glp`, copied here as they stood at `21fa32e5`.
- The paper: `/Users/udi/Grassroots/SRC (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/ggga/GD-S.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/ggga/GD-S.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-16 16:40 UTC by `cascade.sh gd_s`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `21fa32e5`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `GD-S.txt` | `programs/ggga/GD-S.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `GD-S.glp` | `programs/ggga/GD-S.txt` | calculus/tools/adopt.py, copied from `programs/ggga/GD-S.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `GD-S.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `GD-S.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `GD-S.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `GD-S.glp` | programs/gsc, compile_contract/2 | the VMAT form, 9 displays, one per schema |
| `platform.vglp` | `GD-S.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gd_s)
