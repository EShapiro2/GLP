# A run of the cascade on Chess

- Contract: `Chess` (`chess` to the checker), its owner's two files `programs/gce/chess/Chess.txt` and `programs/gce/chess/Chess.glp`, copied here as they stood at `8614fea8`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gce/chess/Chess.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gce/chess/Chess.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-26 09:12 UTC by `cascade.sh chess`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `8614fea8`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Chess.txt` | `programs/gce/chess/Chess.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Chess.glp` | `programs/gce/chess/Chess.txt` | calculus/tools/adopt.py, copied from `programs/gce/chess/Chess.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Chess.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Chess.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Chess.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Chess.glp` | programs/gsc, compile_contract/2 | the VMAT form, 14 displays, one per schema |
| `platform.vglp` | `Chess.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(chess)
