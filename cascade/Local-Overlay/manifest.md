# A run of the cascade on Local-Overlay

- Contract: `Local-Overlay` (`local_overlay` to the checker), its owner's two files `programs/lbm/Local-Overlay.txt` and `programs/lbm/Local-Overlay.glp`, copied here as they stood at `135294cf`.
- The paper: `/Users/udi/Grassroots/GRH (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/lbm/Local-Overlay.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/lbm/Local-Overlay.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-24 10:00 UTC by `cascade.sh local_overlay`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `135294cf`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Local-Overlay.txt` | `programs/lbm/Local-Overlay.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Local-Overlay.glp` | `programs/lbm/Local-Overlay.txt` | calculus/tools/adopt.py, copied from `programs/lbm/Local-Overlay.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Local-Overlay.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Local-Overlay.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Local-Overlay.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Local-Overlay.glp` | programs/gsc, compile_contract/2 | the VMAT form, 8 displays, one per schema |
| `platform.vglp` | `Local-Overlay.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(local_overlay)
