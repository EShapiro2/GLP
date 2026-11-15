# A run of the cascade on GRA-SS

- Contract: `GRA-SS` (`gra_ss` to the checker), its owner's two files `programs/lbm/GRA-SS.txt` and `programs/lbm/GRA-SS.glp`, copied here as they stood at `04f9bdcc`.
- The paper: `/Users/udi/Grassroots/GRH (not on this machine)` at `unknown`, for the paper's own record.
- Parametric: checked as written, with no realisation, so no platform is compiled (GSC, Definition Parametric contract).
- Adopted from the Calculus: `programs/lbm/GRA-SS.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/lbm/GRA-SS.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-15 18:01 UTC by `cascade.sh gra_ss`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `04f9bdcc`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `GRA-SS.txt` | `programs/lbm/GRA-SS.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `GRA-SS.glp` | `programs/lbm/GRA-SS.txt` | calculus/tools/adopt.py, copied from `programs/lbm/GRA-SS.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `GRA-SS.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `GRA-SS.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `GRA-SS.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `GRA-SS.glp` | programs/gsc, compile_contract/2 | the VMAT form, 18 displays, one per schema |
| (no platform) | | | a parametric contract, with no realisation |

What the tools refused or lacked:
- platform: a parametric contract has no realisation (GSC, Definition Parametric contract); its instances are compiled
