# A run of the cascade on GRA-SS

- Contract: `GRA-SS` (`gra_ss` to the checker), its owner's two files `programs/lbm/GRA-SS.txt` and `programs/lbm/GRA-SS.glp`, copied here as they stood at `555c0ddb`.
- The paper: `/Users/udi/Grassroots/GRH (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/lbm/GRA-SS.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/lbm/GRA-SS.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-15 21:29 UTC by `cascade.sh gra_ss`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `555c0ddb`; each artefact's source and tool:

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
| `platform.vglp` | `GRA-SS.glp` read by programs/gsc's as_read/2, and `GRA-SS.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for gra_ss |

What the tools refused or lacked:
- run: vGLP keeps no plays for gra_ss
