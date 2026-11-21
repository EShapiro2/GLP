# A run of the cascade on Local-Overlay

- Contract: `Local-Overlay` (`local_overlay` to the checker), its owner's two files `programs/lbm/Local-Overlay.txt` and `programs/lbm/Local-Overlay.glp`, copied here as they stood at `a6fc2475`.
- The paper: `/Users/udi/Grassroots/GRH (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/lbm/Local-Overlay.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/lbm/Local-Overlay.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-21 15:02 UTC by `cascade.sh local_overlay`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `a6fc2475`; each artefact's source and tool:

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
| `platform.vglp` | `Local-Overlay.glp` read by programs/gsc's as_read/2, and `Local-Overlay.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for local_overlay |

What the tools refused or lacked:
- run: vGLP keeps no plays for local_overlay
