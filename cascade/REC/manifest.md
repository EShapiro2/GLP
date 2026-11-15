# A run of the cascade on REC

- Contract: `REC` (`rec` to the checker), its owner's two files `programs/gce/rec/REC.txt` and `programs/gce/rec/REC.glp`, copied here as they stood at `5139336a`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gce/rec/REC.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gce/rec/REC.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-15 20:11 UTC by `cascade.sh rec`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `5139336a`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `REC.txt` | `programs/gce/rec/REC.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `REC.glp` | `programs/gce/rec/REC.txt` | calculus/tools/adopt.py, copied from `programs/gce/rec/REC.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `REC.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `REC.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `REC.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `REC.glp` | programs/gsc, compile_contract/2 | the VMAT form, 6 displays, one per schema |
| `platform.vglp` | `REC.glp` read by programs/gsc's as_read/2, and `REC.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for rec |

What the tools refused or lacked:
- run: vGLP keeps no plays for rec
