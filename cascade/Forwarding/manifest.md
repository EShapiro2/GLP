# A run of the cascade on Forwarding

- Contract: `Forwarding` (`forwarding` to the checker), its owner's two files `programs/gsc/Forwarding.txt` and `programs/gsc/Forwarding.glp`, copied here as they stood at `df6b63fd`.
- The paper: `/Users/udi/Grassroots/GSC (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gsc/Forwarding.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gsc/Forwarding.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-17 14:27 UTC by `cascade.sh forwarding`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `df6b63fd`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Forwarding.txt` | `programs/gsc/Forwarding.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Forwarding.glp` | `programs/gsc/Forwarding.txt` | calculus/tools/adopt.py, copied from `programs/gsc/Forwarding.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Forwarding.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Forwarding.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Forwarding.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Forwarding.glp` | programs/gsc, compile_contract/2 | the VMAT form, 7 displays, one per schema |
| `platform.vglp` | `Forwarding.glp` read by programs/gsc's as_read/2, and `Forwarding.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for forwarding |

What the tools refused or lacked:
- run: vGLP keeps no plays for forwarding
