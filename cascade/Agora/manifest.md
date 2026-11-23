# A run of the cascade on Agora

- Contract: `Agora` (`agora` to the checker), its owner's two files `programs/currencies/agora/Agora.txt` and `programs/currencies/agora/Agora.glp`, copied here as they stood at `849004b4`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/currencies/agora/Agora.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/currencies/agora/Agora.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-23 14:47 UTC by `cascade.sh agora`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `849004b4`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Agora.txt` | `programs/currencies/agora/Agora.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Agora.glp` | `programs/currencies/agora/Agora.txt` | calculus/tools/adopt.py, copied from `programs/currencies/agora/Agora.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Agora.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Agora.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Agora.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Agora.glp` | programs/gsc, compile_contract/2 | the VMAT form, 38 displays, one per schema |
| `platform.vglp` | `Agora.glp` read by programs/gsc's as_read/2, and `Agora.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for agora |

What the tools refused or lacked:
- run: vGLP keeps no plays for agora
