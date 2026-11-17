# A run of the cascade on Bonds

- Contract: `Bonds` (`bonds` to the checker), its owner's two files `programs/currencies/bonds/Bonds.txt` and `programs/currencies/bonds/Bonds.glp`, copied here as they stood at `661edf76`.
- The paper: `/Users/udi/Grassroots/GB (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/currencies/bonds/Bonds.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/currencies/bonds/Bonds.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-17 23:30 UTC by `cascade.sh bonds`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `661edf76`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Bonds.txt` | `programs/currencies/bonds/Bonds.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Bonds.glp` | `programs/currencies/bonds/Bonds.txt` | calculus/tools/adopt.py, copied from `programs/currencies/bonds/Bonds.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Bonds.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Bonds.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Bonds.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Bonds.glp` | programs/gsc, compile_contract/2 | the VMAT form, 21 displays, one per schema |
| `platform.vglp` | `Bonds.glp` read by programs/gsc's as_read/2, and `Bonds.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for bonds |

What the tools refused or lacked:
- run: vGLP keeps no plays for bonds
