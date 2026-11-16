# A run of the cascade on GKK

- Contract: `GKK` (`gkk` to the checker), its owner's two files `programs/gsc/GKK.txt` and `programs/gsc/GKK.glp`, copied here as they stood at `4afa2207`.
- The paper: `/Users/udi/Grassroots/GSC (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gsc/GKK.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gsc/GKK.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-16 17:41 UTC by `cascade.sh gkk`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `4afa2207`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `GKK.txt` | `programs/gsc/GKK.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `GKK.glp` | `programs/gsc/GKK.txt` | calculus/tools/adopt.py, copied from `programs/gsc/GKK.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `GKK.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `GKK.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `GKK.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `GKK.glp` | programs/gsc, compile_contract/2 | the VMAT form, 3 displays, one per schema |
| `platform.vglp` | `GKK.glp` read by programs/gsc's as_read/2, and `GKK.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| `run.log` | `platform.vglp` | programs/apps/gkk, the plays with scripted persons, in the REPL | the plays' logs: play_keys |
