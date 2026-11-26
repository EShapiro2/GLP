# A run of the cascade on Friendship

- Contract: `Friendship` (`friendship` to the checker), its owner's two files `programs/gsc/Friendship.txt` and `programs/gsc/Friendship.glp`, copied here as they stood at `8614fea8`.
- The paper: `/Users/udi/Grassroots/GSC (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gsc/Friendship.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gsc/Friendship.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-26 09:10 UTC by `cascade.sh friendship`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `8614fea8`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Friendship.txt` | `programs/gsc/Friendship.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Friendship.glp` | `programs/gsc/Friendship.txt` | calculus/tools/adopt.py, copied from `programs/gsc/Friendship.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Friendship.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Friendship.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Friendship.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Friendship.glp` | programs/gsc, compile_contract/2 | the VMAT form, 5 displays, one per schema |
| `platform.vglp` | `Friendship.glp` read by programs/gsc's as_read/2, and `Friendship.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| `run.log` | `platform.vglp` | programs/vglp/cva/social_graph, the plays with scripted persons, in the REPL | the plays' logs: play_refresh play_befriend play_unfriend play_refused play_reacquaint play_menu |
