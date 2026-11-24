# A run of the cascade on Children

- Contract: `Children` (`children` to the checker), its owner's two files `programs/cssn/Children.txt` and `programs/cssn/Children.glp`, copied here as they stood at `47ea8cfd`.
- The paper: `/Users/udi/Grassroots/CSSN (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/cssn/Children.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/cssn/Children.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-24 19:58 UTC by `cascade.sh children`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `47ea8cfd`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Children.txt` | `programs/cssn/Children.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Children.glp` | `programs/cssn/Children.txt` | calculus/tools/adopt.py, copied from `programs/cssn/Children.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Children.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Children.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Children.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [child], [guardianship(child, parent, has_parent)], [relay(child_acquaint, ward(1, child, guardian(3)), free(2)), relay(children_acquaint, ward(1, child, guardian(3)), ward(2, child, guardian(4)))], [identity, person])` |
| `vmat.tex` | `Children.glp` | programs/gsc, compile_contract/2 | the VMAT form, 43 displays, one per schema |
| `platform.vglp` | `Children.glp` read by programs/gsc's as_read/2, and `Children.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| `run.log` | `platform.vglp` | programs/cssn, the plays with scripted persons, in the REPL | the plays' logs: play_village play_figure play_resigned |
