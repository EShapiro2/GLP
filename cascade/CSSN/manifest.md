# A run of the cascade on CSSN

- Contract: `CSSN` (`cssn` to the checker), its owner's two files `programs/cssn/CSSN.txt` and `programs/cssn/CSSN.glp`, copied here as they stood at `7a5958b2`.
- The paper: `git@github.com:EShapiro2/SGLP.git` at `8e8bce33`, for the paper's own record.
- The tools: this repository at `7a5958b2` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/cssn (the plays with scripted persons).
- Run on 2026-10-25 12:53 UTC by `cascade.sh cssn`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `CSSN.txt` | the contract |
| `CSSN.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(meet, [child], [guardianship(child, identity, creator, created, parenting)], [relay(child_meet, ward(1, child, 3), free(2)), relay(child_meet_2, ward(1, child, 3), ward(2, child, 4))], [retired])` |
| `vmat.tex` | the VMAT form, 28 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| `run.log` | the plays' logs: play_village |
