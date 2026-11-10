# A run of the cascade on CSSN

- Contract: `CSSN` (`cssn` to the checker), its owner's two files `programs/cssn/CSSN.txt` and `programs/cssn/CSSN.glp`, copied here as they stood at `9b7f4116`.
- The paper: `/Users/udi/Grassroots/CSSN (not on this machine)` at `unknown`, for the paper's own record.
- The tools: this repository at `9b7f4116` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/cssn (the plays with scripted persons).
- Run on 2026-11-10 20:35 UTC by `cascade.sh cssn`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `CSSN.txt` | the contract |
| `CSSN.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [child], [guardianship(child, parenting, guarded_by)], [relay(ward_acquaint, ward(1, child, guardian(3)), free(2)), relay(wards_acquaint, ward(1, child, guardian(3)), ward(2, child, guardian(4)))], [identity, person])` |
| `vmat.tex` | the VMAT form, 38 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(cssn)
