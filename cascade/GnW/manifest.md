# A run of the cascade on GnW

- Contract: `GnW` (`gnw` to the checker), its owner's two files `programs/gsc/GnW.txt` and `programs/gsc/GnW.glp`, copied here as they stood at `bc84f083`.
- The paper: `git@github.com:EShapiro2/Jurix-Formal-Grassroots-Social-Contracts.git` at `df5e4542`, for the paper's own record.
- The tools: this repository at `bc84f083` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-10 20:28 UTC by `cascade.sh gnw`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GnW.txt` | the contract |
| `GnW.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [ward], [guardianship(ward, guardian_of, guarded_by)], [relay(ward_acquaint, ward(1, ward, no_guardian), free(2)), relay(wards_acquaint, ward(1, ward, no_guardian), ward(2, ward, no_guardian))], [identity, person])` |
| `vmat.tex` | the VMAT form, 17 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gnw)
