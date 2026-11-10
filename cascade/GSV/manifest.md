# A run of the cascade on GSV

- Contract: `GSV` (`gsv` to the checker), its owner's two files `programs/examples/rendezvous/GSV.txt` and `programs/examples/rendezvous/GSV.glp`, copied here as they stood at `34f5a5b7`.
- The paper: `git@github.com:EShapiro2/GLP-Networking-API.git` at `f16af8c5`, for the paper's own record.
- The tools: this repository at `34f5a5b7` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-10 20:40 UTC by `cascade.sh gsv`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSV.txt` | the contract |
| `GSV.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [service], [guardianship(service, guardian_of, guarded_by)], [relay(ward_acquaint, ward(1, service, no_guardian), free(2)), relay(wards_acquaint, ward(1, service, no_guardian), ward(2, service, no_guardian))], [identity, person])` |
| `vmat.tex` | the VMAT form, 17 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gsv)
