# A run of the cascade on GRV

- Contract: `GRV` (`grv` to the checker), its owner's two files `programs/examples/rendezvous/GRV.txt` and `programs/examples/rendezvous/GRV.glp`, copied here as they stood at `596ab265`.
- The paper: `git@github.com:EShapiro2/GLP-Networking-API.git` at `3a3a4a7f`, for the paper's own record.
- An instance of gnw, programs/gsc/GnW.glp, under [=(ward, service)]: its term formed by programs/gsc's instantiate/4 from the base's, its file holding only what it adds or replaces (GSC, Definition Parametric contract).
- The tools: this repository at `596ab265` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-11 23:40 UTC by `cascade.sh grv`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GRV.txt` | the contract |
| `GRV.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [service], [guardianship(service, guardian_of, guarded_by)], [relay(ward_acquaint, ward(1, service, no_guardian), free(2)), relay(wards_acquaint, ward(1, service, no_guardian), ward(2, service, no_guardian))], [identity, person])` |
| `vmat.tex` | the VMAT form, 22 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(grv)
