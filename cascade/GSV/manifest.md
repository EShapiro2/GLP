# A run of the cascade on GSV

- Contract: `GSV` (`gsv` to the checker), its owner's two files `programs/examples/rendezvous/GSV.txt` and `programs/examples/rendezvous/GSV.glp`, copied here as they stood at `c6763c21`.
- The paper: `git@github.com:EShapiro2/GLP-Networking-API.git` at `20f36e74`, for the paper's own record.
- An instance of gnw, programs/gsc/GnW.glp, under [=(ward, service)]: its term formed by programs/gsc's instantiate/4 from the base's, its file holding only what it adds or replaces (GSC, Definition Parametric contract).
- The tools: this repository at `c6763c21` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP).
- Run on 2026-11-13 00:43 UTC by `cascade.sh gsv`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GSV.txt` | the contract |
| `GSV.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [service], [guardianship(service, guardian_of, guarded_by)], [relay(ward_acquaint, ward(1, service, no_guardian), free(2)), relay(wards_acquaint, ward(1, service, no_guardian), ward(2, service, no_guardian))], [identity, person])` |
| `vmat.tex` | the VMAT form, 17 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| (no run) | vGLP keeps no plays for gsv |

What the tools refused or lacked:
- run: vGLP keeps no plays for gsv
