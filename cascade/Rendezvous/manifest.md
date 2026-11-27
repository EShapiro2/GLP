# A run of the cascade on Rendezvous

- Contract: `Rendezvous` (`rendezvous` to the checker), its owner's two files `programs/examples/rendezvous/Rendezvous.txt` and `programs/examples/rendezvous/Rendezvous.glp`, copied here as they stood at `8614fea8`.
- The paper: `/Users/udi/Grassroots/GNA (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/examples/rendezvous/Rendezvous.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/examples/rendezvous/Rendezvous.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-26 09:58 UTC by `cascade.sh rendezvous`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `8614fea8`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Rendezvous.txt` | `programs/examples/rendezvous/Rendezvous.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Rendezvous.glp` | `programs/examples/rendezvous/Rendezvous.txt` | calculus/tools/adopt.py, copied from `programs/examples/rendezvous/Rendezvous.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Rendezvous.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Rendezvous.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Rendezvous.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [ward], [guardianship(ward, guardian, has_guardian)], [relay(ward_acquaint, ward(1, ward, no_guardian), free(2)), relay(wards_acquaint, ward(1, ward, no_guardian), ward(2, ward, no_guardian))], [identity, person])` |
| `vmat.tex` | `Rendezvous.glp` | programs/gsc, compile_contract/2 | the VMAT form, 25 displays, one per schema |
| `platform.vglp` | `Rendezvous.glp` read by programs/gsc's as_read/2, and `Rendezvous.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for rendezvous |

What the tools refused or lacked:
- run: vGLP keeps no plays for rendezvous
