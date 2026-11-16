# A run of the cascade on MCS

- Contract: `MCS` (`mcs` to the checker), its owner's two files `programs/go/MCS.txt` and `programs/go/MCS.glp`, copied here as they stood at `9d3bf8f8`.
- The paper: `/Users/udi/Grassroots/GO (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/go/MCS.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/go/MCS.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-16 08:29 UTC by `cascade.sh mcs`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `9d3bf8f8`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `MCS.txt` | `programs/go/MCS.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `MCS.glp` | `programs/go/MCS.txt` | calculus/tools/adopt.py, copied from `programs/go/MCS.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `MCS.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `MCS.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `MCS.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation], [guardianship(organisation, officer, has_officer)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian))], [identity, person])` |
| `vmat.tex` | `MCS.glp` | programs/gsc, compile_contract/2 | the VMAT form, 147 displays, one per schema |
| `platform.vglp` | `MCS.glp` read by programs/gsc's as_read/2, and `MCS.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for mcs |

What the tools refused or lacked:
- run: vGLP keeps no plays for mcs
