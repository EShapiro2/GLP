# A run of the cascade on Corporation

- Contract: `Corporation` (`corporation` to the checker), its owner's two files `programs/go/Corporation.txt` and `programs/go/Corporation.glp`, copied here as they stood at `d0e167f5`.
- The paper: `/Users/udi/Grassroots/GO (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/go/Corporation.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/go/Corporation.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-21 03:15 UTC by `cascade.sh corporation`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `d0e167f5`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Corporation.txt` | `programs/go/Corporation.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Corporation.glp` | `programs/go/Corporation.txt` | calculus/tools/adopt.py, copied from `programs/go/Corporation.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Corporation.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Corporation.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Corporation.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation], [guardianship(organisation, officer, has_officer)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian))], [identity, person])` |
| `vmat.tex` | `Corporation.glp` | programs/gsc, compile_contract/2 | the VMAT form, 157 displays, one per schema |
| `platform.vglp` | `Corporation.glp` read by programs/gsc's as_read/2, and `Corporation.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for corporation |

What the tools refused or lacked:
- run: vGLP keeps no plays for corporation
