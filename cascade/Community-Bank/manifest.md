# A run of the cascade on Community-Bank

- Contract: `Community-Bank` (`community_bank` to the checker), its owner's two files `programs/go/Community-Bank.txt` and `programs/go/Community-Bank.glp`, copied here as they stood at `758c3693`.
- The paper: `/Users/udi/Grassroots/GO (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/go/Community-Bank.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/go/Community-Bank.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-18 08:46 UTC by `cascade.sh community_bank`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `758c3693`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Community-Bank.txt` | `programs/go/Community-Bank.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Community-Bank.glp` | `programs/go/Community-Bank.txt` | calculus/tools/adopt.py, copied from `programs/go/Community-Bank.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Community-Bank.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Community-Bank.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Community-Bank.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation], [guardianship(organisation, officer, has_officer)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian))], [identity, person])` |
| `vmat.tex` | `Community-Bank.glp` | programs/gsc, compile_contract/2 | the VMAT form, 160 displays, one per schema |
| `platform.vglp` | `Community-Bank.glp` read by programs/gsc's as_read/2, and `Community-Bank.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for community_bank |

What the tools refused or lacked:
- run: vGLP keeps no plays for community_bank
