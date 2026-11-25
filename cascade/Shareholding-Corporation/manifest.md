# A run of the cascade on Shareholding-Corporation

- Contract: `Shareholding-Corporation` (`shareholding_corporation` to the checker), its owner's two files `programs/go/Shareholding-Corporation.txt` and `programs/go/Shareholding-Corporation.glp`, copied here as they stood at `2d339620`.
- The paper: `/Users/udi/Grassroots/GO (not on this machine)` at `unknown`, for the paper's own record.
- Parametric: checked as written, with no realisation, so no platform is compiled (GSC, Definition Parametric contract).
- Adopted from the Calculus: `programs/go/Shareholding-Corporation.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/go/Shareholding-Corporation.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-25 00:49 UTC by `cascade.sh shareholding_corporation`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `2d339620`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Shareholding-Corporation.txt` | `programs/go/Shareholding-Corporation.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Shareholding-Corporation.glp` | `programs/go/Shareholding-Corporation.txt` | calculus/tools/adopt.py, copied from `programs/go/Shareholding-Corporation.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Shareholding-Corporation.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Shareholding-Corporation.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Shareholding-Corporation.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation], [guardianship(organisation, officer, has_officer)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian))], [identity, person])` |
| `vmat.tex` | `Shareholding-Corporation.glp` | programs/gsc, compile_contract/2 | the VMAT form, 128 displays, one per schema |
| (no platform) | | | a parametric contract, with no realisation |

What the tools refused or lacked:
- platform: a parametric contract has no realisation (GSC, Definition Parametric contract); its instances are compiled
