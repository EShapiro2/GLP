# A run of the cascade on Tennis-Courts

- Contract: `Tennis-Courts` (`tennis_courts` to the checker), its owner's two files `programs/gm/Tennis-Courts.txt` and `programs/gm/Tennis-Courts.glp`, copied here as they stood at `8614fea8`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gm/Tennis-Courts.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gm/Tennis-Courts.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-26 09:22 UTC by `cascade.sh tennis_courts`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `8614fea8`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Tennis-Courts.txt` | `programs/gm/Tennis-Courts.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Tennis-Courts.glp` | `programs/gm/Tennis-Courts.txt` | calculus/tools/adopt.py, copied from `programs/gm/Tennis-Courts.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Tennis-Courts.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Tennis-Courts.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Tennis-Courts.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation, ward], [guardianship(organisation, officer, has_officer), guardianship(ward, guardian, has_guardian)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian)), relay(organisation_ward_acquaint, ward(1, organisation, no_guardian), ward(2, ward, no_guardian)), relay(ward_acquaint, ward(1, ward, no_guardian), free(2)), relay(wards_acquaint, ward(1, ward, no_guardian), ward(2, ward, no_guardian))], [identity, person])` |
| `vmat.tex` | `Tennis-Courts.glp` | programs/gsc, compile_contract/2 | the VMAT form, 60 displays, one per schema |
| `platform.vglp` | `Tennis-Courts.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled, having no pragmas(tennis_courts)
