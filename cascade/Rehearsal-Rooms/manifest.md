# A run of the cascade on Rehearsal-Rooms

- Contract: `Rehearsal-Rooms` (`rehearsal_rooms` to the checker), its owner's two files `programs/gm/Rehearsal-Rooms.txt` and `programs/gm/Rehearsal-Rooms.glp`, copied here as they stood at `8614fea8`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gm/Rehearsal-Rooms.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gm/Rehearsal-Rooms.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-26 09:12 UTC by `cascade.sh rehearsal_rooms`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `8614fea8`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Rehearsal-Rooms.txt` | `programs/gm/Rehearsal-Rooms.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Rehearsal-Rooms.glp` | `programs/gm/Rehearsal-Rooms.txt` | calculus/tools/adopt.py, copied from `programs/gm/Rehearsal-Rooms.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Rehearsal-Rooms.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Rehearsal-Rooms.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Rehearsal-Rooms.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation, ward], [guardianship(organisation, officer, has_officer), guardianship(ward, guardian, has_guardian)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian)), relay(organisation_ward_acquaint, ward(1, organisation, no_guardian), ward(2, ward, no_guardian)), relay(ward_acquaint, ward(1, ward, no_guardian), free(2)), relay(wards_acquaint, ward(1, ward, no_guardian), ward(2, ward, no_guardian))], [identity, person])` |
| `vmat.tex` | `Rehearsal-Rooms.glp` | programs/gsc, compile_contract/2 | the VMAT form, 58 displays, one per schema |
| `platform.vglp` | `Rehearsal-Rooms.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled, having no pragmas(rehearsal_rooms)
