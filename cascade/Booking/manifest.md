# A run of the cascade on Booking

- Contract: `Booking` (`booking` to the checker), its owner's two files `programs/go/Booking.txt` and `programs/go/Booking.glp`, copied here as they stood at `12db33d5`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/go/Booking.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/go/Booking.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-21 06:14 UTC by `cascade.sh booking`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `12db33d5`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Booking.txt` | `programs/go/Booking.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Booking.glp` | `programs/go/Booking.txt` | calculus/tools/adopt.py, copied from `programs/go/Booking.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Booking.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Booking.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Booking.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation], [guardianship(organisation, officer, has_officer)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian))], [identity, person])` |
| `vmat.tex` | `Booking.glp` | programs/gsc, compile_contract/2 | the VMAT form, 30 displays, one per schema |
| `platform.vglp` | `Booking.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled, having no pragmas(booking)
