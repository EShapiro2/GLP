# A run of the cascade on TC

- Contract: `TC` (`tc` to the checker), its owner's two files `programs/go/TC.txt` and `programs/go/TC.glp`, copied here as they stood at `d9e4006c`.
- The paper: `/Users/udi/Grassroots/GO (not on this machine)` at `unknown`, for the paper's own record.
- Parametric: checked as written, with no realisation, so no platform is compiled (GSC, Definition Parametric contract).
- Adopted from the Calculus: `programs/go/TC.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/go/TC.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-16 08:22 UTC by `cascade.sh tc`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `d9e4006c`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `TC.txt` | `programs/go/TC.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `TC.glp` | `programs/go/TC.txt` | calculus/tools/adopt.py, copied from `programs/go/TC.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `TC.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `TC.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `TC.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation], [guardianship(organisation, officer, has_officer)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian))], [identity, person])` |
| `vmat.tex` | `TC.glp` | programs/gsc, compile_contract/2 | the VMAT form, 149 displays, one per schema |
| (no platform) | | | a parametric contract, with no realisation |

What the tools refused or lacked:
- platform: a parametric contract has no realisation (GSC, Definition Parametric contract); its instances are compiled
