# A run of the cascade on Community-Market

- Contract: `Community-Market` (`community_market` to the checker), its owner's two files `programs/go/Community-Market.txt` and `programs/go/Community-Market.glp`, copied here as they stood at `8475b1c6`.
- The paper: `/Users/udi/Grassroots/GO (not on this machine)` at `unknown`, for the paper's own record.
- Parametric: checked as written, with no realisation, so no platform is compiled (GSC, Definition Parametric contract).
- Adopted from the Calculus: `programs/go/Community-Market.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/go/Community-Market.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-24 22:31 UTC by `cascade.sh community_market`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `8475b1c6`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Community-Market.txt` | `programs/go/Community-Market.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Community-Market.glp` | `programs/go/Community-Market.txt` | calculus/tools/adopt.py, copied from `programs/go/Community-Market.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Community-Market.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Community-Market.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Community-Market.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation], [guardianship(organisation, officer, has_officer)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian))], [identity, person])` |
| `vmat.tex` | `Community-Market.glp` | programs/gsc, compile_contract/2 | the VMAT form, 153 displays, one per schema |
| (no platform) | | | a parametric contract, with no realisation |

What the tools refused or lacked:
- platform: a parametric contract has no realisation (GSC, Definition Parametric contract); its instances are compiled
