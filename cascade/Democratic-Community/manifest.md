# A run of the cascade on Democratic-Community

- Contract: `Democratic-Community` (`democratic_community` to the checker), its owner's two files `programs/go/Democratic-Community.txt` and `programs/go/Democratic-Community.glp`, copied here as they stood at `fddffe82`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/go/Democratic-Community.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/go/Democratic-Community.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-25 20:10 UTC by `cascade.sh democratic_community`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `fddffe82`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Democratic-Community.txt` | `programs/go/Democratic-Community.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Democratic-Community.glp` | `programs/go/Democratic-Community.txt` | calculus/tools/adopt.py, copied from `programs/go/Democratic-Community.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Democratic-Community.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Democratic-Community.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Democratic-Community.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [organisation], [guardianship(organisation, officer, has_officer)], [relay(organisation_acquaint, ward(1, organisation, no_guardian), free(2)), relay(organisations_acquaint, ward(1, organisation, no_guardian), ward(2, organisation, no_guardian))], [identity, person])` |
| `vmat.tex` | `Democratic-Community.glp` | programs/gsc, compile_contract/2 | the VMAT form, 156 displays, one per schema |
| `platform.vglp` | `Democratic-Community.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled, having no pragmas(democratic_community)
