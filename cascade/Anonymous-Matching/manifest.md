# A run of the cascade on Anonymous-Matching

- Contract: `Anonymous-Matching` (`anonymous_matching` to the checker), its owner's two files `programs/gm/Anonymous-Matching.txt` and `programs/gm/Anonymous-Matching.glp`, copied here as they stood at `3e428e04`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gm/Anonymous-Matching.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gm/Anonymous-Matching.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-22 01:03 UTC by `cascade.sh anonymous_matching`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `3e428e04`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Anonymous-Matching.txt` | `programs/gm/Anonymous-Matching.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Anonymous-Matching.glp` | `programs/gm/Anonymous-Matching.txt` | calculus/tools/adopt.py, copied from `programs/gm/Anonymous-Matching.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Anonymous-Matching.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Anonymous-Matching.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Anonymous-Matching.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [ward], [guardianship(ward, guardian, has_guardian)], [relay(ward_acquaint, ward(1, ward, no_guardian), free(2)), relay(wards_acquaint, ward(1, ward, no_guardian), ward(2, ward, no_guardian))], [identity, person])` |
| `vmat.tex` | `Anonymous-Matching.glp` | programs/gsc, compile_contract/2 | the VMAT form, 31 displays, one per schema |
| `platform.vglp` | `Anonymous-Matching.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled, having no pragmas(anonymous_matching)
