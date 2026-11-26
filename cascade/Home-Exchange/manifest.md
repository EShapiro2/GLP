# A run of the cascade on Home-Exchange

- Contract: `Home-Exchange` (`home_exchange` to the checker), its owner's two files `programs/gm/Home-Exchange.txt` and `programs/gm/Home-Exchange.glp`, copied here as they stood at `10f3fc71`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gm/Home-Exchange.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gm/Home-Exchange.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-26 05:50 UTC by `cascade.sh home_exchange`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `10f3fc71`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Home-Exchange.txt` | `programs/gm/Home-Exchange.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Home-Exchange.glp` | `programs/gm/Home-Exchange.txt` | calculus/tools/adopt.py, copied from `programs/gm/Home-Exchange.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Home-Exchange.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Home-Exchange.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Home-Exchange.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [ward], [guardianship(ward, guardian, has_guardian)], [relay(ward_acquaint, ward(1, ward, no_guardian), free(2)), relay(wards_acquaint, ward(1, ward, no_guardian), ward(2, ward, no_guardian))], [identity, person])` |
| `vmat.tex` | `Home-Exchange.glp` | programs/gsc, compile_contract/2 | the VMAT form, 31 displays, one per schema |
| `platform.vglp` | `Home-Exchange.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled, having no pragmas(home_exchange)
