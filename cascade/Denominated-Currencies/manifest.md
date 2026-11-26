# A run of the cascade on Denominated-Currencies

- Contract: `Denominated-Currencies` (`denominated_currencies` to the checker), its owner's two files `programs/currencies/sovereign/Denominated-Currencies.txt` and `programs/currencies/sovereign/Denominated-Currencies.glp`, copied here as they stood at `10f3fc71`.
- The paper: `/Users/udi/Grassroots/SGC (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/currencies/sovereign/Denominated-Currencies.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/currencies/sovereign/Denominated-Currencies.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-26 05:48 UTC by `cascade.sh denominated_currencies`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `10f3fc71`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Denominated-Currencies.txt` | `programs/currencies/sovereign/Denominated-Currencies.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Denominated-Currencies.glp` | `programs/currencies/sovereign/Denominated-Currencies.txt` | calculus/tools/adopt.py, copied from `programs/currencies/sovereign/Denominated-Currencies.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Denominated-Currencies.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Denominated-Currencies.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Denominated-Currencies.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Denominated-Currencies.glp` | programs/gsc, compile_contract/2 | the VMAT form, 27 displays, one per schema |
| `platform.vglp` | `Denominated-Currencies.glp` read by programs/gsc's as_read/2, and `Denominated-Currencies.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for denominated_currencies |

What the tools refused or lacked:
- run: vGLP keeps no plays for denominated_currencies
