# A run of the cascade on GA

- Contract: `GA` (`ga` to the checker), its owner's two files `programs/social/graph/GA.txt` and `programs/social/graph/GA.glp`, copied here as they stood at `9e96e490`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/social/graph/GA.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/social/graph/GA.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-16 16:36 UTC by `cascade.sh ga`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `9e96e490`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `GA.txt` | `programs/social/graph/GA.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `GA.glp` | `programs/social/graph/GA.txt` | calculus/tools/adopt.py, copied from `programs/social/graph/GA.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `GA.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `GA.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `GA.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [ward], [guardianship(ward, guardian, has_guardian)], [relay(ward_acquaint, ward(1, ward, no_guardian), free(2)), relay(wards_acquaint, ward(1, ward, no_guardian), ward(2, ward, no_guardian))], [identity, person])` |
| `vmat.tex` | `GA.glp` | programs/gsc, compile_contract/2 | the VMAT form, 51 displays, one per schema |
| `platform.vglp` | `GA.glp` read by programs/gsc's as_read/2, and `GA.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for ga |

What the tools refused or lacked:
- run: vGLP keeps no plays for ga
