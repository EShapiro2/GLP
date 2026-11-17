# A run of the cascade on Warded-Groups

- Contract: `Warded-Groups` (`warded_groups` to the checker), its owner's two files `programs/cssn/Warded-Groups.txt` and `programs/cssn/Warded-Groups.glp`, copied here as they stood at `2ba1f742`.
- The paper: `/Users/udi/Grassroots/CSSN (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/cssn/Warded-Groups.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/cssn/Warded-Groups.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-17 10:28 UTC by `cascade.sh warded_groups`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `2ba1f742`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Warded-Groups.txt` | `programs/cssn/Warded-Groups.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Warded-Groups.glp` | `programs/cssn/Warded-Groups.txt` | calculus/tools/adopt.py, copied from `programs/cssn/Warded-Groups.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Warded-Groups.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Warded-Groups.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Warded-Groups.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [ward], [guardianship(ward, guardian, has_guardian)], [relay(ward_acquaint, ward(1, ward, no_guardian), free(2)), relay(wards_acquaint, ward(1, ward, no_guardian), ward(2, ward, no_guardian))], [identity, person])` |
| `vmat.tex` | `Warded-Groups.glp` | programs/gsc, compile_contract/2 | the VMAT form, 42 displays, one per schema |
| `platform.vglp` | `Warded-Groups.glp` read by programs/gsc's as_read/2, and `Warded-Groups.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for warded_groups |

What the tools refused or lacked:
- run: vGLP keeps no plays for warded_groups
