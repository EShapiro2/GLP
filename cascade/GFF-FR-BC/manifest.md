# A run of the cascade on GFF-FR-BC

- Contract: `GFF-FR-BC` (`gff_fr_bc` to the checker), its owner's two files `programs/gff/GFF-FR-BC.txt` and `programs/gff/GFF-FR-BC.glp`, copied here as they stood at `07bfb4f2`.
- The paper: `/Users/udi/Grassroots/GFF (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gff/GFF-FR-BC.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gff/GFF-FR-BC.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-16 19:28 UTC by `cascade.sh gff_fr_bc`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `07bfb4f2`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `GFF-FR-BC.txt` | `programs/gff/GFF-FR-BC.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `GFF-FR-BC.glp` | `programs/gff/GFF-FR-BC.txt` | calculus/tools/adopt.py, copied from `programs/gff/GFF-FR-BC.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `GFF-FR-BC.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `GFF-FR-BC.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `GFF-FR-BC.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `GFF-FR-BC.glp` | programs/gsc, compile_contract/2 | the VMAT form, 17 displays, one per schema |
| `platform.vglp` | `GFF-FR-BC.glp` read by programs/gsc's as_read/2, and `GFF-FR-BC.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for gff_fr_bc |

What the tools refused or lacked:
- run: vGLP keeps no plays for gff_fr_bc
