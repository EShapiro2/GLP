# A run of the cascade on GFF-PS

- Contract: `GFF-PS` (`gff_ps` to the checker), its owner's two files `programs/gff/GFF-PS.txt` and `programs/gff/GFF-PS.glp`, copied here as they stood at `d2f91a57`.
- The paper: `/Users/udi/Grassroots/GFF (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gff/GFF-PS.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gff/GFF-PS.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-16 18:06 UTC by `cascade.sh gff_ps`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `d2f91a57`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `GFF-PS.txt` | `programs/gff/GFF-PS.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `GFF-PS.glp` | `programs/gff/GFF-PS.txt` | calculus/tools/adopt.py, copied from `programs/gff/GFF-PS.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `GFF-PS.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `GFF-PS.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `GFF-PS.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `GFF-PS.glp` | programs/gsc, compile_contract/2 | the VMAT form, 16 displays, one per schema |
| `platform.vglp` | `GFF-PS.glp` read by programs/gsc's as_read/2, and `GFF-PS.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for gff_ps |

What the tools refused or lacked:
- run: vGLP keeps no plays for gff_ps
