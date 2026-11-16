# A run of the cascade on GFF-FR

- Contract: `GFF-FR` (`gff_fr` to the checker), its owner's two files `programs/gff/GFF-FR.txt` and `programs/gff/GFF-FR.glp`, copied here as they stood at `f2b17df3`.
- The paper: `/Users/udi/Grassroots/GFF (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gff/GFF-FR.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gff/GFF-FR.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-16 16:36 UTC by `cascade.sh gff_fr`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `f2b17df3`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `GFF-FR.txt` | `programs/gff/GFF-FR.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `GFF-FR.glp` | `programs/gff/GFF-FR.txt` | calculus/tools/adopt.py, copied from `programs/gff/GFF-FR.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `GFF-FR.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `GFF-FR.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `GFF-FR.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `GFF-FR.glp` | programs/gsc, compile_contract/2 | the VMAT form, 16 displays, one per schema |
| `platform.vglp` | `GFF-FR.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(gff_fr)
