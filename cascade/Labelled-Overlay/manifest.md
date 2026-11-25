# A run of the cascade on Labelled-Overlay

- Contract: `Labelled-Overlay` (`labelled_overlay` to the checker), its owner's two files `programs/gff/Labelled-Overlay.txt` and `programs/gff/Labelled-Overlay.glp`, copied here as they stood at `fddffe82`.
- The paper: `/Users/udi/Grassroots/GFF (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gff/Labelled-Overlay.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gff/Labelled-Overlay.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-25 20:26 UTC by `cascade.sh labelled_overlay`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `fddffe82`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Labelled-Overlay.txt` | `programs/gff/Labelled-Overlay.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Labelled-Overlay.glp` | `programs/gff/Labelled-Overlay.txt` | calculus/tools/adopt.py, copied from `programs/gff/Labelled-Overlay.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Labelled-Overlay.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Labelled-Overlay.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Labelled-Overlay.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Labelled-Overlay.glp` | programs/gsc, compile_contract/2 | the VMAT form, 13 displays, one per schema |
| `platform.vglp` | `Labelled-Overlay.glp` read by programs/gsc's as_read/2, and `Labelled-Overlay.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for labelled_overlay |

What the tools refused or lacked:
- run: vGLP keeps no plays for labelled_overlay
