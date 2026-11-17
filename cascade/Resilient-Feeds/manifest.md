# A run of the cascade on Resilient-Feeds

- Contract: `Resilient-Feeds` (`resilient_feeds` to the checker), its owner's two files `programs/gff/Resilient-Feeds.txt` and `programs/gff/Resilient-Feeds.glp`, copied here as they stood at `04c6f062`.
- The paper: `/Users/udi/Grassroots/GFF (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gff/Resilient-Feeds.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gff/Resilient-Feeds.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-17 10:19 UTC by `cascade.sh resilient_feeds`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `04c6f062`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Resilient-Feeds.txt` | `programs/gff/Resilient-Feeds.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Resilient-Feeds.glp` | `programs/gff/Resilient-Feeds.txt` | calculus/tools/adopt.py, copied from `programs/gff/Resilient-Feeds.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Resilient-Feeds.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Resilient-Feeds.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Resilient-Feeds.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Resilient-Feeds.glp` | programs/gsc, compile_contract/2 | the VMAT form, 16 displays, one per schema |
| `platform.vglp` | `Resilient-Feeds.glp` read by programs/gsc's as_read/2, and `Resilient-Feeds.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for resilient_feeds |

What the tools refused or lacked:
- run: vGLP keeps no plays for resilient_feeds
