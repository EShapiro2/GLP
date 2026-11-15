# A run of the cascade on SGSG

- Contract: `SGSG` (`sgsg` to the checker), its owner's two files `programs/social/spm/SGSG.txt` and `programs/social/spm/SGSG.glp`, copied here as they stood at `10544424`.
- The paper: `/Users/udi/Grassroots/SPM (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/social/spm/SGSG.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/social/spm/SGSG.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-15 17:27 UTC by `cascade.sh sgsg`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `10544424`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `SGSG.txt` | `programs/social/spm/SGSG.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `SGSG.glp` | `programs/social/spm/SGSG.txt` | calculus/tools/adopt.py, copied from `programs/social/spm/SGSG.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `SGSG.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `SGSG.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `SGSG.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `SGSG.glp` | programs/gsc, compile_contract/2 | the VMAT form, 16 displays, one per schema |
| `platform.vglp` | `SGSG.glp` read by programs/gsc's as_read/2, and `SGSG.txt`'s Interface pragmas | programs/vglp/cva/compiler, compile/5 and pragmas.sh | the canonical CVA platform, as printed |
| `platform.glp` | `platform.vglp` | glp_runtime's :emit | the platform compiled to GLP, as emitted |
| `interface.glp` | `platform.vglp` and `platform.glp` | programs/sglp/cva/interface.sh | the platform's interface in sGLP's term form |
| (no run) | | | vGLP keeps no plays for sgsg |

What the tools refused or lacked:
- run: vGLP keeps no plays for sgsg
