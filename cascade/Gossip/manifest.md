# A run of the cascade on Gossip

- Contract: `Gossip` (`gossip` to the checker), its owner's two files `programs/gm/Gossip.txt` and `programs/gm/Gossip.glp`, copied here as they stood at `fddffe82`.
- The paper: ` (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/gm/Gossip.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/gm/Gossip.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-25 20:15 UTC by `cascade.sh gossip`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `fddffe82`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Gossip.txt` | `programs/gm/Gossip.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Gossip.glp` | `programs/gm/Gossip.txt` | calculus/tools/adopt.py, copied from `programs/gm/Gossip.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Gossip.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Gossip.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Gossip.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Gossip.glp` | programs/gsc, compile_contract/2 | the VMAT form, 7 displays, one per schema |
| `platform.vglp` | `Gossip.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled, having no pragmas(gossip)
