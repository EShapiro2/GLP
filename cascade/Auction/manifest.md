# A run of the cascade on Auction

- Contract: `Auction` (`auction` to the checker), its owner's two files `programs/lbm/Auction.txt` and `programs/lbm/Auction.glp`, copied here as they stood at `235bab58`.
- The paper: `/Users/udi/Grassroots/GRH (not on this machine)` at `unknown`, for the paper's own record.
- Adopted from the Calculus: `programs/lbm/Auction.txt` is the contract, its Logic pragmas and its Interface pragmas, and `programs/lbm/Auction.glp` the full schemas as calculus/tools/adopt.py prints them from it (claude.md "Code", "The record of a contract adopted from the Calculus"; calculus/README.md, "Adopting a contract").
- Run on 2026-11-24 07:48 UTC by `cascade.sh auction`; the suite's Section CAS re-runs it and holds these files to what the tools print.
- Every source and every tool at this repository's `235bab58`; each artefact's source and tool:

| Artefact | Source | Tool | What it holds |
|---|---|---|---|
| `Auction.txt` | `programs/lbm/Auction.txt` | copied | the contract, its Logic pragmas and its Interface pragmas |
| `Auction.glp` | `programs/lbm/Auction.txt` | calculus/tools/adopt.py, copied from `programs/lbm/Auction.glp` | the full schemas, the term the checker reads |
| `schemas.txt` | `Auction.txt`'s Logic pragmas | calculus/tools/adopt.py | the speech-act schemas compiled from the Logic pragmas |
| `full-contract.txt` | `Auction.txt` and the contracts it is written from | calculus/tools/adopt.py | the full contract, each clause labelled by its origin |
| `full-pragmas.txt` | the same | calculus/tools/adopt.py | the Logic pragmas' sentences, the relations applied |
| `flat.txt` | the same | calculus/tools/adopt.py | the flat text, the relations applied |
| `full-schemas.txt` | the same | calculus/tools/adopt.py | the schemas, the relations applied |
| `verdict.txt` | `Auction.glp` | programs/gsc, check/2 | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | `Auction.glp` | programs/gsc, compile_contract/2 | the VMAT form, 18 displays, one per schema |
| `platform.vglp` | `Auction.glp` | programs/vglp/cva/compiler, compile/5 | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(auction)
