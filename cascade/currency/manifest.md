# A run of the cascade on currency

- Contract: `currency`, its owner's two files `programs/currencies/coins/contract.md` and `programs/currencies/coins/contract.glp`, copied here as they stood.
- The paper: `git@github.com:EShapiro2/GC-Grassroots-Currencies.git` at `d4ce6ccc`, for the paper's own record.
- The tools: this repository at `3f491dae` --- programs/jurix (the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/4), glp_runtime's :emit (the canonical compilation to GLP), programs/vglp/cva/currency (the plays with scripted persons).
- Run on 2026-10-17 20:38 UTC by `cascade.sh currency`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `contract.md` | the clauses |
| `contract.glp` | the act schemas, each in Jurix's notation above its term |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 4 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `run.log` | the plays' logs: play_pay_swap play_redeem play_busy play_declined |
