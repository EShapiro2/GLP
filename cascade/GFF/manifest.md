# A run of the cascade on GFF

- Contract: `GFF` (`gff` to the checker), its owner's two files `programs/gff/GFF.txt` and `programs/gff/GFF.glp`, copied here as they stood at `a0f786bb`.
- The paper: `https://github.com/EShapiro2/GFF-Grassroots-Feeds-and-Followers.git` at `3bfbfc21`, for the paper's own record.
- The tools: this repository at `a0f786bb` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/apps/gff (the plays with scripted persons).
- Run on 2026-10-27 21:30 UTC by `cascade.sh gff`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `GFF.txt` | the contract |
| `GFF.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots` |
| `vmat.tex` | the VMAT form, 16 displays, one per schema |
| `platform.vglp` | the canonical CVA platform, as printed |
| `platform.glp` | the platform compiled to GLP, as emitted |
| `interface.glp` | the platform's interface in sGLP's term form, as programs/sglp/cva/interface.sh prints it |
| `chooser.glp` | the app's chooser, `programs/apps/gff/chooser.glp` at `a0f786bb`, which the platform imports, chooser#choose, GFF.txt having an Algorithms section (programs/vglp/cva/compiler/CHOOSER.md); the emit and the run link this copy |
| `run.log` | the plays' logs: play_feeds play_choice |
