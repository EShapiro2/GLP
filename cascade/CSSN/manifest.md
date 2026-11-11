# A run of the cascade on CSSN

- Contract: `CSSN` (`cssn` to the checker), its owner's two files `programs/cssn/CSSN.txt` and `programs/cssn/CSSN.glp`, copied here as they stood at `e5840afb`.
- The paper: `git@github.com:EShapiro2/SGLP.git` at `22f5464c`, for the paper's own record.
- An instance of gsnw, programs/cssn/GSNW.glp, under [=(ward, child), =(guardian_of, parenting), =(make_guardian, make_parent), =(end_guardian, end_parent), =(guardian_unfriend, parent_unfriend), =(ward_acquaint, child_acquaint), =(ward_befriend, child_befriend), =(ward_unfriend, child_unfriend), =(ward_end, child_end), =(wards_acquaint, children_acquaint), =(wards_befriend, children_befriend), =(ward_message, child_message), =(ward_create, child_create), =(ward_join, child_join), =(wards_join, children_join), =(ward_leave, child_leave), =(ward_remove, child_remove), =(ward_withdraw, child_withdraw), =(ward_close, child_close), =(guardian_close, parent_close), =(ward_post_own, child_post_own), =(ward_post, child_post)]: its term formed by programs/gsc's instantiate/4 from the base's, its file holding only what it adds or replaces (GSC, Definition Parametric contract).
- The tools: this repository at `e5840afb` --- programs/gsc (the printer of the schemas' notation, notation_contract/1, the checker, check/2, and the compiler to VMAT, compile_contract/2), programs/vglp/cva/compiler (the compiler to the canonical CVA platform, compile/5, and its reader of the pragmas, pragmas.sh), glp_runtime's :emit (the canonical compilation to GLP), programs/cssn (the plays with scripted persons).
- Run on 2026-11-11 23:42 UTC by `cascade.sh cssn`; the suite's Section CAS re-runs it and holds these files to what the tools print.

| Artefact | What it holds |
|---|---|
| `CSSN.txt` | the contract |
| `CSSN.glp` | the act schemas, as terms |
| `schemas.txt` | the act schemas in GSC's notation, printed from the terms |
| `verdict.txt` | the checker's verdict: `syntactically_grassroots_with_wards(acquaint, [child], [guardianship(child, parenting, guarded_by)], [relay(child_acquaint, ward(1, child, guardian(3)), free(2)), relay(children_acquaint, ward(1, child, guardian(3)), ward(2, child, guardian(4)))], [identity, person])` |
| `vmat.tex` | the VMAT form, 38 displays, one per schema |
| `platform.vglp` | the CVA compiler's refusal, and no platform.glp and no run |

What the tools refused or lacked:
- platform: %% not compiled(cssn)
