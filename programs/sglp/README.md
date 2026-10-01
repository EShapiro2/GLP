# sglp

sGLP's directory (Coordination Appendix B; renamed from svglp 2026-10-01): the sGLP simulations' programs, their kinds and their run declarations.  The specification is sGLP's paper, the repository svGLP-Stochastic-Volitional-GLP (the full listings at 8ddff2e).

- `social_graph/` --- the Grassroots Social Graph (the paper's Section 5): `graph.vglp`, the program, its kinds and its run; `run.sh`, the harness that runs it through the REPL and writes its log and friendship graph; `goal.awk`, the initial goal; `friendship.awk`, the friendship graph from the log.
- `test_sglp.sh` --- the checks, run by the suite's Section SGLP.
