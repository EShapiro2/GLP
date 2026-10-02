# sglp

sGLP's directory (Coordination Appendix B; renamed from svglp 2026-10-01): sGLP in GLP, as the stochastic pi-calculus was in FCP, written from sGLP's paper (the repository svGLP-Stochastic-Volitional-GLP at d2f64b6, Section 4) with nothing in the engine, the compiler or the checker.  The simulations written for sGLP's engine extension were removed with it on 2026-10-02 (git holds them, before that date).

A program rooted here: load `programs/sglp` and call its entry point.

- `self.glp` --- the directory: exposes `monitor.glp` and `person.glp` to the subtree; the horizon's units (`span/2`); the entry point `social_graph(Agents, Count, Unit, Seed)`.
- `monitor.glp` --- the monitor: the clock, the queue of pending requests, the release over the guard `when_idle`.
- `person.glp` --- the person process of each agent, and `construct/3`, the construct process of a compiled program in a simulation.
- `social_graph/` --- the Grassroots Social Graph in the translated form, by hand: `graph.glp` (the compiled program with the person channel, the four profiles translated, `one_of`, `length`, `nth`), `harness.glp` (the run), `run.sh` (a run through the REPL, its log and its friendship graph), `friendship.awk`.
- `tests/` and `test_sglp.sh` --- tests (i) to (iv) of the code task of 2026-10-02 00:06 UTC, run by the suite's Section SGLP.
