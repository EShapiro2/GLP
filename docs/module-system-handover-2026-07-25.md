# Handover — TGLP module-system: module-as-value done, co-load decision open (2026-07-25)

**From:** TGLP code session.
**Owner:** TGLP — module-system implementation + `modules.tex`.
**GLP repo HEAD at handover:** `b29c7ffd` (verify on disk; other sessions are active).
**Supersedes** the earlier co-load handover and report, whose assemble-and-link recommendation is reversed below: `/Grassroots/docs/glp-module-coload-crossmodule-2026-07-23.md` (now removed) and `TGLP/docs/coload-crossmodule-handover-2026-07-23.md` (already gone before this handover).

Verify all of this on disk before acting — the GLP tree has moved under several concurrent sessions (IGLP engine work, GrassApp UI). `bash test/run_all_tests.sh` was 581/581 green at handover.

## Done this session (all committed, on the remote)

The module-as-value thread is complete and green.

1. **Retired dynamic-dispatch residue purged** (commit `6c5afba7`): deleted `glp_runtime/glp/system/serve.glp` and `glp_runtime/glp/system/modules.glp`; dropped the `'_activate'` reserved name from `root_scope.dart`. `_select` / `_select/1` deliberately kept (live export-call dispatch).

2. **Body kernels no longer declared in root `self.glp`** (commits `35af85af`, then `e28b135f`). The runtime provides kernels as the base of every module's scope chain (TGLP appendix "GLP Language Primitives"); a declaration in root `self.glp` would wrongly place them in every application module's scope (appendix "Guards, Body Kernels, and System Predicates": kernels are invoked only by system predicates, not user programs). The arithmetic/math/time/univ kernels were never declared there and resolve from the runtime; the eight that were (`_output`, `_send`, `_sign`, the MWM trio, `_self_module`, `_run`) now follow the same rule. Verified: an application module naming a kernel is rejected at load.

3. **Module-as-value wrappers** (concurrent session, commit `8d5e8584`): IGLP renamed the kernel registrations to `_self_module` (`body_kernels.dart:106`) and `_run` (`engine_v2/module_kernels.dart:32`); root `self.glp` now has the user-facing system predicates `self_module(M?) :- '_self_module'(M).` and `run(G, M) :- '_run'(G?, M?).`. A run/2 export check (`9503a8bb`) errs when the goal's predicate is not exported by the module value; entry-point aliases carry their exporting declaration (`b29c7ffd`). Section RM of the suite tests all of this (RM1–RM3).

4. Udi stripped the now-dead `builtinProcedures` kernel entries and added the `Closed` and `Exp` types to root `self.glp`.

`run`'s goal argument stays untyped (`_`) — the accepted gap per `modules.tex` §Dynamic Activation "Modules as values": typing a goal against a module value is future work (hash of the type automaton, dynamic export/import).

## Open — the co-load static-linking bug (decision needed from Udi)

**Still reproduces on `b29c7ffd`.** Fixture: three files in one directory with a `self.glp` root —

```
self.glp    Coin ::= coin(Integer).
wallet.glp  exported procedure mint(Integer?, Coin).
            mint(N, coin(N?)).
top.glp     imported procedure wallet#mint(Integer?, Coin).
            exported procedure make(Integer?, Coin).
            make(N, C?) :- wallet # mint(N?, C).
```

Co-loading `wallet.glp` then `top.glp` via two separate `GlpEngine.loadSource` calls, then goal `make(7, C)` → `WireFormatException: instruction not in the wire ISA: Distribute`. The **same fixture loaded as a directory program links cleanly** (its `make/2 not found` is only the entry-point rule — root `self.glp` exports nothing).

Root cause: `loadSource` (`glp_engine.dart`) runs the linker only for a self-contained source (`_isSelfContained`, glp_engine.dart:514 — false when the source has an `imported` decl or an `M#p` `RemoteGoal`). A source with `M#p` takes the else-branch `_compiler.compile(source)`, which emits the **retired** `Distribute` instruction (`modules.tex` §Implementation: that path "is retired").

### The fork — one design fact decides the whole fix, and it is Udi's to settle

How does `self_module`'s multi-app case compose apps in one isolate?

- **If apps compose as module values** — each app a directory program (linked; `self_module` yields its value; executed via `run/2`), which is exactly what Section RM shows — then co-loading loose sources that cross-call via `M#p` is vestigial. **Fix: reject such a source at load with a clear error.** `def:program` (`modules.tex`) admits only a self-contained module or a directory-with-`self.glp`; a loose source with an unresolved `M#p` is neither, and the Distribute path is retired, so it must fail at load, not compile to a dead instruction. Small, TGLP-owned (`glp_engine.dart` `loadSource`, reusing `_containsRemoteGoal`), touches no IGLP file. A negative regression test belongs in the suite.

- **If the multi-app case genuinely co-loads loose sources that cross-call** — then instead assemble the co-loaded set into a program and static-link it (new linker entry building `List<DiscoveredModule>` from an in-memory set + a `GlpEngine` method mirroring `loadProgram` + rewiring `agent_runtime.dart` / `isolate_manager.dart`). Larger, and it edits IGLP's actively-changing files.

### Evidence gathered (all points to the first / reject-at-load)

- Every real caller sets `programDir` → directory-program mode (`graph_scenario_test.dart:30`, `isolate_protocol.dart:193`, `main_cssg_mad_modules.dart:308`).
- Boot sources call entry points by plain name via `combinedProgram`, never `M#p` (`main_cssg_mad_modules.dart:302` — "only the madGLP boot source").
- Every real `M#p` program (`currencies`, `cssn`, `spm/cva`, `social/graph`) is a directory-with-`self.glp`, loaded via `loadProgram`, which static-links correctly.
- No real program co-loads loose `M#p` sources; the suite is green despite the bug because nothing exercises it.

This **reverses** the 2026-07-23 handover, which recommended assemble-and-link. Recommendation now: **reject-at-load.**

### Do NOT implement before Udi decides

Ask him, in **plain prose** — never a boxed/closed question (`/Grassroots/docs/claude.md`, Working Protocol) — whether multi-app composes apps as module values (→ reject-at-load) or co-loads loose cross-calling sources (→ assemble-and-link). Then implement to his answer: spec-first, baseline before commit, commit-gate (no new failures), path-limit commits, add a permanent regression test.
