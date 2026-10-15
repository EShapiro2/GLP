# How to Run GLP Multi-Agent Programs

**Updated: 2026-10-15**

These are the routes by which `test/run_all_tests.sh` runs multi-agent programs, one section per route, each naming the harness section it comes from.  Every command below was run on 2026-10-12 on a tree at GLP `main` `11744d09`, and what it printed is quoted as it printed it.  Commands start at the root of a worktree (Code: the clone at `/Users/udi/Grassroots/GLP`).

What dGLP and madGLP are is IGLP's, not this file's: Section "Deterministic Implementation of cGLP" (`IGLP/sections/dglp.tex`, `subsec:dglp`), Section "Implementing Multiagent GLP with Deterministic Agents" (`IGLP/sections/madglp.tex`, `subsec:madglp`) and Section "madGLP Specification" (`IGLP/sections/madglp-spec.tex`).  How to operate the REPL, and every GLP and Dart rule, is `GLP/CLAUDE.md`.

---

## 1. One heap, from the REPL (dGLP)

Harness Section P, "The fixture's two single-isolate REPL boots load".  The fixture is `programs/tests/agent_roundtrip/`; its four files are loaded, then a boot file, then the goal `play.`.

Without the mediator:

```bash
cd glp_runtime && printf '../programs/tests/agent_roundtrip/typed_social_agent.glp\n../programs/tests/agent_roundtrip/typed_actors.glp\n../programs/tests/agent_roundtrip/typed_ui_mediator.glp\n../programs/tests/agent_roundtrip/typed_ui_actors.glp\n../programs/tests/agent_roundtrip/play_dglp_boot.glp\nplay.\n:quit\n' | bin/glpc
```

Prints `✓ Loaded:` for each of the five files, then `→ succeeds`.

With the mediator and the UI actors:

```bash
cd glp_runtime && printf '../programs/tests/agent_roundtrip/typed_social_agent.glp\n../programs/tests/agent_roundtrip/typed_actors.glp\n../programs/tests/agent_roundtrip/typed_ui_mediator.glp\n../programs/tests/agent_roundtrip/typed_ui_actors.glp\n../programs/tests/agent_roundtrip/play_ui_dglp_boot.glp\nplay.\n:quit\n' | bin/glpc
```

Prints `✓ Loaded:` for each of the five files, then `→ suspended`.

The harness loads the same five files for each boot and checks that the boot loads with no error; it posts no goal.

---

## 2. A program directory loads (Section MA)

Harness Section MA loads, one at a time, each of the twelve directories under `programs/tests/multiagent/`, then `programs/tests/agent_roundtrip/play_madglp/` and `play_ui_madglp/`, and checks for `Loaded program`.  One of them:

```bash
cd glp_runtime && printf '../programs/tests/multiagent/distribute/\n:quit\n' | bin/glpc
```

Prints `[CERTIFICATE REFUSED] distribute reaches the network or the person: ...` and then `✓ Loaded program: ../programs/tests/multiagent/distribute/`.  The `[CERTIFICATE REFUSED]` line names the procedures by which the program reaches the network or the person, and the load goes on; the boots below print it too.

Each directory's boot clause is in `<name>_boot.glp` beside it, outside the directory, and is run by the routes in Sections 4 and 6, not by this one.

---

## 3. One agent in madGLP mode, from the REPL (Section SK)

`:mad <agent>` puts the REPL in madGLP mode as that agent, so the seam predicates run; Section SK loads its probe so.

```bash
cd glp_runtime && printf ':mad alice\n../programs/tests/sign_probe\nroundtrip(hello(world), R).\n:quit\n' | bin/glpc
```

Prints `madGLP mode on: agent alice, key <64 hex digits>`, `✓ Loaded program: ../programs/tests/sign_probe`, then `R = signed(...)` and `→ succeeds`.

---

## 4. One isolate per agent, from the REPL: `:boot` (Sections MB and SL)

`:boot <f>_boot.glp` runs the boot program with one isolate per agent of its boot clause, each loading the program directory `<f>/` beside the boot file; `:boot <f>_boot.glp <dir>` names the directory.  What an agent sends its person is printed as `[agent] term`, and the command returns when the traffic has settled.

```bash
cd glp_runtime && printf ':boot ../programs/tests/mad_held_links_boot.glp\n:quit\n' | bin/glpc
```

Prints `[carol] held(carol, _w(alice, 1), bob)`, `[alice] held(alice, _w(alice, 1), carol)`, `[alice] delivered(hello_from_carol)`, then `✓ Boot settled: 3 agents (alice, bob, carol)`.  This is Section MB's MB1; MB3 and MB4 boot `mad_stream_writer_boot.glp` and `mad_two_crossings_boot.glp` the same way.

With the directory named, as Section MB's MB5 boots the link probes of `programs/tests/linkprobes/`:

```bash
cd glp_runtime && printf ':boot ../programs/tests/linkprobes/linkprobe7_boot.glp ../programs/tests/linkprobes/linkprobe7\n:quit\n' | bin/glpc
```

Prints six `[bob] saw(...)` lines ending `[bob] saw(probe)`, then `[alice] got(hi)` and `✓ Boot settled: 2 agents (alice, bob)`.

---

## 5. A certified mini-app for an agent to load: `:artefact` (Sections MB and SL)

`:artefact <dir> [<to>]` writes the certified artefact of the program directory `<dir>` as `<to>/<name>.glpw`.  An artefact is a build output, ignored by git: the harness writes it before the boot that reads it and removes it after, as below.  Section MB's MB2:

```bash
cd glp_runtime && printf ':artefact ../programs/tests/mad_ship_hello ../programs/tests/mad_ship_module\n:boot ../programs/tests/mad_ship_module_boot.glp\n:quit\n' | bin/glpc
```

Prints `✓ Wrote ../programs/tests/mad_ship_module/mad_ship_hello.glpw --- certified under <64 hex digits>`, then `[bob] ran([done])` and `✓ Boot settled: 2 agents (alice, bob)`.  Then:

```bash
rm -f programs/tests/mad_ship_module/*.glpw
```

Section SL's SL2 boots `mad_load_module_boot.glp` the same way, after `:artefact ../programs/tests/run_module_probe ../programs/tests/mad_load_module`.

---

## 6. One isolate per agent, from Dart (Sections M and O, run in Section Q)

Sections M and O print only that they are folded into Section Q, which runs `dart test` over all of `glp_runtime/test`; these are its multi-agent files, run alone.

```bash
cd glp_runtime && dart test test/multiagent/isolate_manager_test.dart
```

`+3: All tests passed!`: a trivial three-agent boot, `play_madglp_boot.glp` with `play_madglp/`, and `play_ui_madglp_boot.glp` with `play_ui_madglp/`.  One of them by name:

```bash
cd glp_runtime && dart test test/multiagent/isolate_manager_test.dart -n "no UI"
```

`+1: All tests passed!`.

```bash
cd glp_runtime && dart test test/multiagent/multiagent_glp_test.dart
```

`+12: All tests passed!`: the twelve `programs/tests/multiagent/` programs, each by its boot file.

```bash
cd glp_runtime && dart test test/multiagent/
```

Runs every test file of `glp_runtime/test/multiagent/`; none of them is an entry of the harness's `KNOWN_RED`.

---

## 7. The Flutter package `glp_multiagent` (Section Q)

Section Q generates the bundled GLP assets, then runs `flutter test` over all of `glp_multiagent/test`.

```bash
cd glp_multiagent && bash tool/sync_glp_assets.sh
```

Writes `pingapp.glpw`, `currency.glpw` and `denominated.glpw`, and exits 1 with `sync_glp_assets: childsafe.glpw was not written`: CSSN's artefact, a known-red check of Section Q's asset step.

```bash
cd glp_multiagent && flutter test test/roundtrip_isolate_test.dart
```

`+1: All tests passed!`.

```bash
cd glp_multiagent && flutter test
```

`+116 -2: Some tests failed.`  The two red are entries of `KNOWN_RED`.

Section Q writes each runner's whole output to `/Users/udi/Grassroots/tmp/suite_q_dart.txt` and `/Users/udi/Grassroots/tmp/suite_q_flutter.txt` on every run.  The full suite is run only by Code, on `main`: `GLP/CLAUDE.md`, "Test suites".

The interactive macOS app is not a route of the harness and is not described here; `GLP/CLAUDE.md`, "Flutter `glp_multiagent`", says how it is built.
