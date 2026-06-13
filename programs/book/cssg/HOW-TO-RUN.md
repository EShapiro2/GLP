# How to Run the SG and CSSG Demos

All seven plays typecheck and run correctly (verified 2026-02-21).

## Flutter Apps

There are two separate Flutter apps in `glp_multiagent/`:

- **Social Graph** (`lib/main.dart`) — Green Play 1/2/3 buttons, three agents: Alice, Bob, Charlie
- **Child-Safe Social Graph** (`lib/main_cssg.dart`) — Blue Play 4/5/6/7 buttons, four agents: Alice, Carol, Bob, Dave

Both targets produce the same output path (`glp_multiagent.app`), so build one, copy it aside, then build the other:

```bash
cd /Users/udi/Grassroots/GLP/glp_multiagent

# 1. Build the SG app
flutter build macos

# 2. Copy it aside
cp -R build/macos/Build/Products/Release/glp_multiagent.app \
      build/macos/Build/Products/Release/glp_sg.app

# 3. Build the CSSG app
flutter build macos --target lib/main_cssg.dart

# 4. Copy it aside
cp -R build/macos/Build/Products/Release/glp_multiagent.app \
      build/macos/Build/Products/Release/glp_cssg.app

# 5. Launch both
open build/macos/Build/Products/Release/glp_sg.app
open build/macos/Build/Products/Release/glp_cssg.app
```

### Play results (Flutter UI)

| App | Play | Scenario | Result |
|-----|------|----------|--------|
| SG | Play 1 | Alice connects to Bob, Bob introduces Alice and Charlie, both accept | Alice and Charlie become friends, exchange messages | ✅ |
| SG | Play 2 | Same setup, Alice accepts intro, Charlie rejects | Alice gets rejected(charlie) | ✅ |
| SG | Play 3 | Same setup, both reject intro | Both done immediately | ✅ |
| CSSG | Play 4 | Alice initiates child_introduce, all four consent | Carol and Dave become friends, exchange messages | ✅ |
| CSSG | Play 5 | Bob (parent) rejects | Carol gets rejected(dave), Dave never learns | ✅ |
| CSSG | Play 6 | Carol (child) rejects | Dave gets rejected(carol) | ✅ |
| CSSG | Play 7 | Dave (child) rejects | Carol gets rejected(dave) | ✅ |

## REPL (no Flutter)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/glp_repl.dart
```

At the `GLP>` prompt, load the four files:

```
../programs/book/cssg/typed_social_agent.glp
../programs/book/cssg/typed_ui_mediator.glp
../programs/book/cssg/typed_ui_actors.glp
../programs/book/cssg/play_ui_sim_boot.glp
```

Then run any play:

```
fplay1.   %% SG: both accept intro (tagged output)
fplay2.   %% SG: Alice accepts, Charlie rejects
fplay3.   %% SG: both reject
fplay4.   %% CSSG: all four consent
fplay5.   %% CSSG: Bob (p2) rejects
fplay6.   %% CSSG: Carol (c1) rejects
fplay7.   %% CSSG: Dave (c2) rejects
:quit
```

Silent plays (`play1`–`play7`) run without output (consumed by `sink`).
Flutter plays (`fplay1`–`fplay7`) emit `tagged(id, cmd/notify(...))` lines to stdout.
