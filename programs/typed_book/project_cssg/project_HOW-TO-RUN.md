# How to Run the SG and CSSG Demos

## Flutter Apps

There are two separate Flutter apps in `glp_multiagent/`:

- **Social Graph** (`lib/main.dart`) — Play 1/2/3, three agents: Alice, Bob, Charlie
- **Child-Safe Social Graph** (`lib/main_cssg.dart`) — Play 4/5/6/7, four agents: Alice, Carol, Bob, Dave

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

## REPL (no Flutter)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/glp_repl.dart
```

At the `GLP>` prompt, load the four files:

```
../programs/typed_book/cssg/typed_social_agent.glp
../programs/typed_book/cssg/typed_ui_mediator.glp
../programs/typed_book/cssg/typed_ui_actors.glp
../programs/typed_book/cssg/play_ui_sim_boot.glp
```

Then run any play:

```
play1.   %% SG: both accept intro
play2.   %% SG: Alice accepts, Charlie rejects
play3.   %% SG: both reject
play4.   %% CSSG: all four consent
play5.   %% CSSG: Bob (p2) rejects
play6.   %% CSSG: Carol (c1) rejects
play7.   %% CSSG: Dave (c2) rejects
:quit
```

Silent plays (`play1`–`play7`) run and terminate silently (output consumed by `sink`).
Flutter plays (`fplay1`–`fplay7`) emit `tagged(id, cmd/notify(...))` lines to stdout.
