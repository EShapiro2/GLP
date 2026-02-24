# How to Run the SG and CSSG Demos

## Flutter Apps

There are two separate Flutter apps in `glp_multiagent/`:

- **Social Graph** (`lib/main.dart`) — Play 1/2/3, three agents: Alice, Bob, Charlie
- **Child-Safe Social Graph** (`lib/main_cssg.dart`) — Play 4/5/6/7 (four agents: Alice, Carol, Bob, Dave) and Play 8/9/10/11 (two agents: Alice, Bob — unfriend)

Both targets produce the same output path (`glp_multiagent.app`), so build one, copy it aside, then build the other:

```bash
cd /Users/ohadey/Desktop/Grassroots/GLP2/GLP/glp_multiagent

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
cd /Users/ohadey/Desktop/Grassroots/GLP2/GLP/glp_runtime
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
play8.   %% Unfriend: Alice unfriends Bob, then sends (silently dropped)
play9.   %% Unfriend: Alice unfriends Bob, Alice re-connects
play10.  %% Unfriend: Alice unfriends Bob, Bob re-connects
play11.  %% Unfriend: full lifecycle (3 rounds with negative tests)
:quit
```

Silent plays (`play1`–`play11`) run and terminate silently (output consumed by `sink`).
Flutter plays (`fplay1`–`fplay11`) emit `tagged(id, cmd/notify(...))` lines to stdout.

## Play Descriptions

### Plays 1-3: Social Graph Introduction
- **Play 1**: Alice introduces Bob and Charlie; both accept.
- **Play 2**: Alice introduces Bob and Charlie; Alice accepts, Charlie rejects.
- **Play 3**: Alice introduces Bob and Charlie; both reject.

### Plays 4-7: Child-Safe Social Graph (4 agents)
- **Play 4**: Parent-mediated child introduction; all four consent.
- **Play 5**: Bob (parent 2) rejects the introduction.
- **Play 6**: Carol (child 1) rejects the introduction.
- **Play 7**: Dave (child 2) rejects the introduction.

### Plays 8-11: Unfriend Protocol (2 agents)
- **Play 8**: Alice and Bob connect, then Alice unfriends Bob. Alice sends a message after unfriending — it is silently dropped (channel closed).
- **Play 9**: Alice unfriends Bob, then Alice re-connects to Bob. Tests that re-friending by the unfriender works correctly.
- **Play 10**: Alice unfriends Bob, then Bob re-connects to Alice. Tests that re-friending by the unfriended party works correctly.
- **Play 11**: Full lifecycle test with 3 rounds. Round 1: connect, chat, unfriend, negative test. Round 2: re-connect, chat, unfriend, negative test. Round 3: re-connect, chat, unfriend, negative test. Each round verifies that messages sent after unfriending (`alice_will_not_get_it`) are silently dropped.
