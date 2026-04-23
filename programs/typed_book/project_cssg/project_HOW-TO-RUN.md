# How to Run the CSSG Demos

## Prerequisites

- **Dart SDK** (version 3.9.4 or later)
- **Flutter SDK** (for the GUI app)
- Clone the GLP repository: `git clone https://github.com/EShapiro2/GLP.git`

## Play Descriptions

### Plays 4-7: Parent-Child Consent Protocol (4 agents)

These plays test the child-safe friend request protocol, where children
(Carol, Dave) cannot befriend each other without parental consent from
their parents (Alice, Bob).

- **Play 4** -- All four consent: Alice (parent) and Bob (parent) both approve, Carol and Dave become friends.
- **Play 5** -- Bob rejects: Alice approves but Bob rejects the request. Friendship is not established.
- **Play 6** -- Carol rejects: Carol (child) declines the friend request herself. Friendship is not established.
- **Play 7** -- Dave rejects: Dave (child) declines the friend request himself. Friendship is not established.

### Plays 8-11: Unfriend Protocol (2 agents)

These plays test the unfriend protocol between Alice and Bob directly
(no parent-child layer).

- **Play 8** -- Basic unfriend: Alice connects to Bob, both are friends. Alice unfriends Bob. Both sides receive `unfriended` notification. Bob tries to send a message after unfriend -- it goes nowhere (channel torn down).
- **Play 9** -- Re-friend by unfriender: Alice connects to Bob, then Alice unfriends. Alice re-initiates friendship. Bob accepts again. Bob sends a message to confirm the new connection works.
- **Play 10** -- Re-friend by unfriended party: Alice connects to Bob, then Alice unfriends. Bob (the unfriended party) re-initiates friendship. Alice accepts. Alice sends a message to confirm the new connection works.
- **Play 11** -- Full lifecycle (3 rounds covering all combinations):
  - Round 1: Alice connects Bob, sends `hi1`, then Alice unfriends. Bob tries to send `alice_will_not_get_it` after unfriend (negative test -- Alice never receives it).
  - Round 2: Alice re-connects Bob, Bob sends `hi2`, then Bob unfriends. Bob again tries to send `alice_will_not_get_it` after unfriend (negative test).
  - Round 3: Bob connects Alice, Alice sends `hi3`. Done.

  This play exercises: both sides initiating, both sides unfriending, re-friend after unfriend by both parties, message delivery, and post-unfriend message blocking.

## Running via REPL (no Flutter)

```bash
cd <GLP_ROOT>/glp_runtime
dart run bin/glp_repl.dart
```

At the `GLP>` prompt, load the four source files:

```
../programs/typed_book/project_cssg/project_typed_social_agent.glp
../programs/typed_book/project_cssg/project_typed_ui_mediator.glp
../programs/typed_book/project_cssg/project_typed_ui_actors.glp
../programs/typed_book/project_cssg/project_play_ui_sim_boot.glp
```

Then run any play:

```
play4.    %% CSSG: all four consent
play5.    %% CSSG: Bob (parent) rejects
play6.    %% CSSG: Carol (child) rejects
play7.    %% CSSG: Dave (child) rejects
play8.    %% Unfriend: basic unfriend + post-unfriend send
play9.    %% Unfriend: re-friend by unfriender
play10.   %% Unfriend: re-friend by unfriended party
play11.   %% Unfriend: full lifecycle (3 rounds, all combos)
:quit
```

Silent plays (`playN`) run and terminate silently (output consumed by `sink`).

Flutter plays (`fplayN`) emit `tagged(id, cmd/notify(...))` lines to stdout,
used by the Flutter app to route output to per-agent panels.

## Running via Flutter App (macOS)

```bash
cd <GLP_ROOT>/glp_multiagent

# Build the CSSG app
flutter build macos --target lib/main_cssg.dart

# Copy it aside (the build always produces glp_multiagent.app)
cp -R build/macos/Build/Products/Release/glp_multiagent.app \
      build/macos/Build/Products/Release/glp_cssg.app

# Launch
open build/macos/Build/Products/Release/glp_cssg.app
```

The app shows play buttons (4-11) at the top. Plays 4-7 display 4 agent panels
(Alice/Carol as one family, Bob/Dave as another). Plays 8-11 display 2 agent
panels (Alice and Bob).

Click any play button to run the scenario. Commands (user actions) appear
in blue; notifications (system responses) appear in green/bold.

## Running on Another Computer

1. Clone the repo and ensure Dart and Flutter are installed.

2. The Flutter app resolves the GLP repo root automatically. It checks:
   - The parent of the app's working directory
   - Hardcoded fallback paths (for known developer machines)

   If running from a non-standard location, either:
   - Run the app from within `<GLP_ROOT>/glp_multiagent/` so the parent
     directory resolves correctly, or
   - Add your path as a fallback in `_resolveRepoRoot()` in
     `glp_multiagent/lib/main_cssg.dart`.

3. The app spawns a Dart REPL subprocess (`dart run bin/glp_repl.dart`
   from `<GLP_ROOT>/glp_runtime/`). Dart must be on the PATH or at
   one of the checked locations (`/usr/local/bin/dart`,
   `$HOME/flutter/bin/dart`, etc.).

4. Alternatively, skip the Flutter app entirely and use the REPL directly
   as described above -- it works on any platform with the Dart SDK.
