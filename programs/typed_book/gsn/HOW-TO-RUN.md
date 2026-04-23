# How to Run the GSN Demos

## Flutter App

The GSN Flutter app is in `glp_multiagent/`:

- **Grassroots Social Network** (`lib/main_gsn.dart`) — Play 4/5/6/7 (four agents: Alice, Carol, Bob, Dave), Play 8/9/10/11 (two agents: Alice, Bob — unfriend), and Play 12/13/14 (three agents: Alice, Bob, Charlie — friendship updates)

```bash
cd /Users/ohadey/Desktop/Grassroots/GLP2/GLP/glp_multiagent

# Build the GSN app
flutter build macos --target lib/main_gsn.dart

# Copy it aside
cp -R build/macos/Build/Products/Release/glp_multiagent.app \
      build/macos/Build/Products/Release/glp_gsn.app

# Launch
open build/macos/Build/Products/Release/glp_gsn.app
```

## REPL (no Flutter)

```bash
cd /Users/ohadey/Desktop/Grassroots/GLP2/GLP/glp_runtime
dart run bin/glp_repl.dart
```

At the `GLP>` prompt, load the four files:

```
../programs/typed_book/gsn/typed_social_agent.glp
../programs/typed_book/gsn/typed_ui_mediator.glp
../programs/typed_book/gsn/typed_ui_actors.glp
../programs/typed_book/gsn/play_ui_sim_boot.glp
```

Then run any play:

```
play4.   %% CSSG: all four consent
play5.   %% CSSG: Bob (p2) rejects
play6.   %% CSSG: Carol (c1) rejects
play7.   %% CSSG: Dave (c2) rejects
play8.   %% Unfriend: Alice unfriends Bob, then sends (silently dropped)
play9.   %% Unfriend: Alice unfriends Bob, Alice re-connects
play10.  %% Unfriend: Alice unfriends Bob, Bob re-connects
play11.  %% Unfriend: full lifecycle (3 rounds with negative tests)
play12.  %% Friendship update: Alice initiates to Charlie (reject then accept)
play13.  %% Friendship update: Charlie initiates to Alice (reject then accept)
play14.  %% Friendship update: Alice-Bob friends, Charlie-Bob friends, Alice unfriends Bob
:quit
```

Silent plays (`play4`–`play14`) run and terminate silently (output consumed by `sink`).
Flutter plays (`fplay4`–`fplay14`) emit `tagged(id, cmd/notify(...))` lines to stdout.

## Play Descriptions

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

### Plays 12-14: Friendship Update Protocol (3 agents)
- **Play 12**: Alice and Bob are friends. Alice cold-calls Charlie — Charlie rejects (Bob NOT notified). Alice cold-calls Charlie again — Charlie accepts (Bob gets `update_befriend(charlie)`). Alice unfriends Charlie (Bob gets `update_unfriend(charlie)`).
- **Play 13**: Alice and Bob are friends. Charlie cold-calls Alice — Alice rejects (Bob NOT notified). Charlie cold-calls Alice again — Alice accepts (Bob gets `update_befriend(charlie)`). Charlie unfriends Alice (Bob gets `update_unfriend(charlie)`).
- **Play 14**: Three-way friendship then unfriend. (1) Alice and Bob become friends. (2) Charlie cold-calls Bob — Bob accepts (Alice gets `update_befriend(bob, charlie)`). (3) Alice cold-calls Charlie — Charlie accepts (Bob gets `update_befriend(alice, charlie)` and `update_befriend(charlie, alice)`). (4) Alice unfriends Charlie (Bob gets `update_unfriend(alice, charlie)` and `update_unfriend(charlie, alice)`).
