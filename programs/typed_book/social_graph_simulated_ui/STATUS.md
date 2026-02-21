# Social Graph — Simulated UI (Frozen Snapshot)

**Date: 2026-02-20**

**This is a frozen snapshot** of the social graph code with ack/nack friend introduction, before the child-safe social graph (CSSG) extension. The active development copy is in `../cssg/`.

## What this contains

The ack/nack friend introduction protocol with simulated UI plays, running in the Flutter app via REPL subprocess. Three agents (Alice, Bob, Charlie) execute scripted plays showing cold-call befriend, messaging, and friend-mediated introduction with accept/reject.

## Current state

All files typecheck and load successfully in the REPL.
All plays run correctly: silent (play1–3 with sink) and Flutter (fplay1–3 with tagged output).
Dart/Flutter integration is implemented and working.

## Files

| File | Status | Description |
|------|--------|-------------|
| `typed_social_agent.glp` | Typechecks, runs | Agent with ack/nack intro via channel passing |
| `typed_ui_mediator.glp` | Typechecks, runs | Mediator storing channel(Ch) in pending for intro |
| `typed_ui_actors.glp` | Typechecks, runs | Three test scripts: play1/play2/play3 |
| `play_ui_dglp_boot.glp` | Typechecks, runs | Wiring: network3 + agents + mediators + actors |
| `play_ui_sim_boot.glp` | Typechecks, runs | Simulated UI boot: tee/sink + tee/tagged plays |
| `play_dglp_boot.glp` | Typechecks, runs | Original dGLP entry point (no mediator, no intro) |

## How to run

### REPL (silent plays, no Flutter)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e '../programs/typed_book/social_graph_simulated_ui/typed_social_agent.glp\n../programs/typed_book/social_graph_simulated_ui/typed_ui_mediator.glp\n../programs/typed_book/social_graph_simulated_ui/typed_ui_actors.glp\n../programs/typed_book/social_graph_simulated_ui/play_ui_sim_boot.glp\nplay1.\n:quit' | dart run bin/glp_repl.dart
```

Replace `play1.` with `play2.` or `play3.` for other scenarios.

### REPL (tagged output for Flutter)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e '../programs/typed_book/social_graph_simulated_ui/typed_social_agent.glp\n../programs/typed_book/social_graph_simulated_ui/typed_ui_mediator.glp\n../programs/typed_book/social_graph_simulated_ui/typed_ui_actors.glp\n../programs/typed_book/social_graph_simulated_ui/play_ui_sim_boot.glp\nfplay1.\n:quit' | dart run bin/glp_repl.dart
```

### Flutter app

Build and launch the `glp_multiagent` app, then click Play 1/2/3 buttons. Note: the Flutter app's `ReplPlayRunner` currently points to the `cssg/` directory. To run this snapshot in Flutter, update the file paths in `repl_play_runner.dart`.

## Test plays

| Play | Scenario | Result |
|------|----------|--------|
| play1 | Both accept intro | Alice and Charlie become friends, exchange messages |
| play2 | Alice accepts, Charlie rejects | Alice gets rejected(charlie); Charlie done immediately |
| play3 | Both reject intro | Both done immediately after sending nack |
