# Handover: Multi-Isolate Tests for cssn_modules_v2

## Goal

Create multi-isolate (madGLP) test runs for all 13 plays in `programs/cssn_modules_v2`. Currently these plays run single-isolate with a GLP network switch routing messages. The multi-isolate versions replace the network switch with `IsolateManager`, running each agent in its own Dart isolate.

## Architecture

Read these files first to understand the existing infrastructure:

- `glp_runtime/lib/multiagent/isolate_manager.dart` — spawns isolates, routes messages
- `glp_runtime/lib/multiagent/boot_loader.dart` — parses `@agent` syntax from boot files
- `glp_runtime/test/multiagent/isolate_manager_test.dart` — existing multi-isolate tests
- `programs/typed_book/social_graph/play_madglp_boot.glp` — example madGLP boot file
- `programs/cssn_modules_v2/boot.glp` — the single-isolate boot with all 13 plays

## Key Concept

In single-isolate mode, `boot.glp` creates:
- A network switch (`network2`/`network3`/`village_net`) routing messages between agents
- Per-agent wiring: actor + tee + agent + mediator + tee + send_to_user_tagged
- Parent-child channel wiring via `merge`

In multi-isolate mode:
- **Network switch disappears** — `IsolateManager` routes messages via Dart SendPort
- **Each isolate** runs ONE agent's internal pipeline (actor + tee + agent + mediator + tee + output)
- **Parent-child channels** need special handling: in single-isolate they're GLP streams wired with `merge`; in multi-isolate the child's messages to parent go through the network

## What to Create

### 1. Boot files: `programs/cssn_modules_v2/mad_boot/`

Create one boot file per play: `mad_fplay1.glp` through `mad_fplay13.glp`.

Each file has:
- `-mode(system).`
- `procedure boot.` declaration
- `boot :-` clause with `@agent` directives
- Agent init procedures

**Pattern for adult agents** (fplay1-3, fplay8):
```glp
procedure boot.
boot :-
    agent_init(alice, 1, _)@alice,
    agent_init(bob, 1, _)@bob,
    agent_init(charlie, 1, _)@charlie.

procedure agent_init(Constant?, Constant?, Stream?).
agent_init(Id, PlayNum, NetIn) :-
    ground(Id?), ground(PlayNum?) |
    actor_dispatch(Id?, PlayNum?, ch(ActorIn?, ActorOut)),
    tee(ActorOut?, MedIn, DispCmd),
    agent # agent(Id?, AgentIn?, NetIn?,
          [output('_user', AgentToUser), output('_net', NetOut)]),
    send_to_net(NetOut?),
    mediator # ui_mediator(Id?, ch(AgentToUser?, AgentIn),
                ch(MedIn?, MedOut), [], 1),
    tee(MedOut?, ActorIn, DispNotify),
    send_to_user_tagged(Id?, DispCmd?, DispNotify?).
```

The `send_to_net(NetOut?)` is the madGLP global_send mechanism — it globalizes the network output stream so bindings are sent to remote agents. This is already handled by the madGLP infrastructure when the agent writes to the `'_net'` output.

**Pattern for plays with child agents** (fplay4-7, fplay9-12):
Adult agents use `agent#agent` with `output(child(Name), ChildStream)`.
Child agents use `child_agent#child_agent` with `output(parent(Name), ParentStream)`.
The parent-child streams must be globalized so they route through IsolateManager.

**Pattern for fplay13** (village): 
Uses `village/*.glp` actors with narrative output. Each agent init calls the village-specific actor and `send_to_user_narrate`.

**Actor dispatch**: Each boot file needs to route agent IDs to the correct actor procedures. Since the project is loaded via `loadProject`, all `actors#alice1`, `actors#bob4`, `village/alice#alice13` etc. are available.

### 2. Test file: `glp_runtime/test/multiagent/cssn_v2_isolate_test.dart`

A Dart test file that runs each play via IsolateManager. Follow the pattern in `isolate_manager_test.dart` but:
- Use `config.projectDir = '../programs/cssn_modules_v2'` for project loading
- Load each `mad_fplayN.glp` boot file
- Collect tagged output from agents
- Verify expected output patterns (same checks as `run_all_tests.sh` Section H)

Structure:
```dart
group('CSSN v2 Multi-Isolate', () {
  for (final playNum in List.generate(13, (i) => i + 1)) {
    test('fplay$playNum runs across isolates', () async {
      final bootSource = File('mad_boot/mad_fplay$playNum.glp').readAsStringSync();
      final loader = BootLoader();
      final config = loader.load(bootSource);
      config.projectDir = '../programs/cssn_modules_v2';
      config.rootSelfGlpPath = '...';
      
      final manager = IsolateManager();
      await manager.boot(config);
      manager.start();
      
      // Wait for completion (output stops arriving)
      await Future.delayed(Duration(seconds: 5));
      await manager.shutdown();
      
      // Verify output
    });
  }
});
```

## Derivation Method

For each play (fplay1-fplay13), look at the corresponding clause in `cssn_modules_v2/boot.glp`:

1. **Extract agent list**: Which agents are spawned? Which use `agent#agent` vs `child_agent#child_agent`?
2. **Extract actor calls**: Which `actors#aliceN(...)` or `village/alice#alice13(...)` is called per agent?
3. **Extract parent-child wiring**: Which agents have `output(child(...), ...)` or `output(parent(...), ...)`?
4. **Write boot directive**: `goal(agentId, ...)@agentId` for each agent
5. **Write init procedure**: Per-agent wiring from the single-isolate play, minus network switch

## Summary of plays

| Play | Agents | Notes |
|------|--------|-------|
| fplay1 | alice, bob, charlie | Basic SG: both accept intro |
| fplay2 | alice, bob, charlie | Alice accepts, Charlie rejects |
| fplay3 | alice, bob, charlie | Both reject |
| fplay4 | alice, bob, carol, dave | CSSG: all accept. carol=child(alice), dave=child(bob) |
| fplay5 | alice, bob, carol, dave | CSSG: Bob rejects |
| fplay6 | alice, bob, carol, dave | CSSG: Carol rejects |
| fplay7 | alice, bob, carol, dave | CSSG: Dave rejects |
| fplay8 | alice, bob | CSSN: Group join |
| fplay9 | alice, bob, dave | CSSN: Child joins group. dave=child(bob) |
| fplay10 | alice, bob, dave | CSSN: Parent rejects child's group invite |
| fplay11 | alice, bob, charlie, carol, dave, eve | CSSN: Child-safe groups (6 agents) |
| fplay12 | alice, bob, charlie, dave, eve | CSSN: Parental consent for groups (5 agents) |
| fplay13 | alice, bob, frank, carol, dave, eve | Village scenario (6 agents, narrative output) |

## Important: Start with fplay1

Get fplay1 working first end-to-end before generating the rest. It's the simplest (3 adult agents, no children, no groups). Once the pattern works, the rest are mechanical variations.

## Principles

- Read the existing infrastructure code before writing anything
- The project directory loading means all module code is available in each isolate
- Follow the exact wiring from the single-isolate boot.glp — just extract per-agent
- The madGLP global_send mechanism handles network output automatically
- Test output patterns should match what the single-isolate plays produce
