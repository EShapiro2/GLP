# How to Run Project Files

**Updated: 2026-02-17**

The `project_` prefixed files are independent copies of the social graph files. They can be modified freely without affecting the originals.

---

## dGLP (no UI) — REPL with scripted actors

| Project file | Copied from |
|---|---|
| `project_typed_social_agent.glp` | `typed_social_agent.glp` |
| `project_typed_actors.glp` | `typed_actors.glp` |
| `project_play_dglp_boot.glp` | `play_dglp_boot.glp` |

```bash
cd /Users/ohadey/Desktop/Grassroots/GLP2/GLP/glp_runtime
echo -e 'load ../programs/typed_book/social_graph/project_typed_social_agent.glp\nload ../programs/typed_book/social_graph/project_typed_actors.glp\nload ../programs/typed_book/social_graph/project_play_dglp_boot.glp\nplay.\n:quit' | dart run bin/glp_repl.dart
```

Expected output: `→ suspended`

---

## dGLP (with mediator + UI actors) — REPL with ground-term protocol

| Project file | Copied from |
|---|---|
| `project_typed_social_agent.glp` | `typed_social_agent.glp` |
| `project_typed_ui_mediator.glp` | `typed_ui_mediator.glp` |
| `project_typed_ui_actors.glp` | `typed_ui_actors.glp` |
| `project_play_ui_dglp_boot.glp` | `play_ui_dglp_boot.glp` |

```bash
cd /Users/ohadey/Desktop/Grassroots/GLP2/GLP/glp_runtime
echo -e 'load ../programs/typed_book/social_graph/project_typed_social_agent.glp\nload ../programs/typed_book/social_graph/project_typed_ui_mediator.glp\nload ../programs/typed_book/social_graph/project_typed_ui_actors.glp\nload ../programs/typed_book/social_graph/project_play_ui_dglp_boot.glp\nplay.\n:quit' | dart run bin/glp_repl.dart
```

Expected output: `→ suspended`

---

## Flutter UI — interactive multi-agent app

| Project file | Copied from |
|---|---|
| `project_social_graph_agent.glp` | `social_graph_agent.glp` |
| `project_social_graph_ui_mediator.glp` | `social_graph_ui_mediator.glp` |
| `project_social_graph_ui_boot.glp` | `social_graph_ui_boot.glp` |
| `glp_multiagent/lib/project_main.dart` | `glp_multiagent/lib/main.dart` |

```bash
cd /Users/ohadey/Desktop/Grassroots/GLP2/GLP/glp_multiagent
flutter run -d macos -t lib/project_main.dart
```

---

## All project files

```
programs/typed_book/social_graph/
├── project_typed_social_agent.glp       # agent/4, channel ops, merge, helpers
├── project_typed_actors.glp             # Scripted actors that talk directly to agent/4
├── project_play_dglp_boot.glp           # dGLP boot: network3 + play (no mediator)
├── project_typed_ui_mediator.glp        # Ground-term mediator between agent/4 and UI
├── project_typed_ui_actors.glp          # Scripted UI actors using ground-term protocol
├── project_play_ui_dglp_boot.glp        # dGLP boot with mediator + UI actors
├── project_social_graph_agent.glp       # social_graph/3 for Flutter UI
├── project_social_graph_ui_mediator.glp # Ground-term mediator for Flutter UI
└── project_social_graph_ui_boot.glp     # Flutter UI boot: agent_init/3

glp_multiagent/lib/
└── project_main.dart                    # Flutter entry point loading project_ GLP files
```
