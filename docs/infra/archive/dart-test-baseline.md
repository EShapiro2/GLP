# Dart Test Baseline — 2026-03-13

REPL: 428/428 pass
Dart: 369 passed, 5 skipped, 37 failed

## Failures by file (37 total)

| # | File | Test | Count |
|---|------|------|-------|
| 1 | test/dynamic_dispatch_test.dart | serve/2 compiles and has label | 1 |
| 2 | test/multiagent/ui_mediator_test.dart | grounds befriend, connected, received | 3 |
| 3 | test/multiagent/output_kernel_test.dart | consumes ground stream, waits for stream elements | 2 |
| 4 | test/analysis/type_checker/moded_head_test.dart | DiffList explicit dual preserves internal structure | 1 |
| 5 | test/archive/dump_bytecode_test.dart | dump all social_graph bytecode | 1 |
| 6 | test/archive/actor_single_isolate_test.dart | compile boot, traces actor, traces simple send | 3 |
| 7 | test/archive/direct_structterm_test.dart | copy pattern | 1 |
| 8 | test/archive/debug_goal_args_test.dart | debug goal args | 1 |
| 9 | test/module/cssn_modules_test.dart | static linking type checking, static vs dynamic comparison | 2 |
| 10 | test/module/social_graph_sim_modules_test.dart | project discovery, type checking, static vs dynamic | 3 |
| 11 | test/srsw_test.dart | anonymous variable _ (2 tests) | 2 |
| 12 | test/runtime/cssg_glp_dispatch_test.dart | fplay1 via GLP dispatch | 1 |
| 13 | test/runtime/serve_test.dart | dispatches single goal, multiple exports | 2 |
| 14 | test/runtime/activate_kernel_test.dart | dispatches valid goal, aborts when no _select | 2 |
| 15 | test/heap/arithmetic_pointer_test.dart | setUpAll + Z := 5+3 | 2 |
| 16 | test/bytecode/arithmetic_test.dart | Z := 5+3 | 1 |
| 17 | test/engine/glp_engine_test.dart | clause selection, simple goal | 2 |
| 18 | test/compiler/select_dispatch_test.dart | one export, multiple exports | 2 |
| 19 | test/compiler/project_linker_test.dart | type checking all modules | 1 |

## Skips (5 total)

All in test/multiagent/mad_error_handling_test.dart:
- duplicate LocalizeEntry rejected
- global_send on already-known reader is no-op
- receive for non-existent GlobalizeEntry throws
- receive for non-existent LocalizeEntry throws
- removing non-existent entry is safe
