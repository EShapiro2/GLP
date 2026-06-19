# GLP — Grassroots Logic Programs

**Last updated:** 2026-05-18

A concurrent, multi-agent, grassroots logic programming language with a Dart implementation.

## Core ideas

- **SRSW** (Single-Reader Single-Writer): each variable occurs at most once as writer and at most once as reader per clause.
- **Three-phase execution**: HEAD (tentative unification) → GUARDS (pure three-valued tests) → BODY (mutations).
- **Suspension**: goals suspend on unbound readers and reactivate when writers are bound.
- **Multi-agent**: agents run in separate Dart isolates and communicate via message-passing.

## Quick start

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/glp_repl.dart
```

In the REPL: enter a `.glp` filename to load it (the REPL runs the full pipeline — SRSW → PE → type-check → compile → execute), then enter a goal.  Or enter a directory (e.g. `../programs/cssn/`) to load a multi-module project.

Non-interactive:

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e 'load ../programs/path/to/file.glp\ngoal.\n:quit' | dart run bin/glp_repl.dart
```

## Running tests

From `/Users/udi/Grassroots/GLP/`:

```bash
bash test/run_all_tests.sh           # canonical REPL test suite (489 tests)
cd glp_runtime && dart test          # Dart unit tests (353 tests)
bash test/run_book_tests.sh          # book examples compilation
```

## Directory map

```
GLP/
├── CLAUDE.md                 # development instructions (read first)
├── README.md                 # this file
├── glp_runtime/              # Dart implementation
│   ├── lib/                  # compiler, runtime, type-checker, multi-agent
│   ├── bin/glp_repl.dart     # REPL source
│   └── test/                 # Dart unit tests
├── glp_multiagent/           # Flutter app for multi-agent demos
├── programs/                 # all GLP source files
│   ├── self.glp              # root prelude (types, primitives)
│   ├── book/                 # Art of GLP book examples
│   ├── book/           # typed version of book examples
│   ├── social/               # social platforms (modular projects)
│   │   ├── graph/            # plain social graph
│   │   ├── child_safe/       # child-safe social graph
│   │   └── network/          # child-safe social networking
│   ├── bonds/                # grassroots bonds (secure/ = SecureBonds)
│   └── tests/                # REPL test programs
├── docs/                     # specifications and references
├── test/                     # test scripts
├── AofGLP/                   # Art of GLP book sources
├── ArtOfProlog/              # Art of Prolog reference
└── archive/                  # historical material (pruned 2026-05)
```

## Documentation

- `CLAUDE.md` — development instructions
- `docs/DISCIPLINE.md` — spec-first discipline
- `docs/typed-glp-manual.md` — typed GLP programming manual
- `docs/glp-cheat-sheet.md` — patterns and idioms (GLP is not Prolog)
- `docs/README.md` — full documentation index
- `docs/known-issues.md` — outstanding known issues

## License

See `LICENSE` (if present).
