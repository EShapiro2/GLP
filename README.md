# GLP - Grassroots Logic Programs

A secure concurrent logic programming language implementation in Dart.

## Overview

GLP (Grassroots Logic Programs) is a concurrent logic programming language with:
- **SRSW Constraint**: Single-Reader/Single-Writer - each variable occurs at most once per clause
- **Three-Phase Execution**: HEAD (tentative unification) → GUARDS (pure tests) → BODY (mutations)
- **Suspension Mechanism**: Goals suspend on unbound readers, reactivate when writers are bound

## Quick Start

```bash
# Set up Dart (if needed)
export PATH="/home/user/dart-sdk/bin:$PATH"

# Run the REPL
cd glp_runtime
./glp_repl          # Use compiled executable (fast)
# or
dart run bin/glp_repl.dart   # Recompile each time (slow)

# In REPL:
# Load a file, then run queries
> programs/stdlib/meta.glp
> run(append([1,2], [3,4], X)).
```

## Directory Structure

```
GLP/
├── glp_runtime/           # Main Dart project
│   ├── lib/              # VM implementation
│   ├── test/             # Unit tests
│   └── bin/glp_repl.dart # REPL source
│
├── programs/              # All GLP source files (380 files)
│   ├── stdlib/           # Standard library (6 files)
│   ├── book/             # Art of GLP book examples (140 files)
│   │   ├── recursive/    # Recursive programming
│   │   ├── streams/      # Stream programming
│   │   ├── social_graph/ # Agent protocols
│   │   └── ...
│   ├── tests/            # REPL test files (115 files)
│   ├── lib/              # Reusable modules (8 files)
│   └── archive/          # Historical files (76 files)
│
├── test/                  # Test scripts
│   ├── quick_run_repl_tests.sh  # 30 quick tests (~2s)
│   ├── full_run_repl_tests.sh   # 181 comprehensive tests
│   └── run_book_tests.sh        # 141 book example tests
│
├── docs/                  # Specifications
│   ├── glp-bytecode-v216-complete.md
│   └── glp-runtime-spec.txt
│
└── CLAUDE.md              # Development instructions
```

## Running Tests

```bash
cd glp_runtime

# Quick sanity check (~2 seconds)
bash ../test/quick_run_repl_tests.sh

# Full REPL tests (181 tests)
bash ../test/full_run_repl_tests.sh

# Book examples compilation test (141 files)
bash ../test/run_book_tests.sh

# Dart unit tests
dart test
```

## Standard Library

The `programs/stdlib/` directory contains:
- `meta.glp` - Metainterpreter for running GLP programs
- `arithmetic.glp` - Arithmetic operations (`:=`)
- `time.glp` - Time primitives
- `assign.glp` - Assignment operator
- `unify.glp` - Unification primitives
- `univ.glp` - `=..` operator (term decomposition)

## Book Examples

The `programs/book/` directory contains examples from "The Art of GLP":

| Directory | Contents |
|-----------|----------|
| `recursive/arithmetic_trees/` | factorial, fibonacci, gcd, etc. |
| `recursive/list_processing/` | append, reverse, quicksort, etc. |
| `recursive/structure_processing/` | trees, traversals |
| `streams/producers_consumers/` | merge, distribute, etc. |
| `streams/objects_monitors/` | counters, monitors |
| `social_graph/` | Agent protocols, plays |
| `meta/` | Metainterpreters |
| `constants/` | Logic gates, circuits |

## Documentation

- `CLAUDE.md` - Development instructions for Claude Code
- `docs/glp-bytecode-v216-complete.md` - Bytecode specification
- `docs/glp-runtime-spec.txt` - Runtime architecture

## License

See LICENSE file for details.
