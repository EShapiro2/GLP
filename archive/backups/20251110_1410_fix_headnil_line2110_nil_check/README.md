# GLP Runtime

Dart implementation of the GLP (Grassroots Logic Programs) runtime system.

## Overview

GLP is a secure, multiagent, concurrent logic programming language with single-reader/single-writer (SRSW) logic variables. This runtime implements:

- **Bytecode VM** (`lib/bytecode/`) - v2.16 instruction set with σ̂w commit semantics
- **Runtime System** (`lib/runtime/`) - Heap, scheduler, ROQueues, suspension/reactivation
- **System Predicates** - External functions callable from GLP programs

## Implementation Status

### Core VM ✅
- Three-valued unification (success/suspend/fail)
- Writer MGU with tentative bindings (σ̂w)
- Suspension/reactivation on unbound readers
- Fair scheduling with tail-recursion budgets
- Multi-goal concurrent execution

### System Predicates ✅
All non-channel predicates for Logix support:

- **Arithmetic**: `evaluate/2`
- **Utilities**: `current_time/1`, `unique_id/1`, `variable_name/2`, `copy_term/2`
- **File I/O**: `file_read/2`, `file_write/2`, `file_exists/1`, `file_open/3`, `file_close/1`, `file_read_handle/2`, `file_write_handle/2`
- **Directory**: `directory_list/2`
- **Terminal**: `write/1`, `nl/0`, `read/1`
- **Module Loading**: `link/2`, `load_module/2`

**Deferred**: Channel primitives (`create_merger/2`, `distribute_stream/2`)

## Documentation

See parent directory for specifications:
- `CLAUDE.md` - Implementation guide
- `SPEC_GUIDE.md` - Specification overview
- `docs/glp-bytecode-v216-complete.md` - Normative instruction set
- `docs/glp-runtime-spec.txt` - Runtime architecture

## Testing

```bash
dart test                    # Run all tests
dart test test/custom/       # Run custom tests only
dart analyze                 # Check for issues
```
