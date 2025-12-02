# GLP Source Programs

This directory contains GLP source code representing proper GLP syntax and semantics.

## Status

**These programs represent ideal GLP syntax** based on our bytecode test implementations. The GLP compiler may not fully support all features yet, which is expected and acceptable.

## Programs

### Core Examples

- **hello.glp** - Hello world with terminal I/O
- **echo.glp** - Interactive echo with concurrent sequencing
- **p.glp** - Simple facts: `p(a). p(b).`
- **q.glp** - Calls p: `q(X) :- p(X?).`

### Stream Operations

- **merge.glp** - N-to-1 stream merging with alternation
- **circular_merge.glp** - Circular dependencies creating infinite streams

### Metainterpreter

- **run.glp** - Simple metainterpreter for GLP
- **clause.glp** - Clause database for metainterpreter

### Data Structures

- **list_ops.glp** - Append, member, length operations
- **struct_demo.glp** - Compound term demonstrations

### System Predicates

- **arithmetic.glp** - Arithmetic via `execute('evaluate', ...)`
- **factorial.glp** - Factorial with guards and arithmetic
- **file_demo.glp** - File I/O operations

## GLP Syntax Notes

### Writer/Reader Notation

- `X` - Writer variable (can be written to once)
- `X?` - Reader variable (reads from paired writer)

### Example

```glp
% In head: X is a writer
% In body: X? reads from that writer
append([X|Xs], Ys, [X?|Zs?]) :- append(Xs?, Ys?, Zs).
```

### Guards

Guards appear before the `|` separator:

```glp
fact(N, F) :-
  known(N), ground(N) |
  execute('evaluate', [N - one, N1]),
  fact(N1?, F1).
```

## Compiler Limitations

The current GLP compiler may report violations such as:

- "SRSW violation: Reader variable 'X?' occurs 2 times"
- "SRSW violation: Writer variable 'X' occurs 2 times"

**These are compiler limitations, not errors in the GLP programs.**

The programs in this directory represent correct GLP semantics as implemented
in our bytecode tests. They serve as:

1. **Documentation** - Shows proper GLP syntax
2. **Test cases** - For compiler development
3. **Examples** - For learning GLP semantics

## Working with These Programs

Currently, most programs must be manually constructed as bytecode (see tests
in `glp_runtime/test/custom/`).

Some simple programs can be loaded:
```bash
dart run glp_repl.dart
GLP> :load p.glp
GLP> :load hello.glp
```

Complex programs with advanced SRSW usage will need compiler improvements.

## Related Files

- Tests: `/Users/udi/GLP/glp_runtime/test/custom/`
- Bytecode specs: `/Users/udi/GLP/docs/glp-bytecode-v216-complete.md`
- Implementation status: `/Users/udi/GLP/IMPLEMENTATION_STATUS.md`
