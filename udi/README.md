# Udi's GLP Workspace

Personal workspace for GLP source programs and compiled bytecode.

## Directory Structure

```
udi/
├── glp/          # GLP source files (.glp)
├── bin/          # Compiled bytecode files (.glpc)
├── glp_repl.dart # Workspace-aware REPL
└── pubspec.yaml  # Dart package configuration
```

## Usage

### Start the REPL

From the `udi/` directory:

```bash
dart run glp_repl.dart
```

### REPL Commands

- `:load <file>` - Load and compile a GLP source file from `glp/`
- `:compile <file>` - Compile a GLP file (bytecode serialization coming soon)
- `:list` - Show all loaded programs
- `:help` or `:h` - Show help message
- `:quit` or `:q` - Exit REPL

### Interactive Goals

You can also type GLP goals directly:

```glp
GLP> execute('write', ['Hello World']).
GLP> merge([a,b], [c,d], X).
```

## Example Programs

### hello.glp

Simple hello world program:

```glp
hello :-
  execute('write', ['Hello from GLP!']),
  execute('nl', []).
```

Run with: `:load hello.glp`

### echo.glp

Read input and echo back with proper concurrent sequencing:

```glp
echo :-
  execute('write', ['Enter text: ']),
  execute('read', [Line]),
  wait_then_echo(Line?).

wait_then_echo(Input) :-
  known(Input) |
  execute('write', ['You entered: ']),
  execute('write', [Input]),
  execute('nl', []).
```

Run with: `:load echo.glp`

## Creating New Programs

1. Create a `.glp` file in the `glp/` directory
2. Write your GLP code using the concurrent logic programming model
3. Load it in the REPL with `:load yourfile.glp`

## Notes

- Bytecode serialization to `bin/` is not yet implemented
- Programs are compiled and cached in memory when loaded
- The REPL uses the GLP compiler from `glp_runtime` package
- All system predicates are available (24 total)
