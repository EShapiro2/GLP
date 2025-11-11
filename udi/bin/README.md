# Compiled Bytecode Directory

This directory is reserved for compiled GLP bytecode files (.glpc).

## Current Status

**Bytecode serialization is not yet implemented.**

The GLP compiler currently compiles .glp source files to in-memory bytecode
(List<Op>), but there is no serialization format to save compiled programs
to disk.

## Future Implementation

When bytecode serialization is implemented, this directory will contain:

- `.glpc` files - Compiled bytecode programs
- Fast loading without recompilation
- Bytecode verification and validation
- Version compatibility checks

## Current Workflow

For now, .glp source files are compiled on-demand:

1. Load source with `:load hello.glp`
2. Compiler parses and generates bytecode in memory
3. Program is cached in REPL session
4. Re-compilation needed each session

## Placeholder Files

The `.glpc` stub files in this directory are placeholders that reference
their source files. They will be replaced with actual bytecode when
serialization is implemented.
