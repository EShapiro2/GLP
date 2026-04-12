## Absolute Paths Only

**All file paths must be resolved to absolute at the entry point (REPL, test script, etc.) before passing them to the engine.** The engine and everything below it (linker, type checker, module hierarchy) must never resolve relative paths — they work only with absolute paths.

Relative paths are CWD-dependent, which is fragile. The REPL's working directory varies depending on how it is invoked (`dart run` from `glp_runtime/` vs `GLP/` vs elsewhere).

**Rule:** If you receive a relative path as input, resolve it to absolute immediately. Never store or pass relative paths to infrastructure code.
