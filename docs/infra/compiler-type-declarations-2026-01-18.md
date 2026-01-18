# GLP Compiler: Type Declaration Syntax Not Supported

**Date**: 2026-01-18  
**From**: Multiagent GLP Project  
**To**: GLP Runtime / Typed GLP Project  
**Priority**: Blocking

---

## Summary

The GLP compiler does not handle type declaration syntax (`TypeName ::= ...`). When a GLP program includes type declarations, the compiler fails with a parse error. This blocks any program that includes both type annotations and executable code.

---

## Error Observed

**Program**: `/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp`

**Error**:
```
[ERROR] Expected predicate name
Line 26, Column 1:
NetMsg ::= msg(_, _, _).
^
```

**Context**: The Flutter multiagent app (`glp_multiagent`) uses `GlpCompiler().compile()` to load and compile GLP source files. When the source contains type declarations, the parser fails.

---

## Root Cause

The parser in `/Users/udi/Grassroots/GLP/glp_runtime/lib/compiler/parser.dart` has two parsing methods:

1. `parse()` — Legacy method that calls `_skipDeclarations()` then parses procedures
2. `parseModule()` — Newer method that handles type definitions and procedure declarations

The `GlpCompiler.compile()` method uses `parse()`, and `_skipDeclarations()` does not recognize or skip `TypeName ::= ...` syntax.

---

## Proposed Fix

Update `_skipDeclarations()` in `parser.dart` to recognize and skip type declaration lines. The pattern to skip is:

```
UppercaseAtom ::= alternative ; alternative ; ... .
```

This allows a single GLP source file to include both type annotations (for the type checker) and executable code (for the compiler), with the compiler simply ignoring the type declarations.

---

## Files to Modify

- `/Users/udi/Grassroots/GLP/glp_runtime/lib/compiler/parser.dart` — `_skipDeclarations()` method

---

## Workaround (Current)

Strip type declarations from source before compilation, or maintain two versions of each program (typed for analysis, untyped for execution). Neither is acceptable long-term.

---

## Impact

This blocks testing of any typed GLP program in the runtime, including the multiagent `social_agent.glp` program needed for friend-mediated introduction protocol testing.

---

## Contact

Please notify Multiagent GLP project when fixed so testing can proceed.
