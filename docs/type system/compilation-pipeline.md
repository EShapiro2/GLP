# GLP Compilation Pipeline

**Version**: 0.2  
**Date**: 2026-01-18  
**Status**: DRAFT

## Overview

This document specifies the GLP compilation pipeline and clarifies the distinction between GLP language requirements and optional extensions.

## GLP Language Definition vs Extensions

### SRSW: Part of GLP Definition

The Single-Reader Single-Writer (SRSW) discipline is part of the GLP language definition. A program that violates SRSW is **not a GLP program**, just as a program with syntax errors is not a GLP program.

**Consequence:** SRSW violations are fatal errors that reject the program.

**Exception:** Anonymous writers `_` are permitted without a corresponding reader. The underscore explicitly disclaims interest in the produced value.

### Typing: Optional Extension

The GLP type system is an **extension** to GLP, providing optional static analysis. A program that fails type checking is still a valid GLP program and should compile and run.

**Consequence:** Type errors are advisory. Whether to require well-typed programs is a development policy decision, not a language requirement.

## Compilation Pipeline

The GLP compilation pipeline proceeds in the following order:

```
Source Code
    │
    ▼
┌─────────┐
│  Parse  │  → Syntax errors are fatal
└────┬────┘
     │
     ▼
┌──────────────┐
│ SRSW Check   │  → SRSW violations are fatal (except anonymous _)
└──────┬───────┘
       │
       ▼
┌───────────────────┐
│ Partial Evaluation │  → Unfolds defined guards (=/2, new_channel/2, etc.)
└─────────┬─────────┘
          │
          ▼
┌─────────────┐
│ Type Check  │  → Type errors are advisory (warnings)
└──────┬──────┘
       │
       ▼
┌─────────┐
│ Compile │  → Generates bytecode
└────┬────┘
     │
     ▼
   Run
```

### Phase 1: Parse

Lexical and syntactic analysis. Produces AST or rejects malformed syntax.

**Fatal errors:** Syntax errors, unbalanced parentheses, invalid tokens.

### Phase 2: SRSW Check

Verifies the Single-Reader Single-Writer discipline:
- Each variable has exactly one writer and one reader
- Exception: Anonymous writer `_` needs no reader

**Fatal errors:** SRSW violations (program is not GLP).

### Phase 3: Partial Evaluation

Unfolds defined guards (unit clauses) before type checking. This includes:
- `=/2` (unification)
- `new_channel/2`, `send/3`, `receive/3` (channel operations)
- `dl_append/3`, `dl_to_list/2` (difference list operations)

The type checker sees the program as it will execute, with syntactic abstractions expanded.

### Phase 4: Type Check

Verifies the program against declared types per the paper's Definition [Well-typed GLP program]:

1. **Covariance:** Every clause is well-typed
2. **Contravariance:** Every input path is covered

**Advisory errors:** Type errors do not prevent compilation. They are reported as warnings unless strict mode is enabled by development policy.

### Phase 5: Compile

Generates bytecode from the (possibly partially-evaluated) AST.

### Phase 6: Run

Executes the bytecode.

## Implementation Entry Points

| Entry Point | SRSW | Partial Eval | Type Check | Compile |
|-------------|------|--------------|------------|---------|
| `glp_repl.dart` | ✓ (in parser) | ✓ | ✓ (if declarations present) | ✓ |
| `GlpCompiler` | ✓ (in parser) | ✓ | ✓ (if `--type-check`) | ✓ |
| `check_types.dart` | ✓ (in parser) | ✓ | ✓ | ✗ |

All entry points now correctly apply partial evaluation before type checking.

## Defined Guards

Defined guards are unit clauses that are unfolded at compile time. They have clauses in the prelude but no procedure declarations, because after partial evaluation they no longer appear as calls.

From prelude:
```prolog
% Unification
X? = X.

% Difference lists
dl_append(A\B?, B\C?, A?\C).
dl_to_list(L\[], L?).

% Channels
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
```

## Known Errors

### SRSW Display Bug

There is a reported bug where SRSW violations are not displayed when type errors are present (bug-report-2026-01-17.md, Bug 7). SRSW checking should run and report independently of type checking.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2026-01-18 | Initial draft establishing pipeline and SRSW vs typing distinction |
| 0.2 | 2026-01-18 | Fixed check_types.dart to apply partial evaluation; removed from known issues |
