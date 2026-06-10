# Parameterized Types: Two-Stage Plan

**Created**: 2026-03-06
**Status**: Stage 1 COMPLETE, Stage 2.1 next

## Goal

Introduce parameterized types, convert all existing code to use them, then adopt a tight typing discipline where we do not use `_` or `_?` in user type definitions or procedure declarations.

---

## Stage 1: Implement Parameterized Types

Add parameterized type support as described in the paper (Section 8) and spec (`typed-program.md`). At the end of Stage 1, parameterized types work but the old imprecise types are still accepted.

### 1.1 Parser: parameterized type syntax

Extend the parser to recognize:

- **Type definitions**: `Stream(X) ::= [] ; [X | Stream(X)].`
- **Instantiations in type definitions**: `Pair(Integer, String)` inside another type body
- **Instantiations in procedure declarations**: `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).`
- **Instantiations in imported/exported declarations**: `imported procedure merge(Stream(CounterCall)?, ...)`.
- **Nested instantiations**: `Stream(Pair(Integer, String))`

The expanded name convention is `T<S₁,...,Sₖ>` internally, but the source syntax uses `T(S₁,...,Sₖ)`.

### 1.2 Expansion preprocessing

Implement `param_expansion.dart` (new file in `lib/analysis/type_checker/`) that runs after parsing and before type automaton construction:

1. Collect templates (type definitions with parameters)
2. Collect all instantiations from type definitions, procedure declarations (all three kinds), and type definition bodies
3. Expand each instantiation into a fresh monomorphic type definition
4. Replace all parameterized references with expanded names
5. Remove templates from the type environment

### 1.3 Parameterized procedure declaration inference

When a procedure is declared with type parameters (e.g., `merge(Stream(X)?, Stream(X)?, Stream(X))`), the type checker infers the binding of `X` at each call site by structural matching against the concrete types from the call context. Conflicting bindings produce an error.

### 1.4 Update `self.glp` with parameterized type definitions

Add parameterized versions alongside the existing monomorphic ones:

```
Stream(X) ::= [] ; [X | Stream(X)].
OpenStream(X) ::= [X | Stream(X)].
Channel(In, Out) ::= ch(In, Out?).
```

Keep the old `Stream`, `Channel`, etc. for backward compatibility during Stage 1.

### 1.5 Tests

Add to the test suite:

- **Positive**: basic `Stream(Integer)` expansion with merge
- **Positive**: `Channel(Msg, Msg)` with mode annotations preserved
- **Positive**: nested `Stream(Pair(Integer, String))`
- **Positive**: parameterized procedure declaration with inference
- **Positive**: imported procedure with instantiated parameters
- **Negative**: conflicting type parameter bindings
- **Negative**: wrong arity in instantiation

### 1.6 Validation

Run the full test suite. All existing tests must pass unchanged — parameterized types are purely additive at this stage.

---

## Stage 2: Convert to Tight Typing and Remove Imprecise Types

Convert all existing code to use parameterized types, then adopt a tight typing discipline.

**Key principle**: Both monomorphic and parameterized definitions coexist in `self.glp` throughout the conversion (as established in Step 1.4). Files are converted one directory at a time while both forms are available. Only after all files are converted are the monomorphic definitions removed. **Tests must pass after every step.**

### 2.1 Convert test files (`programs/tests/typed/`)

Convert test files first — they are small, well-understood, and directly exercised by the test suite. Each file's type definitions and procedure declarations are updated to use parameterized types.

Conversion rules:
- `procedure merge(Stream?, Stream?, Stream).` → `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).` (parameterized) or `procedure merge(Stream(Integer)?, Stream(Integer)?, Stream(Integer)).` (concrete instantiation), depending on context.
- Files that define their own `Stream ::= [] ; [_|Stream].` locally: remove the local definition (they inherit `Stream(X)` from `self.glp`) and update procedure declarations to use instantiated types.
- Files that already define domain-specific types (e.g., `CounterCall`) just need procedure declarations updated to reference `Stream(CounterCall)`.

Run full test suite after conversion. All tests must pass.

### 2.2 Convert typed book examples (`programs/typed_book/`)

**Categories by conversion difficulty:**

1. **Arithmetic/recursive** (factorial, fibonacci, quicksort, etc.): Use `Stream` only for list types. Need `Stream(Integer)`, `Stream(Number)`, or `Stream(String)` depending on content. Straightforward.

2. **Stream programs** (merge, copy, producers/consumers, etc.): Core cases. `merge` becomes parameterized. Programs composing multiple stream operations need consistent instantiation.

3. **Monitor/object programs** (counter, queue_manager, etc.): Already define domain types like `CounterCall`. Need `Stream(CounterCall)` in procedure declarations.

4. **Social graph/network programs**: Already have rich type definitions. Need stream and channel types parameterized with the appropriate message types.

5. **Meta-interpreters**: Deferred to future work (see Future Work section).

Run full test suite after conversion. All tests must pass.

### 2.3 Convert module applications (CSSG, CSSN, social_graph_simulated_ui)

These already have domain-specific types in their `self.glp` files. The main change is parameterizing `merge`, `send`, `receive`, `new_channel` declarations in each module's procedure declarations.

The `boot` modules that were previously untyped (noted in the paper as motivation for parameterized types) can now be fully typed.

Run full test suite after conversion. All tests must pass.

### 2.4 Parameterize `self.glp` procedure declarations

Before removing renamed copies, the root self.glp's generic procedures must be parameterized. Otherwise there's nothing to replace the renamed copies with — the generic `send(_?, Channel?, Channel)` can't serve as a replacement for `send_agent(MediatorToAgentMsg?, AgentChannel?, AgentChannel)` because `AgentChannel ≠ Channel`.

Parameterize procedure declarations in `self.glp`:

| Before | After |
|--------|-------|
| `procedure send(_?, Channel?, Channel).` | `procedure send(X?, Channel(Stream(X))?, Channel(Stream(X))).` |
| `procedure receive(_, Channel?, Channel).` | `procedure receive(X, Channel(Stream(X))?, Channel(Stream(X))).` |
| `procedure new_channel(Channel, Channel).` | `procedure new_channel(Channel(X, Y), Channel(Y, X)).` |
| `procedure merge(Stream?, Stream?, Stream).` | `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).` |
| `procedure dl_append(DiffList?, DiffList?, DiffList).` | `procedure dl_append(DiffList(X)?, DiffList(X)?, DiffList(X)).` |
| `procedure dl_to_list(DiffList?, Stream).` | `procedure dl_to_list(DiffList(X)?, Stream(X)).` |

Also remove the old monomorphic type definitions (now that all downstream files use parameterized types):

| Remove | Keep |
|--------|------|
| `Stream ::= [] ; [_\|Stream].` | `Stream(X) ::= [] ; [X \| Stream(X)].` |
| `OpenStream ::= [_\|Stream].` | `OpenStream(X) ::= [X \| Stream(X)].` |
| `Channel ::= ch(Stream, Stream?).` | `Channel(In, Out) ::= ch(In, Out?).` |
| `DiffList ::= Stream \ Stream?.` | `DiffList(X) ::= Stream(X) \ Stream(X)?.` |

Predefined procedure declarations that genuinely accept any term — `ground(_?)`, `=(_?, _)`, `=?=(_?, _?)` — keep `_` and `_?`.

Also convert module-local monomorphic channel types to parameterized instantiations where needed. For example, `AgentChannel ::= ch(AgentToUserStream, MediatorToAgentStream?).` should become a type alias for `Channel(AgentToUserStream, MediatorToAgentStream)`, or the modules should use `Channel(AgentToUserStream, MediatorToAgentStream)` directly.

Run full test suite. All tests must pass.

### 2.5 Remove renamed procedure copies

Now that `send`, `receive`, `new_channel`, and `merge` are parameterized, the Section 14 workarounds (renamed copies like `send_agent`, `send_user`, etc.) can be removed. The parameterized originals serve the same purpose through call-site type parameter inference.

Run full test suite after removal. All tests must pass.

### 2.6 Archive `book/` directory

Move `programs/book/` to `programs/archive/book/`. This directory contains the original untyped examples. It is dated and does not pass type checking. The typed equivalents live in `programs/typed_book/`.

Update `test/run_book_tests.sh` to point to the new location, or retire the script if the book tests are no longer relevant.

Run full test suite after archiving.

### 2.7 Adopt tight typing discipline

The GLP language continues to support `_` and `_?` in type definitions and procedure declarations. However, as a project discipline, we do not use them in our code. All our type definitions use concrete types or type parameters; all our procedure declarations use concrete or parameterized types.

This discipline is documented in:
- `typed-glp-manual.md` (new section)
- `claude.md` / `CLAUDE.md` (as a rule for AI-generated code)
- The Moded-Types paper (as the recommended practice)

System-level builtins in `self.glp` (under `-mode(system)`) that genuinely accept any term — such as `ground(_?)`, `=(_?, _)`, `=?=(_?, _?)` — are the only exception.

### 2.8 Final validation

Run the full test suite. All tests must pass. Verify that no user-mode `.glp` file in `programs/` (outside `self.glp` and `archive/`) uses `_` or `_?` in type definitions or procedure declarations.

---

## What Does NOT Change

- `_` and `_?` remain as primitive types in the type system (final states in the DFA)
- System-level code (`-mode(system)`) can still use `_` and `_?`
- The type automaton, well-typing, subtyping, moded head construction — all unchanged
- `_` (anonymous variable) in clause bodies — unchanged (this is a variable, not a type)

## Future Work

- **Meta-interpreters**: These manipulate arbitrary terms and cannot be precisely typed with the current system. Their conversion to parameterized types is deferred. Options to explore: a dedicated `Goal` type covering the meta-interpreter's domain, or accepting that meta-interpreters are an exception to the tight typing discipline.
