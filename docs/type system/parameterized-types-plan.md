# Parameterized Types: Two-Stage Plan

**Created**: 2026-03-06
**Status**: Stage 1 not started

## Goal

Introduce parameterized types, convert all existing code to use them, then enforce a tight typing discipline by forbidding `_` and `_?` in user type definitions and procedure declarations.

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

Convert all existing code to use parameterized types, then forbid `_` and `_?` in user type definitions and procedure declarations.

### 2.1 Convert `self.glp`

Replace monomorphic type definitions with parameterized ones:

| Before | After |
|--------|-------|
| `Stream ::= [] ; [\|Stream].` | Remove (keep only `Stream(X)`) |
| `OpenStream ::= [_\|Stream].` | Remove (keep only `OpenStream(X)`) |
| `Channel ::= ch(Stream, Stream?).` | Remove (keep only `Channel(In, Out)`) |
| `DiffList ::= Stream \ Stream?.` | `DiffList(X) ::= Stream(X) \ Stream(X)?.` |

The old `DiffList` uses `Stream` which itself uses `_`. After conversion, `DiffList(X)` uses `Stream(X)`.

Predefined procedure declarations in `self.glp` are system-level (under `-mode(system)`) and may continue to use `_` and `_?` where they genuinely accept any term — e.g., `ground(_?)`, `=(_?, _)`, `=?=(_?, _?)`. These are truly polymorphic at the system level.

However, parameterize where possible:

| Before | After |
|--------|-------|
| `procedure send(_?, Channel?, Channel).` | `procedure send(X?, Channel(Stream(X))?, Channel(Stream(X))).` |
| `procedure receive(_, Channel?, Channel).` | `procedure receive(X, Channel(Stream(X))?, Channel(Stream(X))).` |
| `procedure new_channel(Channel, Channel).` | `procedure new_channel(Channel(X, Y), Channel(Y, X)).` |
| `procedure merge(Stream?, Stream?, Stream).` | `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).` |
| `procedure dl_append(DiffList?, DiffList?, DiffList).` | `procedure dl_append(DiffList(X)?, DiffList(X)?, DiffList(X)).` |
| `procedure dl_to_list(DiffList?, Stream).` | `procedure dl_to_list(DiffList(X)?, Stream(X)).` |

### 2.2 Convert book examples (`programs/book/`, `programs/typed_book/`)

These files use `Stream ::= [] ; [_|Stream]` and `Channel ::= ch(Stream, Stream?)` inherited from `self.glp`. After 2.1, they inherit `Stream(X)` and `Channel(In, Out)` instead.

Each file's procedure declarations must be updated to use instantiated types. The conversion is mechanical for most files:

- `procedure merge(Stream?, Stream?, Stream).` → `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).` (or use the inherited parameterized declaration)
- Files that define their own `Stream` must either remove it (inheriting from `self.glp`) or convert it to `Stream(X)`.
- Files that define domain-specific types already (e.g., `CounterCall ::= add ; clear ; read(Integer?).`) just need their procedure declarations updated to reference `Stream(CounterCall)`.

**Categories of book files by conversion difficulty:**

1. **Arithmetic/recursive** (factorial, fibonacci, quicksort, etc.): These typically use `Stream` only for list types. They need `Stream(Integer)`, `Stream(Number)`, or `Stream(String)` depending on content. Most are straightforward.

2. **Stream programs** (merge, copy, producers/consumers, etc.): These are the core cases. `merge` becomes parameterized. Programs that compose multiple stream operations need consistent instantiation.

3. **Monitor/object programs** (counter, queue_manager, etc.): These already define domain types like `CounterCall`. They need `Stream(CounterCall)` in procedure declarations.

4. **Social graph/network programs**: These already have rich type definitions. They need their stream and channel types parameterized with the appropriate message types.

5. **Meta-interpreters**: These are inherently untyped (they manipulate arbitrary terms). Their conversion is deferred to future work (see Open Questions).

### 2.3 Convert test files (`programs/tests/typed/`)

Same mechanical conversion as book files. Each test file's type definitions and procedure declarations are updated.

### 2.4 Convert module applications (CSSG, CSSN, social_graph_simulated_ui)

These already have domain-specific types in their `self.glp` files. The main change is parameterizing `merge`, `send`, `receive`, `new_channel` declarations in each module's procedure declarations.

The `boot` modules that were previously untyped (noted in the paper as motivation for parameterized types) can now be fully typed.

### 2.5 Remove renamed procedure copies

After parameterized types work, all the Section 14 workarounds (renamed copies like `merge_agent`, `send_agent`, etc.) can be removed. The parameterized originals serve the same purpose.

### 2.6 Archive `book/` directory

Move `programs/book/` to `programs/archive/book/`. This directory contains the original untyped examples. It is dated and does not pass type checking. The typed equivalents live in `programs/typed_book/`.

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
