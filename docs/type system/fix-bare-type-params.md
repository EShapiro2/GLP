# Fix: Bare Type Parameters in Parameterized Procedure Declarations

## Problem

`procedure gethead(Stream(X)?, X).` fails with `UnknownTypeError: X` because the expansion step replaces `Stream(X)` with `Stream<X>` but leaves bare `X` as `TypeRef('X')`, which the DFA builder cannot resolve.

More generally, any parameterized procedure declaration where a type parameter appears bare (not only inside a parameterized type ref) fails. Example: `procedure gethead(Stream(X)?, X).`, `procedure first(Stream(X)?, X, Stream(X)).`

## Root Cause

`param_expansion.dart` Step 5 blindly expands all proc decl type refs. But a parameterized proc decl is a *template* that should be instantiated per call site, not expanded globally.

## Fix — Three Parts

### Part A: Add `typeParams` to `ProcDecl` (`type_ast.dart`)

Add a `List<String> typeParams` field to `ProcDecl`, default `const []`. Add `bool get isParameterized => typeParams.isNotEmpty;`. Update the constructor. Make sure all existing call sites that create ProcDecl still compile (the new param has a default value).

### Part B: Preserve parameterized proc decls in expansion (`param_expansion.dart`)

In Step 5, when processing proc decls:

1. **Detect type parameters**: For each proc decl, collect all names that appear as `typeArgs` inside parameterized `TypeRef`s (where the outer TypeRef name is a template). Then check which of those names are NOT defined types (not in templates, not in monoTypeDefs, not builtins). Those are the type parameters.

2. **For parameterized proc decls**: Do NOT call `_replaceParamRefs`. Keep the original `TypeRef` forms (with `typeArgs`). Set `typeParams` on the new `ProcDecl`.

3. **For non-parameterized proc decls**: Behavior unchanged — expand as before.

4. **Also**: Don't generate expanded type defs for instantiations that contain type parameters. For example, `Stream(X)` in a parameterized proc decl should NOT generate a `Stream<X>` type definition. Only concrete instantiations like `Stream(Integer)` should generate expanded type defs. This means `_collectInstantiations` for proc decls should skip type refs whose type args contain type parameter names.

### Part C: Instantiate parameterized proc decls at call sites (`well_typed_clause.dart`)

In `_checkBodyAtomWithTerm` (and `_checkHeadWithTerm` if needed), after looking up the proc decl:

If `procDecl.isParameterized`:

1. **Infer type parameter bindings**: For each argument position, match the declared type against the actual argument's type (known from the head's variable types collected earlier in `checkClause`).

   Matching rules:
   - Declared `TypeRef('Stream', typeArgs: [TypeRef('X')])` against actual variable typed as DFA state `Stream<AgentMsg>` → bind `X = AgentMsg`
   - Declared bare `TypeRef('X')` against actual variable typed as DFA state `AgentMsg` → bind `X = AgentMsg`
   - If actual arg is a constant/structure (not a variable), determine its type from the type environment

   To get the actual arg's type: look it up in `allVariableTypes` (the head variable types collected earlier). The variable type info contains a `DFAState` with a name. Parse the DFA state name to extract concrete type info.

2. **Create concrete ProcDecl**: Substitute all type parameter occurrences in `argTypes`:
   - `TypeRef('Stream', typeArgs: [TypeRef('X')])` + binding `X→AgentMsg` → `TypeRef('Stream<AgentMsg>')`
   - Bare `TypeRef('X')` + binding `X→AgentMsg` → `TypeRef('AgentMsg')`
   
   Ensure the expanded type defs exist (they should, since the concrete types come from the head context which was already expanded).

3. **Use the concrete ProcDecl** for `producedTerm()` and `_checkModedTermPerArg()`.

**Threading variable types**: `_checkBodyAtomWithTerm` needs access to the head variable types to infer bindings. Currently it doesn't have them. Either:
- Pass `allVariableTypes` as an additional parameter, or
- Do a two-pass approach: first collect all variable types from head, then check body goals with that context

Looking at `checkClause`, the head is checked first and `allVariableTypes` is populated from it. The body atoms are checked in a loop after. So `allVariableTypes` is available — just pass it to `_checkBodyAtomWithTerm`.

## Files to Change

1. `glp_runtime/lib/analysis/type_checker/type_ast.dart` — add `typeParams` to `ProcDecl`
2. `glp_runtime/lib/analysis/type_checker/param_expansion.dart` — detect and preserve parameterized proc decls
3. `glp_runtime/lib/analysis/type_checker/well_typed_clause.dart` — instantiate at call site

## Test

After the fix, this should work:

```glp
Stream(X) ::= [] ; [X | Stream(X)].

procedure gethead(Stream(X)?, X).
gethead([H|_], H?).
```

And the `_` workarounds added during Step 2.1 should be reverted to use proper bare type parameters.

## Execution

1. Run baseline tests
2. Implement Part A (type_ast.dart)
3. Implement Part B (param_expansion.dart)
4. Implement Part C (well_typed_clause.dart)
5. Add test: `programs/tests/typed/param_bare_typevar.glp` with `gethead` example
6. Run full test suite — all 388+ tests must pass
7. Revert any `_` workarounds from Step 2.1 files, replacing with proper bare type params
8. Run full test suite again
9. Commit and push
