# Fix: Implement Proper Type Checking for Parameterized Procedure Declarations

## 🔴 MANDATORY READING FIRST

Read `docs/type system/fix-bare-type-params.md` for context on the original bug and the Part C that was skipped.

## Problem

The "bare type params" fix took a shortcut: instead of implementing proper call-site instantiation (Part C of the fix instructions), it skips type checking entirely for parameterized procedure declarations. Four places return success without checking:

1. `type_checker.dart`: `if (procDecl.isParameterized) continue;`
2. `program_dfa.dart`: `if (procDecl.isParameterized) continue;` (×2, for states and automata)
3. `well_typed_clause.dart` `_checkHeadWithTerm`: `return (WellTypedResult.success({}), modedHeadTerm);`
4. `well_typed_clause.dart` `_checkBodyAtomWithTerm`: `return (WellTypedResult.success({}), modedAtomTerm);`

This means parameterized procedures are completely unchecked — any type error inside them passes silently.

## Correct Approach

Parameterized proc decls are templates. They need to be **instantiated** to concrete types before checking. There are two cases:

### Case A: Checking the proc decl's own clauses

A parameterized proc decl like `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).` is universally quantified over X. Its clauses must be valid for ANY X. 

**Solution**: Instantiate all type parameters to `_` (wildcard), creating a concrete proc decl. For `merge(Stream(X)?, Stream(X)?, Stream(X))` with X→`_`, this produces `merge(Stream<_>?, Stream<_>?, Stream<_>)`. The wildcard `_` correctly represents "any element type." Then check clauses against this concrete proc decl using the existing machinery.

Steps:
1. In `param_expansion.dart`: for each parameterized proc decl, generate a "wildcard instantiation" — substitute each type param with `_`, creating a concrete proc decl
2. Also generate the corresponding expanded type definitions (e.g., `Stream<_> ::= [] ; [_ | Stream<_>].`)
3. Add both the concrete proc decl and expanded type defs to the module
4. The DFA builder and type checker see only concrete types — no skipping needed

### Case B: Checking calls to parameterized procedures from other procedures

When `merge(A?, B?, C)` appears in the body of a clause whose head has typed variables (e.g., A has type `Stream<AgentMsg>?`), the type checker must infer X=AgentMsg, create a concrete proc decl `merge(Stream<AgentMsg>?, Stream<AgentMsg>?, Stream<AgentMsg>)`, and check the call against it.

**Solution**: In `_checkBodyAtomWithTerm`, when the looked-up proc decl is parameterized:

1. **Infer type parameter bindings from variable types**: For each argument position, match the declared parameterized type against the actual argument's known type (from `allVariableTypes`).

   Matching algorithm for a single argument:
   - Declared arg is `TypeRef('Stream', typeArgs: [TypeRef('X')])` with isInput — a parameterized type
   - Actual argument is a variable (e.g., `A?`) whose type from `allVariableTypes` has a DFAState with name like `Stream<AgentMsg>?`
   - Parse the DFA state name: strip trailing `?`, then extract template name and args from `Name<Args>` format
   - `Stream<AgentMsg>` → template `Stream`, concrete arg `AgentMsg` → bind X=AgentMsg
   - For bare type param: declared `TypeRef('X')`, actual variable type `AgentMsg` → bind X=AgentMsg
   
2. **Create a concrete proc decl**: Substitute all type params in argTypes. TypeRef('Stream', typeArgs: [TypeRef('X')]) + X→AgentMsg = TypeRef('Stream<AgentMsg>'). Bare TypeRef('X') + X→AgentMsg = TypeRef('AgentMsg'). Preserve isInput flags.

3. **Ensure expanded type defs exist**: The expanded types like `Stream<AgentMsg>` should already exist from the expansion of the caller's head types. If not, the type checker will report a missing type.

4. **Check the body atom against the concrete proc decl**: Use the existing `_checkModedTermPerArg` — no skipping.

### Threading variable types

`_checkBodyAtomWithTerm` currently doesn't receive `allVariableTypes`. It needs to be passed as an additional parameter from `checkClause`. The call chain:

```
checkClause → collects allVariableTypes from head → passes to _checkBodyAtomWithTerm
```

Add `Map<String, VariableTypeInfo> callerVarTypes` as an optional parameter to `_checkBodyAtomWithTerm`. Only used when the looked-up proc decl is parameterized.

## Implementation Steps

### Step 1: Wildcard instantiation for own clauses (Case A)

In `param_expansion.dart`, after the existing expansion steps, for each parameterized proc decl:

1. Create a substitution mapping each type param to `PrimitiveModeAlt(false)` (i.e., `_`)
2. Substitute all argTypes, producing concrete types like `Stream<_>`
3. Generate expanded type defs for `Stream<_>` if they don't already exist (they should — `Stream(_)` is equivalent to the monomorphic `Stream`, which still exists in self.glp during Stage 2)
4. Add the concrete proc decl to the module's procDeclarations (alongside the parameterized one, or replacing it for checking purposes)
5. Keep the parameterized proc decl in the environment too — body call sites need it to find the template

Actually, the simplest approach: in `param_expansion.dart`, generate a wildcard-instantiated concrete proc decl for each parameterized one. Use a naming convention like the original name (since the clauses use the original name). The parameterized proc decl is kept only for call-site inference.

**Wait — there's a naming issue.** Both the parameterized and wildcard-instantiated versions have the same name/arity. The type environment is keyed by name/arity. 

**Resolution**: The expansion step replaces the parameterized proc decl with its wildcard-instantiated version in the environment. The original parameterized proc decl is stored separately (e.g., in a `Map<String, ProcDecl> parameterizedProcDecls` on the Module or passed alongside). When checking a body call, if the concrete proc decl's argTypes contain only `_`/`_?`, check if there's a parameterized version available for inference.

Simpler: store both. In the TypeEnvironment, the proc decl keyed by `merge/3` is the wildcard-instantiated concrete one (for checking merge's own clauses). A separate field `Map<String, ProcDecl> paramProcDecls` stores the parameterized templates (for call-site inference).

### Step 2: Call-site instantiation (Case B)

In `well_typed_clause.dart`:

1. Add `TypeEnvironment env` to `_checkBodyAtomWithTerm` signature (it already has it)
2. Add `Map<String, VariableTypeInfo> callerVarTypes` parameter
3. After looking up procDecl, check if there's a parameterized template for this proc in `env.paramProcDecls`
4. If yes, infer bindings from callerVarTypes and the actual arguments
5. Create a concrete proc decl with inferred types
6. Use the concrete proc decl for `producedTerm` and `_checkModedTermPerArg`

### Step 3: Remove all skips

Remove ALL four skip points. All parameterized proc decls are now concrete after instantiation:

1. `type_checker.dart`: Remove `if (procDecl.isParameterized) continue;`
2. `program_dfa.dart`: Remove both `if (procDecl.isParameterized) continue;`
3. `well_typed_clause.dart`: Remove both `if (procDecl.isParameterized) { return success }` blocks

## Files to Change

1. `type_ast.dart` — Add `paramProcDecls` field to TypeEnvironment
2. `param_expansion.dart` — Generate wildcard-instantiated proc decls; store parameterized originals
3. `program_dfa.dart` — Remove skip (parameterized proc decls are now concrete)
4. `well_typed_clause.dart` — Remove skips; add call-site instantiation logic
5. `type_checker.dart` — Remove skip
6. `type_environment_builder.dart` — May need updates to handle new TypeEnvironment field

## Tests

All 389 existing tests must pass. Additionally:

1. Verify that `param_bare_typevar.glp` (`gethead(Stream(X)?, X)`) still passes
2. Add a negative test: a parameterized proc decl with a type error in its clause (e.g., wrong mode on a variable) — must be caught, not silently accepted
3. After fixing, attempt to remove `send_agent`/`send_user` renamed copies from mediator files — if call-site inference works, these should no longer be needed

## Execution

1. Run baseline tests — commit
2. Implement Step 1 (wildcard instantiation)
3. Implement Step 2 (call-site instantiation) 
4. Implement Step 3 (remove all skips)
5. Run tests — all must pass
6. Add negative test for parameterized proc decl with type error
7. Run tests again
8. Commit and push
