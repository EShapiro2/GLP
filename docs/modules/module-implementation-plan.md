# GLP Module System — Implementation Plan

**Date:** 2026-02-22
**Spec:** `docs/modules/glp-module-system-spec.md`

---

## 1. Existing Code Assessment

### Keep as-is
- **`ImportVector`** — indexed stream distribution, sound design
- **`ExportMessage` / `CallInfo`** — message format works
- **`ServeImport`** — lazy bridge process per import, correct FCP behavior

### Revise
- **`ModuleLoader`** — add hierarchy awareness: directory traversal, `self.glp` loading, ancestor scope assembly
- **`LoadedModule`** — add type declarations (type automata), replace `exports: Set<String>` with typed exported procedure declarations
- **`FcpModuleRegistry`** — add hierarchical name resolution (`ui#actors` → path lookup), load-time type compatibility verification
- **`ModuleRuntime`** — revise boot sequence for hierarchical loading, `self.glp` chain
- **`ModuleHandlers`** — revise to work with typed procedure declarations instead of string signatures

### Replace
- **`-export([proc/arity])` parsing** — replace with `exported procedure p(...)` syntax
- **`-import([module])` parsing** — replace with `imported procedure path#name(...)` syntax
- **`Module` AST node** — revise to reflect new syntax (no import/export lists, procedure-level visibility)

### Add
- **Type scope assembly** — collect type definitions from ancestor `self.glp` chain
- **Cross-module type checking** — verify `#` calls against local `imported` declarations
- **Load-time compatibility checker** — subtype verification: imported vs exported declarations
- **`self.glp` handling** — directory-scope file, type-only files allowed

---

## 2. Implementation Phases

### Phase 1: Syntax

**Goal:** Parse the new module syntax. Old syntax stops working.

**Parser changes:**
- `exported procedure p(T1?, T2, ...)` — public procedure declaration
- `imported procedure mod#p(T1?, T2, ...)` — cross-module dependency with full type signature
- `imported procedure p(T1?, T2, ...)` — ancestor-scope dependency (no path)
- Qualified type references in arguments: `social#AgentChannel?`
- Remove `-export([...])` parsing
- Remove `-import([...])` parsing
- Keep `-module(name).` (optional, defaults to filename)
- Keep `Module # Goal` call syntax

**AST changes:**
- `ProcedureDeclaration` gains `exported: bool`, `imported: bool`, `modulePath: String?`
- Remove `ExportDeclaration` and `ImportDeclaration` nodes
- `Module` AST: just name, list of type defs, list of procedures (each with visibility)

**Tests:** Parse `exported procedure`, parse `imported procedure` (with and without path, with qualified types), reject old `-export`/`-import`.

### Phase 2: Hierarchy and Scoping

**Goal:** Load `self.glp` chain, assemble type scope from ancestors.

**ModuleLoader changes:**
- Given a module path (e.g., `project/ui/actors.glp`), walk up directories collecting `self.glp` files
- Each `self.glp` produces a scope: its type definitions and procedure declarations
- Child scopes shadow parent scopes (same name replaces)
- The assembled scope is the union of all ancestor `self.glp` definitions, with child-wins shadowing

**LoadedModule changes:**
- Carries its **effective type scope**: the assembled chain of ancestor types plus its own
- Carries **type automata** for all types in scope (built during compilation)

**Directory structure handling:**
- `self.glp` may contain only type definitions (no procedures) — parser must allow this
- When loading `project/ui/actors.glp`, the loader reads:
  1. Prelude (root ancestor)
  2. `project/self.glp` (if exists)
  3. `project/ui/self.glp` (if exists)
  4. `project/ui/actors.glp` (the module itself)

**Tests:** Load module with ancestor `self.glp`, verify type visibility, verify shadowing.

### Phase 3: Cross-Module Type Checking

**Goal:** Type-check `M # proc(X?, Y)` against the local `imported procedure` declaration.

**Type checker changes:**
- When encountering `M # proc(args...)`, find the local `imported procedure M#proc(...)` declaration
- Type-check call arguments against the imported declaration using standard well-typing rules
- No need to access module M — the imported declaration provides all type information locally
- Qualified types (`social#AgentChannel`) resolve through the imported declaration's type scope

**Error reporting:**
- "No imported declaration for `M#proc` — add `imported procedure M#proc(...)` to this module"
- "Type mismatch in call to `M # proc(...)`: argument 1 expected `AgentChannel?`, got `Channel?`"

**Tests:** Cross-module calls that type-check against imported declarations, calls that fail on type mismatch, calls without a corresponding imported declaration.

### Phase 4: Dynamic Load-Time Verification

**Goal:** When loading a module at runtime, verify type compatibility.

**Loader changes:**
- A compiled module carries its exported procedure declarations and their type automata (serialized)
- On load, compare actual module's `exported` declarations against the client's `imported` declarations
- Use subtyping (Definition 5.10 of the paper): the exported declaration must be subtype-compatible with the imported declaration, with appropriate variance

**LoadedModule changes:**
- Carries serialized type automata alongside bytecode for all exported procedures

**Verification:**
- For each `imported` declaration in the client: find the corresponding `exported` declaration in the loaded module and verify subtype compatibility
- Reject at load time if incompatible, with clear error message: "Loaded module M's exported procedure p(...) is not compatible with imported declaration"

**Tests:** Load compatible module (passes), load module with changed type (detected), load module with subtype-compatible change (passes).

### Phase 5: Compilation Scope

**Goal:** Support flexible compilation scope — single file, directory, whole project.

**Compiler changes:**
- Single file: compile one `.glp` against its own `imported` declarations
- Directory: compile all `.glp` files in a directory together with their `self.glp`
- Whole project: compile entire tree, enabling cross-module inlining and optimization

**Optimization opportunities (whole-project):**
- Inline cross-module calls when both sides are in scope
- Eliminate runtime type checks (verified statically)
- Dead-code elimination based on subtyping constraints
- Specialize message dispatch based on known types

**Tests:** Compile single file against imported declarations, compile directory, compile project.

---

## 3. File Changes Summary

| File | Action | Phase |
|------|--------|-------|
| `lib/compiler/parser.dart` | Revise: `exported/imported procedure`, remove `-export`/`-import` | 1 |
| `lib/compiler/ast.dart` | Revise: procedure visibility fields, remove export/import nodes | 1 |
| `lib/runtime/module_loader.dart` | Revise: hierarchy traversal, `self.glp` chain | 2 |
| `lib/runtime/loaded_module.dart` | Revise: type scope, type automata | 2 |
| `lib/compiler/type_checker.dart` | Revise: cross-module `#` call checking via imported declarations | 3 |
| `lib/runtime/module_registry.dart` | Revise: hierarchical resolution, type verification | 3, 4 |
| `lib/runtime/module_runtime.dart` | Revise: boot with hierarchy | 2 |
| `lib/runtime/module_handlers.dart` | Revise: typed procedure lookup | 3 |
| `lib/runtime/module_messages.dart` | Keep | — |
| `lib/runtime/import_vector.dart` | Keep | — |
| `lib/runtime/serve_import.dart` | Keep | — |
| `lib/runtime/dispatcher.dart` | Revise: check exported declarations | 3 |

---

## 4. Testing Strategy

Each phase has its own test suite. Tests from earlier phases must continue to pass.

**Phase 1 tests:** `test/module/module_syntax_test.dart` — parsing
**Phase 2 tests:** `test/module/module_hierarchy_test.dart` — scoping and `self.glp`
**Phase 3 tests:** `test/module/module_typecheck_test.dart` — cross-module type checking
**Phase 4 tests:** `test/module/module_compat_test.dart` — dynamic load verification
**Phase 5 tests:** `test/module/module_compile_scope_test.dart` — compilation scope

End-to-end test: the `typed_social_agent` + `typed_ui_mediator` scenario that triggered this redesign — shared types in `self.glp`, both modules type-check against the same definitions, imported declarations for cross-module calls.

---

*Version 1.1 — 2026-02-22*
