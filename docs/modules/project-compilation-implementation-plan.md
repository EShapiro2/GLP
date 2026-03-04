# Project Compilation (Static Linking) — Implementation Plan

**Date:** 2026-03-03  
**Spec:** `docs/modules/glp-project-compilation-spec.md`  
**Paper:** Moded-Types Section 6.2 (Static Linking)

---

## 1. Goal

Implement static linking: given a project root directory, produce a single flat Module AST where all inter-module calls are resolved to renamed local procedures. The output feeds into the existing compilation pipeline unchanged.

---

## 2. Existing Infrastructure

**Already implemented:**

- `module_hierarchy.dart` — `discoverSelfChain()` walks the directory tree and builds ancestor type scope chains.
- `cssg_modules_test.dart` — parses modules with ancestor scoping, type-checks each independently.
- Parser — handles `exported procedure`, `imported procedure`, `RemoteGoal` (`M # p(...)`), module declarations.
- AST — `Module`, `Procedure`, `Goal`, `RemoteGoal`, `ProcDecl` with `exported`/`imported` flags.

**Not yet implemented:**

- Directory tree discovery (collecting all `.glp` files)
- Procedure renaming (`p/n` → `M:p/n`)
- Call resolution (local calls → `M:p`, cross-module `M' # p` → `M':p`, prelude → unchanged)
- Entry point alias generation
- Merging all modules into a single flat AST

---

## 3. Design

### 3.1 The linker is an AST-to-AST transformation

Input: a project root directory path.  
Output: a single `Module` AST (or `Program`) with all procedures renamed and calls resolved.

The transformation happens after parsing and type checking, before partial evaluation and codegen. The output is a normal AST — the rest of the pipeline doesn't know it came from multiple modules.

### 3.2 Renaming scheme

Each procedure `p/n` in module `M` becomes `M:p/n`.  The module name comes from `-module(M).` or the filename without `.glp`.  The colon is just a character in the functor string — no lexer/parser changes needed since this is generated, not parsed.

### 3.3 Prelude detection

Calls to prelude procedures must NOT be renamed. The linker needs a set of known prelude names. This can be derived from the stdlib files or hardcoded from the prelude environment.

Alternatively: a call is a prelude call if it doesn't match any procedure defined in any module. This is safer — no hardcoded list to maintain.

### 3.4 What about `_select/1`?

`_select/1` is generated during compilation, not during linking. Static linking produces a flat program — no modules, no dispatch tables. `_select/1` generation is skipped for statically linked programs (no `exported` declarations survive linking).

---

## 4. Implementation Steps

### Step 1: Project discovery

**New file:** `lib/compiler/project_linker.dart`

**Function:** `discoverProject(String rootDir)` → `List<DiscoveredModule>`

Walk the project directory tree. For each `.glp` file (excluding `self.glp`), create a `DiscoveredModule` record containing:
- File path
- Module name (from `-module(name).` or filename)
- Parsed `Module` AST
- Ancestor type scope (via `discoverSelfChain` + scope assembly)

`self.glp` files are parsed for their type definitions but produce no `DiscoveredModule` (they have no procedures).

### Step 2: Type checking

**Function:** `typeCheckProject(List<DiscoveredModule> modules)` → validated modules

Each module is type-checked independently against its ancestor scope, exactly as `cssg_modules_test.dart` does today. If any module fails, report errors and abort.

### Step 3: Procedure renaming and call resolution

**Function:** `linkProject(List<DiscoveredModule> modules, String topModuleName)` → `Module`

For each module `M`:

1. **Rename procedures:** For each `Procedure` in `M`, change `name` from `p` to `M:p`.

2. **Resolve calls in clause bodies:** For each `Goal` in every clause body:
   - If `RemoteGoal` (`M' # p(...)`): replace with `Goal('M':p', args)`. Remove the `RemoteGoal` wrapper.
   - If local call to `p` that matches a procedure in `M`: replace with `Goal('M:p', args)`.
   - If local call to `p` that matches no module procedure (prelude/system): leave unchanged.

3. **Resolve calls in clause heads:** The head functor `p` becomes `M:p` (matching the renamed procedure).

4. **Collect all procedures** from all modules into a single list.

5. **Generate entry point aliases** for all modules' exported procedures:
   - For each exported `p/n` in any module `M`, add: `p(A1,...,An) :- M:p(A1?,...,An?).`
   - This allows code loaded on top of the linked program (e.g., madGLP boot, REPL goals) to call any exported procedure by its original name.
   - Report a conflict if two modules export the same name/arity.

6. **Collect all type definitions** from all `self.glp` files and all modules, deduplicated (inner scopes shadow outer).

7. **Drop `exported`/`imported` declarations** — they've served their purpose.

8. **Return** a single `Module` AST.

### Step 4: Integration with REPL

**Modification:** Add a REPL command or compiler option to load a project directory instead of a single file.

When the REPL loads a project directory:
1. Discover → Type check → Link → produce flat AST
2. Feed the flat AST into the existing pipeline (PE → analyze → codegen)
3. The result is a single `BytecodeProgram` with all modules' procedures

### Step 5: Tests

**New file:** `test/compiler/project_linker_test.dart`

Tests:
1. **Discovery:** Given `cssg_modules/`, discover 4 modules (agent, mediator, actors, boot) + self.glp types.
2. **Renaming:** After linking, verify `agent:merge/3` exists, `merge/3` does not.
3. **Call resolution:** In boot's linked clauses, verify `agent # agent(...)` became `agent:agent(...)`.
4. **Prelude preservation:** Verify prelude calls (`send`, `receive`, `:=`, etc.) remain unprefixed.
5. **Entry points:** Verify `play1/0` alias exists pointing to `boot:play1/0`.
6. **No name conflicts:** Both `agent.glp` and `boot.glp` define `merge/3` — after linking, `agent:merge/3` and `boot:merge/3` are distinct.
7. **End-to-end:** Link `cssg_modules/`, compile, run `fplay1`, verify correct output (matching the dynamic dispatch test output).

---

## 5. File Summary

| File | Action |
|------|--------|
| `lib/compiler/project_linker.dart` | New — discovery, type checking, renaming, call resolution |
| `bin/glp_repl.dart` | Modify — add project directory loading |
| `test/compiler/project_linker_test.dart` | New — all tests above |

---

## 6. Validation

After project compilation works on CSSG, create modular versions of the remaining multi-file applications:

- `social_graph_simulated_ui/` → `social_graph_simulated_ui_modules/`
- `social_graph/` (selected plays) → `social_graph_modules/`

Each modular version gets tested through both:
- **Static linking:** project compilation → single flat program → run plays
- **Dynamic linking:** activate modules → GLP channels → run plays

Both paths must produce identical output.

---

*Version 1.0 — 2026-03-03*
