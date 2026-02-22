# GLP Module System — Language Specification

**Status:** Draft
**Date:** 2026-02-21
**Supersedes:** `archive/glp-module-system-v1-spec.md`, `archive/glp-modules-spec-future.md`

---

## 1. Design Principles

1. **Types are foundational.** The module system is designed around the moded type system. Type compatibility across module boundaries is verified structurally via type automata and subtyping.

2. **Hierarchy mirrors the file system.** Module structure maps to directory structure, following FCP conventions.

3. **Implicit lexical scoping.** A module sees all type and procedure definitions from its ancestors. No import declarations needed.

4. **Procedure declarations carry types.** A procedure declaration implicitly carries the transitive closure of all types it depends upon. Types are not exported separately.

5. **Structural type compatibility.** Type identity is structural (automata equivalence/subtyping), never nominal. Two independently defined types with the same structure are compatible.

6. **Design vs. implementation separation.** This spec defines language-level semantics: syntax, scoping, type checking, compatibility. Implementation mechanisms (RPC via streams, direct calls, etc.) are orthogonal.

---

## 2. Module Hierarchy

### 2.1 Directory-Based Structure

A GLP project is a directory tree. Each `.glp` file is a module. Each directory is a scope. The hierarchy mirrors the file system, following FCP conventions.

### 2.2 The `self.glp` File

Following FCP's `self.cp` convention, each directory may contain a `self.glp` file that defines the directory's own scope: type definitions, procedure declarations, and exports that apply to the directory as a module.

```
project/
  self.glp               — project-level types and declarations
  agent.glp              — module: sees self.glp definitions
  mediator.glp           — module: sees self.glp definitions
  ui/
    self.glp             — ui-level types and declarations
    actors.glp           — sees ui/self.glp and project/self.glp
    mediator.glp         — sees ui/self.glp and project/self.glp
```

A `self.glp` file may contain only type definitions and no procedures. This is the natural place for shared protocol types (e.g., `AgentContent`, `Response`).

### 2.3 Module Naming and Paths

Module names follow directory paths, using `#` as separator (same as the call operator), following FCP conventions:

- `agent` — a module at the current level
- `ui#actors` — a module in subdirectory `ui/`
- `ui#mediator` — sibling of `ui#actors`

Paths are relative to the current module's position in the hierarchy. The path resolution follows the directory structure.

### 2.4 Prelude

The prelude is the root ancestor of all modules. It defines global types (`Stream`, `Channel`, `Constant`, `Integer`, etc.) and global procedures (`send`, `receive`, `new_channel`, `merge`, etc.). All modules see prelude definitions without qualification.

---

## 3. Scoping Rules

### 3.1 Implicit Ancestor Scoping

A module implicitly sees all definitions from every ancestor scope, from its parent directory up through the root. No import declaration is required.

Given:
```
project/
  protocol.glp       — defines AgentContent, Response, ...
  agent.glp          — sees protocol.glp's types
  ui/
    mediator.glp     — sees protocol.glp's types (grandparent)
```

Both `agent.glp` and `ui/mediator.glp` can use `AgentContent`, `Response`, etc. without any import directive.

### 3.2 Shadowing

A child module may redefine any type or procedure name defined in an ancestor. The child's definition shadows the ancestor's within the child's scope and its descendants.

Whether a shadowed type is compatible with its ancestor is not checked at the point of definition. Compatibility is checked structurally at each point of use, via subtyping.

### 3.3 Cross-Hierarchy References

To reference a definition from a sibling, cousin, or other non-ancestor module, use the `#` operator:

```glp
ui#actors # some_procedure(X?, Y)
```

Only `exported` procedures (Section 4) are reachable via `#`.

---

## 4. Procedure Declarations and Visibility

### 4.1 Private Procedures

By default, procedures are private — visible only within the defining module and its descendants (via ancestor scoping):

```glp
procedure helper(Integer?, Integer).
```

### 4.2 Exported Procedures

The `exported` keyword makes a procedure reachable via `#` from outside the module's subtree:

```glp
exported procedure factorial(Integer?, Integer).
```

### 4.3 No Separate Export/Import Lists

Visibility is declared at the procedure declaration site. There is no `-export([...])` list and no `-import([...])` list.

- **No export list:** the `exported` keyword on each procedure declaration replaces it. Visibility cannot drift out of sync with the declaration.
- **No import list:** the `#` operator names the source module explicitly at each call site. Ancestor scoping handles the rest.

### 4.4 Types Are Carried by Procedures

A procedure declaration implicitly carries the transitive closure of all types referenced in its signature. When a client module references an exported procedure via `#`, it gains access to the procedure's type dependencies.

For example:
```glp
exported procedure agent(Constant?, AgentChannel?, NetChannel?, OutputsList?).
```
implicitly carries `AgentChannel`, `AgentToUserStream`, `AgentContent`, `Response`, `FriendChannel`, `OutputsList`, etc. — every type reachable from the signature.

---

## 5. Type Checking Across Module Boundaries

### 5.1 Static Cross-Module Calls

When a module calls `M # p(X?, Y)`, the type checker:

1. Resolves `M` to a module in the hierarchy.
2. Finds the `exported procedure p(...)` declaration in `M`.
3. Type-checks the call arguments against `p`'s declared types, using the standard well-typing rules (including subtyping).

### 5.2 Type Compatibility

Type compatibility is always structural. The type checker compares type automata, not names. If module A defines `Response ::= accept(Channel) ; no` and module B independently defines the same type with the same structure, they are compatible.

Subtyping (Definition 5.10 of the moded types paper) applies: a call is well-typed if the caller's argument types are subtypes of what the callee's declaration expects (with appropriate variance at mode inversion points).

### 5.3 Shared Types via Ancestor Scoping

The primary mechanism for type sharing is ancestor scoping. Common types are defined at the appropriate level in the hierarchy — the lowest common ancestor of all modules that need them.

```
project/
  protocol.glp       — AgentContent, Response, Channel types
  agent.glp          — uses AgentContent (from parent scope)
  mediator.glp       — uses AgentContent (from parent scope)
```

Both modules see the same definition. The copy-paste problem is eliminated.

---

## 6. Compilation

### 6.1 Compilation Scope

The compilation unit is flexible. A compiler may process:

- A single file
- A directory (all `.glp` files in it)
- An entire project (the full directory tree)

Broader compilation scope enables more optimization:

- **Single file:** type-checks against declared interfaces of dependencies.
- **Directory:** full type checking within the directory, interface-level across boundaries.
- **Whole project:** global type checking, cross-module inlining, dead code elimination, type-driven specialization.

### 6.2 Type-Driven Optimization

When the compiler sees both sides of a typed channel, it can:

- Verify protocol compatibility statically, eliminating runtime checks.
- Specialize message dispatch based on known message types.
- Inline cross-module calls when both caller and callee are in scope.
- Eliminate unreachable alternatives based on subtyping constraints.

### 6.3 Separate Compilation

For separate compilation, a module is compiled against the **interface** of its dependencies: the set of `exported procedure` declarations and their transitive type dependencies. The interface is sufficient for type checking the client.

The compiled module records the interfaces it was compiled against, enabling compatibility verification at load time.

---

## 7. Dynamic Loading

### 7.1 Trusted Dynamic Loading

GLP supports dynamic module loading. Code is loaded from trusted sources (signed by agents the loader trusts). Trust guarantees intent and provenance, not type compatibility.

### 7.2 Load-Time Verification

When a module is loaded dynamically, the loader verifies that the actual module's exported procedure declarations are **subtype-compatible** with the interfaces the client was compiled against.

This means: for each procedure the client calls, the actual module's procedure declaration must accept at least the inputs the client may send (contravariance on inputs) and produce at most the outputs the client expects (covariance on outputs). This is exactly the subtyping relation on procedure types induced by the moded type system.

### 7.3 Type Automata as Runtime Artifacts

For dynamic loading, type automata (or their serialized representation) must be available at runtime — not just at compile time. A compiled module carries its type automata alongside its code, enabling the load-time compatibility check.

---

## 8. Module Declaration

### 8.1 Syntax

A module file may optionally begin with a module declaration:

```glp
-module(name).
```

If omitted, the module name defaults to the filename without the `.glp` extension.

### 8.2 System Mode

Files using reserved constants must declare:

```glp
-mode(system).
```

This is orthogonal to the module system and retained as-is.

---

## 9. Examples

### 9.1 Shared Protocol Types

```
social/
  self.glp
  agent.glp
  mediator.glp
```

**self.glp:**
```glp
Response ::= accept(FriendChannel) ; no.
AgentContent ::= befriend(Constant, Response?)
               ; connected(Constant)
               ; rejected.
AgentChannel ::= ch(AgentToUserStream, MediatorToAgentStream?).
%% ... remaining type definitions ...
```

**agent.glp:**
```glp
-mode(system).

exported procedure agent(Constant?, UserInStream?, NetInStream?, OutputsList?).

agent(Id, [msg('_user', Id1, connect(Target))|UserIn], NetIn, Outs) :-
    %% ... uses AgentContent, Response from self.glp ...
```

**mediator.glp:**
```glp
-mode(system).

exported procedure ui_mediator(Constant?, AgentChannel?, UserChannel?, PendingList?, Constant?).

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    %% ... uses AgentContent, Response from self.glp — same definition ...
```

### 9.2 Cross-Module Calls

```glp
boot(Id) :-
    social#agent # start(Id?, Ch),
    social#mediator # connect(Id?, Ch?).
```

### 9.3 Shadowing

A child module may specialize a parent's type:

**parent/self.glp:**
```glp
Response ::= accept(Channel) ; no.
```

**parent/child/self.glp:**
```glp
Response ::= accept(FriendChannel) ; no ; maybe(Timeout).
```

The child's `Response` shadows the parent's. Calls within the child and its descendants use the child's definition. Calls from outside use whichever definition is in scope at the call site. Compatibility is checked structurally.

---

## 10. Relationship to Previous Designs

The archived specs (`glp-module-system-v1-spec.md`, `glp-modules-spec-future.md`, `module-implementation-plan.md`) designed the module system around FCP's stream-based RPC model without considering the type system. This spec redesigns modules with moded types as the foundation.

Key differences:

| Aspect | Previous Design | This Design |
|--------|----------------|-------------|
| Foundation | FCP stream-based RPC | Moded type system |
| Type sharing | Not addressed | Ancestor scoping, structural compatibility |
| Interface | `-export([proc/arity, ...])` list | `exported procedure` at declaration site |
| Dependencies | `-import([module, ...])` list | Implicit (ancestor scoping) + explicit (`#`) |
| Dynamic verification | Not addressed | Subtype compatibility of type automata |
| Compilation scope | Single module | Flexible: file, directory, project |

---

## 11. Open Questions

1. **Versioning and backward compatibility:** Compiled code is timestamped and signed. Only compatible versions can interact. When a type changes, how do we express which older versions remain compatible? This may require backward compatibility declarations — a module could declare that its current interface is backward-compatible with a specific prior version. Details TBD.

2. **Parameterized types:** The renamed-procedure workaround (Section 14 of the typed GLP manual) is a separate concern. Parameterized types would interact with the module system (e.g., a module could export a parameterized type), but the designs are independent and can proceed in parallel.

---

*Version 1.0 — 2026-02-21*
