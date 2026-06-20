# GLP Module System — Language Specification

**Status:** Draft  
**Date:** 2026-03-11  
**Supersedes:** `archive/glp-module-system-v1-spec.md`, `archive/glp-modules-spec-future.md`

---

## 1. Design Principles

1. **Types are foundational.** The module system is designed around the moded type system. Type compatibility across module boundaries is verified structurally via type automata and subtyping.

2. **Hierarchy mirrors the file system.** Module structure maps to directory structure, following FCP conventions.

3. **Implicit lexical scoping.** A module sees all type and procedure definitions from its ancestors automatically.

4. **Self-contained modules; the linked program is the unit of type checking.** Every module declares all procedures it uses: its own, exported ones for callers, and imported ones from other modules. A module may be checked separately against its own text and its ancestor scope, using its `imported` declarations — no other module need be parsed. Soundness, however, is established on the *linked program* (the typed GLP program obtained by parameterised-type expansion, procedure instantiation, and linking; see §6 and `../type system/typed-program.md`), which is the unit of type checking; it makes no claim about a module checked outside a program. Separate checking is a mode, not the primary framing.

5. **Procedure declarations carry types.** A procedure declaration implicitly carries the transitive closure of all types it depends upon. Types are not exported separately.

6. **Structural type compatibility.** Type identity is structural (automata equivalence/subtyping), never nominal. Two independently defined types with the same structure are compatible.

7. **Design vs. implementation separation.** This spec defines language-level semantics: syntax, scoping, type checking, compatibility. Implementation mechanisms (RPC via streams, direct calls, etc.) are orthogonal.

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

A `self.glp` file may contain type definitions, procedure declarations, and procedure clauses.  Types and procedures defined in `self.glp` are visible to all modules in the subtree without qualification.  This is the natural place for shared protocol types (e.g., `AgentContent`, `Response`) and shared utility procedures.

### 2.3 Module Naming and Paths

Module names follow directory paths, using `#` as separator (same as the call operator), following FCP conventions:

- `agent` — a module at the current level
- `ui#actors` — a module in subdirectory `ui/`
- `ui#mediator` — sibling of `ui#actors`

Paths are relative to the current module's position in the hierarchy. The path resolution follows the directory structure.

### 2.4 The Root Scope

The root ancestor of all modules is `programs/self.glp`. It defines the global types (`Stream`, `Channel`, `Constant`, `Integer`, etc.) and the global procedures (`send`, `receive`, `new_channel`, `merge`, etc.). Its definitions are visible to every module by ordinary ancestor scoping (Section 3.1); the root scope is not otherwise special.

---

## 3. Scoping Rules

### 3.1 Implicit Ancestor Scoping

A module implicitly sees all definitions from every ancestor scope, from its parent directory up through the root. No import declaration is required for ancestor definitions. At project load the chain runs from the loaded directory up to `programs/`, independent of where the project root lies (see the project compilation specification).

Given:
```
project/
  self.glp           — defines AgentContent, Response, ...
  agent.glp          — sees self.glp's types
  ui/
    mediator.glp     — sees self.glp's types (grandparent)
```

Both `agent.glp` and `ui/mediator.glp` can use `AgentContent`, `Response`, etc. without any import directive.

### 3.2 Shadowing

A child module may redefine any type or procedure name defined in an ancestor. The child's definition shadows the ancestor's within the child's scope and its descendants.

Whether a shadowed type is compatible with its ancestor is not checked at the point of definition. Compatibility is checked structurally at each point of use, via subtyping.

Resolution is innermost-first: a name resolves to the nearest enclosing scope that defines it — a module's own definition takes precedence over ancestor definitions, an inner `self.glp` over an outer one, with `programs/self.glp` outermost.

### 3.3 The `-expose` Directive

A `self.glp` may contain an `-expose(M).` directive, where `M` is a module path (e.g. `lib#streams`). `M` names a module **file**, resolved relative to the directory of the `self.glp` containing the directive, within that directory's subtree (`a#b#c` → `<dir>/a/b/c.glp`). It lifts `M`'s **exported** procedures — and the types their signatures carry — into that directory's scope: they become visible to the whole subtree, unqualified, exactly as if defined in that `self.glp`.

Exposure participates in the ancestor chain at the exposing directory's level: an exposed name resolves at the depth of the `self.glp` that exposes it. Innermost-first shadowing (Section 3.2) therefore applies to exposed names like any other — a definition nearer the use site shadows an exposed one, and an exposed name shadows outer scopes.

If two modules exposed at one level contribute the same name/arity, that is a compile-time error.

### 3.4 Cross-Hierarchy References

To reference a definition from a sibling, cousin, or other non-ancestor module, use the `#` operator:

```glp
ui#actors # some_procedure(X?, Y)
```

Only `exported` procedures (Section 4) are reachable via `#`. Cross-module calls require a corresponding `imported` declaration (Section 4.3).

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

### 4.3 Imported Procedures

A module must declare every cross-module procedure it calls. The `imported` keyword declares a procedure from another module, with its full type signature:

```glp
imported procedure social#agent(Constant?, UserInStream?, NetInStream?, OutputsList?).
```

The module path uses `#` on the procedure name, matching call-site syntax. For procedures from an ancestor scope (visible via implicit scoping), no path is needed:

```glp
imported procedure merge(Stream?, Stream?, Stream).
```

The imported declaration provides everything the type checker needs to verify calls to that procedure — no parsing of the source module is required.

Types referenced in imported declarations may use `#` to reference types from other modules:

```glp
imported procedure social#agent(Constant?, social#AgentChannel?, social#OutputsList?).
```

With future parameterized types, imported declarations can instantiate type parameters:

```glp
imported procedure merge(Stream(AgentMsg)?, Stream(AgentMsg)?, Stream(AgentMsg)).
```

### 4.4 Three Procedure Kinds

Every procedure declaration is exactly one of:

| Kind | Syntax | Meaning |
|------|--------|---------|
| Private | `procedure p(...)` | Local to module and descendants |
| Exported | `exported procedure p(...)` | Reachable from outside via `#` |
| Imported | `imported procedure [path#]p(...)` | Dependency on another module |

### 4.5 No Separate Export/Import Lists

Visibility is declared at each procedure declaration site. There are no `-export([...])` or `-import([...])` lists.

- **No export list:** the `exported` keyword on each procedure declaration replaces it.
- **No import list:** `imported` declarations at each procedure replace it.

Both export and import information lives at the declaration site and cannot drift out of sync.

### 4.6 Types Are Carried by Procedures

A procedure declaration — whether `exported`, `imported`, or plain — implicitly carries the transitive closure of all types referenced in its signature.

For example:
```glp
exported procedure agent(Constant?, AgentChannel?, NetChannel?, OutputsList?).
```
implicitly carries `AgentChannel`, `AgentToUserStream`, `AgentContent`, `Response`, `FriendChannel`, `OutputsList`, etc. — every type reachable from the signature.

---

## 5. Type Checking Across Module Boundaries

### 5.1 Static Cross-Module Calls

When a module calls `M # p(X?, Y)`, the type checker:

1. Finds the local `imported procedure M#p(...)` declaration in the calling module.
2. Type-checks the call arguments against the imported declaration's types, using the standard well-typing rules (including subtyping).

The type checker does NOT need to access module `M` — the imported declaration provides all necessary type information to check the call locally. This supports separate compilation as a mode; soundness, though, is established on the linked program (§1 principle 4, §6.3), not on the module checked in isolation.

At link or load time, the system verifies that `M`'s actual `exported procedure p(...)` declaration is subtype-compatible with the caller's `imported` declaration.

### 5.2 Type Compatibility

Type compatibility is always structural. The type checker compares type automata, not names. If module A defines `Response ::= accept(Channel) ; no` and module B independently defines the same type with the same structure, they are compatible.

Subtyping (Definition 5.10 of the moded types paper) applies: a call is well-typed if the caller's argument types are subtypes of what the callee's declaration expects (with appropriate variance at mode inversion points).

### 5.3 Shared Types via Ancestor Scoping

The primary mechanism for type sharing is ancestor scoping. Common types are defined at the appropriate level in the hierarchy — the lowest common ancestor of all modules that need them.

```
project/
  self.glp           — AgentContent, Response, Channel types
  agent.glp          — uses AgentContent (from self.glp)
  mediator.glp       — uses AgentContent (from self.glp)
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

- **Single file:** type-checks against its own `imported` declarations. No access to other modules needed.
- **Directory:** full type checking within the directory, interface-level across boundaries.
- **Whole project:** global type checking, cross-module inlining, dead code elimination, type-driven specialization.

### 6.2 Type-Driven Optimization

When the compiler sees both sides of a typed channel, it can:

- Verify protocol compatibility statically, eliminating runtime checks.
- Specialize message dispatch based on known message types.
- Inline cross-module calls when both caller and callee are in scope.
- Eliminate unreachable alternatives based on subtyping constraints.

### 6.3 Separate Compilation

Separate compilation is a *mode*: a module is compiled against its own `imported` procedure declarations, which suffice to check the module's text locally — no access to other modules is needed. This separate check is not, on its own, a soundness guarantee. Soundness is established on the **linked program** (the unit of type checking; §1 principle 4, `../type system/typed-program.md`): the modules are expanded, their parameterised procedures instantiated, and linked, and the resulting typed GLP program is checked well-typed. A parameterised procedure, in particular, is checked only within a program that instantiates it; checked in isolation with a free type parameter it is not a program and is not type-checked.

The compiled module records its `imported` declarations, enabling compatibility verification at load time.

---

## 7. Dynamic Loading

### 7.1 Trusted Dynamic Loading

GLP supports dynamic module loading. Code is loaded from trusted sources (signed by agents the loader trusts). Trust guarantees intent and provenance, not type compatibility.

### 7.2 Load-Time Verification

When a module is loaded dynamically, the loader verifies that the actual module's `exported procedure` declarations are **subtype-compatible** with the caller's `imported` declarations.

For each imported procedure, the actual module's exported declaration must accept at least the inputs the caller may send (contravariance on inputs) and produce at most the outputs the caller expects (covariance on outputs). This is exactly the subtyping relation on procedure types induced by the moded type system.

The `imported` declaration records the caller's expectations. The `exported` declaration records the callee's guarantees. The loader checks that guarantees meet expectations.

### 7.3 Type Automata as Runtime Artifacts

For dynamic loading, type automata (or their serialized representation) must be available at runtime — not just at compile time. A compiled module carries its type automata alongside its code, enabling the load-time compatibility check.

### 7.4 Runtime Boundary Checking

The type-soundness guarantee holds for a well-typed program **and** a well-typed initial goal. Static linking (Section 6) establishes both at compile time, since every producer is part of the checked program. The load-time check of dynamic linking (Section 7.2) establishes that *declared interfaces* are compatible — it compares declarations, not the terms actually sent. When a producer is not itself type-checked (foreign bytecode, network input, REPL/boot terms), the conformance of the consumer's inputs is therefore not established. Runtime boundary checking closes this gap.

**The forwarder.** For a type `T`, a type-specific *forwarding process* `forward(A?, In?, Out, Err)` — where `A` is `T`'s type automaton (Section 7.3) — copies stream `In` to stream `Out`, checking each instantiated part of every consumed term against `A`. Checking is **incremental**: conforming parts are forwarded immediately; the process **suspends** at uninstantiated positions and resumes as they are instantiated (so partial terms produce no false errors). On the **first violation** it emits `type_error(Culprit, Expected)` on `Err` and stops forwarding. The forwarder is an ordinary GLP process — the check is itself written in GLP.

**Placement.** A guard is placed on the **consumer side** of each boundary stream of a dynamically activated module, in each **direction whose producer is not type-checked**. Every term a guard forwards conforms to the stream's declared type, so a well-typed module all of whose boundary streams are guarded satisfies the well-typed-input hypothesis at runtime, and its outputs conform.

**Activation mode.** Dynamic activation (the `serve`/`activate` machinery of Section 7.1) takes a **guarded** or **unguarded** mode: in guarded mode the runtime interposes a forwarder on each consumed boundary stream of the activated module; in unguarded mode it does not. This is a mechanism — *which* peers are trusted, and hence whether a given activation is guarded, is the caller's policy. On well-typed traffic the two modes behave identically (the forwarder is transparent); they differ only when an ill-typed term arrives.

---

## 8. Module Declaration

### 8.1 Syntax

A module file may optionally begin with a module declaration:

```glp
-module(name).
```

If omitted, the module name defaults to the filename without the `.glp` extension.

### 8.2 System Mode

`-mode(system)` admits a module to the language-primitive layer — it is what permits naming reserved constants (the `_`-prefixed kernel predicates and reserved functors). The directive is confined to that layer: only the root `programs/self.glp` and the modules under `programs/system/` declare it, and no other module names a reserved constant. An application module declares neither, reaching runtime functionality by calling the system predicates that `programs/system/` exports, like any cross-module procedure. A module that names a reserved constant without `-mode(system)` fails to load.

This is now specified in the TGLP paper, Appendix "GLP Language Primitives and the Root `self.glp`" ("Admission to the Primitive Layer"), which is the authoritative source.

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
exported procedure agent(Constant?, UserInStream?, NetInStream?, OutputsList?).

%% Import mediator's procedure for cross-module calls
imported procedure social#mediator#ui_mediator(Constant?, AgentChannel?, UserChannel?, PendingList?, Constant?).

agent(Id, [msg('_user', Id1, connect(Target))|UserIn], NetIn, Outs) :-
    %% ... uses AgentContent, Response from self.glp ...
```

**mediator.glp:**
```glp
exported procedure ui_mediator(Constant?, AgentChannel?, UserChannel?, PendingList?, Constant?).

%% Import merge from ancestor scope (no path needed)
imported procedure merge(Stream?, Stream?, Stream).

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    %% ... uses AgentContent, Response from self.glp — same definition ...
```

### 9.2 Cross-Module Calls

A boot module that calls into the social hierarchy:

```glp
%% Import the procedures we call
imported procedure social#agent#start(Constant?, social#Channel).
imported procedure social#mediator#connect(Constant?, social#Channel?).

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
| Exports | `-export([proc/arity, ...])` list | `exported procedure` at declaration site |
| Imports | `-import([module, ...])` list | `imported procedure path#name(...)` at declaration site |
| Type checking | Not addressed | Local per-module check; linked program is the unit of soundness |
| Separate compilation | Not addressed | Supported as a mode via imported/exported declarations |
| Dynamic verification | Not addressed | Subtype compatibility: imported vs exported |
| Compilation scope | Single module | Flexible: file, directory, project |

---

## 11. Open Questions

1. **Versioning and backward compatibility:** Compiled code is timestamped and signed. Only compatible versions can interact. When a type changes, how do we express which older versions remain compatible? This may require backward compatibility declarations — a module could declare that its current interface is backward-compatible with a specific prior version. Details TBD.

2. **Parameterized types:** The renamed-procedure workaround (Section 14 of the typed GLP manual) is a separate concern. Parameterized types would interact with the module system (e.g., a module could export a parameterized type), but the designs are independent and can proceed in parallel.

---

*Version 1.2 — 2026-03-11*
