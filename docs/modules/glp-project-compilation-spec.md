# GLP Project Compilation — Specification

**Status:** Draft  
**Date:** 2026-03-11  
**Extends:** `glp-module-system-spec.md` Section 6

---

## 1. Overview

Project compilation transforms a hierarchy of modules into a single flat program. All inter-module calls become local calls. The output is indistinguishable from a hand-written single-file program.

---

## 2. Input

A project root directory containing:
- One or more `.glp` module files
- Zero or more `self.glp` files (defining shared types and procedures for their subtree)
- Zero or more subdirectories (recursively)

---

## 3. Process

### 3.1 Discovery

Walk the project directory tree. Collect every `.glp` file. Additionally collect the `self.glp` of every ancestor directory of the project root, up to and including `programs/`; ancestor directories contribute only their `self.glp`, never their other modules. Parse each into a Module AST. Build the ancestor scope chain for each module (per `glp-module-system-spec.md` Section 3).

`self.glp` files contribute both type definitions and procedure definitions to the ancestor scope.  Their procedures are compiled to bytecode and available to all modules in the subtree without qualification (in source), just like their types.  This holds equally for ancestor `self.glp` files above the project root.

**`-expose` directives.**  If a collected `self.glp` contains an `-expose(M).` directive, additionally collect module `M` — and, transitively, any modules `M` itself exposes.  `M` names a module **file**, resolved relative to the directory of the exposing `self.glp`, within that directory's subtree (`a#b#c` → `<self.glp dir>/a/b/c.glp`).  This subtree may lie outside the *loaded project* subtree — e.g. when the exposing `self.glp` is an ancestor above the load point — so discovery must collect the file by path, not rely on it being among the project's modules.  Only `M`'s **exported** procedures (and the types their signatures carry) join the exposing directory's scope; `M`'s non-exported procedures and its sibling modules are not collected.

### 3.2 Procedure Renaming

Every procedure in every `.glp` file (including `self.glp` files) is prefixed with its module path:

| Module file | Procedure | Renamed to |
|---|---|---|
| `self.glp` | `helper/2` | `cssg:helper/2` |
| `agent.glp` | `agent/4` | `agent:agent/4` |
| `agent.glp` | `merge/3` | `agent:merge/3` |
| `ui/mediator.glp` | `ui_mediator/5` | `mediator:ui_mediator/5` |
| `ui/mediator.glp` | `send_agent/3` | `mediator:send_agent/3` |
| `ui/actors.glp` | `alice1/1` | `actors:alice1/1` |
| `boot.glp` | `tee/3` | `boot:tee/3` |
| `boot.glp` | `play1/0` | `boot:play1/0` |

`self.glp` procedures are renamed like any other module's procedures.  This prevents collisions when multiple ancestor scopes define procedures with the same name and arity.  If an inner `self.glp` defines a procedure with the same name and arity as an outer `self.glp`, both receive distinct prefixes based on their module path.

Intermediate ancestor `self.glp` files above the project root are prefixed by their directory name under the same rule. The internal representation of the root scope (`programs/self.glp`) is an implementation choice; whatever the mechanism, resolution and shadowing must behave per the module-system specification §3.1–3.2.

The prefix is the module name (from `-module(name)` or filename), not the full path. If two modules at different levels have the same name, the full relative path is used (e.g., `ui/mediator:proc`).

### 3.3 Call Resolution

Every goal in every clause body is resolved:

**Local calls** — a call to `merge(X, Y, Z)` inside `agent.glp` becomes `agent:merge(X, Y, Z)`.

**Cross-module calls** — a call to `agent # agent(alice, ...)` inside `boot.glp` becomes `agent:agent(alice, ...)`.

**Ancestor self.glp calls** — if no local procedure matches, the linker walks the ancestor `self.glp` chain, which extends beyond the project root up to `programs/`.  A call matching a procedure in an ancestor `self.glp` is resolved to its renamed form.

**Exposed procedure calls** — a call matching a procedure exposed (via `-expose`) into an enclosing `self.glp`'s scope resolves to that procedure's renamed form in its defining module, exactly as if it were defined in the exposing `self.glp` (the paper's Static Linking sentence).  Exposed names sit at the depth of the exposing `self.glp` in the ancestor chain, so innermost-first shadowing (module-system spec §3.2) applies: a local or nearer-ancestor definition takes precedence over an exposed one.

**Root `self.glp` calls** — definitions in the root `programs/self.glp`, single- or multi-clause, resolve for every module per the ancestor scope chain (module-system spec §3.1), subject to innermost-first shadowing. Partial-evaluation unfolding of its single-unit-clause procedures (`=`, `send`, `receive`, `new_channel`) remains an optimisation; it is not the resolution mechanism.

### 3.4 Entry Points

A procedure of a project is externally accessible only if it is exported at the project's root (TGLP manual, Modules; the paper's Static Linking step five generates unprefixed aliases for the root's exported procedures only).

Accordingly, an unprefixed alias is generated for each **exported** procedure of a **root-level** module — a module whose nearest enclosing `self.glp` directory is the loaded project root itself, equivalently a module not contained in any descendant `self.glp` subtree below the root.  If root-level `agent.glp` exports `agent/6`, the output contains both `agent:agent/6` (the renamed procedure) and `agent/6` (an alias that calls it).  If root-level `boot.glp` exports `fplay1/0`, the output contains both `boot:fplay1/0` and `fplay1/0`.

Modules under a **descendant** `self.glp` root — a nested sub-project (e.g. `secure/`, `village/`) — are not part of the loaded root's public surface.  Their procedures keep prefixed names only and receive **no** unprefixed alias, even when exported; they become entry points only when that nested directory is itself the loaded root.  (Same-name modules at different levels are disambiguated by relative path per §3.2, e.g. `boot:` vs `secure/boot:`.)

This is necessary because code loaded on top of a linked project (madGLP boot procedures, REPL goals) calls exported procedures by their original name: it must reach the loaded root's public procedures, but not a nested sub-project's internals.

If two root-level modules export procedures with the same name and arity, a conflict is reported.  The same-name-export conflict rule applies within the aliased (root-level exported) set only.  There is no "top module" and no backwards-compatibility rule that aliases unexported procedures: a project that wants its plays callable by name declares them `exported` in its root-level module.

Code loaded on top of a linked project (REPL goals, madGLP boot procedures) is resolved in the scope of the project root, with the same ancestor `self.glp` chain.

### 3.5 Type Checking

Each module is type-checked independently with its ancestor scope, exactly as today. The renaming step happens after type checking — it is a purely syntactic transformation on well-typed modules.

The exported signatures of `-expose`d modules — and the types those signatures carry — are merged into the exposing directory's type scope, so modules in the subtree type-check against the exposed procedures as if they were declared in the exposing `self.glp`.  **Collision:** if two modules exposed at one level contribute the same name/arity, the linker reports a compile-time error naming both modules.

### 3.6 Imported/Exported Declarations After Linking

After linking, `imported` and `exported` declarations are no longer needed — they have served their purpose during type checking. They are dropped from the output.

### 3.7 Output

A single Module AST containing:
- All type definitions from all `self.glp` files and all modules (deduplicated by name, inner scopes shadow outer)
- All procedures from all `.glp` files (including `self.glp`), all renamed
- Entry point aliases for the exported procedures of root-level modules only (§3.4)

This AST is fed into the existing compilation pipeline (partial evaluation → codegen).

---

## 4. Example

### Input

```
cssg_modules/
  self.glp          — defines Response, AgentContent, ...
  agent.glp         — exported agent/4, private merge/3, lookup_send/4, ...
  boot.glp          — imports agent#agent, mediator#ui_mediator, actors#alice1, ...
  ui/
    mediator.glp    — exported ui_mediator/5, private send_agent/3, ...
    actors.glp      — exported alice1/1, bob1/1, ...
```

### Output (flat program)

```glp
%% Types from self.glp
Response ::= accept(FriendChannel) ; no.
AgentContent ::= ...
%% ... all types ...

%% From agent.glp
procedure agent:merge(Stream?, Stream?, Stream).
agent:merge([X|Xs], Ys, [X?|Zs?]) :- agent:merge(Ys?, Xs?, Zs).
%% ...

procedure agent:agent(Constant?, UserInStream?, NetInStream?, OutputsList?).
agent:agent(Id, [msg(...)|UserIn], NetIn, Outs) :-
    ... agent:lookup_send(...) ... agent:merge(...) ...

%% From ui/mediator.glp
procedure mediator:ui_mediator(Constant?, AgentChannel?, UserChannel?, PendingList?, Constant?).
mediator:ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    ... mediator:send_agent(...) ...

%% From ui/actors.glp
procedure actors:alice1(ActorChannel?).
%% ...

%% From boot.glp
procedure boot:tee(Stream?, Stream, Stream).
%% ...

procedure boot:play1.
boot:play1 :-
    boot:network3(...),
    actors:alice1(...),
    boot:tee(...),
    agent:agent(alice, ...),
    mediator:ui_mediator(alice, ...),
    boot:sink(...), ...

%% Entry point aliases (exported procedures of root-level modules only; §3.4).
%% Here boot.glp declares `exported procedure play1.` etc. so its plays alias.
agent :- agent:agent.      %% from agent.glp
ui_mediator :- mediator:ui_mediator.  %% from ui/mediator.glp
alice1 :- actors:alice1.   %% from ui/actors.glp
bob1 :- actors:bob1.
%% ... all other exported actors ...
play1 :- boot:play1.       %% from boot.glp
play2 :- boot:play2.
%% ...
```

---

## 5. Scope

This spec covers whole-project compilation only. Separate compilation with runtime inter-module calls is a separate concern, not specified here.

A single-module file load is the degenerate case of project compilation: a project of one module, with the same ancestor `self.glp` chain from the file's directory up to `programs/`.
