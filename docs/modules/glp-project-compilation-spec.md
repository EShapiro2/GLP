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

Walk the project directory tree. Collect every `.glp` file. Parse each into a Module AST. Build the ancestor scope chain for each module (per `glp-module-system-spec.md` Section 3).

`self.glp` files contribute both type definitions and procedure definitions to the ancestor scope.  Their procedures are compiled to bytecode and available to all modules in the subtree without qualification, just like their types.  If a `self.glp` file contains procedure clauses, they are compiled but not renamed (see §3.2).

### 3.2 Procedure Renaming

Every procedure in every module is prefixed with its module path:

| Module file | Procedure | Renamed to |
|---|---|---|
| `agent.glp` | `agent/4` | `agent:agent/4` |
| `agent.glp` | `merge/3` | `agent:merge/3` |
| `ui/mediator.glp` | `ui_mediator/5` | `mediator:ui_mediator/5` |
| `ui/mediator.glp` | `send_agent/3` | `mediator:send_agent/3` |
| `ui/actors.glp` | `alice1/1` | `actors:alice1/1` |
| `boot.glp` | `tee/3` | `boot:tee/3` |
| `boot.glp` | `play1/0` | `boot:play1/0` |

Procedures defined in `self.glp` files are not renamed — they act as the local prelude for their subtree.  If an inner `self.glp` defines a procedure with the same name and arity as an outer `self.glp`, the inner definition shadows the outer.

The prefix is the module name (from `-module(name)` or filename), not the full path. If two modules at different levels have the same name, the full relative path is used (e.g., `ui/mediator:proc`).

### 3.3 Call Resolution

Every goal in every clause body is resolved:

**Local calls** — a call to `merge(X, Y, Z)` inside `agent.glp` becomes `agent:merge(X, Y, Z)`.

**Cross-module calls** — a call to `agent # agent(alice, ...)` inside `boot.glp` becomes `agent:agent(alice, ...)`.

**Ancestor self.glp calls** — calls to procedures defined in any ancestor `self.glp` file (including the root prelude) are left unprefixed.  A local call is resolved as a `self.glp` call if no local procedure with that name and arity exists in the module, but one exists in an ancestor `self.glp`.

### 3.4 Entry Points

Every exported procedure in every module receives an unprefixed alias.  If `agent.glp` exports `agent/4`, the output contains both `agent:agent/4` (the renamed procedure) and `agent/4` (an alias that calls it).  If `boot.glp` exports `play1/0`, the output contains both `boot:play1/0` and `play1/0`.

This is necessary because code loaded on top of a linked project (e.g., madGLP boot procedures, REPL goals) must be able to call any exported procedure by its original name, not only the top module's exports.

If two modules export procedures with the same name and arity, a conflict is reported.  If no module has exported procedures, all top-level module procedures get unprefixed aliases (backwards compatibility).

### 3.5 Type Checking

Each module is type-checked independently with its ancestor scope, exactly as today. The renaming step happens after type checking — it is a purely syntactic transformation on well-typed modules.

### 3.6 Imported/Exported Declarations After Linking

After linking, `imported` and `exported` declarations are no longer needed — they have served their purpose during type checking. They are dropped from the output.

### 3.7 Output

A single Module AST containing:
- All type definitions from all `self.glp` files and all modules (deduplicated by name, inner scopes shadow outer)
- All procedures from all modules (renamed) and all `self.glp` files (not renamed)
- Entry point aliases for all modules' exported procedures

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

%% Entry point aliases (all exported procedures)
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
