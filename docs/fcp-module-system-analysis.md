# FCP Module System Analysis

**Purpose:** Analysis of the FCP/Logix module system for informing GLP module system design
**Date:** 2025-12-11
**Source:** `/tmp/FCP/Savannah/Logix/`

## Overview

The FCP module system provides a hierarchical service architecture where modules communicate via Remote Procedure Calls (RPC). The key insight is that **each module runs as a monitor process** that accepts RPC messages on a stream and dispatches them to local procedures.

The implementation spans three layers:
1. **Compiler support** - Transforms RPC syntax into distribute/transmit messages
2. **Logix runtime** - Domain/hierarchy servers manage module loading and message routing
3. **FCP AM (emulator)** - Provides vector operations for efficient message distribution

---

## 1. RPC Syntax and Compiler Transformation

### Source Syntax

```prolog
% Remote Procedure Call to named module
ModuleName # Goal

% Distributed Processor Call (linked execution)
Goal @ LinkName
```

### Compiler Transformation

**File:** `/tmp/FCP/Savannah/Logix/system/compile/precompile/rpc.cp`

The compiler transforms RPC calls at precompile time:

```prolog
% Transform Target#Goal into:
%   distribute # {Integer, Goal}    - when Target is a string (known at compile time)
%   transmit # {Target, Goal}       - when Target is a runtime value

remote_procedure_call(	Name, Goal,
			(distribute # {Index?, Goal})^,
			[import(Name, Index) | Im]^, Im
) :-
    string(Name) |
	true.

remote_procedure_call(	Service, Goal,
			(transmit # {Service, Goal})^,
			Im^, Im
) :-
    otherwise |
	true.
```

**Key points:**
- `distribute` uses an integer index into the module's export vector (fast path)
- `transmit` passes the target name at runtime (slow path, requires lookup)
- The compiler collects all imports and assigns indices

---

## 2. Module Types: Monitors vs Procedures

**File:** `/tmp/FCP/Savannah/Logix/domain_server.cp` (lines 1157-1354)

FCP supports two module types:

### Monitor Modules

A **monitor** is a process that:
1. Accepts messages on an input stream
2. Dispatches to local procedures
3. Can maintain state across calls

```prolog
monitor(In, Domain, ML, MR, Stop, Monitor, Attributes,
        ServiceId, InChannel, OutChannel
) :-
    In ? _Functor(CallInfo, _Scope, Goals, SCC),
    ... |
    monitor_goals(Goals, Domain, ML'?, ML, Stop1?, Monitor, Attributes,
                  ServiceId, InChannel, CallInfo, CO, CL, CR, CC),
    self;
    ...
```

### Procedure Modules

A **procedure** module exposes its predicates directly without a monitor wrapper.

### Module Activation

**File:** `/tmp/FCP/Savannah/Logix/domain_server.cp` (lines 1076-1149)

```prolog
activated(In, Module, ServiceId, Kind, Domain, Attributes,
          Distributor, ActivateControls
) :-
    ActivateControls = Monitor(Input?, ML, MR, Output!),
    Monitor =\= procedures,
    Monitor =\= meta_suspend... |
    Kind = monitor(CCC),
    monitor(In, Domain, MR, done([]), Stop, Monitor, Attributes,
            ServiceId, Input!, Output!
    )...
```

---

## 3. Message Distribution via Vectors

### Vector Creation

**File:** `/tmp/FCP/Savannah/Logix/EMULATOR/kernels.c` (lines 443-479)

The emulator provides `make_vector` to create a distribution vector:

```c
/* make_vector(N?, Ref^, Out^) */
do_make_vector(Arg)
     heapT Arg;
{
  ...
  KOutA = Ref_Word(HP);
  *HP++ = Word(Arity, VctrTag);
  Pa = HP;
  HP += Arity;
  KOutB = Ref_Word(HP);
  *HP++ = Word(Arity, TplTag);
  Pb = HP;
  HP += Arity;
  while (Arity-- > 0) {
    *Pa++ = Ref_Word(HP);
    *HP = Var_Word(Pb, WrtTag);
    *Pb++ = Var_Word(HP, RoTag);
    HP++;
  }
  return(True);
}
```

### Vector Writing (Message Dispatch)

**File:** `/tmp/FCP/Savannah/Logix/EMULATOR/kernels.c` (lines 486-529)

```c
/* write_vector(N?, Element?, Ref?, Ref'^) */
do_write_vector(Arg0, Arg1, Arg2)
     heapT Arg0, Arg1, Arg2;
{
  ...
  /* Index into vector and append element to stream */
  chng(*Pa, Pa, Ref_Word(HP)); /* destructive */
  *HP = Var_Word((HP+2), WrtTag);
  HP++;
  asgn(Va, Pb, Ref_Word(HP));
  *HP++ = Set_List(Arg1);
  *HP = Var_Word((HP-2), RoTag);
  HP++;
  return(True);
}
```

---

## 4. Output Server: Handling distribute/transmit

**File:** `/tmp/FCP/Savannah/Logix/domain_server.cp` (lines 1954-2039)

The output server processes RPC messages from procedure bodies:

```prolog
output(CallInfo, ServiceId, Output, UCC, Done, Vector, Left, Right,
       Circuit, Domain
) :-
    % Fast path: distribute with known index
    Output ? distribute(DX, Goal, DL, DR),
    ServiceId = [_ | Scope],
    integer(DX),
    vector(Vector) |
    unify_without_failure(DL, DR),
    write_vector(DX, export(ServiceId, Scope, Goal, {CO, CM?, CR, CC}),
                 Vector, Vector'),
    self;

    % Slow path: transmit with runtime target
    Output ? transmit(Target, Goal, TL, TR),
    ... |
    transmit(ServiceId, ServiceId, Goal, {CO, CM?, CR, CC}, Domain,
             Target?
    );
    ...
```

**Key insight:** `distribute` uses `write_vector` to directly index into the export vector, while `transmit` requires a lookup through the domain server.

---

## 5. Hierarchy Server: Service Location

**File:** `/tmp/FCP/Savannah/Logix/hierarchy_server.cp`

The hierarchy server manages the tree of services and resolves module references:

```prolog
serve_request(Request, Node, Mode, SId, Domain, SS, Control) :-
    Request = request(Goal, CL, CR),
    arg(1, Goal, Name),
    get_node(Node, Name, Reply, Node', ... |
    serve_request_continue(Reply?, Name, Goal, CL, CR, ...);
    ...
```

### Service Types

```prolog
% Service types in hierarchy
director     - Contains sub-services (directory)
module       - Loadable code module
import       - Reference to external module
exported     - Publicly accessible service
```

---

## 6. LPI (Logic Programs with Inheritance) Module Transformation

**File:** `/tmp/FCP/Savannah/Logix/system/transform/lpi/module.cp`

LPI adds inheritance semantics to modules:

```prolog
procedure clauses(Clauses, Terms, State, IdC, IdZ, Diags, ModuleName).

clauses(Clauses, Terms, State, IdC, IdZ, Diags, MN) :-
    Clauses ? remote(Remote, RCs),
    listener(MN),
    listener(State) |
    % Handle remote module references: ModuleName#ProcedureName
    add_calls(Remote, RCs, Terms, Terms', IdC, IdC', Diags, Diags'),
    clauses;
    ...
```

---

## 7. Message Flow Summary

```
┌─────────────────────────────────────────────────────────────────┐
│                         COMPILE TIME                             │
├─────────────────────────────────────────────────────────────────┤
│  Source: foo # bar(X)                                           │
│     ↓                                                           │
│  RPC Precompiler (rpc.cp)                                       │
│     ↓                                                           │
│  Transformed: distribute # {3, bar(X)}   (index 3 for "foo")   │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                         RUNTIME                                  │
├─────────────────────────────────────────────────────────────────┤
│  1. Goal body executes distribute # {3, bar(X)}                 │
│  2. Output server catches distribute message                    │
│  3. write_vector(3, export(..., bar(X), CCC), Vector)          │
│  4. Message appended to stream at index 3 of export vector     │
│  5. Target module's monitor reads from its input stream         │
│  6. Monitor dispatches bar(X) to local procedure               │
└─────────────────────────────────────────────────────────────────┘
```

---

## 8. Key Design Patterns for GLP

### Pattern 1: Module as Monitor Process

Each module is a process with:
- **Input stream** - receives RPC messages
- **Export vector** - indexed streams for each imported module
- **Local procedures** - actual code to execute

### Pattern 2: Indexed vs Dynamic Dispatch

- **Indexed (distribute):** Known target at compile time → direct vector write
- **Dynamic (transmit):** Runtime target → hierarchy lookup

### Pattern 3: Computation Context (CCC/UCC)

Every RPC carries computation controls:
```prolog
{CO, CL, CR, CC}
% CO = Control Output (signals: abort, suspend, resume)
% CL = Computation Left (input from caller)
% CR = Computation Right (output to caller)
% CC = Computation Channel (for nested requests)
```

### Pattern 4: Scope Threading

Service ID is a path: `[module_name | parent_scope]`
```prolog
ServiceId = [foo, bar, system]  % foo inside bar inside system
```

---

## 9. Files Reference

| Component | File | Key Functions |
|-----------|------|---------------|
| RPC Compiler | `system/compile/precompile/rpc.cp` | `index/3`, `remote_procedure_call/5` |
| Domain Server | `domain_server.cp` | `monitor/10`, `output/10`, `activated/8` |
| Hierarchy Server | `hierarchy_server.cp` | `serve_request/7`, `get_node/5` |
| LPI Transform | `system/transform/lpi/module.cp` | `clauses/7`, `add_calls/8` |
| Vector Ops | `EMULATOR/kernels.c` | `do_make_vector`, `do_write_vector` |
| Module Creation | `EMULATOR/ctl.c` | `make_ctl_module/2` |

---

## 10. FCP Compilation Modes

**File:** `/tmp/FCP/Savannah/Logix/system/compile/control/self.cp`

FCP has **7 compilation modes** that control how much of the CCC (Computation Control Context) is threaded through procedures:

### Mode Definitions

```prolog
-export([boot/3,
         system/3, user/3, monitor/5,
         trust/3, failsafe/3, interrupt/3,
         interpret/3
        ]).
```

| Mode | Args Added | CCC Threading | Purpose |
|------|------------|---------------|---------|
| `trust` | +3 (L,R,V) | Circuit only | Minimal overhead. Just termination circuit for spawned calls. |
| `failsafe` | +3 (L,R,V) | Circuit + exceptions | Adds `_select`/`_unknown` for RPC dispatch. Exception handling. |
| `interrupt` | +4 (S,L,R,V) | Full CCC | Signal channel (S) for suspend/resume. Full control. |
| `interpret` | (delegated) | Full meta | Meta-interpreter: `meta # interpret(...)` |
| `system` | (monitor) | trust+system | Monitor wrapper for system modules |
| `user` | (monitor) | trust+user | Monitor wrapper for user modules |
| `boot` | (system) | system | Bootstrap mode |

### Mode Implementation

```prolog
% From compile/control/self.cp lines 81-94
trust(Linkage, Precompiled, Protected) :-
    protect(Linkage, Precompiled, Protected, trust).

failsafe(Linkage, Precompiled, Protected) :-
    protect(Linkage, Precompiled, Protected, failsafe).

interrupt(Linkage, Precompiled, Protected) :-
    protect(Linkage, Precompiled, Protected, interrupt).

interpret(Linkage, Precompiled, Protected) :-
    meta # interpret(Linkage, Precompiled, Protected).
```

### CCC Threading Based on Mode

```prolog
% Lines 390-396: interrupt adds signal argument
rewrite_head(Mode, Functor, Arguments, R, H, H1) :-
    Mode = interrupt(_) |
        augment_head(Functor, H, Arguments, 5, `interrupt(signal), R, H1);
    otherwise : Mode = _ |
        augment_head(Functor, H, Arguments, 4, _, R, H1).
```

### Meta-interpreter Partial Evaluation

The modes represent **partial evaluations** of a full meta-interpreter:
- `interpret` - Full meta-interpreter (maximum control, maximum overhead)
- `interrupt` - Pre-compiled suspend/resume capability
- `failsafe` - Pre-compiled exception handling
- `trust` - Minimal overhead, just circuit controls

---

## 11. Hierarchical Module Compilation: The "Block" System

**File:** `/tmp/FCP/Savannah/Logix/system/block/`

FCP provides **whole-program optimization** through the "block" system, which compiles multiple modules into a single unit, **eliminating RPC overhead** for intra-block calls.

### Entry Point

```prolog
% From block/self.cp:126-141
compose(Path) + (Options = []) :-
    ...
    tree # load(ScopeId, RootId, Tree, BlockedSource, Report),
    ...
```

### What Blocking Does

1. **Loads module hierarchy** - Traverses the tree of modules
2. **Renames procedures** - Prefixes with path: `mod1$mod2$procedure`
3. **Transforms intra-block RPCs to direct calls** - No vector dispatch!
4. **Produces single "blocked source"** - Compiled as one unit

### Procedure Renaming

**File:** `block/tree/rename/self.cp:138-158`

```prolog
% local(Answer, Atom, Lead, RenamedAtom)
% If Atom refers to a locally defined procedure,
% prefix its functor with Lead in RenamedAtom.

local(Answer, Atom, Lead, RenamedAtom) :-
    Answer = true,
    tuple(Atom),
    arg(1, Atom, Functor),
    string(Functor),
    ...
    string_to_dlist(Lead, RFL, FL),
    string_to_dlist(Functor, FL, []) |
    list_to_string(RFL, RenamedFunctor),
    copy_args(2, Atom, RenamedAtom);
```

### Example Transformation

```
Before blocking:
  Module utils has procedure sort/2
  Code calls: utils # sort(X, Y)

After blocking:
  Procedure renamed to: utils$sort/2
  Call transformed to: utils$sort(X, Y)  (direct call, no RPC!)
```

### Static vs Dynamic Dispatch

**File:** `block/tree/self.cp:93-97`

- **Within block**: RPCs become direct procedure calls (static linking)
- **Outside block**: Remain as RPCs (dynamic dispatch via vector)

```prolog
GCalls ? {Service # RemoteGoal, Graph, Goal},
(Service # RemoteGoal) =\= (self # service_id(_)) |
    graph # rpc(RemoteGoal, Graph, Goal, Reply, Service),
    ...
```

### Block System Files

| File | Purpose |
|------|---------|
| `block/self.cp` | Entry point: `compose(Path, Options)` |
| `block/tree/self.cp` | Loads module tree, coordinates blocking |
| `block/tree/graph.cp` | Transforms hierarchy into processing graph |
| `block/tree/rename/self.cp` | Renames procedures with path prefix |
| `block/tree/rename/clause.cp` | Transforms individual clauses |

---

## 12. Previous GLP Module System Implementation

An earlier implementation effort exists on branch:

**Branch:** `claude/general-session-011HS9n574JAiMzTA783iB95`

### Implementation Phases Completed

| Phase | Description | Commit |
|-------|-------------|--------|
| Phase 1 | Per-module compilation | `b71135d` |
| Phase 2 | Syntax parsing (`Module#Goal`) | `a7b6f8e` |
| Phase 3 | Runtime module execution with `CallRemote` | `abc6278` |
| Phase 3.5 | REPL transition to module system | `851809f` |
| Phase 4 | FCP-style module execution | `08004f3` |

### Files Created

**Documentation:**
- `docs/glp-modules-spec.md`
- `docs/module-implementation-plan.md`
- `docs/module-language-spec.md`

**Implementation:**
- `glp_runtime/lib/runtime/modules.dart`
- `glp_runtime/lib/runtime/loaded_module.dart`

**Tests:**
- `glp_runtime/test/module/module_compile_test.dart`
- `glp_runtime/test/module/module_execution_test.dart`
- `glp_runtime/test/module/module_syntax_test.dart`

### Key Commits

```
08004f3 feat: Phase 4 complete - FCP-style module execution
e50973d feat: Phase 4 step 1 - Runner uses module instructions
16cc6a7 feat: Add moduleName to GoalRef for module-aware execution
809880b docs: Update module plan v3.2 - Phase 3.5 complete
851809f feat: Phase 3.5 - REPL transition to module system
abc6278 feat: Implement Phase 3 - Runtime module execution with CallRemote
a7b6f8e feat: Implement Phase 2 of module system - syntax parsing
b71135d feat: Implement Phase 1 of module system - per-module compilation
```

### Status

This branch was **not merged to main**. The implementation may need review and updating based on subsequent GLP development.

---

## 13. Implications for GLP Module System

1. **Monitor Pattern:** Each GLP module should be a monitor process that receives RPC messages

2. **Compile-Time Indexing:** Transform `Module#Goal` to indexed dispatch when Module is known statically

3. **Vector-Based Distribution:** Use vectors for O(1) message routing to imported modules

4. **Computation Context:** Thread control signals (abort, suspend) through all RPC calls

5. **Hierarchical Naming:** Service IDs as paths enable scoped module resolution

6. **Two Dispatch Modes:**
   - Fast path: compile-time known targets → indexed vector write
   - Slow path: runtime targets → hierarchy server lookup
