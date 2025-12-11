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

## 10. Implications for GLP Module System

1. **Monitor Pattern:** Each GLP module should be a monitor process that receives RPC messages

2. **Compile-Time Indexing:** Transform `Module#Goal` to indexed dispatch when Module is known statically

3. **Vector-Based Distribution:** Use vectors for O(1) message routing to imported modules

4. **Computation Context:** Thread control signals (abort, suspend) through all RPC calls

5. **Hierarchical Naming:** Service IDs as paths enable scoped module resolution

6. **Two Dispatch Modes:**
   - Fast path: compile-time known targets → indexed vector write
   - Slow path: runtime targets → hierarchy server lookup
