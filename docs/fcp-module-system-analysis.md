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

---

## 14. Deep Dive: How Monitor-Based RPC Actually Works

This section answers detailed questions about the FCP RPC mechanism with specific file/line references.

### 14.1 When Module A Calls `B # goal(X, Y)`

**Question:** How does the message get from A to B? What is B's monitor doing? How does the result get back to A?

#### Step 1: Compile-Time Transformation

**File:** `system/compile/precompile/rpc.cp:164-175`

When module A is compiled and contains `math # factorial(5, R)`:

```prolog
% If "math" is a string known at compile time:
remote_procedure_call(Name, Goal,
    (distribute # {Index?, Goal})^,
    [import(Name, Index) | Im]^, Im
) :-
    string(Name) |
        true.
```

The compiler:
1. Collects all imports (like `math`) into a list
2. Assigns an integer index to each (via `indices/3` at line 116)
3. Transforms `math # factorial(5, R)` → `distribute # {1, factorial(5, R)}`

#### Step 2: Module A's Distributor Setup

**File:** `domain_server.cp:910-938`

When module A is activated, a **distributor vector** is created:

```prolog
distributor(Imports, ServiceId, Distributor, Domain)
 + (Index = 0, VectorTuple, Tuple = VectorTuple?) :-

    Imports ? ServiceName, string(ServiceName),
    Index++,
    listener(ServiceId),
    listener(Domain),
    listener(Tuple) |
        arg(Index', Tuple, Stream),
        cache_import(Stream?, ServiceName, ServiceId, Domain),
        self;
    ...
```

This creates a vector with N entries, one per imported module. Each entry is a **stream** that will carry messages to that module.

#### Step 3: Message Dispatch via Output Server

**File:** `domain_server.cp:1964-1975`

When A's body executes `distribute # {1, factorial(5, R)}`:

```prolog
output(CallInfo, ServiceId, Output, UCC, Done, Vector, Left, Right,
        Circuit, Domain
) :-
    Output ? distribute(DX, Goal, DL, DR),
    ServiceId = [_ | Scope],
    UCC = {CO, CL, CR, CC},
    ...
    integer(DX),
    vector(Vector) |
        unify_without_failure(DL, DR),
        UCC' = {CO, CL, CM, CC},
        write_vector(DX, export(ServiceId, Scope, Goal, {CO, CM?, CR, CC}),
                     Vector, Vector'),
        self;
```

**Key:** `write_vector(1, export(..., factorial(5, R), CCC), Vector)` appends the message to stream #1.

#### Step 4: Cache Import Connects to Target Module

**File:** `domain_server.cp:964-1013`

The `cache_import` procedure establishes the connection to module B:

```prolog
cache_import(Stream, Import, ServiceId, Domain) :-
    ...
    list(Stream),
    Import = self,
    ServiceId = [Name | _], Name =\= self,
    channel(Domain) |
        write_channel(find(ServiceId, SSC), Domain, Domain'),
        serve_import(Stream, Import, ServiceId, Domain'?, SSC?);
    ...
```

It:
1. Sends `find([math, ...], SSC)` to the Domain server
2. Domain server locates module B and returns its Service Channel (SSC)
3. `serve_import` then forwards all messages from Stream to SSC

#### Step 5: Module B's Monitor Receives the Message

**File:** `domain_server.cp:1163-1199`

Module B is running as a monitor process:

```prolog
monitor(In, Domain, ML, MR, Stop,  Monitor, Attributes,
        ServiceId, InChannel, OutChannel
) :-
    In ? _Functor(CallInfo, _Scope, Goals, SCC),
    listener(Domain),
    listener(Monitor),
    listener(Attributes),
    listener(ServiceId),
    listener(InChannel),
    SCC = {CO, CL, CR, CC},
    ... |
        monitor_goals(Goals, Domain, ML'?, ML, Stop1?, Monitor, Attributes,
                      ServiceId, InChannel, CallInfo, CO, CL, CR, CC),
        self;
```

The monitor:
1. Reads `export(CallInfo, Scope, factorial(5, R), CCC)` from its input stream `In`
2. Extracts the goal `factorial(5, R)` and the computation controls
3. Calls `monitor_goals` to dispatch to the local `factorial/2` procedure

#### Step 6: Result Returns via CCC

The key to understanding how results return is the **CCC (Computation Control Context)**:

```prolog
{CO, CL, CR, CC}
% CO = Control Output - signals (abort, suspend, resume)
% CL = Computation Left - input from caller
% CR = Computation Right - output to caller
% CC = Computation Channel - for nested requests
```

When module A makes the call:
- A's `CR` (right side) becomes part of the CCC sent to B
- When B binds `R` (in `factorial(5, R)`), the binding flows through this stream back to A
- **The result returns via stream unification, not a separate return message!**

This is the key FCP insight: **variables in the goal are shared between caller and callee**. Results flow back automatically when the callee binds them.

---

### 14.2 The Vector/Distribute Mechanism in Detail

**Question:** From the analysis: `distribute # {Index, Goal}` writes to a vector. Who reads from that vector? Is each imported module a stream in the vector?

#### How the Vector Works

**File:** `EMULATOR/kernels.c:443-479`

```c
do_make_vector(Arg) {
    ...
    KOutA = Ref_Word(HP);        // Vector reference
    *HP++ = Word(Arity, VctrTag);
    Pa = HP;
    HP += Arity;
    KOutB = Ref_Word(HP);        // Tuple reference for reading
    *HP++ = Word(Arity, TplTag);
    Pb = HP;
    HP += Arity;
    while (Arity-- > 0) {
        *Pa++ = Ref_Word(HP);    // Vector entry points to HP
        *HP = Var_Word(Pb, WrtTag); // Writer variable
        *Pb++ = Var_Word(HP, RoTag); // Reader variable
        HP++;
    }
}
```

`make_vector(N, Vector, Tuple)` creates:
- **Vector:** N-element vector where each element is a **writer end** of a stream
- **Tuple:** N-element tuple where each element is the **reader end** of the same stream

#### Who Reads from Each Stream?

**Answer:** Each imported module has a **dedicated reader process** (`serve_import`) that reads from its stream entry.

**File:** `domain_server.cp:1015-1033`

```prolog
serve_import(Stream, Import, ServiceId, Domain, SSC) :-
    Stream ? RPC,
    channel(SSC) |
        write_channel(RPC, SSC, SSC'),  % Forward to target module
        self;

    Stream = [] | Import = _, ServiceId = _, Domain = _, SSC = _ ;
    ...
```

So for a module with imports `[math, io, utils]`:
- Index 1 → stream to `math` → `serve_import` reads and forwards to math's monitor
- Index 2 → stream to `io` → `serve_import` reads and forwards to io's monitor
- Index 3 → stream to `utils` → `serve_import` reads and forwards to utils' monitor

Each `serve_import` connects to its target module **lazily** - the first message triggers a `find(ModuleName, SSC)` lookup.

---

### 14.3 Concrete Example: `math # factorial(5, R)` Message Flow

```
┌─────────────────────────────────────────────────────────────────────────┐
│ COMPILE TIME                                                            │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  Source (in module A):  R := math # factorial(5)                       │
│                                                                         │
│  After RPC transformation (rpc.cp:164-175):                            │
│     distribute # {1, factorial(5, R)}                                  │
│     (assuming math is import #1)                                        │
│                                                                         │
│  Import list: [import(math, 1)]                                        │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ RUNTIME - Module A Activation                                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  1. domain_server.cp:910-938 - Create distributor:                     │
│     make_vector(1, Vector, Tuple)                                       │
│     Vector = {Stream1_Writer}                                           │
│     Tuple = {Stream1_Reader}                                            │
│                                                                         │
│  2. domain_server.cp:964-1013 - Start serve_import for math:           │
│     serve_import(Stream1_Reader, math, [A|Scope], Domain, ?)            │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ RUNTIME - Goal Execution                                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  3. Module A's body executes, encounters:                              │
│     distribute # {1, factorial(5, R)}                                  │
│                                                                         │
│  4. domain_server.cp:1964-1975 - Output server handles:                │
│     write_vector(1, export([A|Scope], Scope, factorial(5,R),           │
│                           {CO, CM?, CR, CC}),                          │
│                  Vector)                                                │
│                                                                         │
│     → Appends export message to Stream1                                 │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ RUNTIME - Message Routing                                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  5. serve_import reads from Stream1_Reader:                             │
│     Stream ? export([A|Scope], Scope, factorial(5,R), CCC)              │
│                                                                         │
│  6. First message triggers lookup (domain_server.cp:970-975):          │
│     write_channel(find([math|Scope], SSC), Domain)                     │
│                                                                         │
│  7. Domain server locates math module, returns SSC                     │
│                                                                         │
│  8. serve_import forwards to math's channel:                           │
│     write_channel(export(..., factorial(5,R), CCC), SSC)               │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ RUNTIME - Module B (math) Execution                                     │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  9. math's monitor reads from its input (domain_server.cp:1167):       │
│     In ? export(CallInfo, Scope, factorial(5,R), SCC)                   │
│                                                                         │
│  10. monitor_goals dispatches to local factorial/2                      │
│                                                                         │
│  11. Local procedure executes: factorial(5, R)                         │
│      → Computes result, binds R = 120                                   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ RESULT RETURN - Via Stream Unification                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  12. When math binds R = 120:                                          │
│      - R is the SAME variable that A passed in the goal                │
│      - The binding propagates automatically through shared reference    │
│      - A's R variable becomes bound to 120                             │
│                                                                         │
│  13. No explicit "return" message needed!                              │
│      FCP uses shared variables for bidirectional communication         │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

### 14.4 Alternatives to Monitors

**Question:** What's the alternative to monitors for cross-module calls?

FCP provides three approaches:

#### Alternative 1: Procedure Modules (`-mode(trust/failsafe/interrupt)`)

**File:** `domain_server.cp:1117-1127`

```prolog
activated(..., ActivateControls, ...) :-
    ActivateControls = procedures(_, _, _) | Attributes = _,
        Kind = procedures,
        in_server(In, Module, failsafe(abort, relay), ServiceId,
                  Domain, Distributor);

    ActivateControls = procedures(_, _, _, _) | Attributes = _,
        Kind = procedures,
        in_server(In, Module, interrupt(interrupt, serve), ServiceId,
                  Domain, Distributor);
```

Procedure modules don't have a user-defined monitor - they expose procedures directly with:
- `trust` - minimal overhead
- `failsafe` - exception handling
- `interrupt` - suspend/resume capability

#### Alternative 2: Block Compilation (Whole-Program Optimization)

**File:** `block/tree/self.cp`

When modules are "blocked" together:
- Intra-block RPCs become **direct procedure calls**
- No monitor, no vector, no message passing
- Procedures renamed: `math$factorial/2`

```prolog
% Before blocking:
math # factorial(5, R)

% After blocking (direct call):
math$factorial(5, R)
```

**This eliminates all RPC overhead for calls within the same block.**

#### Alternative 3: Direct Channel Communication

If you have a reference to a module's channel (not its name), you can write directly:

**File:** `domain_server.cp:789-791`

```prolog
send_rpc_goal(Goal, UCC, DRC, CallInfo, Scope, Target) :-
    channel(Target),
    arity(Target) =:= 1 | DRC = _, CallInfo = _, Scope = _,
        write_channel(Goal, Target),  % Direct write!
        closeCC(UCC);
```

---

### 14.5 Summary: Two-Phase Message Flow

| Phase | Mechanism | Overhead |
|-------|-----------|----------|
| **Static dispatch** | `distribute # {Index, Goal}` → `write_vector` → stream → `serve_import` → target | O(1) vector index + stream message |
| **Dynamic dispatch** | `transmit # {Target, Goal}` → `find(Target, SSC)` → hierarchy lookup → target | O(log n) lookup + stream message |
| **Blocked (inlined)** | Direct procedure call | Zero RPC overhead |

The FCP module system is optimized for the common case (static dispatch) while supporting dynamic dispatch when needed.

---

## 15. Additional FCP Module System Details

### 15.1 Module File Location

**Question:** How does FCP find module files when loading?

**File:** `system/get_module.cp`

FCP uses a **context-relative file search** with specific naming conventions:

```prolog
% From get_module.cp:67-84
update(Context, Name, Options, Output, ModuleResult) :-
    string_to_dlist(Name, ListBin, Bin),
    string_to_dlist(".bin", Bin, []),           % Binary suffix
    string_to_dlist("Bin/", BinListBin, ListBin) | % Bin/ subdirectory
    ...
    file # [execute_in_context(Context, fileinfo(BinName, BinDate)),
            execute_in_context(Context, fileinfo(BinBinName, BinBinDate)),
            execute_in_context(Context, fileinfo(Dot_oName, Dot_oDate)),
            execute_in_context(Context,fileinfo(BinDot_oName, BinDot_oDate))
           | CpInfo],
```

**Search order for module `foo` in context `[path, to, module]`:**

1. `Bin/foo.bin` - compiled bytecode in Bin subdirectory
2. `foo.bin` - compiled bytecode in current directory
3. `Bin/foo.o` - native compiled object in Bin subdirectory
4. `foo.o` - native compiled object in current directory
5. `foo.cp` - source file (if auto-compile enabled)

**Key insight:** FCP uses a **hierarchical directory structure** where:
- Each module directory can have a `Bin/` subdirectory for compiled files
- Source files are `.cp` files
- Binary files are `.bin` (bytecode) or `.o` (native)
- The context is a **path list** like `[module_name, parent_name, grandparent_name]`

**File:** `system/get_source.cp:146-149`

```prolog
Source = file(Context, Name, Requests) :
  Requests ! execute_in_context(Context, isdirectory(Name, IsDirectory)) |
  get_file_source(Abort, Control, IsDirectory, Reply, Terms, Outs,
                  Context, Name, Requests')
```

If a file isn't found in the current context, FCP walks up the hierarchy looking in parent directories.

---

### 15.2 Circular Dependencies

**Question:** How does FCP handle A imports B, B imports A?

**Finding:** FCP uses **lazy loading** which naturally handles circular dependencies.

**Mechanism:**

1. **Import declarations are collected at compile time** but NOT resolved immediately
2. **Modules are loaded on first RPC** - not when imported
3. **The `serve_import` process** connects lazily:

**File:** `domain_server.cp:964-1013`

```prolog
cache_import(Stream, Import, ServiceId, Domain) :-
    Stream = [] | Import = _, ServiceId = _, Domain = _ ;  % Empty = no messages yet

    list(Stream),  % First message arrives
    ... |
        write_channel(find(ServiceId, SSC), Domain, Domain'),
        serve_import(Stream, Import, ServiceId, Domain'?, SSC?);
```

**Why circular dependencies work:**

- Module A can import B and B can import A
- When A loads, B's import entry is created but B isn't loaded yet
- If A calls `B # foo(...)`, B loads on demand
- If B calls `A # bar(...)`, A is already loaded (the caller)
- The vector/stream mechanism allows messages to queue before the target is ready

**No explicit circular dependency detection** is needed because:
- Each module is an independent process
- Messages queue on streams until the receiver is ready
- There's no blocking synchronous "require" step

---

### 15.3 Boot Sequence and Entry Point

**Question:** How does FCP determine which module/goal runs first?

**File:** `system/compile/control/run.cp`

FCP has a special **boot mode** for the entry point module:

```prolog
% From run.cp:66-74
boot_clause({ {Boot, `In, Out},
              {Ask, Tell},
              Body
            },
            { {Boot, `boot(in), Out},
              {Ask, [`boot(in) = [system | `In] | Tell]},
              Body
            }^
).
```

**Boot sequence:**

1. **System starts with `boot/2` procedure** in the root module
2. The boot clause receives `boot(in)` which is bound to `[system | In]`
3. `system` is the system service channel
4. Boot procedure initializes the computation and starts user services

**File:** `hierarchy_server.cp:109-116`

```prolog
start(IHS) :-
    processor # file(working_directory(UID), true),
    server(PIN, FPS'),
    FPS ! find([], not_found(director([], binary(UID), _Close), open(_))),
    domain_server # process_dictionary(FPS, Closes),
    close_services(Closes),
    make_channel(PIOC, PIN),
    hierarchy(IHS, PIOC).
```

**Startup flow:**
1. `hierarchy_server # start(...)` initializes the hierarchy
2. `domain_server` starts with the root module
3. Root module's `boot/2` clause executes
4. Boot procedure spawns initial computation goals

**There is no explicit "main" function** - the boot module is determined by the computation context that starts the system.

---

### 15.4 Error Handling: Missing Modules and Procedures

**Question:** When an RPC target module doesn't exist or procedure isn't exported, what happens?

**File:** `domain_server.cp:574-601`

FCP has explicit error handling for missing services:

```prolog
% Module/service not found
Serve = no_service(Id),
Id =\= processor, Id =\= _@_,
otherwise,
UCC = {_, CL, CR, CC},
channel(CC) | DRC = _, Scope = _, Closed = _,
    write_channel(request(CallInfo, failed(Id # Goals, no_service), CL, CR),
                  CC
    );

% Processor service discarded
Serve = no_service(processor),
otherwise,
UCC = {_, CL, CR, CC},
channel(CC) | DRC = _, Scope = _, Closed = _,
    write_channel(request(CallInfo, failed(processor # Goals, discarded),
                          CL, CR),
                  CC
    );

% Link not found
Serve = no_service(Id @ LinkName),
otherwise,
UCC = {_, CL, CR, CC},
channel(CC) | DRC = _, Scope = _, Closed = _,
    write_channel(request(CallInfo, failed(Id # Goals @ LinkName, no_link),
                          CL, CR),
                  CC
    );
```

**Error types:**

| Error | Meaning | Where Generated |
|-------|---------|-----------------|
| `no_service` | Module/service not found | `domain_server.cp:579` |
| `discarded` | Processor service unavailable | `domain_server.cp:587` |
| `no_link` | Network link not found | `domain_server.cp:597` |
| `unknown` | Procedure not exported/found | `domain_server.cp:698` |
| `unrecognized` | Invalid goal format | `domain_server.cp:439` |

**What happens:**

1. **Error goes to computation channel** via `write_channel(request(..., failed(...), ...), CC)`
2. **Computation exception handler** processes the failure
3. **In trust mode:** Goal simply fails (no exception propagation)
4. **In failsafe/interrupt mode:** Exception can be caught and handled

**File:** `domain_server.cp:698-703`

```prolog
% Unknown procedure in known module
otherwise,		% Designated = super, Scope = []
known(Goal),
UCC = {_, CL, CR, CC},
channel(DRC) | Designated = _, Scope = _,
    write_channel(request(CallerId?, failed(CalledId? # Goal, unknown),
                          CL, CR),
                  CC
    ),
    failed_goal_call(CallInfo, CallerId, CalledId);
```

**Key insight:** FCP doesn't suspend on missing modules in trust mode - it **fails the goal** and reports to the computation channel. This is different from suspension on unbound variables.

---

### 15.5 Module Loading Modes

**File:** `get_module.cp:63`

```prolog
Default ::= query ; auto ; binary.
```

| Mode | Behavior |
|------|----------|
| `query` | Ask user whether to compile if source newer than binary |
| `auto` | Automatically compile if source newer |
| `binary` | Only load binary, never compile |

**File:** `get_module.cp:138-154`

```prolog
update_source(DateCp, DateBin, Context, NameBin, Update) :-
    DateBin @< DateCp :
      Update = update(DateCp, Context, NameBin) ;  % Source newer -> compile

    DateCp = 0, DateBin = 0 : Context = _, NameBin = _,
      Update = false(no_module) ;                  % Neither exists

    otherwise,
    NameBin = '.' : DateCp = _, DateBin = _, Context = _, NameBin = _,
      Update = false(no_module) ;                  % Already searched

    otherwise,
    NameBin =\= '.' |
      name_to_base_and_suffix(NameBin, BaseName, NameSuffix),
      load_file(DateCp, DateBin, Context, NameBin, Update,
                BaseName, NameSuffix).
```

---

### 15.6 Summary: FCP Module System Design Principles

1. **Lazy loading:** Modules load on first use, not at import declaration
2. **Hierarchical paths:** Module identity is a path list `[name, parent, grandparent]`
3. **Convention over configuration:** Standard directory structure (`Bin/`, `.cp`, `.bin`)
4. **Fail-fast for errors:** Missing modules/procedures fail immediately (don't suspend)
5. **Circular dependencies allowed:** Natural consequence of lazy loading + stream communication
6. **Boot protocol:** Special `boot/2` entry point receives system services
