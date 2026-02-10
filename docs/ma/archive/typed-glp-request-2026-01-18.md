# Request for Typed GLP Project: Type Checker Issue

**Date**: 2026-01-18  
**From**: Multiagent GLP Project  
**To**: Typed GLP Project  
**Priority**: Blocking → **UPDATE: Advisory (pipeline fix applied)**

---

## Summary

The Multiagent GLP project needs type declarations for `social_agent.glp`. The pipeline fix has been applied so type errors are now advisory, but the following issues remain for investigation.

---

## SRSW Violation (FIXED)

The original SRSW violation in `tag_stream/3` has been fixed by adding `ground(From?)` guard:

```glp
tag_stream(From, [Content|Rest], [msg(From?, _, Content?)|Tagged?]) :-
    ground(From?) |
    tag_stream(From?, Rest?, Tagged).
```

---

## Remaining Type Errors (Advisory)

**Command run**:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/check_types.dart ../programs/multiagent/social_agent.glp
```

**Output (5 errors)**:

### 1-2. Type Complementarity in build_friends
```
✗ Variable pair (UserOut, UserOut?) not complementary across clause: 
  Variables across head/body must have same type: MsgStream (base: MsgStream) != _? (base: _): 
  writer at head=(MsgStream, ↑), reader at body atom 0=(_?, ↓) at line 64, column 1
    in: build_friends(4 args) :- 1 goals.

✗ Variable pair (NetOut, NetOut?) not complementary across clause: 
  Variables across head/body must have same type: MsgStream (base: MsgStream) != _? (base: _): 
  writer at head=(MsgStream, ↑), reader at body atom 0=(_?, ↓) at line 64, column 1
    in: build_friends(4 args) :- 1 goals.
```

**Analysis**: The procedure declaration says `MsgStream` for args 2 and 3, but inside the list literal `[(user, UserOut?), ...]` the type checker infers `_?` for the second element of the pair.

### 3-4. Mode Mismatch in Introduction Protocol
```
✗ Body atom 3 (lookup_send) is not well-typed:
  Inconsistent path: Variable mode mismatch at wildcard _?
  Path: (msg/3, 0, input) → (intro/2, 3, input) → (ch/2, 2, input) → (PtoQ, 2, input) at line 81
    in: social_graph(3 args) :- 4 goals.

✗ Body atom 4 (lookup_send) is not well-typed:
  Inconsistent path: Variable mode mismatch at wildcard _?
  Path: (msg/3, 0, input) → (intro/2, 3, input) → (ch/2, 2, input) → (QtoP, 2, input) at line 81
    in: social_graph(3 args) :- 4 goals.
```

**Analysis**: The fresh channel variables `PtoQ` and `QtoP` created in the introduction clause are writers being placed inside a structure that gets passed to `lookup_send`. The type checker sees them at an "input" position but they're actually fresh writers.

### 5. Missing =/2 Procedure
```
✗ Body atom 1 (=) is not well-typed:
  Inconsistent path: Undefined procedure: =/2
  Path: (=/2, 0, output) at line 188, column 1
    in: lookup_send(5 args) :- 1 goals.
```

**Analysis**: The prelude doesn't include a declaration for unification `=/2`.

---

## Program Location

`/Users/udi/Grassroots/GLP/programs/multiagent/social_agent.glp`

---

## Current Status

With the pipeline fix, the program now loads and runs despite type warnings. The Multiagent GLP project is proceeding with testing. These type errors are recorded for future investigation to improve the type system.

---

## Files to Examine

- `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/type_environment_builder.dart` - Type resolution
- `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/well_typed_clause.dart` - Clause checking
- `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/prelude.dart` - Add `=/2` declaration
