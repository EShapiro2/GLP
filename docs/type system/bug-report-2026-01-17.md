# Type Checker Bug Report

**Date:** 2026-01-17  
**Reporter:** Claude (Opus 4.5)  
**Source file:** `glp_runtime/glp/social_agent.glp`  
**Status:** Open

---

## Context

While developing `social_agent.glp` (social graph agent with friends list), the type checker reported multiple errors that appear to be bugs rather than actual type violations in the program.

### Type Definitions Under Test

```prolog
FriendEntry ::= (Constant?, Stream).
FriendList ::= [] ; [FriendEntry | FriendList].
Channel ::= ch(Stream?, Stream).

NetMsg ::= msg(Constant, Constant, _).
UserMsg ::= sent(Constant, _) ; received(Constant, _) ; 
            received_ping(Constant) ; received_pong(Constant) ; 
            error(Constant, _).
Msg ::= NetMsg ; UserMsg.

Status ::= found ; not_found.
```

### Procedure Declarations Under Test

```prolog
procedure agent(Constant?, Constant?, Channel?, Channel?, Stream).
procedure build_friends(Constant?, Stream, Stream, Stream, FriendList).
procedure social_graph(Constant?, Stream?, FriendList?).
procedure lookup_send(Constant?, Msg?, FriendList?, FriendList, Status).
procedure handle_send_status(Status?, Constant?, _?, FriendList?, FriendList).
procedure handle_ping_status(Status?, Constant?, Constant?, FriendList?, FriendList).
procedure merge(Stream?, Stream?, Stream).
```

---

## Bug 1: String constants not recognized as Constant type

### Error Message
```
Inconsistent path: Constant does not match any alternative at type position Constant?
Path: (user, 0, input)
```

### Triggering Code
```prolog
lookup_send(user, received_ping(From?), Fs?, Fs1, S1),
```

### Analysis
- First argument of `lookup_send` is typed `Constant?`
- `Constant ::= Number ; String` (from prelude)
- `user` is a string (atom)
- `user` should match `Constant?`

### Expected Behavior
Type checker should accept `user` as a valid `Constant?`

---

## Bug 2: Union type transition not working for Msg?

### Error Message
```
No transition for msg(3,1):↓ from state Msg?
Path: (msg/3, 0, input) → (Id?, 1, input)
```

### Triggering Code
```prolog
lookup_send(To?, msg(Id?, To?, Content?), Fs?, Fs1, Status),
```

### Analysis
- Second argument of `lookup_send` is typed `Msg?`
- `Msg ::= NetMsg ; UserMsg`
- `NetMsg ::= msg(Constant, Constant, _)`
- When consuming `Msg?`, the structure `msg(Id?, To?, Content?)` should be valid
- The inner readers `Id?`, `To?`, `Content?` correspond to consuming the fields of the produced `msg(Constant, Constant, _)`

### Expected Behavior
Type checker should recognize `msg/3` as a valid alternative of `Msg?` and allow the transition

---

## Bug 3: Variable type inconsistency with repeated variable in head

### Error Message
```
Variable Id? has inconsistent types: (Constant?, ↓) vs (_?, ↓)
```

### Triggering Code
```prolog
social_graph(Id, [msg(From, Id?, ping)|In], Fs) :-
    ground(Id?), ground(From?) |
    ...
```

### Analysis
- `Id` appears in first argument position (typed `Constant?` via procedure declaration)
- `Id?` appears inside the list pattern `[msg(From, Id?, ping)|In]`
- The list is typed `Stream?` which is `[] ; [_|Stream]`
- Stream elements are `_` (any type)
- `Id?` inside `msg(From, Id?, ping)` is being matched against `_?`
- Both usages are readers (`Id?`), so the type checker should allow `Constant?` to be used where `_?` is expected

### Expected Behavior
Type checker should allow a more specific type (`Constant?`) to be used where a general type (`_?`) is expected

---

## Bug 4: Wildcard type mismatch in termination clause

### Error Message
```
Variable _? has inconsistent types: (Constant?, ↓) vs (FriendList?, ↓)
```

### Triggering Code
```prolog
social_graph(_, [], _).
```

### Analysis
- First `_` is at position typed `Constant?`
- Third `_` is at position typed `FriendList?`
- Anonymous variables `_` should match any type at their respective positions
- Each `_` is independent and should not be compared with other `_` occurrences

### Expected Behavior
Type checker should treat each `_` independently, accepting any type at its position

---

## Bug 5: Assignment mode mismatch in lookup_send

### Error Message
```
Variable mode mismatch at wildcard _?
Path: ([|]/2, 0, input) → (Out1, 2, input)
```

and

```
Variable pair (Out, Out?) not complementary across clause: Variables across head/body must have same type: _ (base: _) != Stream? (base: Stream): writer at body atom 1=(_, ↑), reader at head=(Stream?, ↓)
```

### Triggering Code
```prolog
lookup_send(Key, Msg, [(K, Out)|Rest], [(K?, Out1?)|Rest?], found) :-
    Key? =?= K? |
    Out = [Msg?|Out1].
```

### Analysis
- `FriendEntry ::= (Constant?, Stream)` - second element is `Stream` (produce mode)
- In head: `(K, Out)` at `FriendEntry` position → `Out` is writer for `Stream`
- In head: `(K?, Out1?)` at `FriendEntry` output → `Out1?` is reader
- In body: `Out = [Msg?|Out1]` assigns list with `Out1` as tail
- `Out` is writer, `Out1` is fresh variable for the tail
- The assignment should be valid SRSW: `Out` written once, `Out1` read once

### Expected Behavior
Type checker should accept this as valid SRSW with proper mode handling

---

## Reproduction

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo "social_agent.glp" | dart run bin/glp_repl.dart
```

---

## Notes

- All bugs appear related to mode/type inference with union types and nested structures
- The program logic appears correct based on similar working code in `play_alice_bob.glp`

---

## Bug 6: Type errors prevent program compilation and execution

### Current Behavior
When type errors are reported, the program fails to compile and cannot be executed:
```
Error: Predicate agent/5 not found
```

### Expected Behavior
Type errors should be warnings, not fatal errors. The program should compile and run even if it has type errors. Type checking is an optional static analysis that helps catch bugs early, but should not prevent execution of otherwise valid GLP code.

---

## Bug 7: SRSW checking not run when type errors exist

### Current Behavior
When the typed `social_agent.glp` is loaded, type errors are reported but SRSW violations are not shown. When the untyped `social_agent_untyped.glp` (identical logic, no type declarations) is loaded, SRSW violations are reported.

### Expected Behavior
SRSW checking is more fundamental than type checking and should run first, regardless of type errors. The checking order should be:
1. SRSW checking (fundamental correctness)
2. Type checking (optional static analysis)

Both should report all violations found, and neither should prevent the other from running.

---

## Union Alias Expansion (v0.8) - Implementation Progress

**Status:** IN PROGRESS - Core implementation done, testing in progress

### Feature Summary

Implementing union aliases that combine multiple type definitions into a single type name:
```glp
Msg ::= NetMsg ; UserMsg.  % Expands to all alternatives from both types
```

This is relevant to Bug 2 above (`Msg ::= NetMsg ; UserMsg`).

### Completed Work

1. **Paper Update** (`/Users/udi/Grassroots/Moded-Types/sections/well-typing.tex`)
   - Extended "Type aliases" section distinguishing simple vs union aliases
   - Documented expansion semantics and constraints

2. **Spec Update** (`type-environment.md` v0.7 → v0.8)
   - Added `isSimpleAlias()` and `isUnionAlias()` functions
   - Documented two-phase alias resolution algorithm
   - Added `AliasExpansionError` for validation failures

3. **Implementation** (`type_environment_builder.dart`)
   - Added `AliasExpansionError` exception class
   - Split `_isTypeAlias()` into `_isSimpleAlias()` and `_isUnionAlias()`
   - Rewrote `_resolveAliases()` for two-phase processing
   - Added `_applyComplementToAlt()` for recursive complement application

4. **Test Files Created**
   - `valid/union_alias_basic.glp` - Basic union of two types
   - `valid/union_alias_three.glp` - Union of three types  
   - `valid/union_alias_simple.glp` - Minimal test (✅ PASSES)
   - `invalid/union_alias_overlap.glp` - Overlapping functors error
   - `invalid/union_alias_refs_alias.glp` - Reference to alias error

### Bugs Found and Fixed During Implementation

#### Bug A: Alias resolution before prelude merge
**Problem:** Union aliases in user code couldn't reference prelude types because alias resolution happened during `_buildEnvironmentFromModule`, before prelude was merged.

**Fix:** Restructured `buildTypeEnvironment()` to resolve aliases AFTER merging prelude and user environment.

#### Bug B: Prelude's `Constant` misclassified as union alias
**Problem:** `Constant ::= Number ; String.` was being treated as a union alias because it has multiple TypeRef alternatives. But `Number` and `String` are predefined primitives, not user-defined types to expand.

**Fix:** Updated `_isUnionAlias()` to return `false` when any referenced type is predefined:
```dart
if (isPredefinedType(typeName)) {
  return false;  // Not a union alias
}
```

### Next Steps for Union Alias

1. Run updated tests:
   ```bash
   cd /Users/udi/Grassroots/GLP/glp_runtime
   dart run bin/glpc.dart --type-check ../test/analysis/type_checker/valid/union_alias_basic.glp
   dart run bin/glpc.dart --type-check ../test/analysis/type_checker/valid/union_alias_three.glp
   dart run bin/glpc.dart --type-check ../test/analysis/type_checker/invalid/union_alias_overlap.glp
   dart run bin/glpc.dart --type-check ../test/analysis/type_checker/invalid/union_alias_refs_alias.glp
   ```

2. Run full regression suite

3. After union alias expansion works, revisit Bug 2 (Msg? transitions)
