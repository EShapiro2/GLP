# Bug: Path checking fails for nested compound types

**Date**: 2025-01-12
**Status**: OPEN - Awaiting paper revision
**Severity**: High (blocks hollow message examples from paper)

## Symptom

Path checking fails with "No transition for X from state Y" when:
1. A type contains another compound type (not just primitives)
2. Path traversal crosses from outer type to inner type

Example failing program:
```glp
CounterCall ::= add ; clear ; read(Integer?).
CallStream ::= [] ; [CounterCall|CallStream].

procedure monitor_loop(Integer?, CallStream?).

monitor_loop(N, [read(N?)|In]) :- monitor_loop(N?, In?).
```

Error: `No transition for read(1,1):↑ from state CounterCall?`

## Root Cause

In `well_typed_term.dart`, `checkPathAgainstAutomaton()` uses a single automaton throughout path traversal. When the path crosses from `CallStream?` into a `CounterCall?` element, it continues using the `CallStream?` automaton instead of switching to the `CounterCall?` automaton.

The `CallStream?` automaton has transitions:
- `[] → _FINAL_`
- `[|](2,1):↓ → CounterCall?`
- `[|](2,2):↓ → CallStream?`

But it does NOT have transitions FROM `CounterCall?` state because those belong to the `CounterCall?` automaton.

## Expected Behavior

When path traversal reaches a new type state (like `CounterCall?`), it should:
1. Recognize that the current state is the start of a different type's automaton
2. Switch to using that type's automaton for further transitions
3. Continue path checking within the new automaton

## Affected Files

- `lib/analysis/type_checker/well_typed_term.dart` - `checkPathAgainstAutomaton()`
- `lib/analysis/type_checker/program_dfa.dart` - May need to support automaton switching

## Paper Reference

This bug blocks the monitor example from Section 4.2 (lines 204-212) which uses:
- Interactive type `CounterCall ::= add ; clear ; read(Integer?)`
- Hollow messages with embedded readers

## Notes

Awaiting paper revision before implementing fix. The spec may change regarding how nested types are handled in path checking.

## Test Case

File: `test/programs/typechecker/positive/monitor_full.glp` (to be created after fix)
