# Handover: Explicit Dual Type Definition Fix

**Date**: 2026-01-25
**From**: Claude Web
**To**: Claude Code

## Baseline Confirmed

After reverting `program_dfa.dart`, the baseline shows exactly **ONE error**:

```
Type Errors (1):
  ✗ Head of network2 is not well-typed:
  Inconsistent path: Variable mode mismatch: reader requires ↓ (consume), got ↑ (produce)
  Path: (ch/2, 0, input) → (_#1?, 2, output)
  Inconsistent path: Variable mode mismatch: reader requires ↓ (consume), got ↑ (produce)
  Path: (ch/2, 0, input) → (_#2?, 2, output) at line 169, column 1
    in: network2(2 args).
```

## The Problem

In `play_alice_bob_full.glp`, the type definition is:
```prolog
Channel ::= ch(Stream, Stream?).
```

The `network2` procedure is declared as:
```prolog
procedure network2(Channel?, Channel?).
```

At line 169:
```prolog
network2(ch([], _), ch([], _)).
```

The issue: When `Channel` appears at `Channel?` position (consumed), the type checker applies recursive complementation which flips the internal modes. Position 2 (`Stream?`) becomes `Stream` (produce mode). But the anonymous variable `_` at that position is converted to a reader `_#1?` (per paper Remark 3.1: each `_` is a fresh writer), and readers require consume mode.

## Root Cause

The type checker computes `Channel?` by flipping all internal modes of `Channel`:
- `Channel ::= ch(Stream, Stream?)` 
- Computed `Channel? = ch(Stream?, Stream)` (modes flipped)

But Channel has **fixed semantics**:
- Position 1: always input stream (partner reads from it)
- Position 2: always output stream (partner writes to it)

These semantics should NOT change based on whether we're producing or consuming the Channel.

## The Solution: Explicit Dual Definitions

The paper now requires explicit dual definitions for ADTs like Channel:

```prolog
Channel ::= ch(Stream, Stream?).
Channel? ::= ch(Stream, Stream?)?.
```

The `?` on the entire structure marks it as a dual but **preserves internal structure**.

## Implementation Tasks

1. **Parser**: Already updated to accept `T? ::= functor(...)?.` syntax ✓

2. **DFA Builder** (`program_dfa.dart`): Needs to recognize explicit dual definitions (type names ending in `?`) and use their definition directly rather than computing via recursive complementation.

3. **Moded Head** (`moded_head.dart`): May need updates to look up explicit dual definitions when computing embedded modes.

## Key Files

- Test file: `/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/play_alice_bob_full.glp`
- DFA builder: `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/program_dfa.dart`
- Moded head: `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/moded_head.dart`

## Testing

After any fix, run:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart run bin/check_types.dart ../programs/typed_book/social_graph/play_alice_bob_full.glp
```

Expected result: 0 type errors (only warnings about undefined procedures are acceptable).

## Important Notes

- Do NOT introduce regressions - all existing tests should still pass
- Run `dart test` before committing any changes
- The fix should be minimal and targeted at the explicit dual handling
