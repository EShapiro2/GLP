# Handover: Fix `wait_until` Runtime Implementation

## What Changed

The paper (`GLP-ICLP-2026/sections/appendix-guards.tex`) and both spec files (`GLP/docs/guards-reference.md`, `GLP/docs/glp-predicate-taxonomy.md`) have been updated. `wait_until(T)` now suspends until time T passes, rather than failing.

## The Fix

**File**: `GLP/glp_runtime/lib/bytecode/runner.dart`

In method `_evaluateGuard`, the `case 'wait_until':` block currently returns `GuardResult.failure` when `now < timestamp`. It should instead use the same timer-based suspension mechanism as `wait`.

**Current (wrong)**:
```dart
case 'wait_until':
    if (args.isEmpty) return GuardResult.failure;
    final timestamp = evaluateNumeric(args[0]);
    if (timestamp == null) return GuardResult.failure;
    final now = DateTime.now().millisecondsSinceEpoch;
    return now >= timestamp ? GuardResult.success : GuardResult.failure;
```

**Correct**: When `now < timestamp`, compute `remaining = timestamp - now`, then do exactly what `wait` does for `duration > 0` — allocate reader/writer pair, start timer for `remaining` ms, add reader to suspension set `U`, return `GuardResult.failure` (which triggers suspension since `U` is non-empty). The `wait` case in the same method has the exact code to copy from.

**Steps**:
1. Read the `case 'wait':` block in `_evaluateGuard` to see the timer/suspension pattern
2. In `case 'wait_until':`, when `now < timestamp`:
   - Compute `final remaining = timestamp.toInt() - now;`
   - Copy the timer + reader/writer + suspension logic from `wait`
   - Use `remaining` as the timer duration
3. Update the comment at the top of the case to say "Suspend" not "FAIL"

## Testing

After the fix, `wait_until(T)` where T is in the future should suspend (not fail), and resume when time T arrives. Existing tests using `wait_until(0)` should still pass since `now >= 0` is always true (immediate success).

## Warning

The bond agent code (`GLP/programs/typed_book/bonds/bond_agent.glp`) has `select_bonds_min_maturity` which uses `wait_until`/`otherwise` as a branch. This will break because `wait_until` will now suspend instead of failing, so `otherwise` won't fire. **Do NOT fix the bond agent code** — that's a separate task for Udi to decide on.
