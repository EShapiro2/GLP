# Current Plan: Subtyping Implementation

Started: 2026-02-21  
Branch: `subtyping`  
Spec: `docs/type system/subtyping.md`  
Paper: Section 4.6, Definitions 4.5–4.9

## 🔴 Branch Instructions

**Work on the existing `subtyping` branch.** Do NOT create a new `claude/...` branch.

```bash
git checkout subtyping
git pull origin subtyping
```

All commits go on this branch. When done, the user will merge `subtyping` into `main`.

## Steps

- [x] 1. Create test programs (positive and negative)
- [ ] 2. Run baseline tests, commit ← CURRENT
- [ ] 3. Create `subtyping.dart` module
- [ ] 4. Integrate into `well_typed_clause.dart`
- [ ] 5. Add tests to `run_all_tests.sh`
- [ ] 6. Run full test suite, verify green
- [ ] 7. Final commit, offer merge

## Context

The type checker currently requires exact type duality for body-body variable pairs (writer X at type S, reader X? at type T?: S must equal T). The paper relaxes this to S <: T (subtyping). This blocks programs where a producer emits a subset of messages that a consumer accepts (e.g., read-only client connected to a full file-system monitor).

## Step Details

### Step 2: Baseline

```bash
cd /Users/udi/Grassroots/GLP
bash test/run_all_tests.sh
```

All 317 existing tests must pass before any code changes. Commit baseline if clean.

### Step 3: Create `subtyping.dart`

**File**: `glp_runtime/lib/analysis/type_checker/subtyping.dart`

**Spec**: `docs/type system/subtyping.md`, Section 4 (Algorithm on the DFA)

**Public API**:
```dart
/// Check if output type A is a subtype of output type B.
/// Both stateA and stateB must be output types (isDual == false).
/// Paper Reference: Definition 4.7 (Subtyping)
bool isSubtype(DFAState stateA, DFAState stateB, ProgramDFA dfa);
```

**Implementation per spec section 4.1–4.5**:

1. Coinductive visited set: `Set<(DFAState, DFAState)>` (use a Set of string keys `"${a.name}:${b.name}"` for efficiency)
2. Reflexivity: `stateA == stateB → true`
3. Wildcard top: `stateB` is `_` → true; `stateA` is `_` and `stateB` is not `_` → false
4. Primitive lattice (spec section 4.3):
   - `Integer <: Number` ✓, `Real <: Number` ✓
   - Any output type `<: _` ✓
   - Otherwise primitives must be identical
5. User-defined types: iterate transitions of automaton A; for each, find matching transition in automaton B. If no match → false. If match, check target compatibility (spec section 4.2):
   - Both output → recurse covariantly
   - Both dual → extract base types, recurse contravariantly (reversed)
   - Mixed → false
6. Handle `_FINAL_` state: treat as equivalent to `_` for subtyping purposes (it's a terminal acceptance state)

**Unit test file**: `glp_runtime/test/analysis/type_checker/subtyping_test.dart`

Test cases:
- Reflexivity: Stream <: Stream
- Wildcard top: Stream <: _
- Wildcard not bottom: _ ≮: Stream (when Stream has structure)
- Primitive lattice: Integer <: Number, Real <: Number, Integer ≮: String
- Simple fewer alternatives: {a, b} <: {a, b, c}
- Wrong direction: {a, b, c} ≮: {a, b}
- Contravariance at mode inversion
- Coinductive cycle: recursive types (Stream <: Stream via cycle)
- Disjoint types: fail

### Step 4: Integrate into `well_typed_clause.dart`

**Spec**: `docs/type system/subtyping.md`, Section 5

**Change**: In `_checkClauseDuality`, for body-body pairs, replace the exact duality check with a subtyping check.

Current code (in `_checkClauseDuality`):
```dart
if (writerNormLoc == readerNormLoc) {
  // Both in head OR both in body: require DUAL types
  final (isCompat, reason) = _areDualTypesWithReason(writerInfo, readerInfo);
```

Change to:
```dart
if (writerNormLoc == readerNormLoc) {
  if (writerNormLoc == 'head') {
    // Both in head: require exact DUAL types (unchanged)
    final (isCompat, reason) = _areDualTypesWithReason(writerInfo, readerInfo);
    ...
  } else {
    // Both in body: require subtyping (S <: T)
    // Writer X has output type S. Reader X? has dual type T?.
    // Need: S <: T (both output types).
    final writerOutputState = writerInfo.typeState;  // S (output, not dual)
    final readerDualState = readerInfo.typeState;     // T? (dual)
    final readerOutputState = dfa.getState(readerDualState.baseName); // T (output)
    final isSub = isSubtype(writerOutputState, readerOutputState, dfa);
    if (!isSub) {
      errors.add(ClauseDualityError(...));
    }
  }
}
```

**IMPORTANT**: `_checkClauseDuality` currently does NOT receive the ProgramDFA. Its signature must be extended to accept it:
```dart
List<ClauseDualityError> _checkClauseDuality(
  Map<String, VariableTypeInfo> variableTypes,
  Map<String, String> variableLocations,
  ProgramDFA dfa,   // NEW PARAMETER
)
```

And the call site in `checkClause` must pass `dfa` through.

### Step 5: Add tests to `run_all_tests.sh`

**Positive tests** — add to `POSITIVE_FILES` array in Section B:
```bash
"$TC_DIR/positive/subtyping/basic_readop_fileop.glp"
"$TC_DIR/positive/subtyping/constants_fewer_alternatives.glp"
"$TC_DIR/positive/subtyping/contravariant_response_slot.glp"
"$TC_DIR/positive/subtyping/direct_constant_subtype.glp"
"$TC_DIR/positive/subtyping/struct_fewer_functors.glp"
```

**Negative tests** — add to `NEGATIVE_FILES` array in Section C:
```bash
"$TC_DIR/negative/subtyping/wrong_direction_fileop_readop.glp"
"$TC_DIR/negative/subtyping/contravariant_wrong_direction.glp"
"$TC_DIR/negative/subtyping/disjoint_types.glp"
"$TC_DIR/negative/subtyping/arg_type_mismatch.glp"
```

### Step 6: Full test suite

```bash
cd /Users/udi/Grassroots/GLP
bash test/run_all_tests.sh
```

**Expected**: All 317 existing tests still pass + 5 new positive + 4 new negative = 326 total.

### Step 7: Commit and merge

```bash
git add -A
git commit -m "Implement subtyping for body-body variable pairs (Definition 4.7)"
```

Then offer merge instructions to user.

## Key Invariants

- Head-head pairs: UNCHANGED (exact duality required)
- Head-body pairs: UNCHANGED (same type required)
- Body-body pairs: RELAXED from exact duality to subtyping
- All 317 existing tests must continue to pass (subtyping is a relaxation, not a restriction)
- No changes to DFA construction, moded term construction, or input coverage
