# Step 2.4e Final: Remove Monomorphic Types from Root self.glp

**Prerequisite**: All test-critical batches F–J done. 390/390 tests passing.

**Goal**: Remove the monomorphic type definitions from `programs/self.glp`. This is the culmination of Step 2.4.

---

## Step 1: Baseline

Run `bash test/run_all_tests.sh`. Confirm 390 pass. Commit any uncommitted work.

## Step 2: Remove monomorphic definitions

In `programs/self.glp`, remove these lines:

```
% Collections (monomorphic — kept for backward compatibility)
Stream ::= [] ; [_|Stream].
OpenStream ::= [_|Stream].
DiffList ::= Stream \ Stream?.
```

and:

```
% Communication (monomorphic — kept for backward compatibility)
Channel ::= ch(Stream, Stream?).
```

Keep the parameterized definitions and update the comments:

```
% Collections
Stream(X) ::= [] ; [X | Stream(X)].
OpenStream(X) ::= [X | Stream(X)].
DiffList(X) ::= Stream(X) \ Stream(X)?.

% Communication
Channel(In, Out) ::= ch(In, Out?).
```

## Step 3: Run tests

Run `bash test/run_all_tests.sh`.

If all 390 pass — commit: `feat(types): remove monomorphic Stream/Channel/DiffList/OpenStream from self.glp`

If tests fail — the failures identify files that still reference bare monomorphic types and were missed in earlier batches. For each failing file:
1. Identify the bare reference (grep for `Stream`, `Channel`, `DiffList`, `OpenStream` without `(`)
2. Apply the same conversion principles used in earlier batches:
   - Local imprecise copy like `MsgStream ::= [] ; [_ | MsgStream].` → remove, use `Stream(Msg)` 
   - Local `Channel ::= ch(MsgStream?, MsgStream).` → remove, use `Channel(MsgStream, MsgStream)`
   - Bare `Stream` in proc decls → `Stream(X)` or concrete instantiation
   - Bare `Channel` in type defs → precise channel type or `Channel(In, Out)` instantiation
3. Run tests again after each fix

## Step 4: Update current_plan.md

Mark Step 7 (Step 2.4) as done. Update context to reflect the new state.

## Step 5: Push

Offer to push all commits.
