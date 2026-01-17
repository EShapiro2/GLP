# Session 3: Book Program Conversion to Typed GLP
**Date:** 2026-01-16
**Goal:** Convert all untyped book programs to well-typed GLP programs
**Source:** `/Users/udi/Grassroots/GLP/programs/book/`
**Target:** `/Users/udi/Grassroots/GLP/programs/typed_book/`

---

## Mission

Convert all programs in `programs/book/` to properly typed programs in `programs/typed_book/` by:
1. Adding type declarations for all custom types
2. Adding procedure type annotations
3. Ensuring programs type check correctly
4. Adding programs to test suite
5. Maintaining original program semantics

## Current State (ESTABLISHED 2026-01-16)

**Baseline:**
- Main REPL: 222/222 tests pass (untyped programs work) ✅
- Typed REPL: 126/222 tests pass (typed programs)

**Source Programs:** `~/programs/book/`
- Total: ~140+ programs across multiple categories
- Status: Original untyped GLP programs
- Organization: By chapter/topic from GLP book

**Target Programs:** `~/programs/typed_book/`
- Current: ~140 programs present
- Type checking: 87/183 positive tests failing (47.5%)
- Issue: Many typed programs don't type check yet

**Note:** Session 2 is working on fixing existing typed_book programs. This session focuses on:
1. Converting any missing programs from book/ to typed_book/
2. Helping Session 2 with type annotation fixes
3. Ensuring all conversions maintain semantics

---

## Directory Structure

```
programs/book/                    programs/typed_book/
├── constants/                    ├── constants/
├── constitutional_consensus/     ├── constitutional_consensus/
├── cryptocurrencies/            ├── cryptocurrencies/
├── meta/                        ├── meta/
│   ├── basic/                   │   ├── plain/
│   ├── debugging/               │   ├── enhanced/
│   ├── enhanced/                │   └── debugging/
│   └── plain/
├── modules/                     ├── modules/
├── recursive/                   ├── recursive/
│   ├── arithmetic_trees/        │   ├── arithmetic_trees/
│   ├── list_processing/         │   ├── list_processing/
│   └── structure_processing/    │   └── structure_processing/
├── social_graph/                ├── social_graph/
├── social_networks/             ├── social_networks/
└── streams/                     └── streams/
```

---

## Conversion Workflow

### Step 1: Identify Work Needed

**A. Find missing programs:**
```bash
# List all book programs
find /Users/udi/Grassroots/GLP/programs/book -name "*.glp" -type f > /tmp/book_all.txt

# Compare with typed_book to find gaps
```

**B. Coordinate with Session 2:**
- Session 2 is fixing existing typed programs that fail
- This session can help by providing correct type annotations
- Avoid duplicate work

### Step 2: For Each Program to Convert

**A. Read Original Program:**
```bash
cat /Users/udi/Grassroots/GLP/programs/book/[category]/[program].glp
```

**B. Understand Program Structure:**
- What types are used?
- What are the input/output modes?
- What procedures are defined?
- Are there any SRSW violations?

**C. Create Typed Version:**

1. **Add type declarations:**
```glp
% Type declarations
type Nat = z | s(Nat).
type List(T) = [] | [T | List(T)].
type Channel(T) = ch(T?, T).
```

2. **Add procedure type annotations:**
```glp
% Procedure: append/3
% Type: append(List(T)↓, List(T)↓, List(T)↑)
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

**D. Verify Type Checking:**
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && \
dart run bin/check_types.dart /Users/udi/Grassroots/GLP/programs/typed_book/[category]/[program].glp
```

**E. Present to User for Approval:**

**CRITICAL: DO NOT SAVE OR COMMIT WITHOUT USER APPROVAL**

Present each conversion (can batch 2-3 at a time):

```
CONVERSION: [program name]

Original: [filepath in book/]
Converted: [filepath in typed_book/]

Original Program:
[show original code]

Converted Program with Type Annotations:
[show converted code with types]

Type Check Result:
[PASS or error message]

Changes Made:
1. Added type declarations: [list types]
2. Added procedure annotations: [list procedures]
3. [Any other changes]

Semantics Preserved: [Yes/No with explanation]

Awaiting approval to save.
```

**F. After User Approval:**

**If Type Check Passes:**
- Save to typed_book/
- Add to test suite
- Report completion

**If Type Check Fails:**
- Present error diagnosis
- Determine: Program needs fix OR Type Checker bug
- Get user approval for next step
- If Type Checker bug: Hand to Session 1

---

## Type Annotation Guide

### Basic Types

**Natural Numbers:**
```glp
type Nat = z | s(Nat).

% Procedure: plus/3
% Type: plus(Nat↓, Nat↓, Nat↑)
plus(z, N, N).
plus(s(M), N, s(P)) :- plus(M, N, P).
```

**Lists:**
```glp
type List(T) = [] | [T | List(T)].

% Procedure: append/3
% Type: append(List(T)↓, List(T)↓, List(T)↑)
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```

**Binary Trees:**
```glp
type Tree(T) = leaf | node(Tree(T), T, Tree(T)).

% Procedure: tree_sum/2
% Type: tree_sum(Tree(Nat)↓, Nat↑)
tree_sum(leaf, z).
tree_sum(node(L, X, R), Sum) :-
    tree_sum(L, SL),
    tree_sum(R, SR),
    plus(SL, X, Tmp),
    plus(Tmp, SR, Sum).
```

### Interactive Types

**Channels:**
```glp
type Channel(T) = ch(T?, T).

% Procedure: new_channel/1
% Type: new_channel(Channel(T)↑)
new_channel(ch(X?, X)).
```

**Monitors:**
```glp
type Counter = count(Nat↓, Nat↑).

% Procedure: counter/3
% Type: counter(Nat↓, Counter, Counter)
counter(N, count(inc, M), count(M, M1)) :- 
    plus(N, s(z), M), 
    counter(M, M1, M1).
counter(N, count(get, N), M) :- counter(N, M, M).
```

### Universal Types

**When type is truly universal:**
```glp
type Any = <universal>.

% Procedure: copy/2
% Type: copy(Any↓, Any↑)
copy(X, X).
```

---

## Common Conversion Issues

### Issue 1: Using `_` in Body

**Problem:**
```glp
process(X, _) :- compute(X).  % SRSW violation: _ in body
```

**Solution:**
Use a named variable:
```glp
process(X, Y) :- compute(X).  % Y is writer at output, never read
```

### Issue 2: Multiple Writer Occurrences

**Problem:**
```glp
duplicate(X, X, X).  % SRSW violation
```

**Solution:**
```glp
duplicate(X, Y, Z) :- Y = X, Z = X.
```

### Issue 3: Mode Annotation Errors

**Problem:**
Incorrect ↑/↓ annotations

**Solution:**
Analyze data flow:
- ↓ (consume/input): variable is read from
- ↑ (produce/output): variable is written to

---

## Priority Order

**Phase 1: Simple Recursive (Coordinate with Session 2):**
- Most basic programs likely already converted
- Help Session 2 fix type annotations

**Phase 2: Check for Missing Programs:**
- Compare book/ vs typed_book/ directories
- Identify any programs not yet converted

**Phase 3: Complex Interactive Types:**
- Social networks, consensus, cryptocurrencies
- These may need Session 2 to fix first

---

## Tools and Commands

### Type Check Converted Program:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && \
dart run bin/check_types.dart /Users/udi/Grassroots/GLP/programs/typed_book/[path]/[file].glp
```

### Compare Original vs Typed:
```bash
diff /Users/udi/Grassroots/GLP/programs/book/[path]/[file].glp \
     /Users/udi/Grassroots/GLP/programs/typed_book/[path]/[file].glp
```

---

## Approval Workflow

**IMPORTANT: All conversions and fixes require user approval before saving**

### Workflow Summary:
1. Convert program (add type annotations)
2. Type check the conversion
3. Present conversion to user with results
4. **WAIT for user approval**
5. After approval:
   - If passes: Save and add to test suite
   - If fails with program issue: Fix and re-present
   - If fails with type checker bug: Hand to Session 1
6. Report completion

### Batching for Efficiency:
**Recommended:** Complete 2-3 conversions before presenting to user
- Group similar programs together (e.g., all list processing)
- Present as a batch for more efficient approval
- User can approve all at once or individually
- Allows systematic progress through categories

### What You Can Do After Approval:
- Save converted programs to typed_book/
- Fix type annotations in programs
- Add type declarations
- Update test suite

### What Gets Handed to Session 1:
- Type checker implementation bugs
- Specification gaps or ambiguities

---

## Communication with Other Sessions

### To User (Udi) - REQUIRED BEFORE SAVING:
```
CONVERSION: [program name]

Original: [filepath in book/]
Converted: [filepath in typed_book/]

Original Program:
[show original]

Converted Program:
[show with type annotations]

Type Check Result:
[PASS or error]

Changes Made:
[list changes]

Semantics Preserved: [Yes/No]

Awaiting approval to save.
```

### With Session 2:
- Coordinate on which programs to work on
- Share type annotation insights
- Help fix type errors in existing programs

### Report to Session 1 (After User Approves Type Checker Bug):
```
BUG REPORT (USER APPROVED):
Program: [filepath]
Type annotations: [show relevant parts]
Error: [exact error message]
Why should be valid: [explanation]
User approval: [date/time]
```

---

## Success Criteria

**Goal:** All book programs converted and properly typed

This means:
- Every program in book/ has typed version in typed_book/
- All well-typed programs type check successfully
- Ill-typed programs marked as such
- All converted programs in test suite
- No loss of original program semantics

---

## Development Principles

1. **RTFM:** Always read original program fully
2. **Preserve Semantics:** Typed version must behave identically
3. **Coordinate:** Work with Session 2 to avoid duplication
4. **Test Immediately:** Type check after each conversion
5. **Document Decisions:** Comment non-obvious type choices

---

## Getting Started

**Step 1:** ✅ DONE - Baseline established

**Step 2:** Coordinate with Session 2
- Understand what they're working on
- Identify gaps in coverage
- Decide division of labor

**Step 3:** Start systematic conversion/fixing

**Step 4:** Report progress

---

## Status

🟢 **Ready to start - Baseline established**

**Recommended approach:** Wait for Session 2 to start and coordinate work division

Full baseline: `/Users/udi/Grassroots/GLP/test_output/test_baseline_summary.txt`

**Last Updated:** 2026-01-16
