# Session 2: Typed REPL Test Suite Remediation
**Date:** 2026-01-16
**Goal:** Get typed REPL test suite from 126/222 to 222/222 passing
**Test Location:** `/Users/udi/Grassroots/GLP/test/run_typechecker_repl_tests.sh`

---

## Mission

Fix all failing tests in the typed REPL test suite by:
1. Identifying root cause of each failure
2. Fixing programs that need correction
3. Marking ill-typed programs appropriately
4. Moving misclassified tests to correct category
5. Reporting type checker bugs to Session 1

## Current Baseline (ESTABLISHED 2026-01-16)

**Results:**
- Total tests: 222 (183 positive + 37 negative + 2 SRSW)
- Currently passing: 126 (56.7%)
- Currently failing: 96 (43.3%)
  - Positive tests failing: 87/183 (47.5%)
  - Negative tests passing: 37/37 (100%) ✅
  - SRSW tests passing: 2/2 (100%) ✅

**Failure Breakdown:**
- "unexpected type errors": 64 programs
- "loading error": 32 programs

**By Category (failures only):**
- recursive/list_processing: 8
- recursive/structure_processing: 9
- streams/producers_consumers: 11
- streams/buffered_communication: 3
- streams/objects_monitors: 9
- meta/*: 10
- social_graph: 23
- social_networks: 13
- constitutional_consensus: 6
- cryptocurrencies: 6
- Other: 2

**Output:** `/Users/udi/Grassroots/GLP/test_output/typed_repl_baseline.txt`

---

## Test Suite Structure

### Test Script:
`/Users/udi/Grassroots/GLP/test/run_typechecker_repl_tests.sh`

### Test Categories:

**1. Positive Tests (183 programs - should load successfully):**
   - `/Users/udi/Grassroots/GLP/glp_runtime/test/programs/typechecker/positive/` (12)
   - `/Users/udi/Grassroots/GLP/glp_runtime/test/programs/moded_types/valid/` (31)
   - `/Users/udi/Grassroots/GLP/programs/typed_book/` (140)

**2. Negative Tests (37 programs - should be rejected):**
   - `/Users/udi/Grassroots/GLP/glp_runtime/test/programs/typechecker/negative/` (18)
   - `/Users/udi/Grassroots/GLP/glp_runtime/test/programs/moded_types/invalid/` (19)

**3. SRSW Tests (2 programs - should be rejected by parser):**
   - `/Users/udi/Grassroots/GLP/glp_runtime/test/programs/typechecker/negative/head/` (2)

---

## Workflow for Each Failing Test

### Step 1: Run Test Suite
```bash
cd /Users/udi/Grassroots/GLP && \
./test/run_typechecker_repl_tests.sh > /Users/udi/Grassroots/GLP/test_output/current_test.txt 2>&1
```

### Step 2: Identify Failures
Look for `FAIL:` lines in output

### Step 3: Get Detailed Error
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && \
dart run bin/check_types.dart /Users/udi/Grassroots/GLP/programs/typed_book/[path]/[file].glp
```

### Step 4: Read the Program Source
**CRITICAL:** Always read the actual program file:
```bash
cat /Users/udi/Grassroots/GLP/programs/typed_book/[path]/[file].glp
```

### Step 5: Triage and Diagnose

**A. Type Checker Bug?**
- Error message doesn't make sense per specification
- Program is clearly well-typed but rejected
- **Diagnosis Required**

**B. Program has SRSW Violation?**
- Uses `_` in body (not allowed)
- Multiple writer occurrences (violates SRSW)
- Reader in output position (unless arithmetic guard)
- **Diagnosis Required**

**C. Program Needs Type Annotation Fix?**
- Missing procedure type declarations
- Incorrect mode annotations
- Missing type definitions
- **Diagnosis Required**

**D. Program is Genuinely Ill-Typed?**
- Type mismatch in program design
- Missing clause coverage
- Undefined procedures
- **Diagnosis Required**

### Step 6: Present Diagnosis to User

**CRITICAL: DO NOT FIX ANYTHING WITHOUT USER APPROVAL**

For each diagnosed issue, present to user (can batch 3-5 at a time):

```
DIAGNOSIS #[N]: [program name]

File: [full filepath]

Category: [Type Checker Bug | Program Bug | Type Definition Issue]

Error Message:
[exact error from type checker]

Program Excerpt:
[relevant lines of code]

Diagnosis:
[Detailed explanation of what's wrong]

Proposed Fix:
[Exactly what needs to change]

Justification:
[Why this fix is correct per specification]

Awaiting approval to proceed.
```

### Step 7: After User Approval

**If approved as Program Bug or Type Definition Issue:**
- Make the fix yourself
- Test that it type checks
- Report completion
- Move to next test

**If approved as Type Checker Bug:**
- Hand off to Session 1 (Type Checker Standby) with:
  - Program filepath
  - Error message
  - Your diagnosis
  - Spec reference
- Wait for Session 1 to fix
- Verify fix works
- Move to next test

---

## Tools and Commands

### Run Full Test Suite:
```bash
cd /Users/udi/Grassroots/GLP && \
./test/run_typechecker_repl_tests.sh > /Users/udi/Grassroots/GLP/test_output/current_test.txt 2>&1
```

### Check Single File:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && \
dart run bin/check_types.dart <filepath.glp>
```

### View Program:
```bash
cat /Users/udi/Grassroots/GLP/programs/typed_book/[category]/[program].glp
```

---

## Known Issues to Watch For

### 1. Programs Already Marked ILL-TYPED:

These programs have `% STATUS: ILL-TYPED` comments:
- `sum_list.glp` - SRSW violations in reduce/2 meta-interpreter
- `bubble_sort.glp` - Uses `_` in body
- `cooperative.glp` - Uses `_` in body

Check if these are in correct test category.

### 2. Arithmetic Expression Support:

Type checker supports arithmetic expressions in guards:
- `+`, `-`, `*`, `/`, `//`, `mod`
- These should work in guard conditions

### 3. Parser Issues vs Type Checker Issues:

If a program fails to parse:
- This is NOT a type checker issue
- Note it but don't try to fix via type annotations
- Report to user for parser fix

---

## Progress Tracking

Maintain: `/Users/udi/Grassroots/GLP/test_output/remediation_progress.txt`

Format:
```
Test Remediation Progress
Starting: 126/222 passing (56.7%)

Batch 1: [date]
- Fixed: [programs]
- Moved to negative: [programs]
- Reported bugs: [list]
- Current: Y/222 passing

Batch 2: ...
```

---

## Approval Workflow

**IMPORTANT: All fixes require user approval before implementation**

### Workflow Summary:
1. Diagnose issue (program bug vs type checker bug)
2. Present diagnosis to user with proposed fix
3. **WAIT for user approval**
4. After approval:
   - Program/type definition bugs: Fix yourself
   - Type checker bugs: Hand to Session 1
5. Test and report completion

### Batching for Efficiency:
**Recommended:** Diagnose 3-5 issues before presenting to user
- Group similar issues together
- Present as a batch for more efficient approval
- User can approve all at once or individually
- Allows you to work through approvals systematically

### What You Can Fix After Approval:
- GLP program source code (.glp files)
- Type declarations in programs
- Procedure type annotations
- Test suite classification (POSITIVE_FILES ↔ NEGATIVE_FILES)

### What Gets Handed to Session 1:
- Type checker implementation bugs
- Specification gaps or ambiguities
- Core GLP runtime issues

---

## Communication with Other Sessions

### To User (Udi) - REQUIRED BEFORE ANY FIX:
```
DIAGNOSIS #[N]: [program name]

File: [filepath]
Category: [Type Checker Bug | Program Bug | Type Definition Issue]

Error Message:
[exact error]

Diagnosis:
[what's wrong]

Proposed Fix:
[what to change]

Justification:
[why this is correct]

Awaiting approval to proceed.
```

### Report to Session 1 (After User Approves Type Checker Bug):
```
BUG REPORT (USER APPROVED):
Program: [filepath]
Error: [exact error message]
Why should be valid: [explanation per spec]
Spec reference: [which spec file, section]
User approval: [date/time]
```

---

## Success Criteria

**Goal:** 222/222 tests passing

This means:
- All positive tests load successfully (no type errors)
- All negative tests are properly rejected (type errors as expected)
- All SRSW tests are rejected by parser
- No genuine type checker bugs remain
- Test classifications are correct

---

## Development Principles

1. **RTFM:** Always read actual program source files
2. **Never assume:** Don't trust old information or snippets
3. **One at a time:** Fix/triage one test at a time
4. **Verify:** Always re-run after changes
5. **Positive + Negative:** Maintain proper test balance
6. **Document:** Keep progress log updated

---

## Getting Started

**Step 1:** ✅ DONE - Baseline established

**Step 2:** Categorize failures by type:
```bash
cd /Users/udi/Grassroots/GLP && \
grep "FAIL:" test_output/typed_repl_baseline.txt | \
grep "unexpected type errors" | wc -l  # Should show 64

grep "FAIL:" test_output/typed_repl_baseline.txt | \
grep "loading error" | wc -l  # Should show 32
```

**Step 3:** Start with easiest category (e.g., simple list processing)

**Step 4:** Follow workflow for each failure

**Step 5:** Report progress and issues

---

## Status

🟢 **Ready to start - Baseline established**

Full baseline: `/Users/udi/Grassroots/GLP/test_output/test_baseline_summary.txt`

**Last Updated:** 2026-01-16
