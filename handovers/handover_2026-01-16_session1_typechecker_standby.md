# Session 1: Type Checker Bug Standby
**Date:** 2026-01-16
**Status:** Standing by for bug reports
**Primary Contact:** This Claude instance (oversight/coordination)

---

## Mission

Monitor all parallel sessions and immediately debug/fix any type checker bugs discovered during test suite remediation or book program conversion.

## Current Baseline (ESTABLISHED 2026-01-16)

**Test Results:**
- Main REPL: 222/222 PASS (100%) ✅
- Typed REPL: 126/222 PASS (56.7%), 96 FAIL

**Type Checker Status:**
- Implementation: 100% complete
- Known bugs: **NONE**
- All 9 specification modules: complete and aligned
- Negative tests: 37/37 PASS ✅
- SRSW tests: 2/2 PASS ✅

**Failures to investigate:**
- 87 positive tests failing (should pass but don't)
- 64 with "unexpected type errors"
- 32 with "loading error"

**Specifications Location:**
- `/Users/udi/Grassroots/GLP/docs/type system/` (11 files)

**Implementation Location:**
- `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/`

---

## Responsibilities

### 1. Monitor for Bug Reports

Watch for reports from:
- **Session 2** (test suite fixing) - may discover type checker bugs during remediation
- **Session 3** (book program conversion) - may discover bugs when adding type annotations

### 2. Triage Bug Reports

For each failure reported:
1. **Get the failing program source** - NEVER assume, always read actual file
2. **Get the error message** - exact type checker output
3. **Read the specification** - what should happen per spec?
4. **Read the implementation** - what is actually happening?
5. **Determine root cause:**
   - Type checker bug → Fix immediately
   - Program bug (SRSW violation, ill-typed) → Report back to originating session
   - Test classification error → Report back to originating session

### 3. Fix Type Checker Bugs

When a genuine type checker bug is found:

**Process:**
1. **Identify which spec module is affected**
2. **Read the complete spec file** for that module
3. **Read the complete implementation file** for that module
4. **Determine exact fix needed**
5. **Verify fix doesn't break existing functionality**
6. **Prepare exact code changes** for Claude Code
7. **Update relevant spec if needed** (Paper → Spec → Implementation)
8. **Create positive and negative test cases**

**Never:**
- Add workarounds or layers
- Make assumptions about file contents
- Change specs to match implementation
- Push to main without testing

---

## Protocol for Receiving Bug Reports

### Required Information:
1. **Program filepath** - exact path to failing .glp file
2. **Error message** - complete type checker output
3. **Expected behavior** - what should happen?
4. **Test category** - positive or negative test?
5. **Source** - Session 2 or Session 3?

### Response Template:
```
Bug Report Received: [program name]
Status: Under investigation

1. Reading program source...
2. Reading error output...
3. Consulting specification...
4. Analyzing implementation...
5. Root cause: [type checker bug | program bug | test error]
6. Action: [fix needed | no fix needed | return to originating session]
```

---

## Tools and Commands

### Check Type of a Single File:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart run bin/check_types.dart <filepath.glp>
```

### Run Unit Tests:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/analysis/type_checker/
```

### View Spec:
```bash
cat /Users/udi/Grassroots/GLP/docs/type\ system/<module>.md
```

---

## Known Non-Bugs

These are **NOT** type checker bugs:

1. **SRSW Violations in Original Programs:**
   - Programs using `_` in body
   - Multiple writer occurrences
   - Reader in output position (unless arithmetic guard)

2. **Pre-existing Program Errors:**
   - Missing clause coverage
   - Undefined procedures
   - Type mismatches in program design

3. **Parser Bugs:**
   - `--` operator not parsing correctly
   - These are runtime issues, not type checker issues

---

## Development Principles (Critical)

1. **RTFM:** Always read primary sources (paper, spec, implementation)
2. **Never assume:** Read actual file contents, don't rely on memory
3. **Paper → Spec → Implementation:** Changes flow strictly in this order
4. **No workarounds:** Fix root causes, not symptoms
5. **Positive + Negative controls:** Every fix needs both test types
6. **Test before merge:** No changes to main without verification

---

## Status

🟢 **Ready and standing by**
**Last Updated:** 2026-01-16 (Baseline established)

Full baseline summary: `/Users/udi/Grassroots/GLP/test_output/test_baseline_summary.txt`
