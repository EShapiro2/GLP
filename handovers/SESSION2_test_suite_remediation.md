# Session 2: Typed REPL Test Suite Remediation
**Date:** 2026-01-16
**Goal:** Get typed REPL test suite from 126/222 to 222/222 passing

---

## Mission

Fix all failing tests by:
1. Diagnosing each failure
2. **Presenting diagnosis to USER for approval**
3. After approval: fixing programs OR escalating type checker bugs to Session 1

## Current Baseline

- Total: 222 tests
- Passing: 126 (57%)
- Failing: 96 (43%)

## Workflow

1. Run test suite
2. Pick a failing test
3. Read the actual program file
4. Diagnose: Program bug OR Type checker bug
5. **PRESENT TO USER - WAIT FOR APPROVAL**
6. After approval: Fix program OR hand to Session 1

## CRITICAL: Present diagnosis in this format

```
DIAGNOSIS #[N]: [program name]

File: [filepath]
Category: [Program Bug | Type Checker Bug]
Error: [exact error]
Diagnosis: [what's wrong]
Proposed Fix: [what to change]

Awaiting approval.
```

**Batch 3-5 diagnoses before presenting**

Full details: see complete handover in `.claude/` directory
