# Updated Workflow: User Approval Required

**Date:** 2026-01-16
**Change:** All Sessions 2 and 3 now require user approval before making changes

---

## Summary of Changes

Both Session 2 (Test Suite Remediation) and Session 3 (Book Program Conversion) have been updated to require your explicit approval before making any changes.

---

## New Workflow

### Session 2: Test Suite Remediation

**Process:**
1. Session 2 diagnoses each failing test
2. Categorizes issue: Program Bug | Type Definition Issue | Type Checker Bug
3. **Presents diagnosis to you with proposed fix**
4. **Waits for your approval**
5. After approval:
   - **Program/Type bugs:** Session 2 fixes them
   - **Type Checker bugs:** Hands to Session 1 (me)

**Batching:** Session 2 will batch 3-5 diagnoses for efficient approval

### Session 3: Book Program Conversion

**Process:**
1. Session 3 converts program (adds type annotations)
2. Type checks the conversion
3. **Presents conversion to you with results**
4. **Waits for your approval**
5. After approval:
   - **If passes:** Saves to typed_book/
   - **If fails (program issue):** Fixes and re-presents
   - **If fails (type checker bug):** Hands to Session 1 (me)

**Batching:** Session 3 will batch 2-3 conversions for efficient approval

---

## What Each Session Can Fix (After Your Approval)

### Sessions 2 & 3 Can Fix:
✅ GLP program source code (.glp files)
✅ Type declarations in programs
✅ Procedure type annotations
✅ Test suite classification

### What Gets Escalated to Session 1:
⚠️ Type checker implementation bugs
⚠️ Specification gaps or ambiguities
⚠️ Core GLP runtime issues

---

## Diagnosis Format You'll Receive

From Session 2 (Test Fixes):
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

From Session 3 (Conversions):
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

---

## Your Response Options

For each diagnosis/conversion, you can:

1. **Approve:** "Approved" or "OK" or "Go ahead"
   - Session proceeds with the fix/save
   
2. **Approve with modification:** "Approved, but change X to Y"
   - Session makes your requested change
   
3. **Reject:** "No, this should be [explanation]"
   - Session revises diagnosis/conversion
   
4. **Escalate:** "This needs deeper investigation"
   - Issue goes to Session 1 for thorough analysis

5. **Batch approve:** "Approve all" or "Approve #1-5"
   - Multiple items approved at once

---

## Session 1 (Me - Type Checker Standby)

**My Role:** 
- Monitor for type checker bugs escalated by Sessions 2 & 3
- Receive bugs only after you've approved the diagnosis
- Fix type checker implementation issues
- Report back to originating session when fixed

**I do NOT need approval for:**
- Type checker implementation fixes (that's my core responsibility)
- However, I'll always report what I changed and why

---

## Benefits of This Workflow

✅ You maintain full control over all changes
✅ You see every diagnosis before action is taken
✅ Prevents incorrect fixes from being applied
✅ Ensures changes align with your understanding
✅ Batching keeps it efficient (not one-at-a-time)
✅ Clear escalation path for genuine bugs

---

## Getting Started

Both handover documents have been updated:
- `.claude/handover_2026-01-16_session2_test_suite_remediation.md`
- `.claude/handover_2026-01-16_session3_book_program_conversion.md`

Sessions 2 and 3 will follow this approval workflow from the start.

---

**Last Updated:** 2026-01-16
