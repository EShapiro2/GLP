# GLP Development Discipline

**Version**: 2.0  
**Date**: 2026-01-18  
**Status**: DRAFT (for review)

This document consolidates all development standards, testing protocols, and handover requirements for GLP projects. It supersedes `DEVELOPMENT_DISCIPLINE_v1.1.md` by incorporating practical operational guidance.

---

## Part I: Foundational Principles

### 1.1 Specification-First Development

The GLP project follows a strict specification hierarchy:

```
Paper (mathematical definitions)
    ↓
Specification (docs/)
    ↓
Tests
    ↓
Implementation
```

**Rules:**
1. The paper is the source of truth for semantics
2. Specs must faithfully translate paper definitions to algorithmic form
3. Tests are derived from specs before implementation
4. Implementation must satisfy tests derived from specs

**Corollary:** No implementation proceeds without a corresponding specification. If the spec is missing or unclear, fix the spec first.

### 1.2 No Workarounds

When a bug or issue is discovered:

1. **STOP** current work immediately
2. **Report** the issue precisely (expected vs actual behavior)
3. **Check the spec** — is the behavior consistent with the spec?
4. **Decide**: Fix the code (if violates spec) OR fix the spec (if spec is wrong/unclear)
5. **Never** bypass, work around, or defer bugs

### 1.3 Traceability

Every artifact must be traceable:

| Artifact | Must Reference |
|----------|----------------|
| Spec section | Paper definition (if applicable) |
| Test file | Spec section being tested |
| Implementation | Spec section and test file |

### 1.4 Verify Before Acting on Reported Information

When receiving information about the codebase state (via pasted transcripts, reports, or descriptions):

1. **Always read the actual files** before acting on reported issues
2. **Transcripts may be outdated** — the situation may have been fixed between when the transcript was created and when you receive it
3. **Trust filesystem reads over pasted content** when they conflict

### 1.5 Command Output Conventions

When providing shell commands for the user to execute:

1. **Always provide complete one-liner scripts** that can be copy-pasted directly
2. **If user explicitly requests terminal output** (e.g., merge, push commands): output goes to terminal
3. **Otherwise**: redirect ALL output to a file in `/private/tmp` that Claude can read later

**Standard Testing One-Liner (use this exact format):**

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test > /private/tmp/glp-tests.txt 2>&1 && cd .. && bash test/full_run_repl_tests.sh >> /private/tmp/glp-tests.txt 2>&1
```

This runs both Dart tests and REPL tests, appending all output to `/private/tmp/glp-tests.txt`.

**Other Examples:**

```bash
# User asks for merge commands → output to terminal (user explicitly wants to see it)
cd /Users/udi/Grassroots/GLP && git checkout main && git pull origin main && git merge origin/claude/branch-name

# Claude needs to see any output → redirect to /private/tmp file
cd /Users/udi/Grassroots/GLP && bash test/run_book_tests.sh > /private/tmp/book-tests.txt 2>&1
```

**Rules:**
- Use `> /private/tmp/filename.txt 2>&1` for first command, `>> /private/tmp/filename.txt 2>&1` to append
- Never use `| cat` or `| tee` 
- Never ask user to paste output — read from /private/tmp file instead

### 1.6 Terminology: Errors, Not "Limitations" or "Issues"

When code does not conform to the spec, use precise language:

- **Correct**: "The code is incorrect", "This is an error", "The implementation does not match the spec"
- **Avoid**: "limitation", "issue", "known issue", "edge case", "quirk"

If the spec is unclear, say "the spec needs clarification". If the code violates the spec, say "the code is wrong" or "this is an error". Do not soften errors with euphemisms.

### 1.7 Bug Handling: Never Bypass, Always Report

When encountering a bug in the codebase — whether in your own area of responsibility or in another module:

1. **STOP immediately** — do not attempt to work around, bypass, or compensate for the bug
2. **Report the bug precisely** — describe the expected behavior, actual behavior, and steps to reproduce
3. **Wait for confirmation** — do not proceed with any related work until you receive explicit acknowledgment that the bug report was received
4. **Never proceed without confirmation** — if no response is received, escalate or ask again

**Rationale:** Bypassing bugs creates hidden dependencies, obscures the true state of the system, and makes future debugging exponentially harder. A workaround that "works" today becomes technical debt tomorrow.

**What counts as bypassing:**
- Adding special-case code to avoid triggering the bug
- Restructuring your code to "route around" the broken functionality  
- Using a different approach specifically because the correct approach hits a bug
- Commenting out tests that fail due to the bug
- Marking tests as "expected to fail" without fixing the underlying issue

**Correct behavior:**
- Report the bug with full details
- Wait for acknowledgment ("I see it", "logged", "will fix", etc.)
- Either fix the bug (if in scope) or wait for fix before proceeding
- If the bug blocks your work entirely, say so explicitly

---

## Part II: Test Baseline Protocol

### 2.1 Available Test Suites

| Suite | Tests | Command | Location |
|-------|-------|---------|----------|
| **Dart Unit Tests** | ~236 | `cd glp_runtime && dart test` | `glp_runtime/test/` |
| **REPL Tests** | 222 | `bash test/full_run_repl_tests.sh` | `test/` |
| **Typed REPL Tests** | varies | `bash test/run_typechecker_repl_tests.sh` | `test/` |
| **Book Compilation** | 141 files | `bash test/run_book_tests.sh` | `test/` |
| **Multiagent Tests** | 139 | `cd glp_runtime && dart test test/multiagent/` | `glp_runtime/test/multiagent/` |
| **Flutter Build** | — | `cd glp_multiagent && flutter build macos` | `glp_multiagent/` |

### 2.2 Mandatory: Before and After Every Change

**BEFORE making any code change:**

```bash
# 1. Record baseline - run from /Users/udi/Grassroots/GLP
cd /Users/udi/Grassroots/GLP

# 2. Dart unit tests
cd glp_runtime && dart test && cd ..

# 3. REPL tests
bash test/full_run_repl_tests.sh

# 4. Note the results (e.g., "236 unit tests passing, 222 REPL tests passing")
```

**AFTER making any code change:**

```bash
# 1. Run the same tests
cd glp_runtime && dart test && cd ..
bash test/full_run_repl_tests.sh

# 2. Compare results to baseline
# - Same count = good
# - More passing = good (if intentional)
# - Any new failures = STOP and investigate
```

### 2.3 Test Failure Protocol

If tests fail AFTER a change but passed BEFORE:

1. **Do not proceed** with additional changes
2. **Identify** which tests failed and why
3. **Either**: Fix the code to pass the tests, OR
4. **Discuss**: If the test is wrong, discuss updating the test (but this is rare)

If tests fail BEFORE a change (baseline failure):

1. **Report** the failure before starting work
2. **Do not make changes** that could obscure the existing failure
3. **Optionally**: Fix the baseline failure first (if within scope)

### 2.4 Adding New Tests

When adding a feature or fixing a bug:

1. **Write the test first** (TDD: red → green → refactor)
2. **Include both positive and negative controls**:
   - Positive: Input that should succeed
   - Negative: Similar input that should fail
3. **Add to the permanent test suite** — tests are never removed

---

## Part III: Handover Protocol

### 3.1 When to Write a Handover

Write a handover document when:
- Ending a work session with incomplete work
- Completing a significant phase of work
- Transferring work to another project or conversation

### 3.2 Handover Document Location

| Project | Location |
|---------|----------|
| Multiagent GLP | `docs/ma/` |
| Typed GLP | `docs/type system/` |
| General/Other | `docs/handover/` |

### 3.3 Handover Document Format

```markdown
# [Feature/Phase] Handover Report

**Date:** YYYY-MM-DD
**Author:** [Claude session identifier or human name]
**Status:** [In Progress | Completed | Blocked]

---

## Summary

One paragraph describing what was accomplished and what remains.

---

## Completed Work

### [Subsection for each completed item]

- Files created/modified (with paths)
- Key changes made
- Test results

---

## Current State

### Test Status

| Suite | Result |
|-------|--------|
| Dart Unit Tests | X/Y passing |
| REPL Tests | X/Y passing |
| [Other relevant] | X/Y passing |

### Known Issues

List any known issues, failures, or edge cases discovered.

---

## Next Steps

Numbered list of what needs to be done next, in order of priority.

---

## Files Changed

List of all files created, modified, or deleted during this work.

---

## Notes for Next Session

Any context, gotchas, or important information for whoever continues this work.
```

### 3.4 Handover Naming Convention

Use this format: `[topic]-handover-YYYY-MM-DD.md`

Examples:
- `phase6-handover-2026-01-17.md`
- `union-alias-v0.8-handover.md`
- `clause-validation-handover-2026-01-18.md`

---

## Part IV: Project Coordination

### 4.1 Project Responsibilities

| Project | Primary Focus | Can Modify |
|---------|---------------|------------|
| Overview GLP | Consistency, testing, structure | All files (coordination) |
| Multiagent GLP | Multiagent implementation | `lib/multiagent/`, `glp_multiagent/`, `programs/multiagent/` |
| Typed GLP | Type system | `lib/analysis/`, `docs/type system/` |
| Book GLP | Book content | `programs/book/`, `programs/typed_book/` |
| ICLP GLP | Paper | `GLP-ICLP-2026/` |

### 4.2 Cross-Project Changes

When a project needs to modify files outside its primary focus:

1. **Check for conflicts**: Ensure the change doesn't contradict another project's work
2. **Update relevant specs**: If changing behavior, update the spec
3. **Run all tests**: Not just tests for your project
4. **Note in handover**: Document what was changed and why

### 4.3 Spec Ownership

Each project updates their own specs. Overview GLP ensures:
- No contradictions between specs
- No overlapping definitions
- Consistent terminology across all docs

---

## Part V: Code Standards

### 5.1 Types First

For any new GLP predicate:
1. Type declaration first
2. Procedure declaration (with modes)
3. Implementation

```glp
% 1. Type declaration
MyList ::= [] ; [_ | MyList].

% 2. Procedure declaration
procedure append(MyList?, MyList?, MyList).

% 3. Implementation
append([], Ys, Ys?) :- true | true.
append([X|Xs], Ys, [X?|Zs]) :- true | append(Xs?, Ys?, Zs).
```

### 5.2 Error Handling

- No silent failures
- All errors include context for diagnosis
- Use typed exceptions in Dart, not generic strings

### 5.3 Documentation

Public APIs require doc comments:

```dart
/// Checks if the clause is well-typed according to the procedure declaration.
///
/// Implements: docs/type system/well-typed-clause.md
/// 
/// Returns [WellTypedResult] with success/failure and any errors.
WellTypedResult checkClause(Clause clause, ProcDecl decl) { ... }
```

---

## Part VI: Git Standards

### 6.1 Branch Strategy

```
main                    # Stable, all tests pass
  └── claude/<name>     # Claude session branches
  └── feature/<name>    # Feature development
  └── fix/<name>        # Bug fixes
```

### 6.2 Commit Messages

```
<type>(<scope>): <description>

[optional body]
```

Types: `feat`, `fix`, `test`, `spec`, `refactor`, `docs`

### 6.3 Commit After Every Revision

**Rule:** Commit after every code revision, even minor ones.

**Rationale:** Frequent commits create restore points. If a change introduces a subtle bug or breaks something unexpectedly, you can revert to any prior state. Without commits, all uncommitted work is lost if you need to roll back.

**Practice:**
1. Make a change (even a single line)
2. Test that it works (or at least compiles)
3. Commit immediately with a descriptive message
4. Repeat

**Commit messages for minor changes:**
```
fix(parser): correct off-by-one in line count
refactor(types): rename variable for clarity
test(repl): add edge case for empty list
```

Do not batch multiple unrelated changes into a single commit. Each commit should be atomic and revertible.

### 6.4 Before Merging to Main

- [ ] All tests pass (baseline check)
- [ ] Handover document written (if work is significant)
- [ ] No uncommitted changes
- [ ] Spec updated (if behavior changed)

---

## Part VII: Quick Reference

### Test Commands (Copy-Paste Ready)

```bash
# Full baseline check (run from /Users/udi/Grassroots/GLP)
cd /Users/udi/Grassroots/GLP
cd glp_runtime && dart test && cd ..
bash test/full_run_repl_tests.sh

# Individual test suites
cd glp_runtime && dart test                           # Unit tests
cd glp_runtime && dart test test/multiagent/          # Multiagent only
bash test/full_run_repl_tests.sh                      # REPL tests
bash test/run_typechecker_repl_tests.sh               # Type checker REPL
bash test/run_book_tests.sh                           # Book compilation

# Flutter build
cd glp_multiagent && flutter build macos
```

### File Locations

| What | Where |
|------|-------|
| Discipline document | `docs/DISCIPLINE.md` (this file) |
| Project descriptions | `docs/projects/` |
| Test scripts | `test/` |
| Multiagent specs | `docs/ma/` |
| Type system specs | `docs/type system/` |
| Programs | `programs/` |

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-01-07 | Original discipline document |
| 1.1 | 2025-01-12 | Added filesystem access section |
| 2.0 | 2026-01-18 | Consolidated with test protocol and handover standards |
| 2.1 | 2026-01-18 | Added section 1.7: Bug Handling - Never Bypass, Always Report |
| 2.2 | 2026-01-18 | Added section 6.3: Commit After Every Revision |

