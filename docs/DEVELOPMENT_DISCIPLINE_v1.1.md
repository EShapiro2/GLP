# GLP Software Development Discipline

**Version**: 1.1  
**Date**: 2025-01-12  
**Status**: APPROVED

---

## Changes from v1.0

- Added Section 12.3: Filesystem Access and Direct Implementation

---

## 1. Purpose and Scope

This document defines the software development discipline for all Dart programming in the GLP project. It is based on established methodologies:

- **Test-Driven Development (TDD)** — Beck, 2002
- **Behavior-Driven Development (BDD)** — North, 2006

All contributors and tools (including AI assistants) must follow this discipline.

---

## 2. Foundational Principles

### 2.1 Specification-First Development

The GLP project has a formal specification hierarchy:

```
Paper (mathematical definitions)
    ↓
Specification (/docs/modules/)
    ↓
Tests
    ↓
Implementation
```

**Rules:**
1. The paper is the source of truth for semantics
2. The specification must faithfully translate paper definitions to algorithmic form
3. Tests must be derived from specifications before implementation
4. Implementation must satisfy tests derived from specification

**Corollary:** No implementation work proceeds without a corresponding specification. If the spec is missing or unclear, fix the spec first.

### 2.2 Test-Driven Development

All code is developed using the TDD cycle:

```
RED → GREEN → REFACTOR
```

1. **RED**: Write a failing test that defines expected behavior
2. **GREEN**: Write the minimum code to make the test pass
3. **REFACTOR**: Improve the code while keeping tests green

**Rules:**
- No production code without a failing test first
- Tests are written before implementation, not after
- Each test tests one thing

### 2.3 Traceability

Every artifact must be traceable to its source:

| Artifact | Must Reference |
|----------|----------------|
| Spec section | Paper definition number |
| Test file | Spec section |
| Implementation | Spec section and test file |

---

## 3-11. [Unchanged from v1.0]

See DEVELOPMENT_DISCIPLINE_v1.0.md for Sections 3-11.

---

## 12. AI Assistant Guidelines

### 12.1 Information Flow

- AI assistants work from **specs only**, not from the paper
- If spec is incomplete, update spec before instructing AI
- AI cannot see the paper; everything needed must be in spec

### 12.2 Instruction Format

Instructions to AI must be:
- **Specific**: Exact file paths, exact code changes
- **Complete**: All necessary context included
- **Verifiable**: Clear success criteria

### 12.3 Filesystem Access and Direct Implementation

When Claude Web has filesystem access (via MCP tools or similar):

1. **Claude Web implements directly**: Do not delegate to Claude Code. Do not provide "instructions for Claude Code". Make the edits yourself using file editing tools.
2. **Read full source files**: Never work from snippets or partial information. Always read the complete file before making changes.
3. **Test verification**: After making changes, provide the user with exact commands to run tests. The user runs tests and saves output to a file that Claude Web can read.
4. **Iterate until passing**: Read test results, diagnose failures, make fixes, repeat until tests pass.

**CRITICAL**: If you have filesystem access, you ARE the implementer. Never say "Instructions for Claude Code" or similar—just make the changes directly.

**Workflow:**
```
1. Claude Web reads source files
2. Claude Web makes edits directly (using edit_file, str_replace, etc.)
3. User runs: <test command> 2>&1 | tee <results_file>
4. Claude Web reads results file
5. If failures: Claude Web fixes and returns to step 3
6. If passing: Done
```

### 12.4 AI Code Review

Claude Web and Claude Code review each other's work:
- Claude Code produces implementation → Claude Web reviews
- Claude Web produces spec/design → Claude Code may review for implementability

All AI-generated code requires review and approval before merge.

---

## 13. Adoption

### 13.1 New Code

All new code follows this discipline immediately.

### 13.2 Existing Code

Existing code is brought into compliance when modified. No separate refactoring efforts unless specifically planned.

### 13.3 Exceptions

Exceptions to this discipline require explicit acknowledgment and a plan for eventual compliance.

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-01-07 | APPROVED |
| 1.1 | 2025-01-12 | Added Section 12.3: Filesystem access and direct implementation |
