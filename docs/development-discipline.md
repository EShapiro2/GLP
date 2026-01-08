# GLP Software Development Discipline

**Version**: 1.0
**Date**: 2025-01-07
**Status**: APPROVED

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

**Example:**
```dart
/// Implements: docs/modules/type-checker.md Section 3.2
/// Tests: test/type_checker_test.dart
/// Paper: Definition 4.5 (Consistent Paths)
bool areConsistent(TermPath t, TypePath p) { ... }
```

---

## 3. Specification Standards

### 3.1 Specification Document Structure

Each module has a spec document in `/docs/modules/` containing:

```markdown
# Module: <Name>

**Version**: X.Y
**Date**: YYYY-MM-DD
**Status**: DRAFT | APPROVED
**Paper References**: Definition X.Y, ... (if applicable)

## Purpose

One paragraph: what this module does.

## Dependencies

Modules this module requires (imports).

## Public Interface

### Types

Description of each public type.

### Functions

For each public function:
- Signature
- Preconditions (what must be true before calling)
- Postconditions (what is true after calling)
- Errors (what exceptions can be thrown)

## Algorithms

Step-by-step procedures for non-trivial operations.

## Examples

Input/output examples for key operations.

## Error Conditions

Table of error conditions and corresponding exceptions.
```

### 3.2 Specification Versioning

Specs use semantic versioning: `MAJOR.MINOR`

- **MAJOR**: Breaking changes to interfaces or semantics
- **MINOR**: Additions, clarifications, bug fixes

### 3.3 Change Protocols

**When the paper changes:**

1. Identify affected definitions/theorems by number
2. Identify all spec sections that reference those definitions
3. Update affected specs (new version numbers)
4. Update or add tests for changed behavior
5. Update implementation to pass tests

**When the spec changes (without paper change):**

1. Update spec document with new version number
2. Mark changed sections clearly
3. Update or add tests for changed behavior
4. Only then update implementation

The full cascade is always: Paper → Spec → Tests → Implementation. No step is skipped.

---

## 4. Testing Standards

### 4.1 Test Organization

```
test/
  <module_name>_test.dart       # Unit tests for each module
  integration/
    <feature>_test.dart         # Cross-module integration tests
  fixtures/
    programs/                   # GLP program files for testing
    types/                      # Type definition files for testing
```

### 4.2 Test Naming Convention

Tests use descriptive names:

```dart
test('<unit under test> <condition> <expected result>', () {
  // ...
});
```

**Examples:**
```dart
test('areConsistent returns true when term path is prefix ending in reader at consumed position', () { });
test('areConsistent returns false when reader appears at produced position', () { });
test('wellTypedClause rejects clause with non-complementary variable types', () { });
```

### 4.3 Positive and Negative Controls

**Every feature requires both:**

- **Positive controls**: Inputs that should succeed
- **Negative controls**: Inputs that should fail

**Rule:** For each positive test, ask "What similar input should fail?" and write that test.

```dart
group('Moded Head Construction', () {
  // Positive control
  test('constructs valid moded head for merge clause', () {
    // ...
  });

  // Negative control
  test('rejects head with variable mode inconsistent with type', () {
    expect(() => constructModedHead(head, type), throwsTypeError);
  });
});
```

### 4.4 Golden Tests

Programs from the paper serve as golden tests. Golden tests must never be modified to make them pass — if a golden test fails, the implementation is wrong.

```dart
@Tags(['golden'])
test('merge program is well-typed with correct declaration', () {
  final program = loadProgram('test/fixtures/programs/merge.glp');
  final result = typeCheck(program);
  expect(result.isWellTyped, isTrue);
});
```

### 4.5 Bottom-Up Testing

Modules are tested with real implementations in dependency order:

1. Test lowest-level modules first (those with no project dependencies)
2. Once a module's tests pass, modules depending on it can be tested
3. Tests use real dependencies, not test doubles (mocks/fakes)

**Clarification:** "No test doubles" means we don't mock dependencies. We still craft invalid inputs directly (malformed strings, incorrect type definitions) to test error handling.

---

## 5. Modularity

### 5.1 Module Definition

A **module** is a unit of code with:
- A **public interface**: Types, functions, and classes exposed to other modules
- A **private implementation**: Internal code not visible outside the module
- **Explicit dependencies**: Other modules this module requires
- **A specification**: A document in `/docs/modules/` describing the interface

### 5.2 Module Granularity

Module boundaries are determined by **specification complexity**:

- If a spec document becomes difficult to understand as a unit, split the module
- If a spec document is trivially simple, consider merging with a related module

**Guideline:** A module spec should fit in roughly 2-5 pages.

### 5.3 Module Structure in Dart

```
lib/
  src/
    <module_name>/
      <module_name>.dart        # Public interface (barrel file)
      src/                      # Private implementation
        <impl>.dart

docs/
  modules/
    <module_name>.md            # Module specification

test/
  <module_name>_test.dart       # Module tests
```

**The barrel file** exports only the public interface:

```dart
// lib/src/nfa_compiler/nfa_compiler.dart

/// NFA Compiler Module
/// Specification: docs/modules/nfa-compiler.md
library nfa_compiler;

export 'src/compiler.dart' show NfaCompiler, CompilationResult;
export 'src/errors.dart' show CompilationError;
// Internal helpers are NOT exported
```

### 5.4 Dependency Rules

Modules form a **directed acyclic graph** (DAG):

1. **No circular dependencies**: If A depends on B, B cannot depend on A
2. **Import interfaces only**: Import the barrel file, not internal files
3. **Minimal dependencies**: Import only what is needed

```dart
// CORRECT: Import module interface
import 'package:glp/src/nfa_compiler/nfa_compiler.dart';

// WRONG: Import internal implementation
import 'package:glp/src/nfa_compiler/src/internal.dart';
```

### 5.5 Module Development Order

Develop and test in dependency order:

1. Identify the dependency DAG
2. Implement and test leaf modules first (no dependencies)
3. Work upward: implement a module only when all its dependencies are tested

### 5.6 Interface Stability

Once a module interface is in use:

1. **Adding** new functions/types is safe
2. **Changing** existing signatures requires updating all callers
3. **Removing** functions/types requires updating all callers

**Protocol for interface changes:**
1. Update the module spec first
2. Identify all dependent modules
3. Update dependent module specs if affected
4. Update implementation and tests together

### 5.7 Module Cohesion

Each module has a **single responsibility** expressible in one phrase.

**Warning signs of poor cohesion:**
- Module does "X and Y" (conjunction suggests two responsibilities)
- Unrelated changes touch the same module
- Module is hard to name concisely

When cohesion is poor, split the module and update specs accordingly.

---

## 6. Implementation Standards

### 6.1 Single Responsibility

Each function/class does one thing:
- If a function does two things, split it
- If a class has multiple reasons to change, split it

### 6.2 Explicit Error Handling

- No silent failures
- All error conditions documented in spec
- Errors include enough context to diagnose
- Use typed exceptions, not strings

```dart
class InconsistentPathError extends TypeError {
  final TermPath termPath;
  final TypePath typePath;
  final String reason;

  InconsistentPathError(this.termPath, this.typePath, this.reason);

  @override
  String toString() => 'Inconsistent paths: $reason\n'
      '  Term path: $termPath\n'
      '  Type path: $typePath';
}
```

### 6.3 No Workarounds

- If a test fails, fix the implementation or fix the spec
- Never comment out tests
- Never add special cases to "make it work"
- If a bug is found, stop and fix immediately — no bug tracking

---

## 7. Bug Handling

**There is no bug tracking.** When a bug is discovered:

1. **Stop** current work
2. **Write a test** that exposes the bug (the test must fail)
3. **Fix** the bug
4. **Verify** the test passes
5. **Add the test to the compliance suite** (permanent regression test)
6. **Resume** previous work

Every bug produces at least one new test. These tests accumulate in the compliance suite, ensuring bugs never regress.

Bugs are never deferred, tracked, or worked around.

---

## 8. Release Process

### 8.1 Compliance Suite

The **compliance suite** is the complete set of tests that must pass for release:

- Module unit tests
- Integration tests
- Golden tests (from paper examples)
- Regression tests (from fixed bugs)

The compliance suite grows monotonically: tests are added but never removed.

### 8.2 Release Criteria

**When all compliance suite tests pass, the code is releasable.**

There is no separate release branch or release procedure. The `main` branch always contains code that passes the full compliance suite.

---

## 9. Version Control Standards

### 9.1 Branch Strategy

```
main              # Stable, all tests pass
  └── feature/<name>   # Feature development
  └── fix/<name>       # Bug fixes
```

### 9.2 Commit Standards

Commits are atomic and well-described:

```
<type>(<scope>): <description>

[optional body]

[optional footer with references]
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `test`: Adding/updating tests
- `spec`: Specification changes
- `refactor`: Code change that neither fixes nor adds
- `docs`: Documentation only

### 9.3 Merge Requirements

Code merges to `main` only when:
1. All tests pass
2. Code review completed (see Section 10)

---

## 10. Code Review

Code is reviewed before merging to `main`.

**Review process:**
1. Author (Claude Code or Claude Web) produces code
2. Reviewer (the other Claude instance) reviews the code
3. Reviewer verifies:
   - Code traces to spec
   - Tests exist and pass
   - Implementation matches spec
   - No workarounds or hacks
4. Reviewer approves or requests changes
5. After approval, code is merged

**Review checklist:**
- [ ] Spec reference is present and correct
- [ ] Tests cover positive and negative cases
- [ ] All tests pass
- [ ] Code follows spec algorithm
- [ ] Error handling is explicit
- [ ] No commented-out code
- [ ] No TODOs without resolution

---

## 11. Documentation Standards

### 11.1 Code Documentation

Public APIs must have doc comments:

```dart
/// Checks if a term path and type path are consistent.
///
/// Implements Definition 4.5 (Consistent Paths) from the paper.
/// See docs/modules/type-checker.md Section 3.2 for algorithm.
///
/// Returns `true` if paths are consistent, `false` otherwise.
///
/// Throws [ArgumentError] if either path is empty.
bool areConsistent(TermPath termPath, TypePath typePath) { ... }
```

### 11.2 Inline Comments

Use inline comments to explain **why**, not **what**:

```dart
// BAD: explains what (obvious from code)
counter++;  // Increment counter

// GOOD: explains why (not obvious from code)
// Reader variables flip to writers in moded head (Definition 4.7)
final flippedVar = variable.paired;
```

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

**Bad instruction:**
> "Fix the path consistency checking"

**Good instruction:**
> "In `lib/src/type_checker/src/consistent_paths.dart`, modify the `areConsistent` function to handle case 2(a) from spec Section 3.2: when term path is a prefix ending in a reader and the corresponding type position has mode consume (↓), return true."

### 12.3 AI Code Review

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

## Appendix A: Checklist Before Merge

- [ ] Spec exists and is up-to-date
- [ ] Spec references paper definitions
- [ ] Tests exist for all spec behaviors
- [ ] Positive controls present
- [ ] Negative controls present
- [ ] All tests pass
- [ ] Code references spec section
- [ ] Doc comments on public API
- [ ] No TODOs without resolution
- [ ] No commented-out code
- [ ] Review completed and approved

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1-draft | 2025-01-07 | Initial draft |
| 0.2-draft | 2025-01-07 | Added modularity, bug handling, release process, code review |
| 0.3-draft | 2025-01-07 | Clarified compliance suite: bug tests become permanent regression tests |
| 0.4-draft | 2025-01-07 | Added paper revision cascade; clarified test doubles vs invalid inputs |
| 1.0 | 2025-01-07 | APPROVED |
