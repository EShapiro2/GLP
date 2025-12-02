# Files to Upload to Claude Web
**Date:** 2025-11-17
**Purpose:** Fix Bug 2 - HeadStructure emission for nested structure arguments

---

## Required Files (Upload All)

### 1. Main Handover Document
- **`/Users/udi/GLP/udi/HANDOVER_TO_CLAUDE_WEB_20251117.md`**
  - Complete problem description and context
  - **READ THIS FIRST**

### 2. Specifications (Normative)
- **`/Users/udi/GLP/docs/glp-bytecode-v216-complete.md`**
  - Complete bytecode instruction set specification
  - Focus on Sections 6, 8.6, 8.7, 12

- **`/Users/udi/GLP/docs/glp-runtime-spec.txt`**
  - Runtime architecture and execution semantics
  - Focus on HEAD phase execution

### 3. Source Code (Current State)
- **`/Users/udi/GLP/glp_runtime/lib/compiler/codegen.dart`**
  - The file with the bug (and my attempted fix)
  - Contains `_generateHead()`, `_generateHeadArgument()`, `_generateStructureElement()`

- **`/Users/udi/GLP/glp_runtime/lib/bytecode/opcodes.dart`**
  - Opcode definitions (HeadStructure, UnifyVariable, etc.)
  - For understanding instruction parameters

### 4. Test Case
- **`/Users/udi/GLP/udi/glp/qsort.glp`**
  - The failing test case (line 32)
  - Context for the metainterpreter pattern

### 5. Analysis Documents
- **`/Users/udi/GLP/udi/COMPILER_BUGS_STATUS_20251117.md`**
  - Bug 1 and Bug 2 analysis
  - Expected vs actual bytecode

- **`/Users/udi/GLP/udi/SPEC_UPDATES_FOR_CLAUDE_WEB_20251117.md`**
  - Previous spec clarifications

---

## Optional (For Deep Debugging)

### Runtime Code (If Needed)
- **`/Users/udi/GLP/glp_runtime/lib/bytecode/runner.dart`**
  - HeadStructure execution handler
  - Only needed if runtime behavior is unclear

### Reference Papers (If Needed)
- **`/Users/udi/GLP/docs/wam.pdf`**
  - Warren's Abstract Machine paper
  - For understanding HEAD matching patterns

---

## File Reading Order

1. **`HANDOVER_TO_CLAUDE_WEB_20251117.md`** - Start here
2. **`glp-bytecode-v216-complete.md`** - Section 6.1, 8.6, 8.7, 12
3. **`codegen.dart`** - Lines 139-368 (clause/head/argument generation)
4. **`qsort.glp`** - Line 32 (test case)
5. **`COMPILER_BUGS_STATUS_20251117.md`** - Bug analysis
6. **`opcodes.dart`** - HeadStructure definition (if needed)

---

## Quick Summary for User

**To upload to Claude Web:**

```
Core files (6):
1. udi/HANDOVER_TO_CLAUDE_WEB_20251117.md
2. docs/glp-bytecode-v216-complete.md
3. docs/glp-runtime-spec.txt
4. glp_runtime/lib/compiler/codegen.dart
5. udi/glp/qsort.glp
6. udi/COMPILER_BUGS_STATUS_20251117.md

Optional (3):
7. glp_runtime/lib/bytecode/opcodes.dart
8. udi/SPEC_UPDATES_FOR_CLAUDE_WEB_20251117.md
9. glp_runtime/lib/bytecode/runner.dart
```

All paths relative to `/Users/udi/GLP/`
