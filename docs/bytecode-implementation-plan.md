# GLP Binary Bytecode Implementation Plan

**Status**: Planned
**Prerequisite**: docs/glp-bytecode-binary-spec.md (FCP bytecode spec)

## Current State

- Instructions are Dart objects (`Label`, `ClauseTry`, `HeadVariable`, etc.)
- Runner dispatches via `is` type checks (`if (op is Label)`, `if (op is Spawn)`, etc.)
- Module system working with ServiceRegistry
- All tests passing (64 dart, 74 REPL)

## Target State

- Instructions are 16-bit numeric opcodes (FCP format)
- Runner dispatches on numeric opcode values
- Binary bytecode format for modules (.glpc files)
- Heap words use FCP tagged format (32-bit)

## Implementation Phases

### Phase 1: Define Opcode Constants

**Goal**: Add FCP constants without changing runtime behavior.

**Files to create**:
- `lib/bytecode/fcp_opcodes.dart` - opcode constants from FCP opcodes.h
- `lib/bytecode/fcp_tags.dart` - tag/word format from FCP fcp.h

**Contents of fcp_opcodes.dart**:
```dart
/// FCP Opcode Constants
/// From: https://github.com/EShapiro2/FCP/blob/main/Savannah/Logix/EMULATOR/opcodes.h

class FcpOp {
  // Dereference (0x001-0x00f)
  static const int deref_2 = 0x001;
  static const int deref_3 = 0x002;
  // ... etc

  // Load (0x010-0x020)
  static const int load_we_var = 0x010;
  static const int load_ro_of_reg = 0x012;
  // ... etc

  // Execution Control (0x11e-0x13e)
  static const int enqueue = 0x11e;
  static const int iterate = 0x120;
  static const int execute = 0x121;
  static const int halt = 0x124;
  static const int commit1 = 0x125;
  static const int suspend = 0x129;
  // ... etc

  // Ask Kernels (0x140-0x170)
  static const int otherwise = 0x140;
  static const int is_nonvar = 0x141;
  static const int is_we = 0x145;
  static const int is_ro = 0x147;
  static const int grounded = 0x150;
  // ... etc
}
```

**Contents of fcp_tags.dart**:
```dart
/// FCP Word Format
/// From: https://github.com/EShapiro2/FCP/blob/main/Savannah/Logix/EMULATOR/fcp.h

class FcpTag {
  // Flags (2 bits)
  static const int refFlag = 0x0;
  static const int regularFlag = 0x1;
  static const int listFlag = 0x2;

  // Type codes (4 bits)
  static const int wrtCode = 0x0;   // Writer
  static const int roCode = 0x1;    // Reader
  static const int intCode = 0x2;   // Integer
  static const int realCode = 0x3;  // Real
  static const int strCode = 0x4;   // String
  static const int nilCode = 0x5;   // Nil
  static const int tplCode = 0x6;   // Tuple/Structure
  static const int vctrCode = 0x7;  // Vector
  static const int invldCode = 0x8; // Invalid

  static const int tagShift = 6;
  static const int tagMask = 0x3F;
  static const int valueMask = 0xFFFFFF; // 26 bits

  static int tag(int code, int flag) => (code << 2) | flag;
  static int word(int value, int tag) => (value << tagShift) | tag;
  static int getValue(int word) => word >> tagShift;
  static int getTag(int word) => word & tagMask;
  static int getCode(int word) => (word >> 2) & 0xF;
  static int getFlag(int word) => word & 0x3;
}
```

**Risk**: None - pure addition, no existing code changes.

**Validation**: Code compiles, existing tests still pass.

---

### Phase 2: Binary Encoder

**Goal**: Compiler emits `Uint16List` bytecode alongside object list.

**Files to modify**:
- `lib/compiler/compiler.dart` - add binary emission

**Files to create**:
- `lib/bytecode/binary_encoder.dart` - encode objects to binary

**Approach**:
1. After compiling to `List<Op>`, also encode to `Uint16List`
2. Build string table (functor names, string constants)
3. Build label table (procedure name → PC)
4. Emit opcodes with operand encoding

**Binary encoder structure**:
```dart
class BinaryEncoder {
  final StringTable strings = StringTable();
  final List<int> code = [];

  Uint16List encode(List<dynamic> ops) {
    for (final op in ops) {
      _encodeOp(op);
    }
    return Uint16List.fromList(code);
  }

  void _encodeOp(dynamic op) {
    if (op is Label) {
      // Labels are metadata, not emitted as opcodes
      // But record in label table
    } else if (op is ClauseTry) {
      code.add(FcpOp.clause_try); // or appropriate FCP opcode
    } else if (op is Commit) {
      code.add(FcpOp.commit1);
    }
    // ... etc
  }
}
```

**Risk**: Low - parallel output, doesn't affect existing execution.

**Validation**:
- Encode then decode, compare with original
- Print bytecode disassembly, verify matches object list

---

### Phase 3: Binary Runner

**Goal**: New runner that executes from `Uint16List`.

**Files to create**:
- `lib/bytecode/binary_runner.dart` - numeric opcode dispatch

**Approach**:
1. Create `BinaryRunner` alongside existing `BytecodeRunner`
2. Dispatch via switch on opcode value
3. Same execution semantics, different dispatch mechanism

**Structure**:
```dart
class BinaryRunner {
  final Uint16List code;
  final StringTable strings;

  RunResult run(RunnerContext cx) {
    var pc = cx.kappa;
    while (pc < code.length) {
      final opcode = code[pc];
      switch (opcode) {
        case FcpOp.commit1:
          // commit logic
          pc++;
          break;
        case FcpOp.enqueue:
          // spawn logic
          final label = code[pc + 1];
          final arity = code[pc + 2];
          pc += 3;
          break;
        // ... etc
      }
    }
  }
}
```

**Risk**: Medium - new execution path, must match old runner exactly.

**Validation**:
- Run same programs through both runners
- Compare execution traces
- All tests must pass with either runner

---

### Phase 4: Module Binary Format

**Goal**: Serialize/deserialize modules as binary files.

**Files to create**:
- `lib/bytecode/module_writer.dart` - write .glpc files
- `lib/bytecode/module_reader.dart` - read .glpc files

**Module file format** (from spec):
```
Header (fixed size)
String Table (variable)
Label Table (variable)
Export Table (variable)
Code (variable)
```

**Approach**:
1. `ModuleWriter.write(LoadedModule, File)`
2. `ModuleReader.read(File) -> LoadedModule`
3. Update REPL to load .glpc files
4. Update ServiceRegistry to work with binary modules

**Risk**: Medium - file I/O, binary format correctness.

**Validation**:
- Round-trip: write then read, compare with original
- Load .glpc in REPL, execute queries
- All REPL tests pass with binary modules

---

### Phase 5: Cleanup

**Goal**: Remove object-based instruction support.

**Files to modify/delete**:
- `lib/bytecode/opcodes.dart` - delete or deprecate
- `lib/bytecode/opcodes_v2.dart` - delete or deprecate
- `lib/bytecode/runner.dart` - remove object dispatch, keep only binary

**Approach**:
1. Ensure all tests use binary path
2. Remove object-based code paths
3. Simplify runner to single binary dispatch
4. Update compiler to emit only binary

**Risk**: High - removes fallback, must be confident in binary path.

**Validation**:
- All 64 dart tests pass
- All 74 REPL tests pass
- Manual testing of complex programs (hanoi, merge, qsort)

---

## Order of Work

```
Phase 1 (constants)     → no risk, pure addition
Phase 2 (encoder)       → compiler changes, tests still use objects
Phase 3 (binary runner) → parallel runners, can compare results
Phase 4 (module format) → file I/O, persistence
Phase 5 (cleanup)       → remove old code after validation
```

## Testing Strategy

**Each phase must**:
1. Keep existing tests passing
2. Add new tests for new functionality
3. Compare old/new behavior where applicable

**Regression tests**:
- `dart test` (64 unit tests)
- `run_repl_tests.sh` (74 REPL tests)
- Manual: hello, merge, hanoi, qsort

## Dependencies

- Phase 2 depends on Phase 1
- Phase 3 depends on Phase 2
- Phase 4 depends on Phase 3
- Phase 5 depends on Phase 4

Phases 1-3 can proceed without file I/O (in-memory binary).
Phase 4 adds persistence.
Phase 5 is cleanup only after full validation.

## Estimated Complexity

| Phase | Files | Risk | Notes |
|-------|-------|------|-------|
| 1 | 2 new | None | Constants only |
| 2 | 2 new, 1 mod | Low | Parallel output |
| 3 | 1 new | Medium | New execution path |
| 4 | 2 new, 2 mod | Medium | File I/O |
| 5 | 3 delete/mod | High | Removes fallback |
