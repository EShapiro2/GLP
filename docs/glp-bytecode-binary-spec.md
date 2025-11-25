# GLP Binary Bytecode Specification

**Status**: Draft
**Based on**: FCP/Logix Abstract Machine (EFCP)

## Overview

This specification defines the binary bytecode format for GLP, following the FCP abstract machine design. The format uses:
- 32-bit heap words with tagged representation
- 16-bit instruction opcodes
- Module-based organization

## 1. Word Format

Following FCP, heap words are 32-bit unsigned integers with tag/value encoding:

```
┌─────────────────────────────────────────────────────────────────┐
│  31                           6  5        2  1        0         │
│  ├──────── Value (26 bits) ─────┼─ Code (4) ─┼─ Flags (2) ─┤    │
└─────────────────────────────────────────────────────────────────┘
```

### 1.1 Flags (2 bits)

| Flag | Value | Meaning |
|------|-------|---------|
| RefFlag | 0x0 | Reference (variable) |
| RegularFlag | 0x1 | Regular term |
| ListFlag | 0x2 | List cell |

### 1.2 Type Codes (4 bits)

| Code | Value | Type | Description |
|------|-------|------|-------------|
| WrtCode | 0x0 | Writer Variable | Writable/output variable |
| RoCode | 0x1 | Reader Variable | Read-only/input variable |
| IntCode | 0x2 | Integer | Signed integer (26-bit range) |
| RealCode | 0x3 | Real | Floating point (heap indirect) |
| StrCode | 0x4 | String | String (heap indirect) |
| NilCode | 0x5 | Nil | Empty list [] |
| TplCode | 0x6 | Tuple/Structure | Compound term |
| VctrCode | 0x7 | Vector | Array-like structure |
| InvldCode | 0x8 | Invalid | Error marker |

### 1.3 Tag Construction

```
Tag = (Code << 2) | Flags
Word = (Value << 6) | Tag
```

Examples:
- Writer variable with ID 42: `Word(42, (WrtCode << 2) | RefFlag)` = `(42 << 6) | 0x00`
- Reader variable with ID 42: `Word(42, (RoCode << 2) | RefFlag)` = `(42 << 6) | 0x04`
- Integer 100: `Word(100, (IntCode << 2) | RegularFlag)` = `(100 << 6) | 0x09`
- Nil: `Word(0, (NilCode << 2) | RegularFlag)` = `0x15`

## 2. GLP Opcode Mapping to FCP

### 2.1 Current GLP Instructions → FCP Opcodes

| Current GLP (Dart Object) | FCP Opcode | Value | Notes |
|---------------------------|------------|-------|-------|
| **Clause Control** ||||
| `Label` | label | 0x000 | Pseudo-op (metadata) |
| `ClauseTry` | clause_try | 0x0f0 | Start clause attempt |
| `ClauseNext` | clause_next | 0x0f1 | Next clause alternative |
| `NoMoreClauses` | no_more_clauses | 0x0f2 | End of clauses |
| `Otherwise` | otherwise | 0x140 | FCP: ask kernel |
| `GuardFail` | guard_fail | 0x0f4 | Guard failure |
| `Commit` | commit | 0x125 | FCP: commit1 |
| `Proceed` | proceed | 0x0d4 | Return from procedure |
| `SuspendEnd` | suspend_end | 0x0f5 | End suspension block |
| **Head Matching** ||||
| `HeadConstant` | head_const | 0x102 | Match constant |
| `HeadNil` | head_nil | 0x104 | Match [] |
| `HeadStructure` | head_struct | 0x105 | Match structure |
| `HeadList` | head_list | 0x106 | Match [H\|T] |
| `HeadVariable(isReader:false)` | head_var_we | 0x100 | Match writer var |
| `HeadVariable(isReader:true)` | head_var_ro | 0x101 | Match reader var |
| `GetVariable(isReader:false)` | get_var_we | 0x110 | Get writer from arg |
| `GetVariable(isReader:true)` | get_var_ro | 0x111 | Get reader from arg |
| `GetValue(isReader:false)` | get_val_we | 0x112 | Get writer value |
| `GetValue(isReader:true)` | get_val_ro | 0x113 | Get reader value |
| **Structure Traversal** ||||
| `Push` | push | 0x180 | Save struct context |
| `Pop` | pop | 0x181 | Restore struct context |
| `UnifyStructure` | unify_struct | 0x0ae | FCP: unify_args variant |
| `UnifyConstant` | unify_const | 0x184 | Unify with constant |
| `UnifyVariable(isReader:false)` | unify_var_we | 0x182 | Unify writer in struct |
| `UnifyVariable(isReader:true)` | unify_var_ro | 0x183 | Unify reader in struct |
| `UnifyVoid` | unify_void | 0x188 | Skip/create void |
| **Body Building** ||||
| `PutConstant` | put_const | 0x132 | Put constant in arg |
| `PutNil` | put_nil | 0x134 | Put [] in arg |
| `PutStructure` | put_struct | 0x135 | Put structure in arg |
| `PutList` | put_list | 0x136 | Put [H\|T] in arg |
| `PutVariable(isReader:false)` | put_var_we | 0x130 | Put writer in arg |
| `PutVariable(isReader:true)` | put_var_ro | 0x131 | Put reader in arg |
| `SetConstant` | set_const | 0x187 | Set constant in struct |
| `SetVariable(isReader:false)` | set_var_we | 0x185 | Set writer in struct |
| `SetVariable(isReader:true)` | set_var_ro | 0x186 | Set reader in struct |
| **Spawn & Execution** ||||
| `Spawn` | spawn | 0x160 | FCP: enqueue |
| `CallRemote` | spawn_remote | 0x161 | Cross-module spawn |
| `Requeue` | iterate | 0x120 | FCP: iterate/tail call |
| `TailStep` | tail_step | 0x11f | FCP: iterate1 |
| `Execute` | execute | 0x121 | FCP: execute |
| `Halt` | halt | 0x124 | FCP: halt |
| **Guards** ||||
| `IfVariable(isReader:false)` | if_writer | 0x145 | FCP: is_we |
| `IfVariable(isReader:true)` | if_reader | 0x147 | FCP: is_ro |
| `Ground` | ground | 0x148 | Test ground term |
| `Known` | known | 0x141 | FCP: is_nonvar |
| `Guard` | guard_call | 0x14a | Call guard predicate |
| **Environment** ||||
| `Allocate` | allocate | 0x021 | FCP: allocate_vars |
| `Deallocate` | deallocate | 0x02a | Deallocate frame |
| **Control Flow** ||||
| `Nop` | nop | 0x000 | No operation |
| `goto` (implicit) | goto | 0x089 | FCP: goto_there |

## 3. Instruction Formats

### 3.1 No-operand instructions
```
┌────────────────┐
│  opcode (16)   │
└────────────────┘
```
Examples: `proceed`, `halt`, `commit`

### 3.2 Single-operand instructions
```
┌────────────────┬────────────────┐
│  opcode (16)   │  operand (16)  │
└────────────────┴────────────────┘
```
Examples: `goto`, `head_const`, `put_int`

### 3.3 Two-operand instructions
```
┌────────────────┬────────────────┬────────────────┐
│  opcode (16)   │  operand1 (16) │  operand2 (16) │
└────────────────┴────────────────┴────────────────┘
```
Examples: `head_var_reader(varIndex, argSlot)`, `copy_reg_reg(src, dst)`

### 3.4 Variable-length instructions
```
┌────────────────┬────────────────┬─────────────────────┐
│  opcode (16)   │  count (16)    │  data (count × 16)  │
└────────────────┴────────────────┴─────────────────────┘
```
Examples: `switch_on_tag`, `branch_integer`

## 4. Module Format

### 4.1 Module Header
```
┌─────────────────────────────────────────────────────────────┐
│  Magic Number (32): 0x474C5001 ("GLP\x01")                  │
│  Version (16): 0x0001                                       │
│  Flags (16): Module flags                                   │
│  Name Offset (32): Offset to module name in string table    │
│  String Table Offset (32): Offset to string table           │
│  String Table Size (32): Size of string table               │
│  Code Offset (32): Offset to instruction stream             │
│  Code Size (32): Size of instruction stream                 │
│  Label Table Offset (32): Offset to label table             │
│  Label Count (32): Number of labels                         │
│  Export Table Offset (32): Offset to export table           │
│  Export Count (32): Number of exports                       │
└─────────────────────────────────────────────────────────────┘
```

### 4.2 String Table
```
┌─────────────────────────────────────────────────────────────┐
│  String 0: Length (16) + UTF-8 bytes + padding              │
│  String 1: Length (16) + UTF-8 bytes + padding              │
│  ...                                                        │
└─────────────────────────────────────────────────────────────┘
```

### 4.3 Label Table
```
┌─────────────────────────────────────────────────────────────┐
│  Label 0: Name Index (16) + Arity (8) + PC (32)             │
│  Label 1: Name Index (16) + Arity (8) + PC (32)             │
│  ...                                                        │
└─────────────────────────────────────────────────────────────┘
```

### 4.4 Export Table
```
┌─────────────────────────────────────────────────────────────┐
│  Export 0: Name Index (16) + Arity (8)                      │
│  Export 1: Name Index (16) + Arity (8)                      │
│  ...                                                        │
└─────────────────────────────────────────────────────────────┘
```

### 4.5 Instruction Stream
```
┌─────────────────────────────────────────────────────────────┐
│  Instruction 0: opcode (16) + operands...                   │
│  Instruction 1: opcode (16) + operands...                   │
│  ...                                                        │
└─────────────────────────────────────────────────────────────┘
```

## 5. Comparison with Current GLP Implementation

### 5.1 Current State (Object-Based)

The current GLP implementation uses Dart objects for instructions:
```dart
final ops = [Label('merge/3'), ClauseTry(), HeadVariable(0, isReader: true), ...]
```

### 5.2 Target State (Binary Bytecode)

The target implementation uses numeric opcodes:
```dart
final bytecode = Uint16List.fromList([
  0x0000, // Label: name_index=0
  0x0f00, // clause_try
  0x0100, 0x0000, // head_var_reader: varIndex=0
  ...
]);
```

## 6. Migration Path

1. **Phase 1**: Define opcode constants matching FCP
2. **Phase 2**: Add binary encoder to compiler
3. **Phase 3**: Add binary decoder to runner
4. **Phase 4**: Update runner to dispatch on numeric opcodes
5. **Phase 5**: Remove object-based instruction support

## 7. Open Questions

1. Should we use exact FCP opcode values or define our own?
2. What's the integer range we need (FCP uses 26-bit)?
3. Do we need all FCP opcodes or a subset for GLP?
4. How to handle GLP-specific features (SRSW, etc.)?

## References

- FCP/EFCP opcodes.h: https://github.com/EShapiro2/FCP/blob/main/Savannah/Logix/EMULATOR/opcodes.h
- FCP/EFCP fcp.h: https://github.com/EShapiro2/FCP/blob/main/Savannah/Logix/EMULATOR/fcp.h
