# GLP Binary Bytecode Specification

**Status**: Draft
**Based on**: FCP/Logix Abstract Machine (EFCP)

## Overview

GLP uses the FCP abstract machine bytecode format. This specification documents the FCP format as used by GLP.

## 1. Word Format (FCP fcp.h)

Heap words are 32-bit unsigned integers with tagged representation:

```
┌─────────────────────────────────────────────────────────────────┐
│  31                           6  5        2  1        0         │
│  ├──────── Value (26 bits) ─────┼─ Code (4) ─┼─ Flags (2) ─┤    │
└─────────────────────────────────────────────────────────────────┘
```

### 1.1 Flags (2 bits)

| Flag | Value | FCP Name |
|------|-------|----------|
| RefFlag | 0x0 | Reference (variable) |
| RegularFlag | 0x1 | Regular term |
| ListFlag | 0x2 | List cell |

### 1.2 Type Codes (4 bits)

| Code | Value | FCP Name | Description |
|------|-------|----------|-------------|
| WrtCode | 0x0 | Writable | Writer variable (we) |
| RoCode | 0x1 | Read-Only | Reader variable (ro) |
| IntCode | 0x2 | Integer | Signed integer (±2²⁵) |
| RealCode | 0x3 | Real | Float (heap indirect) |
| StrCode | 0x4 | String | String (heap indirect) |
| NilCode | 0x5 | Nil | Empty list [] |
| TplCode | 0x6 | Tuple | Structure f/n |
| VctrCode | 0x7 | Vector | Array |
| InvldCode | 0x8 | Invalid | Error marker |

### 1.3 Word Construction

```c
#define Word(Val, Tag) ((((heapT)(Val)) << 6) | (Tag))
#define Tag(Code, Flag) (((Code) << 2) | (Flag))
```

## 2. Opcodes (FCP opcodes.h)

Instructions are 16-bit opcodes. FCP organizes them by category:

### 2.1 Dereference Operations (0x001-0x00f)

| Opcode | Value | Description |
|--------|-------|-------------|
| deref_2 | 0x001 | Dereference variable |
| deref_3 | 0x002 | Dereference with 3 args |
| deref_subarg_3 | 0x003 | Dereference structure subarg |
| deref_subarg_2 | 0x004 | Dereference subarg (2 args) |
| deref_car_3 | 0x005 | Dereference list head |
| deref_car_2 | 0x006 | Dereference list head (2 args) |
| deref_list | 0x007 | Dereference list |
| deref_car_list | 0x008 | Dereference car of list |
| deref_sub_list | 0x009 | Dereference sublist |
| deref_vars | 0x00a | Dereference multiple vars |
| deref_var1 | 0x00b | Dereference single var |
| deref_2_addr | 0x00c | Dereference to address |
| deref_integer1 | 0x00d | Dereference integer |
| load_car | 0x00e | Load car |
| deref_list3 | 0x00f | Dereference list (3 args) |

### 2.2 Load Operations (0x010-0x020)

| Opcode | Value | Description |
|--------|-------|-------------|
| load_we_var | 0x010 | Load writer variable |
| load_ref_to_we_var | 0x011 | Load ref to writer |
| load_ro_of_reg | 0x012 | Load reader from register |
| load_ref_to_ro_of_reg | 0x013 | Load ref to reader |
| load_ro_of_subarg | 0x014 | Load reader from subarg |
| load_ref_to_ro_of_subarg | 0x015 | Load ref to reader subarg |
| load_reg | 0x016 | Load register |
| load_reg_indirect | 0x017 | Load register indirect |
| load_car_of_reg | 0x018 | Load car of register |
| load_pr_arg | 0x019 | Load procedure argument |
| load_subarg | 0x01a | Load subargument |
| load_ref_to_subarg | 0x01b | Load ref to subarg |
| load_nil | 0x01c | Load nil |
| load_word | 0x01d | Load word constant |
| load_real | 0x01e | Load real |
| load_ref_to_real | 0x01f | Load ref to real |
| load_ref_to_string | 0x020 | Load ref to string |

### 2.3 Allocate Operations (0x021-0x029)

| Opcode | Value | Description |
|--------|-------|-------------|
| allocate_var | 0x021 | Allocate variable |
| allocate_vars | 0x022 | Allocate N variables |
| allocate_tuple | 0x023 | Allocate tuple/structure |
| allocate_list_cell | 0x024 | Allocate list cell |
| allocate_list_we | 0x025 | Allocate list with writer |
| allocate_listN | 0x026 | Allocate list of N |
| allocate_pr | 0x027 | Allocate process record |
| list_assign_with_check | 0x028 | List assign with check |
| list_assign | 0x029 | List assign |

### 2.4 Copy Operations (0x02b-0x051)

FCP has many copy variants for different source/destination combinations:
- Sources: Rs (register), CpIs (constant), SRs (subregister), WeVar, etc.
- Destinations: Rd (register), CpId (constant), SRd (subregister)

| Opcode | Value | Description |
|--------|-------|-------------|
| copy_Rs_Rd | 0x02b | Copy register to register |
| copy_CpIs_Rd | 0x02c | Copy constant to register |
| copy_SRs_Rd | 0x02d | Copy subreg to register |
| ... | ... | (many variants) |
| copy_WeVar_SRd | 0x051 | Copy writer var to subreg |

### 2.5 Control Flow (0x089-0x0a6)

| Opcode | Value | Description |
|--------|-------|-------------|
| goto_there | 0x089 | Unconditional jump |
| if_not_reference | 0x08a | Branch if not reference |
| if_not_writable | 0x08b | Branch if not writer |
| if_not_read_only | 0x08c | Branch if not reader |
| if_not_integer | 0x08d | Branch if not integer |
| if_not_real | 0x08e | Branch if not real |
| if_not_string | 0x08f | Branch if not string |
| if_not_nil | 0x090 | Branch if not nil |
| if_not_list | 0x091 | Branch if not list |
| if_not_tuple | 0x092 | Branch if not tuple |
| if_not_module | 0x093 | Branch if not module |
| if_not_vector | 0x094 | Branch if not vector |

### 2.6 Unification (0x0ad-0x0c2)

| Opcode | Value | Description |
|--------|-------|-------------|
| switch_on_tag | 0x0ad | Multi-way branch on type |
| unify_args | 0x0ae | Unify structure arguments |
| unify_reg_reg | 0x0af | Unify register with register |
| unify_reg_xreg | 0x0b0 | Unify reg with indexed reg |
| ... | ... | (many variants) |

### 2.7 Branching (0x0c3-0x0c7)

| Opcode | Value | Description |
|--------|-------|-------------|
| branch_integer | 0x0c3 | Branch on integer value |
| branch_real | 0x0c4 | Branch on real value |
| branch_tuple | 0x0c5 | Branch on tuple functor |
| case_hash_integer | 0x0c6 | Hash table for integers |
| case_hash_string | 0x0c7 | Hash table for strings |

### 2.8 Execution Control (0x11e-0x13e)

| Opcode | Value | Description |
|--------|-------|-------------|
| enqueue | 0x11e | Enqueue goal (spawn) |
| iterate1 | 0x11f | Iterate (single) |
| iterate | 0x120 | Iterate (tail call) |
| execute | 0x121 | Execute procedure |
| execute2 | 0x122 | Execute (2 args) |
| execute1 | 0x123 | Execute (1 arg) |
| halt | 0x124 | Halt execution |
| commit1 | 0x125 | Commit clause |
| commit_nolabel | 0x126 | Commit without label |
| suspend2 | 0x127 | Suspend (2 args) |
| suspend1 | 0x128 | Suspend (1 arg) |
| suspend | 0x129 | Suspend goal |
| suspend_on | 0x12a | Suspend on variable |
| set_HBT | 0x12d | Set heap backtrack |
| undo | 0x12e | Undo to backtrack point |

### 2.9 Ask Kernels (0x140-0x170)

| Opcode | Value | Description |
|--------|-------|-------------|
| otherwise | 0x140 | Otherwise guard |
| is_nonvar | 0x141 | Test if not variable |
| is_known | 0x142 | Test if known |
| is_unknown | 0x143 | Test if unknown |
| is_var | 0x144 | Test if variable |
| is_we | 0x145 | Test if writer |
| is_not_we | 0x146 | Test if not writer |
| is_ro | 0x147 | Test if reader |
| is_integer | 0x148 | Test if integer |
| is_real | 0x149 | Test if real |
| is_string | 0x14a | Test if string |
| is_list | 0x14b | Test if list |
| is_tuple | 0x14c | Test if tuple |
| is_vector | 0x14d | Test if vector |
| is_module | 0x14e | Test if module |
| is_number | 0x14f | Test if number |
| grounded | 0x150 | Test if ground |
| ... | ... | (comparison ops, arithmetic) |

## 3. Instruction Encoding

### 3.1 Format

All instructions start with 16-bit opcode, followed by operands:

```
┌────────────────┬────────────────────────────────┐
│  opcode (16)   │  operands (variable length)    │
└────────────────┴────────────────────────────────┘
```

### 3.2 Operand Encoding

- Register index: 16 bits
- Constant index: 16 bits (into constant table)
- PC offset: 16 bits (relative jump)
- Arity: 8 bits
- Functor: 16 bits (string table index)

## 4. Module Format

### 4.1 Header

```
┌─────────────────────────────────────────────────────────────┐
│  Magic: 0x474C5001 ("GLP\x01")                    (32 bits) │
│  Version                                          (16 bits) │
│  Flags                                            (16 bits) │
│  Module Name Index                                (16 bits) │
│  String Table Offset                              (32 bits) │
│  String Table Size                                (32 bits) │
│  Code Offset                                      (32 bits) │
│  Code Size                                        (32 bits) │
│  Label Table Offset                               (32 bits) │
│  Label Count                                      (16 bits) │
│  Export Table Offset                              (32 bits) │
│  Export Count                                     (16 bits) │
└─────────────────────────────────────────────────────────────┘
```

### 4.2 String Table

Length-prefixed UTF-8 strings, word-aligned:

```
┌──────────────────────────────────────┐
│  Length (16) │ UTF-8 bytes │ padding │
└──────────────────────────────────────┘
```

### 4.3 Label Table

```
┌──────────────────────────────────────────────┐
│  Name Index (16) │ Arity (8) │ PC (32)       │
└──────────────────────────────────────────────┘
```

### 4.4 Export Table

```
┌──────────────────────────────────────┐
│  Name Index (16) │ Arity (8)         │
└──────────────────────────────────────┘
```

## 5. Migration from Current Implementation

The current GLP implementation uses Dart objects. Migration path:

1. Define FCP opcode constants in Dart
2. Compiler emits binary bytecode
3. Runner decodes and dispatches on opcodes
4. Remove object-based instructions

## References

- FCP opcodes.h: https://github.com/EShapiro2/FCP/blob/main/Savannah/Logix/EMULATOR/opcodes.h
- FCP fcp.h: https://github.com/EShapiro2/FCP/blob/main/Savannah/Logix/EMULATOR/fcp.h
- FCP emulate.c: https://github.com/EShapiro2/FCP/blob/main/Savannah/Logix/EMULATOR/emulate.c
