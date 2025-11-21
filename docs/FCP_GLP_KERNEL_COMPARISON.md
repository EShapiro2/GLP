# FCP Ask/Tell Kernels vs GLP Guard/System Predicates Comparison

## FCP ASK KERNELS (Three-valued: succeed/fail/suspend)

These are guard predicates that can suspend on unbound readers:

### Type Testing Ask Kernels
1. **`otherwise`** - default clause guard
2. **`nonvar(X?)`** - test if X is not a variable
3. **`ask_unknown(X?)`** - test if X is unknown
4. **`known(X?)`** - test if X is known (not unbound variable)
5. **`we(X?)`** - test if X is a writer (writable variable)
6. **`not_we(X?)`** - test if X is not a writer
7. **`ro(X?)`** - test if X is a reader (read-only variable)
8. **`integer(X?)`** - test if X is an integer
9. **`real(X?)`** - test if X is a real number
10. **`string(X?)`** - test if X is a string
11. **`list(X?)`** - test if X is a list
12. **`tuple(X?)`** - test if X is a tuple
13. **`vector(X?)`** - test if X is a vector
14. **`module(X?)`** - test if X is a module
15. **`constant(X?)`** - test if X is a constant
16. **`compound(X?)`** - test if X is compound term
17. **`number(X?)`** - test if X is a number (int or real)
18. **`exceptions(X?)`** - test if exceptions exist
19. **`grounded(X?)`** - test if X is fully ground (no variables)

### Comparison Ask Kernels
20. **`=?=(X?, Y?)`** - test if X and Y are identical
21. **`=\=(X?, Y?)`** - test if X and Y are not identical
22. **`@<(X?, Y?)`** - term ordering less than
23. **`\=(X?, Y?)`** - test if X and Y do not unify
24. **`<(X?, Y?)`** - arithmetic less than
25. **`=<(X?, Y?)`** - arithmetic less than or equal

### Conversion/Utility Ask Kernels
26. **`bitwise_not(X?, Y^)`** - bitwise NOT operation
27. **`string_length(S?, L^)`** - get string length
28. **`string_hash(S?, H^)`** - get string hash
29. **`cnvrt_to_integer(X?, I^)`** - convert to integer
30. **`cnvrt_to_real(X?, R^)`** - convert to real
31. **`cnvrt_to_string(X?, S^)`** - convert to string
32. **`string_to_dlist(S?, L^, Tail^)`** - string to difference list
33. **`list_to_string(Mode?, L?, S^)`** - list to string
34. **`make_tuple(N?, T^)`** - create tuple of size N
35. **`read_vector(N?, Vec?, Element^)`** - read from vector
36. **`info(ID?, Value^)`** - get runtime statistics
37. **`code_info(Code?, Info^)`** - get code information

---

## FCP TELL KERNELS (Two-valued: succeed/fail, can have side effects)

These predicates execute after commit and can mutate state:

### Control/Scheduling Tell Kernels
1. **`deschedule()`** - deschedule current process
2. **`priority(Old?, New?)`** - change process priority

### I/O Tell Kernels
3. **`machine_output(X?)`** - output to machine stream
4. **`ttyput_byte(C?)`** - output byte to terminal
5. **`ttyput_integer(I?)`** - output integer to terminal
6. **`ttyput_string(S?)`** - output string to terminal
7. **`request(Request?, Answer^)`** - add request to request stream

### Variable/Term Manipulation Tell Kernels
8. **`=(X?, Y?)`** - unification (tell form)
9. **`bind_ro(RO?, Value?)`** - bind read-only variable
10. **`make_unshared(X?)`** - make term unshared
11. **`make_shared(X?, Writer^, Reader^)`** - create shared variable
12. **`make_invalid(X?, Y^)`** - create invalid term

### Vector Manipulation Tell Kernels
13. **`close_vector(N?, Vec?)`** - close vector at position N
14. **`write_vector(N?, Element?, Vec?, Vec'^)`** - write to vector
15. **`store_vector(N?, Element?, Vec?, Vec'^)`** - store in vector
16. **`make_vector(N?, Ref^, Out^)`** - create vector with N elements

### Meta/System Tell Kernels
17. **`execute(Goal?, Result^)`** - execute goal (compute arithmetic, etc.)
18. **`activate(Goal?, Priority?, Result^)`** - activate suspended goal
19. **`link(ObjLibs?, Offset^)`** - dynamic linking
20. **`melt(OldHeap?, NewHeap^, Success^)`** - heap melting
21. **`debug(Type?, Mode?)`** - set debug flags

---

## GLP GUARD PREDICATES (Ask-like)

From `/home/user/GLP/docs/glp-bytecode-v216-complete.md`:

### Implemented Guards
1. **`ground(X)`** - test if X contains no variables
2. **`known(X)`** - test if X is not a variable
3. **`writer(X)`** - test if X is unbound writer
4. **`reader(X)`** - test if X is unbound reader
5. **`otherwise`** - default clause guard

### Planned Guards (Section 19)
6. **`guard_less(X, Y)`** - arithmetic X < Y
7. **`guard_greater(X, Y)`** - arithmetic X > Y
8. **`guard_less_equal(X, Y)`** - arithmetic X <= Y
9. **`guard_greater_equal(X, Y)`** - arithmetic X >= Y
10. **`guard_arith_equal(X, Y)`** - arithmetic X == Y
11. **`guard_arith_not_equal(X, Y)`** - arithmetic X != Y
12. **`guard_ground(X)`** - alias for ground
13. **`guard_known(X)`** - alias for known
14. **`guard_integer(X)`** - test if integer
15. **`guard_number(X)`** - test if number
16. **`guard_writer(X)`** - alias for writer
17. **`guard_reader(X)`** - alias for reader
18. **`guard_true()`** - always succeeds
19. **`guard_otherwise()`** - alias for otherwise
20. **`guard_unify(X, Y)`** - test if X and Y unify
21. **`guard_not_unifiable(X, Y)`** - test if X and Y don't unify

---

## GLP SYSTEM PREDICATES (Tell-like)

From `/home/user/GLP/docs/glp-bytecode-v216-complete.md`:

### Arithmetic (Two-valued, aborts on unbound)
1. **`evaluate(Expression, Result)`** - evaluate arithmetic expression

### Utilities
2. **`current_time(Time)`** - get current timestamp
3. **`unique_id(ID)`** - generate unique ID
4. **`variable_name(Var, Name)`** - get variable name
5. **`copy_term(Term, Copy)`** - deep copy term

### File I/O (Simple)
6. **`file_read(Path, Contents)`** - read entire file
7. **`file_write(Path, Contents)`** - write entire file
8. **`file_exists(Path)`** - check if file exists

### File I/O (Handle-based)
9. **`file_open(Path, Mode, Handle)`** - open file, get handle
10. **`file_close(Handle)`** - close file handle
11. **`file_read_handle(Handle, Contents)`** - read from handle
12. **`file_write_handle(Handle, Contents)`** - write to handle

### Directory
13. **`directory_list(Path, Entries)`** - list directory contents

### Terminal I/O
14. **`write(Term)`** - write term to output
15. **`nl()`** - write newline
16. **`read(Term)`** - read term from input

### Module Loading
17. **`link(ModulePath, Handle)`** - load/link module
18. **`load_module(FileName, Module)`** - load module from file

---

## COMPARISON ANALYSIS

### FCP Ask Kernels NOT in GLP Guards

**Type Tests Missing in GLP:**
- `nonvar` - GLP has `known` which is similar
- `ask_unknown` - opposite of `known`
- `not_we` - can be derived from `writer` failing
- `real` - GLP has `guard_number` but not specifically `real`
- `string` - no string type test guard
- `list` - no list type test guard
- `tuple` - no tuple/structure type test guard
- `vector` - no vector type test guard
- `module` - no module type test guard
- `constant` - no constant test guard
- `compound` - no compound term test guard
- `exceptions` - no exception testing

**Comparison Operators Missing in GLP:**
- `=?=` (identity test) - not in GLP
- `=\=` (non-identity) - not in GLP
- `@<` (term ordering) - not in GLP
- `\=` (non-unifiable) - GLP has `guard_not_unifiable` (planned)

**Conversion/Utility Missing in GLP:**
- `bitwise_not` - no bitwise operations in guards
- `string_length` - no string operations in guards
- `string_hash` - no string operations in guards
- `cnvrt_to_integer/real/string` - no conversion guards
- `string_to_dlist` - no string/list conversion
- `list_to_string` - no list/string conversion
- `make_tuple` - no tuple creation in guards
- `read_vector` - no vector operations in guards
- `info` - no runtime info guards
- `code_info` - no code introspection guards

### FCP Tell Kernels NOT in GLP System Predicates

**Control/Scheduling:**
- `deschedule()` - no explicit deschedule
- `priority()` - no priority management
- `activate()` - no explicit goal activation (handled by runtime)

**I/O:**
- `machine_output()` - no machine stream concept
- `ttyput_byte()` - GLP has `write()` but not byte-level
- `ttyput_integer()` - GLP `write()` handles this
- `ttyput_string()` - GLP `write()` handles this
- `request()` - no request/answer stream

**Variable Manipulation:**
- `bind_ro()` - no explicit RO binding predicate
- `make_unshared()` - no sharing control
- `make_shared()` - no explicit shared variable creation
- `make_invalid()` - no invalid term creation

**Vector Operations:**
- All vector operations (`close_vector`, `write_vector`, `store_vector`, `make_vector`) - **GLP has no vector type**

**Meta/System:**
- `melt()` - no heap melting operation
- `debug()` - no runtime debug control predicate
- FCP `execute()` is similar to GLP `evaluate()` but more general

### GLP Predicates NOT in FCP

**GLP-Specific System Predicates:**
- `current_time()` - FCP uses `info(12, Time)` for elapsed time
- `unique_id()` - no equivalent in FCP
- `variable_name()` - no equivalent in FCP
- `copy_term()` - no equivalent in FCP
- File I/O predicates - **FCP has no built-in file I/O**
- Directory operations - **FCP has no directory operations**
- `read()` - FCP has no term reading predicate
- `load_module()` - FCP has `link()` for dynamic linking

---

## MISSING PREDICATE CATEGORIES

### 1. **String Operations** (FCP has, GLP missing)
FCP has extensive string manipulation (`string_length`, `string_hash`, `string_to_dlist`, `list_to_string`, conversions). GLP has no string guards or string system predicates.

### 2. **Vector/Mutable Arrays** (FCP has, GLP missing)
FCP has full vector support (`make_vector`, `read_vector`, `write_vector`, `store_vector`, `close_vector`). **GLP has no vector type at all**.

### 3. **Type Testing Guards** (FCP has more)
FCP has detailed type tests (`integer`, `real`, `string`, `list`, `tuple`, `vector`, `module`, `constant`, `compound`, `number`). GLP only has `ground`, `known`, `writer`, `reader`, plus planned `guard_integer`, `guard_number`.

### 4. **Term Comparison** (FCP has, GLP missing)
FCP has identity tests (`=?=`, `=\=`) and term ordering (`@<`). GLP has no identity or term ordering guards.

### 5. **Bitwise Operations** (FCP has, GLP missing)
FCP has `bitwise_not`. GLP has no bitwise operations.

### 6. **Runtime Introspection** (FCP has, GLP missing)
FCP has `info()` for statistics, `code_info()` for code introspection, `exceptions()` for exception testing. GLP has no introspection predicates.

### 7. **Process Control** (FCP has, GLP missing)
FCP has `deschedule()`, `priority()`, `activate()`. GLP scheduler is implicit, no process control predicates.

### 8. **Shared Variable Management** (FCP has, GLP missing)
FCP has `make_shared()`, `make_unshared()`, `bind_ro()`. GLP SRSW design doesn't need explicit sharing control.

### 9. **File I/O** (GLP has, FCP missing)
**GLP has extensive file I/O** (`file_read`, `file_write`, `file_open`, `file_close`, `file_exists`, handle-based operations, `directory_list`). **FCP has NO file I/O predicates**.

### 10. **Module Loading** (Both have, different approaches)
- FCP: `link()` for dynamic C linking
- GLP: `link()` and `load_module()` for GLP module loading

---

## RECOMMENDATIONS

### Critical Missing Predicates for GLP

**High Priority (Core Language Features):**
1. **Type guards**: `guard_string`, `guard_list`, `guard_tuple` - needed for practical programming
2. **Term comparison**: `guard_identical`, `guard_not_identical`, `guard_term_less` - needed for term ordering
3. **Conversion guards**: `guard_to_integer`, `guard_to_string` - useful for parsing/validation

**Medium Priority (Nice to Have):**
4. **String operations**: `string_length`, `string_concat` - if GLP gets first-class strings
5. **Runtime info**: `runtime_info(Key, Value)` - for debugging and monitoring
6. **Bitwise operations**: If needed for systems programming

**Low Priority (Design-Specific):**
7. **Vector operations** - Only if GLP adds mutable vector type
8. **Process control** - Only if exposing scheduler control is desired
9. **Shared variable control** - Not needed due to SRSW design

### FCP Predicates That Don't Apply to GLP

1. **Vector operations** - GLP philosophy: immutable data structures
2. **`deschedule/activate/priority`** - GLP hides scheduler from user
3. **`make_shared/make_unshared/bind_ro`** - SRSW makes these unnecessary
4. **`machine_output/request`** - FCP-specific I/O model

---

## CONCLUSION

**FCP has 37 ask kernels, GLP has 21 guard predicates (5 implemented, 16 planned).**
**FCP has 21 tell kernels, GLP has 18 system predicates.**

**Major differences:**
1. **FCP has vectors, GLP doesn't** - explains 5 missing tell kernels
2. **GLP has file I/O, FCP doesn't** - explains 9 extra GLP system predicates
3. **FCP has extensive type tests, GLP is minimal** - explains 12+ missing ask kernels
4. **FCP exposes process control, GLP hides it** - explains 3 missing tell kernels
5. **SRSW simplifies GLP** - no need for `make_shared`, `bind_ro`, etc.

**The core ask/tell vs guard/system distinction is preserved.** The differences are mostly:
- **Feature scope** (vectors, strings, type tests)
- **Design philosophy** (SRSW vs full concurrency, hidden vs exposed scheduler)
- **I/O model** (files vs streams)
