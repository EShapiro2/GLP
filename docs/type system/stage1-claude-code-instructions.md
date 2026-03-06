# Stage 1: Parameterized Types — Claude Code Instructions

## 🔴 MANDATORY READING — Complete Before Any Work

Read these files IN ORDER before doing anything else:

1. **`/Users/udi/Grassroots/GLP/CLAUDE.md`** — Claude Code rules, workflow, git protocol, working modes
2. **`/Users/udi/Grassroots/claude.md`** — Project-wide rules (output size, writing style, bibliography, git push workflow)
3. **`/Users/udi/Grassroots/GLP/docs/DISCIPLINE.md`** — Development discipline rules
4. **`/Users/udi/Grassroots/GLP/docs/typed-glp-manual.md`** — GLP programming manual (especially Section 17: Parameterized Types)
5. **`/Users/udi/Grassroots/GLP/docs/glp-cheat-sheet.md`** — GLP programming cheat sheet
6. **`/Users/udi/Grassroots/GLP/docs/type system/typed-program.md`** — Type system spec including parameterized types section
7. **`/Users/udi/Grassroots/GLP/docs/type system/type-automaton.md`** — Type automaton spec (note: unchanged by this work)
8. **`/Users/udi/Grassroots/GLP/docs/type system/IMPLEMENTATION-PLAN.md`** — Implementation plan (Phase 7 is this work)
9. **`/Users/udi/Grassroots/GLP/docs/type system/parameterized-types-plan.md`** — Two-stage plan for parameterized types
10. **This file** (`docs/type system/stage1-claude-code-instructions.md`) — Detailed implementation instructions

Key source files to read before implementing:

11. **`glp_runtime/lib/analysis/type_checker/type_ast.dart`** — Type AST (TypeDef, TypeRef, ProcDecl, etc.)
12. **`glp_runtime/lib/compiler/parser.dart`** — Parser (type definition and procedure declaration parsing)
13. **`glp_runtime/lib/analysis/type_checker/type_conversion.dart`** — Term-to-TypeExpr conversion
14. **`glp_runtime/lib/compiler/analyzer.dart`** — Compilation pipeline (integration point for expansion)

After reading all of the above, acknowledge each one, then STOP and WAIT for direction.

---

**Spec**: `docs/type system/typed-program.md`, section "Parameterized Types"
**Paper**: Moded-Types paper, Section 8
**Plan**: `docs/type system/parameterized-types-plan.md`

## Overview

Add parameterized type support. This is a **three-part change**:

1. **Type AST** (`type_ast.dart`): Add fields for type parameters and type arguments
2. **Parser** (`parser.dart`): Parse parameterized syntax in type definitions and procedure declarations
3. **Expansion** (`param_expansion.dart`): New preprocessing step that expands parameterized types to monomorphic types before type automaton construction

After expansion, all downstream machinery (type automaton, well-typing, subtyping) is unchanged.

---

## Part 1: Type AST Changes

**File**: `glp_runtime/lib/analysis/type_checker/type_ast.dart`

### 1a. Add type parameters to `TypeDef`

```dart
class TypeDef {
  final String name;
  final List<String> typeParams;  // NEW: e.g., ['X'] for Stream(X), [] for monomorphic
  final List<TypeExpr> alternatives;
  final int line;
  final int column;

  TypeDef(this.name, this.alternatives, this.line, this.column, {this.typeParams = const []});

  bool get isParameterized => typeParams.isNotEmpty;
  // ... rest unchanged
}
```

### 1b. Add type arguments to `TypeRef`

```dart
class TypeRef extends TypeExpr {
  final String name;
  final bool isInput;
  final List<TypeExpr> typeArgs;  // NEW: e.g., [TypeRef('Integer')] for Stream(Integer), [] for simple refs

  TypeRef(this.name, int line, int column, {this.isInput = false, this.typeArgs = const []})
      : super(line, column);

  bool get isParameterized => typeArgs.isNotEmpty;

  TypeRef dual() => TypeRef(name, line, column, isInput: !isInput, typeArgs: typeArgs);

  // Update toString
  @override
  String toString() {
    final argsStr = typeArgs.isNotEmpty ? '(${typeArgs.join(', ')})' : '';
    return isInput ? '$name$argsStr?' : '$name$argsStr';
  }

  // Update == and hashCode to include typeArgs
  @override
  bool operator ==(Object other) =>
      other is TypeRef && other.name == name && other.isInput == isInput &&
      _listEquals(other.typeArgs, typeArgs);

  @override
  int get hashCode => Object.hash(name, isInput, Object.hashAll(typeArgs));

  static bool _listEquals(List<TypeExpr> a, List<TypeExpr> b) {
    if (a.length != b.length) return false;
    for (int i = 0; i < a.length; i++) {
      if (a[i] != b[i]) return false;
    }
    return true;
  }
}
```

No other type AST classes change.

---

## Part 2: Parser Changes

**File**: `glp_runtime/lib/compiler/parser.dart`

### 2a. `_isTypeDefinition()` — skip type parameters before checking for `::=`

Current code checks `VARIABLE` then looks for `::=`. With parameterized types, `Stream(X) ::=` has `(X)` between the name and `::=`.

```dart
bool _isTypeDefinition() {
  if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
    final saved = _current;
    _advance();  // consume type name

    // Skip optional type parameters: (X, Y, ...)
    if (_check(TokenType.LPAREN)) {
      _advance(); // consume (
      // Skip to matching )
      int depth = 1;
      while (!_isAtEnd() && depth > 0) {
        if (_check(TokenType.LPAREN)) depth++;
        if (_check(TokenType.RPAREN)) depth--;
        _advance();
      }
    }

    final isTypeDef = _check(TokenType.COLONCOLONEQ);
    _current = saved;
    return isTypeDef;
  }
  return false;
}
```

### 2b. `_parseTypeDef()` — parse optional type parameter list

After consuming the type name token, check for `(` and parse parameter names:

```dart
TypeDef _parseTypeDef() {
  final typeNameToken = _check(TokenType.READER)
      ? _advance()
      : _consume(TokenType.VARIABLE, 'Expected type name');

  final typeName = typeNameToken.type == TokenType.READER
      ? '${typeNameToken.lexeme}?'
      : typeNameToken.lexeme;
  final line = typeNameToken.line;
  final column = typeNameToken.column;

  // NEW: Parse optional type parameters: (X, Y, ...)
  final typeParams = <String>[];
  if (_match(TokenType.LPAREN)) {
    // First parameter
    final firstParam = _consume(TokenType.VARIABLE, 'Expected type parameter name');
    typeParams.add(firstParam.lexeme);
    // Additional parameters
    while (_match(TokenType.COMMA)) {
      final param = _consume(TokenType.VARIABLE, 'Expected type parameter name');
      typeParams.add(param.lexeme);
    }
    _consume(TokenType.RPAREN, 'Expected ")" after type parameters');
  }

  _consume(TokenType.COLONCOLONEQ, 'Expected "::=" in type definition');

  final alternatives = <TypeExpr>[];
  alternatives.add(_parseTypeAlt());
  while (_match(TokenType.SEMICOLON)) {
    alternatives.add(_parseTypeAlt());
  }
  _consume(TokenType.DOT, 'Expected "." after type definition');

  return TypeDef(typeName, alternatives, line, column, typeParams: typeParams);
}
```

### 2c. `_parseProcArgType()` — parse parameterized type references

After consuming a type name (VARIABLE or READER), check for `(` and parse type arguments:

```dart
TypeExpr _parseProcArgType() {
  final line = _peek().line;
  final column = _peek().column;

  // Primitive: _ or _?
  if (_match(TokenType.UNDERSCORE)) {
    final isInput = _match(TokenType.QUESTION);
    return PrimitiveModeAlt(isInput, line, column);
  }

  // Qualified type reference: atom # TypeName (unchanged)
  if (_check(TokenType.ATOM) && _current + 1 < tokens.length && tokens[_current + 1].type == TokenType.HASH) {
    // ... existing qualified type parsing, unchanged ...
  }

  // Type reference with optional mode and optional type arguments
  if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
    final token = _advance();
    final baseName = token.lexeme;

    // NEW: Parse optional type arguments: (Type1, Type2, ...)
    final typeArgs = <TypeExpr>[];
    if (_match(TokenType.LPAREN)) {
      typeArgs.add(_parseProcArgType());  // recursive — supports nested parameterized types
      while (_match(TokenType.COMMA)) {
        typeArgs.add(_parseProcArgType());
      }
      _consume(TokenType.RPAREN, 'Expected ")" after type arguments');
    }

    final isInput = token.type == TokenType.READER || _match(TokenType.QUESTION);
    return TypeRef(baseName, line, column, isInput: isInput, typeArgs: typeArgs);
  }

  throw CompileError(
    'Expected type in procedure argument',
    _peek().line,
    _peek().column,
    phase: 'parser',
  );
}
```

### 2d. Type alternative parsing — parameterized type references in type bodies

In type definition bodies, `Stream(X)` currently parses as `StructTerm('Stream', [VarTerm('X')])` which converts via `termToTypeExpr` to `StructAlt('Stream', [TypeRef('X')])`.

We need `Stream(X)` in a type body to become `TypeRef('Stream', typeArgs: [TypeRef('X')])` instead.

**The fix is in `_parseTypeAltPrimary()`**: When an uppercase-initial token (VARIABLE) is followed by `(`, it's a parameterized type reference, not a struct alternative. Struct functors in type definitions are always lowercase atoms.

In `_parseTypeAltPrimary()`, add handling before the existing ATOM structure parsing:

```dart
// In _parseTypeAltPrimary(), add BEFORE the existing VARIABLE/READER handling:

// Parameterized type reference in type body: TypeName(Arg1, Arg2, ...)
// Uppercase names followed by ( are parameterized type refs, not structs
if ((_check(TokenType.VARIABLE) || _check(TokenType.READER)) &&
    _current + 1 < tokens.length && tokens[_current + 1].type == TokenType.LPAREN) {
  final token = _advance();  // consume type name
  final isReader = token.type == TokenType.READER;
  _advance();  // consume (
  final typeArgs = <Term>[];
  if (!_check(TokenType.RPAREN)) {
    typeArgs.add(_parseTypeAltExpression());
    while (_match(TokenType.COMMA)) {
      typeArgs.add(_parseTypeAltExpression());
    }
  }
  _consume(TokenType.RPAREN, 'Expected ")" after type arguments');
  // Allow trailing ? 
  final trailingQuestion = _match(TokenType.QUESTION);
  final finalIsReader = isReader || trailingQuestion;
  // Return as VarTerm with special encoding — or we handle in type_conversion
  // Actually: return a StructTerm with uppercase functor. type_conversion will
  // distinguish uppercase (parameterized type ref) from lowercase (struct alt).
  return StructTerm(token.lexeme, typeArgs, token.line, token.column);
  // The isReader/isInput handling needs to be captured. See type_conversion changes below.
}
```

Wait — this approach has a problem: the reader/input mode is lost. Better approach: leave the parser as-is for type body parsing (uppercase `(...)` still becomes `StructTerm`), and handle the distinction in `type_conversion.dart`.

### 2e. Type conversion — distinguish parameterized type refs from struct alts

**File**: `glp_runtime/lib/analysis/type_checker/type_conversion.dart`

In `termToTypeExpr()`, when converting a `StructTerm`, check if the functor is uppercase-initial (a type name) vs lowercase (a struct functor):

```dart
if (term is StructTerm) {
  // Difference list: A \ B
  if (term.functor == '\\' && term.args.length == 2) {
    return DiffListAlt(
      termToTypeExpr(term.args[0]),
      termToTypeExpr(term.args[1]),
      term.line,
      term.column,
    );
  }

  // NEW: Parameterized type reference — uppercase-initial functor
  // e.g., Stream(X), Channel(In, Out), Pair(Integer, String)
  if (term.functor.isNotEmpty && term.functor[0].toUpperCase() == term.functor[0] &&
      term.functor[0] != '_') {
    return TypeRef(
      term.functor,
      term.line,
      term.column,
      typeArgs: term.args.map(termToTypeExpr).toList(),
    );
  }

  // Regular structure (lowercase functor)
  return StructAlt(
    term.functor,
    term.args.map(termToTypeExpr).toList(),
    term.line,
    term.column,
  );
}
```

**But wait**: What about `Stream(X)?` in a type body? The trailing `?` is consumed by `_parseTypeAltPrimary` (which already does `_match(TokenType.QUESTION)` on structs). But the `?` is lost because `StructTerm` has no `isReader` field. 

For type bodies, trailing `?` on parameterized refs like `Stream(X)?` needs special handling. The current parser already consumes trailing `?` on struct terms in type alt context but discards it. We need to preserve it.

**Solution**: In `_parseTypeAltPrimary()`, for uppercase VARIABLE followed by `(`, parse the arguments, check for trailing `?`, and return a `VarTerm` with the `isReader` flag set, plus store the args somehow. The cleanest approach: create a small wrapper.

Actually, the simplest solution: handle it in `_parseTypeAltPrimary()` by returning a VarTerm for bare type references (no args) and a special encoded StructTerm for parameterized ones, then fix `termToTypeExpr` to handle both. The trailing `?` on a `StructTerm` with uppercase functor can be handled by wrapping the check in `_parseTypeAltPrimary()`.

Let me reconsider. The cleanest approach:

**In `_parseTypeAltPrimary()`**: When we see `VARIABLE(` or `READER(`, parse it as before (becoming `StructTerm`). The `READER` case already captures the `?`. For `VARIABLE` followed by args then `?`, the existing `_match(TokenType.QUESTION)` after struct parsing consumes it, but we lose it.

**Revised approach**: Add a new AST node `ParamTypeRefTerm` that extends `Term`:

No — keep it simple. Use the existing `StructTerm` but in `_parseTypeAltPrimary`, when we detect uppercase+`(`, mark the reader mode by prefixing the functor name. Too hacky.

**Cleanest approach**: Handle `VARIABLE(...)` and `VARIABLE(...)?` explicitly in `_parseTypeAltPrimary()` before falling through to the generic VARIABLE case. Return a VarTerm-like construct. Since `termToTypeExpr` needs to produce a `TypeRef` with `typeArgs` and `isInput`, just do the conversion right there in the parser:

```dart
// In _parseTypeAltPrimary(), BEFORE the existing VARIABLE/READER handling:
if ((_check(TokenType.VARIABLE) || _check(TokenType.READER)) &&
    _current + 1 < tokens.length && tokens[_current + 1].type == TokenType.LPAREN) {
  final token = _advance();
  final isReader = token.type == TokenType.READER;
  _advance(); // consume (
  final args = <Term>[];
  if (!_check(TokenType.RPAREN)) {
    args.add(_parseTypeAltExpression());
    while (_match(TokenType.COMMA)) {
      args.add(_parseTypeAltExpression());
    }
  }
  _consume(TokenType.RPAREN, 'Expected ")" after type arguments');
  final trailingQ = _match(TokenType.QUESTION);
  // Encode as StructTerm with a marker — functor prefixed with '?:' if input
  // OR: just use StructTerm and let type_conversion handle it via uppercase check
  // The isInput = isReader || trailingQ. We'll encode it in the functor:
  final effectiveName = (isReader || trailingQ) ? '${token.lexeme}?' : token.lexeme;
  return StructTerm(effectiveName, args, token.line, token.column);
}
```

Then in `termToTypeExpr`, the uppercase check becomes:

```dart
if (term is StructTerm) {
  var functor = term.functor;
  // ... difference list check ...

  // Check for parameterized type reference (uppercase initial, possibly with trailing ?)
  bool isInput = false;
  if (functor.endsWith('?')) {
    functor = functor.substring(0, functor.length - 1);
    isInput = true;
  }
  if (functor.isNotEmpty && functor[0].toUpperCase() == functor[0] && functor[0] != '_') {
    return TypeRef(functor, term.line, term.column,
        isInput: isInput,
        typeArgs: term.args.map(termToTypeExpr).toList());
  }

  // Regular struct alt
  return StructAlt(term.functor, term.args.map(termToTypeExpr).toList(), term.line, term.column);
}
```

This works. The key insight: in type definition bodies, an uppercase functor with arguments is always a parameterized type reference, never a struct alternative (struct functors are lowercase atoms).

---

## Part 3: Expansion Preprocessing

**New file**: `glp_runtime/lib/analysis/type_checker/param_expansion.dart`

This runs after parsing and before type automaton construction.

### Entry point

```dart
/// Expand all parameterized types in a module to monomorphic equivalents.
/// Returns a new Module with only monomorphic type definitions and
/// procedure declarations. The original Module is not modified.
Module expandParameterizedTypes(Module module) { ... }
```

### Algorithm

```dart
Module expandParameterizedTypes(Module module) {
  // Step 1: Separate templates from monomorphic types
  final templates = <String, TypeDef>{};  // name -> parameterized TypeDef
  final monoTypeDefs = <TypeDef>[];       // monomorphic type defs to keep

  for (final td in module.typeDefs) {
    if (td.isParameterized) {
      templates[td.name] = td;
    } else {
      monoTypeDefs.add(td);
    }
  }

  if (templates.isEmpty) return module;  // nothing to expand

  // Step 2: Collect all instantiations from type defs and proc decls
  final instantiations = <String, List<TypeExpr>>{};  // expanded name -> type args
  // Scan monomorphic type def bodies
  for (final td in monoTypeDefs) {
    for (final alt in td.alternatives) {
      _collectInstantiations(alt, templates, instantiations);
    }
  }
  // Scan procedure declarations
  for (final pd in module.procDeclarations) {
    for (final arg in pd.argTypes) {
      _collectInstantiations(arg, templates, instantiations);
    }
  }
  // Also scan template bodies for cross-references (collected during expansion)

  // Step 3: Expand each instantiation
  final expandedDefs = <TypeDef>[];
  final expanded = <String>{};  // track which have been expanded
  // Use worklist — expansion may discover new instantiations
  while (instantiations.length > expanded.length) {
    for (final entry in Map.of(instantiations).entries) {
      if (expanded.contains(entry.key)) continue;
      final expandedName = entry.key;
      final typeArgs = entry.value;
      // Find the template name from the expanded name
      final templateName = _templateNameFromExpanded(expandedName);
      final template = templates[templateName]!;
      // Substitute parameters
      final substitution = Map.fromIterables(template.typeParams, typeArgs);
      final newAlts = template.alternatives
          .map((alt) => _substituteTypeExpr(alt, substitution, templates, instantiations))
          .toList();
      expandedDefs.add(TypeDef(expandedName, newAlts, template.line, template.column));
      expanded.add(expandedName);
    }
  }

  // Step 4: Replace references in monomorphic type defs
  final replacedTypeDefs = monoTypeDefs.map((td) {
    final newAlts = td.alternatives
        .map((alt) => _replaceParamRefs(alt, templates))
        .toList();
    return TypeDef(td.name, newAlts, td.line, td.column);
  }).toList();

  // Step 5: Replace references in procedure declarations
  final replacedProcDecls = module.procDeclarations.map((pd) {
    final newArgTypes = pd.argTypes
        .map((arg) => _replaceParamRefs(arg, templates))
        .toList();
    return ProcDecl(pd.name, newArgTypes, pd.line, pd.column,
        exported: pd.exported, imported: pd.imported, modulePath: pd.modulePath);
  }).toList();

  return Module(
    declaration: module.declaration,
    typeDefs: [...replacedTypeDefs, ...expandedDefs],
    procDeclarations: replacedProcDecls,
    procedures: module.procedures,
    compileMode: module.compileMode,
    line: module.line,
    column: module.column,
  );
}
```

### Helper functions

```dart
/// Generate expanded name: Stream(Integer) -> "Stream<Integer>"
String _expandedName(String templateName, List<TypeExpr> typeArgs) {
  return '$templateName<${typeArgs.join(',')}>';
}

/// Collect parameterized type references from a TypeExpr
void _collectInstantiations(TypeExpr expr, Map<String, TypeDef> templates,
    Map<String, List<TypeExpr>> instantiations) {
  if (expr is TypeRef && expr.typeArgs.isNotEmpty && templates.containsKey(expr.name)) {
    final name = _expandedName(expr.name, expr.typeArgs);
    instantiations.putIfAbsent(name, () => expr.typeArgs);
    // Recurse into type args (for nested parameterized types)
    for (final arg in expr.typeArgs) {
      _collectInstantiations(arg, templates, instantiations);
    }
  }
  // Recurse into sub-expressions
  if (expr is StructAlt) {
    for (final arg in expr.args) {
      _collectInstantiations(arg, templates, instantiations);
    }
  }
  if (expr is ListConsAlt) {
    _collectInstantiations(expr.head, templates, instantiations);
    _collectInstantiations(expr.tail, templates, instantiations);
  }
  if (expr is DiffListAlt) {
    _collectInstantiations(expr.content, templates, instantiations);
    _collectInstantiations(expr.hole, templates, instantiations);
  }
}

/// Substitute type parameters in a TypeExpr
TypeExpr _substituteTypeExpr(TypeExpr expr, Map<String, TypeExpr> substitution,
    Map<String, TypeDef> templates, Map<String, List<TypeExpr>> instantiations) {
  if (expr is TypeRef) {
    // If this is a type parameter, substitute it
    if (substitution.containsKey(expr.name) && expr.typeArgs.isEmpty) {
      final replacement = substitution[expr.name]!;
      // Apply isInput from the original reference
      if (expr.isInput && replacement is TypeRef) {
        return TypeRef(replacement.name, replacement.line, replacement.column,
            isInput: true, typeArgs: replacement.typeArgs);
      }
      if (expr.isInput && replacement is PrimitiveModeAlt) {
        return PrimitiveModeAlt(true, replacement.line, replacement.column);
      }
      return replacement;
    }
    // If this is a parameterized reference to a template, record and replace
    if (expr.typeArgs.isNotEmpty && templates.containsKey(expr.name)) {
      final substArgs = expr.typeArgs
          .map((a) => _substituteTypeExpr(a, substitution, templates, instantiations))
          .toList();
      final expandedName = _expandedName(expr.name, substArgs);
      instantiations.putIfAbsent(expandedName, () => substArgs);
      return TypeRef(expandedName, expr.line, expr.column, isInput: expr.isInput);
    }
    return expr;
  }
  if (expr is StructAlt) {
    return StructAlt(expr.functor,
        expr.args.map((a) => _substituteTypeExpr(a, substitution, templates, instantiations)).toList(),
        expr.line, expr.column);
  }
  if (expr is ListConsAlt) {
    return ListConsAlt(
        _substituteTypeExpr(expr.head, substitution, templates, instantiations),
        _substituteTypeExpr(expr.tail, substitution, templates, instantiations),
        expr.line, expr.column);
  }
  if (expr is DiffListAlt) {
    return DiffListAlt(
        _substituteTypeExpr(expr.content, substitution, templates, instantiations),
        _substituteTypeExpr(expr.hole, substitution, templates, instantiations),
        expr.line, expr.column);
  }
  // PrimitiveModeAlt, ConstantAlt, ListNilAlt — no substitution needed
  return expr;
}

/// Replace parameterized type refs with expanded names (for non-template types)
TypeExpr _replaceParamRefs(TypeExpr expr, Map<String, TypeDef> templates) {
  if (expr is TypeRef && expr.typeArgs.isNotEmpty && templates.containsKey(expr.name)) {
    final expandedName = _expandedName(expr.name, expr.typeArgs);
    return TypeRef(expandedName, expr.line, expr.column, isInput: expr.isInput);
  }
  if (expr is StructAlt) {
    return StructAlt(expr.functor,
        expr.args.map((a) => _replaceParamRefs(a, templates)).toList(),
        expr.line, expr.column);
  }
  if (expr is ListConsAlt) {
    return ListConsAlt(
        _replaceParamRefs(expr.head, templates),
        _replaceParamRefs(expr.tail, templates),
        expr.line, expr.column);
  }
  if (expr is DiffListAlt) {
    return DiffListAlt(
        _replaceParamRefs(expr.content, templates),
        _replaceParamRefs(expr.hole, templates),
        expr.line, expr.column);
  }
  return expr;
}
```

### Integration point

The expansion must be called after parsing and before type automaton construction. Find where the `Module` is used to build the type environment / DFA and insert the call:

```dart
final module = parser.parseModule();
final expandedModule = expandParameterizedTypes(module);
// ... use expandedModule for type checking ...
```

The likely integration point is in `analyzer.dart` or wherever the compilation pipeline calls `parseModule()` and then builds the type environment.

---

## Part 4: Integration into Compilation Pipeline

**File**: `glp_runtime/lib/compiler/analyzer.dart` (or wherever the pipeline is)

Read this file to find where `parseModule()` result is used. Insert `expandParameterizedTypes()` call between parsing and type environment construction.

---

## Part 5: Tests

Add test programs to `programs/tests/typed/`:

### Positive tests (should pass type checking)

**`param_stream_integer.glp`** — basic expansion:
```
Stream(X) ::= [] ; [X | Stream(X)].

procedure merge(Stream(Integer)?, Stream(Integer)?, Stream(Integer)).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge(Xs, [], Xs?).
merge([], Ys, Ys?).
```

**`param_channel.glp`** — mode annotations preserved:
```
Stream(X) ::= [] ; [X | Stream(X)].
Channel(In, Out) ::= ch(In, Out?).
Msg ::= hello ; goodbye.

procedure new_channel(Channel(Stream(Msg), Stream(Msg)), Channel(Stream(Msg), Stream(Msg))).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
```

**`param_procedure_inference.glp`** — parameterized procedure declaration:
```
Stream(X) ::= [] ; [X | Stream(X)].

procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge(Xs, [], Xs?).
merge([], Ys, Ys?).

CounterCall ::= inc ; dec ; read(Integer?).

procedure monitor(Integer?, Stream(CounterCall)?).
monitor(N, [add|In]) :- N1 := N? + 1, monitor(N1?, In?).
monitor(N, [clear|In]) :- monitor(0, In?).
monitor(N, [read(N?)|In]) :- monitor(N?, In?).
```

### Negative tests (should fail)

**`param_arity_mismatch.glp`** — wrong number of type args:
```
Stream(X) ::= [] ; [X | Stream(X)].
procedure bad(Stream(Integer, String)?).
bad([]).
```

### Add to test suite

Add the positive files to the `POSITIVE_FILES` array in Section B of `test/run_all_tests.sh`.
Add the negative file to the `NEGATIVE_FILES` array in Section C.

---

## Execution Order

1. Run baseline tests: `bash test/run_all_tests.sh` — commit baseline
2. Implement Part 1 (type_ast.dart changes) — ensure it compiles, run tests (should pass unchanged)
3. Implement Part 2 (parser changes) — ensure it compiles, run tests
4. Implement Part 3 (param_expansion.dart) — new file
5. Implement Part 4 (integration) — wire expansion into pipeline
6. Add test programs (Part 5) — run tests
7. Run full test suite — all existing tests must still pass
8. Commit and push

## Important Notes

- **Do not modify** any file not listed above. The downstream type checking machinery is unchanged.
- The `self.glp` file is NOT modified in Stage 1. Parameterized types are added alongside existing monomorphic types.
- If a program has no parameterized types, `expandParameterizedTypes` returns the module unchanged — zero overhead for existing programs.
- Type parameter names (X, Y, etc.) in type definition bodies look like type references. They are distinguished during expansion: a name that matches a template's parameter list is a parameter, not a type reference.
