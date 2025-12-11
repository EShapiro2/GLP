# PMT Implementation Plan

**Status:** Draft
**Depends on:** Module system
**Date:** 2025-12-11

## Overview

Implementation of Polymorphic Moded Types (PMT) for GLP, providing static SRSW verification.

## Prerequisites

- Module system implemented (declarations live in module headers)
- Parser handles `:=` for definitions

## File Structure

```
lib/
  compiler/
    pmt/
      mode_table.dart      # Stores pred/arity → modes
      occurrence.dart      # Classifies variable occurrences
      checker.dart         # SRSW verification
      errors.dart          # PMT-specific errors
      deriver.dart         # Annotation derivation (optional)

test/
  pmt/
    mode_table_test.dart
    occurrence_test.dart
    checker_test.dart
    programs/              # Test GLP programs
      valid/
      invalid/
```

## Phase 1: Parser Extension

### New Token
None needed — uses existing `ATOM`, `LPAREN`, `RPAREN`, `QUESTION`, `ASSIGN`, `DOT`.

### New AST Node

```dart
class ModeDeclaration extends AstNode {
  final String typeName;           // e.g., "Merge"
  final List<String> typeParams;   // e.g., ["A"]
  final String predicate;          // e.g., "merge"
  final List<ModedArg> args;       // e.g., [reader, reader, writer]

  ModeDeclaration(this.typeName, this.typeParams, this.predicate,
                  this.args, int line, int column) : super(line, column);

  int get arity => args.length;
  String get signature => '$predicate/$arity';
}

class ModedArg {
  final String typeName;           // e.g., "List" or "Num"
  final List<String> typeParams;   // e.g., ["A"]
  final bool isReader;             // true if T?, false if T

  ModedArg(this.typeName, this.typeParams, this.isReader);
}
```

### Parser Changes

In `_parseProcedure()` or new `_parseDeclaration()`:

```dart
// Detect: CapitalizedName(...) := pred(...).
if (_check(TokenType.ATOM) && _isCapitalized(_peek().lexeme)) {
  return _parseModeDeclaration();
}
```

```dart
ModeDeclaration _parseModeDeclaration() {
  final typeNameToken = _advance();
  final typeName = typeNameToken.lexeme;

  // Optional type parameters
  final typeParams = <String>[];
  if (_match(TokenType.LPAREN)) {
    // Parse type params (capitalized identifiers)
    typeParams.add(_consume(TokenType.ATOM, 'Expected type parameter').lexeme);
    while (_match(TokenType.COMMA)) {
      typeParams.add(_consume(TokenType.ATOM, 'Expected type parameter').lexeme);
    }
    _consume(TokenType.RPAREN, 'Expected )');
  }

  _consume(TokenType.ASSIGN, 'Expected :=');

  // Predicate name
  final predToken = _consume(TokenType.ATOM, 'Expected predicate name');
  final predicate = predToken.lexeme;

  // Arguments with modes
  final args = <ModedArg>[];
  _consume(TokenType.LPAREN, 'Expected (');
  if (!_check(TokenType.RPAREN)) {
    args.add(_parseModedArg());
    while (_match(TokenType.COMMA)) {
      args.add(_parseModedArg());
    }
  }
  _consume(TokenType.RPAREN, 'Expected )');
  _consume(TokenType.DOT, 'Expected .');

  return ModeDeclaration(typeName, typeParams, predicate, args,
                         typeNameToken.line, typeNameToken.column);
}

ModedArg _parseModedArg() {
  final typeToken = _consume(TokenType.ATOM, 'Expected type name');
  final typeName = typeToken.lexeme;

  // Optional type parameters
  final typeParams = <String>[];
  if (_match(TokenType.LPAREN)) {
    typeParams.add(_consume(TokenType.ATOM, 'Expected type parameter').lexeme);
    while (_match(TokenType.COMMA)) {
      typeParams.add(_consume(TokenType.ATOM, 'Expected type parameter').lexeme);
    }
    _consume(TokenType.RPAREN, 'Expected )');
  }

  // Check for reader marker
  final isReader = _match(TokenType.QUESTION);

  return ModedArg(typeName, typeParams, isReader);
}
```

## Phase 2: Mode Table

```dart
// lib/compiler/pmt/mode_table.dart

enum Mode { reader, writer }

class ModeTable {
  final Map<String, List<Mode>> _modes = {};  // "pred/arity" → modes

  void addDeclaration(ModeDeclaration decl) {
    final key = decl.signature;
    if (_modes.containsKey(key)) {
      throw PmtError('Duplicate mode declaration for $key',
                     decl.line, decl.column);
    }
    _modes[key] = decl.args.map((a) => a.isReader ? Mode.reader : Mode.writer).toList();
  }

  List<Mode>? getModes(String predicate, int arity) {
    return _modes['$predicate/$arity'];
  }

  bool hasDeclaration(String predicate, int arity) {
    return _modes.containsKey('$predicate/$arity');
  }
}
```

## Phase 3: Occurrence Classifier

```dart
// lib/compiler/pmt/occurrence.dart

enum OccurrenceType { writer, reader }

class Occurrence {
  final String variable;
  final OccurrenceType type;
  final int line;
  final int column;

  Occurrence(this.variable, this.type, this.line, this.column);
}

class OccurrenceClassifier {
  final ModeTable modeTable;

  OccurrenceClassifier(this.modeTable);

  /// Classify all variable occurrences in a clause
  List<Occurrence> classifyClause(Clause clause, List<Mode> headModes) {
    final occurrences = <Occurrence>[];

    // Head arguments
    for (int i = 0; i < clause.head.args.length; i++) {
      final mode = headModes[i];
      _classifyTerm(clause.head.args[i], mode, true, occurrences);
    }

    // Body goals
    if (clause.body != null) {
      for (final goal in clause.body!) {
        final goalModes = modeTable.getModes(goal.functor, goal.arity);
        if (goalModes == null) {
          // No declaration — skip or error depending on strictness
          continue;
        }
        for (int i = 0; i < goal.args.length; i++) {
          _classifyTerm(goal.args[i], goalModes[i], false, occurrences);
        }
      }
    }

    return occurrences;
  }

  void _classifyTerm(Term term, Mode argMode, bool isHead, List<Occurrence> out) {
    // Determine occurrence type:
    // Head + reader arg → writer occurrence
    // Head + writer arg → reader occurrence
    // Body + reader arg → reader occurrence
    // Body + writer arg → writer occurrence

    OccurrenceType occType;
    if (isHead) {
      occType = (argMode == Mode.reader) ? OccurrenceType.writer : OccurrenceType.reader;
    } else {
      occType = (argMode == Mode.reader) ? OccurrenceType.reader : OccurrenceType.writer;
    }

    _collectVariables(term, occType, out);
  }

  void _collectVariables(Term term, OccurrenceType type, List<Occurrence> out) {
    if (term is VarTerm) {
      out.add(Occurrence(term.name, type, term.line, term.column));
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _collectVariables(arg, type, out);
      }
    } else if (term is ListTerm) {
      if (term.head != null) _collectVariables(term.head!, type, out);
      if (term.tail != null) _collectVariables(term.tail!, type, out);
    }
    // ConstTerm, UnderscoreTerm — no variables
  }
}
```

## Phase 4: SRSW Checker

```dart
// lib/compiler/pmt/checker.dart

class PmtChecker {
  final ModeTable modeTable;
  final OccurrenceClassifier classifier;

  PmtChecker(this.modeTable) : classifier = OccurrenceClassifier(modeTable);

  /// Check all clauses in a procedure
  List<PmtError> checkProcedure(Procedure proc) {
    final errors = <PmtError>[];

    final modes = modeTable.getModes(proc.name, proc.arity);
    if (modes == null) {
      // No mode declaration — skip checking
      return errors;
    }

    for (final clause in proc.clauses) {
      errors.addAll(checkClause(clause, modes));
    }

    return errors;
  }

  /// Check a single clause
  List<PmtError> checkClause(Clause clause, List<Mode> modes) {
    final errors = <PmtError>[];
    final occurrences = classifier.classifyClause(clause, modes);

    // Group by variable name
    final byVar = <String, List<Occurrence>>{};
    for (final occ in occurrences) {
      byVar.putIfAbsent(occ.variable, () => []).add(occ);
    }

    // Check for ground guards
    final groundedVars = _extractGroundedVars(clause);

    // Verify SRSW for each variable
    for (final entry in byVar.entries) {
      final varName = entry.key;
      final occs = entry.value;

      final writers = occs.where((o) => o.type == OccurrenceType.writer).toList();
      final readers = occs.where((o) => o.type == OccurrenceType.reader).toList();

      if (writers.isEmpty) {
        errors.add(PmtError(
          'Variable $varName has no writer occurrence',
          occs.first.line, occs.first.column
        ));
      } else if (writers.length > 1) {
        errors.add(PmtError(
          'Variable $varName has ${writers.length} writer occurrences (expected 1)',
          writers[1].line, writers[1].column
        ));
      }

      if (readers.isEmpty) {
        errors.add(PmtError(
          'Variable $varName has no reader occurrence',
          occs.first.line, occs.first.column
        ));
      } else if (readers.length > 1 && !groundedVars.contains(varName)) {
        errors.add(PmtError(
          'Variable $varName has ${readers.length} reader occurrences; add ground($varName) guard',
          readers[1].line, readers[1].column
        ));
      }
    }

    return errors;
  }

  Set<String> _extractGroundedVars(Clause clause) {
    final grounded = <String>{};
    if (clause.guards != null) {
      for (final guard in clause.guards!) {
        if (guard.predicate == 'ground' && guard.args.length == 1) {
          final arg = guard.args[0];
          if (arg is VarTerm) {
            grounded.add(arg.name);
          }
        }
      }
    }
    return grounded;
  }
}
```

## Phase 5: Integration

```dart
// In compiler pipeline, after parsing:

class GlpCompiler {
  BytecodeProgram compile(String source, {bool checkPmt = true}) {
    final tokens = lexer.tokenize(source);
    final ast = parser.parse(tokens);

    if (checkPmt) {
      final modeTable = ModeTable();

      // Collect declarations
      for (final decl in ast.declarations) {
        modeTable.addDeclaration(decl);
      }

      // Check procedures
      final checker = PmtChecker(modeTable);
      final errors = <PmtError>[];
      for (final proc in ast.procedures) {
        errors.addAll(checker.checkProcedure(proc));
      }

      if (errors.isNotEmpty) {
        throw PmtErrors(errors);
      }
    }

    // Continue with code generation...
  }
}
```

## Test Cases

### Valid Programs

```glp
% test/pmt/programs/valid/merge.glp
Merge(A) := merge(List(A)?, List(A)?, List(A)).

merge([], Ys, Ys).
merge([X|Xs], Ys, [X|Zs]) :- merge(Ys, Xs, Zs).
```

```glp
% test/pmt/programs/valid/half_adder.glp
HalfAdder := half_adder(Num?, Num?, Num, Num).
Xor := xor(Num?, Num?, Num).
And := and(Num?, Num?, Num).

half_adder(A, B, Sum, Carry) :-
    ground(A), ground(B) |
    xor(A, B, Sum), and(A, B, Carry).
```

### Invalid Programs

```glp
% test/pmt/programs/invalid/two_writers.glp
Eq := eq(Num?, Num?).

eq(X, X).  % ERROR: X has 2 writers, 0 readers
```

```glp
% test/pmt/programs/invalid/two_readers.glp
Dup := dup(Num?, Num, Num).

dup(X, X, X).  % ERROR: X has 1 writer, 2 readers (no ground guard)
```

```glp
% test/pmt/programs/invalid/orphan.glp
Bad := bad(Num?, Num).

bad(X, Y).  % ERROR: X has no reader, Y has no writer
```

## Milestones

- [ ] Phase 1: Parser extension (AST nodes, parsing)
- [ ] Phase 2: Mode table (storage, lookup)
- [ ] Phase 3: Occurrence classifier
- [ ] Phase 4: SRSW checker
- [ ] Phase 5: Integration with compiler
- [ ] Phase 6: Test suite
- [ ] Phase 7: Error message polish
- [ ] Phase 8: Annotation deriver (optional)
