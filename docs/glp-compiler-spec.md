# GLP Compiler Specification v2.16
## Complete Design Document for Dart Implementation

**Target**: Compile GLP source code to bytecode v2.16 compatible with the GLP Dart runtime

**Date**: 2025-11-06

**Status**: Complete design specification

---

## Table of Contents

1. [Overview](#1-overview)
2. [Architecture](#2-architecture)
3. [Lexical Analysis](#3-lexical-analysis)
4. [Syntax Analysis (Parser)](#4-syntax-analysis-parser)
5. [Semantic Analysis (Analyzer)](#5-semantic-analysis-analyzer)
6. [Code Generation](#6-code-generation)
7. [Optimization Passes](#7-optimization-passes)
8. [Error Handling](#8-error-handling)
9. [Implementation Notes](#9-implementation-notes)

---

## 1. Overview

### 1.1 Compilation Pipeline

```
Source Code (.glp)
    ↓
[Lexer] → Tokens
    ↓
[Parser] → AST (Abstract Syntax Tree)
    ↓
[Analyzer] → Annotated AST (with variable tables, SRSW verification)
    ↓
[Code Generator] → Bytecode Program (v2.16)
    ↓
[Optimizer] → Optimized Bytecode (optional)
    ↓
BytecodeProgram (ready for runtime)
```

### 1.2 Design Principles

1. **Correctness First**: Strict adherence to GLP semantics and bytecode v2.16 spec
2. **SRSW Enforcement**: Compile-time verification of Single-Reader/Single-Writer constraint
3. **Clear Error Messages**: Provide source location and helpful diagnostics
4. **Modularity**: Clean separation between compilation phases
5. **Maintainability**: Well-documented, testable components

---

## 2. Architecture

### 2.1 Main Compiler Interface

```dart
class GlpCompiler {
  final Lexer lexer;
  final Parser parser;
  final Analyzer analyzer;
  final CodeGenerator codegen;
  final Optimizer optimizer;

  /// Compile GLP source to bytecode program
  BytecodeProgram compile(String source, {bool optimize = true}) {
    try {
      // Phase 1: Lexical analysis
      final tokens = lexer.tokenize(source);

      // Phase 2: Syntax analysis
      final ast = parser.parse(tokens);

      // Phase 3: Semantic analysis
      final annotatedAst = analyzer.analyze(ast);

      // Phase 4: Code generation
      final bytecode = codegen.generate(annotatedAst);

      // Phase 5: Optimization (optional)
      if (optimize) {
        return optimizer.optimize(bytecode);
      }

      return bytecode;
    } on CompileError catch (e) {
      // Rethrow with context
      throw CompileError(e.message, e.line, e.column, source: source);
    }
  }
}
```

### 2.2 Error Handling

```dart
class CompileError implements Exception {
  final String message;
  final int line;
  final int column;
  final String? source;
  final String? phase;  // "lexer", "parser", "analyzer", "codegen"

  CompileError(this.message, this.line, this.column, {this.source, this.phase});

  @override
  String toString() {
    final loc = 'Line $line, Column $column';
    final phaseInfo = phase != null ? '[$phase] ' : '';

    if (source != null) {
      final lines = source.split('\n');
      if (line > 0 && line <= lines.length) {
        final sourceLine = lines[line - 1];
        final pointer = ' ' * (column - 1) + '^';
        return '$phaseInfo$message\n$loc:\n$sourceLine\n$pointer';
      }
    }

    return '$phaseInfo$message at $loc';
  }
}
```

---

## 3. Lexical Analysis

### 3.1 Token Types

```dart
enum TokenType {
  // Identifiers and literals
  ATOM,           // lowercase: p, merge, foo_bar
  VARIABLE,       // uppercase: X, Ys, Result
  READER,         // variable with ?: X?, Ys?
  NUMBER,         // 42, 3.14, -17
  STRING,         // "hello", 'world'

  // Delimiters
  LPAREN,         // (
  RPAREN,         // )
  LBRACKET,       // [
  RBRACKET,       // ]
  LBRACE,         // { (future: sets)
  RBRACE,         // } (future: sets)

  // Punctuation
  DOT,            // .
  COMMA,          // ,
  PIPE,           // |
  QUESTION,       // ? (for readers)

  // Operators
  IMPLIES,        // :-
  GUARD_SEP,      // | (in clause: Head :- Guards | Body)

  // Special
  UNDERSCORE,     // _ (anonymous variable)

  // End of file
  EOF
}

class Token {
  final TokenType type;
  final String lexeme;
  final int line;
  final int column;
  final Object? literal;  // For NUMBER, STRING

  Token(this.type, this.lexeme, this.line, this.column, [this.literal]);

  @override
  String toString() => '$type($lexeme) at $line:$column';
}
```

### 3.2 Lexer Implementation

```dart
class Lexer {
  final String source;
  int _current = 0;
  int _line = 1;
  int _column = 1;

  Lexer(this.source);

  List<Token> tokenize() {
    final tokens = <Token>[];

    while (!_isAtEnd()) {
      _skipWhitespaceAndComments();
      if (_isAtEnd()) break;

      final token = _scanToken();
      tokens.add(token);
    }

    tokens.add(Token(TokenType.EOF, '', _line, _column));
    return tokens;
  }

  Token _scanToken() {
    final start = _current;
    final startLine = _line;
    final startColumn = _column;
    final c = _advance();

    switch (c) {
      case '(': return _makeToken(TokenType.LPAREN, startLine, startColumn);
      case ')': return _makeToken(TokenType.RPAREN, startLine, startColumn);
      case '[': return _makeToken(TokenType.LBRACKET, startLine, startColumn);
      case ']': return _makeToken(TokenType.RBRACKET, startLine, startColumn);
      case '{': return _makeToken(TokenType.LBRACE, startLine, startColumn);
      case '}': return _makeToken(TokenType.RBRACE, startLine, startColumn);
      case '.': return _makeToken(TokenType.DOT, startLine, startColumn);
      case ',': return _makeToken(TokenType.COMMA, startLine, startColumn);
      case '?': return _makeToken(TokenType.QUESTION, startLine, startColumn);
      case '|': return _makeToken(TokenType.PIPE, startLine, startColumn);

      case ':':
        if (_match('-')) {
          return _makeToken(TokenType.IMPLIES, startLine, startColumn);
        }
        throw CompileError('Unexpected character :', startLine, startColumn, phase: 'lexer');

      case '_':
        if (!_isAlphaNumeric(_peek())) {
          return _makeToken(TokenType.UNDERSCORE, startLine, startColumn);
        }
        // Otherwise fall through to identifier
        return _identifier(start, startLine, startColumn);

      case '"':
      case "'":
        return _string(c, startLine, startColumn);

      default:
        if (_isDigit(c) || (c == '-' && _isDigit(_peek()))) {
          return _number(start, startLine, startColumn);
        }
        if (_isAlpha(c)) {
          return _identifier(start, startLine, startColumn);
        }
        throw CompileError('Unexpected character: $c', startLine, startColumn, phase: 'lexer');
    }
  }

  Token _identifier(int start, int line, int column) {
    while (_isAlphaNumeric(_peek())) {
      _advance();
    }

    final text = source.substring(start, _current);

    // Check for reader syntax (Variable?)
    if (_peek() == '?' && _isUpper(text[0])) {
      _advance(); // consume '?'
      return Token(TokenType.READER, text, line, column);
    }

    // Variable (uppercase) or Atom (lowercase)
    final type = _isUpper(text[0]) ? TokenType.VARIABLE : TokenType.ATOM;
    return Token(type, text, line, column);
  }

  Token _number(int start, int line, int column) {
    // Handle negative sign
    if (source[start] == '-') {
      _current = start + 1;
    }

    while (_isDigit(_peek())) {
      _advance();
    }

    // Handle decimal point
    if (_peek() == '.' && _isDigit(_peekNext())) {
      _advance(); // consume '.'
      while (_isDigit(_peek())) {
        _advance();
      }
    }

    final text = source.substring(start, _current);
    final value = text.contains('.') ? double.parse(text) : int.parse(text);
    return Token(TokenType.NUMBER, text, line, column, value);
  }

  Token _string(String quote, int line, int column) {
    final buffer = StringBuffer();

    while (!_isAtEnd() && _peek() != quote) {
      if (_peek() == '\\') {
        _advance();
        if (_isAtEnd()) break;

        // Handle escape sequences
        switch (_peek()) {
          case 'n': buffer.write('\n'); break;
          case 't': buffer.write('\t'); break;
          case 'r': buffer.write('\r'); break;
          case '\\': buffer.write('\\'); break;
          case quote: buffer.write(quote); break;
          default: buffer.write(_peek());
        }
        _advance();
      } else {
        buffer.write(_advance());
      }
    }

    if (_isAtEnd()) {
      throw CompileError('Unterminated string', line, column, phase: 'lexer');
    }

    _advance(); // closing quote
    return Token(TokenType.STRING, buffer.toString(), line, column, buffer.toString());
  }

  void _skipWhitespaceAndComments() {
    while (!_isAtEnd()) {
      final c = _peek();

      // Whitespace
      if (c == ' ' || c == '\t' || c == '\r') {
        _advance();
      } else if (c == '\n') {
        _advance();
        _line++;
        _column = 1;
      }
      // Line comment: % to end of line
      else if (c == '%') {
        while (!_isAtEnd() && _peek() != '\n') {
          _advance();
        }
      }
      // Block comment: /* ... */
      else if (c == '/' && _peekNext() == '*') {
        _advance(); // /
        _advance(); // *
        while (!_isAtEnd()) {
          if (_peek() == '*' && _peekNext() == '/') {
            _advance(); // *
            _advance(); // /
            break;
          }
          if (_peek() == '\n') {
            _line++;
            _column = 0;
          }
          _advance();
        }
      } else {
        break;
      }
    }
  }

  // Helper methods
  String _advance() {
    _column++;
    return source[_current++];
  }

  bool _match(String expected) {
    if (_isAtEnd()) return false;
    if (source[_current] != expected) return false;
    _advance();
    return true;
  }

  String _peek() => _isAtEnd() ? '\0' : source[_current];
  String _peekNext() => _current + 1 >= source.length ? '\0' : source[_current + 1];
  bool _isAtEnd() => _current >= source.length;

  bool _isDigit(String c) => c.codeUnitAt(0) >= '0'.codeUnitAt(0) && c.codeUnitAt(0) <= '9'.codeUnitAt(0);
  bool _isAlpha(String c) {
    final code = c.codeUnitAt(0);
    return (code >= 'a'.codeUnitAt(0) && code <= 'z'.codeUnitAt(0)) ||
           (code >= 'A'.codeUnitAt(0) && code <= 'Z'.codeUnitAt(0)) ||
           c == '_';
  }
  bool _isAlphaNumeric(String c) => _isAlpha(c) || _isDigit(c);
  bool _isUpper(String c) => c.codeUnitAt(0) >= 'A'.codeUnitAt(0) && c.codeUnitAt(0) <= 'Z'.codeUnitAt(0);

  Token _makeToken(TokenType type, int line, int column) {
    final lexeme = source.substring(_current - 1, _current);
    return Token(type, lexeme, line, column);
  }
}
```

---

## 4. Syntax Analysis (Parser)

### 4.1 AST Node Definitions

```dart
// Base class for all AST nodes
abstract class AstNode {
  final int line;
  final int column;

  AstNode(this.line, this.column);
}

// Top-level program
class Program extends AstNode {
  final List<Procedure> procedures;

  Program(this.procedures, int line, int column) : super(line, column);
}

// Procedure: all clauses with same functor/arity
class Procedure extends AstNode {
  final String name;
  final int arity;
  final List<Clause> clauses;

  Procedure(this.name, this.arity, this.clauses, int line, int column)
      : super(line, column);

  String get signature => '$name/$arity';
}

// Clause: Head :- Guards | Body.
class Clause extends AstNode {
  final Atom head;
  final List<Guard>? guards;  // Optional guard list before |
  final List<Goal>? body;     // Optional body goals after |

  Clause(this.head, {this.guards, this.body, required int line, required int column})
      : super(line, column);
}

// Atom: predicate in clause head
class Atom extends AstNode {
  final String functor;
  final List<Term> args;

  Atom(this.functor, this.args, int line, int column) : super(line, column);

  int get arity => args.length;
}

// Goal: predicate call in clause body
class Goal extends AstNode {
  final String functor;
  final List<Term> args;

  Goal(this.functor, this.args, int line, int column) : super(line, column);

  int get arity => args.length;
}

// Guard: pure test in guard section
class Guard extends AstNode {
  final String predicate;
  final List<Term> args;

  Guard(this.predicate, this.args, int line, int column) : super(line, column);
}

// Terms (expressions)
abstract class Term extends AstNode {
  Term(int line, int column) : super(line, column);
}

class VarTerm extends Term {
  final String name;
  final bool isReader;  // true for X?, false for X

  VarTerm(this.name, this.isReader, int line, int column) : super(line, column);

  @override
  String toString() => isReader ? '$name?' : name;
}

class StructTerm extends Term {
  final String functor;
  final List<Term> args;

  StructTerm(this.functor, this.args, int line, int column) : super(line, column);

  int get arity => args.length;

  @override
  String toString() => '$functor/${args.length}';
}

class ListTerm extends Term {
  final Term? head;
  final Term? tail;

  // [H|T] -> ListTerm(H, T)
  // []    -> ListTerm(null, null)
  ListTerm(this.head, this.tail, int line, int column) : super(line, column);

  bool get isNil => head == null && tail == null;

  @override
  String toString() {
    if (isNil) return '[]';
    if (tail == null) return '[$head]';
    return '[$head|$tail]';
  }
}

class ConstTerm extends Term {
  final Object? value;  // String, int, double, or atom name

  ConstTerm(this.value, int line, int column) : super(line, column);

  @override
  String toString() => value.toString();
}

class UnderscoreTerm extends Term {
  // Anonymous variable _
  UnderscoreTerm(int line, int column) : super(line, column);

  @override
  String toString() => '_';
}
```

### 4.2 Parser Implementation

```dart
class Parser {
  final List<Token> tokens;
  int _current = 0;

  Parser(this.tokens);

  Program parse() {
    final procedures = <Procedure>[];

    while (!_isAtEnd()) {
      procedures.add(_parseProcedure());
    }

    return Program(procedures, 1, 1);
  }

  // Procedure: one or more clauses with same head functor/arity
  Procedure _parseProcedure() {
    final clauses = <Clause>[];

    // Parse first clause
    final firstClause = _parseClause();
    clauses.add(firstClause);

    final name = firstClause.head.functor;
    final arity = firstClause.head.arity;

    // Parse additional clauses with same functor/arity
    while (!_isAtEnd() && _peek().type == TokenType.ATOM) {
      if (_peek().lexeme == name) {
        final clause = _parseClause();

        // Verify same arity
        if (clause.head.arity != arity) {
          throw CompileError(
            'Clause for $name has arity ${clause.head.arity}, expected $arity',
            clause.line,
            clause.column,
            phase: 'parser'
          );
        }

        clauses.add(clause);
      } else {
        break;  // Different procedure
      }
    }

    return Procedure(name, arity, clauses, firstClause.line, firstClause.column);
  }

  // Clause: Head :- Guards | Body.
  //     or: Head.
  Clause _parseClause() {
    final head = _parseAtom();

    List<Guard>? guards;
    List<Goal>? body;

    // Check for :- (clause with body)
    if (_match(TokenType.IMPLIES)) {
      // Parse guards before |
      guards = <Guard>[];

      while (!_check(TokenType.PIPE) && !_check(TokenType.DOT)) {
        guards.add(_parseGuard());

        if (!_check(TokenType.PIPE) && !_check(TokenType.DOT)) {
          _consume(TokenType.COMMA, 'Expected "," between guards');
        }
      }

      // Parse body after | (if present)
      if (_match(TokenType.PIPE)) {
        body = <Goal>[];

        body.add(_parseGoal());

        while (_match(TokenType.COMMA)) {
          body.add(_parseGoal());
        }
      }

      // Empty guard list = no guards
      if (guards.isEmpty) guards = null;
    }

    _consume(TokenType.DOT, 'Expected "." at end of clause');

    return Clause(head, guards: guards, body: body, line: head.line, column: head.column);
  }

  // Atom: functor(arg1, arg2, ...)
  Atom _parseAtom() {
    final functorToken = _consume(TokenType.ATOM, 'Expected predicate name');
    final args = <Term>[];

    if (_match(TokenType.LPAREN)) {
      if (!_check(TokenType.RPAREN)) {
        args.add(_parseTerm());

        while (_match(TokenType.COMMA)) {
          args.add(_parseTerm());
        }
      }

      _consume(TokenType.RPAREN, 'Expected ")" after arguments');
    }

    return Atom(functorToken.lexeme, args, functorToken.line, functorToken.column);
  }

  // Goal: same as Atom
  Goal _parseGoal() {
    final functorToken = _consume(TokenType.ATOM, 'Expected predicate name');
    final args = <Term>[];

    if (_match(TokenType.LPAREN)) {
      if (!_check(TokenType.RPAREN)) {
        args.add(_parseTerm());

        while (_match(TokenType.COMMA)) {
          args.add(_parseTerm());
        }
      }

      _consume(TokenType.RPAREN, 'Expected ")" after arguments');
    }

    return Goal(functorToken.lexeme, args, functorToken.line, functorToken.column);
  }

  // Guard: same as Goal but marked as guard
  Guard _parseGuard() {
    final functorToken = _consume(TokenType.ATOM, 'Expected guard predicate name');
    final args = <Term>[];

    if (_match(TokenType.LPAREN)) {
      if (!_check(TokenType.RPAREN)) {
        args.add(_parseTerm());

        while (_match(TokenType.COMMA)) {
          args.add(_parseTerm());
        }
      }

      _consume(TokenType.RPAREN, 'Expected ")" after arguments');
    }

    return Guard(functorToken.lexeme, args, functorToken.line, functorToken.column);
  }

  // Term: variable, structure, list, constant, underscore
  Term _parseTerm() {
    // Variable or Reader
    if (_check(TokenType.VARIABLE)) {
      final token = _advance();
      return VarTerm(token.lexeme, false, token.line, token.column);
    }

    if (_check(TokenType.READER)) {
      final token = _advance();
      return VarTerm(token.lexeme, true, token.line, token.column);
    }

    // Underscore (anonymous variable)
    if (_match(TokenType.UNDERSCORE)) {
      final token = _previous();
      return UnderscoreTerm(token.line, token.column);
    }

    // Number
    if (_check(TokenType.NUMBER)) {
      final token = _advance();
      return ConstTerm(token.literal, token.line, token.column);
    }

    // String
    if (_check(TokenType.STRING)) {
      final token = _advance();
      return ConstTerm(token.literal, token.line, token.column);
    }

    // List
    if (_check(TokenType.LBRACKET)) {
      return _parseList();
    }

    // Structure or Constant Atom
    if (_check(TokenType.ATOM)) {
      final functorToken = _advance();

      // Structure with arguments
      if (_match(TokenType.LPAREN)) {
        final args = <Term>[];

        if (!_check(TokenType.RPAREN)) {
          args.add(_parseTerm());

          while (_match(TokenType.COMMA)) {
            args.add(_parseTerm());
          }
        }

        _consume(TokenType.RPAREN, 'Expected ")" after structure arguments');

        return StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
      } else {
        // Constant atom
        return ConstTerm(functorToken.lexeme, functorToken.line, functorToken.column);
      }
    }

    throw CompileError(
      'Expected term, got ${_peek().type}',
      _peek().line,
      _peek().column,
      phase: 'parser'
    );
  }

  // List: [], [H|T], [X], [X,Y,Z]
  Term _parseList() {
    final bracketToken = _consume(TokenType.LBRACKET, 'Expected "["');

    // Empty list []
    if (_match(TokenType.RBRACKET)) {
      return ListTerm(null, null, bracketToken.line, bracketToken.column);
    }

    // Parse elements
    final elements = <Term>[];
    Term? tail;

    elements.add(_parseTerm());

    // Check for tail syntax [H|T]
    if (_match(TokenType.PIPE)) {
      tail = _parseTerm();
      _consume(TokenType.RBRACKET, 'Expected "]" after list tail');

      // Build right-associative list: [X|T]
      return ListTerm(elements[0], tail, bracketToken.line, bracketToken.column);
    }

    // Parse remaining elements [X, Y, Z]
    while (_match(TokenType.COMMA)) {
      elements.add(_parseTerm());
    }

    _consume(TokenType.RBRACKET, 'Expected "]" after list elements');

    // Build right-associative list: [X, Y, Z] -> [X|[Y|[Z|[]]]]
    Term result = ListTerm(null, null, bracketToken.line, bracketToken.column); // []
    for (int i = elements.length - 1; i >= 0; i--) {
      result = ListTerm(elements[i], result, bracketToken.line, bracketToken.column);
    }

    return result;
  }

  // Helper methods
  bool _match(TokenType type) {
    if (_check(type)) {
      _advance();
      return true;
    }
    return false;
  }

  bool _check(TokenType type) {
    if (_isAtEnd()) return false;
    return _peek().type == type;
  }

  Token _advance() {
    if (!_isAtEnd()) _current++;
    return _previous();
  }

  Token _peek() => tokens[_current];
  Token _previous() => tokens[_current - 1];
  bool _isAtEnd() => _peek().type == TokenType.EOF;

  Token _consume(TokenType type, String message) {
    if (_check(type)) return _advance();

    throw CompileError(message, _peek().line, _peek().column, phase: 'parser');
  }
}
```

---

## 5. Semantic Analysis (Analyzer)

### 5.1 Variable Analysis

```dart
class VariableInfo {
  final String name;
  final bool isWriter;

  // Occurrence tracking
  int writerOccurrences = 0;
  int readerOccurrences = 0;

  // First occurrence location
  AstNode? firstOccurrence;

  // Register assignment (filled during analysis)
  int? registerIndex;

  // For readers: the paired writer name
  String? pairedWriter;

  // Variable classification
  bool isTemporary = false;  // X register (doesn't cross calls)
  bool isPermanent = false;  // Y register (survives across calls)

  VariableInfo(this.name, this.isWriter);

  bool get isSRSWValid {
    // Writer can occur once as writer
    if (isWriter && writerOccurrences > 1) return false;

    // Reader can occur once (unless ground guard allows multiple)
    // For now, simple check: reader occurs at most once
    // TODO: Allow multiple reader occurrences with ground guard
    if (!isWriter && readerOccurrences > 1) return false;

    return true;
  }
}

class VariableTable {
  final Map<String, VariableInfo> _vars = {};

  // Track guard context
  bool _hasGroundGuard = false;
  final Set<String> _groundedVars = {};

  void recordWriterOccurrence(String name, AstNode node) {
    final info = _vars.putIfAbsent(name, () => VariableInfo(name, true));
    info.writerOccurrences++;
    info.firstOccurrence ??= node;
  }

  void recordReaderOccurrence(String name, AstNode node) {
    final writerName = name;  // Reader X? pairs with writer X

    // Ensure writer exists or create it
    final writerInfo = _vars.putIfAbsent(writerName, () => VariableInfo(writerName, true));

    // Record reader occurrence
    writerInfo.readerOccurrences++;
    writerInfo.firstOccurrence ??= node;
  }

  void markGrounded(String varName) {
    _groundedVars.add(varName);
  }

  bool isGrounded(String varName) => _groundedVars.contains(varName);

  void verifySRSW() {
    for (final info in _vars.values) {
      // Check writer occurrences
      if (info.writerOccurrences > 1) {
        throw CompileError(
          'SRSW violation: Writer variable "${info.name}" occurs ${info.writerOccurrences} times in clause',
          info.firstOccurrence?.line ?? 0,
          info.firstOccurrence?.column ?? 0,
          phase: 'analyzer'
        );
      }

      // Check reader occurrences (unless grounded)
      if (info.readerOccurrences > 1 && !isGrounded(info.name)) {
        throw CompileError(
          'SRSW violation: Reader variable "${info.name}?" occurs ${info.readerOccurrences} times without ground guard',
          info.firstOccurrence?.line ?? 0,
          info.firstOccurrence?.column ?? 0,
          phase: 'analyzer'
        );
      }
    }
  }

  List<VariableInfo> getAllVars() => _vars.values.toList();
  VariableInfo? getVar(String name) => _vars[name];
}
```

### 5.2 Annotated AST

```dart
class AnnotatedProgram {
  final Program ast;
  final List<AnnotatedProcedure> procedures;

  AnnotatedProgram(this.ast, this.procedures);
}

class AnnotatedProcedure {
  final Procedure ast;
  final String name;
  final int arity;
  final List<AnnotatedClause> clauses;

  // Code generation metadata (filled during codegen)
  int? entryPC;         // κ (kappa) - entry point for this procedure
  String? entryLabel;   // e.g., "merge/3"

  AnnotatedProcedure(this.ast, this.name, this.arity, this.clauses);

  String get signature => '$name/$arity';
}

class AnnotatedClause {
  final Clause ast;
  final VariableTable varTable;

  // Clause metadata
  bool hasGuards;
  bool hasBody;

  AnnotatedClause(this.ast, this.varTable, {this.hasGuards = false, this.hasBody = false});
}
```

### 5.3 Analyzer Implementation

```dart
class Analyzer {
  AnnotatedProgram analyze(Program program) {
    final annotatedProcs = <AnnotatedProcedure>[];

    for (final proc in program.procedures) {
      annotatedProcs.add(_analyzeProcedure(proc));
    }

    return AnnotatedProgram(program, annotatedProcs);
  }

  AnnotatedProcedure _analyzeProcedure(Procedure proc) {
    final annotatedClauses = <AnnotatedClause>[];

    for (final clause in proc.clauses) {
      annotatedClauses.add(_analyzeClause(clause));
    }

    return AnnotatedProcedure(proc, proc.name, proc.arity, annotatedClauses);
  }

  AnnotatedClause _analyzeClause(Clause clause) {
    final varTable = VariableTable();

    // Analyze head
    _analyzeAtom(clause.head, varTable);

    // Analyze guards (if present)
    final hasGuards = clause.guards != null && clause.guards!.isNotEmpty;
    if (hasGuards) {
      for (final guard in clause.guards!) {
        _analyzeGuard(guard, varTable);
      }
    }

    // Analyze body (if present)
    final hasBody = clause.body != null && clause.body!.isNotEmpty;
    if (hasBody) {
      for (final goal in clause.body!) {
        _analyzeGoal(goal, varTable);
      }
    }

    // Verify SRSW constraint
    varTable.verifySRSW();

    // Assign register indices
    _assignRegisters(varTable);

    return AnnotatedClause(clause.ast, varTable, hasGuards: hasGuards, hasBody: hasBody);
  }

  void _analyzeAtom(Atom atom, VariableTable varTable) {
    for (final arg in atom.args) {
      _analyzeTerm(arg, varTable);
    }
  }

  void _analyzeGoal(Goal goal, VariableTable varTable) {
    for (final arg in goal.args) {
      _analyzeTerm(arg, varTable);
    }
  }

  void _analyzeGuard(Guard guard, VariableTable varTable) {
    // Special handling for ground/1 and known/1
    if (guard.predicate == 'ground' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm && arg.isReader) {
        // ground(X?) allows multiple reader occurrences
        varTable.markGrounded(arg.name);
      }
    }

    // Analyze guard arguments
    for (final arg in guard.args) {
      _analyzeTerm(arg, varTable);
    }
  }

  void _analyzeTerm(Term term, VariableTable varTable) {
    if (term is VarTerm) {
      if (term.isReader) {
        varTable.recordReaderOccurrence(term.name, term);
      } else {
        varTable.recordWriterOccurrence(term.name, term);
      }
    } else if (term is StructTerm) {
      for (final arg in term.args) {
        _analyzeTerm(arg, varTable);
      }
    } else if (term is ListTerm) {
      if (term.head != null) {
        _analyzeTerm(term.head!, varTable);
      }
      if (term.tail != null) {
        _analyzeTerm(term.tail!, varTable);
      }
    }
    // ConstTerm and UnderscoreTerm have no variables to track
  }

  void _assignRegisters(VariableTable varTable) {
    int nextIndex = 0;

    for (final info in varTable.getAllVars()) {
      // For now, all variables are temporaries (X registers)
      // TODO: Analyze variable lifetimes to distinguish X vs Y registers
      info.isTemporary = true;
      info.registerIndex = nextIndex++;
    }
  }
}
```

---

## 6. Code Generation

### 6.1 Code Generation Context

```dart
class CodeGenContext {
  // Bytecode accumulator
  final List<Op> instructions = [];

  // Label management
  final Map<String, int> labels = {};
  final List<String> pendingLabels = [];  // Labels waiting to be placed

  // Temporary variable allocation
  int nextTempVar = 0;
  final Map<String, int> tempAllocation = {};

  // Current procedure context
  String? currentProcedure;
  int currentClauseIndex = 0;

  // Phase tracking
  bool inHead = false;
  bool inGuard = false;
  bool inBody = false;

  // Structure traversal state
  bool inStructure = false;
  int structureNestingDepth = 0;

  int get currentPC => instructions.length;

  void emit(Op instruction) {
    // Attach any pending labels to this instruction
    for (final label in pendingLabels) {
      labels[label] = currentPC;
    }
    pendingLabels.clear();

    instructions.add(instruction);
  }

  void emitLabel(String label) {
    pendingLabels.add(label);
  }

  int allocateTemp() => nextTempVar++;

  void resetTemps() {
    nextTempVar = 0;
    tempAllocation.clear();
  }
}
```

### 6.2 Code Generator Implementation

```dart
class CodeGenerator {
  BytecodeProgram generate(AnnotatedProgram program) {
    final ctx = CodeGenContext();

    // Generate code for each procedure
    for (final proc in program.procedures) {
      _generateProcedure(proc, ctx);
    }

    // Build final bytecode program
    return BytecodeProgram(
      instructions: ctx.instructions,
      labels: ctx.labels,
    );
  }

  void _generateProcedure(AnnotatedProcedure proc, CodeGenContext ctx) {
    ctx.currentProcedure = proc.signature;

    // Entry label: "p/1", "merge/3", etc.
    final entryLabel = proc.signature;
    ctx.emitLabel(entryLabel);

    // Record entry PC (κ)
    proc.entryPC = ctx.currentPC;
    proc.entryLabel = entryLabel;

    // Generate each clause
    for (int i = 0; i < proc.clauses.length; i++) {
      ctx.currentClauseIndex = i;
      final isLastClause = (i == proc.clauses.length - 1);

      final clause = proc.clauses[i];
      final nextLabel = isLastClause
          ? '${entryLabel}_end'
          : '${entryLabel}_c${i + 1}';

      _generateClause(clause, ctx, nextLabel, isLastClause);
    }

    // End of procedure
    ctx.emitLabel('${entryLabel}_end');
    ctx.emit(NoMoreClausesOp());  // Suspend if U non-empty, else fail
  }

  void _generateClause(AnnotatedClause clause, CodeGenContext ctx, String nextLabel, bool isLastClause) {
    ctx.resetTemps();

    // CLAUSE_TRY: Initialize Si=∅, σ̂w=∅
    ctx.emit(ClauseTryOp());

    // HEAD PHASE
    ctx.inHead = true;
    ctx.inGuard = false;
    ctx.inBody = false;

    _generateHead(clause.ast.head, clause.varTable, ctx);

    // GUARD PHASE (if present)
    if (clause.hasGuards && clause.ast.guards != null) {
      ctx.inHead = false;
      ctx.inGuard = true;

      for (final guard in clause.ast.guards!) {
        _generateGuard(guard, clause.varTable, ctx);
      }
    }

    // COMMIT or CLAUSE_NEXT
    if (isLastClause) {
      // Last clause: commit and execute body
      ctx.emit(CommitOp());  // Apply σ̂w, enter BODY phase

      // BODY PHASE
      ctx.inHead = false;
      ctx.inGuard = false;
      ctx.inBody = true;

      if (clause.hasBody && clause.ast.body != null) {
        _generateBody(clause.ast.body!, clause.varTable, ctx);
      } else {
        // Empty body: just proceed
        ctx.emit(ProceedOp());
      }

      // Label for clause end (before no_more_clauses)
      ctx.emitLabel('${ctx.currentProcedure}_c${ctx.currentClauseIndex}_end');

    } else {
      // Not last clause: clause_next to try next clause
      // Note: This is emitted BEFORE the label, so execution jumps over it
      ctx.emitLabel('${ctx.currentProcedure}_c${ctx.currentClauseIndex}_end');
      ctx.emit(ClauseNextOp(nextLabel));
    }
  }

  void _generateHead(Atom head, VariableTable varTable, CodeGenContext ctx) {
    // Process each head argument
    for (int i = 0; i < head.args.length; i++) {
      final arg = head.args[i];
      _generateHeadArgument(arg, i, varTable, ctx);
    }
  }

  void _generateHeadArgument(Term term, int argSlot, VariableTable varTable, CodeGenContext ctx) {
    if (term is VarTerm) {
      // Get variable register index
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      if (term.isReader) {
        // Reader in head: use GetVariable to load reader into register
        // Note: This creates a tentative association in σ̂w
        ctx.emit(GetVariableOp(regIndex, argSlot));
      } else {
        // Writer in head: use GetVariable to load writer into register
        ctx.emit(GetVariableOp(regIndex, argSlot));
      }

    } else if (term is ConstTerm) {
      // Constant in head: match with head_constant
      ctx.emit(HeadConstantOp(term.value, argSlot));

    } else if (term is ListTerm) {
      if (term.isNil) {
        // Empty list: head_nil
        ctx.emit(HeadNilOp(argSlot));
      } else {
        // Non-empty list [H|T]: head_list then structure traversal
        ctx.emit(HeadListOp(argSlot));  // Equivalent to head_structure('[|]', 2, argSlot)

        // Process head element
        if (term.head != null) {
          _generateStructureElement(term.head!, varTable, ctx);
        }

        // Process tail element
        if (term.tail != null) {
          _generateStructureElement(term.tail!, varTable, ctx);
        }
      }

    } else if (term is StructTerm) {
      // Structure in head: head_structure then traverse arguments
      ctx.emit(HeadStructureOp(term.functor, term.arity, argSlot));

      for (final subArg in term.args) {
        _generateStructureElement(subArg, varTable, ctx);
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable: void/1
      ctx.emit(VoidOp(1));
    }
  }

  void _generateStructureElement(Term term, VariableTable varTable, CodeGenContext ctx) {
    // Structure element processing for HEAD/GUARD phases
    // For BODY phase, use _generateStructureElementInBody
    // Called during structure traversal (S register in use)

    if (term is VarTerm) {
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      if (term.isReader) {
        // Reader at position S
        ctx.emit(ReaderOp(regIndex));
      } else {
        // Writer at position S
        ctx.emit(WriterOp(regIndex));
      }

    } else if (term is ConstTerm) {
      // Constant at position S
      ctx.emit(ConstantOp(term.value));

    } else if (term is ListTerm) {
      // Nested list: need to extract into temp, then match
      final tempReg = ctx.allocateTemp();

      if (ctx.inHead) {
        // READ mode: extract value at S into temp
        ctx.emit(WriterOp(tempReg));  // Extract into temp (uses Writer instruction)

        // Then match temp against nested structure
        if (term.isNil) {
          ctx.emit(HeadNilOp(tempReg));
        } else {
          ctx.emit(HeadListOp(tempReg));
          if (term.head != null) _generateStructureElement(term.head!, varTable, ctx);
          if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx);
        }
      } else {
        // WRITE mode: build nested structure first
        if (term.isNil) {
          ctx.emit(PutNilOp(tempReg));
        } else {
          ctx.emit(PutListOp(tempReg));
          if (term.head != null) _generateStructureElement(term.head!, varTable, ctx);
          if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx);
        }

        // Then write temp to current S position
        ctx.emit(WriterOp(tempReg));
      }

    } else if (term is StructTerm) {
      // Nested structure: extract into temp, then match
      final tempReg = ctx.allocateTemp();

      if (ctx.inHead) {
        // READ mode
        ctx.emit(WriterOp(tempReg));  // Extract into temp
        ctx.emit(HeadStructureOp(term.functor, term.arity, tempReg));
        for (final subArg in term.args) {
          _generateStructureElement(subArg, varTable, ctx);
        }
      } else {
        // WRITE mode
        ctx.emit(PutStructureOp(term.functor, term.arity, tempReg));
        for (final subArg in term.args) {
          _generateStructureElement(subArg, varTable, ctx);
        }
        ctx.emit(WriterOp(tempReg));
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable in structure
      ctx.emit(VoidOp(1));
    }
  }

  void _generateGuard(Guard guard, VariableTable varTable, CodeGenContext ctx) {
    // Special built-in guards
    if (guard.predicate == 'ground' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        final varInfo = varTable.getVar(arg.name);
        if (varInfo != null) {
          ctx.emit(GroundOp(varInfo.registerIndex!));
          return;
        }
      }
    }

    if (guard.predicate == 'known' && guard.args.length == 1) {
      final arg = guard.args[0];
      if (arg is VarTerm) {
        final varInfo = varTable.getVar(arg.name);
        if (varInfo != null) {
          ctx.emit(KnownOp(varInfo.registerIndex!));
          return;
        }
      }
    }

    if (guard.predicate == 'otherwise' && guard.args.isEmpty) {
      ctx.emit(OtherwiseOp());
      return;
    }

    // Generic guard predicate call
    // Setup arguments, then call guard
    for (int i = 0; i < guard.args.length; i++) {
      _generatePutArgument(guard.args[i], i, varTable, ctx);
    }

    ctx.emit(GuardOp(guard.predicate, guard.args.length));
  }

  void _generateBody(List<Goal> goals, VariableTable varTable, CodeGenContext ctx) {
    for (int i = 0; i < goals.length; i++) {
      final goal = goals[i];
      final isTailPosition = (i == goals.length - 1);

      // Setup arguments in A registers
      for (int j = 0; j < goal.args.length; j++) {
        _generatePutArgument(goal.args[j], j, varTable, ctx);
      }

      // Spawn or Requeue
      if (isTailPosition) {
        // Tail call: requeue
        ctx.emit(RequeueOp(goal.functor, goal.arity));
      } else {
        // Non-tail: spawn
        ctx.emit(SpawnOp(goal.functor, goal.arity));
      }
    }

    // If no tail call (e.g., empty body), emit proceed
    if (goals.isEmpty) {
      ctx.emit(ProceedOp());
    }
  }

  void _generatePutArgument(Term term, int argSlot, VariableTable varTable, CodeGenContext ctx) {
    if (term is VarTerm) {
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      if (term.isReader) {
        // Reader: put_reader
        ctx.emit(PutReaderOp(regIndex, argSlot));
      } else {
        // Writer: put_writer
        ctx.emit(PutWriterOp(regIndex, argSlot));
      }

    } else if (term is ConstTerm) {
      // Constant: put_constant
      ctx.emit(PutConstantOp(term.value, argSlot));

    } else if (term is ListTerm) {
      if (term.isNil) {
        ctx.emit(PutNilOp(argSlot));
      } else {
        // Build list structure
        ctx.emit(PutListOp(argSlot));
        if (term.head != null) _generateStructureElement(term.head!, varTable, ctx);
        if (term.tail != null) _generateStructureElement(term.tail!, varTable, ctx);
      }

    } else if (term is StructTerm) {
      // Build structure
      ctx.emit(PutStructureOp(term.functor, term.arity, argSlot));
      for (final arg in term.args) {
        _generateStructureElementInBody(arg, varTable, ctx);  // Use BODY-specific handler
      }

    } else if (term is UnderscoreTerm) {
      // Anonymous variable: create fresh unbound writer
      final tempReg = ctx.allocateTemp();
      ctx.emit(PutWriterOp(tempReg, argSlot));
    }
  }

  void _generateStructureElementInBody(Term term, VariableTable varTable, CodeGenContext ctx) {
    // Structure building in BODY phase - handles both ground and non-ground terms

    if (term is VarTerm) {
      // Variable in structure - emit as variable reference, not constant
      final varInfo = varTable.getVar(term.name);
      if (varInfo == null) {
        throw CompileError('Undefined variable in structure: ${term.name}', term.line, term.column, phase: 'codegen');
      }

      final regIndex = varInfo.registerIndex!;

      if (term.isReader) {
        ctx.emit(SetReader(regIndex));  // set_reader Xi
      } else {
        ctx.emit(SetWriter(regIndex));  // set_writer Xi
      }

    } else if (term is ConstTerm) {
      ctx.emit(SetConstant(term.value));  // set_constant c

    } else if (term is ListTerm) {
      // Nested list in structure
      if (term.isNil) {
        ctx.emit(SetConstant('nil'));
      } else {
        // For non-empty nested lists, need special handling
        throw CompileError('Nested non-empty lists in structures not yet supported', term.line, term.column, phase: 'codegen');
      }

    } else if (term is StructTerm) {
      // Nested structure - requires pre-building
      throw CompileError('Nested structures in BODY not yet supported', term.line, term.column, phase: 'codegen');

    } else if (term is UnderscoreTerm) {
      // Anonymous variable in structure
      final tempReg = ctx.allocateTemp();
      ctx.emit(SetWriter(tempReg));  // Create fresh writer
    }
  }
}

// Bytecode Program result
class BytecodeProgram {
  final List<Op> instructions;
  final Map<String, int> labels;

  BytecodeProgram({required this.instructions, required this.labels});

  int? getLabel(String name) => labels[name];
}
```

---

## 7. Optimization Passes

### 7.1 Peephole Optimization

```dart
class Optimizer {
  BytecodeProgram optimize(BytecodeProgram program) {
    var instructions = program.instructions;

    // Apply optimization passes
    instructions = _peepholeOptimization(instructions);
    instructions = _deadCodeElimination(instructions);

    // Rebuild label map after optimization
    final labels = _rebuildLabels(instructions, program.labels);

    return BytecodeProgram(instructions: instructions, labels: labels);
  }

  List<Op> _peepholeOptimization(List<Op> instructions) {
    final optimized = <Op>[];

    for (int i = 0; i < instructions.length; i++) {
      final current = instructions[i];
      final next = i + 1 < instructions.length ? instructions[i + 1] : null;

      // Pattern: GetVariable X, A followed by PutWriter X, A
      // Optimize: skip both (argument already in place)
      if (current is GetVariableOp && next is PutWriterOp) {
        if (current.varIndex == next.varIndex && current.argSlot == next.argSlot) {
          i++; // skip next
          continue;
        }
      }

      // Pattern: HeadConstant followed by Commit then Proceed
      // Could fuse into HeadConstantProceed (future optimization)

      optimized.add(current);
    }

    return optimized;
  }

  List<Op> _deadCodeElimination(List<Op> instructions) {
    // Remove unreachable code after Proceed, Requeue, ClauseNext, NoMoreClauses
    // (but preserve labels)
    final reachable = <Op>[];
    bool unreachable = false;

    for (final instr in instructions) {
      // Labels are always reachable (they're jump targets)
      if (instr is LabelOp) {
        unreachable = false;
        reachable.add(instr);
        continue;
      }

      if (unreachable) {
        // Skip unreachable instructions (except labels)
        continue;
      }

      reachable.add(instr);

      // Mark code after terminal instructions as unreachable
      if (instr is ProceedOp ||
          instr is RequeueOp ||
          instr is ClauseNextOp ||
          instr is NoMoreClausesOp ||
          instr is HaltOp) {
        unreachable = true;
      }
    }

    return reachable;
  }

  Map<String, int> _rebuildLabels(List<Op> instructions, Map<String, int> oldLabels) {
    final newLabels = <String, int>{};

    for (int i = 0; i < instructions.length; i++) {
      if (instructions[i] is LabelOp) {
        final label = (instructions[i] as LabelOp).name;
        newLabels[label] = i;
      }
    }

    return newLabels;
  }
}
```

### 7.2 Clause Indexing (Future)

```dart
// Future optimization: first-argument indexing for faster clause selection
class ClauseIndexer {
  // Build index on first argument of each clause
  // Hash table: constant → clause PC
  // Allows O(1) clause selection instead of O(n) scanning

  Map<Object?, List<int>> buildIndex(List<AnnotatedClause> clauses) {
    final index = <Object?, List<int>>{};

    for (int i = 0; i < clauses.length; i++) {
      final clause = clauses[i];
      final firstArg = clause.ast.head.args.isNotEmpty ? clause.ast.head.args[0] : null;

      if (firstArg is ConstTerm) {
        // Index on constant
        index.putIfAbsent(firstArg.value, () => []).add(i);
      } else if (firstArg is StructTerm) {
        // Index on functor
        index.putIfAbsent(firstArg.functor, () => []).add(i);
      }
      // Variables, readers, etc. go in default bucket
    }

    return index;
  }
}
```

---

## 8. Error Handling

### 8.1 Error Categories

```dart
enum ErrorCategory {
  lexical,    // Invalid characters, unterminated strings
  syntax,     // Malformed clauses, missing delimiters
  semantic,   // SRSW violations, undefined variables
  codegen,    // Internal compiler errors during bytecode generation
}

class CompileError implements Exception {
  final String message;
  final int line;
  final int column;
  final String? source;
  final ErrorCategory? category;

  CompileError(
    this.message,
    this.line,
    this.column,
    {this.source, String? phase}
  ) : category = phase != null ? _categoryFromPhase(phase) : null;

  static ErrorCategory? _categoryFromPhase(String phase) {
    switch (phase) {
      case 'lexer': return ErrorCategory.lexical;
      case 'parser': return ErrorCategory.syntax;
      case 'analyzer': return ErrorCategory.semantic;
      case 'codegen': return ErrorCategory.codegen;
      default: return null;
    }
  }

  @override
  String toString() {
    final categoryName = category != null ? '[${category.toString().split('.').last}] ' : '';
    final loc = 'Line $line, Column $column';

    if (source != null) {
      final lines = source!.split('\n');
      if (line > 0 && line <= lines.length) {
        final sourceLine = lines[line - 1];
        final pointer = ' ' * (column - 1) + '^';
        return '$categoryName$message\n$loc:\n$sourceLine\n$pointer';
      }
    }

    return '$categoryName$message at $loc';
  }
}
```

### 8.2 Common Errors

```dart
// Example error messages:

// SRSW violation
throw CompileError(
  'SRSW violation: Writer variable "X" occurs 2 times in clause',
  10, 15,
  phase: 'analyzer'
);

// Undefined variable
throw CompileError(
  'Variable "Z" used in body but not defined in head or guards',
  15, 20,
  phase: 'analyzer'
);

// Arity mismatch
throw CompileError(
  'Clause for p/2 has arity 3, expected 2',
  5, 1,
  phase: 'parser'
);

// Unterminated string
throw CompileError(
  'Unterminated string literal',
  12, 8,
  phase: 'lexer'
);
```

---

## 9. Implementation Notes

### 9.1 Integration with Runtime

The compiled bytecode integrates with the GLP runtime as follows:

```dart
// Example usage
final compiler = GlpCompiler(
  lexer: Lexer(),
  parser: Parser(),
  analyzer: Analyzer(),
  codegen: CodeGenerator(),
  optimizer: Optimizer(),
);

final source = '''
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], [], []).
''';

try {
  final program = compiler.compile(source);

  // Create runtime and runner
  final rt = GlpRuntime();
  final runner = BytecodeRunner(program);
  final sched = Scheduler(rt: rt, runner: runner);

  // Execute program...

} on CompileError catch (e) {
  print('Compilation failed:\n$e');
}
```

### 9.2 Testing Strategy

1. **Lexer tests**: Token recognition, error detection
2. **Parser tests**: AST construction, syntax validation
3. **Analyzer tests**: SRSW verification, variable classification
4. **Codegen tests**: Bytecode correctness, label resolution
5. **End-to-end tests**: Compile and execute sample programs

### 9.3 Future Enhancements

1. **Type system**: Optional static typing for safety
2. **Module system**: Import/export between GLP modules
3. **Clause indexing**: First-argument hashing for fast clause selection
4. **Inline optimization**: Inline small predicates
5. **Register allocation**: Optimize X vs Y register usage
6. **Parallel compilation**: Compile procedures concurrently
7. **Source maps**: Map bytecode back to source for debugging
8. **IDE support**: Language server protocol (LSP) for editors

### 9.4 Performance Considerations

- **Lexer**: Use StringBuilder for string accumulation
- **Parser**: Recursive descent is simple but may hit stack limits on deeply nested terms
- **Analyzer**: Use hash maps for variable lookup (O(1) average case)
- **Codegen**: Linear pass over AST, no backtracking needed
- **Optimizer**: Multiple passes may be expensive for large programs

### 9.5 Debugging Support

```dart
class DebugCompiler extends GlpCompiler {
  bool dumpTokens = false;
  bool dumpAST = false;
  bool dumpVariableTables = false;
  bool dumpBytecode = false;

  @override
  BytecodeProgram compile(String source, {bool optimize = true}) {
    if (dumpTokens) {
      final tokens = lexer.tokenize(source);
      print('=== TOKENS ===');
      for (final token in tokens) {
        print(token);
      }
    }

    if (dumpAST) {
      final tokens = lexer.tokenize(source);
      final ast = parser.parse(tokens);
      print('=== AST ===');
      print(_formatAST(ast));
    }

    if (dumpVariableTables) {
      final tokens = lexer.tokenize(source);
      final ast = parser.parse(tokens);
      final annotated = analyzer.analyze(ast);
      print('=== VARIABLE TABLES ===');
      for (final proc in annotated.procedures) {
        print('Procedure: ${proc.signature}');
        for (int i = 0; i < proc.clauses.length; i++) {
          print('  Clause $i:');
          final varTable = proc.clauses[i].varTable;
          for (final info in varTable.getAllVars()) {
            print('    ${info.name}: writer×${info.writerOccurrences} reader×${info.readerOccurrences} reg=${info.registerIndex}');
          }
        }
      }
    }

    final program = super.compile(source, optimize: optimize);

    if (dumpBytecode) {
      print('=== BYTECODE ===');
      for (int i = 0; i < program.instructions.length; i++) {
        final instr = program.instructions[i];
        print('$i: $instr');
      }
      print('=== LABELS ===');
      for (final entry in program.labels.entries) {
        print('${entry.key} → ${entry.value}');
      }
    }

    return program;
  }

  String _formatAST(AstNode node, {int indent = 0}) {
    // Pretty-print AST tree
    // ... implementation ...
    return '';
  }
}
```

---

## 10. Complete Example

### 10.1 Source Code

```glp
% Merge two streams into one, dovetailing
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
merge([], [], []).
```

### 10.2 Compilation Trace

```
=== TOKENS ===
ATOM(merge) at 1:1
LPAREN(() at 1:6
LBRACKET([) at 1:7
VARIABLE(X) at 1:8
PIPE(|) at 1:9
...

=== AST ===
Program
└── Procedure: merge/3
    ├── Clause 1
    │   ├── head: merge([X|Xs], Ys, [X?|Zs?])
    │   └── body: merge(Ys?, Xs?, Zs)
    ├── Clause 2
    │   ├── head: merge(Xs, [Y|Ys], [Y?|Zs?])
    │   └── body: merge(Xs?, Ys?, Zs)
    └── Clause 3
        └── head: merge([], [], [])

=== VARIABLE TABLES ===
Procedure: merge/3
  Clause 0:
    X: writer×1 reader×1 reg=0
    Xs: writer×1 reader×1 reg=1
    Ys: writer×1 reader×1 reg=2
    Zs: writer×1 reader×1 reg=3
  Clause 1:
    Xs: writer×1 reader×1 reg=0
    Y: writer×1 reader×1 reg=1
    Ys: writer×1 reader×1 reg=2
    Zs: writer×1 reader×1 reg=3
  Clause 2:
    (no variables)

=== BYTECODE ===
0: Label(merge/3)
1: ClauseTry()
2: HeadList(0)
3:   Writer(0)          # X
4:   Writer(1)          # Xs
5: GetVariable(2, 1)    # Ys
6: HeadList(2)
7:   Reader(0)          # X?
8:   Reader(3)          # Zs?
9: Commit()
10: PutReader(2, 0)     # A0 = Ys?
11: PutReader(1, 1)     # A1 = Xs?
12: PutWriter(3, 2)     # A2 = Zs
13: Requeue(merge, 3)
14: Label(merge/3_c0_end)
15: ClauseNext(merge/3_c1)
16: Label(merge/3_c1)
17: ClauseTry()
... (clause 2)
... (clause 3)
50: Label(merge/3_end)
51: NoMoreClauses()

=== LABELS ===
merge/3 → 0
merge/3_c0_end → 14
merge/3_c1 → 16
merge/3_c1_end → ...
merge/3_c2 → ...
merge/3_end → 50
```

---

## Appendix: Opcode Definitions

See `lib/bytecode/opcodes.dart` for complete opcode definitions. All opcodes mentioned in this document are defined there:

- `ClauseTryOp()` - Initialize clause attempt
- `ClauseNextOp(label)` - Move to next clause
- `CommitOp()` - Commit to current clause
- `NoMoreClausesOp()` - All clauses exhausted
- `HeadStructureOp(functor, arity, argSlot)` - Match structure in head
- `HeadConstantOp(value, argSlot)` - Match constant in head
- `HeadListOp(argSlot)` - Match list in head
- `HeadNilOp(argSlot)` - Match [] in head
- `WriterOp(varIndex)` - Process writer in structure
- `ReaderOp(varIndex)` - Process reader in structure
- `ConstantOp(value)` - Process constant in structure
- `VoidOp(count)` - Skip anonymous variables
- `GetVariableOp(varIndex, argSlot)` - Load argument into register
- `GetValueOp(varIndex, argSlot)` - Unify argument with register
- `PutStructureOp(functor, arity, argSlot)` - Create structure in body
- `PutWriterOp(varIndex, argSlot)` - Place writer in argument
- `PutReaderOp(varIndex, argSlot)` - Place reader in argument
- `PutConstantOp(value, argSlot)` - Place constant in argument
- `PutListOp(argSlot)` - Create list in body
- `PutNilOp(argSlot)` - Place [] in argument
- `GroundOp(varIndex)` - Test if variable is ground
- `KnownOp(varIndex)` - Test if variable is bound
- `OtherwiseOp()` - Default guard
- `GuardOp(predicate, arity)` - Call guard predicate
- `SpawnOp(functor, arity)` - Spawn concurrent goal
- `RequeueOp(functor, arity)` - Tail call
- `ProceedOp()` - Return from procedure
- `LabelOp(name)` - Jump target marker
- `HaltOp()` - Terminate execution

---

**End of GLP Compiler Specification v2.16**
