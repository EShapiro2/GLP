// lib/analysis/type_checker/type_parser.dart
//
// Parser for GLP type declarations following Yardeni-Shapiro syntax.
// Type declarations are first-class syntactic elements, like clauses.
//
// Grammar:
//   type_decl  ::= type_name '::=' type_expr '.'
//   type_expr  ::= type_alt (';' type_alt)*
//   type_alt   ::= constant | functor '(' type_args ')' | '[]' | '[' type_ref '|' type_ref ']'
//   type_args  ::= type_ref (',' type_ref)*
//   type_ref   ::= type_name
//   type_name  ::= UppercaseIdentifier
//   proc_decl  ::= 'procedure' atom '(' type_refs ')' '.'

import 'type_ast.dart';

/// Token types for type parsing
enum TypeTokenType {
  typeName,      // Uppercase: Nat, List, Number
  atom,          // lowercase: s, cons, append
  number,        // 0, 42, 3.14
  string,        // "hello", 'world'
  colonColonEq,  // ::=
  semicolon,     // ;
  dot,           // .
  lparen,        // (
  rparen,        // )
  lbracket,      // [
  rbracket,      // ]
  pipe,          // |
  comma,         // ,
  procedure,     // keyword 'procedure'
  unknown,       // Unknown token (skip)
  eof,
}

class TypeToken {
  final TypeTokenType type;
  final String lexeme;
  final Object? literal;
  final int line;
  final int column;
  
  TypeToken(this.type, this.lexeme, this.line, this.column, [this.literal]);
  
  @override
  String toString() => '$type($lexeme)';
}

/// Lexer for type declarations
class TypeLexer {
  final String source;
  int _current = 0;
  int _line = 1;
  int _column = 1;
  
  TypeLexer(this.source);
  
  List<TypeToken> tokenize() {
    final tokens = <TypeToken>[];
    
    while (!_isAtEnd()) {
      _skipWhitespaceAndComments();
      if (_isAtEnd()) break;
      
      final token = _scanToken();
      if (token != null) {
        tokens.add(token);
      }
    }
    
    tokens.add(TypeToken(TypeTokenType.eof, '', _line, _column));
    return tokens;
  }
  
  void _skipWhitespaceAndComments() {
    while (!_isAtEnd()) {
      final c = _peek();
      if (c == ' ' || c == '\t' || c == '\r') {
        _advance();
      } else if (c == '\n') {
        _line++;
        _column = 1;
        _advance();
      } else if (c == '%') {
        // Skip comment to end of line
        while (!_isAtEnd() && _peek() != '\n') {
          _advance();
        }
      } else {
        break;
      }
    }
  }
  
  TypeToken? _scanToken() {
    final startLine = _line;
    final startColumn = _column;
    final c = _advance();
    
    switch (c) {
      case '(':
        return TypeToken(TypeTokenType.lparen, '(', startLine, startColumn);
      case ')':
        return TypeToken(TypeTokenType.rparen, ')', startLine, startColumn);
      case '[':
        return TypeToken(TypeTokenType.lbracket, '[', startLine, startColumn);
      case ']':
        return TypeToken(TypeTokenType.rbracket, ']', startLine, startColumn);
      case '|':
        return TypeToken(TypeTokenType.pipe, '|', startLine, startColumn);
      case ',':
        return TypeToken(TypeTokenType.comma, ',', startLine, startColumn);
      case ';':
        return TypeToken(TypeTokenType.semicolon, ';', startLine, startColumn);
      case '.':
        return TypeToken(TypeTokenType.dot, '.', startLine, startColumn);
      case ':':
        if (_match(':') && _match('=')) {
          return TypeToken(TypeTokenType.colonColonEq, '::=', startLine, startColumn);
        }
        // Skip unknown characters (like : from :- in clauses)
        return TypeToken(TypeTokenType.unknown, c, startLine, startColumn);
      case '"':
      case "'":
        return _string(c, startLine, startColumn);
      default:
        if (_isDigit(c) || (c == '-' && _isDigit(_peek()))) {
          return _number(c, startLine, startColumn);
        }
        if (_isAlpha(c)) {
          return _identifier(c, startLine, startColumn);
        }
        // Skip unknown characters (clause syntax, operators, etc.)
        return TypeToken(TypeTokenType.unknown, c, startLine, startColumn);
    }
  }
  
  TypeToken _identifier(String first, int line, int column) {
    final sb = StringBuffer(first);
    while (!_isAtEnd() && _isAlphaNumeric(_peek())) {
      sb.write(_advance());
    }
    final lexeme = sb.toString();
    
    // Check for keywords
    if (lexeme == 'procedure') {
      return TypeToken(TypeTokenType.procedure, lexeme, line, column);
    }
    
    // Type names start with uppercase
    if (lexeme[0].toUpperCase() == lexeme[0] && _isAlpha(lexeme[0])) {
      return TypeToken(TypeTokenType.typeName, lexeme, line, column);
    }
    
    // Otherwise it's an atom
    return TypeToken(TypeTokenType.atom, lexeme, line, column);
  }
  
  TypeToken _number(String first, int line, int column) {
    final sb = StringBuffer(first);
    while (!_isAtEnd() && _isDigit(_peek())) {
      sb.write(_advance());
    }
    
    // Check for decimal
    if (!_isAtEnd() && _peek() == '.' && _isDigit(_peekNext())) {
      sb.write(_advance()); // consume '.'
      while (!_isAtEnd() && _isDigit(_peek())) {
        sb.write(_advance());
      }
      return TypeToken(TypeTokenType.number, sb.toString(), line, column, double.parse(sb.toString()));
    }
    
    return TypeToken(TypeTokenType.number, sb.toString(), line, column, int.parse(sb.toString()));
  }
  
  TypeToken _string(String quote, int line, int column) {
    final sb = StringBuffer();
    while (!_isAtEnd() && _peek() != quote) {
      if (_peek() == '\n') {
        _line++;
        _column = 1;
      }
      sb.write(_advance());
    }
    
    if (_isAtEnd()) {
      throw TypeParseError('Unterminated string', line, column);
    }
    
    _advance(); // closing quote
    return TypeToken(TypeTokenType.string, sb.toString(), line, column, sb.toString());
  }
  
  String _advance() {
    final c = source[_current++];
    _column++;
    return c;
  }
  
  String _peek() => _isAtEnd() ? '\x00' : source[_current];
  String _peekNext() => _current + 1 >= source.length ? '\x00' : source[_current + 1];
  
  bool _match(String expected) {
    if (_isAtEnd() || source[_current] != expected) return false;
    _current++;
    _column++;
    return true;
  }
  
  bool _isAtEnd() => _current >= source.length;
  bool _isDigit(String c) => c.compareTo('0') >= 0 && c.compareTo('9') <= 0;
  bool _isAlpha(String c) => 
      (c.compareTo('a') >= 0 && c.compareTo('z') <= 0) ||
      (c.compareTo('A') >= 0 && c.compareTo('Z') <= 0) ||
      c == '_';
  bool _isAlphaNumeric(String c) => _isAlpha(c) || _isDigit(c);
}

/// Parser for type declarations
class TypeParser {
  final List<TypeToken> tokens;
  int _current = 0;
  
  TypeParser(this.tokens);
  
  /// Parse all type definitions and procedure declarations from source
  TypeEnvironment parse() {
    final types = <String, TypeDef>{};
    final procedures = <String, ProcDecl>{};

    while (!_isAtEnd()) {
      // Check for type definition: TypeName ::= ...
      if (_check(TypeTokenType.typeName) && _peekNext()?.type == TypeTokenType.colonColonEq) {
        final typeDef = _parseTypeDef();
        types[typeDef.name] = typeDef;
      }
      // Check for procedure declaration: procedure name(...) .
      else if (_check(TypeTokenType.procedure)) {
        final procDecl = _parseProcDecl();
        procedures[procDecl.key] = procDecl;
      }
      // Skip everything else (clauses, operators, etc.)
      else {
        _advance();
      }
    }

    return TypeEnvironment(types, procedures);
  }

  /// Look ahead one token without consuming
  TypeToken? _peekNext() {
    if (_current + 1 >= tokens.length) return null;
    return tokens[_current + 1];
  }
  
  /// Parse: TypeName ::= type_expr .
  TypeDef _parseTypeDef() {
    final nameToken = _consume(TypeTokenType.typeName, 'Expected type name');
    _consume(TypeTokenType.colonColonEq, 'Expected "::=" after type name');
    
    final alternatives = _parseTypeExpr();
    
    _consume(TypeTokenType.dot, 'Expected "." after type definition');
    
    return TypeDef(nameToken.lexeme, alternatives, nameToken.line, nameToken.column);
  }
  
  /// Parse: type_alt (; type_alt)*
  List<TypeExpr> _parseTypeExpr() {
    final alts = <TypeExpr>[_parseTypeAlt()];
    
    while (_match(TypeTokenType.semicolon)) {
      alts.add(_parseTypeAlt());
    }
    
    return alts;
  }
  
  /// Parse: constant | functor(args) | [] | [H|T]
  TypeExpr _parseTypeAlt() {
    final token = _peek();
    
    // Empty list: []
    if (_match(TypeTokenType.lbracket)) {
      if (_match(TypeTokenType.rbracket)) {
        return ListNilAlt(token.line, token.column);
      }
      
      // List cons: [H | T]
      final head = _parseTypeRef();
      _consume(TypeTokenType.pipe, 'Expected "|" in list type');
      final tail = _parseTypeRef();
      _consume(TypeTokenType.rbracket, 'Expected "]" after list type');
      return ListConsAlt(head, tail, token.line, token.column);
    }
    
    // Number constant
    if (_check(TypeTokenType.number)) {
      final numToken = _advance();
      return ConstantAlt(numToken.literal!, numToken.line, numToken.column);
    }
    
    // String constant
    if (_check(TypeTokenType.string)) {
      final strToken = _advance();
      return ConstantAlt(strToken.literal!, strToken.line, strToken.column);
    }
    
    // Type reference (uppercase)
    if (_check(TypeTokenType.typeName)) {
      return _parseTypeRef();
    }
    
    // Atom constant or functor
    if (_check(TypeTokenType.atom)) {
      final atomToken = _advance();
      
      // Check for functor(args)
      if (_match(TypeTokenType.lparen)) {
        final args = <TypeExpr>[_parseTypeRef()];
        while (_match(TypeTokenType.comma)) {
          args.add(_parseTypeRef());
        }
        _consume(TypeTokenType.rparen, 'Expected ")" after functor arguments');
        return StructAlt(atomToken.lexeme, args, atomToken.line, atomToken.column);
      }
      
      // Plain atom constant
      return ConstantAlt(atomToken.lexeme, atomToken.line, atomToken.column);
    }
    
    throw TypeParseError('Expected type alternative', token.line, token.column);
  }
  
  /// Parse: TypeName
  TypeRef _parseTypeRef() {
    final token = _consume(TypeTokenType.typeName, 'Expected type name');
    return TypeRef(token.lexeme, token.line, token.column);
  }
  
  /// Parse: procedure name(Type1, Type2, ...) .
  ProcDecl _parseProcDecl() {
    _consume(TypeTokenType.procedure, 'Expected "procedure"');
    final nameToken = _consume(TypeTokenType.atom, 'Expected procedure name');
    _consume(TypeTokenType.lparen, 'Expected "(" after procedure name');
    
    final argTypes = <TypeRef>[];
    if (!_check(TypeTokenType.rparen)) {
      argTypes.add(_parseTypeRef());
      while (_match(TypeTokenType.comma)) {
        argTypes.add(_parseTypeRef());
      }
    }
    
    _consume(TypeTokenType.rparen, 'Expected ")" after procedure arguments');
    _consume(TypeTokenType.dot, 'Expected "." after procedure declaration');
    
    return ProcDecl(nameToken.lexeme, argTypes, nameToken.line, nameToken.column);
  }
  
  // Helper methods
  
  bool _match(TypeTokenType type) {
    if (_check(type)) {
      _advance();
      return true;
    }
    return false;
  }
  
  bool _check(TypeTokenType type) {
    if (_isAtEnd()) return false;
    return _peek().type == type;
  }
  
  TypeToken _advance() {
    if (!_isAtEnd()) _current++;
    return _previous();
  }
  
  TypeToken _peek() => tokens[_current];
  TypeToken _previous() => tokens[_current - 1];
  bool _isAtEnd() => _peek().type == TypeTokenType.eof;
  
  TypeToken _consume(TypeTokenType type, String message) {
    if (_check(type)) return _advance();
    throw TypeParseError(message, _peek().line, _peek().column);
  }
}

/// Parse type declarations from GLP source
TypeEnvironment parseTypes(String source) {
  final lexer = TypeLexer(source);
  final tokens = lexer.tokenize();
  final parser = TypeParser(tokens);
  return parser.parse();
}

/// Error during type parsing
class TypeParseError implements Exception {
  final String message;
  final int line;
  final int column;
  
  TypeParseError(this.message, this.line, this.column);
  
  @override
  String toString() => 'Type parse error at line $line, column $column: $message';
}
