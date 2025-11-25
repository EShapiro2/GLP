import 'token.dart';
import 'error.dart';

/// Lexical analyzer for GLP source code
class Lexer {
  final String source;
  int _current = 0;
  int _line = 1;
  int _column = 1;

  Lexer(this.source);

  /// Tokenize the entire source into a list of tokens
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

  /// Scan a single token
  Token _scanToken() {
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
      case ';': return _makeToken(TokenType.SEMICOLON, startLine, startColumn);

      // Arithmetic operators
      case '+': return _makeToken(TokenType.PLUS, startLine, startColumn);
      case '*': return _makeToken(TokenType.STAR, startLine, startColumn);
      case '/':
        if (_match('/')) {
          final lexeme = source.substring(_current - 2, _current);
          return Token(TokenType.SLASH_SLASH, lexeme, startLine, startColumn);
        }
        return _makeToken(TokenType.SLASH, startLine, startColumn);

      // Comparison operators
      case '<':
        return _makeToken(TokenType.LESS, startLine, startColumn);
      case '>':
        if (_match('=')) {
          final lexeme = source.substring(_current - 2, _current);
          return Token(TokenType.GREATER_EQUAL, lexeme, startLine, startColumn);
        }
        return _makeToken(TokenType.GREATER, startLine, startColumn);
      case '=':
        if (_match('<')) {
          final lexeme = source.substring(_current - 2, _current);
          return Token(TokenType.LESS_EQUAL, lexeme, startLine, startColumn);
        }
        if (_match(':')) {
          if (_match('=')) {
            final lexeme = source.substring(_current - 3, _current);
            return Token(TokenType.ARITH_EQUAL, lexeme, startLine, startColumn);
          }
          throw CompileError('Expected "=" after "=:"', startLine, startColumn, phase: 'lexer');
        }
        if (_match('\\')) {
          if (_match('=')) {
            final lexeme = source.substring(_current - 3, _current);
            return Token(TokenType.ARITH_NOT_EQUAL, lexeme, startLine, startColumn);
          }
          throw CompileError('Expected "=" after "=\\"', startLine, startColumn, phase: 'lexer');
        }
        return _makeToken(TokenType.EQUALS, startLine, startColumn);

      case ':':
        if (_match('-')) {
          final lexeme = source.substring(_current - 2, _current);
          return Token(TokenType.IMPLIES, lexeme, startLine, startColumn);
        }
        if (_match('=')) {
          final lexeme = source.substring(_current - 2, _current);
          return Token(TokenType.ASSIGN, lexeme, startLine, startColumn);
        }
        throw CompileError('Unexpected character :', startLine, startColumn, phase: 'lexer');

      case '_':
        if (!_isAlphaNumeric(_peek())) {
          return _makeToken(TokenType.UNDERSCORE, startLine, startColumn);
        }
        // Otherwise fall through to identifier
        return _identifier(_current - 1, startLine, startColumn);

      case '"':
      case "'":
        return _string(c, startLine, startColumn);

      case '-':
        // Check if it's a negative number literal or minus operator
        if (_isDigit(_peek())) {
          return _number(_current - 1, startLine, startColumn);
        } else {
          return _makeToken(TokenType.MINUS, startLine, startColumn);
        }

      default:
        if (_isDigit(c)) {
          return _number(_current - 1, startLine, startColumn);
        }
        if (_isAlpha(c)) {
          return _identifier(_current - 1, startLine, startColumn);
        }
        throw CompileError('Unexpected character: $c', startLine, startColumn, phase: 'lexer');
    }
  }

  /// Scan identifier or variable
  Token _identifier(int start, int line, int column) {
    while (_isAlphaNumeric(_peek())) {
      _advance();
    }

    final text = source.substring(start, _current);

    // Check for 'mod' keyword - but only if not followed by '(' (predicate call)
    if (text == 'mod' && _peek() != '(') {
      return Token(TokenType.MOD, text, line, column);
    }

    // Check for reader syntax (Variable?)
    if (_peek() == '?' && _isUpper(text[0])) {
      _advance(); // consume '?'
      return Token(TokenType.READER, text, line, column);
    }

    // Variable (uppercase) or Atom (lowercase)
    final type = _isUpper(text[0]) ? TokenType.VARIABLE : TokenType.ATOM;
    return Token(type, text, line, column);
  }

  /// Scan number (integer or float)
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

  /// Scan string literal
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
          case '"': buffer.write('"'); break;
          case "'": buffer.write("'"); break;
          default: buffer.write(_peek());
        }
        _advance();
      } else {
        if (_peek() == '\n') {
          _line++;
          _column = 0;
        }
        buffer.write(_advance());
      }
    }

    if (_isAtEnd()) {
      throw CompileError('Unterminated string', line, column, phase: 'lexer');
    }

    _advance(); // closing quote
    return Token(TokenType.STRING, buffer.toString(), line, column, buffer.toString());
  }

  /// Skip whitespace and comments
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

  String _peek() => _isAtEnd() ? '\x00' : source[_current];
  String _peekNext() => _current + 1 >= source.length ? '\x00' : source[_current + 1];
  bool _isAtEnd() => _current >= source.length;

  bool _isDigit(String c) {
    if (c == '\x00') return false;
    final code = c.codeUnitAt(0);
    return code >= '0'.codeUnitAt(0) && code <= '9'.codeUnitAt(0);
  }

  bool _isAlpha(String c) {
    if (c == '\x00') return false;
    final code = c.codeUnitAt(0);
    return (code >= 'a'.codeUnitAt(0) && code <= 'z'.codeUnitAt(0)) ||
           (code >= 'A'.codeUnitAt(0) && code <= 'Z'.codeUnitAt(0)) ||
           c == '_';
  }

  bool _isAlphaNumeric(String c) => _isAlpha(c) || _isDigit(c);

  bool _isUpper(String c) {
    if (c == '\x00') return false;
    final code = c.codeUnitAt(0);
    return code >= 'A'.codeUnitAt(0) && code <= 'Z'.codeUnitAt(0);
  }

  Token _makeToken(TokenType type, int line, int column) {
    final lexeme = source.substring(_current - 1, _current);
    return Token(type, lexeme, line, column);
  }
}
