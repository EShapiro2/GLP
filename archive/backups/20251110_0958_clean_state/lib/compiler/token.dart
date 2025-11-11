/// Token types for GLP lexical analysis
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

/// Token representing a lexical unit in GLP source code
class Token {
  final TokenType type;
  final String lexeme;
  final int line;
  final int column;
  final Object? literal;  // For NUMBER, STRING

  Token(this.type, this.lexeme, this.line, this.column, [this.literal]);

  @override
  String toString() {
    if (literal != null) {
      return '$type($lexeme=$literal) at $line:$column';
    }
    return '$type($lexeme) at $line:$column';
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is Token &&
        other.type == type &&
        other.lexeme == lexeme &&
        other.line == line &&
        other.column == column &&
        other.literal == literal;
  }

  @override
  int get hashCode => Object.hash(type, lexeme, line, column, literal);
}
