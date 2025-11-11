import 'token.dart';
import 'ast.dart';
import 'error.dart';

/// Parser for GLP source code
class Parser {
  final List<Token> tokens;
  int _current = 0;

  Parser(this.tokens);

  /// Parse tokens into an AST
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
  //     or: Head :- Body.
  //     or: Head.
  Clause _parseClause() {
    final head = _parseAtom();

    List<Guard>? guards;
    List<Goal>? body;

    // Check for :- (clause with guards/body)
    if (_match(TokenType.IMPLIES)) {
      // Parse everything before | as guards (or body if no |)
      final predicates = <dynamic>[];

      predicates.add(_parseGoalOrGuard());

      while (_match(TokenType.COMMA)) {
        predicates.add(_parseGoalOrGuard());
      }

      // Check for | separator
      if (_match(TokenType.PIPE)) {
        // Everything before | were guards - convert Goal to Guard
        guards = predicates.map((g) {
          return Guard(g.functor, g.args, g.line, g.column);
        }).toList();

        // Parse body after |
        body = <Goal>[];
        body.add(_parseGoal());

        while (_match(TokenType.COMMA)) {
          body.add(_parseGoal());
        }
      } else {
        // No | separator, so everything was body goals
        body = predicates.cast<Goal>();
      }
    }

    _consume(TokenType.DOT, 'Expected "." at end of clause');

    return Clause(head, guards: guards, body: body, line: head.line, column: head.column);
  }

  // Parse a predicate that could be either a guard or a goal
  dynamic _parseGoalOrGuard() {
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

    // Return as Goal for now (will be cast to Guard if before |)
    return Goal(functorToken.lexeme, args, functorToken.line, functorToken.column);
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

  // Term: variable, structure, list, constant, underscore, tuple
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

    // Parenthesized expression - could be tuple (A, B) or single term (A)
    if (_match(TokenType.LPAREN)) {
      final startToken = _previous();
      final terms = <Term>[];

      // Parse first term
      terms.add(_parseTerm());

      // Check for comma - indicates tuple/conjunction
      if (_match(TokenType.COMMA)) {
        // Build right-associative tuple: (A, B, C) = ','(A, ','(B, C))
        terms.add(_parseTerm());

        while (_match(TokenType.COMMA)) {
          terms.add(_parseTerm());
        }

        _consume(TokenType.RPAREN, 'Expected ")" after tuple');

        // Build right-associative structure
        Term result = terms.last;
        for (int i = terms.length - 2; i >= 0; i--) {
          result = StructTerm(',', [terms[i], result], startToken.line, startToken.column);
        }

        return result;
      } else {
        // Single parenthesized term - just return the term itself
        _consume(TokenType.RPAREN, 'Expected ")" after term');
        return terms[0];
      }
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
