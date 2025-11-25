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
    // Skip any module declarations
    _skipDeclarations();

    final procedures = <Procedure>[];

    while (!_isAtEnd()) {
      procedures.add(_parseProcedure());
    }

    return Program(procedures, 1, 1);
  }

  /// Parse module declarations and return ModuleInfo
  /// Call this before parse() if you need module metadata
  ModuleInfo parseModuleInfo() {
    String? name;
    final exports = <String>{};
    final imports = <String>[];
    String language = 'glp';
    String mode = 'user';
    String serviceType = 'procedures';

    // Parse declarations until we hit a non-declaration
    while (!_isAtEnd() && _check(TokenType.MINUS)) {
      _advance(); // consume '-'

      if (!_check(TokenType.ATOM)) {
        // Not a declaration, back up
        _current--;
        break;
      }

      final keyword = _advance();

      switch (keyword.lexeme) {
        case 'module':
          _consume(TokenType.LPAREN, 'Expected "(" after module');
          name = _parseModuleName();
          _consume(TokenType.RPAREN, 'Expected ")" after module name');
          _consume(TokenType.DOT, 'Expected "." after module declaration');
          break;

        case 'export':
          _consume(TokenType.LPAREN, 'Expected "(" after export');
          _consume(TokenType.LBRACKET, 'Expected "[" for export list');
          exports.addAll(_parseProcRefList());
          _consume(TokenType.RBRACKET, 'Expected "]" after export list');
          _consume(TokenType.RPAREN, 'Expected ")" after export');
          _consume(TokenType.DOT, 'Expected "." after export declaration');
          break;

        case 'import':
          _consume(TokenType.LPAREN, 'Expected "(" after import');
          _consume(TokenType.LBRACKET, 'Expected "[" for import list');
          imports.addAll(_parseAtomList());
          _consume(TokenType.RBRACKET, 'Expected "]" after import list');
          _consume(TokenType.RPAREN, 'Expected ")" after import');
          _consume(TokenType.DOT, 'Expected "." after import declaration');
          break;

        case 'language':
          _consume(TokenType.LPAREN, 'Expected "(" after language');
          final langToken = _consume(TokenType.ATOM, 'Expected language name');
          language = langToken.lexeme;
          _consume(TokenType.RPAREN, 'Expected ")" after language');
          _consume(TokenType.DOT, 'Expected "." after language declaration');
          break;

        case 'mode':
          _consume(TokenType.LPAREN, 'Expected "(" after mode');
          final modeToken = _consume(TokenType.ATOM, 'Expected mode name');
          mode = modeToken.lexeme;
          _consume(TokenType.RPAREN, 'Expected ")" after mode');
          _consume(TokenType.DOT, 'Expected "." after mode declaration');
          break;

        case 'service_type':
          _consume(TokenType.LPAREN, 'Expected "(" after service_type');
          final typeToken = _consume(TokenType.ATOM, 'Expected service type');
          serviceType = typeToken.lexeme;
          _consume(TokenType.RPAREN, 'Expected ")" after service_type');
          _consume(TokenType.DOT, 'Expected "." after service_type declaration');
          break;

        default:
          // Unknown declaration, back up to the '-'
          _current = _current - 2;
          break;
      }
    }

    return ModuleInfo(
      name: name,
      exports: exports,
      imports: imports,
      language: language,
      mode: mode,
      serviceType: serviceType,
    );
  }

  /// Parse hierarchical module name (e.g., utils.list)
  String _parseModuleName() {
    final parts = <String>[];
    parts.add(_consume(TokenType.ATOM, 'Expected module name').lexeme);

    while (_match(TokenType.DOT) && _check(TokenType.ATOM)) {
      parts.add(_consume(TokenType.ATOM, 'Expected module name part').lexeme);
    }

    // Back up if we consumed a DOT but the next token wasn't ATOM
    // (This handles -module(foo). where DOT ends the declaration)
    if (_previous().type == TokenType.DOT && !_check(TokenType.ATOM)) {
      _current--;
    }

    return parts.join('.');
  }

  /// Parse list of procedure references: [proc/arity, ...]
  List<String> _parseProcRefList() {
    final refs = <String>[];

    if (_check(TokenType.RBRACKET)) return refs;  // Empty list

    refs.add(_parseProcRef());

    while (_match(TokenType.COMMA)) {
      refs.add(_parseProcRef());
    }

    return refs;
  }

  /// Parse single procedure reference: name/arity
  String _parseProcRef() {
    final name = _consume(TokenType.ATOM, 'Expected procedure name').lexeme;
    _consume(TokenType.SLASH, 'Expected "/" in procedure reference');
    final arityToken = _consume(TokenType.NUMBER, 'Expected arity');
    final arity = arityToken.literal as int;
    return '$name/$arity';
  }

  /// Parse list of atoms: [atom, ...]
  List<String> _parseAtomList() {
    final atoms = <String>[];

    if (_check(TokenType.RBRACKET)) return atoms;  // Empty list

    atoms.add(_parseModuleName());

    while (_match(TokenType.COMMA)) {
      atoms.add(_parseModuleName());
    }

    return atoms;
  }

  /// Skip module declarations at start of file
  void _skipDeclarations() {
    while (!_isAtEnd() && _check(TokenType.MINUS)) {
      final startPos = _current;
      _advance(); // consume '-'

      if (!_check(TokenType.ATOM)) {
        _current = startPos;  // Back up, not a declaration
        break;
      }

      final keyword = _peek().lexeme;

      if (['module', 'export', 'import', 'language', 'mode', 'service_type'].contains(keyword)) {
        // Skip to the next DOT
        while (!_isAtEnd() && !_check(TokenType.DOT)) {
          _advance();
        }
        if (_check(TokenType.DOT)) {
          _advance();  // consume '.'
        }
      } else {
        // Not a declaration keyword, back up
        _current = startPos;
        break;
      }
    }
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
    // Special case: := clauses start with VARIABLE, not ATOM
    while (!_isAtEnd()) {
      // Check if next clause could be part of this procedure
      bool couldBeSameProcedure = false;

      if (_peek().type == TokenType.ATOM && _peek().lexeme == name) {
        // Same predicate name
        couldBeSameProcedure = true;
      } else if (name == ':=' && _peek().type == TokenType.VARIABLE) {
        // := clauses start with variable (e.g., "Result := X + Y")
        // Look ahead to see if it's followed by :=
        if (_current + 1 < tokens.length && tokens[_current + 1].type == TokenType.ASSIGN) {
          couldBeSameProcedure = true;
        }
      }

      if (!couldBeSameProcedure) break;

      final clause = _parseClause();

      // Verify same functor and arity
      if (clause.head.functor != name || clause.head.arity != arity) {
        throw CompileError(
          'Clause for ${clause.head.functor}/${clause.head.arity} found, expected $name/$arity',
          clause.line,
          clause.column,
          phase: 'parser'
        );
      }

      clauses.add(clause);
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
    // Check for parenthesized disjunction: (Goal1 ; Goal2)
    if (_check(TokenType.LPAREN)) {
      final startToken = _advance(); // consume '('
      final firstGoal = _parseGoalOrGuard();

      if (_match(TokenType.SEMICOLON)) {
        // This is a disjunction
        final secondGoal = _parseGoalOrGuard();
        _consume(TokenType.RPAREN, 'Expected ")" after disjunction');
        // Return as ';'(Goal1, Goal2) - need to convert goals to terms
        final firstTerm = _goalToTerm(firstGoal);
        final secondTerm = _goalToTerm(secondGoal);
        return Goal(';', [firstTerm, secondTerm], startToken.line, startToken.column);
      } else {
        // Not a disjunction - put back what we parsed and try again
        // This is complex, so for now just expect closing paren
        _consume(TokenType.RPAREN, 'Expected ")" after guard');
        return firstGoal;
      }
    }

    // Check for assignment: Var := Expr
    if (_check(TokenType.VARIABLE)) {
      final varToken = _peek();
      // Look ahead for :=
      if (tokens.length > _current + 1 && tokens[_current + 1].type == TokenType.ASSIGN) {
        _advance(); // consume variable
        _advance(); // consume :=
        final varTerm = VarTerm(varToken.lexeme, false, varToken.line, varToken.column);
        final expr = _parseExpression();
        return Goal(':=', [varTerm, expr], varToken.line, varToken.column);
      }
    }

    // Try to parse as regular predicate first
    if (_check(TokenType.ATOM)) {
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

      final goal = Goal(functorToken.lexeme, args, functorToken.line, functorToken.column);

      // Check for Module # Goal syntax
      if (_match(TokenType.HASH)) {
        if (args.isNotEmpty) {
          throw CompileError(
            'Module name cannot have arguments: ${functorToken.lexeme}',
            functorToken.line,
            functorToken.column,
            phase: 'parser'
          );
        }
        final remoteGoal = _parseGoal();
        return RemoteGoal(functorToken.lexeme, remoteGoal, functorToken.line, functorToken.column);
      }

      // Return as Goal for now (will be cast to Guard if before |)
      return goal;
    }

    // Otherwise, try to parse as infix comparison (e.g., X < Y)
    // Use _parsePrimary to avoid parsing < as expression operator
    final left = _parsePrimary();

    // Check for comparison operator
    if (_check(TokenType.LESS) || _check(TokenType.GREATER) ||
        _check(TokenType.LESS_EQUAL) || _check(TokenType.GREATER_EQUAL) ||
        _check(TokenType.EQUALS) || _check(TokenType.ARITH_EQUAL) ||
        _check(TokenType.ARITH_NOT_EQUAL)) {
      final opToken = _advance();
      final right = _parsePrimary();

      // Transform infix to prefix: X < Y → <(X, Y)
      final functor = opToken.lexeme;
      return Goal(functor, [left, right], opToken.line, opToken.column);
    }

    // Not a valid guard or goal
    throw CompileError(
      'Expected predicate name or comparison',
      _peek().line,
      _peek().column,
      phase: 'parser'
    );
  }

  // Convert a Goal to a Term representation (for disjunction)
  Term _goalToTerm(dynamic goal) {
    if (goal is Goal) {
      return StructTerm(goal.functor, goal.args, goal.line, goal.column);
    }
    throw CompileError('Expected goal', 0, 0, phase: 'parser');
  }

  // Atom: functor(arg1, arg2, ...) or Var := Expr (for clause heads)
  Atom _parseAtom() {
    // Check for := pattern: Var := Expr
    if (_check(TokenType.VARIABLE)) {
      final varToken = _advance();
      if (_match(TokenType.ASSIGN)) {
        // Parse as ':='(Var, Expr)
        final varTerm = VarTerm(varToken.lexeme, false, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Atom(':=', [varTerm, expr], varToken.line, varToken.column);
      } else {
        // Not an assignment - put variable back by rewinding
        _current--;
      }
    }

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

  // Goal: same as Atom, but handles Module # Goal syntax and assignment (Var := Expr)
  Goal _parseGoal() {
    // Check for assignment: Var := Expr
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
      final varToken = _advance();
      if (_match(TokenType.ASSIGN)) {
        // Parse as ':='(Var, Expr)
        final varTerm = varToken.type == TokenType.READER
            ? VarTerm(varToken.lexeme, true, varToken.line, varToken.column)
            : VarTerm(varToken.lexeme, false, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Goal(':=', [varTerm, expr], varToken.line, varToken.column);
      } else {
        // Not an assignment - this is an error in goal position
        throw CompileError(
          'Expected predicate name or assignment, got variable "${varToken.lexeme}"',
          varToken.line,
          varToken.column,
          phase: 'parser'
        );
      }
    }

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

    final goal = Goal(functorToken.lexeme, args, functorToken.line, functorToken.column);

    // Check for Module # Goal syntax
    if (_match(TokenType.HASH)) {
      // The first atom was actually the module name
      if (args.isNotEmpty) {
        throw CompileError(
          'Module name cannot have arguments: ${functorToken.lexeme}',
          functorToken.line,
          functorToken.column,
          phase: 'parser'
        );
      }

      // Parse the actual goal
      final remoteGoal = _parseGoal();
      return RemoteGoal(functorToken.lexeme, remoteGoal, functorToken.line, functorToken.column);
    }

    return goal;
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

  // Term: variable, structure, list, constant, underscore, tuple, or expression
  Term _parseTerm() {
    // Try to parse as expression (handles arithmetic operators)
    return _parseExpression();
  }

  // Expression parsing with precedence (Pratt parsing)
  // This handles arithmetic operators with proper precedence
  Term _parseExpression([int minPrecedence = 0]) {
    var left = _parsePrimary();

    while (_isOperator(_peek()) && _precedence(_peek()) >= minPrecedence) {
      final op = _advance();
      final right = _parseExpression(_precedence(op) + 1);
      left = StructTerm(_operatorFunctor(op), [left, right], op.line, op.column);
    }

    return left;
  }

  // Primary expression: variable, number, string, list, structure, parenthesized, unary minus
  Term _parsePrimary() {
    // Unary minus: -X becomes neg(X)
    if (_match(TokenType.MINUS)) {
      final minusToken = _previous();
      final operand = _parsePrimary();
      return StructTerm('neg', [operand], minusToken.line, minusToken.column);
    }

    // Variable or Reader - check for := assignment
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
      final token = _advance();
      final isReader = token.type == TokenType.READER;

      // Check for := assignment (Var := Expr)
      if (_match(TokenType.ASSIGN)) {
        final varTerm = VarTerm(token.lexeme, isReader, token.line, token.column);
        final expr = _parseExpression();
        return StructTerm(':=', [varTerm, expr], token.line, token.column);
      }

      return VarTerm(token.lexeme, isReader, token.line, token.column);
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

    // Parenthesized expression - could be tuple (A, B) or single term (A) or arithmetic (A + B)
    if (_match(TokenType.LPAREN)) {
      final startToken = _previous();
      final terms = <Term>[];

      // Parse first term (which may be an expression)
      terms.add(_parseExpression());

      // Check for comma - indicates tuple/conjunction
      if (_match(TokenType.COMMA)) {
        // Build right-associative tuple: (A, B, C) = ','(A, ','(B, C))
        terms.add(_parseExpression());

        while (_match(TokenType.COMMA)) {
          terms.add(_parseExpression());
        }

        _consume(TokenType.RPAREN, 'Expected ")" after tuple');

        // Build right-associative structure
        Term result = terms.last;
        for (int i = terms.length - 2; i >= 0; i--) {
          result = StructTerm(',', [terms[i], result], startToken.line, startToken.column);
        }

        return result;
      } else {
        // Single parenthesized expression - return it
        _consume(TokenType.RPAREN, 'Expected ")" after expression');
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
          args.add(_parseExpression());

          while (_match(TokenType.COMMA)) {
            args.add(_parseExpression());
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

  // Check if token is an arithmetic operator
  bool _isOperator(Token token) {
    return token.type == TokenType.PLUS ||
           token.type == TokenType.MINUS ||
           token.type == TokenType.STAR ||
           token.type == TokenType.SLASH ||
           token.type == TokenType.SLASH_SLASH ||
           token.type == TokenType.MOD ||
           token.type == TokenType.LESS ||
           token.type == TokenType.GREATER ||
           token.type == TokenType.LESS_EQUAL ||
           token.type == TokenType.GREATER_EQUAL ||
           token.type == TokenType.EQUALS ||
           token.type == TokenType.ARITH_EQUAL ||
           token.type == TokenType.ARITH_NOT_EQUAL;
  }

  // Get operator precedence
  int _precedence(Token op) {
    switch (op.type) {
      case TokenType.STAR:
      case TokenType.SLASH:
      case TokenType.SLASH_SLASH:
      case TokenType.MOD:
        return 20;  // Multiplicative
      case TokenType.PLUS:
      case TokenType.MINUS:
        return 10;  // Additive
      case TokenType.LESS:
      case TokenType.GREATER:
      case TokenType.LESS_EQUAL:
      case TokenType.GREATER_EQUAL:
      case TokenType.EQUALS:
      case TokenType.ARITH_EQUAL:
      case TokenType.ARITH_NOT_EQUAL:
        return 5;   // Comparison (lower than arithmetic)
      default:
        return 0;
    }
  }

  // Get operator functor name for AST
  String _operatorFunctor(Token op) {
    switch (op.type) {
      case TokenType.PLUS:
        return '+';
      case TokenType.MINUS:
        return '-';
      case TokenType.STAR:
        return '*';
      case TokenType.SLASH:
        return '/';
      case TokenType.SLASH_SLASH:
        return '//';
      case TokenType.MOD:
        return 'mod';
      case TokenType.LESS:
        return '<';
      case TokenType.GREATER:
        return '>';
      case TokenType.LESS_EQUAL:
        return '=<';
      case TokenType.GREATER_EQUAL:
        return '>=';
      case TokenType.EQUALS:
        return '=';
      case TokenType.ARITH_EQUAL:
        return '=:=';
      case TokenType.ARITH_NOT_EQUAL:
        return '=\\=';
      default:
        throw CompileError(
          'Unknown operator: ${op.type}',
          op.line,
          op.column,
          phase: 'parser'
        );
    }
  }

  // List: [], [H|T], [X], [X,Y,Z], [X,Y,Z|T]
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

    // Parse remaining elements and check for tail
    while (_match(TokenType.COMMA)) {
      elements.add(_parseTerm());
    }

    // Check for tail syntax [H|T] or [X,Y|T]
    if (_match(TokenType.PIPE)) {
      tail = _parseTerm();
      _consume(TokenType.RBRACKET, 'Expected "]" after list tail');

      // Build right-associative list: [X,Y,Z|T] -> [X|[Y|[Z|T]]]
      Term result = tail;
      for (int i = elements.length - 1; i >= 0; i--) {
        result = ListTerm(elements[i], result, bracketToken.line, bracketToken.column);
      }
      return result;
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
