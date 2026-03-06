import 'token.dart';
import 'ast.dart';
import 'error.dart';
import '../analysis/type_checker/type_ast.dart';
import '../analysis/type_checker/type_conversion.dart';
import '../analysis/type_checker/prelude.dart' show builtinProcedures;

/// Parser for GLP source code
class Parser {
  final List<Token> tokens;
  int _current = 0;
  Clause? _pendingClause;  // Clause parsed but belongs to different procedure

  Parser(this.tokens);

  /// Parse tokens into an AST (legacy method, skips declarations)
  Program parse() {
    // Skip any module declarations at the start
    _skipDeclarations();

    final procedures = <Procedure>[];

    while (!_isAtEnd()) {
      procedures.add(_parseProcedure());
    }

    // Check for non-contiguous clauses (same name/arity appearing multiple times)
    _checkContiguousClauses(procedures);

    return Program(procedures, 1, 1);
  }

  /// Check that all clauses for each procedure are contiguous in the source.
  /// GLP requires clauses to be grouped together - non-contiguous clauses
  /// cause the compiler to generate incorrect bytecode.
  void _checkContiguousClauses(List<Procedure> procedures) {
    final seen = <String, Procedure>{};  // signature -> first occurrence
    
    for (final proc in procedures) {
      final sig = '${proc.name}/${proc.arity}';
      
      if (seen.containsKey(sig)) {
        final first = seen[sig]!;
        throw CompileError(
          'Non-contiguous clauses for "$sig".\n'
          '  First group at line ${first.line}, second group at line ${proc.line}.\n'
          '  All clauses for a predicate must be together in the source file.',
          proc.line,
          proc.column,
          phase: 'parser'
        );
      }
      
      seen[sig] = proc;
    }
  }

  /// Parse tokens into a Module AST (includes declarations)
  Module parseModule() {
    ModuleDeclaration? moduleDecl;
    CompileMode compileMode = CompileMode.user;  // default: user mode

    // Parse declarations at the start of the file
    while (!_isAtEnd() && _check(TokenType.MINUS)) {
      final startPos = _current;
      final startLine = _peek().line;
      final startCol = _peek().column;
      _advance(); // consume '-'

      if (!_check(TokenType.ATOM)) {
        _current = startPos;  // Back up, not a declaration
        break;
      }

      final keyword = _advance();

      switch (keyword.lexeme) {
        case 'module':
          _consume(TokenType.LPAREN, 'Expected "(" after module');
          final name = _parseModuleName();
          _consume(TokenType.RPAREN, 'Expected ")" after module name');
          _consume(TokenType.DOT, 'Expected "." after module declaration');
          moduleDecl = ModuleDeclaration(name, startLine, startCol);
          break;

        case 'stdlib':
          // -stdlib. is deprecated — treated as -mode(system).
          _consume(TokenType.DOT, 'Expected "." after stdlib declaration');
          compileMode = CompileMode.system;
          break;

        case 'mode':
          // -mode(user). or -mode(system). declaration
          _consume(TokenType.LPAREN, 'Expected "(" after mode');
          if (!_check(TokenType.ATOM)) {
            throw CompileError(
              'Expected "user" or "system" in mode declaration',
              _peek().line,
              _peek().column,
              phase: 'parser'
            );
          }
          final modeToken = _advance();
          if (modeToken.lexeme == 'user') {
            compileMode = CompileMode.user;
          } else if (modeToken.lexeme == 'system') {
            compileMode = CompileMode.system;
          } else {
            throw CompileError(
              'Invalid mode "${modeToken.lexeme}". Expected "user" or "system".',
              modeToken.line,
              modeToken.column,
              phase: 'parser'
            );
          }
          _consume(TokenType.RPAREN, 'Expected ")" after mode');
          _consume(TokenType.DOT, 'Expected "." after mode declaration');
          break;

        case 'export':
          throw CompileError(
            'The -export() declaration is no longer supported. Use \'exported procedure\' instead.',
            startLine,
            startCol,
            phase: 'parser'
          );

        case 'import':
          throw CompileError(
            'The -import() declaration is no longer supported. Use \'imported procedure\' instead.',
            startLine,
            startCol,
            phase: 'parser'
          );

        default:
          // Unknown declaration, back up to the '-'
          _current = startPos;
          break;
      }
    }

    // Parse type definitions, procedure declarations, and clauses in order.
    // New rules (per typed-program.md):
    // - Type definitions can appear anywhere before first use
    // - Procedure declarations must appear immediately before the first clause
    // - All clauses for a procedure must be contiguous
    final typeDefs = <TypeDef>[];
    final procDeclarations = <ProcDecl>[];
    final procedures = <Procedure>[];

    // Track pending procedure declaration (waiting for its first clause)
    ProcDecl? pendingProcDecl;
    // Track which procedures we've seen clauses for (signature -> first Procedure)
    final seenProcedures = <String, Procedure>{};

    while (!_isAtEnd()) {
      // Check for procedure declaration: 'procedure ...' or 'exported procedure ...' or 'imported procedure ...'
      final isProcedureDecl = _check(TokenType.PROCEDURE) ||
          (_check(TokenType.ATOM) && (_peek().lexeme == 'exported' || _peek().lexeme == 'imported') &&
           _current + 1 < tokens.length && tokens[_current + 1].type == TokenType.PROCEDURE);

      if (isProcedureDecl) {
        // Procedure declaration (possibly exported or imported)
        if (pendingProcDecl != null) {
          // Check if the pending declaration is for a builtin or imported (no clauses needed)
          final pendingSig = '${pendingProcDecl.name}/${pendingProcDecl.argTypes.length}';
          if (!builtinProcedures.contains(pendingSig) && !pendingProcDecl.imported) {
            throw CompileError(
              'Procedure declaration for "${pendingProcDecl.name}" has no clauses.\n'
              '  A procedure declaration must be immediately followed by its clauses.',
              pendingProcDecl.line,
              pendingProcDecl.column,
              phase: 'parser'
            );
          }
          // Builtin or imported - clear pending without error
          pendingProcDecl = null;
        }
        final decl = _parseProcDeclaration();
        procDeclarations.add(decl);
        // Imported procedures are declaration-only — no clauses expected
        if (!decl.imported) {
          pendingProcDecl = decl;
        }
      } else if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
        // Might be a type definition (TypeName ::= ...) or a clause head
        final startPos = _current;
        final token = _peek();

        // Look ahead to see if this is a type definition (has ::=)
        if (_isTypeDefinition()) {
          // Type definition
          if (pendingProcDecl != null) {
            // Check if the pending declaration is for a builtin or imported (no clauses needed)
            final pendingSig = '${pendingProcDecl.name}/${pendingProcDecl.argTypes.length}';
            if (!builtinProcedures.contains(pendingSig) && !pendingProcDecl.imported) {
              throw CompileError(
                'Type definition cannot appear between procedure declaration and its clauses.\n'
                '  Procedure "${pendingProcDecl.name}" declared at line ${pendingProcDecl.line} needs clauses.',
                token.line,
                token.column,
                phase: 'parser'
              );
            }
            // Builtin or imported - clear pending without error
            pendingProcDecl = null;
          }
          typeDefs.add(_parseTypeDef());
        } else {
          // It's a clause - parse the procedure
          _current = startPos;
          final proc = _parseProcedure();
          final sig = '${proc.name}/${proc.arity}';

          // Check if this matches pending declaration
          if (pendingProcDecl != null) {
            final pendingSig = '${pendingProcDecl.name}/${pendingProcDecl.argTypes.length}';
            if (sig == pendingSig) {
              // This clause matches the pending declaration - good
              pendingProcDecl = null;
            } else if (builtinProcedures.contains(pendingSig)) {
              // Pending was a builtin (no clauses needed) - clear it
              pendingProcDecl = null;
            } else {
              throw CompileError(
                'Clause for "$sig" appears between procedure declaration and clauses for "$pendingSig".\n'
                '  Procedure declaration at line ${pendingProcDecl.line} must be immediately followed by its clauses.',
                proc.line,
                proc.column,
                phase: 'parser'
              );
            }
          }

          // Check for non-contiguous clauses
          if (seenProcedures.containsKey(sig)) {
            final first = seenProcedures[sig]!;
            throw CompileError(
              'Non-contiguous clauses for "$sig".\n'
              '  First group at line ${first.line}, second group at line ${proc.line}.\n'
              '  All clauses for a predicate must be together in the source file.',
              proc.line,
              proc.column,
              phase: 'parser'
            );
          }

          seenProcedures[sig] = proc;
          procedures.add(proc);
        }
      } else if (_check(TokenType.ATOM)) {
        // Clause starting with atom (procedure name)
        final proc = _parseProcedure();
        final sig = '${proc.name}/${proc.arity}';

        // Check if this matches pending declaration
        if (pendingProcDecl != null) {
          final pendingSig = '${pendingProcDecl.name}/${pendingProcDecl.argTypes.length}';
          if (sig == pendingSig) {
            // This clause matches the pending declaration - good
            pendingProcDecl = null;
          } else if (builtinProcedures.contains(pendingSig)) {
            // Pending was a builtin (no clauses needed) - clear it
            pendingProcDecl = null;
          } else {
            throw CompileError(
              'Clause for "$sig" appears between procedure declaration and clauses for "$pendingSig".\n'
              '  Procedure declaration at line ${pendingProcDecl.line} must be immediately followed by its clauses.',
              proc.line,
              proc.column,
              phase: 'parser'
            );
          }
        }

        // Check for non-contiguous clauses
        if (seenProcedures.containsKey(sig)) {
          final first = seenProcedures[sig]!;
          throw CompileError(
            'Non-contiguous clauses for "$sig".\n'
            '  First group at line ${first.line}, second group at line ${proc.line}.\n'
            '  All clauses for a predicate must be together in the source file.',
            proc.line,
            proc.column,
            phase: 'parser'
          );
        }

        seenProcedures[sig] = proc;
        procedures.add(proc);
      } else {
        // Unexpected token
        throw CompileError(
          'Unexpected token: ${_peek().lexeme}',
          _peek().line,
          _peek().column,
          phase: 'parser'
        );
      }
    }

    // Check for dangling procedure declaration at end of file
    if (pendingProcDecl != null) {
      final pendingSig = '${pendingProcDecl.name}/${pendingProcDecl.argTypes.length}';
      if (!builtinProcedures.contains(pendingSig) && !pendingProcDecl.imported) {
        throw CompileError(
          'Procedure declaration for "${pendingProcDecl.name}" has no clauses.\n'
          '  A procedure declaration must be immediately followed by its clauses.',
          pendingProcDecl.line,
          pendingProcDecl.column,
          phase: 'parser'
        );
      }
    }

    return Module(
      declaration: moduleDecl,
      typeDefs: typeDefs,
      procDeclarations: procDeclarations,
      procedures: procedures,
      compileMode: compileMode,
      line: 1,
      column: 1,
    );
  }

  /// Skip module declarations at start of file (for legacy parse())
  void _skipDeclarations() {
    while (!_isAtEnd() && _check(TokenType.MINUS)) {
      final startPos = _current;
      _advance(); // consume '-'

      if (!_check(TokenType.ATOM)) {
        _current = startPos;  // Back up, not a declaration
        break;
      }

      final keyword = _peek().lexeme;

      if (['module', 'stdlib', 'mode'].contains(keyword)) {
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

  // _parseProcRefList, _parseProcRef, _parseAtomList removed in Phase 1.
  // These were only used for -export([...]) and -import([...]) syntax.

  // Procedure: one or more clauses with same head functor/arity
  Procedure _parseProcedure() {
    final clauses = <Clause>[];

    // Use pending clause if available, otherwise parse first clause
    final Clause firstClause;
    if (_pendingClause != null) {
      firstClause = _pendingClause!;
      _pendingClause = null;
    } else {
      firstClause = _parseClause();
    }
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
      } else if (name == ':=' && (_peek().type == TokenType.VARIABLE || _peek().type == TokenType.READER || _peek().type == TokenType.UNDERSCORE)) {
        // := clauses start with variable or underscore (e.g., "Result := X + Y" or "_ := X / 0")
        // Look ahead to see if it's followed by :=
        if (_current + 1 < tokens.length && tokens[_current + 1].type == TokenType.ASSIGN) {
          couldBeSameProcedure = true;
        }
      } else if (name == '=..' && (_peek().type == TokenType.VARIABLE || _peek().type == TokenType.READER || _peek().type == TokenType.UNDERSCORE)) {
        // =.. clauses start with variable or underscore (e.g., "X? =.. Y")
        // Look ahead to see if it's followed by =..
        if (_current + 1 < tokens.length && tokens[_current + 1].type == TokenType.UNIV) {
          couldBeSameProcedure = true;
        }
      } else if (name == '..=' && (_peek().type == TokenType.VARIABLE || _peek().type == TokenType.READER || _peek().type == TokenType.UNDERSCORE)) {
        // ..= clauses start with variable or underscore (e.g., "List ..= X?")
        // Look ahead to see if it's followed by ..=
        if (_current + 1 < tokens.length && tokens[_current + 1].type == TokenType.UNIV_DECOMPOSE) {
          couldBeSameProcedure = true;
        }
      } else if (name == '=' && (_peek().type == TokenType.VARIABLE || _peek().type == TokenType.READER || _peek().type == TokenType.UNDERSCORE)) {
        // = clauses start with variable or underscore (e.g., "X? = Y")
        // Look ahead to see if it's followed by =
        if (_current + 1 < tokens.length && tokens[_current + 1].type == TokenType.EQUALS) {
          couldBeSameProcedure = true;
        }
      }

      if (!couldBeSameProcedure) break;

      final clause = _parseClause();

      // If functor matches but arity differs, this is a different procedure
      // Store it as pending and break
      if (clause.head.functor == name && clause.head.arity != arity) {
        _pendingClause = clause;
        break;
      }

      // Verify same functor (arity already checked above for same-name case)
      if (clause.head.functor != name) {
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
          // Detect negated guards (functor starts with ~)
          final isNegated = g.functor.startsWith('~');
          final actualFunctor = isNegated ? g.functor.substring(1) : g.functor;
          return Guard(actualFunctor, g.args, g.line, g.column, negated: isNegated);
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
    // Check for guard negation: ~G
    bool negated = false;
    int negLine = _peek().line;
    int negColumn = _peek().column;
    if (_match(TokenType.TILDE)) {
      negated = true;
      negLine = _previous().line;
      negColumn = _previous().column;

      // Check for double negation ~~G (syntactically forbidden)
      if (_check(TokenType.TILDE)) {
        throw CompileError(
          'Double negation ~~G is not allowed',
          _peek().line,
          _peek().column,
          phase: 'parser'
        );
      }
    }

    // Check for parenthesized expression: (Goal) or (Goal1 ; Goal2)
    if (_check(TokenType.LPAREN)) {
      final startToken = _advance(); // consume '('
      final firstGoal = _parseGoalOrGuard();

      if (_match(TokenType.SEMICOLON)) {
        // This is a disjunction - negation not allowed
        if (negated) {
          throw CompileError(
            'Guard negation (~) cannot be applied to disjunction',
            negLine,
            negColumn,
            phase: 'parser'
          );
        }
        final secondGoal = _parseGoalOrGuard();
        _consume(TokenType.RPAREN, 'Expected ")" after disjunction');
        // Return as ';'(Goal1, Goal2) - need to convert goals to terms
        final firstTerm = _goalToTerm(firstGoal);
        final secondTerm = _goalToTerm(secondGoal);
        return Goal(';', [firstTerm, secondTerm], startToken.line, startToken.column);
      } else {
        // Parenthesized single goal - apply negation if present
        _consume(TokenType.RPAREN, 'Expected ")" after guard');
        if (negated) {
          // Apply negation to the parsed goal
          final functor = '~${firstGoal.functor}';
          return Goal(functor, firstGoal.args, negLine, negColumn);
        }
        return firstGoal;
      }
    }

    // Check for assignment (Var := Expr) or univ (Var =.. Expr)
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
      final varToken = _peek();
      final isReader = varToken.type == TokenType.READER;
      // Look ahead for := or =..
      if (tokens.length > _current + 1 && tokens[_current + 1].type == TokenType.ASSIGN) {
        _advance(); // consume variable
        _advance(); // consume :=
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final expr = _parseExpression();
        return Goal(':=', [varTerm, expr], varToken.line, varToken.column);
      } else if (tokens.length > _current + 1 && tokens[_current + 1].type == TokenType.UNIV) {
        _advance(); // consume variable
        _advance(); // consume =..
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Goal('=..', [varTerm, expr], varToken.line, varToken.column);
      } else if (tokens.length > _current + 1 && tokens[_current + 1].type == TokenType.UNIV_DECOMPOSE) {
        _advance(); // consume variable
        _advance(); // consume ..=
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Goal('..=', [varTerm, expr], varToken.line, varToken.column);
      } else if (tokens.length > _current + 1 && tokens[_current + 1].type == TokenType.EQUALS) {
        _advance(); // consume variable
        _advance(); // consume =
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final term = _parseTerm();
        return Goal('=', [varTerm, term], varToken.line, varToken.column);
      } else if (tokens.length > _current + 1 && tokens[_current + 1].type == TokenType.HASH) {
        // Dynamic remote goal: Var # Goal (e.g., M? # factorial(5, R))
        _advance(); // consume variable
        _advance(); // consume #
        // Negation not allowed on remote goals
        if (negated) {
          throw CompileError(
            'Guard negation (~) cannot be applied to remote goal',
            negLine,
            negColumn,
            phase: 'parser'
          );
        }
        final moduleTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final innerGoal = _parseGoal();
        return RemoteGoal(moduleTerm, innerGoal, varToken.line, varToken.column);
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

      // Check for static remote goal: atom # goal (e.g., math # factorial(5, R))
      if (_match(TokenType.HASH)) {
        // Module name cannot have arguments
        if (args.isNotEmpty) {
          throw CompileError(
            'Module name cannot have arguments: ${functorToken.lexeme}',
            functorToken.line,
            functorToken.column,
            phase: 'parser'
          );
        }
        // Negation not allowed on remote goals
        if (negated) {
          throw CompileError(
            'Guard negation (~) cannot be applied to remote goal',
            negLine,
            negColumn,
            phase: 'parser'
          );
        }
        final moduleTerm = ConstTerm(functorToken.lexeme, functorToken.line, functorToken.column);
        final innerGoal = _parseGoal();
        return RemoteGoal(moduleTerm, innerGoal, functorToken.line, functorToken.column);
      }

      // Check if followed by = (e.g., foo = bar, or foo(a) = X)
      if (_match(TokenType.EQUALS)) {
        final leftTerm = args.isEmpty
            ? ConstTerm(functorToken.lexeme, functorToken.line, functorToken.column)
            : StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
        final rightTerm = _parseTerm();
        // Negation not allowed on unification goals
        if (negated) {
          throw CompileError(
            'Guard negation (~) cannot be applied to unification',
            negLine,
            negColumn,
            phase: 'parser'
          );
        }
        return Goal('=', [leftTerm, rightTerm], functorToken.line, functorToken.column);
      }

      // Return as Goal for now (will be cast to Guard if before |)
      // Use ~functor convention if negated (will be detected during Guard conversion)
      final functor = negated ? '~${functorToken.lexeme}' : functorToken.lexeme;
      final goal = Goal(functor, args, negated ? negLine : functorToken.line, negated ? negColumn : functorToken.column);

      // Check for spawn annotation: Goal@AgentId
      if (_match(TokenType.AT)) {
        final agentToken = _consume(TokenType.ATOM, 'Expected agent identifier after @');
        return SpawnGoal(goal, agentToken.lexeme, functorToken.line, functorToken.column);
      }

      return goal;
    }

    // Otherwise, try to parse as infix comparison (e.g., X < Y, X? mod P? =:= 0)
    // Use _parseExpression(6) to parse arithmetic but stop at comparison operators
    final left = _parseExpression(6);

    // Check for comparison operator
    if (_check(TokenType.LESS) || _check(TokenType.GREATER) ||
        _check(TokenType.LESS_EQUAL) || _check(TokenType.GREATER_EQUAL) ||
        _check(TokenType.EQUALS) || _check(TokenType.ARITH_EQUAL) ||
        _check(TokenType.ARITH_NOT_EQUAL) || _check(TokenType.GROUND_EQUAL)) {
      final opToken = _advance();
      final right = _parseExpression(6);

      // Transform infix to prefix: X < Y → <(X, Y)
      // For negation: ~(X =?= Y) → use ~=?= functor convention
      final functor = negated ? '~${opToken.lexeme}' : opToken.lexeme;
      return Goal(functor, [left, right], negated ? negLine : opToken.line, negated ? negColumn : opToken.column);
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

  // Atom: functor(arg1, arg2, ...) or Var := Expr or Var =.. Expr (for clause heads)
  Atom _parseAtom() {
    // Check for := or =.. pattern: Var := Expr or Var =.. Expr or _ := Expr
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER) || _check(TokenType.UNDERSCORE)) {
      final varToken = _advance();
      final isReader = varToken.type == TokenType.READER;
      final isUnderscore = varToken.type == TokenType.UNDERSCORE;
      if (_match(TokenType.ASSIGN)) {
        // Parse as ':='(Var, Expr) or ':='(_, Expr)
        final lhsTerm = isUnderscore
            ? UnderscoreTerm(varToken.line, varToken.column)
            : VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Atom(':=', [lhsTerm, expr], varToken.line, varToken.column);
      } else if (_match(TokenType.UNIV)) {
        // Parse as '=..'(Var, Expr)
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Atom('=..', [varTerm, expr], varToken.line, varToken.column);
      } else if (_match(TokenType.UNIV_DECOMPOSE)) {
        // Parse as '..='(Var, Expr)
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Atom('..=', [varTerm, expr], varToken.line, varToken.column);
      } else if (_match(TokenType.EQUALS)) {
        // Parse as '='(Var, Term) - unification
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final term = _parseTerm();
        return Atom('=', [varTerm, term], varToken.line, varToken.column);
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

    // Check if this is followed by =.. (e.g., foo(a,b) =.. L)
    if (_match(TokenType.UNIV)) {
      // Convert the already-parsed atom to a StructTerm
      final leftTerm = StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
      final rightTerm = _parseTerm();
      return Atom('=..', [leftTerm, rightTerm], functorToken.line, functorToken.column);
    }

    // Check if this is followed by = (e.g., foo = bar, foo(a) = X)
    if (_match(TokenType.EQUALS)) {
      final leftTerm = args.isEmpty
          ? ConstTerm(functorToken.lexeme, functorToken.line, functorToken.column)
          : StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
      final rightTerm = _parseTerm();
      return Atom('=', [leftTerm, rightTerm], functorToken.line, functorToken.column);
    }

    return Atom(functorToken.lexeme, args, functorToken.line, functorToken.column);
  }

  // Goal: same as Atom, or assignment (Var := Expr) or univ (Var =.. Expr)
  // Also handles remote goals: Module # Goal
  Goal _parseGoal() {
    // Check for assignment or univ: Var := Expr or Var =.. Expr
    // Also check for dynamic remote goal: Var # Goal
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
      final varToken = _advance();
      final isReader = varToken.type == TokenType.READER;

      // Check for dynamic remote goal: Var # Goal (e.g., M # factorial(5, R))
      if (_match(TokenType.HASH)) {
        final moduleTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final innerGoal = _parseGoal();
        return RemoteGoal(moduleTerm, innerGoal, varToken.line, varToken.column);
      } else if (_match(TokenType.ASSIGN)) {
        // Parse as ':='(Var, Expr)
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Goal(':=', [varTerm, expr], varToken.line, varToken.column);
      } else if (_match(TokenType.UNIV)) {
        // Parse as '=..'(Var, Expr)
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Goal('=..', [varTerm, expr], varToken.line, varToken.column);
      } else if (_match(TokenType.UNIV_DECOMPOSE)) {
        // Parse as '..='(Var, Expr)
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final expr = _parseTerm();
        return Goal('..=', [varTerm, expr], varToken.line, varToken.column);
      } else if (_match(TokenType.EQUALS)) {
        // Parse as '='(Var, Term) - unification
        final varTerm = VarTerm(varToken.lexeme, isReader, varToken.line, varToken.column);
        final term = _parseTerm();
        return Goal('=', [varTerm, term], varToken.line, varToken.column);
      } else {
        // Not an assignment or univ - this is an error in goal position
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

    // Check for static remote goal: Module # Goal (e.g., math # factorial(5, R))
    if (_match(TokenType.HASH)) {
      // Module name cannot have arguments
      if (args.isNotEmpty) {
        throw CompileError(
          'Module name cannot have arguments: ${functorToken.lexeme}',
          functorToken.line,
          functorToken.column,
          phase: 'parser'
        );
      }
      final moduleTerm = ConstTerm(functorToken.lexeme, functorToken.line, functorToken.column);
      final innerGoal = _parseGoal();
      return RemoteGoal(moduleTerm, innerGoal, functorToken.line, functorToken.column);
    }

    // Check if this is followed by =.. (e.g., foo(a,b) =.. L)
    if (_match(TokenType.UNIV)) {
      // Convert the already-parsed atom to a StructTerm
      final leftTerm = StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
      final rightTerm = _parseTerm();
      return Goal('=..', [leftTerm, rightTerm], functorToken.line, functorToken.column);
    }

    final goal = Goal(functorToken.lexeme, args, functorToken.line, functorToken.column);

    // Check for spawn annotation: Goal@AgentId
    if (_match(TokenType.AT)) {
      final agentToken = _consume(TokenType.ATOM, 'Expected agent identifier after @');
      return SpawnGoal(goal, agentToken.lexeme, functorToken.line, functorToken.column);
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
    // Operator as functor (for type definitions like Exp ::= +(Exp?, Exp?))
    // Must check BEFORE unary minus so -(X,Y) is parsed as struct, not neg((X,Y))
    if (_check(TokenType.PLUS) || _check(TokenType.MINUS) || _check(TokenType.STAR) ||
        _check(TokenType.SLASH) || _check(TokenType.SLASH_SLASH) || _check(TokenType.MOD)) {
      // Look ahead: if followed by (, treat as functor
      if (_current + 1 < tokens.length && tokens[_current + 1].type == TokenType.LPAREN) {
        final functorToken = _advance();
        _advance();  // consume (
        final args = <Term>[];
        if (!_check(TokenType.RPAREN)) {
          args.add(_parseExpression());
          while (_match(TokenType.COMMA)) {
            args.add(_parseExpression());
          }
        }
        _consume(TokenType.RPAREN, 'Expected ")" after operator struct arguments');
        return StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
      }
      // Otherwise fall through - will be handled as unary minus or infix operator
    }

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

    // Underscore (anonymous variable) - can have reader mark: _ or _?
    if (_match(TokenType.UNDERSCORE)) {
      final token = _previous();
      final isReader = _match(TokenType.QUESTION);
      return UnderscoreTerm(token.line, token.column, isReader: isReader);
    }

    // Number
    if (_check(TokenType.NUMBER)) {
      final token = _advance();
      // Check for invalid reader mark on number
      if (_check(TokenType.QUESTION)) {
        throw CompileError(
          'Reader mark "?" can only be applied to variables, not numbers',
          _peek().line,
          _peek().column,
          phase: 'parser'
        );
      }
      return ConstTerm(token.literal, token.line, token.column);
    }

    // String - preserve quotes for type checking string detection
    if (_check(TokenType.STRING)) {
      final token = _advance();
      // Check for invalid reader mark on string
      if (_check(TokenType.QUESTION)) {
        throw CompileError(
          'Reader mark "?" can only be applied to variables, not strings',
          _peek().line,
          _peek().column,
          phase: 'parser'
        );
      }
      // Wrap in quotes so type checker can distinguish strings from atoms
      return ConstTerm('"${token.literal}"', token.line, token.column);
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

        // Check for invalid reader mark on structure
        if (_check(TokenType.QUESTION)) {
          throw CompileError(
            'Reader mark "?" can only be applied to variables, not structures like ${functorToken.lexeme}(...)',
            _peek().line,
            _peek().column,
            phase: 'parser'
          );
        }

        return StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
      } else {
        // Constant atom - check for invalid reader mark
        if (_check(TokenType.QUESTION)) {
          throw CompileError(
            'Reader mark "?" can only be applied to variables, not constants like "${functorToken.lexeme}"',
            _peek().line,
            _peek().column,
            phase: 'parser'
          );
        }
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

  // Check if token is an arithmetic operator, # (module operator), or \ (difference list)
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
           token.type == TokenType.ARITH_NOT_EQUAL ||
           token.type == TokenType.HASH ||
           token.type == TokenType.BACKSLASH;
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
      case TokenType.HASH:
        return 2;   // Module operator (very low, so M # foo(X,Y) parses correctly)
      case TokenType.BACKSLASH:
        return 1;   // Difference list operator (lowest, so [H|T]\T parses correctly)
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
      case TokenType.HASH:
        return '#';
      case TokenType.BACKSLASH:
        return '\\';
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
      // Check for invalid reader mark on list
      if (_check(TokenType.QUESTION)) {
        throw CompileError(
          'Reader mark "?" can only be applied to variables, not lists',
          _peek().line,
          _peek().column,
          phase: 'parser'
        );
      }
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

      // Check for invalid reader mark on list
      if (_check(TokenType.QUESTION)) {
        throw CompileError(
          'Reader mark "?" can only be applied to variables, not lists',
          _peek().line,
          _peek().column,
          phase: 'parser'
        );
      }

      // Build right-associative list: [X,Y,Z|T] -> [X|[Y|[Z|T]]]
      Term result = tail;
      for (int i = elements.length - 1; i >= 0; i--) {
        result = ListTerm(elements[i], result, bracketToken.line, bracketToken.column);
      }
      return result;
    }

    _consume(TokenType.RBRACKET, 'Expected "]" after list elements');

    // Check for invalid reader mark on list
    if (_check(TokenType.QUESTION)) {
      throw CompileError(
        'Reader mark "?" can only be applied to variables, not lists',
        _peek().line,
        _peek().column,
        phase: 'parser'
      );
    }

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

  // ============================================================================
  // Yardeni-Shapiro Type Declaration Parser Methods
  // ============================================================================

  /// Check if we're at a type definition or procedure declaration
  bool _isTypeOrProcDeclaration() {
    // procedure keyword
    if (_check(TokenType.PROCEDURE)) return true;

    // TypeName ::= ... (type names are capitalized, tokenized as VARIABLE)
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
      // Look ahead for ::=
      final saved = _current;
      _advance();  // consume type name

      final isTypeDef = _check(TokenType.COLONCOLONEQ);

      _current = saved;  // restore position
      return isTypeDef;
    }

    return false;
  }

  /// Check if we're at a type definition (TypeName ::= ... or TypeName(X) ::= ...)
  /// Used to distinguish type definitions from clause heads starting with capitalized variable.
  bool _isTypeDefinition() {
    // TypeName ::= ... (type names are capitalized, tokenized as VARIABLE)
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
      // Look ahead for ::=, skipping optional type parameters (X, Y, ...)
      final saved = _current;
      _advance();  // consume type name

      // Skip optional type parameters: (X, Y, ...)
      if (_check(TokenType.LPAREN)) {
        _advance(); // consume (
        int depth = 1;
        while (!_isAtEnd() && depth > 0) {
          if (_check(TokenType.LPAREN)) depth++;
          if (_check(TokenType.RPAREN)) depth--;
          _advance();
        }
      }

      final isTypeDef = _check(TokenType.COLONCOLONEQ);

      _current = saved;  // restore position
      return isTypeDef;
    }

    return false;
  }

  /// Parse a type definition: TypeName ::= alt ; alt ; alt.
  /// Also supports parameterized: TypeName(X, Y) ::= alt ; alt.
  /// Also supports explicit dual definitions: TypeName? ::= alt.
  TypeDef _parseTypeDef() {
    final typeNameToken = _check(TokenType.READER)
        ? _advance()
        : _consume(TokenType.VARIABLE, 'Expected type name');

    // For READER tokens (e.g., Channel?), append '?' to the name
    // This supports explicit dual type definitions
    final typeName = typeNameToken.type == TokenType.READER
        ? '${typeNameToken.lexeme}?'
        : typeNameToken.lexeme;
    final line = typeNameToken.line;
    final column = typeNameToken.column;

    // Parse optional type parameters: (X, Y, ...)
    final typeParams = <String>[];
    if (_match(TokenType.LPAREN)) {
      final firstParam = _consume(TokenType.VARIABLE, 'Expected type parameter name');
      typeParams.add(firstParam.lexeme);
      while (_match(TokenType.COMMA)) {
        final param = _consume(TokenType.VARIABLE, 'Expected type parameter name');
        typeParams.add(param.lexeme);
      }
      _consume(TokenType.RPAREN, 'Expected ")" after type parameters');
    }

    _consume(TokenType.COLONCOLONEQ, 'Expected "::=" in type definition');

    // Parse alternatives separated by ;
    final alternatives = <TypeExpr>[];
    alternatives.add(_parseTypeAlt());

    while (_match(TokenType.SEMICOLON)) {
      alternatives.add(_parseTypeAlt());
    }

    _consume(TokenType.DOT, 'Expected "." after type definition');

    return TypeDef(typeName, alternatives, line, column, typeParams: typeParams);
  }

  /// Parse a single type alternative using unified term parsing.
  /// Per spec (type-conversion.md): Parse as Term, then convert to TypeExpr.
  /// 
  /// For explicit dual definitions like `Channel? ::= ch(Stream?, Stream)?.`,
  /// the trailing `?` on the structure is allowed and consumed. The duality
  /// is captured in the type name (Channel?), so the trailing `?` is
  /// documentation that confirms the definition is for the dual form.
  TypeExpr _parseTypeAlt() {
    final term = _parseTypeAltTerm();
    return termToTypeExpr(term);
  }

  /// Parse a term in type alternative context.
  /// Similar to _parseTerm() but allows trailing `?` on structures.
  Term _parseTypeAltTerm() {
    return _parseTypeAltExpression();
  }

  /// Parse expression in type alternative context.
  /// Handles operators like \ for difference lists.
  Term _parseTypeAltExpression([int minPrecedence = 0]) {
    var left = _parseTypeAltPrimary();

    while (_isOperator(_peek()) && _precedence(_peek()) >= minPrecedence) {
      final op = _advance();
      final right = _parseTypeAltExpression(_precedence(op) + 1);
      left = StructTerm(_operatorFunctor(op), [left, right], op.line, op.column);
    }

    // Check for trailing ? on the whole expression (for explicit duals)
    // This is allowed in type definitions and simply consumed
    _match(TokenType.QUESTION);

    return left;
  }

  /// Parse primary term in type alternative context.
  /// Allows trailing `?` on structures (for explicit dual definitions).
  Term _parseTypeAltPrimary() {
    // Operator as functor (for type definitions like Exp ::= +(Exp?, Exp?))
    if (_check(TokenType.PLUS) || _check(TokenType.MINUS) || _check(TokenType.STAR) ||
        _check(TokenType.SLASH) || _check(TokenType.SLASH_SLASH) || _check(TokenType.MOD)) {
      if (_current + 1 < tokens.length && tokens[_current + 1].type == TokenType.LPAREN) {
        final functorToken = _advance();
        _advance();  // consume (
        final args = <Term>[];
        if (!_check(TokenType.RPAREN)) {
          args.add(_parseTypeAltExpression());
          while (_match(TokenType.COMMA)) {
            args.add(_parseTypeAltExpression());
          }
        }
        _consume(TokenType.RPAREN, 'Expected ")" after operator struct arguments');
        // Allow trailing ? on structure in type definitions
        _match(TokenType.QUESTION);
        return StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
      }
    }

    // Parameterized type reference in type body: TypeName(Arg1, Arg2, ...)
    // Uppercase names followed by ( are parameterized type refs, not structs.
    // Encode reader mode in functor name for type_conversion to decode.
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
      final effectiveName = (isReader || trailingQ) ? '${token.lexeme}?' : token.lexeme;
      return StructTerm(effectiveName, args, token.line, token.column);
    }

    // Variable or Reader (simple, non-parameterized)
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
      final token = _advance();
      final isReader = token.type == TokenType.READER;
      return VarTerm(token.lexeme, isReader, token.line, token.column);
    }

    // Underscore (anonymous variable) - can have reader mark: _ or _?
    if (_match(TokenType.UNDERSCORE)) {
      final token = _previous();
      final isReader = _match(TokenType.QUESTION);
      return UnderscoreTerm(token.line, token.column, isReader: isReader);
    }

    // Number
    if (_check(TokenType.NUMBER)) {
      final token = _advance();
      return ConstTerm(token.literal, token.line, token.column);
    }

    // String
    if (_check(TokenType.STRING)) {
      final token = _advance();
      return ConstTerm('"${token.literal}"', token.line, token.column);
    }

    // List
    if (_check(TokenType.LBRACKET)) {
      return _parseTypeAltList();
    }

    // Parenthesized expression or tuple
    if (_match(TokenType.LPAREN)) {
      final startToken = _previous();
      final terms = <Term>[];
      terms.add(_parseTypeAltExpression());

      if (_match(TokenType.COMMA)) {
        terms.add(_parseTypeAltExpression());
        while (_match(TokenType.COMMA)) {
          terms.add(_parseTypeAltExpression());
        }
        _consume(TokenType.RPAREN, 'Expected ")" after tuple');
        Term result = terms.last;
        for (int i = terms.length - 2; i >= 0; i--) {
          result = StructTerm(',', [terms[i], result], startToken.line, startToken.column);
        }
        // Allow trailing ? on parenthesized expression
        _match(TokenType.QUESTION);
        return result;
      } else {
        _consume(TokenType.RPAREN, 'Expected ")" after expression');
        // Allow trailing ? on parenthesized expression
        _match(TokenType.QUESTION);
        return terms[0];
      }
    }

    // Structure or Constant Atom
    if (_check(TokenType.ATOM)) {
      final functorToken = _advance();

      if (_match(TokenType.LPAREN)) {
        final args = <Term>[];
        if (!_check(TokenType.RPAREN)) {
          args.add(_parseTypeAltExpression());
          while (_match(TokenType.COMMA)) {
            args.add(_parseTypeAltExpression());
          }
        }
        _consume(TokenType.RPAREN, 'Expected ")" after structure arguments');
        // Allow trailing ? on structure in type definitions (for explicit duals)
        _match(TokenType.QUESTION);
        return StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
      } else {
        return ConstTerm(functorToken.lexeme, functorToken.line, functorToken.column);
      }
    }

    throw CompileError(
      'Expected type alternative term, got ${_peek().type}',
      _peek().line,
      _peek().column,
      phase: 'parser'
    );
  }

  /// Parse list in type alternative context.
  /// Allows trailing ? on lists (for explicit duals).
  Term _parseTypeAltList() {
    final bracketToken = _consume(TokenType.LBRACKET, 'Expected "["');

    if (_match(TokenType.RBRACKET)) {
      // Allow trailing ? on empty list in type definitions
      _match(TokenType.QUESTION);
      return ListTerm(null, null, bracketToken.line, bracketToken.column);
    }

    final elements = <Term>[];
    Term? tail;

    elements.add(_parseTypeAltTerm());

    while (_match(TokenType.COMMA)) {
      elements.add(_parseTypeAltTerm());
    }

    if (_match(TokenType.PIPE)) {
      tail = _parseTypeAltTerm();
      _consume(TokenType.RBRACKET, 'Expected "]" after list tail');
      // Allow trailing ? on list in type definitions
      _match(TokenType.QUESTION);
      Term result = tail;
      for (int i = elements.length - 1; i >= 0; i--) {
        result = ListTerm(elements[i], result, bracketToken.line, bracketToken.column);
      }
      return result;
    }

    _consume(TokenType.RBRACKET, 'Expected "]" after list elements');
    // Allow trailing ? on list in type definitions
    _match(TokenType.QUESTION);

    Term result = ListTerm(null, null, bracketToken.line, bracketToken.column);
    for (int i = elements.length - 1; i >= 0; i--) {
      result = ListTerm(elements[i], result, bracketToken.line, bracketToken.column);
    }
    return result;
  }

  /// Parse a procedure declaration: procedure name(Type?, Type).
  /// or: exported procedure name(Type?, Type).
  /// or: imported procedure [path#]name(Type?, Type).
  ProcDecl _parseProcDeclaration() {
    // Check for 'exported' or 'imported' keyword before 'procedure'
    bool exported = false;
    bool imported = false;
    final startLine = _peek().line;
    final startColumn = _peek().column;
    if (_check(TokenType.ATOM) && _peek().lexeme == 'exported') {
      _advance(); // consume 'exported'
      exported = true;
    } else if (_check(TokenType.ATOM) && _peek().lexeme == 'imported') {
      _advance(); // consume 'imported'
      imported = true;
    }
    _consume(TokenType.PROCEDURE, 'Expected "procedure" keyword');
    final line = startLine;
    final column = startColumn;

    // Parse procedure name, possibly with module path for imported procedures.
    // For imported: 'social#agent' → modulePath='social', name='agent'
    //              'ui#actors#render' → modulePath='ui#actors', name='render'
    //              'merge' → modulePath=null, name='merge'
    String? modulePath;

    // Procedure name can be atom or operator (<, >, =<, >=, =:=, =\=, =?=, =)
    Token nameToken;
    if (_check(TokenType.ATOM)) {
      nameToken = _advance();
    } else if (_check(TokenType.LESS)) {
      nameToken = _advance();
    } else if (_check(TokenType.GREATER)) {
      nameToken = _advance();
    } else if (_check(TokenType.LESS_EQUAL)) {
      nameToken = _advance();
    } else if (_check(TokenType.GREATER_EQUAL)) {
      nameToken = _advance();
    } else if (_check(TokenType.ARITH_EQUAL)) {
      nameToken = _advance();
    } else if (_check(TokenType.ARITH_NOT_EQUAL)) {
      nameToken = _advance();
    } else if (_check(TokenType.GROUND_EQUAL)) {
      nameToken = _advance();
    } else if (_check(TokenType.EQUALS)) {
      nameToken = _advance();
    } else if (_check(TokenType.UNIV)) {
      nameToken = _advance();
    } else if (_check(TokenType.UNIV_DECOMPOSE)) {
      nameToken = _advance();
    } else if (_check(TokenType.ASSIGN)) {
      nameToken = _advance();
    } else {
      throw CompileError(
        'Expected procedure name',
        _peek().line,
        _peek().column,
        phase: 'parser',
      );
    }

    // For imported procedures, parse #-separated path: social#agent, ui#actors#render
    // The last component is the procedure name, everything before is the module path.
    var name = nameToken.lexeme;
    if (imported) {
      final parts = <String>[name];
      while (_match(TokenType.HASH)) {
        // Next token should be an atom (next path component or procedure name)
        if (!_check(TokenType.ATOM)) {
          throw CompileError(
            'Expected module path component or procedure name after "#"',
            _peek().line,
            _peek().column,
            phase: 'parser',
          );
        }
        parts.add(_advance().lexeme);
      }
      // Last part is the procedure name, rest is the module path
      name = parts.last;
      if (parts.length > 1) {
        modulePath = parts.sublist(0, parts.length - 1).join('#');
      }
    }

    // Parentheses are optional for nullary procedures:
    // procedure play_introduction.    (valid - nullary)
    // procedure play_introduction().  (valid - nullary with explicit parens)
    // procedure double(Number?, Number). (valid - with args)
    final argTypes = <TypeExpr>[];
    if (_match(TokenType.LPAREN)) {
      // Parse argument types if not empty
      if (!_check(TokenType.RPAREN)) {
        argTypes.add(_parseProcArgType());
        while (_match(TokenType.COMMA)) {
          argTypes.add(_parseProcArgType());
        }
      }
      _consume(TokenType.RPAREN, 'Expected ")" after procedure arguments');
    }
    // If no LPAREN, argTypes remains empty (nullary procedure)

    _consume(TokenType.DOT, 'Expected "." after procedure declaration');

    return ProcDecl(name, argTypes, line, column, exported: exported, imported: imported, modulePath: modulePath);
  }

  /// Parse a procedure argument type: TypeName, TypeName?, _, _?,
  /// or qualified: mod#TypeName, mod#TypeName?
  TypeExpr _parseProcArgType() {
    final line = _peek().line;
    final column = _peek().column;

    // Primitive: _ or _?
    if (_match(TokenType.UNDERSCORE)) {
      final isInput = _match(TokenType.QUESTION);
      return PrimitiveModeAlt(isInput, line, column);
    }

    // Qualified type reference: atom # TypeName or atom # TypeName?
    // e.g., social#AgentChannel, social#AgentChannel?
    if (_check(TokenType.ATOM) && _current + 1 < tokens.length && tokens[_current + 1].type == TokenType.HASH) {
      // Collect path: atom # atom # ... # TypeName
      final pathParts = <String>[];
      while (_check(TokenType.ATOM) && _current + 1 < tokens.length && tokens[_current + 1].type == TokenType.HASH) {
        pathParts.add(_advance().lexeme); // consume atom
        _advance(); // consume #
      }
      // Now parse the final type name (must be VARIABLE or READER)
      if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
        final typeToken = _advance();
        final isInput = typeToken.type == TokenType.READER || _match(TokenType.QUESTION);
        final qualifiedName = '${pathParts.join('#')}#${typeToken.lexeme}';
        return TypeRef(qualifiedName, line, column, isInput: isInput);
      }
      throw CompileError(
        'Expected type name after module path in qualified type reference',
        _peek().line,
        _peek().column,
        phase: 'parser',
      );
    }

    // Type reference with optional type arguments and optional mode
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
      final token = _advance();
      final baseName = token.lexeme;

      // Parse optional type arguments: (Type1, Type2, ...)
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
}
