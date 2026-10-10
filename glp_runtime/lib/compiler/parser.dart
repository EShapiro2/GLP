import 'token.dart';
import 'ast.dart';
import 'error.dart';
import '../analysis/type_checker/type_ast.dart';
import '../analysis/type_checker/type_conversion.dart';
import '../analysis/type_checker/root_scope.dart' show builtinProcedures;

/// Parser for GLP source code
class Parser {
  final List<Token> tokens;
  int _current = 0;
  Clause? _pendingClause;  // Clause parsed but belongs to different procedure

  /// Read a .vglp source (vGLP, sections/vglp.tex, Definition "Guarded
  /// Clause, Volitional Procedure, Interactive Type, Interactive Term,
  /// Ordinary Clause, Procedure, vGLP Program"): a volitional procedure p of
  /// arity n "is declared `procedure (T)*p(T1, ..., Tn).`", T its interactive
  /// type, in writer or reader mode as an argument type is, and "a clause of it
  /// has the form `(A)*p(S1, ..., Sn) :- G | B`, where the interactive term A
  /// is a term of type T, possibly a variable but not the anonymous variable,
  /// and it is the guarded clause `p(S1, ..., Sn, A) :- G | B`, of arity n+1".
  /// The parser reads each as what the Definition says it is: the declaration
  /// as `p(T1, ..., Tn, T)`, listed in [Module.volitionalDeclarations], and the
  /// clause as that guarded clause, its interactive term marked
  /// ([Clause.interactiveTerm]).  The declaration takes TGLP's `exported` and
  /// parameter list as any procedure declaration does ("vGLP is typed as GLP
  /// is, by the parameterised moded type system of [TGLP] ... not restated
  /// here", vGLP Section "Volition-Guarded GLP"), and `imported` as well: an
  /// import mirrors its export's declaration (TGLP modules.tex, "Self-contained
  /// type checking": "Every module declares the full moded type of every
  /// cross-module procedure it calls, via imported procedure declarations"),
  /// so `imported procedure (T)*M#p(T1, ..., Tn).` is read as its export is,
  /// `M#p(T1, ..., Tn, T)` (GLP #3 Cowork, 2026-10-10 07:48 UTC, "19:55":
  /// "a volitional export is imported as it is declared").  Until 2026-10-10
  /// it was refused.
  ///
  /// It also admits the volition guards and else-branches of the Definition
  /// that one replaced ("Guarded Clause, Volition-Guarded Clause, ..."), in
  /// which the .vglp sources not yet in the Definition's syntax are written.
  ///
  /// False for a .glp source: "GLP is vGLP without volitional procedures"
  /// (vGLP Section "Volition-Guarded GLP"), and either syntax in one is a parse
  /// error, not silently ignored.
  final bool vglp;

  Parser(this.tokens, {this.vglp = false});

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
    CompileMode compileMode = CompileMode.user;  // default: user mode
    final exposes = <String>[];  // `-expose(M).` module paths

    // Parse declarations at the start of the file
    declarations:
    while (!_isAtEnd() && _check(TokenType.MINUS)) {
      final startPos = _current;
      _advance(); // consume '-'

      if (!_check(TokenType.ATOM)) {
        _current = startPos;  // Back up, not a declaration
        break;
      }

      final keyword = _advance();

      switch (keyword.lexeme) {
        case 'mode':
          // -mode(system). declaration (GLP-Spec appendix-guards.tex, Naming and
          // admission of body kernels; TGLP app:system-mode), the one mode
          // declaration: a module without it is a user module.
          _consume(TokenType.LPAREN, 'Expected "(" after mode');
          if (!_check(TokenType.ATOM)) {
            throw CompileError(
              'Expected "system" in mode declaration',
              _peek().line,
              _peek().column,
              phase: 'parser'
            );
          }
          final modeToken = _advance();
          if (modeToken.lexeme != 'system') {
            throw CompileError(
              'Invalid mode "${modeToken.lexeme}". The mode declaration is '
              '-mode(system); a module without it is a user module.',
              modeToken.line,
              modeToken.column,
              phase: 'parser'
            );
          }
          compileMode = CompileMode.system;
          _consume(TokenType.RPAREN, 'Expected ")" after mode');
          _consume(TokenType.DOT, 'Expected "." after mode declaration');
          break;

        case 'expose':
          // -expose(a#b#c). — lift module file <self.glp dir>/a/b/c.glp's
          // exported procedures into this directory's scope.
          _consume(TokenType.LPAREN, 'Expected "(" after expose');
          if (!_check(TokenType.ATOM)) {
            throw CompileError('Expected a module path in -expose(...)',
                _peek().line, _peek().column, phase: 'parser');
          }
          final exposeParts = <String>[_advance().lexeme];
          while (_match(TokenType.HASH)) {
            if (!_check(TokenType.ATOM)) {
              throw CompileError(
                  'Expected module path component after "#" in -expose(...)',
                  _peek().line, _peek().column, phase: 'parser');
            }
            exposeParts.add(_advance().lexeme);
          }
          _consume(TokenType.RPAREN, 'Expected ")" after expose path');
          _consume(TokenType.DOT, 'Expected "." after expose declaration');
          exposes.add(exposeParts.join('#'));
          break;

        default:
          // Not a directive: back up to the '-' and leave the directives, and
          // the loop below refuses it as an unexpected token.  A bare `break`
          // here left the switch and not the loop, which met the same '-'
          // again and never ended.
          _current = startPos;
          break declarations;
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
    final displayDecls = <DisplayDecl>[];
    final volitionalDecls = <VolitionalDeclaration>[];

    // Track pending procedure declaration (waiting for its first clause)
    ProcDecl? pendingProcDecl;
    // Whether the pending declaration is a volitional procedure's.
    var pendingVolitional = false;
    // Track which procedures we've seen clauses for (signature -> first Procedure)
    final seenProcedures = <String, Procedure>{};

    while (!_isAtEnd()) {
      if (!vglp) _refuseVolitionalSyntax();

      // A procedure declaration: 'procedure ...', 'exported procedure ...' or
      // 'imported procedure ...', a name after the keyword and its parameter
      // list ([_atProcDeclaration]); any other item beginning `procedure` is a
      // clause of the procedure of that name.
      final isProcedureDecl = _atProcDeclaration();

      if (_atDisplayDecl()) {
        // A display declaration is a declaration, not a clause, so it does not
        // break the run of clauses a pending procedure declaration is waiting
        // for; it may stand anywhere a type definition may.
        displayDecls.add(_parseDisplayDecl());
      } else if (isProcedureDecl) {
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
        final volitional = vglp && _interactiveDeclarationAt(_current) != null;
        final decl = _parseProcDeclaration();
        procDeclarations.add(decl);
        if (volitional) volitionalDecls.add(VolitionalDeclaration(decl));
        // Imported procedures are declaration-only — no clauses expected
        if (!decl.imported) {
          pendingProcDecl = decl;
          pendingVolitional = volitional;
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
          var declaredVolitional = false;

          // Check if this matches pending declaration
          if (pendingProcDecl != null) {
            final pendingSig = '${pendingProcDecl.name}/${pendingProcDecl.argTypes.length}';
            if (sig == pendingSig) {
              // This clause matches the pending declaration - good
              pendingProcDecl = null;
              declaredVolitional = pendingVolitional;
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
          _checkInteractiveTerms(proc, declaredVolitional);

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
      } else if (_check(TokenType.ATOM) || _check(TokenType.PROCEDURE) ||
          (vglp && (_check(TokenType.STAR) || _check(TokenType.LPAREN)))) {
        // Clause starting with an atom (procedure name), `procedure` among
        // them where no declaration begins there, or, in a .vglp source, with
        // the interactive term `(A)*` of a clause of a volitional procedure or
        // the volition guard `*(...)` preceding one.
        final proc = _parseProcedure();
        final sig = '${proc.name}/${proc.arity}';
        var declaredVolitional = false;

        // Check if this matches pending declaration
        if (pendingProcDecl != null) {
          final pendingSig = '${pendingProcDecl.name}/${pendingProcDecl.argTypes.length}';
          if (sig == pendingSig) {
            // This clause matches the pending declaration - good
            pendingProcDecl = null;
            declaredVolitional = pendingVolitional;
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
        _checkInteractiveTerms(proc, declaredVolitional);

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
      typeDefs: typeDefs,
      procDeclarations: procDeclarations,
      procedures: procedures,
      compileMode: compileMode,
      exposes: exposes,
      displayDecls: displayDecls,
      volitionalDeclarations: volitionalDecls,
      line: 1,
      column: 1,
    );
  }

  /// The clauses of [proc] against its declaration, [volitional] where it is
  /// `procedure (T)*p(T1, ..., Tn).` (vGLP, Definition "Guarded Clause,
  /// Volitional Procedure, ..."): "a clause of it has the form
  /// `(A)*p(S1, ..., Sn) :- G | B`", so there every clause is written so; and
  /// a clause written so is of a procedure declared so, which "is declared
  /// `procedure (T)*p(T1, ..., Tn).`", T the type of A, so elsewhere none is.
  void _checkInteractiveTerms(Procedure proc, bool volitional) {
    final n = proc.arity - 1;
    for (final c in proc.clauses) {
      if (volitional && c.interactiveTerm == null) {
        throw CompileError(
          'A clause of the volitional procedure ${proc.name}/$n is written '
          '"(A)*${proc.name}(S1, ..., Sn) :- G | B", with its interactive '
          'term (vGLP, Definition "Guarded Clause, Volitional Procedure, ...")',
          c.line, c.column, phase: 'parser');
      }
      if (!volitional && c.interactiveTerm != null) {
        throw CompileError(
          'The clause (A)*${proc.name}/$n is of no procedure declared '
          '"procedure (T)*${proc.name}(T1, ..., Tn)." immediately before its '
          'clauses: a volitional procedure is declared with its interactive '
          'type (vGLP, Definition "Guarded Clause, Volitional Procedure, ...")',
          c.line, c.column, phase: 'parser');
      }
    }
  }

  /// In a .glp source, the refusal of vGLP's syntax for a volitional
  /// procedure where an item begins with it: "GLP is vGLP without volitional
  /// procedures" (vGLP Section "Volition-Guarded GLP").  Neither form parses
  /// as GLP, the declaration's interactive type standing where a name is
  /// expected and the clause's interactive term where a head is.
  void _refuseVolitionalSyntax() {
    final declaration = _interactiveDeclarationAt(_current) != null;
    if (!declaration && _indexAfterInteractiveTerm(_current) == _current) {
      return;
    }
    throw CompileError(
      '${declaration ? 'The declaration "procedure (T)*p(...)"' : 'The clause "(A)*p(...)"'} '
      'of a volitional procedure may appear only in a .vglp source: GLP is '
      'vGLP without volitional procedures (vGLP, Definition "Guarded Clause, '
      'Volitional Procedure, ...")',
      _peek().line, _peek().column, phase: 'parser');
  }

  /// Parse an interface section: type definitions and procedure declarations
  /// alone, with no clauses.
  ///
  /// [parseModule] requires every procedure declaration to be followed by its
  /// clauses. That rule is right for a module and wrong for an interface
  /// section, which carries declarations and no clauses: the artefact's
  /// interface table is the program interface carried as declaration source
  /// text, from which the loader derives the type automata (IGLP appendix
  /// §Program Artefact). This is the entry point that reads that text back.
  ///
  /// Returns a [Module] whose `procedures` is empty. A clause in the input is a
  /// parse error, as is a `-mode`/`-expose` directive: an interface section is
  /// not a module and carries neither.
  Module parseInterface() {
    final typeDefs = <TypeDef>[];
    final procDeclarations = <ProcDecl>[];

    while (!_isAtEnd()) {
      final isProcedureDecl = _atProcDeclaration();

      if (isProcedureDecl) {
        procDeclarations.add(_parseProcDeclaration());
      } else if (_isTypeDefinition()) {
        typeDefs.add(_parseTypeDef());
      } else {
        throw CompileError(
          'Unexpected token "${_peek().lexeme}" in an interface section.\n'
          '  An interface section carries type definitions and procedure '
          'declarations only — no clauses and no directives.',
          _peek().line,
          _peek().column,
          phase: 'parser',
        );
      }
    }

    return Module(
      typeDefs: typeDefs,
      procDeclarations: procDeclarations,
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

      if (['module', 'mode', 'expose'].contains(keyword)) {
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
  // _parseModuleName removed: the -module directive is no longer supported
  // (a module's name is its file/directory path from the program root).

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

      // An interactive term or a volition guard precedes the head, so look
      // past it for the name.
      final headIdx = vglp
          ? _indexAfterVolitionGuard(_indexAfterInteractiveTerm(_current))
          : _current;
      if (headIdx < tokens.length &&
          (tokens[headIdx].type == TokenType.ATOM ||
              tokens[headIdx].type == TokenType.PROCEDURE) &&
          tokens[headIdx].lexeme == name &&
          !_atProcDeclaration(headIdx)) {
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

  /// The index of the parenthesis closing the one at [open], or -1 where none
  /// does before the end.
  int _closingParen(int open) {
    var depth = 0;
    for (var i = open; i < tokens.length; i++) {
      final t = tokens[i].type;
      if (t == TokenType.EOF) return -1;
      if (t == TokenType.LPAREN) depth++;
      if (t == TokenType.RPAREN && --depth == 0) return i;
    }
    return -1;
  }

  /// The index of the first token past the interactive term `(A)*` of a
  /// clause of a volitional procedure beginning at [i], or [i] itself if none
  /// begins there (vGLP, Definition "Guarded Clause, Volitional Procedure,
  /// ...").  Used to look at the head of the clause without parsing it.
  int _indexAfterInteractiveTerm(int i) {
    if (i >= tokens.length || tokens[i].type != TokenType.LPAREN) return i;
    final close = _closingParen(i);
    if (close < 0 ||
        close + 1 >= tokens.length ||
        tokens[close + 1].type != TokenType.STAR) {
      return i;
    }
    return close + 2;
  }

  /// Where a volitional procedure's declaration beginning at [at] carries its
  /// interactive type, `(T)*` before the name (vGLP, Definition "Guarded
  /// Clause, Volitional Procedure, ..."): [open], the index of the
  /// parenthesis before T, and [name], that of the token after `*`.  The
  /// declaration is `procedure (T)*p(...)`, or `procedure(X, ...) (T)*p(...)`
  /// with TGLP's parameter list, either after `exported` or `imported`.  Null
  /// where no such declaration begins at [at].
  ({int open, int name})? _interactiveDeclarationAt(int at) {
    var i = at;
    if (i < tokens.length &&
        tokens[i].type == TokenType.ATOM &&
        (tokens[i].lexeme == 'exported' || tokens[i].lexeme == 'imported')) {
      i++;
    }
    if (i >= tokens.length || tokens[i].type != TokenType.PROCEDURE) {
      return null;
    }
    i++;
    if (i >= tokens.length || tokens[i].type != TokenType.LPAREN) return null;
    final close = _closingParen(i);
    if (close < 0 || close + 1 >= tokens.length) return null;
    if (tokens[close + 1].type == TokenType.STAR) {
      return (open: i, name: close + 2);
    }
    // A parameter list, then the interactive type.
    final open = close + 1;
    if (tokens[open].type != TokenType.LPAREN) return null;
    final close2 = _closingParen(open);
    if (close2 < 0 ||
        close2 + 1 >= tokens.length ||
        tokens[close2 + 1].type != TokenType.STAR) {
      return null;
    }
    return (open: open, name: close2 + 2);
  }

  /// Parse the interactive term `(A)*` of a clause of a volitional procedure
  /// if one precedes the head, in a .vglp source; null where the clause is
  /// ordinary (vGLP, Definition "Guarded Clause, Volitional Procedure, ...").
  /// "The interactive term A is a term of type T, possibly a variable but not
  /// the anonymous variable": `_`, `_?`, `_Name` and `_Name?` are refused, an
  /// anonymous variable being any variable whose name begins with `_`
  /// (GLP-Spec, Remark "Anonymous Variables").
  Term? _parseInteractiveTermOpt() {
    if (!vglp || !_check(TokenType.LPAREN)) return null;
    final open = _advance();
    if (_check(TokenType.RPAREN)) {
      throw CompileError(
        'An empty interactive term "()": a clause of a volitional procedure '
        'is written "(A)*p(S1, ..., Sn) :- G | B", A a term (vGLP, Definition '
        '"Guarded Clause, Volitional Procedure, ...")',
        open.line, open.column, phase: 'parser');
    }
    final term = _parseTerm();
    _consume(TokenType.RPAREN, 'Expected ")" after the interactive term');
    _consume(TokenType.STAR,
        'Expected "*" after the interactive term: a clause of a volitional '
        'procedure is written "(A)*p(S1, ..., Sn) :- G | B"');
    if (term is UnderscoreTerm ||
        (term is VarTerm && term.name.startsWith('_'))) {
      throw CompileError(
        'The interactive term "$term" is the anonymous variable: "the '
        'interactive term A is a term of type T, possibly a variable but not '
        'the anonymous variable" (vGLP, Definition "Guarded Clause, '
        'Volitional Procedure, ..."; GLP-Spec, Remark "Anonymous Variables")',
        term.line, term.column, phase: 'parser');
    }
    if (!_check(TokenType.ATOM) && !_check(TokenType.PROCEDURE)) {
      throw CompileError(
        'Expected the volitional procedure\'s name after "(A)*"',
        _peek().line, _peek().column, phase: 'parser');
    }
    return term;
  }

  /// The index of the first token past a volition guard beginning at [i], or
  /// [i] itself if none begins there.  Used to look at the head of a clause the
  /// guard precedes without parsing it.
  int _indexAfterVolitionGuard(int i) {
    if (i >= tokens.length || tokens[i].type != TokenType.STAR) return i;
    var j = i + 1;
    if (j < tokens.length && tokens[j].type == TokenType.LPAREN) {
      var depth = 0;
      while (j < tokens.length) {
        if (tokens[j].type == TokenType.LPAREN) depth++;
        if (tokens[j].type == TokenType.RPAREN) {
          depth--;
          if (depth == 0) { j++; break; }
        }
        j++;
      }
    }
    return j;
  }

  /// Whether the STAR at the current position opens an else-branch rather than
  /// standing for multiplication.  An else-branch is `*(T'1, ..., T'i) B'`, so
  /// the parenthesised group is followed by a goal; a product `A * (B)` is
  /// followed by an operator, a comma, or the clause's full stop.
  bool _isElseBranchStar() {
    if (!vglp || !_check(TokenType.STAR)) return false;
    if (_current + 1 >= tokens.length ||
        tokens[_current + 1].type != TokenType.LPAREN) return false;
    final after = _indexAfterVolitionGuard(_current);
    if (after >= tokens.length) return false;
    final t = tokens[after].type;
    return t == TokenType.ATOM || t == TokenType.VARIABLE ||
           t == TokenType.READER || t == TokenType.UNDERSCORE;
  }

  /// Parse a volition guard if one precedes the clause (vGLP, Definition
  /// "Guarded Clause, Volition-Guarded Clause, ...").  Returns null where the
  /// clause is ordinary.
  ///
  /// The paper's abbreviations decide each position: `X_l=T_l` is written in
  /// full; a bare writer `X_l` abbreviates `X_l=_`, an anonymous value, which
  /// is a field of the construct; a bare ground term `T_l` abbreviates `_=T_l`,
  /// an anonymous writer; and a reader `Y_l?` is a context position.
  VolitionGuard? _parseVolitionGuardOpt({bool inDisplay = false}) {
    if (!_check(TokenType.STAR)) return null;
    if (!vglp && !inDisplay) {
      throw CompileError(
        'A volition guard "*" may appear only in a .vglp source.\n'
        '  GLP is vGLP without volition-guarded clauses.',
        _peek().line, _peek().column, phase: 'parser');
    }
    final star = _advance();
    final question = <QuestionPosition>[];
    final context = <VarTerm>[];

    // Bare `*` is the volition guard with i = j = 0.
    if (!_match(TokenType.LPAREN)) {
      return VolitionGuard(question, context, star.line, star.column);
    }

    if (!_check(TokenType.RPAREN)) {
      do {
        final term = _parsePrimary();
        if (_match(TokenType.EQUALS)) {
          // X_l = T_l, with either side possibly anonymous.
          final value = _parsePrimary();
          if (term is VarTerm && !term.isReader) {
            question.add(QuestionPosition(
                writer: term,
                value: value is UnderscoreTerm ? null : value));
          } else if (term is UnderscoreTerm && !term.isReader) {
            question.add(QuestionPosition(writer: null, value: value));
          } else {
            throw CompileError(
              'The left side of a volition-guard position must be a writer or "_", got "$term".',
              term.line, term.column, phase: 'parser');
          }
        } else if (term is VarTerm && term.isReader) {
          context.add(term);              // Y_l? — a context position
        } else if (term is VarTerm) {
          question.add(QuestionPosition(writer: term, value: null));  // X_l = _
        } else {
          question.add(QuestionPosition(writer: null, value: term));  // _ = T_l
        }
      } while (_match(TokenType.COMMA));
    }
    _consume(TokenType.RPAREN, 'Expected ")" after volition guard');
    return VolitionGuard(question, context, star.line, star.column);
  }

  /// Whether a display declaration begins at the current position.
  ///
  /// `display` is an ordinary atom, not a keyword, so a procedure may be called
  /// `display`: a clause of one has `(` or `:-` after the name, a declaration
  /// has the predicate or the message pattern, which begins with an atom.
  bool _atDisplayDecl() {
    if (!_check(TokenType.ATOM) || _peek().lexeme != 'display') return false;
    return _current + 1 < tokens.length &&
        tokens[_current + 1].type == TokenType.ATOM;
  }

  /// Parse a display declaration (vGLP, Definition "Display Declaration,
  /// Default Display").  Admitted in a .glp source as well as a .vglp one: the
  /// compiled program carries its declarations unchanged, for the bridge to
  /// read, so GLP must parse what the compilation emits.
  DisplayDecl _parseDisplayDecl() {
    final start = _advance();  // consume 'display'
    final nameToken = _consume(TokenType.ATOM,
        'Expected a predicate or a message pattern after "display"');

    String? predicate;
    VolitionGuard? guard;
    Term? pattern;

    if (_check(TokenType.STAR)) {
      // Clause form: display p *(...) : ...
      predicate = nameToken.lexeme;
      guard = _parseVolitionGuardOpt(inDisplay: true);
    } else {
      // Message form: display m : ...  — m a term, possibly with arguments.
      final args = <Term>[];
      if (_match(TokenType.LPAREN)) {
        if (!_check(TokenType.RPAREN)) {
          args.add(_parseTerm());
          while (_match(TokenType.COMMA)) {
            args.add(_parseTerm());
          }
        }
        _consume(TokenType.RPAREN, 'Expected ")" after the message pattern');
      }
      pattern = args.isEmpty
          ? ConstTerm(nameToken.lexeme, nameToken.line, nameToken.column)
          : StructTerm(nameToken.lexeme, args, nameToken.line, nameToken.column);
    }

    _consume(TokenType.COLON, 'Expected ":" after the subject of a display declaration');

    final items = <DisplayItem>[];
    do {
      final itemToken = _consume(TokenType.ATOM, 'Expected a display item');
      final itemArgs = <Term>[];
      if (_match(TokenType.LPAREN)) {
        if (!_check(TokenType.RPAREN)) {
          itemArgs.add(_parseTerm());
          while (_match(TokenType.COMMA)) {
            itemArgs.add(_parseTerm());
          }
        }
        _consume(TokenType.RPAREN, 'Expected ")" after display item arguments');
      }
      items.add(DisplayItem(itemToken.lexeme, itemArgs,
          itemToken.line, itemToken.column));
    } while (_match(TokenType.COMMA));

    _consume(TokenType.DOT, 'Expected "." at end of display declaration');

    return DisplayDecl(predicate: predicate, guard: guard, pattern: pattern,
        items: items, line: start.line, column: start.column);
  }

  // Clause: Head :- Guards | Body.
  //     or: Head :- Body.
  //     or: Head.
  //
  // A clause of a volitional procedure, (A)*p(S1, ..., Sn) :- G | B, is read
  // as the guarded clause p(S1, ..., Sn, A) :- G | B, of arity n+1, that it is
  // (vGLP, Definition "Guarded Clause, Volitional Procedure, ..."), A marked
  // as its interactive term.
  //
  // A vGLP clause of the Definition that one replaced may be preceded by a
  // volition guard and, if it is, followed by an else-branch before the full
  // stop.
  Clause _parseClause() {
    final volitionGuard = _parseVolitionGuardOpt();
    final interactiveTerm =
        volitionGuard == null ? _parseInteractiveTermOpt() : null;
    final written = _parseAtom();
    final head = interactiveTerm == null
        ? written
        : Atom(written.functor, [...written.args, interactiveTerm],
            written.line, written.column);

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
        guards = predicates
            .map((g) => Guard(g.functor, g.args, g.line, g.column))
            .toList();

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

    // Else-branch: *(T'1, ..., T'i) B', after the body and before the full stop.
    ElseBranch? elseBranch;
    if (_isElseBranchStar()) {
      if (volitionGuard == null) {
        throw CompileError(
          'An else-branch may only follow a volition-guarded clause.',
          _peek().line, _peek().column, phase: 'parser');
      }
      final star = _advance();
      _consume(TokenType.LPAREN, 'Expected "(" after "*" of an else-branch');
      final answer = <Term>[];
      if (!_check(TokenType.RPAREN)) {
        do {
          answer.add(_parsePrimary());
        } while (_match(TokenType.COMMA));
      }
      _consume(TokenType.RPAREN, 'Expected ")" after the else answer');
      if (answer.length != volitionGuard.question.length) {
        throw CompileError(
          'The else answer has ${answer.length} positions, '
          'the clause\'s question ${volitionGuard.question.length}.',
          star.line, star.column, phase: 'parser');
      }
      final elseBody = <Goal>[_parseGoal()];
      while (_match(TokenType.COMMA)) {
        elseBody.add(_parseGoal());
      }
      elseBranch = ElseBranch(answer, elseBody, star.line, star.column);
    }

    _consume(TokenType.DOT, 'Expected "." at end of clause');

    return Clause(head, guards: guards, body: body,
        volitionGuard: volitionGuard, elseBranch: elseBranch,
        interactiveTerm: interactiveTerm,
        line: head.line, column: head.column);
  }

  // Parse a predicate that could be either a guard or a goal
  dynamic _parseGoalOrGuard() {
    // `~` begins no GLP construct.  A guard is a conjunction of guard
    // predicates (GLP-Spec glp.tex, Definition "Guarded Clause"), and guard
    // negation is not part of the language (GLP-Spec 98913b4), so `~G` is
    // refused here, as a syntax error.
    if (_check(TokenType.TILDE)) {
      throw CompileError(
        '"~" is not GLP syntax: a guard is a conjunction of guard predicates, '
        'and there is no guard negation',
        _peek().line,
        _peek().column,
        phase: 'parser'
      );
    }

    // Check for parenthesized expression: (Goal) or (Goal1 ; Goal2)
    if (_check(TokenType.LPAREN)) {
      final startToken = _advance(); // consume '('
      final firstGoal = _parseGoalOrGuard();

      if (_match(TokenType.SEMICOLON)) {
        final secondGoal = _parseGoalOrGuard();
        _consume(TokenType.RPAREN, 'Expected ")" after disjunction');
        // Return as ';'(Goal1, Goal2) - need to convert goals to terms
        final firstTerm = _goalToTerm(firstGoal);
        final secondTerm = _goalToTerm(secondGoal);
        return Goal(';', [firstTerm, secondTerm], startToken.line, startToken.column);
      } else {
        // Parenthesized single goal
        _consume(TokenType.RPAREN, 'Expected ")" after guard');
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
        throw _variableModuleError(varToken);
      }
    }

    // Try to parse as regular predicate first
    if (_check(TokenType.ATOM) || _check(TokenType.PROCEDURE)) {
      final start = _current;
      final functorToken = _consumePredicateName();
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

      // A structure or a constant on the left of an infix guard,
      // `w(X?) =?= Y?` or `f(X?) + 1 > 2`: no predicate, but the left operand,
      // parsed below as an expression, as the right one is.  Until 2026-10-02
      // it was taken for a predicate, and the operator after it was a syntax
      // error, where `[X?] =?= Y?` and `1 + X? > 3` parsed (GLP #3 Cowork,
      // 2026-10-02 17:12 UTC, S2).
      if (_continuesAsInfixGuard(_peek())) {
        _current = start;
      } else {
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
          return Goal('=', [leftTerm, rightTerm], functorToken.line, functorToken.column);
        }

        // Return as Goal for now (will be cast to Guard if before |)
        final goal = Goal(functorToken.lexeme, args, functorToken.line, functorToken.column);

        // Check for spawn annotation: Goal@AgentId
        if (_match(TokenType.AT)) {
          final agentToken = _consume(TokenType.ATOM, 'Expected agent identifier after @');
          return SpawnGoal(goal, agentToken.lexeme, functorToken.line, functorToken.column);
        }

        return goal;
      }
    }

    // Otherwise, try to parse as infix comparison (e.g., X < Y, X? mod P? =:= 0)
    // Use _parseExpression(6) to parse arithmetic but stop at comparison operators
    final left = _parseExpression(6);

    // Check for comparison operator
    if (_check(TokenType.LESS) || _check(TokenType.GREATER) ||
        _check(TokenType.LESS_EQUAL) || _check(TokenType.GREATER_EQUAL) ||
        _check(TokenType.EQUALS) || _check(TokenType.ARITH_EQUAL) ||
        _check(TokenType.ARITH_NOT_EQUAL) || _check(TokenType.GROUND_EQUAL) ||
        _check(TokenType.GROUND_NOT_EQUAL) || _check(TokenType.AT_LESS)) {
      final opToken = _advance();
      final right = _parseExpression(6);

      // Transform infix to prefix: X < Y → <(X, Y)
      return Goal(opToken.lexeme, [left, right], opToken.line, opToken.column);
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

    final functorToken = _consumePredicateName();
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

  /// The refusal of a cross-module call whose module is a variable, `M # G` or
  /// `M? # G`.  The qualifier of a cross-module call is a child directory or
  /// module file of the caller's directory (TGLP modules.tex, "Cross-module
  /// type checking"), and a module value is run with run/2 or run/3 (GLP-Spec
  /// appendix-guards.tex, "Dynamic activation"); the dynamic dispatch that took
  /// a variable cannot be typed and is gone (TGLP modules.tex, Implementation).
  CompileError _variableModuleError(Token varToken) {
    final mark = varToken.type == TokenType.READER ? '?' : '';
    return CompileError(
      'A cross-module call names its module: "${varToken.lexeme}$mark # ..." '
      'has a variable there. The module of M # G is a child directory or '
      'module file of the caller\'s directory; a module value is run with '
      'run/2 or run/3.',
      varToken.line,
      varToken.column,
      phase: 'parser',
    );
  }

  // Goal: same as Atom, or assignment (Var := Expr) or univ (Var =.. Expr)
  // Also handles remote goals: Module # Goal
  Goal _parseGoal() {
    // Check for assignment or univ: Var := Expr or Var =.. Expr
    if (_check(TokenType.VARIABLE) || _check(TokenType.READER)) {
      final varToken = _advance();
      final isReader = varToken.type == TokenType.READER;

      if (_check(TokenType.HASH)) {
        throw _variableModuleError(varToken);
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

    final functorToken = _consumePredicateName();
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

  // Term: variable, structure, list, constant, underscore, tuple, or expression
  Term _parseTerm() {
    // Try to parse as expression (handles arithmetic operators)
    return _parseExpression();
  }

  // Expression parsing with precedence (Pratt parsing)
  // This handles arithmetic operators with proper precedence
  Term _parseExpression([int minPrecedence = 0]) {
    var left = _parsePrimary();

    while (_isOperator(_peek()) && _precedence(_peek()) >= minPrecedence &&
           !_isElseBranchStar()) {
      final op = _advance();
      final right = _parseExpression(_precedence(op) + 1);
      left = StructTerm(_operatorFunctor(op), [left, right], op.line, op.column);
    }

    return left;
  }

  /// The operator names and keywords the lexer makes tokens of, punctuation
  /// apart.  Each is a name: GLP-Spec reserves no word (appendix-lp.tex,
  /// Definition "Logic Programs Syntax": a term is a variable, a constant or a
  /// compound term f(T1, ..., Tn), in standard LP notions), so where a term is
  /// expected the reader takes one as the constant of that name, and as the
  /// functor of a compound term where "(" follows it, as Prolog does (GLP #3
  /// Cowork, 2026-10-03 21:18 UTC, "11:58. 2": "`mod` and `procedure` are
  /// constants ... compliance, fix it").  Until 2026-10-03 an unquoted `mod`
  /// or `procedure` in a term was "Expected term, got TokenType.MOD", and
  /// `f(=)`, `f(+)`, `[-]` likewise; a quoted name was the one way to write
  /// them.  `,` and `|` stay punctuation, quoted when meant as names, as in
  /// Prolog; `?` is the reader mark.
  static const Set<TokenType> _operatorNames = {
    TokenType.PLUS, TokenType.MINUS, TokenType.STAR, TokenType.SLASH,
    TokenType.SLASH_SLASH, TokenType.MOD,
    TokenType.LESS, TokenType.GREATER, TokenType.LESS_EQUAL,
    TokenType.GREATER_EQUAL, TokenType.EQUALS, TokenType.ARITH_EQUAL,
    TokenType.ARITH_NOT_EQUAL, TokenType.GROUND_EQUAL,
    TokenType.GROUND_NOT_EQUAL, TokenType.AT_LESS, TokenType.UNIV,
    TokenType.UNIV_DECOMPOSE,
    TokenType.IMPLIES, TokenType.ASSIGN, TokenType.COLONCOLONEQ,
    TokenType.SEMICOLON, TokenType.COLON,
    TokenType.TILDE, TokenType.HASH, TokenType.BACKSLASH, TokenType.AT,
    TokenType.PROCEDURE,
  };

  /// Whether the reader takes a token of [type] where a term is expected as
  /// a name ([_operatorNames]): the functor of a compound term where "("
  /// follows it.  The printer asks it of a functor (glp_printer.dart,
  /// `functorNameSource`).
  static bool isOperatorName(TokenType type) => _operatorNames.contains(type);

  /// The tokens that end an operand: an operator name before one of them has
  /// no operand of its own and is the constant of its name.
  static const Set<TokenType> _endsOperand = {
    TokenType.COMMA, TokenType.RPAREN, TokenType.RBRACKET, TokenType.PIPE,
    TokenType.DOT, TokenType.SEMICOLON, TokenType.EOF,
  };

  /// Whether an operator name stands at the current position where a term is
  /// expected, and how it reads there: 'functor' where "(" follows it,
  /// 'constant' where it has no operand --- an operand-ending token follows,
  /// or it is `mod` or `procedure`, a word that is no prefix operator ---
  /// and null otherwise (a prefix minus, or no term at all).
  String? _operatorNameAt() {
    if (_isAtEnd() || !_operatorNames.contains(_peek().type)) return null;
    final next = _current + 1 < tokens.length
        ? tokens[_current + 1].type
        : TokenType.EOF;
    if (next == TokenType.LPAREN) return 'functor';
    final t = _peek().type;
    if (t == TokenType.MOD || t == TokenType.PROCEDURE) return 'constant';
    if (_endsOperand.contains(next)) return 'constant';
    return null;
  }

  // Primary expression: variable, number, string, list, structure, parenthesized, unary minus
  Term _parsePrimary() {
    // An operator name where a term is expected (see [_operatorNames]): the
    // functor of a compound term before "(", Exp ::= +(Exp?, Exp?) among them,
    // checked before unary minus so -(X, Y) is a structure and not
    // neg((X, Y)); otherwise, with no operand, the constant of its name.
    final operatorName = _operatorNameAt();
    if (operatorName == 'functor') {
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
    if (operatorName == 'constant') {
      final nameToken = _advance();
      if (_check(TokenType.QUESTION)) {
        throw CompileError(
          'Reader mark "?" can only be applied to variables, not constants like "${nameToken.lexeme}"',
          _peek().line,
          _peek().column,
          phase: 'parser'
        );
      }
      return ConstTerm(nameToken.lexeme, nameToken.line, nameToken.column);
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

  /// Whether [t], after a term, makes the term the left operand of an infix
  /// guard: a comparison, or an arithmetic operator of the expression
  /// compared (`_parseExpression(6)`'s, whose precedence is above the
  /// comparisons').  `=` is not among them: `foo(a) = X` is the unification
  /// goal it was; nor `#` and `@`, of a remote goal and a spawn; nor a `*`
  /// beginning a vGLP else branch.
  bool _continuesAsInfixGuard(Token t) {
    switch (t.type) {
      case TokenType.LESS:
      case TokenType.GREATER:
      case TokenType.LESS_EQUAL:
      case TokenType.GREATER_EQUAL:
      case TokenType.ARITH_EQUAL:
      case TokenType.ARITH_NOT_EQUAL:
      case TokenType.GROUND_EQUAL:
      case TokenType.GROUND_NOT_EQUAL:
      case TokenType.AT_LESS:
      case TokenType.PLUS:
      case TokenType.MINUS:
      case TokenType.SLASH:
      case TokenType.SLASH_SLASH:
      case TokenType.MOD:
        return true;
      case TokenType.STAR:
        return !_isElseBranchStar();
      default:
        return false;
    }
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
  /// A `?` marks a type name only ([_markedTypeAltPrimary]); after a
  /// structure, a list or a parenthesised term it marks no type name and is
  /// refused ([_refuseMarkAfter]).
  TypeExpr _parseTypeAlt() {
    final term = _parseTypeAltTerm();
    return termToTypeExpr(term);
  }

  /// Parse a term in type alternative context.
  /// Similar to _parseTerm(), with a `?` on a type name read as its dual.
  Term _parseTypeAltTerm() {
    return _parseTypeAltExpression();
  }

  /// Parse expression in type alternative context.
  /// Handles operators like \ for difference lists.
  Term _parseTypeAltExpression([int minPrecedence = 0]) {
    var left = _markedTypeAltPrimary(_parseTypeAltPrimary());

    while (_isOperator(_peek()) && _precedence(_peek()) >= minPrecedence) {
      final op = _advance();
      final right = _parseTypeAltExpression(_precedence(op) + 1);
      left = StructTerm(_operatorFunctor(op), [left, right], op.line, op.column);
    }

    return left;
  }

  /// [term], a primary of a type alternative, with the `?` standing apart
  /// after it read: `T ?` is `T?`, the dual of the type T, as a procedure
  /// declaration reads it ([_parseProcArgType]).  `?` is the complementation
  /// operator on a type (TGLP typed-glp.tex, "Type Declarations": "GLP types
  /// are specified using BNF rules with the complementation operator ?", and
  /// "its dual (for example Stream?) is an input type"), so after anything
  /// but a type name not yet complemented it marks nothing, and it is refused
  /// rather than dropped (GLP #3 Cowork, 2026-10-10 07:48 UTC, "00:26": "A
  /// parser that drops a mark silently is at fault whatever the syntax").
  /// Until 2026-10-10 a `?` standing apart was consumed here and dropped, so
  /// `Q ::= f(R ?).` was read as `f(R)`.
  Term _markedTypeAltPrimary(Term term) {
    while (_check(TokenType.QUESTION)) {
      final q = _advance();
      if (term is VarTerm && !term.isReader) {
        term = VarTerm(term.name, true, term.line, term.column);
        continue;
      }
      // Shown as written: a constant by its name, and a parameterised type
      // reference in reader mode, the structure whose functor carries the
      // mark ([_parseTypeAltPrimary]), with the mark last.
      final follows = term is ConstTerm
          ? '${term.value}'
          : term is StructTerm && term.functor.endsWith('?')
              ? '${term.functor.substring(0, term.functor.length - 1)}'
                  '(${term.args.join(", ")})?'
              : '$term';
      throw CompileError(
        'A "?" in a type definition marks the type name before it, "T ?" '
        'being "T?", the dual of T; here it follows "$follows", and marks '
        'nothing',
        q.line,
        q.column,
        phase: 'parser',
      );
    }
    return term;
  }

  /// Refuses a `?` next, after [what]: a structure, a list or a parenthesised
  /// term of a type alternative, which is no type name, so that the `?` marks
  /// no type name.  TGLP gives `?` a meaning on a type name only, its dual
  /// (typed-glp.tex, "Type Declarations"), and GLP-Spec on a variable only, a
  /// reader (glp.tex, Definition "GLP Variables"); a `?` after anything else
  /// is refused, not dropped (GLP, 2026-10-10 08:40 UTC).  Until 2026-10-10
  /// such a `?` was consumed here and dropped, for an "explicit dual" written
  /// `Channel? ::= ch(Stream?, Stream)?.`, which TGLP does not have.
  void _refuseMarkAfter(String what) {
    if (!_check(TokenType.QUESTION)) return;
    final q = _peek();
    throw CompileError(
      'A "?" in a type definition marks the type name before it, "T ?" '
      'being "T?", the dual of T; here it follows $what, which is not a type '
      'name, and marks no type name',
      q.line,
      q.column,
      phase: 'parser',
    );
  }

  /// Parse primary term in type alternative context.  A `?` after a
  /// structure, a list or a parenthesised term is refused ([_refuseMarkAfter]).
  Term _parseTypeAltPrimary() {
    // An operator name in a type alternative is a name, as in a term
    // ([_operatorNames]): the functor of a structure alternative before "(",
    // Exp ::= +(Exp?, Exp?) among them, and otherwise, with no operand, a
    // constant alternative, Op ::= + ; mod.
    final operatorName = _operatorNameAt();
    if (operatorName == 'functor') {
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
      _refuseMarkAfter('the structure "${functorToken.lexeme}(...)"');
      return StructTerm(functorToken.lexeme, args, functorToken.line, functorToken.column);
    }
    if (operatorName == 'constant') {
      final nameToken = _advance();
      return ConstTerm(nameToken.lexeme, nameToken.line, nameToken.column);
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
        _refuseMarkAfter('the parenthesised term "(...)"');
        return result;
      } else {
        _consume(TokenType.RPAREN, 'Expected ")" after expression');
        // Refused here, and not left to [_markedTypeAltPrimary], which would
        // read "(T) ?" as "T?": the "?" follows the parenthesised term.
        _refuseMarkAfter('the parenthesised term "(...)"');
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
        _refuseMarkAfter('the structure "${functorToken.lexeme}(...)"');
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

  /// Parse list in type alternative context.  A `?` after the list is
  /// refused ([_refuseMarkAfter]).
  Term _parseTypeAltList() {
    final bracketToken = _consume(TokenType.LBRACKET, 'Expected "["');

    if (_match(TokenType.RBRACKET)) {
      _refuseMarkAfter('the list "[]"');
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
      _refuseMarkAfter('the list "[...]"');
      Term result = tail;
      for (int i = elements.length - 1; i >= 0; i--) {
        result = ListTerm(elements[i], result, bracketToken.line, bracketToken.column);
      }
      return result;
    }

    _consume(TokenType.RBRACKET, 'Expected "]" after list elements');
    _refuseMarkAfter('the list "[...]"');

    Term result = ListTerm(null, null, bracketToken.line, bracketToken.column);
    for (int i = elements.length - 1; i >= 0; i--) {
      result = ListTerm(elements[i], result, bracketToken.line, bracketToken.column);
    }
    return result;
  }

  /// The token types a declared procedure's name may be
  /// ([_parseProcDeclaration]).
  static const Set<TokenType> _procedureNameTokens = {
    TokenType.ATOM, TokenType.PROCEDURE, TokenType.LESS, TokenType.GREATER, TokenType.LESS_EQUAL,
    TokenType.GREATER_EQUAL, TokenType.ARITH_EQUAL, TokenType.ARITH_NOT_EQUAL,
    TokenType.GROUND_EQUAL, TokenType.GROUND_NOT_EQUAL, TokenType.AT_LESS,
    TokenType.EQUALS, TokenType.UNIV, TokenType.UNIV_DECOMPOSE,
    TokenType.ASSIGN,
  };

  /// Whether a procedure declaration begins at token [at], the current one
  /// by default: `procedure`, after `exported` or `imported` or not, then its
  /// parameter list or none, then a procedure name before "(", "." or "#" ---
  /// `procedure p(X).`, `procedure(X) merge(...).` (TGLP
  /// parameterized-types.tex, "Parameterised Procedure Declarations": the
  /// parameters are named "in a list after the keyword").  No word is
  /// reserved (GLP #3 Cowork, 2026-10-04 09:06 UTC, "23:49. Q2: `procedure`
  /// immediately before "(" is a functor ...; `procedure p(X).` is a
  /// declaration"): `procedure` with no name after it --- `procedure(a).`,
  /// `procedure(X) :- q(X?).`, `procedure.` --- begins a clause of the
  /// procedure named `procedure`.  Until 2026-10-04 every `procedure` there
  /// began a declaration, and such a clause was a syntax error.
  ///
  /// In a .vglp source the declaration of a volitional procedure carries its
  /// interactive type before the name, `procedure (T)*p(...)`
  /// ([_interactiveDeclarationAt]).
  bool _atProcDeclaration([int? at]) {
    var i = at ?? _current;
    final interactive = vglp ? _interactiveDeclarationAt(i) : null;
    if (interactive != null) {
      i = interactive.name;
      if (i + 1 >= tokens.length ||
          !_procedureNameTokens.contains(tokens[i].type)) {
        return false;
      }
      final after = tokens[i + 1].type;
      return after == TokenType.LPAREN ||
          after == TokenType.DOT ||
          after == TokenType.HASH;
    }
    if (i < tokens.length &&
        tokens[i].type == TokenType.ATOM &&
        (tokens[i].lexeme == 'exported' || tokens[i].lexeme == 'imported')) {
      i++;
    }
    if (i >= tokens.length || tokens[i].type != TokenType.PROCEDURE) {
      return false;
    }
    i++;
    if (i < tokens.length && tokens[i].type == TokenType.LPAREN) {
      var depth = 0;
      for (; i < tokens.length; i++) {
        final t = tokens[i].type;
        if (t == TokenType.EOF) return false;
        if (t == TokenType.LPAREN) depth++;
        if (t == TokenType.RPAREN && --depth == 0) break;
      }
      i++;
    }
    if (i + 1 >= tokens.length ||
        !_procedureNameTokens.contains(tokens[i].type)) {
      return false;
    }
    final after = tokens[i + 1].type;
    return after == TokenType.LPAREN ||
        after == TokenType.DOT ||
        after == TokenType.HASH;
  }

  /// A predicate's name, in a clause head or a goal: a name, or `procedure`,
  /// which reserves nothing there (GLP #3 Cowork, 2026-10-04 09:06 UTC,
  /// "23:49. Q2"; [_atProcDeclaration]).
  Token _consumePredicateName() {
    if (_check(TokenType.PROCEDURE)) return _advance();
    return _consume(TokenType.ATOM, 'Expected predicate name');
  }

  /// Parse a procedure declaration: procedure name(Type?, Type).
  /// or: exported procedure name(Type?, Type).
  /// or: imported procedure [path#]name(Type?, Type).
  ///
  /// The keyword may carry a type-parameter list, which names the declaration's
  /// parameters: procedure(X) merge(Stream(X)?, Stream(X)?, Stream(X)).
  /// The list follows `exported` and `imported` the same way.  A declaration
  /// with no parameters is written without a list.
  /// Spec: Moded-Types, sections/parameterized-types.tex, Parameterised
  /// Procedure Declarations and the paragraph Declaration parameters.
  ProcDecl _parseProcDeclaration() {
    // A volitional procedure's declaration, in a .vglp source: its interactive
    // type stands before its name ([_interactiveDeclarationAt]).
    final interactive = vglp ? _interactiveDeclarationAt(_current) : null;

    // Check for 'exported' or 'imported' keyword before 'procedure'
    bool exported = false;
    bool imported = false;
    final startLine = _peek().line;
    final startColumn = _peek().column;
    // An imported declaration of a volitional procedure, `imported procedure
    // (T)*M#p(...)`, is read as its export is (see [vglp]).
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

    // Optional type-parameter list: procedure(X, Y) p(...).
    // No other declaration form has "(" directly after the keyword, so the
    // list is unambiguous, save in a .vglp source, where "(" there may open
    // the interactive type of a volitional procedure, `procedure (T)*p(...)`.
    final typeParams = <String>[];
    if ((interactive == null || _current != interactive.open) &&
        _match(TokenType.LPAREN)) {
      typeParams.add(_consume(TokenType.VARIABLE, 'Expected type parameter name').lexeme);
      while (_match(TokenType.COMMA)) {
        typeParams.add(_consume(TokenType.VARIABLE, 'Expected type parameter name').lexeme);
      }
      _consume(TokenType.RPAREN, 'Expected ")" after type parameters');
      for (var i = 0; i < typeParams.length; i++) {
        if (typeParams.indexOf(typeParams[i]) != i) {
          throw CompileError(
            'Type parameter "${typeParams[i]}" is named twice in the parameter list',
            line,
            column,
            phase: 'parser',
          );
        }
      }
    }

    // The interactive type T of a volitional procedure, `(T)*`, in writer or
    // reader mode as an argument type is (vGLP, Definition "Guarded Clause,
    // Volitional Procedure, ...").
    TypeExpr? interactiveType;
    if (interactive != null) {
      _consume(TokenType.LPAREN, 'Expected "(" before the interactive type');
      interactiveType = _parseProcArgType();
      _consume(TokenType.RPAREN, 'Expected ")" after the interactive type');
      _consume(TokenType.STAR, 'Expected "*" after the interactive type');
    }

    // Parse procedure name, possibly with module path for imported procedures.
    // For imported: 'social#agent' → modulePath='social', name='agent'
    //              'ui#actors#render' → modulePath='ui#actors', name='render'
    //              'merge' → modulePath=null, name='merge'
    String? modulePath;

    // Procedure name can be atom or operator (<, >, =<, >=, =:=, =\=, =?=, =?\=, =, @<),
    // or `procedure`, a name after the keyword like any other.
    Token nameToken;
    if (_check(TokenType.ATOM) || _check(TokenType.PROCEDURE)) {
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
    } else if (_check(TokenType.GROUND_NOT_EQUAL)) {
      nameToken = _advance();
    } else if (_check(TokenType.AT_LESS)) {
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
        // Next token should be an atom (next path component or procedure
        // name), `procedure` among them
        if (!_check(TokenType.ATOM) && !_check(TokenType.PROCEDURE)) {
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

    // A volitional procedure's clauses are the guarded clauses
    // p(S1, ..., Sn, A) of arity n+1, A of type T (vGLP, Definition "Guarded
    // Clause, Volitional Procedure, ..."), so its declaration is theirs,
    // p(T1, ..., Tn, T).
    if (interactiveType != null) argTypes.add(interactiveType);

    _consume(TokenType.DOT, 'Expected "." after procedure declaration');

    return ProcDecl(name, argTypes, line, column, typeParams: typeParams, exported: exported, imported: imported, modulePath: modulePath);
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
