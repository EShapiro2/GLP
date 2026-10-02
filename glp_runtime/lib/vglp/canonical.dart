// glp_runtime/lib/vglp/canonical.dart
//
// The canonical compilation of a vGLP program written in the paper's syntax.
// Spec: vGLP at db03e2d --- sections/vglp.tex, Definition "Guarded Clause,
// Volitional Procedure, Interactive Type, Interactive Term, Ordinary Clause,
// Procedure, vGLP Program"; sections/elicitation.tex, Definition "Canonical
// Compilation".
//
// The syntax:
//
//     procedure (T)*p(T1, ..., Tn).
//     (A)*p(S1, ..., Sn) :- G | B.
//
// T is the interactive type, in writer or reader mode as an argument type is;
// A, the interactive term, is a term of type T, possibly a variable, or `_`,
// the anonymous variable, if T is in reader mode; in writer mode, which would
// leave the output unwritten, the anonymous variable is refused.  The clause
// "is the guarded clause p(S1, ..., Sn, A) :- G | B, of arity n+1" (Definition
// "Guarded Clause, ..."), so the front end reads it as exactly that: a token
// rewrite puts A after the last argument, and T after the last argument type,
// and the ordinary GLP parser reads the result.
//
// The compilation, by the Definition:
//
//   - every ordinary clause, each call q(S1, ..., Sn) of a volitional
//     procedure q in its body replaced by q_a(S1, ..., Sn);
//   - for each volitional procedure q of interactive type T, its clauses as
//     guarded clauses of arity n+1, their calls replaced likewise; a clause
//     with the interactive term `_` keeps it there, dropping the reader, and
//     no goal is added: there is no built-in (vGLP's code task of 2026-10-02
//     00:13 UTC, item B');
//   - the asking clause
//         q_a(S1, ..., Sn) :- construct(T, X), q(S1', ..., Sn', X?).
//     S'_l the reader of S_l at an input position and the writer at an output
//     position, X and X? exchanged where T is in writer mode;
//   - typed: the asking clause declared with the source declaration's argument
//     types, q with the argument of T added in its mode.
//
// THE NAMES.  The asking clause takes the source name, q_a = q, so a call of
// q in a body, and in an initial goal, is already the call of its asking
// clause; the (n+1)-ary procedure takes `q1`, made fresh against every name the
// program uses.
//
// THE CONSTRUCT.  T is passed as the constant naming the moded interactive
// type as written, 'Menu' or 'Menu?'.  construct(T, X) is the construct
// process, which is Part 2's runtime and not declared here.

import '../compiler/ast.dart';
import '../compiler/error.dart';
import '../compiler/glp_printer.dart';
import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../compiler/token.dart';
import '../analysis/type_checker/type_ast.dart'
    show ProcDecl, TypeExpr;
import 'mediator.dart' show printTypeDef, typeSource;
import 'program_compilation.dart' show compiledHeader;

/// The construct process of an interactive type (vGLP, Definition "Canonical
/// Compilation").
const constructGoal = 'construct';

/// One volitional procedure of the source, and the names the compilation
/// gives it.
class VolitionalProcedure {
  /// The source name, which the asking clause keeps.
  final String name;

  /// The source arity n.
  final int arity;

  /// The interactive type T, in its mode.
  final TypeExpr interactiveType;

  /// Whether T is in reader mode: the person writes the variable, and the
  /// asked goal receives its reader.
  final bool readerMode;

  /// The name of the (n+1)-ary procedure, the clauses of q.
  final String guardedName;

  VolitionalProcedure(this.name, this.arity, this.interactiveType,
      this.readerMode, this.guardedName);

  /// The name of the asking clause.
  String get askingName => name;

  /// The constant naming the moded interactive type as written.
  String get typeConstant => interactiveType.toString();
}

/// The canonical compilation of one source: the GLP module's text, the module
/// it parses to, and the volitional procedures it compiled.
class CanonicalProgram {
  final String source;
  final Module module;
  final List<VolitionalProcedure> volitional;

  CanonicalProgram(this.source, this.module, this.volitional);
}

/// Whether [tokens] are a vGLP program in the paper's syntax: some declaration
/// `procedure (T)*p(...)` or some clause `(A)*p(...)`.  A source with neither
/// is in the old syntax, or has no volitional procedure, and keeps the old
/// compilation (the code task of 2026-10-01, item 6).
///
/// The test reads the tokens and nothing more: a source whose brackets do not
/// balance is left to the parser that compiles it to report.
bool isPaperSyntax(List<Token> tokens) {
  for (final s in _itemStarts(tokens)) {
    if (tokens[s].type == TokenType.LPAREN) return true;
    try {
      if (_interactiveDeclaration(tokens, s) != null) return true;
    } on CompileError {
      return false;
    }
  }
  return false;
}

/// [isPaperSyntax] of a source's text.
bool isPaperSyntaxSource(String text) => isPaperSyntax(Lexer(text).tokenize());

/// Compile [text], a vGLP program in the paper's syntax, by the canonical
/// compilation.
CanonicalProgram compileCanonical(String text) {
  final parsed = _parse(text);
  final m = parsed.module;

  _checkNoOldDesign(m);

  // The volitional procedures, by their source signature p/n.
  final declsByKey = {for (final d in m.procDeclarations) d.key: d};
  final procsByKey = {for (final p in m.procedures) p.signature: p};
  final taken = _namesUsed(m);
  final volitional = <String, VolitionalProcedure>{};
  for (final v in parsed.declarations) {
    final sig = '${v.name}/${v.arity}';
    final decl = declsByKey['${v.name}/${v.arity + 1}']!;
    if (declsByKey.containsKey(sig) || procsByKey.containsKey(sig)) {
      throw CompileError(
          'The volitional procedure $sig is declared with an interactive type, '
          'and $sig is also declared or defined without one: its asking clause '
          'is $sig (vGLP, Definition "Canonical Compilation")',
          v.line, v.column, phase: 'analyzer');
    }
    final t = decl.argTypes.last;
    final guarded = _fresh('${v.name}1', taken);
    volitional[sig] = VolitionalProcedure(
        v.name, v.arity, t, decl.isInputArg(v.arity), guarded);
  }

  // Every clause written (A)*p is of a procedure declared (T)*p, and every
  // clause of such a procedure is written (A)*p.
  for (final c in parsed.clauses) {
    if (!volitional.containsKey('${c.name}/${c.arity}')) {
      throw CompileError(
          'The clause (A)*${c.name}/${c.arity} is of no procedure declared '
          '"procedure (T)*${c.name}(...)": a volitional procedure is declared '
          'with its interactive type (vGLP, Definition "Guarded Clause, ...")',
          c.line, c.column, phase: 'analyzer');
    }
  }
  final written = {for (final c in parsed.clauses) '${c.line}:${c.column}'};
  for (final v in volitional.values) {
    final p = procsByKey['${v.name}/${v.arity + 1}'];
    if (p == null) continue;  // the parser requires a declaration's clauses
    for (final c in p.clauses) {
      if (!written.contains('${c.line}:${c.column}')) {
        throw CompileError(
            'A clause of the volitional procedure ${v.name}/${v.arity} is '
            'written "(A)*${v.name}(S1, ..., Sn)", with its interactive term '
            '(vGLP, Definition "Guarded Clause, ...")',
            c.line, c.column, phase: 'analyzer');
      }
    }
  }
  _checkNoAskedCall(m, volitional);
  _checkNoAnonymousOutput(m, volitional);

  // The compiled procedures, each with its declaration, in source order; a
  // volitional procedure becomes its asking clause and its (n+1)-ary
  // procedure.
  final askedProcs = {
    for (final v in volitional.values) '${v.name}/${v.arity + 1}': v
  };
  final out = <_Emitted>[];
  for (final p in m.procedures) {
    final v = askedProcs[p.signature];
    if (v == null) {
      out.add(_Emitted(declsByKey[p.signature], p));
      continue;
    }
    final decl = declsByKey[p.signature]!;
    out.add(_Emitted(
        _askingDeclaration(decl, v), _askingClause(decl, v, constructGoal)));
    out.add(_Emitted(
        ProcDecl(v.guardedName, decl.argTypes, decl.line, decl.column,
            typeParams: decl.typeParams),
        _guardedProcedure(p, v)));
  }
  // Declarations with no clauses of their own: imported procedures, and
  // declarations of procedures the runtime implements.
  final defined = {for (final p in m.procedures) p.signature};
  final bare = [
    for (final d in m.procDeclarations)
      if (!defined.contains(d.key)) d
  ];

  final source = _emit(m, out, bare);
  final module = Parser(Lexer(source).tokenize()).parseModule();
  return CanonicalProgram(source, module, volitional.values.toList());
}

// ---------------------------------------------------------------------------
// The front end: the paper's syntax as the guarded clauses it denotes
// ---------------------------------------------------------------------------

/// A declaration `procedure (T)*p(T1, ..., Tn)`, at its `procedure` token.
class _Declared {
  final String name;
  final int arity;
  final int line, column;
  _Declared(this.name, this.arity, this.line, this.column);
}

/// A clause `(A)*p(S1, ..., Sn) :- ...`, at the token of its name, which is
/// where the parser places the clause it reads.
class _Written {
  final String name;
  final int arity;
  final int line, column;
  _Written(this.name, this.arity, this.line, this.column);
}

class _Parsed {
  final Module module;
  final List<_Declared> declarations;
  final List<_Written> clauses;
  _Parsed(this.module, this.declarations, this.clauses);
}

_Parsed _parse(String text) {
  final tokens = Lexer(text).tokenize();
  final declared = <_Declared>[];
  final written = <_Written>[];
  final out = <Token>[];

  var i = 0;
  final starts = _itemStarts(tokens).toSet();
  while (i < tokens.length) {
    if (!starts.contains(i)) {
      out.add(tokens[i]);
      i++;
      continue;
    }
    final t = tokens[i];

    // The old syntax's volition guard, which the paper's syntax replaces.
    if (t.type == TokenType.STAR) {
      throw CompileError(
          'A volition guard "*(...)" in a source in the paper\'s syntax: a '
          'source is written in the old syntax or in the paper\'s, not both '
          '(vGLP, Definition "Guarded Clause, ...")',
          t.line, t.column, phase: 'parser');
    }

    // A clause (A)*p(S1, ..., Sn) :- ...  reads as p(S1, ..., Sn, A) :- ...
    if (t.type == TokenType.LPAREN) {
      final close = _matching(tokens, i);
      if (close + 2 >= tokens.length ||
          tokens[close + 1].type != TokenType.STAR ||
          tokens[close + 2].type != TokenType.ATOM) {
        throw CompileError(
            'Expected "(A)*p(S1, ..., Sn)": an interactive term in '
            'parentheses, "*" and the procedure\'s name '
            '(vGLP, Definition "Guarded Clause, ...")',
            t.line, t.column, phase: 'parser');
      }
      final interactive = tokens.sublist(i + 1, close);
      if (interactive.isEmpty) {
        throw CompileError('An empty interactive term "()"', t.line, t.column,
            phase: 'parser');
      }
      final name = tokens[close + 2];
      var j = close + 3;
      var args = <Token>[];
      if (j < tokens.length && tokens[j].type == TokenType.LPAREN) {
        final a = _matching(tokens, j);
        args = tokens.sublist(j + 1, a);
        j = a + 1;
      }
      written.add(_Written(
          name.lexeme, _countArgs(args), name.line, name.column));
      out
        ..add(name)
        ..add(_tok(TokenType.LPAREN, '(', name))
        ..addAll(args);
      if (args.isNotEmpty) out.add(_tok(TokenType.COMMA, ',', name));
      out
        ..addAll(interactive)
        ..add(_tok(TokenType.RPAREN, ')', name));
      i = j;
      continue;
    }

    // A declaration procedure (T)*p(T1, ..., Tn) reads as
    // procedure p(T1, ..., Tn, T).
    final d = _interactiveDeclaration(tokens, i);
    if (d != null) {
      if (tokens[i].type == TokenType.ATOM && tokens[i].lexeme == 'imported') {
        throw CompileError(
            'An imported declaration names the asking clause, p/n, and carries '
            'no interactive type',
            t.line, t.column, phase: 'parser');
      }
      final name = tokens[d.nameIndex];
      if (name.type != TokenType.ATOM) {
        throw CompileError(
            'Expected the volitional procedure\'s name after "(T)*"',
            name.line, name.column, phase: 'parser');
      }
      var j = d.nameIndex + 1;
      var args = <Token>[];
      if (j < tokens.length && tokens[j].type == TokenType.LPAREN) {
        final a = _matching(tokens, j);
        args = tokens.sublist(j + 1, a);
        j = a + 1;
      }
      declared.add(_Declared(name.lexeme, _countArgs(args), t.line, t.column));
      out
        ..addAll(tokens.sublist(i, d.prefixEnd))
        ..add(name)
        ..add(_tok(TokenType.LPAREN, '(', name))
        ..addAll(args);
      if (args.isNotEmpty) out.add(_tok(TokenType.COMMA, ',', name));
      out
        ..addAll(tokens.sublist(d.typeStart, d.typeEnd))
        ..add(_tok(TokenType.RPAREN, ')', name));
      i = j;
      continue;
    }

    out.add(t);
    i++;
  }

  final module = Parser(out).parseModule();
  return _Parsed(module, declared, written);
}

/// Where the tokens of an interactive declaration lie: its prefix
/// (`exported`, `procedure`, a parameter list) ends at [prefixEnd], its
/// interactive type is [typeStart, typeEnd), and its name is at [nameIndex].
class _DeclarationAt {
  final int prefixEnd, typeStart, typeEnd, nameIndex;
  _DeclarationAt(this.prefixEnd, this.typeStart, this.typeEnd, this.nameIndex);
}

/// The interactive declaration beginning at [s], or null: `procedure (T)*p`,
/// `procedure(X, ...) (T)*p`, either after `exported` or `imported`.
_DeclarationAt? _interactiveDeclaration(List<Token> tokens, int s) {
  var j = s;
  if (j < tokens.length &&
      tokens[j].type == TokenType.ATOM &&
      (tokens[j].lexeme == 'exported' || tokens[j].lexeme == 'imported')) {
    j++;
  }
  if (j >= tokens.length || tokens[j].type != TokenType.PROCEDURE) return null;
  j++;
  if (j >= tokens.length || tokens[j].type != TokenType.LPAREN) return null;
  final k = _matching(tokens, j);
  if (k + 1 < tokens.length && tokens[k + 1].type == TokenType.STAR) {
    return _DeclarationAt(j, j + 1, k, k + 2);
  }
  // A parameter list, then the interactive type.
  if (k + 1 < tokens.length && tokens[k + 1].type == TokenType.LPAREN) {
    final m = _matching(tokens, k + 1);
    if (m + 1 < tokens.length && tokens[m + 1].type == TokenType.STAR) {
      return _DeclarationAt(k + 1, k + 2, m, m + 2);
    }
  }
  return null;
}

/// The indices at which a top-level item --- a declaration, a definition or a
/// clause --- begins: the first token, and each token after a full stop.
List<int> _itemStarts(List<Token> tokens) {
  final starts = <int>[];
  var depth = 0;
  var atStart = true;
  for (var i = 0; i < tokens.length; i++) {
    final t = tokens[i].type;
    if (t == TokenType.EOF) break;
    if (atStart && depth == 0) starts.add(i);
    atStart = false;
    if (t == TokenType.LPAREN ||
        t == TokenType.LBRACKET ||
        t == TokenType.LBRACE) {
      depth++;
    } else if (t == TokenType.RPAREN ||
        t == TokenType.RBRACKET ||
        t == TokenType.RBRACE) {
      if (depth > 0) depth--;
    } else if (t == TokenType.DOT && depth == 0) {
      atStart = true;
    }
  }
  return starts;
}

/// The index of the bracket closing the one at [open].
int _matching(List<Token> tokens, int open) {
  var depth = 0;
  for (var i = open; i < tokens.length; i++) {
    final t = tokens[i].type;
    if (t == TokenType.LPAREN ||
        t == TokenType.LBRACKET ||
        t == TokenType.LBRACE) {
      depth++;
    } else if (t == TokenType.RPAREN ||
        t == TokenType.RBRACKET ||
        t == TokenType.RBRACE) {
      depth--;
      if (depth == 0) return i;
    } else if (t == TokenType.EOF) {
      break;
    }
  }
  final at = tokens[open];
  throw CompileError('Unbalanced "${at.lexeme}"', at.line, at.column,
      phase: 'parser');
}

/// The number of top-level comma-separated arguments in [args].
int _countArgs(List<Token> args) {
  if (args.isEmpty) return 0;
  var n = 1;
  var depth = 0;
  for (final t in args) {
    if (t.type == TokenType.LPAREN ||
        t.type == TokenType.LBRACKET ||
        t.type == TokenType.LBRACE) {
      depth++;
    } else if (t.type == TokenType.RPAREN ||
        t.type == TokenType.RBRACKET ||
        t.type == TokenType.RBRACE) {
      depth--;
    } else if (t.type == TokenType.COMMA && depth == 0) {
      n++;
    }
  }
  return n;
}

Token _tok(TokenType type, String lexeme, Token at) =>
    Token(type, lexeme, at.line, at.column);

// ---------------------------------------------------------------------------
// Checks
// ---------------------------------------------------------------------------

/// The paper's syntax has no display declaration: the redesign of 2026-09-23
/// replaced them (vGLP, Definition "Widget Declaration, Default Widget").
void _checkNoOldDesign(Module m) {
  if (m.displayDecls.isNotEmpty) {
    final d = m.displayDecls.first;
    throw CompileError(
        'A display declaration in a source in the paper\'s syntax: a source is '
        'written in the old syntax or in the paper\'s, not both',
        d.line, d.column, phase: 'analyzer');
  }
}

/// A goal of a volitional procedure is n-ary until it is asked (Definition
/// "Guarded Clause, ..."), so no body calls p with n+1 arguments.
void _checkNoAskedCall(Module m, Map<String, VolitionalProcedure> volitional) {
  final asked = {
    for (final v in volitional.values) '${v.name}/${v.arity + 1}': v
  };
  for (final p in m.procedures) {
    for (final c in p.clauses) {
      for (final g in _calls(c.body ?? const [])) {
        final v = asked['${g.functor}/${g.args.length}'];
        if (v != null) {
          throw CompileError(
              'A call of ${v.name} with ${v.arity + 1} arguments: a goal of '
              'the volitional procedure ${v.name}/${v.arity} is '
              '${v.arity}-ary until it is asked (vGLP, Definition "Guarded '
              'Clause, ...")',
              g.line, g.column, phase: 'analyzer');
        }
      }
    }
  }
}

/// The interactive term is "a term of type T, possibly a variable, or the
/// anonymous variable if T is in reader mode" (Definition "Guarded Clause,
/// ..."): "In writer mode the anonymous variable, which would leave the output
/// unwritten, is not allowed; there the program closes a question inside its
/// output by dropping the question's reader and, where the type provides for
/// it, by writing more of the output".  It is refused written `_`, and written
/// `_?`, TGLP's anonymous output, the interactive term's position being a
/// produced one in writer mode.
void _checkNoAnonymousOutput(
    Module m, Map<String, VolitionalProcedure> volitional) {
  for (final v in volitional.values) {
    if (v.readerMode) continue;
    for (final p in m.procedures) {
      if (p.signature != '${v.name}/${v.arity + 1}') continue;
      for (final c in p.clauses) {
        final a = c.head.args.last;
        if (a is! UnderscoreTerm) continue;
        throw CompileError(
            'The clause ${_asWritten(c, v)} has the anonymous variable as its '
            'interactive term, and the interactive type ${v.typeConstant} of '
            '${v.name}/${v.arity} is in writer mode, where the anonymous '
            'variable, which would leave the output unwritten, is not allowed: '
            'there the program closes a question inside its output by dropping '
            'the question\'s reader and, where the type provides for it, by '
            'writing more of the output (vGLP, Definition "Guarded Clause, '
            '...")',
            c.line, c.column, phase: 'analyzer');
      }
    }
  }
}

/// The head of a clause of a volitional procedure as the source writes it,
/// `(A)*p(S1, ..., Sn)`.
String _asWritten(Clause c, VolitionalProcedure v) {
  final printer = SourcePrinter();
  final args = c.head.args;
  final term = printer.printTerm(args.last);
  final rest = args.sublist(0, args.length - 1).map(printer.printTerm);
  return '($term)*${v.name}${rest.isEmpty ? '' : '(${rest.join(', ')})'}';
}

/// The calls a body makes, a placed goal by its inner goal.
Iterable<Goal> _calls(List<Goal> body) sync* {
  for (final g in body) {
    if (g is SpawnGoal) {
      yield* _calls([g.innerGoal]);
    } else if (g is RemoteGoal) {
      continue;
    } else {
      yield g;
    }
  }
}

/// Every procedure name the module uses, so that a name the compilation adds
/// is fresh against all of them.
Set<String> _namesUsed(Module m) {
  final names = <String>{};
  for (final d in m.procDeclarations) {
    names.add(d.name);
  }
  for (final p in m.procedures) {
    names.add(p.name);
    for (final c in p.clauses) {
      for (final g in c.guards ?? const <Guard>[]) {
        names.add(g.predicate);
      }
      for (final g in _calls(c.body ?? const [])) {
        names.add(g.functor);
      }
    }
  }
  return names;
}

String _fresh(String stem, Set<String> taken) {
  var name = stem;
  var n = 1;
  while (taken.contains(name)) {
    name = '${stem}_$n';
    n++;
  }
  taken.add(name);
  return name;
}

// ---------------------------------------------------------------------------
// The compiled procedures
// ---------------------------------------------------------------------------

/// The asking clause's declaration: the source declaration's argument types.
ProcDecl _askingDeclaration(ProcDecl decl, VolitionalProcedure v) => ProcDecl(
    v.askingName, decl.argTypes.sublist(0, v.arity), decl.line, decl.column,
    typeParams: decl.typeParams, exported: decl.exported);

/// The asking clause
///
///     q(S1, ..., Sn) :- construct(T, X), q1(S1', ..., Sn', X?).
///
/// S'_l the reader of S_l at an input position and the writer at an output
/// position, the head carrying the pair's other end; X and X? exchanged where
/// T is in writer mode.
Procedure _askingClause(ProcDecl decl, VolitionalProcedure v, String goal) {
  final l = decl.line, c = decl.column;
  final head = <Term>[];
  final passed = <Term>[];
  for (var k = 0; k < v.arity; k++) {
    final s = 'S${k + 1}';
    final input = decl.isInputArg(k);
    head.add(VarTerm(s, !input, l, c));
    passed.add(VarTerm(s, input, l, c));
  }
  final clause = Clause(Atom(v.askingName, head, l, c),
      body: [
        Goal(goal, [
          ConstTerm(v.typeConstant, l, c),
          VarTerm('X', !v.readerMode, l, c),
        ], l, c),
        Goal(v.guardedName, [...passed, VarTerm('X', v.readerMode, l, c)],
            l, c),
      ],
      line: l,
      column: c);
  return Procedure(v.askingName, v.arity, [clause], l, c);
}

/// The clauses of q as guarded clauses of arity n+1, named q1.  A clause whose
/// interactive term is `_` keeps it at the interactive position, where it
/// drops the reader, and no goal is added (vGLP's code task of 2026-10-02
/// 00:13 UTC, item B': no built-in).
Procedure _guardedProcedure(Procedure p, VolitionalProcedure v) {
  final clauses = [
    for (final c in p.clauses)
      Clause(Atom(v.guardedName, c.head.args, c.head.line, c.head.column),
          guards: c.guards, body: c.body, line: c.line, column: c.column)
  ];
  return Procedure(v.guardedName, v.arity + 1, clauses, p.line, p.column);
}

// ---------------------------------------------------------------------------
// Emission
// ---------------------------------------------------------------------------

class _Emitted {
  final ProcDecl? decl;
  final Procedure procedure;
  _Emitted(this.decl, this.procedure);
}

String _emit(Module m, List<_Emitted> procs, List<ProcDecl> bare) {
  final b = StringBuffer();
  final printer = SourcePrinter();
  b.write(compiledHeader);
  b.writeln();

  for (final td in m.typeDefs) {
    b.writeln(printTypeDef(td));
  }
  b.writeln();

  for (final d in bare) {
    b.writeln(printDeclaration(d));
  }
  if (bare.isNotEmpty) b.writeln();
  for (final e in procs) {
    if (e.decl != null) b.writeln(printDeclaration(e.decl!));
    for (final c in e.procedure.clauses) {
      b.writeln(printer.printClause(c));
    }
    b.writeln();
  }
  return b.toString();
}

/// A procedure declaration as GLP source, an imported one with its module
/// path.
String printDeclaration(ProcDecl d) {
  final prefix = d.exported ? 'exported ' : (d.imported ? 'imported ' : '');
  final params = d.typeParams.isEmpty ? '' : '(${d.typeParams.join(', ')})';
  return '${prefix}procedure$params ${d.qualifiedName}'
      '(${d.argTypes.map(typeSource).join(', ')}).';
}

/// GLP source from the AST, a constant as the lexer reads it back: an atom
/// bare where it can be, else in single quotes (`'Menu?'`), and a string
/// literal, whose value carries its double quotes, as it was written; and the
/// anonymous variable as it was written, `_`, or `_?` at a produced head
/// position, TGLP's anonymous output (TGLP, "Anonymous variables"), which
/// GlpPrinter prints `_`.
class SourcePrinter extends GlpPrinter {
  @override
  String printTerm(Term term) {
    if (term is ConstTerm && term.value is String) {
      return constantSource(term.value as String);
    }
    if (term is UnderscoreTerm) return term.isReader ? '_?' : '_';
    return super.printTerm(term);
  }
}

/// A constant's value as source.
String constantSource(String v) {
  if (v.length >= 2 && v.startsWith('"') && v.endsWith('"')) {
    return '"${_escape(v.substring(1, v.length - 1), '"')}"';
  }
  if (RegExp(r'^[a-z][A-Za-z0-9_]*$').hasMatch(v)) return v;
  return "'${_escape(v, "'")}'";
}

String _escape(String s, String quote) => s
    .replaceAll('\\', '\\\\')
    .replaceAll(quote, '\\$quote')
    .replaceAll('\n', '\\n')
    .replaceAll('\t', '\\t');
