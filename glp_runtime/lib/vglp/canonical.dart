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
// The compilation, by the Definition (vGLP's code task of 2026-10-02 00:13
// UTC, item F):
//
//   - a procedure REACHES A QUESTION if it is volitional or a clause of it
//     calls a procedure that does, the least fixpoint over the program's
//     calls; each such procedure has one more argument, last, its ASK STREAM;
//   - in a clause of such a procedure, each body call of a procedure that
//     reaches a question is given a fresh writer as its ask stream and the
//     head carries D?, D the clause's ask stream: one such call is given D
//     itself, more are merged into D by merge goals, and with none the head
//     carries [] in place of D?;
//   - every ordinary clause so extended, each call q(S1, ..., Sn) of a
//     volitional procedure q in its body replaced by q_a(S1, ..., Sn);
//   - for each volitional procedure q of interactive type T, its clauses as
//     guarded clauses of arity n+1, with the HANDLE added after the
//     interactive term --- withdraw in a clause whose interactive term is the
//     anonymous variable, `_` or `_Name`, and the anonymous variable in every
//     other --- then extended, and their calls replaced likewise.  The handle
//     is at a produced head position, where TGLP writes the anonymous
//     variable `_?` (TGLP, "Anonymous variables"); `_` is refused there.  A
//     clause whose interactive term is the anonymous variable keeps it, and
//     no goal is added: there is no built-in (item B');
//   - the asking clause
//         q_a(S1, ..., Sn, [ask(T, t(X), W?) | D?]) :- q(S1', ..., Sn', X?, W, D).
//     S'_l the reader of S_l at an input position and the writer at an output
//     position, the head carrying the pair's other end; X and X? exchanged
//     where T is in writer mode; t the functor of T in Question (below);
//   - typed: the types of the source; the handle's, Handle ::= withdraw; the
//     questions, Question ::= t1(T1) ; ... ; tk(Tk), one functor per moded
//     interactive type, each type as written in its mode; the ask stream's
//     element, Ask ::= ask(Constant, Question, Handle), one ask/3 over the
//     union of those functors (vGLP #4 Cowork, 2026-10-02 08:26 UTC, Q1: TGLP
//     refuses two alternatives of one functor, "Two alternatives with the
//     same functor are not allowed", typed-glp.tex, so the asks of two types
//     cannot each be an alternative of their own); each procedure that
//     reaches a question declared with Stream(Ask) added, the asking clause
//     with the source declaration's argument types and it, and q with the
//     argument of T in its mode, the handle and it.
//
// Not here: the initial goal and the dispatcher on its ask stream and the
// person channel (item F.5, with Part 2).
//
// THE NAMES.  The asking clause takes the source name, q_a = q, so a call of
// q in a body is already the call of its asking clause; the (n+3)-ary
// procedure takes `q1`, made fresh against every name the program uses, and
// the three types the compilation adds take `Ask`, `Handle` and `Question`,
// made fresh against the program's types.
//
// THE ASK.  T is the constant naming the moded interactive type as written,
// 'Menu' or 'Menu?'; t is the type as written, each name with its first
// letter lowercased, the names joined by `_`, then `_r` in reader mode and
// `_w` in writer mode: menu_r, menu_w (questionFunctorStem); two moded types
// giving the same t are told apart by `_2`, `_3`, ... in the order of their
// declarations.

import '../compiler/ast.dart';
import '../compiler/error.dart';
import '../compiler/glp_printer.dart';
import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../compiler/token.dart';
import '../analysis/type_checker/type_ast.dart'
    show ConstantAlt, ProcDecl, StructAlt, TypeDef, TypeExpr, TypeRef;
import 'mediator.dart' show printTypeDef, typeSource;
import 'program_compilation.dart' show compiledHeader;

/// The functor of the ask a goal of a volitional procedure sends on its ask
/// stream, ask(T, X, W?) (vGLP, Definition "Canonical Compilation").
const askFunctor = 'ask';

/// What a clause whose interactive term is the anonymous variable binds the
/// handle to (vGLP, Definition "Canonical Compilation").
const withdrawHandle = 'withdraw';

/// The goal that merges two ask streams into one (vGLP, Definition "Canonical
/// Compilation": "merge goals merge them"), the root's merge/3.
const mergeGoal = 'merge';

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

  /// The type parameters of the procedure's declaration.
  final List<String> typeParams;

  /// The functor of T in Question, set once every interactive type of the
  /// program is known.
  late final String functor;

  final int line, column;

  VolitionalProcedure(this.name, this.arity, this.interactiveType,
      this.readerMode, this.guardedName, this.typeParams, this.line,
      this.column);

  /// The name of the asking clause.
  String get askingName => name;

  /// The constant naming the moded interactive type as written.
  String get typeConstant => interactiveType.toString();
}

/// The canonical compilation of one source: the GLP module's text, the module
/// it parses to, the volitional procedures it compiled, the procedures that
/// reach a question by their source signature p/n, the names of the three
/// types it adds, and the functor of each moded interactive type in Question.
class CanonicalProgram {
  final String source;
  final Module module;
  final List<VolitionalProcedure> volitional;
  final Set<String> reaching;
  final String askType;
  final String handleType;
  final String questionType;

  /// The functor of each moded interactive type in Question, by the type as
  /// written: 'Request?' request_r.
  final Map<String, String> functors;

  CanonicalProgram(this.source, this.module, this.volitional, this.reaching,
      this.askType, this.handleType,
      {required this.questionType, required this.functors});
}

/// The functor the compilation gives a moded interactive type in Question
/// (vGLP #4 Cowork, 2026-10-02 08:26 UTC, Q1): the type as written, each name
/// with its first letter lowercased, the names joined by `_` in prefix order,
/// then `_r` in reader mode and `_w` in writer mode --- `Request?` gives
/// request_r, `Card` card_w, `Stream(String)?` stream_string_r.  The caller
/// makes two that coincide distinct.
String questionFunctorStem(TypeExpr t, bool readerMode) =>
    '${_functorStem(t)}_${readerMode ? 'r' : 'w'}';

String _functorStem(TypeExpr t) {
  if (t is TypeRef) {
    final head = '${t.name[0].toLowerCase()}${t.name.substring(1)}';
    return [head, for (final a in t.typeArgs) _functorStem(a)].join('_');
  }
  return 'any';
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
    volitional[sig] = VolitionalProcedure(v.name, v.arity, t,
        decl.isInputArg(v.arity), guarded, decl.typeParams, v.line, v.column);
  }

  // The functor of each moded interactive type in Question, in the order of
  // the declarations; two that coincide told apart (vGLP #4 Cowork,
  // 2026-10-02 08:26 UTC, Q1).
  final functors = <String, String>{};
  final functorsTaken = <String>{};
  for (final v in volitional.values) {
    final written = typeSource(v.interactiveType);
    final f = functors.putIfAbsent(written, () {
      final stem = questionFunctorStem(v.interactiveType, v.readerMode);
      var name = stem;
      for (var n = 2; functorsTaken.contains(name); n++) {
        name = '${stem}_$n';
      }
      functorsTaken.add(name);
      return name;
    });
    v.functor = f;
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

  // The (n+1)-ary procedures of the volitional procedures, by their parsed
  // signature q/(n+1).
  final askedProcs = {
    for (final v in volitional.values) '${v.name}/${v.arity + 1}': v
  };
  final reaching = _reaching(m, volitional, askedProcs);

  // The types the compilation adds.
  final typeNames = {for (final td in m.typeDefs) td.name};
  final added = _addedTypes(volitional.values, declsByKey,
      ask: _freshType('Ask', typeNames),
      handle: _freshType('Handle', typeNames),
      question: _freshType('Question', typeNames));

  // The compiled procedures, each with its declaration, in source order; a
  // volitional procedure becomes its asking clause and its (n+3)-ary
  // procedure, and a procedure that reaches a question is extended.
  final out = <_Emitted>[];
  for (final p in m.procedures) {
    final v = askedProcs[p.signature];
    if (v == null) {
      final decl = declsByKey[p.signature];
      if (!reaching.contains(p.signature)) {
        out.add(_Emitted(decl, p));
        continue;
      }
      out.add(_Emitted(
          decl == null ? null : _withAskStream(decl, decl.name, const [], added),
          Procedure(p.name, p.arity + 1, [
            for (final c in p.clauses)
              _extended(c, p.name, c.head.args, reaching)
          ], p.line, p.column)));
      continue;
    }
    final decl = declsByKey[p.signature]!;
    out.add(_Emitted(_askingDeclaration(decl, v, added), _askingClause(decl, v)));
    out.add(_Emitted(
        _withAskStream(decl, v.guardedName,
            [TypeRef(added.handle, decl.line, decl.column)], added),
        _guardedProcedure(p, v, reaching)));
  }
  // Declarations with no clauses of their own: imported procedures, and
  // declarations of procedures the runtime implements.
  final defined = {for (final p in m.procedures) p.signature};
  final bare = [
    for (final d in m.procDeclarations)
      if (!defined.contains(d.key)) d
  ];

  final source = _emit(m, added.typeDefs, out, bare);
  final module = Parser(Lexer(source).tokenize()).parseModule();
  return CanonicalProgram(source, module, volitional.values.toList(),
      reaching, added.ask, added.handle,
      questionType: added.question, functors: functors);
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
/// produced one in writer mode; and written `_Name` or `_Name?`, an anonymous
/// variable being any variable whose name begins with `_` (TGLP, "Anonymous
/// variables"; vGLP's task of 2026-10-02 00:52 UTC, item 2).
void _checkNoAnonymousOutput(
    Module m, Map<String, VolitionalProcedure> volitional) {
  for (final v in volitional.values) {
    if (v.readerMode) continue;
    for (final p in m.procedures) {
      if (p.signature != '${v.name}/${v.arity + 1}') continue;
      for (final c in p.clauses) {
        if (!_isAnonymous(c.head.args.last)) continue;
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


/// A type name, [stem] or `stem_N`, fresh against [taken].
String _freshType(String stem, Set<String> taken) {
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
// Reaching a question
// ---------------------------------------------------------------------------

/// The procedures that reach a question, by their source signature p/n: "A
/// procedure of M reaches a question if it is volitional or a clause of it
/// calls a procedure that does" (Definition "Canonical Compilation"), the
/// least fixpoint over the program's calls.  A volitional procedure is named
/// by its source arity, which is the arity of its calls.  A remote call
/// M # p(...) calls no procedure of the program.
Set<String> _reaching(Module m, Map<String, VolitionalProcedure> volitional,
    Map<String, VolitionalProcedure> askedProcs) {
  final reaching = {for (final v in volitional.values) '${v.name}/${v.arity}'};
  var changed = true;
  while (changed) {
    changed = false;
    for (final p in m.procedures) {
      if (askedProcs.containsKey(p.signature) ||
          reaching.contains(p.signature)) {
        continue;
      }
      final calls = p.clauses.expand((c) => _calls(c.body ?? const []));
      if (calls.any((g) => reaching.contains('${g.functor}/${g.args.length}'))) {
        reaching.add(p.signature);
        changed = true;
      }
    }
  }
  return reaching;
}

/// Whether the body goal [g] calls a procedure that reaches a question: a
/// placed goal by its inner goal, and a remote goal never.
bool _callsReaching(Goal g, Set<String> reaching) {
  if (g is RemoteGoal) return false;
  final call = g is SpawnGoal ? g.innerGoal : g;
  return reaching.contains('${call.functor}/${call.args.length}');
}

/// The body goal [g] with [stream] added as its last argument, a placed goal
/// to its inner goal.
Goal _withStream(Goal g, Term stream) {
  if (g is SpawnGoal) {
    final inner = g.innerGoal;
    return SpawnGoal(
        Goal(inner.functor, [...inner.args, stream], inner.line, inner.column),
        g.agentId,
        g.line,
        g.column);
  }
  return Goal(g.functor, [...g.args, stream], g.line, g.column);
}

// ---------------------------------------------------------------------------
// The types the compilation adds
// ---------------------------------------------------------------------------

/// The handle's type, [handle] ::= withdraw; the questions, [question], one
/// alternative per moded interactive type, its functor wrapping the type as
/// written in its mode, t(T); and the ask stream's element, [ask],
/// ask(Constant, Question, Handle), one ask/3 over the union of the functors
/// (vGLP #4 Cowork, 2026-10-02 08:26 UTC, Q1).  An interactive type that names
/// a type parameter of its procedure gives Question and Ask that parameter,
/// and each declaration with an ask stream takes it.
class _AddedTypes {
  final String ask;
  final String handle;
  final String question;
  final List<String> params;

  /// Each moded interactive type, once, with its functor.
  final List<(TypeExpr, String)> interactiveTypes;

  _AddedTypes(this.ask, this.handle, this.question, this.params,
      this.interactiveTypes);

  List<TypeRef> _paramRefs(int l, int c) =>
      [for (final p in params) TypeRef(p, l, c)];

  /// Stream(Ask), the type of an ask stream.
  TypeRef stream(int l, int c) => TypeRef('Stream', l, c, typeArgs: [
        TypeRef(ask, l, c, typeArgs: _paramRefs(l, c))
      ]);

  /// A declaration's type parameters with the ask type's added.
  List<String> paramsFor(List<String> own) =>
      [...own, for (final p in params) if (!own.contains(p)) p];

  List<TypeDef> get typeDefs => [
        TypeDef(handle, [ConstantAlt(withdrawHandle, 0, 0)], 0, 0),
        TypeDef(
            question,
            [
              for (final (t, f) in interactiveTypes) StructAlt(f, [t], 0, 0)
            ],
            0,
            0,
            typeParams: params),
        TypeDef(
            ask,
            [
              StructAlt(askFunctor, [
                TypeRef('Constant', 0, 0),
                TypeRef(question, 0, 0, typeArgs: _paramRefs(0, 0)),
                TypeRef(handle, 0, 0),
              ], 0, 0)
            ],
            0,
            0,
            typeParams: params),
      ];
}

_AddedTypes _addedTypes(Iterable<VolitionalProcedure> volitional,
    Map<String, ProcDecl> declsByKey,
    {required String ask, required String handle, required String question}) {
  final types = <(TypeExpr, String)>[];
  final seen = <String>{};
  final params = <String>[];
  void collect(TypeExpr t, List<String> own) {
    if (t is TypeRef) {
      if (own.contains(t.name) && !params.contains(t.name)) params.add(t.name);
      for (final a in t.typeArgs) {
        collect(a, own);
      }
    }
  }

  for (final v in volitional) {
    final decl = declsByKey['${v.name}/${v.arity + 1}']!;
    final t = v.interactiveType;
    collect(t, decl.typeParams);
    if (seen.add(typeSource(t))) types.add((t, v.functor));
  }
  return _AddedTypes(ask, handle, question, params, types);
}

// ---------------------------------------------------------------------------
// The compiled procedures
// ---------------------------------------------------------------------------

/// A procedure's declaration with [extra] argument types and then the ask
/// stream, Stream(Ask), added.
ProcDecl _withAskStream(ProcDecl decl, String name, List<TypeExpr> extra,
        _AddedTypes added, {bool? exported}) =>
    ProcDecl(
        name,
        [...decl.argTypes, ...extra, added.stream(decl.line, decl.column)],
        decl.line,
        decl.column,
        typeParams: added.paramsFor(decl.typeParams),
        exported: exported ?? decl.exported);

/// The asking clause's declaration: the source declaration's argument types
/// and the ask stream.
ProcDecl _askingDeclaration(
        ProcDecl decl, VolitionalProcedure v, _AddedTypes added) =>
    ProcDecl(
        v.askingName,
        [
          ...decl.argTypes.sublist(0, v.arity),
          added.stream(decl.line, decl.column)
        ],
        decl.line,
        decl.column,
        typeParams: added.paramsFor(decl.typeParams),
        exported: decl.exported);

/// The asking clause
///
///     q(S1, ..., Sn, [ask(T, t(X), W?) | D?]) :- q1(S1', ..., Sn', X?, W, D).
///
/// S'_l the reader of S_l at an input position and the writer at an output
/// position, the head carrying the pair's other end; X and X? exchanged where
/// T is in writer mode; t the functor of T in Question.
Procedure _askingClause(ProcDecl decl, VolitionalProcedure v) {
  final l = decl.line, c = decl.column;
  final head = <Term>[];
  final passed = <Term>[];
  for (var k = 0; k < v.arity; k++) {
    final s = 'S${k + 1}';
    final input = decl.isInputArg(k);
    head.add(VarTerm(s, !input, l, c));
    passed.add(VarTerm(s, input, l, c));
  }
  final ask = StructTerm(askFunctor, [
    ConstTerm(v.typeConstant, l, c),
    StructTerm(v.functor, [VarTerm('X', !v.readerMode, l, c)], l, c),
    VarTerm('W', true, l, c),
  ], l, c);
  final clause = Clause(
      Atom(v.askingName, [...head, ListTerm(ask, VarTerm('D', true, l, c), l, c)],
          l, c),
      body: [
        Goal(
            v.guardedName,
            [
              ...passed,
              VarTerm('X', v.readerMode, l, c),
              VarTerm('W', false, l, c),
              VarTerm('D', false, l, c),
            ],
            l,
            c),
      ],
      line: l,
      column: c);
  return Procedure(v.askingName, v.arity + 1, [clause], l, c);
}

/// The clauses of q as guarded clauses of arity n+1, named q1, with the handle
/// added after the interactive term and then extended (Definition "Canonical
/// Compilation").  A clause whose interactive term is the anonymous variable
/// keeps it at the interactive position, where it drops the reader, and no
/// goal is added (item B': no built-in).
Procedure _guardedProcedure(
    Procedure p, VolitionalProcedure v, Set<String> reaching) {
  final clauses = [
    for (final c in p.clauses)
      _extended(c, v.guardedName, [...c.head.args, _handle(c.head.args.last)],
          reaching)
  ];
  return Procedure(v.guardedName, v.arity + 3, clauses, p.line, p.column);
}

/// The handle in the head of a clause whose interactive term is [a]: withdraw
/// where [a] is the anonymous variable, `_` or `_Name` (vGLP's task of
/// 2026-10-02 00:52 UTC, item 2), and the anonymous variable in every other
/// clause, written `_?`, TGLP's anonymous output, the handle's being a
/// produced position.
Term _handle(Term a) => _isAnonymous(a) && !_isReader(a)
    ? ConstTerm(withdrawHandle, a.line, a.column)
    : UnderscoreTerm(a.line, a.column, isReader: true);

/// Whether [t] is an anonymous variable, in either mode: `_`, `_?`, or a
/// variable whose name begins with `_` (TGLP, "Anonymous variables").
bool _isAnonymous(Term t) =>
    t is UnderscoreTerm || (t is VarTerm && t.name.startsWith('_'));

bool _isReader(Term t) =>
    (t is UnderscoreTerm && t.isReader) || (t is VarTerm && t.isReader);

/// A clause of a procedure that reaches a question, named [name], its head's
/// arguments [headArgs] and then its ask stream (Definition "Canonical
/// Compilation"): each body call of a procedure that reaches a question is
/// given a fresh writer as its ask stream, and the head carries D?, D the
/// clause's ask stream --- the one such call given D itself, more merged into
/// D by merge goals, a chain of them after the body's own goals --- and []
/// in place of D? where there is none.
Clause _extended(
    Clause c, String name, List<Term> headArgs, Set<String> reaching) {
  final body = c.body ?? const <Goal>[];
  final l = c.head.line, col = c.head.column;
  final k = body.where((g) => _callsReaching(g, reaching)).length;
  if (k == 0) {
    return Clause(
        Atom(name, [...headArgs, ListTerm(null, null, l, col)], l, col),
        guards: c.guards,
        body: c.body,
        line: c.line,
        column: c.column);
  }
  final fresh = _FreshVariables(c);
  final d = fresh.next('D');
  final streams = k == 1 ? [d] : [for (var i = 0; i < k; i++) fresh.next('D')];
  var i = 0;
  final extended = <Goal>[
    for (final g in body)
      _callsReaching(g, reaching)
          ? _withStream(g, VarTerm(streams[i++], false, g.line, g.column))
          : g
  ];
  var merged = streams.first;
  for (var j = 1; j < k; j++) {
    final into = j == k - 1 ? d : fresh.next('D');
    extended.add(Goal(mergeGoal, [
      VarTerm(merged, true, l, col),
      VarTerm(streams[j], true, l, col),
      VarTerm(into, false, l, col),
    ], l, col));
    merged = into;
  }
  return Clause(Atom(name, [...headArgs, VarTerm(d, true, l, col)], l, col),
      guards: c.guards, body: extended, line: c.line, column: c.column);
}

/// Variable names fresh against a clause's.
class _FreshVariables {
  final Set<String> _taken = {};

  _FreshVariables(Clause c) {
    void scan(Term t) {
      if (t is VarTerm) _taken.add(t.name);
      if (t is StructTerm) t.args.forEach(scan);
      if (t is ListTerm) {
        if (t.head != null) scan(t.head!);
        if (t.tail != null) scan(t.tail!);
      }
    }

    c.head.args.forEach(scan);
    for (final g in c.guards ?? const <Guard>[]) {
      g.args.forEach(scan);
    }
    for (final g in c.body ?? const <Goal>[]) {
      g.args.forEach(scan);
    }
  }

  /// [stem] if it is fresh, else the first of stem1, stem2, ... that is.
  String next(String stem) {
    var name = stem;
    var n = 1;
    while (_taken.contains(name)) {
      name = '$stem$n';
      n++;
    }
    _taken.add(name);
    return name;
  }
}

// ---------------------------------------------------------------------------
// Emission
// ---------------------------------------------------------------------------

class _Emitted {
  final ProcDecl? decl;
  final Procedure procedure;
  _Emitted(this.decl, this.procedure);
}

String _emit(Module m, List<TypeDef> added, List<_Emitted> procs,
    List<ProcDecl> bare) {
  final b = StringBuffer();
  final printer = SourcePrinter();
  b.write(compiledHeader);
  b.writeln();

  for (final td in m.typeDefs) {
    b.writeln(printTypeDef(td));
  }
  b.writeln();
  for (final td in added) {
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
