// glp_runtime/lib/vglp/canonical.dart
//
// The canonical compilation of a vGLP program written in the paper's syntax.
// Spec: vGLP at c994328 --- sections/vglp.tex, Definition "Guarded Clause,
// Volitional Procedure, Interactive Type, Interactive Term, Ordinary Clause,
// Procedure, vGLP Program"; sections/elicitation.tex, Definition "Canonical
// Compilation" and the paragraph before it.  (_) and the handle are out of the
// language (Udi, 2026-10-03; vGLP c994328, c2e8b57, as vGLP #5 Cowork reports
// it, 2026-10-03 08:16 UTC).
//
// The syntax:
//
//     procedure (T)*p(T1, ..., Tn).
//     (A)*p(S1, ..., Sn) :- G | B.
//
// T is the interactive type, in writer or reader mode as an argument type is;
// A, the interactive term, "is a term of type T, possibly a variable but not
// the anonymous variable": `_`, `_?`, `_Name` and `_Name?` are refused in
// either mode, an anonymous variable being any variable whose name begins
// with `_` (GLP-Spec, Remark "Anonymous Variables").  The clause "is the
// guarded clause p(S1, ..., Sn, A) :- G | B, of arity n+1" (Definition
// "Guarded Clause, ..."), so the front end reads it as exactly that: a token
// rewrite puts A after the last argument, and T after the last argument type,
// and the ordinary GLP parser reads the result.
//
// The compilation, by the Definition:
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
//     guarded clauses of arity n+1, extended and with their calls replaced
//     likewise: a compiled clause of arity n+2, its ask stream and nothing
//     else added;
//   - the asking clause
//         q_a(S1, ..., Sn, [ask(T, t(X)) | D?]) :- q(S1', ..., Sn', X?, D).
//     S'_l the reader of S_l at an input position and the writer at an output
//     position, the head carrying the pair's other end; X and X? exchanged
//     where T is in writer mode; t the functor of T in Question (below);
//   - typed: the types of the source; the questions, Question ::= t1(T1) ;
//     ... ; tk(Tk), one functor per moded interactive type, each type as
//     written in its mode; the ask stream's element, Ask ::= ask(Constant,
//     Question), one ask/2 over the union of those functors (vGLP #4 Cowork,
//     2026-10-02 08:26 UTC, Q1: TGLP refuses two alternatives of one functor,
//     "Two alternatives with the same functor are not allowed", typed-glp.tex,
//     so the asks of two types cannot each be an alternative of their own);
//     each procedure that reaches a question declared with Stream(Ask) added,
//     the asking clause with the source declaration's argument types and it,
//     and q with the argument of T in its mode and it;
//   - "A vGLP program is organised in modules as a GLP program is ... A
//     procedure that reaches a question is exported, imported and shared as
//     any procedure is, with its ask stream added, so the questions of a
//     program are those of all its modules.  The compilation forms the type
//     of the asks, the construct processes and the dispatcher for the linked
//     program" (vGLP at a89a751, sections/parts/canonical-compilation.tex;
//     vGLP's task of 2026-10-15 18:28 UTC, which lifts the one-module rule
//     of 2026-10-13 00:43 UTC): an import of a volitional procedure of
//     another module, `imported procedure (T)*N#p(T1, ..., Tn)`, is the
//     import of its asking clause, `imported procedure N#p(T1, ..., Tn,
//     Stream(Ask))`; an import of an ordinary procedure of another module
//     that reaches a question, where N's source tells it ([ReachingReader]),
//     has its ask stream added; and a call of either, or of a procedure of
//     the source's scope that reaches a question (a self.vglp's,
//     [CanonicalContext.scopeReaching]), is given a fresh ask stream, merged
//     as a call within the module is.  Where the loader compiles the source
//     as a module of a linked program, the questions, the construct
//     processes and the dispatcher are those of the program
//     ([CanonicalContext.interactive]), so that every module's asks are of
//     one type;
//   - the dispatcher and the construct processes (Part 2): the dispatcher's
//     generic source, programs/vglp/dispatcher.glp, its names made fresh
//     against the program's (dispatcher.dart), and the clause of construct/4
//     of each interactive type with the clauses it calls, typed at the type
//     (constructs.dart).  Without the generic source the compilation emits
//     neither, which only a test of the first half asks for.
//
// The initial goal and the dispatcher on its ask stream and the person channel
// are spawned by whatever spawns the initial goal --- the bridge for the app, a
// play or the REPL in a test (vGLP #4 Cowork, 2026-10-02 08:26 UTC, item 5):
// the compiled program exports dispatch/3 for it.
//
// THE NAMES.  The asking clause takes the source name, q_a = q, so a call of
// q in a body is already the call of its asking clause; the (n+2)-ary
// procedure takes `q1`, made fresh against every name the program uses, and
// the two types the compilation adds take `Ask` and `Question`, made fresh
// against the program's types.
//
// THE ASK.  T is the constant naming the moded interactive type as written,
// 'Menu' or 'Menu?'; t is the type as written, each name with its first
// letter lowercased, the names joined by `_`, then `_r` in reader mode and
// `_w` in writer mode: menu_r, menu_w (questionFunctorStem); two moded types
// giving the same t are told apart by `_2`, `_3`, ... in the order of their
// declarations.
//
// WIDGET DECLARATIONS, T =::= W (Definition "Widget Declaration, Default
// Widget"; W an atom, vGLP 2026-10-01 23:55 UTC, E, Q3), are read from the
// source text before it is lexed, GLP's lexer having no token =::= (vGLP #4
// Cowork, 2026-10-02 08:26 UTC, item 7).  A declaration in the source holds
// in its module; those of its scope, read from the self.vglp beside each
// self.glp of its ancestor chain, the root's included, are given by the
// caller, and the source's own override them (vGLP #5 Cowork, 2026-10-03
// 08:16 UTC, item 7: "Widget declarations are scoped as type declarations
// are: a declaration at the root holds for every program, one in a module
// holds in that module, and a local declaration overrides a global one").
// A declared widget is drawn as named(W, D), D the default widget of its
// type, from which W reads the type's constructors (vGLP 92e6d3b, Sections
// 6.2 and 6.3; constructs.dart, _widget).

import '../compiler/ast.dart';
import '../compiler/error.dart';
import '../compiler/glp_printer.dart';
import '../compiler/lexer.dart';
import '../compiler/parser.dart';
import '../compiler/token.dart';
import '../analysis/type_checker/type_ast.dart'
    show
        DiffListAlt,
        ListConsAlt,
        ProcDecl,
        StructAlt,
        TypeDef,
        TypeEnvironment,
        TypeExpr,
        TypeRef;
import 'constructs.dart';
import 'dispatcher.dart';
import 'mediator.dart' show printTypeDef, typeSource;
import 'program_compilation.dart' show compiledHeader;

/// The functor of the ask a goal of a volitional procedure sends on its ask
/// stream, ask(T, t(X)) (vGLP, Definition "Canonical Compilation").
const askFunctor = 'ask';

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

  /// The name of the (n+2)-ary procedure, the clauses of q with the
  /// interactive variable and the ask stream.
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

/// The procedures of another module N that reach a question, as an importer
/// reads them from N's source ([reachingSignatures]): by every signature an
/// import may name one by, each to its source signature p/n; null where N is
/// no vGLP module the compilation reads --- a GLP module, none of whose
/// procedures reaches a question.  [module] is the qualifier as the
/// declaration writes it, N or N1#...#Nk.
typedef ReachingReader = Map<String, String>? Function(String module);

/// An interactive type of the linked program, as the program's order gives
/// it: the moded type as its declaration writes it, its mode, the type
/// parameters of its procedure's declaration, where it is declared, and
/// [resolve], which reads a type name in the scope of the source that
/// declares it, so that a module of the program whose scope does not define
/// a name the type references carries a copy of its definition.
class ProgramInteractive {
  final TypeExpr type;
  final bool readerMode;
  final List<String> params;
  final int line, column;
  final TypeDef? Function(String name) resolve;

  /// The source that declares it, by its path.
  final String? source;

  /// The stem of its functor, t before any coincidence count, each name in
  /// it formed in the scope of the source that declares it, a name two
  /// modules of the program define prefixed (vGLP ed75782, sections/parts/
  /// canonical-compilation.tex; [VglpProgram]).  Null where the program
  /// does not give it: then the stem of the type as written.
  final String? functor;

  /// The widget declarations in scope where the volitional procedure asking
  /// it is declared, by moded type: "The widget of a question's type is the
  /// one named by the widget declaration in scope where the volitional
  /// procedure asking the question is declared" (vGLP ed75782, Definition
  /// "Default Widget").  Null where the program does not give them: then
  /// those of the source compiled.
  final Map<String, String>? widgets;

  ProgramInteractive(this.type, this.readerMode, this.params, this.line,
      this.column, this.resolve,
      {this.source, this.functor, this.widgets});
}

/// What the compilation of one source takes from the linked program it is a
/// module of (vGLP, sections/parts/canonical-compilation.tex: "The
/// compilation forms the type of the asks, the construct processes and the
/// dispatcher for the linked program").
class CanonicalContext {
  /// The interactive types of the linked program, every module's, in the
  /// program's order: the questions of the source's compiled module, its
  /// construct processes and its dispatcher are formed over these, so that
  /// the asks of every module are of one type.
  final List<ProgramInteractive> interactive;

  /// The procedures of the source's scope --- those of the self.vglp of each
  /// directory above it --- that reach a question, by the signature p/n a
  /// call names them by, each with its record where it is volitional and
  /// null where it is ordinary.
  final Map<String, VolitionalCall?> scopeReaching;

  /// Whether the type name is defined in the source's scope, its own types
  /// apart.
  final bool Function(String name) resolvable;

  /// Type and procedure names the compilation's own must be fresh against:
  /// those of the self.glp whose layer a self.vglp's compiled text joins.
  final Set<String> reservedTypes;
  final Set<String> reservedProcedures;

  /// Whether the source is a self.vglp: its compiled text joins its
  /// directory's self.glp, and carries neither the dispatcher nor the
  /// construct processes, which are each module's; its widget declarations
  /// are its scope's ([scopeWidgetDeclarations]).
  final bool layer;

  /// The source compiled, by its path, and the definition a type name
  /// denotes in its scope ([resolveSelf]).
  final String? self;
  final TypeDef? Function(String name)? resolveSelf;

  /// The module whose definition a type name denotes in the scope of a
  /// source of the program, as a key, or null where no module of the
  /// program's chains defines it ([VglpProgram.definerOf]): a question's
  /// type whose name the source's scope gives another definition is copied
  /// into it under a fresh name.
  final String? Function(String source, String name)? definerOf;

  CanonicalContext(
      {required this.interactive,
      this.scopeReaching = const {},
      required this.resolvable,
      this.reservedTypes = const {},
      this.reservedProcedures = const {},
      this.layer = false,
      this.self,
      this.resolveSelf,
      this.definerOf});
}

/// The canonical compilation of one source: the GLP module's text, the module
/// it parses to, the volitional procedures it compiled, the procedures that
/// reach a question by their source signature p/n, the names of the two
/// types it adds, the functor of each moded interactive type in Question, and
/// --- where the dispatcher's generic source was given --- the names of the
/// dispatcher's entry point and of the construct processes, and the widget of
/// each interactive type.
class CanonicalProgram {
  final String source;
  final Module module;
  final List<VolitionalProcedure> volitional;
  final Set<String> reaching;
  final String askType;
  final String questionType;

  /// The functor of each moded interactive type in Question, by the type as
  /// written: 'Request?' request_r.
  final Map<String, String> functors;

  /// dispatch/3, the compiled program's entry point to the dispatcher, as
  /// emitted; null without the generic source.
  final String? dispatchName;

  /// construct/4, as emitted; null without the generic source.  The
  /// dispatch/3 the compiled program exports takes [dispatchName].
  final String? constructName;

  /// The widget term of each interactive type, by the type as written.
  final Map<String, String> widgets;

  CanonicalProgram(this.source, this.module, this.volitional, this.reaching,
      this.askType,
      {required this.questionType,
      required this.functors,
      this.dispatchName,
      this.constructName,
      this.widgets = const {}});
}

/// The functor the compilation gives a moded interactive type in Question
/// (vGLP #4 Cowork, 2026-10-02 08:26 UTC, Q1): the type as written, each name
/// with its first letter lowercased, the names joined by `_` in prefix order,
/// then `_r` in reader mode and `_w` in writer mode --- `Request?` gives
/// request_r, `Card` card_w, `Stream(String)?` stream_string_r.  The caller
/// makes two that coincide distinct.
String questionFunctorStem(TypeExpr t, bool readerMode) =>
    questionFunctorStemBy(t, readerMode,
        (name) => '${name[0].toLowerCase()}${name.substring(1)}');

/// The same, each name in [t] formed by [name]: "the name of T in
/// lowercase, the names of its type arguments, so formed, appended" (vGLP
/// ed75782, sections/parts/canonical-compilation.tex), a name two modules of
/// the program define prefixed ([VglpProgram]).
String questionFunctorStemBy(
        TypeExpr t, bool readerMode, String Function(String name) name) =>
    '${_functorStem(t, name)}_${readerMode ? 'r' : 'w'}';

String _functorStem(TypeExpr t, String Function(String name) name) {
  if (t is TypeRef) {
    return [name(t.name), for (final a in t.typeArgs) _functorStem(a, name)]
        .join('_');
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

/// [isPaperSyntax] of a source's text, its widget declarations set aside: a
/// source with one is in the paper's syntax only if it is so without it.
bool isPaperSyntaxSource(String text) {
  final stripped = text.contains('=::=')
      ? extractWidgetDeclarations(text).stripped
      : text;
  return isPaperSyntax(Lexer(stripped).tokenize());
}

/// Compile [text], a vGLP program in the paper's syntax, by the canonical
/// compilation.
///
/// [dispatcher] is the generic source of the dispatcher and the construct
/// processes, programs/vglp/dispatcher.glp; without it the compilation emits
/// neither, which only a test of the first half, and a self.vglp, ask for.
/// [scope] is the source's scope, whose types the construct processes are
/// built from where the source does not define them; without it, the root's.
/// [scopeWidgets] are the widget declarations of that scope, by moded type,
/// which the source's own override.  [reachingOf] reads, of a module the
/// source imports from, the procedures that reach a question: "A procedure
/// that reaches a question is exported, imported and shared as any procedure
/// is, with its ask stream added" (vGLP, sections/parts/canonical-
/// compilation.tex), so an import of one is given its ask stream, and so is
/// each call of it.  Without it an import of a volitional procedure,
/// `imported procedure (T)*N#p(...)`, is known by its declaration still, and
/// every other import is an import as GLP's are.  [context] is the linked
/// program the source is a module of, where the loader compiles it
/// ([CanonicalContext]); without it the questions are those of the source's
/// own volitional procedures and of those it imports.
CanonicalProgram compileCanonical(String text,
    {DispatcherSource? dispatcher,
    TypeEnvironment? scope,
    Map<String, String> scopeWidgets = const {},
    ReachingReader? reachingOf,
    CanonicalContext? context}) {
  final widgetDecls = extractWidgetDeclarations(text);
  final parsed = _parse(widgetDecls.stripped);
  final m = parsed.module;
  final layer = context?.layer ?? false;

  _checkNoOldDesign(m);

  // The volitional procedures, by their source signature p/n.
  final declsByKey = {
    for (final d in m.procDeclarations)
      if (!d.imported) d.key: d
  };
  final procsByKey = {for (final p in m.procedures) p.signature: p};
  final taken = _namesUsed(m)..addAll(context?.reservedProcedures ?? const {});
  final volitional = <String, VolitionalProcedure>{};
  for (final v in parsed.declarations) {
    if (v.module != null) continue;  // an imported one: below
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

  // The volitional procedures the source imports, `imported procedure
  // (T)*N#p(T1, ..., Tn)`, by N#p/n: each the import of its asking clause.
  final importedVolitional = _importedVolitional(parsed);

  // Whether a call reaches a question through a procedure that is not the
  // source's own: a call N # p(...) of a procedure of N that reaches one,
  // read off N's source ([reachingOf]) or declared volitional by the
  // import; and a call p(...) of a procedure of the source's scope that
  // reaches one, the source defining no p of its own.
  final read = <String, Map<String, String>?>{};
  Map<String, String>? reachingIn(String module) =>
      read.putIfAbsent(module, () => reachingOf?.call(module));
  bool importReaches(String module, String sig) =>
      importedVolitional.containsKey('$module#$sig') ||
      reachingIn(module)?[sig] == sig;
  final own = <String>{
    ...procsByKey.keys,
    ...declsByKey.keys,
    for (final v in volitional.values) '${v.name}/${v.arity}',
  };
  bool external(Goal g) {
    if (g is RemoteGoal) {
      final inner = g.goal;
      if (inner is RemoteGoal) return false;
      return importReaches(
          g.staticModuleName, '${inner.functor}/${inner.arity}');
    }
    final sig = '${g.functor}/${g.args.length}';
    return !own.contains(sig) &&
        (context?.scopeReaching.containsKey(sig) ?? false);
  }

  _checkNoAskedCall(m, volitional);
  _checkNoAnonymousTerm(m, volitional);
  _checkLaterClausesRead(m, volitional, declsByKey);
  _checkWritersAtOutputs(m, volitional, declsByKey,
      imported: {
        for (final e in importedVolitional.entries) e.key: e.value.$1
      },
      scope: {
        for (final e in (context?.scopeReaching ?? const {}).entries)
          if (e.value != null && !own.contains(e.key)) e.key: e.value!
      });

  // The definitions of the types the program's questions name that the
  // source's scope does not define, each copied from the scope of the source
  // that declares the question: a type is its automaton, whatever module
  // defines it (TGLP modules.tex, "Structural type compatibility").  A name
  // the source's scope gives another definition than the declaring source's
  // --- two modules each defining a Card --- is copied under a fresh name,
  // and the names in the copy that are so renamed are renamed in it, so that
  // the question is of the declaring module's type.
  final ownTypes = {for (final td in m.typeDefs) td.name: td};
  final copies = <String, TypeDef>{};
  // The emitted type of each question of the linked program, in its order.
  final emittedTypes = <TypeExpr>[];
  if (context != null) {
    // The name each type name of a declaring source is emitted under, by the
    // module that defines it and the name; and the copy each emitted name
    // holds, by the declaring source that gave it.
    final renamed = <String, String>{};
    final copiedFrom = <String, ProgramInteractive>{};
    TypeDef? here(String name) =>
        ownTypes[name] ?? context.resolveSelf?.call(name);
    bool resolvesHere(String name) =>
        ownTypes.containsKey(name) || context.resolvable(name);
    late String Function(ProgramInteractive, String, List<String>) nameIn;
    TypeExpr renameIn(ProgramInteractive pi, TypeExpr t, List<String> params) =>
        _renameTypeNames(t, (n) => nameIn(pi, n, params));
    String freshCopy(String name) {
      for (var n = 1;; n++) {
        final c = '${name}_$n';
        if (!ownTypes.containsKey(c) &&
            !copies.containsKey(c) &&
            !context.reservedTypes.contains(c) &&
            !context.resolvable(c)) {
          return c;
        }
      }
    }

    void copyAs(String emitted, TypeDef td, ProgramInteractive pi) {
      copiedFrom[emitted] = pi;
      copies[emitted] = td;  // its place first, as the copies are ordered
      final alts = [
        for (final alt in td.alternatives) renameIn(pi, alt, td.typeParams)
      ];
      var same = emitted == td.name;
      for (var i = 0; same && i < alts.length; i++) {
        same = identical(alts[i], td.alternatives[i]);
      }
      if (!same) {
        copies[emitted] = TypeDef(emitted, alts, td.line, td.column,
            typeParams: td.typeParams);
      }
    }

    nameIn = (pi, name, params) {
      if (params.contains(name)) return name;
      final source = pi.source;
      final def = source == null ? null : context.definerOf?.call(source, name);
      final key = '${def ?? '?'}\u0000$name';
      final known = renamed[key];
      if (known != null) return known;
      final td = pi.resolve(name);
      if (td == null) return renamed[key] = name;
      if (resolvesHere(name)) {
        final hereDef = context.self == null
            ? null
            : context.definerOf?.call(context.self!, name);
        if (def == hereDef ||
            _sameType(here(name), here, td, pi.resolve, <String>{})) {
          return renamed[key] = name;
        }
      } else {
        final taken = copiedFrom[name];
        if (taken == null) {
          renamed[key] = name;
          copyAs(name, td, pi);
          return name;
        }
        if (_sameType(taken.resolve(name), taken.resolve, td, pi.resolve,
            <String>{})) {
          return renamed[key] = name;
        }
      }
      final fresh = freshCopy(name);
      renamed[key] = fresh;
      copyAs(fresh, td, pi);
      return fresh;
    };

    for (final pi in context.interactive) {
      emittedTypes.add(renameIn(pi, pi.type, pi.params));
    }
  }

  // The interactive types the questions are formed over: the linked
  // program's, where the source is compiled as a module of one ("The
  // compilation forms the type of the asks, the construct processes and the
  // dispatcher for the linked program"), the source's own among them; else
  // the source's own and those of the volitional procedures it imports, in
  // the order of their declarations.  Each with the type its functor is
  // named by, as its declaring source writes it, its functor's stem, and the
  // widget declarations in scope where it is declared.
  final seeds = <(TypeExpr, bool, List<String>, int, int, TypeExpr, String?,
      Map<String, String>?)>[
    if (context != null)
      for (var i = 0; i < context.interactive.length; i++)
        (
          emittedTypes[i],
          context.interactive[i].readerMode,
          context.interactive[i].params,
          context.interactive[i].line,
          context.interactive[i].column,
          context.interactive[i].type,
          context.interactive[i].functor,
          context.interactive[i].widgets
        ),
  ];
  final seen = {for (final s in seeds) typeSource(s.$1)};
  for (final s in <(TypeExpr, bool, List<String>, int, int)>[
    for (final v in volitional.values)
      (v.interactiveType, v.readerMode, v.typeParams, v.line, v.column),
    for (final e in importedVolitional.values)
      (
        e.$2.argTypes.last,
        e.$2.isInputArg(e.$1.arity),
        e.$2.typeParams,
        e.$2.line,
        e.$2.column
      ),
  ]) {
    if (context == null || seen.add(typeSource(s.$1))) {
      seeds.add((s.$1, s.$2, s.$3, s.$4, s.$5, s.$1, null, null));
    }
  }

  // The functor of each moded interactive type in Question, in that order:
  // the Definition's t, each name in it formed in the scope of the source
  // that declares the question and prefixed where two modules of the
  // program define types of that name, as the program gives its stem
  // (vGLP ed75782, sections/parts/canonical-compilation.tex), two that
  // still coincide told apart (vGLP #4 Cowork, 2026-10-02 08:26 UTC, Q1).
  // Keyed by the type as emitted, and by the stem where the program gives
  // one.
  final functors = <String, String>{};
  final functorsTaken = <String>{};
  String functorOf(TypeExpr t, TypeExpr named, bool readerMode,
          String? given) =>
      functors.putIfAbsent(
          given == null ? typeSource(t) : '$given#${typeSource(t)}', () {
        final stem = given ?? questionFunctorStem(named, readerMode);
        var name = stem;
        for (var n = 2; functorsTaken.contains(name); n++) {
          name = '${stem}_$n';
        }
        functorsTaken.add(name);
        return name;
      });
  final interactive = <InteractiveType>[
    for (final s in seeds)
      InteractiveType(
          s.$1, s.$2, functorOf(s.$1, s.$6, s.$2, s.$7), s.$3, s.$4, s.$5,
          widgets: s.$8)
  ];
  // A volitional procedure's functor is its own question's: the source's
  // own type, as the program gives its stem.
  final ownStem = <String, String?>{
    if (context != null && context.self != null)
      for (final pi in context.interactive)
        if (pi.source == context.self) typeSource(pi.type): pi.functor,
  };
  for (final v in volitional.values) {
    v.functor = functorOf(v.interactiveType, v.interactiveType, v.readerMode,
        ownStem[typeSource(v.interactiveType)]);
  }

  // The (n+1)-ary procedures of the volitional procedures, by their parsed
  // signature q/(n+1).
  final askedProcs = {
    for (final v in volitional.values) '${v.name}/${v.arity + 1}': v
  };
  final reaching = _reaching(m, volitional, askedProcs, external);
  bool reaches(Goal g) => _reachesBy(g, reaching, external);

  // The questions, named fresh against the program's types; the dispatcher's
  // generic source, its names fresh against the program's and those, which
  // defines the asks, Ask(Q) (vGLP #4 Cowork, 2026-10-02 21:02 UTC, B); and
  // without the generic source, the compilation's own Ask(Q), the same.
  //
  // Fresh, too, against every type the source's declarations and types name
  // from its scope, and the questions name: a type of the source's own
  // shadows its scope's of that name within it, so a name the compilation
  // adds that a scope type of the source has would take that type from it,
  // a self.vglp's types among them.
  final typeNames = {
    ...ownTypes.keys,
    ...copies.keys,
    ...?context?.reservedTypes,
    for (final d in m.procDeclarations)
      for (final t in d.argTypes) ..._typeNames(t),
    for (final td in m.typeDefs)
      for (final a in td.alternatives) ..._typeNames(a),
    for (final s in seeds) ..._typeNames(s.$1),
  };
  final question = _freshType('Question', typeNames);
  // The type of what the person channel carries, the union of the values the
  // program draws, each under the functor of its ask, construct 0's under
  // output, and of the grants' raw inputs: the parameter T of the program's
  // person channel (vGLP, Definition "Construct Process"; vGLP 2026-10-14
  // 11:36 UTC, Udi: yes).
  final drawn = _freshType('Drawn', typeNames);
  final procTaken = {...taken};
  final inst = dispatcher == null
      ? null
      : instantiateDispatcher(dispatcher,
          freshType: (stem) => _freshType(stem, typeNames),
          freshProc: (stem) => _fresh(stem, procTaken));
  final added = _addedTypes(interactive,
      ask: inst?.type(askTypeName) ?? _freshType('Ask', typeNames),
      question: question,
      emitAsk: inst == null);

  // The compiled procedures, each with its declaration, in source order; a
  // volitional procedure becomes its asking clause and its (n+2)-ary
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
              _extended(c, p.name, c.head.args, reaches)
          ], p.line, p.column)));
      continue;
    }
    final decl = declsByKey[p.signature]!;
    out.add(_Emitted(_askingDeclaration(decl, v, added), _askingClause(decl, v)));
    out.add(_Emitted(_withAskStream(decl, v.guardedName, const [], added),
        _guardedProcedure(p, v, reaches)));
  }
  // Declarations with no clauses of their own: imported procedures, an
  // import of one that reaches a question with its ask stream added, and
  // declarations of procedures the runtime implements.
  final defined = {for (final p in m.procedures) p.signature};
  final bare = [
    for (final d in m.procDeclarations)
      if (!defined.contains(d.key))
        _bareCompiled(d, importedVolitional, importReaches, added)
  ];

  // Part 2: the dispatcher and the construct processes.
  _Elicitation? elicitation;
  if (dispatcher != null) {
    elicitation = _elicitation(
        dispatcher,
        inst!,
        m,
        interactive,
        added,
        procTaken,
        {...scopeWidgets, ...widgetDecls.byModedType},
        scope,
        drawn,
        copies);
  } else if (!layer && widgetDecls.byModedType.isNotEmpty) {
    final d = widgetDecls.positions.first;
    throw CompileError(
        'A widget declaration, and no dispatcher\'s generic source to build '
        'the construct processes it serves', d.$1, d.$2, phase: 'analyzer');
  }

  final source =
      _emit(m, copies.values.toList(), added.typeDefs, out, bare, elicitation);
  final module = Parser(Lexer(source).tokenize()).parseModule();
  return CanonicalProgram(source, module, volitional.values.toList(),
      reaching, added.ask,
      questionType: added.question,
      functors: functors,
      dispatchName: elicitation?.dispatchName,
      constructName: elicitation?.constructName,
      widgets: elicitation?.widgets ?? const <String, String>{});
}

/// The volitional procedures [parsed] imports, `imported procedure
/// (T)*N#p(T1, ..., Tn)`, by N#p/n, each with its record and its declaration
/// as the front end reads it, `imported procedure N#p(T1, ..., Tn, T)`.
Map<String, (VolitionalCall, ProcDecl)> _importedVolitional(_Parsed parsed) {
  final out = <String, (VolitionalCall, ProcDecl)>{};
  for (final v in parsed.declarations) {
    final module = v.module;
    if (module == null) continue;
    final decl = parsed.module.procDeclarations
        .where((d) =>
            d.imported &&
            d.modulePath == module &&
            d.name == v.name &&
            d.arity == v.arity + 1)
        .firstOrNull;
    if (decl == null) continue;
    out.putIfAbsent('$module#${v.name}/${v.arity}',
        () => (VolitionalCall.of(v.name, v.arity, decl), decl));
  }
  return out;
}

/// A declaration with no clauses as the compiled module carries it: the
/// import of a volitional procedure as the import of its asking clause, its
/// source arguments and the ask stream, "a volitional procedure's
/// declaration being its asking clause's" (vGLP, sections/parts/canonical-
/// compilation.tex); the import of an ordinary procedure that reaches a
/// question with its ask stream added; any other as it is.
ProcDecl _bareCompiled(
    ProcDecl d,
    Map<String, (VolitionalCall, ProcDecl)> importedVolitional,
    bool Function(String module, String sig) importReaches,
    _AddedTypes added) {
  final module = d.modulePath;
  if (!d.imported || module == null) return d;
  final n = d.arity - 1;
  final List<TypeExpr> args;
  if (n >= 0 && importedVolitional.containsKey('$module#${d.name}/$n')) {
    args = d.argTypes.sublist(0, n);
  } else if (importReaches(module, d.key)) {
    args = d.argTypes;
  } else {
    return d;
  }
  return ProcDecl(d.name, [...args, added.stream(d.line, d.column)], d.line,
      d.column,
      typeParams: added.paramsFor(d.typeParams),
      imported: true,
      modulePath: module);
}

/// The type names [t] references.
Iterable<String> _typeNames(TypeExpr t) sync* {
  if (t is TypeRef) {
    yield t.name;
    for (final a in t.typeArgs) {
      yield* _typeNames(a);
    }
  } else if (t is StructAlt) {
    for (final a in t.args) {
      yield* _typeNames(a);
    }
  } else if (t is ListConsAlt) {
    yield* _typeNames(t.head);
    yield* _typeNames(t.tail);
  } else if (t is DiffListAlt) {
    yield* _typeNames(t.content);
    yield* _typeNames(t.hole);
  }
}

/// [t] with each type name renamed by [rename], the same object where none
/// is renamed.
TypeExpr _renameTypeNames(TypeExpr t, String Function(String) rename) {
  if (t is TypeRef) {
    final name = rename(t.name);
    final args = [for (final a in t.typeArgs) _renameTypeNames(a, rename)];
    var same = name == t.name;
    for (var i = 0; same && i < args.length; i++) {
      same = identical(args[i], t.typeArgs[i]);
    }
    return same
        ? t
        : TypeRef(name, t.line, t.column, isInput: t.isInput, typeArgs: args);
  }
  if (t is StructAlt) {
    final args = [for (final a in t.args) _renameTypeNames(a, rename)];
    var same = true;
    for (var i = 0; same && i < args.length; i++) {
      same = identical(args[i], t.args[i]);
    }
    return same ? t : StructAlt(t.functor, args, t.line, t.column);
  }
  if (t is ListConsAlt) {
    final h = _renameTypeNames(t.head, rename);
    final tl = _renameTypeNames(t.tail, rename);
    return identical(h, t.head) && identical(tl, t.tail)
        ? t
        : ListConsAlt(h, tl, t.line, t.column);
  }
  if (t is DiffListAlt) {
    final c = _renameTypeNames(t.content, rename);
    final h = _renameTypeNames(t.hole, rename);
    return identical(c, t.content) && identical(h, t.hole)
        ? t
        : DiffListAlt(c, h, t.line, t.column);
  }
  return t;
}

/// Whether [a], in the scope [ra], and [b], in the scope [rb], are one type:
/// the same definition, alternative by alternative, each name in it one type
/// in the two scopes.  Two that are absent are one.
bool _sameType(TypeDef? a, TypeDef? Function(String) ra, TypeDef? b,
    TypeDef? Function(String) rb, Set<String> seen) {
  if (a == null || b == null) return a == null && b == null;
  if (!seen.add('${a.name}\u0000${b.name}')) return true;
  String body(TypeDef td) => '${td.typeParams.join(',')}::='
      '${td.alternatives.map(typeSource).join(' ; ')}';
  if (body(a) != body(b)) return false;
  for (final alt in a.alternatives) {
    for (final n in _typeNames(alt)) {
      if (a.typeParams.contains(n)) continue;
      if (!_sameType(ra(n), ra, rb(n), rb, seen)) return false;
    }
  }
  return true;
}

// ---------------------------------------------------------------------------
// The procedures of another module that reach a question
// ---------------------------------------------------------------------------
//
// "A procedure that reaches a question is exported, imported and shared as
// any procedure is, with its ask stream added, so the questions of a program
// are those of all its modules" (vGLP at a89a751, sections/parts/canonical-
// compilation.tex).  Which procedures of a module N reach a question is read
// off N's source ([reachingSignatures]), its own calls and those of the
// procedures it imports and calls from its scope; the loader reads them off
// every source of the program at once ([programReaching]), a module reaching
// a question through another's.

/// The procedures of the vGLP source [text] that reach a question, by every
/// signature an importer may name one by, each to its source signature p/n:
/// p/n itself; p/(n+1), the compiled procedure with its ask stream, which the
/// GLP module forming the initial goal imports, where no other procedure of
/// the source has that signature; and, of a volitional procedure q, q1/(n+2),
/// its guarded clauses under their fresh name.  [reachingOf] and
/// [scopeReaching] are as for [compileCanonical]: the procedures of the
/// modules [text] imports from, and of its scope, that reach one.  Null where
/// [text] is not read by the front end, the old syntax among them: no
/// procedure of it reaches a question by the canonical compilation, its own
/// compilation refusing it where it is no program.
Map<String, String>? reachingSignatures(String text,
    {ReachingReader? reachingOf,
    Map<String, VolitionalCall?> scopeReaching = const {}}) {
  final front = VglpFront.of(text);
  if (front == null) return null;
  final reaching = programReaching({'': front}, target: (_, g) {
    if (g is RemoteGoal) {
      final inner = g.goal;
      final sig = '${inner.functor}/${inner.arity}';
      return reachingOf?.call(g.staticModuleName)?[sig] == sig
          ? ('*', sig)
          : null;
    }
    final sig = '${g.functor}/${g.args.length}';
    return scopeReaching.containsKey(sig) ? ('*', sig) : null;
  })['']!;
  final out = <String, String>{};
  for (final s in reaching) {
    out[s] = s;
    final slash = s.lastIndexOf('/');
    final name = s.substring(0, slash);
    final n = int.parse(s.substring(slash + 1));
    final compiled = '$name/${n + 1}';
    if (front.volitional.containsKey(s) || !front.own.contains(compiled)) {
      out[compiled] = s;
    }
    final v = front.guardedNames[s];
    if (v != null) out['$v/${n + 2}'] = s;
  }
  // A source in neither syntax none of whose procedures reaches a question
  // is a GLP module.
  if (out.isEmpty && !isPaperSyntaxSource(text)) return null;
  return out;
}

/// A vGLP source as the loader reads it before it compiles any module of the
/// program: what the questions of the linked program, and which of its
/// procedures reach one, are read from.
class VglpFront {
  /// The source's own volitional procedures, by source signature p/n.
  final Map<String, VolitionalCall> volitional;

  /// The fresh name of each one's guarded clauses, by source signature.
  final Map<String, String> guardedNames;

  /// The volitional procedures it imports, by N#p/n.
  final Map<String, VolitionalCall> importedVolitional;

  /// Its interactive types, its own volitional procedures' and then those it
  /// imports, in the order of their declarations: the type, its mode, its
  /// procedure's type parameters, and where it is declared.
  final List<(TypeExpr, bool, List<String>, int, int)> interactive;

  /// The signatures the source defines or declares, a volitional procedure
  /// by its source arity: a call of one is the source's own.
  final Set<String> own;

  /// The calls of each of its procedures, by source signature.
  final Map<String, List<Goal>> calls;

  /// The source's own type definitions, by name.
  final Map<String, TypeDef> types;

  VglpFront._(this.volitional, this.guardedNames, this.importedVolitional,
      this.interactive, this.own, this.calls, this.types);

  /// The front of [text]; null where it is not read by the front end, the
  /// old syntax and a display declaration among what it does not read.
  static VglpFront? of(String text) {
    final _Parsed parsed;
    try {
      parsed = _parse(extractWidgetDeclarations(text).stripped);
    } on CompileError {
      return null;
    }
    final m = parsed.module;
    if (m.displayDecls.isNotEmpty) return null;
    final decls = {
      for (final d in m.procDeclarations)
        if (!d.imported) d.key: d
    };
    final taken = _namesUsed(m);
    final volitional = <String, VolitionalCall>{};
    final guarded = <String, String>{};
    final interactive = <(TypeExpr, bool, List<String>, int, int)>[];
    for (final v in parsed.declarations) {
      if (v.module != null) continue;
      final decl = decls['${v.name}/${v.arity + 1}'];
      if (decl == null) continue;
      final sig = '${v.name}/${v.arity}';
      if (volitional.containsKey(sig)) continue;
      volitional[sig] = VolitionalCall.of(v.name, v.arity, decl);
      guarded[sig] = _fresh('${v.name}1', taken);
      interactive.add((decl.argTypes.last, decl.isInputArg(v.arity),
          decl.typeParams, v.line, v.column));
    }
    final imported = <String, VolitionalCall>{};
    for (final e in _importedVolitional(parsed).entries) {
      imported[e.key] = e.value.$1;
      final d = e.value.$2;
      interactive.add((d.argTypes.last, d.isInputArg(e.value.$1.arity),
          d.typeParams, d.line, d.column));
    }
    final calls = <String, List<Goal>>{};
    for (final p in m.procedures) {
      final asked = p.arity > 0 &&
          volitional.containsKey('${p.name}/${p.arity - 1}');
      final sig = asked ? '${p.name}/${p.arity - 1}' : p.signature;
      calls.putIfAbsent(sig, () => []).addAll(
          p.clauses.expand((c) => _goals(c.body ?? const <Goal>[])));
    }
    return VglpFront._(
        volitional,
        guarded,
        imported,
        interactive,
        {...decls.keys, for (final p in m.procedures) p.signature,
          ...volitional.keys},
        calls,
        {for (final td in m.typeDefs) td.name: td});
  }
}

/// The procedures of each source of a program that reach a question, by
/// source signature, each source by its key in [fronts]: "A procedure of M
/// reaches a question if it is volitional or a clause of it calls a procedure
/// that does" (vGLP, Definition "Canonical Compilation"), the least fixpoint
/// over the calls of every module of the program.  [target] names the
/// procedure of another source a call that is not the caller's own reaches
/// --- a call N # p(...) the procedure p of the module N names, a call p(...)
/// the p of the caller's scope --- as its source's key and its signature, or
/// null where it names none; a key not in [fronts] stands for a procedure
/// known to reach one.  A call of a volitional procedure a source imports
/// reaches one by its declaration.
Map<String, Set<String>> programReaching(Map<String, VglpFront> fronts,
    {required (String, String)? Function(String key, Goal call) target}) {
  final reaching = {
    for (final e in fronts.entries) e.key: {...e.value.volitional.keys}
  };
  bool reachesTarget((String, String)? t) =>
      t != null &&
      (!fronts.containsKey(t.$1) || reaching[t.$1]!.contains(t.$2));
  bool callReaches(String key, VglpFront front, Goal g) {
    if (g is SpawnGoal) return callReaches(key, front, g.innerGoal);
    if (g is RemoteGoal) {
      final inner = g.goal;
      if (inner is RemoteGoal) return false;
      if (front.importedVolitional.containsKey(
          '${g.staticModuleName}#${inner.functor}/${inner.arity}')) {
        return true;
      }
      return reachesTarget(target(key, g));
    }
    final sig = '${g.functor}/${g.args.length}';
    if (reaching[key]!.contains(sig)) return true;
    if (front.own.contains(sig)) return false;
    return reachesTarget(target(key, g));
  }

  var changed = true;
  while (changed) {
    changed = false;
    for (final e in fronts.entries) {
      final r = reaching[e.key]!;
      for (final pc in e.value.calls.entries) {
        if (r.contains(pc.key)) continue;
        if (pc.value.any((g) => callReaches(e.key, e.value, g))) {
          r.add(pc.key);
          changed = true;
        }
      }
    }
  }
  return reaching;
}

// ---------------------------------------------------------------------------
// Part 2: the dispatcher and the construct processes
// ---------------------------------------------------------------------------

/// What the compilation emits of the dispatcher's generic source and the
/// construct processes.
class _Elicitation {
  final List<TypeDef> typeDefs;
  final List<ProcDecl> procDecls;
  final List<Procedure> procedures;
  final String constructs;
  final String dispatchName;
  final String constructName;
  final Map<String, String> widgets;

  _Elicitation(this.typeDefs, this.procDecls, this.procedures, this.constructs,
      this.dispatchName, this.constructName, this.widgets);
}

_Elicitation _elicitation(
    DispatcherSource dispatcher,
    InstantiatedDispatcher inst,
    Module m,
    List<InteractiveType> interactive,
    _AddedTypes added,
    Set<String> procTaken,
    Map<String, String> widgets,
    TypeEnvironment? scope,
    String drawn,
    Map<String, TypeDef> copies) {
  // Every name is fresh against the program's and against the names the
  // compilation has already given.
  String freshProc(String stem) => _fresh(stem, procTaken);
  final constructName = freshProc(constructHook);
  final constructsName = freshProc(constructsHook);

  final generic = GenericNames(
    dispatch: inst.proc(dispatchEntry),
    constructs: constructsName,
    runEnd: inst.proc('run_end'),
    run: (f) => freshProc('run_$f'),
    shown: inst.proc('shown'),
    thread: inst.proc('thread'),
    consViews: inst.proc('cons_views'),
    allDone: inst.proc('all_done'),
    path: inst.proc('path'),
    formString: inst.proc('form_string'),
    formInteger: inst.proc('form_integer'),
    formNumber: inst.proc('form_number'),
    formReal: inst.proc('form_real'),
    formConstant: inst.proc('form_constant'),
    formModule: inst.proc('form_module'),
    formedType: inst.type('Formed'),
    doneType: inst.type('Done'),
    drawn: drawn,
    drawType: '$drawTypeName($drawn)',
    inputType: '$inputTypeName($drawn)',
    inputsType: '${inst.type(inputsTypeName)}($drawn)',
    pathType: pathTypeName,
    personChannelType: '$personChannelTypeName($drawn)',
    spawnType: inst.type(spawnTypeName),
    output: (stem) => freshProc(stem),
    askType: added.ask,
  );

  // The source's own types first, then those copied from the program's
  // other modules, then its scope's, then the root's.
  final own = {for (final td in m.typeDefs) td.name: td};
  final outer = scope ?? TypeEnvironment.empty();
  TypeDef? resolve(String name) =>
      own[name] ??
      copies[name] ??
      outer.typeTemplates[name] ??
      outer.types[name];

  // Each interactive type once: its construct process is the program's
  // (Definition "Canonical Compilation").
  final types = <InteractiveType>[];
  final seen = <String>{};
  // Each by its functor: two modules' types of one name are two questions.
  for (final t in interactive) {
    if (!seen.add(t.functor)) continue;
    types.add(t);
  }
  // A declaration names a moded type and holds wherever a position of an
  // interactive variable is of it; one naming a type no position is of is
  // unused, as an unused type definition is.
  final constructs = buildConstructs(
    types: types,
    constructName: constructName,
    questionType: added.question,
    generic: generic,
    resolve: resolve,
    declared: widgets,
    fresh: freshProc,
    output: resolve(outputTypeName),
  );

  // The generic procedures emitted: those reached from the dispatcher's entry
  // point and from those the construct processes call.
  final reached =
      reachableGeneric(dispatcher, [dispatchEntry, ...GenericNames.used]);
  final emittedNames = {for (final g in reached) inst.procNames[g]!};
  return _Elicitation(
    inst.typeDefs,
    [
      for (final d in inst.procDecls)
        if (emittedNames.contains(d.name)) d
    ],
    [
      for (final p in inst.procedures)
        if (emittedNames.contains(p.name)) p
    ],
    constructs.source,
    inst.proc(dispatchEntry),
    constructName,
    constructs.widgets,
  );
}

// ---------------------------------------------------------------------------
// Widget declarations: T =::= W, read from the source text
// ---------------------------------------------------------------------------

/// The widget declarations of a source, and its text with each blanked out,
/// its lines kept, for GLP's lexer, which has no token =::=.
class WidgetDeclarations {
  final String stripped;

  /// The widget each declaration names, an atom, by its moded type as
  /// written: 'Card' inbox_card.
  final Map<String, String> byModedType;

  /// Where each declaration begins, line and column, in source order.
  final List<(int, int)> positions;

  WidgetDeclarations(this.stripped, this.byModedType, this.positions);

  /// Where the declaration of the moded type [key] begins, or null.
  (int, int)? positionOf(String key) {
    var i = 0;
    for (final k in byModedType.keys) {
      if (k == key) return positions[i];
      i++;
    }
    return null;
  }
}

/// Read the widget declarations `T =::= W.` of [text]: T a moded type, W an
/// atom naming a widget of the construct family (Definition "Widget
/// Declaration, Default Widget"; vGLP 2026-10-01 23:55 UTC, E, Q3).
WidgetDeclarations extractWidgetDeclarations(String text) {
  if (!text.contains('=::=')) {
    return WidgetDeclarations(text, const {}, const []);
  }
  final chars = text.split('');
  final byType = <String, String>{};
  final positions = <(int, int)>[];
  var i = 0;
  int? itemStart;
  while (i < text.length) {
    final c = text[i];
    if (c == '%') {
      while (i < text.length && text[i] != '\n') {
        i++;
      }
      continue;
    }
    if (c == '\'' || c == '"') {
      itemStart ??= i;
      i = _skipQuoted(text, i);
      continue;
    }
    if (c.trim().isEmpty) {
      i++;
      continue;
    }
    itemStart ??= i;
    if (text.startsWith('=::=', i)) {
      final start = itemStart;
      final end = _itemEnd(text, i + 4);
      final (line, column) = _lineColumn(text, start);
      if (end < 0) {
        throw CompileError(
            'A widget declaration "T =::= W" with no full stop', line, column,
            phase: 'parser');
      }
      final left = text.substring(start, i).trim();
      final right = text.substring(i + 4, end).trim();
      final key = _widgetType(left, line, column);
      final widget = _widgetAtom(right, line, column);
      if (byType.containsKey(key)) {
        throw CompileError(
            'Two widget declarations for the moded type $key', line, column,
            phase: 'parser');
      }
      byType[key] = widget;
      positions.add((line, column));
      for (var k = start; k <= end; k++) {
        if (chars[k] != '\n') chars[k] = ' ';
      }
      i = end + 1;
      itemStart = null;
      continue;
    }
    if (c == '.' && _isFullStop(text, i)) {
      itemStart = null;
    }
    i++;
  }
  return WidgetDeclarations(chars.join(), byType, positions);
}

/// The index past the quoted atom or string literal opening at [i].
int _skipQuoted(String text, int i) {
  final q = text[i];
  var j = i + 1;
  while (j < text.length) {
    if (text[j] == '\\') {
      j += 2;
      continue;
    }
    if (text[j] == q) return j + 1;
    j++;
  }
  return j;
}

/// Whether the `.` at [i] ends an item: followed by whitespace, a comment or
/// the end of the text.
bool _isFullStop(String text, int i) =>
    i + 1 >= text.length || text[i + 1].trim().isEmpty || text[i + 1] == '%';

/// The index of the full stop ending the item that goes on at [from], or -1.
int _itemEnd(String text, int from) {
  var i = from;
  while (i < text.length) {
    final c = text[i];
    if (c == '%') {
      while (i < text.length && text[i] != '\n') {
        i++;
      }
      continue;
    }
    if (c == '\'' || c == '"') {
      i = _skipQuoted(text, i);
      continue;
    }
    if (c == '.' && _isFullStop(text, i)) return i;
    i++;
  }
  return -1;
}

(int, int) _lineColumn(String text, int offset) {
  var line = 1, column = 1;
  for (var k = 0; k < offset; k++) {
    if (text[k] == '\n') {
      line++;
      column = 1;
    } else {
      column++;
    }
  }
  return (line, column);
}

/// The moded type a widget declaration names, as written: parsed as the
/// argument type of an imported declaration, which has no clauses.
String _widgetType(String left, int line, int column) {
  try {
    final m = Parser(Lexer('imported procedure m#w($left).').tokenize())
        .parseModule();
    final d = m.procDeclarations.single;
    if (d.argTypes.length == 1) return typeSource(d.argTypes.single);
  } on Object {
    // reported below
  }
  throw CompileError(
      'A widget declaration "T =::= W" whose T, "$left", is not a moded type',
      line, column, phase: 'parser');
}

/// The widget a declaration names: an atom (vGLP 2026-10-01 23:55 UTC, E,
/// Q3), as its constant.  An atom is a string, and a quoted name and a
/// double-quoted string are two spellings of one string (GLP-Spec
/// appendix-lp.tex, Definition "Logic Programs Syntax"), so `'inbox_card'`
/// and `"inbox_card"` name one widget.
String _widgetAtom(String right, int line, int column) {
  if (RegExp(r'^[a-z][A-Za-z0-9_]*$').hasMatch(right)) return right;
  final q = right.isEmpty ? '' : right[0];
  if (right.length >= 2 && (q == "'" || q == '"') && right.endsWith(q)) {
    final toks = Lexer(right).tokenize();
    if (toks.length == 2 &&
        (toks.first.type == TokenType.ATOM ||
            toks.first.type == TokenType.STRING)) {
      return toks.first.lexeme;
    }
  }
  throw CompileError(
      'A widget declaration "T =::= W" whose W, "$right", is not an atom '
      'naming a widget', line, column, phase: 'parser');
}

// ---------------------------------------------------------------------------
// The front end: the paper's syntax as the guarded clauses it denotes
// ---------------------------------------------------------------------------

/// A declaration `procedure (T)*p(T1, ..., Tn)`, at its `procedure` token, or
/// `imported procedure (T)*N#p(T1, ..., Tn)`, [module] its N.
class _Declared {
  final String name;
  final int arity;
  final String? module;
  final int line, column;
  _Declared(this.name, this.arity, this.line, this.column, {this.module});
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
    // procedure p(T1, ..., Tn, T); an imported one, imported procedure
    // (T)*N#p(T1, ..., Tn), as imported procedure N#p(T1, ..., Tn, T), which
    // the compilation makes the import of its asking clause
    // ([_bareCompiled]).
    final d = _interactiveDeclaration(tokens, i);
    if (d != null) {
      final imported =
          tokens[i].type == TokenType.ATOM && tokens[i].lexeme == 'imported';
      var name = tokens[d.nameIndex];
      if (name.type != TokenType.ATOM) {
        throw CompileError(
            'Expected the volitional procedure\'s name after "(T)*"',
            name.line, name.column, phase: 'parser');
      }
      // The module path of an imported one, N or N1#...#Nk, and the name p
      // after it.
      String? module;
      var last = d.nameIndex;
      if (imported) {
        final path = <String>[];
        while (last + 2 < tokens.length &&
            tokens[last + 1].type == TokenType.HASH &&
            tokens[last + 2].type == TokenType.ATOM) {
          path.add(tokens[last].lexeme);
          last += 2;
        }
        if (path.isEmpty) {
          throw CompileError(
              'An imported declaration of a volitional procedure names the '
              'module it is of, "imported procedure (T)*N#p(T1, ..., Tn)" '
              '(vGLP, Definition "Canonical Compilation")',
              t.line, t.column, phase: 'parser');
        }
        module = path.join('#');
        name = tokens[last];
      }
      var j = last + 1;
      var args = <Token>[];
      if (j < tokens.length && tokens[j].type == TokenType.LPAREN) {
        final a = _matching(tokens, j);
        args = tokens.sublist(j + 1, a);
        j = a + 1;
      }
      declared.add(_Declared(name.lexeme, _countArgs(args), t.line, t.column,
          module: module));
      out
        ..addAll(tokens.sublist(i, d.prefixEnd))
        ..addAll(tokens.sublist(d.nameIndex, last + 1))
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

/// The interactive term "is a term of type T, possibly a variable but not the
/// anonymous variable" (Definition "Guarded Clause, ..."), in either mode of
/// T: written `_` or `_?`, or `_Name` or `_Name?`, an anonymous variable being
/// any variable whose name begins with `_` (GLP-Spec, Remark "Anonymous
/// Variables"), it is refused.  (_) and the handle are out of the language
/// (Udi, 2026-10-03; vGLP c994328, c2e8b57, as vGLP #5 Cowork reports it).
void _checkNoAnonymousTerm(
    Module m, Map<String, VolitionalProcedure> volitional) {
  for (final v in volitional.values) {
    for (final p in m.procedures) {
      if (p.signature != '${v.name}/${v.arity + 1}') continue;
      for (final c in p.clauses) {
        if (!_isAnonymous(c.head.args.last)) continue;
        throw CompileError(
            'The clause ${_asWritten(c, v)} of ${v.name}/${v.arity}, whose '
            'interactive type ${v.typeConstant} is in '
            '${v.readerMode ? 'reader' : 'writer'} mode, has the anonymous '
            'variable as its interactive term: the interactive term is not '
            'the anonymous variable (vGLP, Definition "Guarded Clause, '
            'Volitional Procedure, ...")',
            c.line, c.column, phase: 'analyzer');
      }
    }
  }
}

/// "A clause of a volitional procedure reads an argument at an input
/// position, by a non-variable term at that position in its head or by a
/// guard on it, only if every later clause of the procedure reads it too"
/// (vGLP, Definition "Clause of a Volitional Procedure, Interactive Term").
/// The asking clause passes each
/// argument on through a pair of its own, so a later clause that does not
/// read it could be taken where the source goal takes the earlier one
/// (vGLP, the paragraph after Definition "Canonical Compilation").
///
/// The arguments are S1, ..., Sn, the interactive term A not among them: the
/// asking clause passes S1, ..., Sn through its pairs and A is matched
/// against the goal's end of its fresh interactive variable.  A position is
/// an input position where its declared type is in reader mode, as the
/// asking clause reads it ([_askingClause]).  A guard is on the argument
/// where the variable at that position occurs in it, as writer or reader.
void _checkLaterClausesRead(Module m,
    Map<String, VolitionalProcedure> volitional,
    Map<String, ProcDecl> declsByKey) {
  for (final v in volitional.values) {
    final decl = declsByKey['${v.name}/${v.arity + 1}']!;
    final p = m.procedures
        .where((p) => p.signature == '${v.name}/${v.arity + 1}')
        .firstOrNull;
    if (p == null) continue;
    bool reads(Clause c, int k) {
      final s = c.head.args[k];
      if (s is UnderscoreTerm) return false;
      if (s is! VarTerm) return true;
      for (final g in c.guards ?? const <Guard>[]) {
        if (g.args.any((t) => _occurs(s.name, t))) return true;
      }
      return false;
    }

    for (var k = 0; k < v.arity; k++) {
      if (!decl.isInputArg(k)) continue;
      for (var i = 0; i < p.clauses.length; i++) {
        if (!reads(p.clauses[i], k)) continue;
        for (var j = i + 1; j < p.clauses.length; j++) {
          final later = p.clauses[j];
          if (reads(later, k)) continue;
          final c = p.clauses[i];
          throw CompileError(
              'The clause ${_asWritten(c, v)} of the volitional procedure '
              '${v.name}/${v.arity}, at line ${c.line}, reads its argument at '
              'input position ${k + 1}, and the later clause '
              '${_asWritten(later, v)}, at line ${later.line}, does not: a '
              'clause of a volitional procedure reads an argument at an input '
              'position, by a non-variable term at that position in its head '
              'or by a guard on it, only if every later clause of the '
              'procedure reads it too (vGLP, Definition "Clause of a '
              'Volitional Procedure, Interactive Term")',
              later.line, later.column, phase: 'analyzer');
        }
      }
    }
  }
}

/// "Every call of a volitional procedure, in a clause body or in the initial
/// goal, holds a writer at each output position" (vGLP, Definition "Clause
/// of a Volitional Procedure, Interactive Term"): "at an output position the
/// asking clause's head
/// carries a reader, which a writer of the goal matches and a term of the
/// goal that is not a writer does not" (the paragraph after Definition
/// "Canonical Compilation").
///
/// The calls are those of every clause body of the source, of its own
/// volitional procedures, of those it imports ([imported], by N#p/n) and of
/// those of its scope ([scope], by p/n); the initial goal is not in the
/// source (the head comment, "The initial goal"): it is formed by a clause of
/// the GLP module that imports the compiled procedures, checked at link time
/// (program_linker.dart), or posted directly (glp_engine.dart), each by the
/// same rule, [checkWritersAtOutputs], over the record [volitionalRecord].
void _checkWritersAtOutputs(Module m,
    Map<String, VolitionalProcedure> volitional,
    Map<String, ProcDecl> declsByKey,
    {Map<String, VolitionalCall> imported = const {},
    Map<String, VolitionalCall> scope = const {}}) {
  final ownClauses = {
    for (final v in volitional.values) '${v.name}/${v.arity + 1}': v
  };
  final printer = GlpPrinter();
  String clauseOf(Procedure p, Clause c) {
    final v = ownClauses[p.signature];
    return v != null ? _asWritten(c, v) : printer.printAtom(c.head);
  }

  for (final p in m.procedures) {
    for (final c in p.clauses) {
      final body = c.body ?? const <Goal>[];
      for (final g in _goals(body)) {
        final where = 'in the clause ${clauseOf(p, c)} at line ${c.line}';
        if (g is RemoteGoal) {
          // A volitional procedure the source imports.
          final inner = g.goal;
          if (inner is RemoteGoal) continue;
          final r = imported[
              '${g.staticModuleName}#${inner.functor}/${inner.arity}'];
          if (r == null) continue;
          checkWritersAtOutputs(g, inner.args, r,
              calledAs: '${g.staticModuleName}#${r.name}', where: where);
          continue;
        }
        final sig = '${g.functor}/${g.args.length}';
        final v = volitional[sig];
        if (v == null) {
          // A volitional procedure of the source's scope.
          final r = scope[sig];
          if (r != null) {
            checkWritersAtOutputs(g, g.args, r, calledAs: r.name, where: where);
          }
          continue;
        }
        checkWritersAtOutputs(
            g,
            g.args,
            VolitionalCall.of(
                v.name, v.arity, declsByKey['${v.name}/${v.arity + 1}']!),
            calledAs: v.name,
            where: where);
      }
    }
  }
}

/// A volitional procedure as a call of it sees it: its name, its source
/// arity n, and its output positions, counted from 0 among its n arguments
/// --- those whose declared type is in writer mode (Definition "Volitional
/// Procedure, ..."; [_askingClause]).  The one record of volition that
/// crosses from the `.vglp` front to the places that check a call of a
/// compiled volitional procedure: a clause of the GLP module that forms the
/// program's initial goal (program_linker.dart) and a goal posted directly
/// (glp_engine.dart), the compiled module itself carrying no mark of it.
class VolitionalCall {
  final String name;
  final int arity;
  final List<int> outputs;

  const VolitionalCall(this.name, this.arity, this.outputs);

  /// The record of the volitional procedure [name]/[arity] declared by
  /// [decl], its declaration of arity n+1, the interactive type last.
  factory VolitionalCall.of(String name, int arity, ProcDecl decl) =>
      VolitionalCall(name, arity,
          [for (var k = 0; k < arity; k++) if (!decl.isInputArg(k)) k]);

  /// The signature of its compiled asking clause, `name/(n+1)`, which the
  /// GLP module forming the initial goal calls with the ask stream added.
  String get compiledKey => '$name/${arity + 1}';
}

/// The rule of a call of a volitional procedure: "every call of a volitional
/// procedure, in a clause body or in the initial goal, holds a writer at each
/// output position" (vGLP, Definition "Clause of a Volitional Procedure,
/// Interactive Term").  [call] is the call as
/// written, printed in the refusal; [args] its arguments, the procedure's n
/// first, an ask stream after them not read; [calledAs] the procedure as the
/// call names it, `q` or `N#q`; [where] the clause or the goal the call is in,
/// as the refusal names it.  A writer is a writer variable or an anonymous
/// variable, which "denotes a fresh writer" (GLP-Spec, Remark "Anonymous
/// Variables"); any other term --- a reader, a constant, a compound --- is
/// refused, as an error: the asking clause's head carries a reader there,
/// which only a writer of the goal matches.
void checkWritersAtOutputs(Goal call, List<Term> args, VolitionalCall v,
    {required String calledAs, required String where}) {
  bool isWriter(Term t) =>
      (t is VarTerm && !t.isReader) || (t is UnderscoreTerm && !t.isReader);
  final printer = GlpPrinter();
  for (final k in v.outputs) {
    final s = args[k];
    if (isWriter(s)) continue;
    throw CompileError(
        'The call ${printer.printGoal(call)} of the volitional procedure '
        '$calledAs/${v.arity}, $where, holds ${printer.printTerm(s)} at '
        'output position ${k + 1}, which is not a writer: every call of a '
        'volitional procedure, in a clause body or in the initial goal, '
        'holds a writer at each output position (vGLP, Definition "Clause '
        'of a Volitional Procedure, Interactive Term"), the asking '
        'clause\'s head carrying a reader '
        'there, which only a writer of the goal matches (vGLP, the paragraph '
        'after Definition "Canonical Compilation")',
        call.line, call.column, phase: 'analyzer');
  }
}

/// The volitional procedures of the vGLP source [text], each by the
/// signature of its compiled asking clause, `q/(n+1)` ([VolitionalCall]):
/// what a module compiled from [text] is called by, which its compiled
/// declarations do not say.  Empty where [text] is not in the paper's syntax
/// or not read by the front end, its own compilation refusing it where it is
/// no program.
Map<String, VolitionalCall> volitionalRecord(String text) {
  final _Parsed parsed;
  try {
    if (!isPaperSyntaxSource(text)) return const {};
    parsed = _parse(extractWidgetDeclarations(text).stripped);
  } on CompileError {
    return const {};
  }
  final decls = {
    for (final d in parsed.module.procDeclarations)
      if (!d.imported) d.key: d
  };
  final out = <String, VolitionalCall>{};
  for (final v in parsed.declarations) {
    if (v.module != null) continue;
    final decl = decls['${v.name}/${v.arity + 1}'];
    if (decl == null) continue;
    final call = VolitionalCall.of(v.name, v.arity, decl);
    out.putIfAbsent(call.compiledKey, () => call);
  }
  return out;
}

/// Whether the variable [name] occurs in [t], as writer or reader.
bool _occurs(String name, Term t) {
  if (t is VarTerm) return t.name == name;
  if (t is StructTerm) return t.args.any((a) => _occurs(name, a));
  if (t is ListTerm) {
    return (t.head != null && _occurs(name, t.head!)) ||
        (t.tail != null && _occurs(name, t.tail!));
  }
  return false;
}

/// The head of a clause of a volitional procedure as the source writes it,
/// `(A)*p(S1, ..., Sn)`, its terms by GLP's printer (glp_printer.dart).
String _asWritten(Clause c, VolitionalProcedure v) {
  final printer = GlpPrinter();
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
/// least fixpoint over the source's calls.  A volitional procedure is named
/// by its source arity, which is the arity of its calls.  A call of a
/// procedure that is not the source's own --- of another module, M # p(...),
/// or of the source's scope --- reaches a question where [external] says it
/// does ([compileCanonical]).
Set<String> _reaching(Module m, Map<String, VolitionalProcedure> volitional,
    Map<String, VolitionalProcedure> askedProcs,
    [bool Function(Goal)? external]) {
  final reaching = {for (final v in volitional.values) '${v.name}/${v.arity}'};
  var changed = true;
  while (changed) {
    changed = false;
    for (final p in m.procedures) {
      if (askedProcs.containsKey(p.signature) ||
          reaching.contains(p.signature)) {
        continue;
      }
      final calls = p.clauses.expand((c) => _goals(c.body ?? const []));
      if (calls.any((g) => _reachesBy(g, reaching, external))) {
        reaching.add(p.signature);
        changed = true;
      }
    }
  }
  return reaching;
}

/// Whether the body goal [g] calls a procedure that reaches a question: a
/// placed goal by its inner goal; one of the source's own by [reaching];
/// one of another module, or of the source's scope, by [external].
bool _reachesBy(Goal g, Set<String> reaching, bool Function(Goal)? external) {
  if (g is SpawnGoal) return _reachesBy(g.innerGoal, reaching, external);
  if (g is RemoteGoal) return external?.call(g) ?? false;
  return reaching.contains('${g.functor}/${g.args.length}') ||
      (external?.call(g) ?? false);
}

/// The goals a body calls, a placed goal by its inner goal, a call of another
/// module's procedure as written.
Iterable<Goal> _goals(List<Goal> body) sync* {
  for (final g in body) {
    if (g is SpawnGoal) {
      yield* _goals([g.innerGoal]);
    } else {
      yield g;
    }
  }
}

/// The body goal [g] with [stream] added as its last argument, a placed goal
/// to its inner goal, a call of another module's procedure to the goal it
/// calls there.
Goal _withStream(Goal g, Term stream) {
  if (g is SpawnGoal) {
    return SpawnGoal(_withStream(g.innerGoal, stream), g.agentId, g.line,
        g.column);
  }
  if (g is RemoteGoal) {
    return RemoteGoal(g.module, _withStream(g.goal, stream), g.line, g.column);
  }
  return Goal(g.functor, [...g.args, stream], g.line, g.column);
}

// ---------------------------------------------------------------------------
// The types the compilation adds
// ---------------------------------------------------------------------------

/// The questions, [question], one alternative per moded interactive type, its
/// functor wrapping the type as written in its mode, t(T); and the ask
/// stream's element, [ask] at Question, Ask(Q) ::= ask(Constant, Q), one
/// ask/2 over the union of the functors (vGLP #4 Cowork, 2026-10-02 08:26
/// UTC, Q1), which the dispatcher's generic source defines (2026-10-02 21:02
/// UTC, B) and the compilation defines only where it has no generic source
/// ([emitAsk]).  An interactive type that names a type parameter of its
/// procedure gives Question that parameter, and each declaration with an ask
/// stream takes it.
class _AddedTypes {
  final String ask;
  final String question;
  final bool emitAsk;
  final List<String> params;

  /// Each moded interactive type, once, with its functor.
  final List<(TypeExpr, String)> interactiveTypes;

  _AddedTypes(this.ask, this.question, this.emitAsk, this.params,
      this.interactiveTypes);

  List<TypeRef> _paramRefs(int l, int c) =>
      [for (final p in params) TypeRef(p, l, c)];

  /// Question, or Question(X, ...) where the interactive types name
  /// parameters.
  TypeRef questionRef(int l, int c) =>
      TypeRef(question, l, c, typeArgs: _paramRefs(l, c));

  /// Stream(Ask(Question)), the type of an ask stream.
  TypeRef stream(int l, int c) => TypeRef('Stream', l, c, typeArgs: [
        TypeRef(ask, l, c, typeArgs: [questionRef(l, c)])
      ]);

  /// A declaration's type parameters with the questions' added.
  List<String> paramsFor(List<String> own) =>
      [...own, for (final p in params) if (!own.contains(p)) p];

  /// None where the program has no question: nothing then has an ask
  /// stream.
  List<TypeDef> get typeDefs => interactiveTypes.isEmpty
      ? const []
      : [
        TypeDef(
            question,
            [
              for (final (t, f) in interactiveTypes) StructAlt(f, [t], 0, 0)
            ],
            0,
            0,
            typeParams: params),
        if (emitAsk)
          TypeDef(
              ask,
              [
                StructAlt(askFunctor, [
                  TypeRef('Constant', 0, 0),
                  TypeRef('Q', 0, 0),
                ], 0, 0)
              ],
              0,
              0,
              typeParams: const ['Q']),
      ];
}

/// The types the compilation adds, over the program's interactive types, in
/// the order of their declarations, each with the
/// type parameters of its declaration.
_AddedTypes _addedTypes(Iterable<InteractiveType> interactive,
    {required String ask, required String question, required bool emitAsk}) {
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

  for (final v in interactive) {
    final t = v.type;
    collect(t, v.params);
    if (seen.add(v.functor)) types.add((t, v.functor));
  }
  return _AddedTypes(ask, question, emitAsk, params, types);
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
///     q(S1, ..., Sn, [ask(T, t(X)) | D?]) :- q1(S1', ..., Sn', X?, D).
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
              VarTerm('D', false, l, c),
            ],
            l,
            c),
      ],
      line: l,
      column: c);
  return Procedure(v.askingName, v.arity + 1, [clause], l, c);
}

/// The clauses of q as guarded clauses of arity n+1, named q1, then extended
/// (Definition "Canonical Compilation"): a compiled clause of arity n+2, its
/// ask stream and nothing else added.
Procedure _guardedProcedure(
    Procedure p, VolitionalProcedure v, bool Function(Goal) reaches) {
  final clauses = [
    for (final c in p.clauses) _extended(c, v.guardedName, c.head.args, reaches)
  ];
  return Procedure(v.guardedName, v.arity + 2, clauses, p.line, p.column);
}

/// Whether [t] is an anonymous variable, in either mode: `_`, `_?`, or a
/// variable whose name begins with `_` (GLP-Spec, Remark "Anonymous
/// Variables").
bool _isAnonymous(Term t) =>
    t is UnderscoreTerm || (t is VarTerm && t.name.startsWith('_'));

/// A clause of a procedure that reaches a question, named [name], its head's
/// arguments [headArgs] and then its ask stream (Definition "Canonical
/// Compilation"): each body call of a procedure that reaches a question is
/// given a fresh writer as its ask stream, and the head carries D?, D the
/// clause's ask stream --- the one such call given D itself, more merged into
/// D by merge goals, a chain of them after the body's own goals --- and []
/// in place of D? where there is none.
Clause _extended(
    Clause c, String name, List<Term> headArgs, bool Function(Goal) reaches) {
  final body = c.body ?? const <Goal>[];
  final l = c.head.line, col = c.head.column;
  final k = body.where(reaches).length;
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
      reaches(g)
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

String _emit(Module m, List<TypeDef> copied, List<TypeDef> added,
    List<_Emitted> procs, List<ProcDecl> bare, _Elicitation? elicitation) {
  final b = StringBuffer();
  // Every clause by GLP's printer, a constant in its one canonical print
  // (glp_printer.dart, [constantSource]): GLP-Spec appendix-lp.tex,
  // Definition "Logic Programs Syntax", one string constant, 'x' and "x" one
  // value, the double-quoted form read and never printed.
  final printer = GlpPrinter();
  b.write(compiledHeader);
  b.writeln();

  for (final td in m.typeDefs) {
    b.writeln(printTypeDef(td));
  }
  // The definitions copied from the program's other modules, of the types
  // its questions name that the source's scope does not define.
  for (final td in copied) {
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
  if (elicitation == null) return b.toString();

  // The dispatcher and what the construct processes share, from
  // programs/vglp/dispatcher.glp; then the construct process of each
  // interactive type.
  b.writeln('%% --- the dispatcher (programs/vglp/dispatcher.glp) ---');
  b.writeln();
  for (final td in elicitation.typeDefs) {
    b.writeln(printTypeDef(td));
  }
  b.writeln();
  final decls = <String, List<ProcDecl>>{};
  for (final d in elicitation.procDecls) {
    decls.putIfAbsent(d.name, () => []).add(d);
  }
  for (final p in elicitation.procedures) {
    for (final d in decls[p.name] ?? const <ProcDecl>[]) {
      if (d.arity == p.arity) b.writeln(printDeclaration(d));
    }
    for (final c in p.clauses) {
      b.writeln(printer.printClause(c));
    }
    b.writeln();
  }
  b.writeln('%% --- the construct process of each interactive type ---');
  b.writeln();
  b.write(elicitation.constructs);
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
