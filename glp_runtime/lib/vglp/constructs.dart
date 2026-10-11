// glp_runtime/lib/vglp/constructs.dart
//
// The construct processes of the canonical compilation: for each interactive
// type T of a program, the clause of construct/4 that is spawned on an ask of
// T, and the clauses it calls, typed at T; and the two procedures by which the
// compiled program spawns them, constructs/1, which reads the spawns the
// dispatcher writes and calls construct/4 on each, and dispatch/3, the
// program's entry point to the dispatcher, which spawns the dispatcher's
// dispatch/4 and constructs/1 (vGLP #4 Cowork, 2026-10-02 21:02 UTC, B,
// without the handle: vGLP #5 Cowork, 2026-10-03 08:16 UTC, item 2).
// Spec: vGLP, sections/vglp.tex, Definition "vmaGLP Transition System" (Ask by
// the mode of the interactive type, Answer, Present) and the paragraph after
// it; sections/elicitation.tex, Definition "Construct, Submission, Complete
// Widget", Definition "Widget Declaration, Default Widget", Definition
// "Canonical Compilation" and the paragraph before it.  The messages to and
// from the bridge are vGLP's of 2026-10-01 23:55 UTC (E, Q2--Q4).
//
// THE CONSTRUCT PROCESS OF T holds the person's end of the interactive
// variable: the writer where T is in reader mode, the person writing the
// variable, and the reader where T is in writer mode.  It walks T's moded
// structure, each position written by the program or by the person, a `?` in
// a type definition exchanging the two (TGLP's moded types):
//
//   - a position the program writes is shown as the program writes it: the
//     view waits for it, as Present delivers every output before the inputs
//     inside it; a stream the program writes and the person does not is
//     shown element by element as it arrives, a thread;
//   - a position the person writes is a QUESTION, shown as input(P), P its
//     position: the path of argument indices from the root of the
//     interactive variable's term to it, [] the root, a stream the person
//     writes having the stream's own path, each submission one element
//     (vGLP #5 Cowork, 2026-10-04 09:05 UTC, C); a grant forms a term of its
//     type and binds it, a stream type taking one element per grant, an
//     input box;
//   - the views are drawn as they change, draw(Id, W, V), W the widget of T;
//   - the grants routed to the construct arrive whole, input(Id, P, R), each
//     naming its question by P, which the bridge sends back from the view; a
//     construct may hold two questions of one type, and each takes only the
//     grants with its own P, the question answer_N(P, ...) guarding on it and
//     passing a grant for another P on; the question reads the person's
//     input R from a grant it takes, and a grant whose R forms no term of its
//     type, or whose P names no open question of the construct, answers
//     none;
//   - its grants never close: the person channel never closes (Definition
//     "Person Channel, Person Writer, GLP with Persons, Grant", at 7838827),
//     so neither does a construct's grant stream, Inputs ::= [Input |
//     Inputs], which carries the person's grants on, and no clause reads a
//     [] of it; a question whose grants never come stays open, its goal
//     suspended, as the semantics has it (vGLP #5 Cowork, 2026-10-03 08:16
//     UTC, item 3, and 21:13 UTC, Q1);
//   - it withdraws when every question it holds is answered and none is
//     still to come, and on nothing else (run/5 in
//     programs/vglp/dispatcher.glp): "The construct process withdraws when
//     every question it holds is answered.  A reader the program drops leaves
//     its construct on screen, showing what the program writes into it"
//     (sections/elicitation.tex, the paragraph before Definition "Canonical
//     Compilation", at c994328).
//
// THE FORMING CLAUSES form a term of a question's type from the person's
// input, which is typed at _: a constant by matching it, a primitive by its
// guard (string/1, integer/1, ...), a structure argument by argument, the
// children's results joined.  Their result carries the term, Formed(T) ::=
// formed(T) ; refused, so every clause writes what it holds (vGLP #5 Cowork,
// 2026-10-03 08:16 UTC, item 3; GLP-Spec's Remark "Anonymous Variables" at
// c3d3fc6, no anonymous reader), the term written in the body where a guard
// narrows the input to the type (21:13 UTC, Q2).  They are typed at the
// question's type, so a question is bound only with a term of its type
// (vGLP, 2026-10-02 08:26 UTC, item 6).
//
// LISTS, STREAMS AND FORMS (vGLP #6 Cowork, 2026-10-11 12:55 UTC, Task B,
// from Definition "Widget Declaration, Default Widget" and the Answer rule
// at 90988ea).  A stream the person writes is a type of [] and [E | S] named
// Stream, a list one named List, told apart by the name: a list is ended by
// a grant of [], a stream never, and the list's widget is input_list(W), the
// stream's input_box(W).  A stream or a list inside a form the person writes
// is a question of its own: the form is formed with [] there, its answer
// carries a fresh variable there (open_N), answered at its own position from
// the grants that follow, and the construct is drawn again with the answer;
// its entry in the form's widget is question(L, W) and every other field(L,
// W), L the name of the field's declared type.  A position of type Key, or
// of a type defined as Key, is a peer field.
//
// WHAT IS NOT BUILT, each a compile error naming the type: a position the
// program writes inside one the person writes (the Answer transition leaves
// it to the program, and the construct has no output to show for it yet); a
// position the person writes of a type parameter (no input forms a term the
// checker can type at it, _checkPersonWritable); a recursive type the person
// writes that is neither a Stream nor a List, and a list cell the person
// writes outside one (the Definition gives them no widget); a stream or a
// list inside a form that is an element of a stream or a list the person
// writes (the elements share one position); a stream the program writes
// whose elements hold questions; a difference list or a mutual reference.
// A position the person writes of type Real is built, formed by real/1 and
// drawn as a number field (vGLP #5 Cowork, 2026-10-03 08:16 UTC, item 6;
// Definition "Widget Declaration, Default Widget" at c2e8b57: "a number
// (Integer or Real)").

import '../compiler/error.dart';
import '../analysis/type_checker/type_ast.dart';
import '../compiler/glp_printer.dart' show constantSource, functorNameSource;
import 'mediator.dart' show typeSource;

/// A moded interactive type of the program: its type as written, its mode,
/// the functor the compilation gives it in Question, and the type parameters
/// of the procedure that declares it.  One of another module N, which an
/// import brings (vGLP at 66b6500, Definition "Canonical Compilation": "the
/// interactive types of N join those of M, each with the functor and the
/// widget its export gives it, M declaring no widget for a type it
/// imports"), carries the module it is [from] and the [widget] N gives it;
/// [alsoOwn] where a volitional procedure of the program's own has it too.
class InteractiveType {
  final TypeExpr type;
  final bool readerMode;
  final String functor;
  final List<String> params;
  final int line, column;

  /// The widget term the type's module gives it, where it is another
  /// module's; null for the program's own, whose widget is the one in its
  /// scope.
  final String? widget;

  /// The module whose export brings the type, or null for the program's own.
  final String? from;

  /// Whether a volitional procedure of the program's own has this imported
  /// type too: the two are one alternative of the questions, of one
  /// functor, and one construct process serves both.
  final bool alsoOwn;

  InteractiveType(this.type, this.readerMode, this.functor, this.params,
      this.line, this.column,
      {this.widget, this.from, this.alsoOwn = false});

  /// The moded type as written, `Card` or `Request?`.
  String get written => typeSource(type);

  /// The same type with [widget] as its widget.
  InteractiveType withWidget(String? widget) =>
      InteractiveType(type, readerMode, functor, params, line, column,
          widget: widget, from: from, alsoOwn: alsoOwn);

  /// The same imported type, a volitional procedure of the program's own
  /// having it too.
  InteractiveType withOwn() =>
      InteractiveType(type, readerMode, functor, params, line, column,
          widget: widget, from: from, alsoOwn: true);
}

/// A type's name as a lowercase atom stem, `Stream(String)` stream_string,
/// from which the procedures of a position of it are named.
String _typeStem(TypeExpr t) {
  if (t is TypeRef) {
    final head = t.name.isEmpty
        ? 'type'
        : '${t.name[0].toLowerCase()}${t.name.substring(1)}';
    return [head, for (final a in t.typeArgs) _typeStem(a)].join('_');
  }
  if (t is PrimitiveModeAlt) return 'any';
  return 'type';
}

/// The names the construct processes call in the dispatcher's generic
/// source, and the types they use from it, as the program emits them.
class GenericNames {
  final String dispatch;
  final String constructs;
  final String run;
  final String shown;
  final String thread;
  final String allDone;
  final String path;
  final Map<_Leaf, String> _leafFormers;
  final String formedType;
  final String doneType;
  final String drawType;
  final String inputType;
  final String inputsType;
  final String pathType;
  final String personInType;
  final String spawnType;
  final String askType;

  GenericNames(
      {required this.dispatch,
      required this.constructs,
      required this.run,
      required this.shown,
      required this.thread,
      required this.allDone,
      required this.path,
      required String formString,
      required String formInteger,
      required String formNumber,
      required String formReal,
      required String formConstant,
      required String formModule,
      required String formAny,
      required this.formedType,
      required this.doneType,
      required this.drawType,
      required this.inputType,
      required this.inputsType,
      required this.pathType,
      required this.personInType,
      required this.spawnType,
      required this.askType})
      : _leafFormers = {
          _Leaf.string: formString,
          _Leaf.integer: formInteger,
          _Leaf.real: formReal,
          _Leaf.number: formNumber,
          _Leaf.constant: formConstant,
          _Leaf.module: formModule,
          _Leaf.any: formAny,
          _Leaf.param: formAny,
        };

  /// The generic procedures the construct processes may call, by their
  /// generic names: the roots, with the dispatcher's entry point, of what the
  /// compilation emits of the generic source.
  static const used = [
    'run',
    'shown',
    'thread',
    'all_done',
    'path',
    'form_string',
    'form_integer',
    'form_number',
    'form_real',
    'form_constant',
    'form_module',
    'form_any',
  ];
}

/// The construct processes of a program.
class ConstructProcesses {
  /// The GLP text of their declarations and clauses: dispatch/3,
  /// constructs/1 and construct/4 first.
  final String source;

  /// The widget of each interactive type, by the moded type as written.
  final Map<String, String> widgets;

  ConstructProcesses(this.source, this.widgets);
}

/// Build the construct processes of [types].
///
/// [resolve] gives the definition of a type name the program uses --- its own
/// types, its scope's, the root's --- or null where there is none.
/// [declared] gives the widget a declaration in scope names for a moded type,
/// by the moded type as written.  [fresh] gives a procedure name fresh against
/// the program's for a stem.
ConstructProcesses buildConstructs({
  required List<InteractiveType> types,
  required String constructName,
  required String questionType,
  required GenericNames generic,
  required TypeDef? Function(String name) resolve,
  required Map<String, String> declared,
  required String Function(String stem) fresh,
}) {
  final g = _Generator(
      generic, resolve, declared, fresh, questionType, constructName);
  return g.build(types);
}

// ---------------------------------------------------------------------------
// The type walk
// ---------------------------------------------------------------------------

/// The primitive positions: each formed by its guard and shown as it is.
enum _Leaf { integer, real, number, string, constant, module, any, param }

/// One alternative of a type, flattened through the unions it names.
abstract class _Alt {}

class _ConstAlt extends _Alt {
  final Object value;
  _ConstAlt(this.value);
}

class _StructAlt extends _Alt {
  final String functor;
  final List<TypeExpr> args;
  _StructAlt(this.functor, this.args);
}

class _NilAlt extends _Alt {}

class _ConsAlt extends _Alt {
  final TypeExpr head, tail;
  _ConsAlt(this.head, this.tail);
}

class _LeafAlt extends _Alt {
  final _Leaf leaf;
  _LeafAlt(this.leaf);
}

/// A position of an interactive variable: its type, mode removed, and who
/// writes it.
class _Node {
  final TypeExpr type;
  final bool person;
  final Set<String> params;

  _Node(TypeExpr t, this.person, this.params) : type = _unmoded(t);

  String get typeKey => typeSource(type);
  String get key => '$typeKey@${person ? 'person' : 'program'}';

  /// A child at a position of type [t]: `?` in the definition exchanges who
  /// writes it.
  _Node child(TypeExpr t) => _Node(t, person != _isInput(t), params);

  /// The moded type as a widget declaration names it: `T?` where the person
  /// writes it, `T` where the program does.
  String get moded => person ? '$typeKey?' : typeKey;
}

bool _isInput(TypeExpr t) =>
    (t is TypeRef && t.isInput) || (t is PrimitiveModeAlt && t.isInput);

TypeExpr _unmoded(TypeExpr t) {
  if (t is TypeRef && t.isInput) {
    return TypeRef(t.name, t.line, t.column, typeArgs: t.typeArgs);
  }
  if (t is PrimitiveModeAlt && t.isInput) {
    return PrimitiveModeAlt(false, t.line, t.column);
  }
  return t;
}

/// [t] with each parameter in [m] replaced, the mode of the occurrence and
/// that of the argument composed.
TypeExpr _subst(TypeExpr t, Map<String, TypeExpr> m) {
  if (t is TypeRef) {
    final r = m[t.name];
    if (r != null && t.typeArgs.isEmpty) {
      if (!t.isInput) return r;
      if (r is TypeRef) {
        return TypeRef(r.name, r.line, r.column,
            isInput: !r.isInput, typeArgs: r.typeArgs);
      }
      if (r is PrimitiveModeAlt) {
        return PrimitiveModeAlt(!r.isInput, r.line, r.column);
      }
      return r;
    }
    return TypeRef(t.name, t.line, t.column,
        isInput: t.isInput,
        typeArgs: [for (final a in t.typeArgs) _subst(a, m)]);
  }
  if (t is StructAlt) {
    return StructAlt(t.functor, [for (final a in t.args) _subst(a, m)],
        t.line, t.column);
  }
  if (t is ListConsAlt) {
    return ListConsAlt(_subst(t.head, m), _subst(t.tail, m), t.line, t.column);
  }
  if (t is DiffListAlt) {
    return DiffListAlt(
        _subst(t.content, m), _subst(t.hole, m), t.line, t.column);
  }
  return t;
}

// ---------------------------------------------------------------------------
// The generator
// ---------------------------------------------------------------------------

class _Generator {
  final GenericNames gen;
  final TypeDef? Function(String) resolve;
  final Map<String, String> declared;
  final String Function(String) fresh;
  final String questionType;
  final String constructName;

  final _out = StringBuffer();
  final _emitted = <String>{};
  final _names = <String, String>{};
  final _altsMemo = <String, List<_Alt>>{};
  final _questionMemo = <String, bool>{};
  final _multiMemo = <String, bool>{};
  final _suffixes = <String, String>{};
  final _suffixesUsed = <String, Set<String>>{};
  final _nestedMemo = <String, bool>{};

  /// The interactive type whose construct process is being built, for the
  /// position of a compile error.
  InteractiveType? _at;

  _Generator(this.gen, this.resolve, this.declared, this.fresh,
      this.questionType, this.constructName);

  ConstructProcesses build(List<InteractiveType> types) {
    final hook = StringBuffer();
    final params = <String>{for (final t in types) ...t.params};
    final qt = params.isEmpty
        ? questionType
        : '$questionType(${params.join(', ')})';
    final p = params.isEmpty ? '' : '(${params.join(', ')})';
    const ch = 'Channel(Stream(_), Stream(_))';
    final d = gen.dispatch, cs = gen.constructs;
    hook
      // dispatch/3: the dispatcher on the ask stream and the person channel,
      // and constructs/1 on the spawns it writes.
      ..writeln('exported procedure$p $d(Stream(${gen.askType}($qt))?, '
          'Channel(${gen.personInType}, Stream(_))?, $ch).')
      ..writeln('$d(Asks, PCh, MCh?) :- $d(Asks?, PCh?, MCh, Ss), $cs(Ss?).')
      ..writeln()
      // constructs/1: the construct process of each spawn.
      ..writeln('procedure$p $cs(Stream(${gen.spawnType}($qt))?).')
      ..writeln('$cs([spawn(Id, Q, Gs, Ds?) | Ss]) :- '
          '$constructName(Id?, Q?, Gs?, Ds), $cs(Ss?).')
      ..writeln('$cs([]).')
      ..writeln()
      ..writeln('procedure$p $constructName(Integer?, $qt?, '
          '${gen.inputsType}?, Stream(${gen.drawType})).');
    final widgets = <String, String>{};
    for (final t in types) {
      _at = t;
      final root = _Node(t.type, t.readerMode, t.params.toSet());
      final w = _typeWidget(t, root);
      widgets[t.written] = w;
      hook.writeln(_constructClause(t, root, w));
    }
    hook.writeln();
    return ConstructProcesses('$hook$_out', widgets);
  }

  /// The widget of interactive type [t], whose root is [root]: the one its
  /// module gives it where it is another module's, the importer declaring
  /// none for it, a volitional procedure of the program's own of the type
  /// included ("the interactive types of N join those of M, each with the
  /// functor and the widget its export gives it, M declaring no widget for
  /// a type it imports", vGLP at 66b6500, Definition "Canonical
  /// Compilation"; vGLP #7 Cowork, 2026-10-11 14:19 UTC, item 1: "the
  /// widget travels with the type from the module that exports it, as the
  /// functor does"); and the one in the program's scope where it is the
  /// program's own.
  String _typeWidget(InteractiveType t, _Node root) =>
      t.widget ?? _widget(root, {});

  // --- the construct clause -------------------------------------------------

  /// The construct clause of [t]: the root of its interactive variable's term
  /// is at the path [].
  String _constructClause(InteractiveType t, _Node root, String w) {
    final f = t.functor;
    if (root.person) {
      _checkPersonWritable(root, t);
      final answer = _answer(root);
      if (_holdsNested(root)) {
        // The form's answer leaves its streams and lists open, each a
        // question of its own drawn under it: the view after the answer.
        return '$constructName(Id, $f(X?), Gs, Ds?) :- '
            '$answer([], X, Gs?, _, Vs, Done), '
            '${gen.run}(Id?, $w, [input([]) | Vs?], Done?, Ds).';
      }
      return '$constructName(Id, $f(X?), Gs, Ds?) :- '
          '$answer([], X, Gs?, _, Done), '
          '${gen.run}(Id?, $w, [input([])], Done?, Ds).';
    }
    if (!_hasQuestion(root)) {
      // A type the person writes nowhere holds no question, so its construct
      // has every question it holds answered at once, and withdraws (Definition
      // "Construct, Submission, Complete Widget": a construct is presented
      // while its question is open).
      return '$constructName(Id, $f(_), _, [withdraw(Id?)]).';
    }
    final present = _present(root, t);
    return '$constructName(Id, $f(X), Gs, Ds?) :- '
        '$present([], X?, Gs?, _, Vs, Done), '
        '${gen.run}(Id?, $w, Vs?, Done?, Ds).';
  }

  // --- types ------------------------------------------------------------------

  /// The alternatives of [t], flattened through the unions it names.
  List<_Alt> _alts(TypeExpr t, Set<String> params) {
    final key = typeSource(_unmoded(t));
    final memo = _altsMemo[key];
    if (memo != null) return memo;
    final out = <_Alt>[];
    _flatten(t, params, out, <String>{});
    _altsMemo[key] = out;
    return out;
  }

  void _flatten(
      TypeExpr t, Set<String> params, List<_Alt> out, Set<String> seen) {
    if (t is PrimitiveModeAlt) {
      out.add(_LeafAlt(_Leaf.any));
      return;
    }
    if (t is ConstantAlt) {
      out.add(_ConstAlt(t.value));
      return;
    }
    if (t is StructAlt) {
      out.add(_StructAlt(t.functor, t.args));
      return;
    }
    if (t is ListNilAlt) {
      out.add(_NilAlt());
      return;
    }
    if (t is ListConsAlt) {
      out.add(_ConsAlt(t.head, t.tail));
      return;
    }
    if (t is! TypeRef) {
      throw CompileError(
          'The construct of a question of type ${typeSource(t)} is not built: '
          'a difference list is not formed from a person\'s input',
          t.line, t.column, phase: 'analyzer');
    }
    final leaf = _leafOf(t.name, params);
    if (leaf != null) {
      out.add(_LeafAlt(leaf));
      return;
    }
    if (t.name == 'MutualRef') {
      throw CompileError(
          'The construct of a question of type MutualRef is not built: a '
          'mutual reference is not a term the person or the bridge holds',
          t.line, t.column, phase: 'analyzer');
    }
    final key = typeSource(_unmoded(t));
    if (!seen.add(key)) return;
    final def = resolve(t.name);
    if (def == null) {
      throw CompileError(
          'The type ${t.name} of an interactive variable is defined neither '
          'in the source nor in its scope, so its construct cannot be built',
          t.line, t.column, phase: 'analyzer');
    }
    final m = <String, TypeExpr>{
      for (var i = 0; i < def.typeParams.length && i < t.typeArgs.length; i++)
        def.typeParams[i]: t.typeArgs[i]
    };
    for (final a in def.alternatives) {
      final s = m.isEmpty ? a : _subst(a, m);
      if (s is TypeRef && s.isInput) {
        throw CompileError(
            'The construct of a question of type ${t.name} is not built: its '
            'alternative ${typeSource(s)} is a type in the other mode',
            t.line, t.column, phase: 'analyzer');
      }
      _flatten(s, params, out, seen);
    }
  }

  _Leaf? _leafOf(String name, Set<String> params) {
    if (params.contains(name)) return _Leaf.param;
    switch (name) {
      case 'Integer':
        return _Leaf.integer;
      case 'Real':
        return _Leaf.real;
      case 'Number':
        return _Leaf.number;
      case 'String':
        return _Leaf.string;
      case 'Constant':
        return _Leaf.constant;
      case 'Module':
        return _Leaf.module;
    }
    return null;
  }

  /// The alternatives of a node, with each a leaf of its own.
  List<_Alt> _nodeAlts(_Node n) => _alts(n.type, n.params);

  /// The single leaf a node is, or null.
  _Leaf? _soleLeaf(_Node n) {
    final alts = _nodeAlts(n);
    if (alts.length == 1 && alts.single is _LeafAlt) {
      return (alts.single as _LeafAlt).leaf;
    }
    return null;
  }

  /// The element of a stream type: its alternatives are [] and [E | S], S
  /// the same type.
  TypeExpr? _streamElement(_Node n) {
    final alts = _nodeAlts(n);
    if (alts.length != 2) return null;
    final nil = alts.whereType<_NilAlt>();
    final cons = alts.whereType<_ConsAlt>();
    if (nil.length != 1 || cons.length != 1) return null;
    final c = cons.single;
    if (_isInput(c.tail)) return null;
    if (typeSource(_unmoded(c.tail)) != n.typeKey) return null;
    return c.head;
  }

  /// The children of an alternative, as nodes.
  List<_Node> _children(_Node n, _Alt a) {
    if (a is _StructAlt) return [for (final t in a.args) n.child(t)];
    if (a is _ConsAlt) return [n.child(a.head), n.child(a.tail)];
    return const [];
  }

  /// Whether a position of [n] or below it is written by the person.
  bool _hasQuestion(_Node n) {
    final memo = _questionMemo[n.key];
    if (memo != null) return memo;
    if (n.person) return _questionMemo[n.key] = true;
    _questionMemo[n.key] = false; // least fixpoint through recursive types
    var result = false;
    for (final a in _nodeAlts(n)) {
      for (final c in _children(n, a)) {
        if (_hasQuestion(c)) result = true;
      }
    }
    return _questionMemo[n.key] = result;
  }

  /// Whether a position the person writes contains one the program writes.
  bool _hasProgramInside(_Node n, Set<String> seen) {
    if (!n.person) return true;
    if (!seen.add(n.key)) return false;
    for (final a in _nodeAlts(n)) {
      for (final c in _children(n, a)) {
        if (_hasProgramInside(c, seen)) return true;
      }
    }
    return false;
  }

  /// A leaf of a position the person writes that no forming clause can type,
  /// or null.
  _Leaf? _unformable(_Node n, Set<String> seen) {
    if (!seen.add(n.key)) return null;
    for (final a in _nodeAlts(n)) {
      if (a is _LeafAlt && a.leaf == _Leaf.param) return a.leaf;
      for (final c in _children(n, a)) {
        final l = _unformable(c, seen);
        if (l != null) return l;
      }
    }
    return null;
  }

  /// The construct forms every position the person writes: none is written by
  /// the program; and none is of a type parameter, of which no input forms a
  /// term the checker can type (the paper is silent on parametrised
  /// interactive types: vGLP #4 Cowork, 2026-10-02 08:26 UTC, item 2).  The
  /// Definition now gives such a position a text field (vGLP #6 Cowork,
  /// 2026-10-11 12:55 UTC, item 2, Q2), and the widget is text, but its
  /// forming clause, form_any/2's Formed(_) passed where Formed(X) is read,
  /// is refused by the checker at the abstract instance (TGLP
  /// parameterized-types.tex, sec:abstract-parameters): the refusal stands
  /// until the paper says how the construct forms a term of a parameter.
  void _checkPersonWritable(_Node n, InteractiveType t) {
    if (_hasProgramInside(n, {})) {
      throw CompileError(
          'The construct of the interactive type ${t.written} is not built: '
          'the person writes ${n.typeKey}, and a position of it is written by '
          'the program, which the construct can neither form nor show',
          t.line, t.column, phase: 'analyzer');
    }
    switch (_unformable(n, {})) {
      case _Leaf.param:
        throw CompileError(
            'The construct of the interactive type ${t.written} is not built: '
            'the person writes ${n.typeKey}, and a position of it is of a type '
            'parameter, of which no input forms a term of a known type',
            t.line, t.column, phase: 'analyzer');
      default:
        return;
    }
  }

  /// A stream or a list the person writes: [n] written by the person, of a
  /// type whose alternatives are [] and [E | S], S the same type, named
  /// `Stream` or `List` --- the two told apart by the type's name, the
  /// Answer rule allowing [] "for a list" and not for a stream (Definition
  /// "vmaGLP Transition System", Answer; vGLP #6 Cowork, 2026-10-11 12:55
  /// UTC, item 2, Q1).  Its element type and whether it is a list; null for
  /// any other node.
  ({TypeExpr elem, bool list})? _personSeq(_Node n) {
    if (!n.person) return null;
    final t = n.type;
    if (t is! TypeRef || (t.name != 'Stream' && t.name != 'List')) return null;
    final e = _streamElement(n);
    if (e == null) return null;
    return (elem: e, list: t.name == 'List');
  }

  /// Whether [k], a child of [n], is a stream or a list inside a form the
  /// person writes: "submitted as a question of its own and drawn under the
  /// form" (Definition "Widget Declaration, Default Widget"), the form's
  /// answer carrying a fresh variable at its position (Definition "vmaGLP
  /// Transition System", Answer: "a reader Y_i? at each position the person
  /// writes whose type is a stream or list type").
  bool _isNestedSeq(_Node n, _Node k) =>
      n.person && _personSeq(n) == null && _personSeq(k) != null;

  /// Whether [n], a position the person writes that is not itself a stream
  /// or a list, holds a stream or a list inside it, in an argument or in an
  /// argument's argument: its answer leaves that one open, a question of its
  /// own.
  bool _holdsNested(_Node n) {
    if (!n.person || _personSeq(n) != null) return false;
    final memo = _nestedMemo[n.key];
    if (memo != null) return memo;
    _nestedMemo[n.key] = false; // least fixpoint through recursive types
    var result = false;
    for (final a in _nodeAlts(n)) {
      if (a is! _StructAlt) continue;
      for (final c in _children(n, a)) {
        if (_isNestedSeq(n, c) || _holdsNested(c)) result = true;
      }
    }
    return _nestedMemo[n.key] = result;
  }

  /// Whether [t] is `Key`, or a type defined as `Key`, directly or through
  /// such definitions: an agent identifier, drawn as a peer field whatever
  /// the type is named (vGLP #6 Cowork, 2026-10-11 12:55 UTC, item 3, Q1).
  bool _isKey(TypeExpr t, [Set<String>? seen]) {
    if (t is! TypeRef || t.isInput || t.typeArgs.isNotEmpty) return false;
    if (t.name == 'Key') return true;
    final s = seen ?? <String>{};
    if (!s.add(t.name)) return false;
    final def = resolve(t.name);
    if (def == null || def.typeParams.isNotEmpty) return false;
    final alts = def.alternatives;
    return alts.length == 1 && _isKey(alts.single, s);
  }

  /// Whether the view of [n] can change once drawn: a position the program
  /// writes holding a stream the program writes, or one the person writes
  /// whose answer leaves a stream or list open under it.
  bool _multi(_Node n) {
    if (n.person) return _holdsNested(n);
    final memo = _multiMemo[n.key];
    if (memo != null) return memo;
    _multiMemo[n.key] = false;
    var result = _streamElement(n) != null;
    if (!result) {
      for (final a in _nodeAlts(n)) {
        for (final c in _children(n, a)) {
          if (_multi(c)) result = true;
        }
      }
    }
    return _multiMemo[n.key] = result;
  }

  // --- names --------------------------------------------------------------------

  String _name(String role, _Node n, [String suffix = '']) {
    final key = '$role:${n.typeKey}:$suffix';
    return _names.putIfAbsent(key, () {
      final stem = _typeStem(n.type);
      return fresh(suffix.isEmpty ? '${role}_$stem' : '${role}_${stem}_$suffix');
    });
  }

  /// The suffix of the procedures of alternative [a] of node [n], join_N and
  /// start_N: `cons` for a list cell, and for a structure its functor's
  /// characters made a name, each character outside a name (a letter, a
  /// digit or `_`, lexer.dart) replaced by `_`, then its arity ---
  /// `'odd one'(Choice)` odd_one_1.  Two alternatives of one node whose
  /// suffixes so made coincide are told apart by `_2`, `_3`, ... in the
  /// order met, as the Definition's t is (canonical.dart, THE ASK; vGLP #6
  /// Cowork, 2026-10-11 12:55 UTC, Task A).
  String _altSuffix(_Node n, _Alt a) {
    if (a is! _StructAlt) return 'cons';
    final raw = '${a.functor}/${a.args.length}';
    return _suffixes.putIfAbsent('${n.typeKey}|$raw', () {
      final made = a.functor.replaceAll(RegExp(r'[^A-Za-z0-9_]'), '_');
      final stem = '${made}_${a.args.length}';
      final used = _suffixesUsed.putIfAbsent(n.typeKey, () => <String>{});
      var s = stem;
      for (var k = 2; !used.add(s); k++) {
        s = '${stem}_$k';
      }
      return s;
    });
  }

  String _declParams(_Node n) {
    final used = <String>{};
    void scan(TypeExpr t) {
      if (t is TypeRef) {
        if (n.params.contains(t.name)) used.add(t.name);
        t.typeArgs.forEach(scan);
      }
    }

    scan(n.type);
    return used.isEmpty ? '' : '(${used.join(', ')})';
  }

  // --- a question: the person writes the position -------------------------------

  /// answer_N(P, X, Gs?, Gs1, Done): the question X of node [n], at the
  /// position P of its construct, taking from the grants Gs the first for P
  /// whose input forms a term of its type --- each, for a stream type,
  /// forming one element, the stream's tail keeping P --- and passing on
  /// every other in Gs1; Done once answered, never for a stream.  Its grants
  /// never close, so it has no clause for []: a question whose grants never
  /// come stays open, its goal suspended (vGLP #5 Cowork, 2026-10-03 08:16
  /// UTC, item 3, and 21:13 UTC, Q1).  The grant reaches it whole,
  /// input(Id, P1, R); it guards on P1 being its own P and forms the term
  /// from R, and a grant for another position goes on through take_N's
  /// refused clause, rebuilt whole (2026-10-04 09:05 UTC, C).
  ///
  /// A list the person writes is ended by the person: a grant of [] for its
  /// position binds the rest of the list to [], and the question is
  /// answered; a stream is not, its question never answered (Definition
  /// "vmaGLP Transition System", Answer: "or [] for a list"; vGLP #6 Cowork,
  /// 2026-10-11 12:55 UTC, item 2, Q1).
  ///
  /// A question of a type holding a stream or a list inside a form the
  /// person writes ([_holdsNested]) is answer_N(P, X, Gs?, Gs1, Vs, Done):
  /// its answer carries a fresh variable at each such position, which stays
  /// open, a question of its own at its own position, answered from the
  /// grants that follow; Vs the one view after the answer, the term answered
  /// with each such position marked input(Q) (open_N); Done once every one
  /// of them is (Definition "Widget Declaration, Default Widget": "a stream
  /// or list inside a form the person writes: submitted as a question of
  /// its own and drawn under the form"; vGLP #6 Cowork, 2026-10-11 12:55 UTC,
  /// item 2).
  String _answer(_Node n) {
    final name = _name('answer', n);
    if (!_emitted.add('answer:${n.typeKey}')) return name;
    final take = _name('take', n);
    final t = n.typeKey;
    final p = _declParams(n);
    final done = gen.doneType;
    final formed = gen.formedType;
    final gs = gen.inputsType;
    final pt = gen.pathType;
    final seq = _personSeq(n);
    final e = seq == null ? null : n.child(seq.elem);
    if (e != null && _holdsNested(e)) {
      final at = _at!;
      throw CompileError(
          'The construct of the interactive type ${at.written} is not built: '
          'the person writes ${n.typeKey}, whose element ${e.typeKey} holds a '
          'stream or a list, a question of its own at each element\'s '
          'answer, and the elements of a stream or a list share one position',
          at.line, at.column, phase: 'analyzer');
    }
    final nested = _holdsNested(n);
    final vs = nested ? ', Vs' : '';
    final vsOut = nested ? ', Vs?' : '';
    final vsType = nested ? ', Stream(_)' : '';
    final form = _former(e ?? n);
    final open = nested ? _open(n) : null;
    _out.writeln('procedure$p $name($pt?, $t, $gs?, $gs$vsType, $done).');
    if (seq != null && seq.list) {
      _out.writeln('$name(P, [], [input(_, P1, []) | Gs], Gs?, done) :- '
          'P1? =?= P? | true.');
    }
    _out
      ..writeln('$name(P, X?, [input(Id, P1, R) | Gs], Gs1?$vsOut, Done?) :- '
          'P1? =?= P?, ground(R?) | $form(R?, F), '
          '$take(F?, P?, X, input(Id?, P1?, R?), Gs?, Gs1$vs, Done).')
      ..writeln('$name(P, X?, [input(Id, P1, R) | Gs], Gs1?$vsOut, Done?) :- '
          'otherwise | '
          '$take(refused, P?, X, input(Id?, P1?, R?), Gs?, Gs1$vs, Done).')
      ..writeln()
      ..writeln('procedure$p $take($formed(${(e ?? n).typeKey})?, $pt?, $t, '
          '${gen.inputType}?, $gs?, $gs$vsType, $done).');
    if (e != null) {
      _out.writeln('$take(formed(V), P, [V? | X1?], _, Gs, Gs1?, Done?) :- '
          '$name(P?, X1, Gs?, Gs1, Done).');
    } else if (nested) {
      _out.writeln('$take(formed(V), P, X?, _, Gs, Gs1?, [W?], Done?) :- '
          '$open(V?, P?, X, Gs?, Gs1, W, Done).');
    } else {
      _out.writeln('$take(formed(V), _, V?, _, Gs, Gs?, done).');
    }
    _out
      ..writeln('$take(refused, P, X?, G, Gs, [G? | Gs1?]$vsOut, Done?) :- '
          '$name(P?, X, Gs?, Gs1$vs, Done).')
      ..writeln();
    return name;
  }

  /// open_N(V?, P, X, Gs?, Gs1, W, Done): X the answer of node [n], a type
  /// holding a stream or a list inside a form the person writes, from V, the
  /// term formed from the person's input, which holds [] at each such
  /// position: X is V with a fresh variable there, its writer answered at
  /// its own position below P from the grants Gs, a question of its own; W
  /// the view of the answer, V with each such position marked input(Q);
  /// Done once every one of them is answered.  An alternative holding none
  /// is X = V, its view V, answered at once.
  String _open(_Node n) {
    final name = _name('open', n);
    if (!_emitted.add('open:${n.typeKey}')) return name;
    final t = n.typeKey;
    final p = _declParams(n);
    final gs = gen.inputsType;
    final clauses = <String>[];
    for (final a in _nodeAlts(n)) {
      if (a is! _StructAlt) continue;
      final kids = _children(n, a);
      if (!kids.any((k) => _isNestedSeq(n, k) || _holdsNested(k))) continue;
      final head = <String>[], xs = <String>[], views = <String>[];
      final goals = <String>[], guards = <String>[], dones = <String>[];
      var gsIn = 'Gs';
      var g = 0;
      var pathReads = 0;
      for (var i = 0; i < kids.length; i++) {
        final k = kids[i];
        final idx = i + 1;
        if (_isNestedSeq(n, k)) {
          final y = 'Y$idx', m = 'M$idx', q = 'Q$idx', d = 'D$idx';
          final gsOut = 'Gs${++g}';
          head.add('_');
          xs.add('$y?');
          views.add('input($m?)');
          goals
            ..add('${gen.path}(P?, $idx, $m)')
            ..add('${gen.path}(P?, $idx, $q)')
            ..add('${_answer(k)}($q?, $y, $gsIn?, $gsOut, $d)');
          pathReads += 2;
          gsIn = gsOut;
          dones.add(d);
        } else if (_holdsNested(k)) {
          final h = 'H$idx', x = 'X$idx', v = 'V$idx', q = 'Q$idx';
          final d = 'D$idx';
          final gsOut = 'Gs${++g}';
          head.add(h);
          xs.add('$x?');
          views.add('$v?');
          final open = _open(k);
          goals
            ..add('${gen.path}(P?, $idx, $q)')
            ..add('$open($h?, $q?, $x, $gsIn?, $gsOut, $v, $d)');
          pathReads += 1;
          gsIn = gsOut;
          dones.add(d);
        } else {
          final h = 'H$idx';
          head.add(h);
          xs.add('$h?');
          views.add('$h?');
          guards.add('ground($h?)');
        }
      }
      if (pathReads > 1) guards.insert(0, 'ground(P?)');
      String doneHead;
      if (dones.length == 1) {
        doneHead = '${dones.single}?';
      } else {
        doneHead = 'Done?';
        goals.add(
            '${gen.allDone}([${dones.map((d) => '$d?').join(', ')}], Done)');
      }
      final g0 = guards.isEmpty ? '' : '${guards.join(', ')} | ';
      clauses.add('$name(${_apply(a, head)}, P, ${_apply(a, xs)}, Gs, '
          '$gsIn?, ${_apply(a, views)}, $doneHead) :- $g0${goals.join(', ')}.');
    }
    _out.writeln('procedure$p $name($t?, ${gen.pathType}?, $t, $gs?, $gs, _, '
        '${gen.doneType}).');
    for (final c in clauses) {
      _out.writeln(c);
    }
    _out
      ..writeln('$name(V, _, X?, Gs, Gs?, W?, done) :- '
          'ground(V?) | X = V?, W = V?.')
      ..writeln();
    return name;
  }

  /// form_N(R?, F): F formed(X), X the person's input R as a term of node
  /// [n]'s type; or F refused.  A primitive type's is the generic one.  A
  /// constant or nil alternative forms itself; a structure or cons
  /// alternative calls its children's formers and joins their results
  /// (join_N); a primitive alternative of a union is formed by its guard, the
  /// term written in the body, where the guard narrows it (TGLP
  /// typed-glp.tex, "Type checking of guards"; vGLP #5 Cowork, 2026-10-03
  /// 21:13 UTC, Q2).
  String _former(_Node n) {
    final leaf = _soleLeaf(n);
    if (leaf != null) return gen._leafFormers[leaf]!;
    final name = _name('form', n);
    if (!_emitted.add('form:${n.typeKey}')) return name;
    final t = n.typeKey;
    final p = _declParams(n);
    final formed = gen.formedType;
    final alts = _nodeAlts(n);
    final lines = <String>[];
    final leaves = <String>[];
    for (final a in alts) {
      if (a is _ConstAlt) {
        final c = constantSource(a.value);
        lines.add('$name($c, formed($c)).');
      } else if (a is _NilAlt) {
        lines.add('$name([], formed([])).');
      } else if (a is _StructAlt || a is _ConsAlt) {
        // A stream or a list inside the form is not formed from the input:
        // it holds [] in the term formed, and the answer a fresh variable
        // there, a question of its own (open_N).
        final kids = _children(n, a);
        final k = kids.length;
        final own = [
          for (var i = 0; i < k; i++)
            if (!_isNestedSeq(n, kids[i])) i
        ];
        final rs = [
          for (var i = 0; i < k; i++) own.contains(i) ? 'R${i + 1}' : '_'
        ];
        if (own.isEmpty) {
          lines.add('$name(${_apply(a, rs)}, '
              'formed(${_apply(a, List.filled(k, '[]'))})).');
          continue;
        }
        final calls = [
          for (var j = 0; j < own.length; j++)
            '${_former(kids[own[j]])}(R${own[j] + 1}?, F${j + 1})',
          '${_join(n, a, kids)}('
              '${[for (var j = 0; j < own.length; j++) 'F${j + 1}?'].join(', ')}, F)',
        ];
        lines.add('$name(${_apply(a, rs)}, F?) :- ${calls.join(', ')}.');
      } else if (a is _LeafAlt) {
        leaves.add('$name(R, F?) :- ${_guard(a.leaf)}(R?) | '
            'F = formed(R?).');
      }
    }
    _out.writeln('procedure$p $name(_?, $formed($t)).');
    for (final l in [...lines, ...leaves]) {
      _out.writeln(l);
    }
    _out
      ..writeln('$name(_, refused) :- otherwise | true.')
      ..writeln();
    return name;
  }

  /// join_N(F1?, ..., Fk?, F): the result of forming a structure or cons
  /// alternative of node [n] from its children's, formed(f(X1?, ..., Xk?))
  /// where each child formed, and refused where any was refused, one clause
  /// per position (vGLP #5 Cowork, 2026-10-03 08:16 UTC, item 3).  A stream
  /// or a list inside a form the person writes is no child formed: it is []
  /// in the term (open_N).
  String _join(_Node n, _Alt a, List<_Node> kids) {
    final suffix = _altSuffix(n, a);
    final name = _name('join', n, suffix);
    if (!_emitted.add('join:${n.typeKey}:$suffix')) return name;
    final p = _declParams(n);
    final formed = gen.formedType;
    final own = [
      for (final c in kids)
        if (!_isNestedSeq(n, c)) c
    ];
    final k = own.length;
    final ins = [for (final c in own) '$formed(${c.typeKey})?'];
    var j = 0;
    final args = [
      for (final c in kids) _isNestedSeq(n, c) ? '[]' : 'X${++j}?'
    ];
    _out
      ..writeln('procedure$p $name(${ins.join(', ')}, $formed(${n.typeKey})).')
      ..writeln('$name(${[for (var i = 0; i < k; i++) 'formed(X${i + 1})'].join(', ')}, '
          'formed(${_apply(a, args)})).');
    for (var i = 0; i < k; i++) {
      _out.writeln('$name('
          '${[for (var j = 0; j < k; j++) j == i ? 'refused' : '_'].join(', ')}, '
          'refused).');
    }
    _out.writeln();
    return name;
  }

  // --- output: the program writes the position ----------------------------------

  /// present_N(P, X?, Gs?, Gs1, Vs, Done): node [n], at the position P of
  /// its construct, which the program writes and which holds a question: the
  /// views of X as it is written, each question in it marked input(Q), Q its
  /// position, P extended by the argument index of each step down to it; its
  /// questions answered from the grants Gs, each taking those for its own
  /// position, the grants they do not take passed on in Gs1, Done once every
  /// one is answered.  The grants never close (Inputs).
  String _present(_Node n, InteractiveType it) {
    final name = _name('present', n);
    if (!_emitted.add('present:${n.typeKey}')) return name;
    if (_streamElement(n) != null) {
      throw CompileError(
          'The construct of the interactive type ${it.written} is not built: '
          'the program writes the stream ${n.typeKey}, whose elements hold '
          'questions',
          it.line, it.column, phase: 'analyzer');
    }
    final t = n.typeKey;
    final p = _declParams(n);
    final done = gen.doneType;
    final clauses = <String>[];
    final later = <void Function()>[];
    for (final a in _nodeAlts(n)) {
      if (a is _ConstAlt) {
        final c = constantSource(a.value);
        clauses.add('$name(_, $c, Gs, Gs?, [$c], done).');
      } else if (a is _NilAlt) {
        clauses.add('$name(_, [], Gs, Gs?, [[]], done).');
      } else if (a is _LeafAlt) {
        clauses.add('$name(_, X, Gs, Gs?, [X?], done) :- '
            '${_guard(a.leaf)}(X?) | true.');
      } else {
        clauses.add(_presentStruct(n, a, name, later, it));
      }
    }
    _out.writeln('procedure$p $name(${gen.pathType}?, $t?, '
        '${gen.inputsType}?, ${gen.inputsType}, Stream(_), $done).');
    for (final c in clauses) {
      _out.writeln(c);
    }
    _out.writeln();
    for (final f in later) {
      f();
    }
    return name;
  }

  String _presentStruct(_Node n, _Alt a, String name,
      List<void Function()> later, InteractiveType it) {
    final kids = _children(n, a);
    final head = <String>[];
    final goals = <String>[];
    final guards = <String>[];
    final views = <String>[];
    final dones = <String>[];
    var gsIn = 'Gs';
    var g = 0;
    // The position of the argument i of this alternative is the node's own,
    // P, with i at its end: a question's is read twice, for its mark in the
    // view and for its answer_N, and a structure's holding one once, for its
    // present_N.  P is ground, so where it is read more than once the clause
    // guards on it.
    var pathReads = 0;
    final direct = kids.every((k) => k.person
        ? !_holdsNested(k)
        : (!_hasQuestion(k) && !_multi(k)));
    for (var i = 0; i < kids.length; i++) {
      final k = kids[i];
      final h = 'H${i + 1}';
      final idx = i + 1;
      if (k.person) {
        _checkPersonWritable(k, it);
        head.add('$h?');
        final gsOut = 'Gs${++g}';
        final d = 'D${i + 1}';
        final m = 'M$idx', q = 'Q$idx';
        // A question holding a stream or a list inside a form the person
        // writes is drawn again once answered, that one open under it.
        final nested = _holdsNested(k);
        final w = 'W$idx';
        goals
          ..add('${gen.path}(P?, $idx, $m)')
          ..add('${gen.path}(P?, $idx, $q)')
          ..add('${_answer(k)}($q?, $h, $gsIn?, $gsOut'
              '${nested ? ', $w' : ''}, $d)');
        pathReads += 2;
        gsIn = gsOut;
        dones.add(d);
        views.add(direct
            ? 'input($m?)'
            : nested
                ? '[input($m?) | $w?]'
                : '[input($m?)]');
      } else if (!_hasQuestion(k)) {
        head.add(h);
        if (direct) {
          guards.add('ground($h?)');
          views.add('$h?');
        } else if (_streamElement(k) != null) {
          goals.add('${gen.thread}($h?, V${i + 1})');
          views.add('V${i + 1}?');
        } else {
          goals.add('${gen.shown}($h?, V${i + 1})');
          views.add('V${i + 1}?');
        }
      } else {
        head.add(h);
        final gsOut = 'Gs${++g}';
        final d = 'D${i + 1}';
        final q = 'Q$idx';
        goals
          ..add('${gen.path}(P?, $idx, $q)')
          ..add('${_present(k, it)}($q?, $h?, $gsIn?, $gsOut, V${i + 1}, $d)');
        pathReads += 1;
        gsIn = gsOut;
        dones.add(d);
        views.add('V${i + 1}?');
      }
    }
    if (pathReads > 1) guards.insert(0, 'ground(P?)');
    final pattern = _apply(a, head);
    final gsHead = gsIn == 'Gs' ? 'Gs?' : '$gsIn?';
    String doneHead;
    if (dones.isEmpty) {
      doneHead = 'done';
    } else if (dones.length == 1) {
      doneHead = '${dones.single}?';
    } else {
      doneHead = 'Done?';
      goals.add('${gen.allDone}([${dones.map((d) => '$d?').join(', ')}], Done)');
    }
    String vsHead;
    if (direct) {
      vsHead = '[${_apply(a, views)}]';
    } else {
      final start = _combiner(n, a, kids);
      goals.add('$start(${views.join(', ')}, Vs)');
      vsHead = 'Vs?';
    }
    final g0 = guards.isEmpty ? '' : '${guards.join(', ')} | ';
    final body = goals.isEmpty
        ? (guards.isEmpty ? '' : ' :- $g0' 'true')
        : ' :- $g0${goals.join(', ')}';
    // An alternative holding no question does not read its position.
    final at = pathReads == 0 ? '_' : 'P';
    return '$name($at, $pattern, Gs, $gsHead, $vsHead, $doneHead)$body.';
  }

  /// start_N_f(V1s?, ..., Vns?, Vs): the views of a structure from the views
  /// of its arguments, again each time one of them changes.
  String _combiner(_Node n, _Alt a, List<_Node> kids) {
    final suffix = _altSuffix(n, a);
    final start = _name('start', n, suffix);
    if (!_emitted.add('start:${n.typeKey}:$suffix')) return start;
    final p = _declParams(n);
    final k = kids.length;
    final streams = List.filled(k, 'Stream(_)?').join(', ');
    final vs = [for (var i = 0; i < k; i++) 'V${i + 1}'];
    final vr = [for (var i = 0; i < k; i++) 'V${i + 1}?'];
    final ground = [for (var i = 0; i < k; i++) 'ground(V${i + 1}?)'].join(', ');
    final multi = [for (final c in kids) _multi(c)];
    // An argument's views never close before their first, but each argument
    // is a stream, whose [] the procedure covers.
    final closed = [
      for (var i = 0; i < k; i++)
        '$start(${[for (var j = 0; j < k; j++) j == i ? '[]' : '_'].join(', ')}, [])'
            '.'
    ];
    if (!multi.contains(true)) {
      _out
        ..writeln('procedure$p $start($streams, Stream(_)).')
        ..writeln('$start(${[for (final v in vs) '[$v | _]'].join(', ')}, '
            '[${_apply(a, vr)}]).');
      closed.forEach(_out.writeln);
      _out.writeln();
      return start;
    }
    final comb = _name('comb', n, suffix);
    final ss = [for (var i = 0; i < k; i++) 'S${i + 1}'];
    final sr = [for (var i = 0; i < k; i++) 'S${i + 1}?'];
    _out
      ..writeln('procedure$p $start($streams, Stream(_)).')
      ..writeln('$start(${[for (var i = 0; i < k; i++) '[${vs[i]} | ${ss[i]}]'].join(', ')}, '
          '[${_apply(a, vr)} | Vs?]) :- $ground | '
          '$comb(${vr.join(', ')}, ${sr.join(', ')}, Vs).');
    closed.forEach(_out.writeln);
    _out
      ..writeln()
      ..writeln('procedure$p $comb(${List.filled(k, '_?').join(', ')}, '
          '$streams, Stream(_)).');
    for (var i = 0; i < k; i++) {
      if (!multi[i]) continue;
      final olds = [
        for (var j = 0; j < k; j++) j == i ? '_' : vs[j]
      ];
      final ins = [
        for (var j = 0; j < k; j++) j == i ? '[${vs[j]} | ${ss[j]}]' : ss[j]
      ];
      _out.writeln('$comb(${olds.join(', ')}, ${ins.join(', ')}, '
          '[${_apply(a, vr)} | Vs?]) :- $ground | '
          '$comb(${vr.join(', ')}, ${sr.join(', ')}, Vs).');
    }
    _out
      ..writeln('$comb(${List.filled(k, '_').join(', ')}, '
          '${List.filled(k, '[]').join(', ')}, []).')
      ..writeln();
    return start;
  }

  // --- widgets --------------------------------------------------------------------

  /// The widget of node [n]: the one a declaration in scope names for its
  /// moded type, else its default (Definition "Widget Declaration, Default
  /// Widget"), as the term vGLP's Q3 names (2026-10-01 23:55 UTC, E).
  String _widget(_Node n, Set<String> visiting) {
    final d = declared[n.moded];
    if (d != null) return constantSource(d);
    if (!visiting.add(n.key)) {
      if (n.person) {
        // "a recursive type the person writes that is not a list is refused
        // by the compiler with a message naming the type, until the paper
        // gives it a widget" (vGLP #6 Cowork, 2026-10-11 12:55 UTC, item 2,
        // Q1).
        final at = _at!;
        throw CompileError(
            'The construct of the interactive type ${at.written} is not '
            'built: the person writes ${n.typeKey}, a recursive type that is '
            'neither a Stream nor a List, and the Definition gives it no '
            'widget (Definition "Widget Declaration, Default Widget")',
            at.line, at.column, phase: 'analyzer');
      }
      return "ref(${constantSource(n.moded)})";
    }
    try {
      return _defaultWidget(n, visiting);
    } finally {
      visiting.remove(n.key);
    }
  }

  String _defaultWidget(_Node n, Set<String> visiting) {
    final t = n.type;
    if (!n.person) {
      if (!_hasQuestion(n)) {
        return _streamElement(n) != null ? 'thread' : 'shown';
      }
      final alts = _nodeAlts(n);
      if (alts.length == 1 && _isPicker(n, alts.single)) return 'picker';
      if (alts.length == 1) return _altWidget(n, alts.single, visiting);
      return 'menu([${alts.map((a) => _altWidget(n, a, visiting)).join(', ')}])';
    }
    if (t is TypeRef && t.name == 'Date') return 'date';
    // An agent identifier: Peer, or Key or a type defined as Key, whatever
    // it is named (vGLP #6 Cowork, 2026-10-11 12:55 UTC, item 3, Q1).
    if (t is TypeRef && (t.name == 'Peer' || _isKey(t))) return 'peer';
    // A stream the person writes: the widget of its element type, kept open,
    // one element per submission; a list the person writes: the same, with
    // a button that ends the list (Definition "Widget Declaration, Default
    // Widget").
    final seq = _personSeq(n);
    if (seq != null) {
      final w = _widget(n.child(seq.elem), visiting);
      return seq.list ? 'input_list($w)' : 'input_box($w)';
    }
    final alts = _nodeAlts(n);
    if (alts.every((a) => a is _ConstAlt)) {
      final cs = alts.map((a) => constantSource((a as _ConstAlt).value));
      return alts.length == 1 ? 'button(${cs.single})' : 'buttons([${cs.join(', ')}])';
    }
    if (alts.length == 1) return _altWidget(n, alts.single, visiting);
    return 'menu([${alts.map((a) => _altWidget(n, a, visiting)).join(', ')}])';
  }

  /// The widget of alternative [a] of node [n].  A tuple is a form with one
  /// field per argument, field(L, W), "each labelled by the name of its
  /// declared type", L that name (Definition "Widget Declaration, Default
  /// Widget"; vGLP #6 Cowork, 2026-10-11 12:55 UTC, item 3, Q2); a stream or
  /// a list inside a form the person writes is no field of it but
  /// question(L, W), "submitted as a question of its own and drawn under the
  /// form" (item 2).
  String _altWidget(_Node n, _Alt a, Set<String> visiting) {
    if (a is _ConstAlt) {
      return n.person ? 'button(${constantSource(a.value)})' : 'shown';
    }
    if (a is _NilAlt) return n.person ? 'button([])' : 'shown';
    if (a is _LeafAlt) return n.person ? _leafWidget(a.leaf) : 'shown';
    if (!n.person && !_altHasQuestion(n, a)) return 'shown';
    final kids = _children(n, a);
    final ws = [
      for (final k in kids)
        '${_isNestedSeq(n, k) ? 'question' : 'field'}'
            '(${constantSource(k.typeKey)}, ${_widget(k, visiting)})'
    ].join(', ');
    if (a is _ConsAlt) {
      if (n.person) {
        // A list cell the person writes outside a Stream or a List: the
        // Definition gives it no widget, and the compiler emits no
        // form('.', ...) (vGLP #6 Cowork, 2026-10-11 12:55 UTC, item 2, Q1).
        final at = _at!;
        throw CompileError(
            'The construct of the interactive type ${at.written} is not '
            'built: the person writes ${n.typeKey}, a list cell of which is '
            'neither a Stream nor a List, and the Definition gives it no '
            'widget (Definition "Widget Declaration, Default Widget")',
            at.line, at.column, phase: 'analyzer');
      }
      return 'form(\'.\', [$ws])';
    }
    return 'form(${constantSource((a as _StructAlt).functor)}, [$ws])';
  }

  bool _altHasQuestion(_Node n, _Alt a) =>
      _children(n, a).any(_hasQuestion);

  String _leafWidget(_Leaf l) {
    switch (l) {
      case _Leaf.string:
        return 'text';
      case _Leaf.integer:
      case _Leaf.real:
      case _Leaf.number:
        return 'number';
      case _Leaf.constant:
      case _Leaf.module:
      case _Leaf.any:
      case _Leaf.param:
        return 'text';
    }
  }

  /// A picker: "a list the program writes, with a choice coming back" ---
  /// a structure of a list the program writes and a position the person
  /// writes of the list's element type.
  bool _isPicker(_Node n, _Alt a) {
    if (a is! _StructAlt || a.args.length != 2) return false;
    final kids = _children(n, a);
    for (var i = 0; i < 2; i++) {
      final list = kids[i], choice = kids[1 - i];
      if (list.person || !choice.person || _hasQuestion(list)) continue;
      final e = _streamElement(list);
      if (e != null && typeSource(_unmoded(e)) == choice.typeKey) return true;
    }
    return false;
  }

  // --- text -----------------------------------------------------------------------

  /// The term of alternative [a] over argument texts [args].
  String _apply(_Alt a, List<String> args) {
    if (a is _ConsAlt) return '[${args[0]} | ${args[1]}]';
    final s = a as _StructAlt;
    return '${functorNameSource(s.functor)}(${args.join(', ')})';
  }

  String _guard(_Leaf l) {
    switch (l) {
      case _Leaf.integer:
        return 'integer';
      case _Leaf.real:
        return 'real';
      case _Leaf.number:
        return 'number';
      case _Leaf.string:
        return 'string';
      case _Leaf.constant:
        return 'constant';
      case _Leaf.module:
        return 'module';
      case _Leaf.any:
      case _Leaf.param:
        return 'ground';
    }
  }
}
