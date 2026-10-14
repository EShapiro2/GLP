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
//   - a position the program writes is shown as the program writes it, and
//     each widget position the program has not yet written whole --- the
//     root, a field, an element of any stream, the thread's included --- is
//     shown pending(P), P its position, until written whole, and shown again
//     once it is; a stream the program writes is shown as its elements so
//     far, its unwritten tail not shown, a thread; so the questions beside a
//     position not yet written are offered at once, and a pending position
//     and a stream's unwritten tail pass the grants after them on (vGLP
//     118e04d, Definition "Construct Process": "each widget position the
//     program has not yet written whole --- the root, a field, an element of
//     any stream, the thread's included --- drawn as pending, and a stream's
//     unwritten tail not drawn, so that the questions beside it are offered
//     at once"; vGLP #8 Cowork, 2026-10-11 22:51 UTC, Udi approved).  Whether
//     a position is written when it is drawn is decided by the first clause
//     that can reduce (GLP-Spec, Definition "cGLP Transition System",
//     Reduce): the written position's clause where it is, the pending clause
//     after it where it is not;
//   - a position the person writes is a QUESTION, shown as input(P), P its
//     position: the path of argument indices from the root of the
//     interactive variable's term to it, [] the root, a stream the person
//     writes having the stream's own path, each submission one element
//     (vGLP #5 Cowork, 2026-10-04 09:05 UTC, C); a grant forms a term of its
//     type and binds it, a stream type taking one element per grant, an
//     input box;
//   - the views are drawn as they change, draw(Id, W, f(V)), W the widget of
//     T and f the functor of its ask, so that every drawn value is of the
//     program's T, the type of its person channel: the union of the values
//     drawn, each under its ask's functor, construct 0's under output, and of
//     the grants' raw inputs, the types of its questions (vGLP, Definition
//     "Construct Process"; vGLP 2026-10-14 11:36 UTC, Udi: yes).  The drawn
//     value under each functor is typed _: the views are built by
//     procedures typed at _;
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
//     still to come, and on nothing else (run_f/5, f its ask's functor,
//     each drawn view under f): "The construct process withdraws when
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
// Stream, a list one named List, or a named type defined as one of them,
// `Lines ::= Stream(String)`, which draws as the stream it names (vGLP #7
// Cowork, 2026-10-11 14:31 UTC, item 2); the two are told apart by the name:
// a list is ended by the grant of its end button, a stream never, and the
// list's widget is input_list(W), the stream's input_box(W).  The end
// button grants [end], a list cell, which no element of a flat type can be
// (listEnd; 14:31 UTC, item 3).  A stream or a list inside a form the
// person writes is a question of its own: the form is formed with [] there,
// its answer carries a fresh variable there (open_N), answered at its own
// position from the grants that follow, and the construct is drawn again
// with the answer; its entry in the form's widget is question(L, W) and
// every other field(L, W), L the name of the field's declared type.  A
// position of type Key, or of a type defined as Key, is a peer field.
//
// FLAT TYPES (vGLP at 66b6500, Definition "Volitional Procedure,
// Interactive Type, Interactive Term, Ordinary Clause, vGLP Program": "An
// interactive type is flat: it is built from constants, String, Integer,
// Real, Key, Constant, Module, named types so built, tuples and unions, and
// Stream or List as the whole type or as an argument of a tuple, never under
// another Stream or List; it has no other recursive type, and no type
// parameter at a position the person writes").  Refused, each naming the
// type, over the whole interactive type, a structure the program writes
// included, one holding no question too (vGLP #7 Cowork, 2026-10-11 14:31
// UTC, items 1 and 2; vGLP #8 Cowork, 2026-10-11 15:47 UTC, items 1 and 3;
// _checkFlat): a stream or a list in an element of a stream or a list; a
// list cell outside a Stream or a List, a tuple '.'(H, T) being one; a
// recursive type other than a Stream or a List; a position the person
// writes of a type parameter, which has no widget; and a position of the
// type _, whoever writes it, none of the Definition's building blocks
// (vGLP #8 Cowork, 2026-10-11 17:24 UTC, Q2).
// So the compilation emits neither ref(T) nor form('.', ...).
//
// A STREAM OR A LIST THE PROGRAM WRITES WHOSE ELEMENTS HOLD QUESTIONS (vGLP
// #8 Cowork, 2026-10-11 15:47 UTC, item 2, Task D): each element presented
// as it arrives, at its own position, its questions answered as any grant;
// its widget thread(W), W the element type's, its views the elements' views
// so far, each pending until written, the unwritten tail not shown
// (_presentSeq).
//
// WHAT IS NOT BUILT, each a compile error naming the type: a position the
// program writes inside one the person writes (the Answer transition leaves
// it to the program, and the construct has no output to show for it yet,
// _checkPersonWritable); a difference list or a mutual reference.
// A position the person writes of type Real is built, formed by real/1 and
// drawn as a number field (vGLP #5 Cowork, 2026-10-03 08:16 UTC, item 6;
// Definition "Widget Declaration, Default Widget" at c2e8b57: "a number
// (Integer or Real)").

import '../compiler/error.dart';
import '../analysis/type_checker/type_ast.dart';
import '../compiler/glp_printer.dart' show constantSource, functorNameSource;
import 'mediator.dart' show typeSource;

/// The Definition an interactive type is refused by where it is not flat.
const _flatDefinition = 'vGLP, Definition "Volitional Procedure, Interactive '
    'Type, Interactive Term, Ordinary Clause, vGLP Program"';

/// The term the end button of a list the person writes grants, R in
/// input(Id, P, R): "a list the person writes: the same, with a button that
/// ends the list" (vGLP at 66b6500, Definition "Widget Declaration, Default
/// Widget"), its grant "a term of its own that no element can be" (vGLP #7
/// Cowork, 2026-10-11 14:31 UTC, item 3).  It is a list cell, and no element
/// of a list the person writes is one: the element type is flat, a list
/// cell occurring in it only as a Stream's or a List's, and a Stream or a
/// List never under another ([_Generator._checkFlat]); `[]`, a constant
/// (GLP-Spec appendix-lp.tex: "a constant (numbers, strings, or the empty
/// list [])"), may be an element.  The bridge's end button grants the same
/// (glp_multiagent/lib/ui_runtime/bridge.dart, listEnd).
const listEnd = '[end]';

/// A moded interactive type of the program: its type as written, its mode,
/// the functor the compilation gives it in Question, and the type parameters
/// of the procedure that declares it.  Every one is the program's own: "No
/// vGLP module imports from another module a procedure that reaches a
/// question, so the questions of a program are those of one module" (vGLP,
/// Definition "Canonical Compilation").
class InteractiveType {
  final TypeExpr type;
  final bool readerMode;
  final String functor;
  final List<String> params;
  final int line, column;

  InteractiveType(this.type, this.readerMode, this.functor, this.params,
      this.line, this.column);

  /// The moded type as written, `Card` or `Request?`.
  String get written => typeSource(type);
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

  /// The drawing of construct processes of the ask functor f, run_f/5, a
  /// name fresh against the program's for each f; and run_end/3.
  final String Function(String functor) run;
  final String runEnd;

  /// A procedure name fresh against the program's, for the drawing of the
  /// program's output.
  final String Function(String stem) output;
  final String shown;
  final String thread;
  final String consViews;
  final String allDone;
  final String path;
  final Map<_Leaf, String> _leafFormers;
  final String formedType;
  final String doneType;
  /// The type of what the person channel carries, T, the union the
  /// compilation forms; and the types at T the construct processes name:
  /// Draw(T), Input(T), Inputs(T), Path and PersonChannel(T) as applied.
  final String drawn;
  final String drawType;
  final String inputType;
  final String inputsType;
  final String pathType;
  final String personChannelType;
  final String spawnType;
  final String askType;

  GenericNames(
      {required this.dispatch,
      required this.constructs,
      required this.run,
      required this.runEnd,
      required this.output,
      required this.shown,
      required this.thread,
      required this.consViews,
      required this.allDone,
      required this.path,
      required String formString,
      required String formInteger,
      required String formNumber,
      required String formReal,
      required String formConstant,
      required String formModule,
      required this.formedType,
      required this.doneType,
      required this.drawn,
      required this.drawType,
      required this.inputType,
      required this.inputsType,
      required this.pathType,
      required this.personChannelType,
      required this.spawnType,
      required this.askType})
      : _leafFormers = {
          _Leaf.string: formString,
          _Leaf.integer: formInteger,
          _Leaf.real: formReal,
          _Leaf.number: formNumber,
          _Leaf.constant: formConstant,
          _Leaf.module: formModule,
        };

  /// The generic procedures the construct processes may call, by their
  /// generic names: the roots, with the dispatcher's entry point, of what the
  /// compilation emits of the generic source.
  static const used = [
    'run_end',
    'shown',
    'thread',
    'cons_views',
    'all_done',
    'path',
    'form_string',
    'form_integer',
    'form_number',
    'form_real',
    'form_constant',
    'form_module',
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
  TypeDef? output,
}) {
  final g = _Generator(
      generic, resolve, declared, fresh, questionType, constructName);
  return g.build(types, output: output);
}

/// The reserved name of the type of a program's output to its person: "A
/// vGLP program declares the type of its output to its person by defining
/// the type Output, a reserved name; the dispatcher draws that output as a
/// thread whose elements are drawn by the widget of Output" (vGLP 169b008,
/// Definition "Canonical Compilation").
const outputTypeName = 'Output';

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

/// A list cell; [from] the Stream or the List it was flattened from, as
/// written, where it is one's: `Stream(Integer)` where a union names it.
class _ConsAlt extends _Alt {
  final TypeExpr head, tail;
  final String? from;
  _ConsAlt(this.head, this.tail, [this.from]);
}

/// A primitive; [name] the type parameter's, where it is one.
class _LeafAlt extends _Alt {
  final _Leaf leaf;
  final String? name;
  _LeafAlt(this.leaf, [this.name]);
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
  final _suffixes = <String, String>{};
  final _suffixesUsed = <String, Set<String>>{};
  final _nestedMemo = <String, bool>{};

  /// The types of the grants' raw inputs, R in input(Id, P, R): each
  /// question's type, or its element type where it is a stream or a list,
  /// and the end button's grant of a list (vGLP, Definition "Construct
  /// Process": "the grants' raw inputs are of the types of its questions").
  final _rTypes = <String, _Node>{};
  var _rListEnd = false;

  /// The drawing procedure of each ask functor, run_f/5.
  final _runs = <String, String>{};

  _Generator(this.gen, this.resolve, this.declared, this.fresh,
      this.questionType, this.constructName);

  ConstructProcesses build(List<InteractiveType> types, {TypeDef? output}) {
    final hook = StringBuffer();
    final params = <String>{for (final t in types) ...t.params};
    final qt = params.isEmpty
        ? questionType
        : '$questionType(${params.join(', ')})';
    final p = params.isEmpty ? '' : '(${params.join(', ')})';
    // The program's output to its person, of the type Output where the
    // program defines it: drawn by the dispatcher as the thread of its
    // elements, construct 0, its widget thread(W), W Output's; where the
    // program defines no Output, its output end is anonymous and nothing is
    // drawn (vGLP 169b008, Definition "Canonical Compilation"; vGLP,
    // 2026-10-13 19:25 UTC, items 1, 3 and 4, Udi: yes).
    final outputWidget = output == null ? null : _outputWidget(output);
    final ch = output == null
        ? 'Channel(Stream(_), Stream(_))'
        : 'Channel(Stream(_), Stream($outputTypeName))';
    final d = gen.dispatch, cs = gen.constructs;
    // The drawing of the program's output, construct 0, by procedures typed
    // at the program's T, its value under the functor output (vGLP,
    // Definition "Construct Process": "the output thread's value is under
    // the functor output").
    final out = outputWidget == null ? null : _outputDrawing();
    hook
      // dispatch/3: the dispatcher on the ask stream and the person channel,
      // and constructs/1 on the spawns it writes.
      ..writeln('exported procedure$p $d(Stream(${gen.askType}($qt))?, '
          '${gen.personChannelType}?, $ch).')
      ..writeln(out == null
          ? '$d(Asks, PCh, MCh?) :- $d(Asks?, PCh?, MCh, Ss), $cs(Ss?).'
          : '$d(Asks, PCh, ch(MIn?, MOut)) :- '
              '$d(Asks?, PCh?, MIn, ODs?, Ss), '
              '$out($outputWidget, MOut?, ODs), $cs(Ss?).')
      ..writeln()
      // constructs/1: the construct process of each spawn.
      ..writeln('procedure$p $cs(Stream(${gen.spawnType}($qt, ${gen.drawn}))?).')
      ..writeln('$cs([spawn(Id, Q, Gs, Ds?) | Ss]) :- '
          '$constructName(Id?, Q?, Gs?, Ds), $cs(Ss?).')
      ..writeln('$cs([]).')
      ..writeln()
      ..writeln('procedure$p $constructName(Integer?, $qt?, '
          '${gen.inputsType}?, Stream(${gen.drawType})).');
    final widgets = <String, String>{};
    final drawnAlts = <String>[];
    for (final t in types) {
      final root = _Node(t.type, t.readerMode, t.params.toSet());
      _checkFlat(t, root);
      final w = _typeWidget(t, root);
      widgets[t.written] = w;
      hook.writeln(_constructClause(t, root, w));
      drawnAlts.add('${t.functor}(${_drawnOf(root)})');
    }
    if (outputWidget != null) {
      widgets[outputTypeName] = outputWidget;
      drawnAlts.add('output(List($outputTypeName))');
    }
    drawnAlts.addAll(_rUnion(drawnAlts));
    hook.writeln();
    // T, the type of what the person channel carries: the values the program
    // draws, each construct's under the functor of its ask and construct 0's
    // under output, and the grants' raw inputs, of the types of its
    // questions (vGLP, Definition "Construct Process"; vGLP 2026-10-14 11:36
    // UTC, Udi: yes).
    // The end button's grant of a list, [end], as a type: a list cell of the
    // constant end and [].
    final endDefs = drawnAlts.contains('[${gen.drawn}End | ${gen.drawn}Nil]')
        ? '${gen.drawn}End ::= end.\n${gen.drawn}Nil ::= [].\n'
        : '';
    final drawnDef =
        '${gen.drawn} ::= ${drawnAlts.join(' ; ')}.\n$endDefs\n';
    return ConstructProcesses('$drawnDef$hook$_out', widgets);
  }

  /// run_f(Id?, W?, Views?, Done?, Ds): draw each view V of construct Id
  /// with its widget W as it arrives, under the functor f of its ask,
  /// draw(Id, W, f(V)) (vGLP, Definition "Construct Process": "with V under
  /// the functor t of its ask"); withdraw when Done is bound, every question
  /// the construct holds answered and no question still to come, and on
  /// nothing else; once the views end, wait to withdraw (run_end/3).
  String _runFor(String f) {
    final known = _runs[f];
    if (known != null) return known;
    final r = gen.run(f);
    _runs[f] = r;
    _out
      ..writeln('procedure $r(Integer?, _?, Stream(_)?, ${gen.doneType}?, '
          'Stream(${gen.drawType})).')
      ..writeln('$r(Id, _, _, done, [withdraw(Id?)]).')
      ..writeln('$r(Id, W, [V | Vs], Done, [draw(Id?, W?, $f(V?)) | Ds?]) :- '
          'ground(W?), ground(V?) | $r(Id?, W?, Vs?, Done?, Ds).')
      ..writeln('$r(Id, _, [], Done, Ds?) :- ${gen.runEnd}(Id?, Done?, Ds).')
      ..writeln();
    return r;
  }

  /// output(W?, Out?, Draws): the program's output to its person, drawn as
  /// the construct 0 by its widget W, a thread, its value the elements so far
  /// under the functor output: drawn at once with no element, and again as
  /// each element arrives whole.  It holds no question and never withdraws
  /// while the output stream stands open (vGLP 169b008, Definition
  /// "Canonical Compilation"; Definition "Construct Process").
  String _outputDrawing() {
    final o = gen.output('output');
    final more = gen.output('output_more');
    final draw = gen.output('output_draw');
    final add = gen.output('output_add');
    const ms = 'Stream($outputTypeName)';
    const es = 'List($outputTypeName)';
    final ds = 'Stream(${gen.drawType})';
    _out
      ..writeln('procedure $o(_?, $ms?, $ds).')
      ..writeln('$o(W, Ms, [draw(0, W?, output([])) | Ds?]) :- '
          'ground(W?) | $more(W?, Ms?, [], Ds).')
      ..writeln()
      ..writeln('procedure $more(_?, $ms?, $es?, $ds).')
      ..writeln('$more(W, [M | Ms], Es, Ds?) :- ground(M?), ground(Es?) | '
          '$add(Es?, M?, Es1), $draw(W?, Ms?, Es1?, Ds).')
      ..writeln('$more(_, [], _, []).')
      ..writeln()
      ..writeln('procedure $draw(_?, $ms?, $es?, $ds).')
      ..writeln('$draw(W, Ms, Es, [draw(0, W?, output(Es?)) | Ds?]) :- '
          'ground(W?), ground(Es?) | $more(W?, Ms?, Es?, Ds).')
      ..writeln()
      ..writeln('procedure $add($es?, $outputTypeName?, $es).')
      ..writeln('$add([E | Es], M, [E? | Es1?]) :- $add(Es?, M?, Es1).')
      ..writeln('$add([], M, [M?]).')
      ..writeln();
    return o;
  }

  /// The terms each alternative of [n]'s type covers, for telling whether
  /// two alternatives of a union overlap: a primitive by its kind, a
  /// constant by its value, a structure by its functor and arity.
  Set<String> _covers(_Node n) {
    final out = <String>{};
    for (final a in _nodeAlts(n)) {
      if (a is _ConstAlt) {
        out.add('c:${constantSource(a.value)}');
      } else if (a is _StructAlt) {
        out.add('f:${a.functor}/${a.args.length}');
      } else if (a is _NilAlt) {
        out.add('[]');
      } else if (a is _ConsAlt) {
        out.add('.');
      } else if (a is _LeafAlt) {
        switch (a.leaf) {
          case _Leaf.integer:
            out.add('integer');
          case _Leaf.real:
            out.add('real');
          case _Leaf.number:
            out.addAll(['integer', 'real']);
          case _Leaf.string:
            out.add('string');
          case _Leaf.module:
            out.add('module');
          case _Leaf.constant:
            out.addAll(['integer', 'real', 'string', 'module']);
          default:
            out.add('any');
        }
      }
    }
    return out;
  }

  /// The grants' raw inputs as alternatives of T: the type of each question,
  /// or its element type where it is a stream or a list, and the end
  /// button's grant of a list.  A type whose terms another already covers is
  /// that other's (Date beside Integer); one covering others replaces them
  /// (Constant beside String), so that no two alternatives of T overlap.
  /// Two that overlap and neither covers the other are refused, naming them,
  /// as are an ask functor and a raw input of one functor.
  List<String> _rUnion(List<String> drawnAlts) {
    final kept = <(String, Set<String>)>[];
    for (final MapEntry(key: name, value: node) in _rTypes.entries) {
      final c = _covers(node);
      final all = {for (final k in kept) ...k.$2};
      if (all.containsAll(c)) continue;
      kept.removeWhere((k) => c.containsAll(k.$2));
      for (final k in kept) {
        if (k.$2.intersection(c).isNotEmpty) {
          throw CompileError(
              'The person\'s raw inputs to questions of types ${k.$1} and '
              '$name overlap, and neither type holds the other\'s, so the '
              'type of the person channel, their union, is not formed (vGLP, '
              'Definition "Construct Process")',
              0, 0, phase: 'analyzer');
        }
      }
      kept.add((name, c));
    }
    final asks = {
      for (final d in drawnAlts) d.substring(0, d.indexOf('(')),
    };
    for (final k in kept) {
      for (final f in k.$2) {
        if (f.startsWith('f:') && asks.contains(f.substring(2, f.indexOf('/')))) {
          throw CompileError(
              'The person\'s raw input to a question of type ${k.$1} has the '
              'functor ${f.substring(2)} of an ask, so the type of the person '
              'channel is not formed (vGLP, Definition "Construct Process")',
              0, 0, phase: 'analyzer');
        }
      }
    }
    final covered = {for (final k in kept) ...k.$2};
    return [
      for (final k in kept) k.$1,
      if (_rListEnd && !covered.contains('.'))
        '[${gen.drawn}End | ${gen.drawn}Nil]',
    ];
  }

  /// The drawn type of the interactive type whose root is [root]: the type
  /// of the views its construct draws, each under the functor of its ask.
  String _drawnOf(_Node root) => '_';

  /// The widget the program's output is drawn by: a thread whose elements
  /// are drawn by the widget of Output, `thread(W)`, or `thread` where W is
  /// `shown`.  Output is a value the program writes, refused where it is not
  /// flat, as an interactive type is (_checkFlat), and refused, naming it,
  /// where the person writes a position of it: the output is "outside a
  /// question" (vGLP, 2026-10-13 19:25 UTC, item 4).
  String _outputWidget(TypeDef output) {
    final t = TypeRef(outputTypeName, output.line, output.column);
    final it = InteractiveType(t, false, 'output', const [], output.line,
        output.column);
    final root = _Node(t, false, const {});
    _checkFlat(it, root);
    if (_hasQuestion(root)) {
      throw CompileError(
          'The type $outputTypeName, the type of the program\'s output to its '
          'person, has a position the person writes, and the output is outside '
          'a question: the dispatcher draws it as a thread whose elements are '
          'drawn by the widget of $outputTypeName (vGLP 169b008, Definition '
          '"Canonical Compilation"; vGLP, 2026-10-13 19:25 UTC, item 4)',
          output.line, output.column, phase: 'analyzer');
    }
    final w = _widget(root, {});
    return w == 'shown' ? 'thread' : 'thread($w)';
  }

  /// The widget of interactive type [t], whose root is [root]: the one in the
  /// program's scope.
  String _typeWidget(InteractiveType t, _Node root) => _widget(root, {});

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
            '${_runFor(f)}(Id?, $w, [input([]) | Vs?], Done?, Ds).';
      }
      return '$constructName(Id, $f(X?), Gs, Ds?) :- '
          '$answer([], X, Gs?, _, Done), '
          '${_runFor(f)}(Id?, $w, [input([])], Done?, Ds).';
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
        '${_runFor(f)}(Id?, $w, Vs?, Done?, Ds).';
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

  /// [from] is the Stream or the List, as written, whose definition [t] is
  /// an alternative of, where it is one's.
  void _flatten(
      TypeExpr t, Set<String> params, List<_Alt> out, Set<String> seen,
      [String? from]) {
    if (t is PrimitiveModeAlt) {
      out.add(_LeafAlt(_Leaf.any));
      return;
    }
    if (t is ConstantAlt) {
      out.add(_ConstAlt(t.value));
      return;
    }
    if (t is StructAlt) {
      // '.'(H, T) is the list cell [H | T] (GLP-Spec appendix-lp.tex,
      // Definition "Logic Programs Syntax": "[X|Xs] for a list cell").
      out.add(t.functor == '.' && t.args.length == 2
          ? _ConsAlt(t.args[0], t.args[1], from)
          : _StructAlt(t.functor, t.args));
      return;
    }
    if (t is ListNilAlt) {
      out.add(_NilAlt());
      return;
    }
    if (t is ListConsAlt) {
      out.add(_ConsAlt(t.head, t.tail, from));
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
      out.add(_LeafAlt(leaf, leaf == _Leaf.param ? t.name : null));
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
      _flatten(s, params, out, seen, _seqName(t.name) != null ? key : from);
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
  /// the same type; a named type defined as a Stream or a List, `Lines ::=
  /// Stream(String)`, is the stream it names ([_seqType]).
  TypeExpr? _streamElement(_Node n) {
    final s = _seqType(n.type);
    final m = s == null ? n : _Node(s, n.person, n.params);
    final alts = _nodeAlts(m);
    if (alts.length != 2) return null;
    final nil = alts.whereType<_NilAlt>();
    final cons = alts.whereType<_ConsAlt>();
    if (nil.length != 1 || cons.length != 1) return null;
    final c = cons.single;
    if (_isInput(c.tail)) return null;
    if (typeSource(_unmoded(c.tail)) != m.typeKey) return null;
    return c.head;
  }

  /// The Stream or the List [t] is, or names through named types each
  /// defined as one type: `Stream(String)` itself, and `Lines` where `Lines
  /// ::= Stream(String)`, "a named type so built", which "draws as the
  /// stream it names, like Date and Key draw by their names" (vGLP at
  /// 66b6500, Definition "Volitional Procedure, Interactive Type, ...";
  /// vGLP #7 Cowork, 2026-10-11 14:31 UTC, item 2); null for any other
  /// type.
  ///
  /// A scope's type definitions name an instantiated Stream or List by its
  /// instantiation, `Stream<Instrument>`, which is `Stream(Instrument)`
  /// ([_seqName]).
  TypeRef? _seqType(TypeExpr t, [Set<String>? seen]) {
    if (t is! TypeRef) return null;
    final u = _unmoded(t) as TypeRef;
    if (_seqName(u.name) != null) return u;
    if (u.name == 'MutualRef' || _leafOf(u.name, const {}) != null) {
      return null;
    }
    final s = seen ?? <String>{};
    if (!s.add(typeSource(u))) return null;
    final def = resolve(u.name);
    if (def == null || def.alternatives.length != 1) return null;
    final m = <String, TypeExpr>{
      for (var i = 0; i < def.typeParams.length && i < u.typeArgs.length; i++)
        def.typeParams[i]: u.typeArgs[i]
    };
    final a = m.isEmpty
        ? def.alternatives.single
        : _subst(def.alternatives.single, m);
    if (a is! TypeRef || a.isInput) return null;
    return _seqType(a, s);
  }

  /// A stream or a list, whoever writes it: [n] of a [_seqType], whose
  /// alternatives are [] and [E | S]; its element type and whether it is a
  /// list; null for any other node.
  ({TypeExpr elem, bool list})? _seqOf(_Node n) {
    final s = _seqType(n.type);
    if (s == null) return null;
    final e = _streamElement(n);
    if (e == null) return null;
    return (elem: e, list: _seqName(s.name) == 'List');
  }

  /// `Stream` or `List` where [name] is one of the two, as written or as a
  /// scope instantiates it, `Stream<Instrument>`; null for any other name.
  static String? _seqName(String name) {
    final lt = name.indexOf('<');
    final base = lt < 0 ? name : name.substring(0, lt);
    return base == 'Stream' || base == 'List' ? base : null;
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

  /// The construct forms every position the person writes: none is written by
  /// the program.
  void _checkPersonWritable(_Node n, InteractiveType t) {
    if (_hasProgramInside(n, {})) {
      throw CompileError(
          'The construct of the interactive type ${t.written} is not built: '
          'the person writes ${n.typeKey}, and a position of it is written by '
          'the program, which the construct can neither form nor show',
          t.line, t.column, phase: 'analyzer');
    }
  }

  /// Refuse the interactive type [it], whose root is [root], where it is not
  /// flat: "An interactive type is flat: it is built from constants, String,
  /// Integer, Real, Key, Constant, Module, named types so built, tuples and
  /// unions, and Stream or List as the whole type or as an argument of a
  /// tuple, never under another Stream or List; it has no other recursive
  /// type, and no type parameter at a position the person writes" (vGLP at
  /// 66b6500, Definition "Volitional Procedure, Interactive Type, ...").
  /// Each is refused over the whole interactive type, a structure the
  /// program writes included, one holding no question too (vGLP #7 Cowork,
  /// 2026-10-11 14:31 UTC, items 1 and 2; vGLP #8 Cowork, 2026-10-11 15:47
  /// UTC, item 1: "The flat refusals hold over the whole interactive type, a
  /// program-written structure holding no question included"):
  ///
  ///   - a stream or a list in an element of a stream or a list, whoever
  ///     writes it, the element itself one or holding one;
  ///   - a list cell outside a Stream or a List: a list cell type, a tuple
  ///     '.'(H, T), or a Stream or a List as an alternative of a union, none
  ///     the whole type nor an argument of a tuple;
  ///   - a recursive type other than a Stream or a List;
  ///   - a position the person writes of a type parameter, refused outright
  ///     (14:31 UTC, item 1);
  ///   - a position of the type `_`, whoever writes it, which is none of the
  ///     types an interactive type is built from (15:47 UTC, item 3: "The
  ///     type `_` where the person writes: refused, with the flat refusals;
  ///     it is not among the Definition's building blocks"; vGLP #8 Cowork,
  ///     2026-10-11 17:24 UTC, Q2: "the type _ refused everywhere in an
  ///     interactive type, the building blocks excluding it").
  ///
  /// A stream the program writes whose elements hold questions is flat, and
  /// is presented element by element ([_presentSeq]).
  void _checkFlat(InteractiveType it, _Node root) {
    // The clauses of the Definition's sentence each refusal quotes.
    const builtFrom = 'built from constants, String, Integer, Real, Key, '
        'Constant, Module, named types so built, tuples and unions, and '
        'Stream or List';
    const wholeOrTuple =
        'Stream or List as the whole type or as an argument of a tuple';
    const notUnder = 'never under another Stream or List';
    const noRecursive = 'it has no other recursive type';
    Never refuse(String what, String clause) => throw CompileError(
        'The interactive type ${it.written} is not flat: $what, and an '
        'interactive type is flat, "$clause" ($_flatDefinition)',
        it.line, it.column, phase: 'analyzer');
    String who(_Node n) => n.person ? 'the person' : 'the program';
    String holds(_Node n) =>
        n.person || !_hasQuestion(n) ? '' : ', holding a question';

    // [path] the types of the positions above [n], a Stream's or a List's
    // left out, whose recursion is its tail's; [inSeq] the stream or the
    // list that [n] is in an element of, with that element.
    void walk(_Node n, List<String> path, (_Node, _Node)? inSeq) {
      final seq = _seqOf(n);
      if (seq != null) {
        if (inSeq != null) {
          final (outer, elem) = inSeq;
          refuse(
              '${who(outer)} writes ${outer.typeKey}, whose element '
              '${elem.typeKey} ${identical(elem, n) ? 'is' : 'holds'} a '
              'stream or a list${holds(n)}',
              notUnder);
        }
        final e = n.child(seq.elem);
        walk(e, path, (n, e));
        return;
      }
      if (path.contains(n.typeKey)) {
        refuse(
            '${who(n)} writes ${n.typeKey}, a recursive type that is neither '
            'a Stream nor a List${holds(n)}',
            noRecursive);
      }
      final below = [...path, n.typeKey];
      for (final a in _nodeAlts(n)) {
        if (a is _LeafAlt && a.leaf == _Leaf.param && n.person) {
          throw CompileError(
              'The interactive type ${it.written} is refused: the person '
              'writes a position of type ${a.name}, a type parameter, and an '
              'interactive type has "no type parameter at a position the '
              'person writes" ($_flatDefinition)',
              it.line, it.column, phase: 'analyzer');
        }
        if (a is _LeafAlt && a.leaf == _Leaf.any) {
          refuse(
              '${who(n)} writes a position of type _, none of the types '
              'an interactive type is built from',
              builtFrom);
        }
        if (a is _ConsAlt) {
          final kids = _children(n, a);
          if (kids.any((k) => below.contains(k.typeKey))) {
            refuse(
                '${who(n)} writes ${n.typeKey}, a recursive type that is '
                'neither a Stream nor a List${holds(n)}',
                noRecursive);
          }
          if (a.from != null) {
            refuse(
                '${who(n)} writes ${n.typeKey}, a union one of whose '
                'alternatives is ${a.from}${holds(n)}, neither the whole type '
                'nor an argument of a tuple',
                wholeOrTuple);
          }
          refuse(
              '${who(n)} writes ${n.typeKey}, a list cell of which is neither '
              'a Stream nor a List${holds(n)}',
              builtFrom);
        }
        for (final k in _children(n, a)) {
          walk(k, below, inSeq);
        }
      }
    }

    walk(root, const [], null);
  }

  /// A stream or a list the person writes: [n] written by the person, of a
  /// type whose alternatives are [] and [E | S], S the same type, named
  /// `Stream` or `List` or defined as one ([_seqType]) --- the two told apart
  /// by the type's name, the Answer rule allowing [] "for a list" and not for
  /// a stream (Definition "vmaGLP Transition System", Answer; vGLP #6
  /// Cowork, 2026-10-11 12:55 UTC, item 2, Q1).  Its element type and whether
  /// it is a list; null for any other node.
  ({TypeExpr elem, bool list})? _personSeq(_Node n) =>
      n.person ? _seqOf(n) : null;

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
  /// writes, drawn pending until it is written whole and drawn again once it
  /// is, a stream again as each cell and each element is written (vGLP
  /// 118e04d, Definition "Construct Process"); or one the person writes whose
  /// answer leaves a stream or list open under it.
  bool _multi(_Node n) => n.person ? _holdsNested(n) : true;

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
  /// A list the person writes is ended by the person: the grant of its end
  /// button, [listEnd], for its position binds the rest of the list to [],
  /// and the question is answered; a stream is not, its question never
  /// answered (Definition "vmaGLP Transition System", Answer: "or [] for a
  /// list"; vGLP #6 Cowork, 2026-10-11 12:55 UTC, item 2, Q1).  The end's
  /// grant is no element's, so no input both ends the list and forms an
  /// element (vGLP #7 Cowork, 2026-10-11 14:31 UTC, item 3).
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
    // An element holding a stream or a list is refused, the type not flat
    // (_checkFlat).
    final seq = _personSeq(n);
    final e = seq == null ? null : n.child(seq.elem);
    final nested = _holdsNested(n);
    final vs = nested ? ', Vs' : '';
    final vsOut = nested ? ', Vs?' : '';
    final vsType = nested ? ', Stream(_)' : '';
    final form = _former(e ?? n);
    _rTypes.putIfAbsent((e ?? n).typeKey, () => e ?? n);
    if (seq != null && seq.list) _rListEnd = true;
    final open = nested ? _open(n) : null;
    _out.writeln('procedure$p $name($pt?, $t, $gs?, $gs$vsType, $done).');
    if (seq != null && seq.list) {
      _out.writeln('$name(P, [], [input(_, P1, $listEnd) | Gs], Gs?, done) '
          ':- P1? =?= P? | true.');
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
    if (leaf != null) {
      // The type _ and a type parameter where the person writes are refused
      // before any position is formed (_checkFlat), so no construct calls a
      // former of either, and form_any is not emitted (vGLP #8 Cowork,
      // 2026-10-11 17:24 UTC, NOTED).
      final f = gen._leafFormers[leaf];
      if (f == null) {
        throw StateError('A position the person writes of type '
            '${n.typeKey}, which _checkFlat refuses, is formed');
      }
      return f;
    }
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
  ///
  /// X not yet written when it is drawn --- no alternative's clause can
  /// reduce, the last clause can --- it is drawn pending(P), and present_N_wait
  /// passes the grants on, none being for a question of it, until X is
  /// written, and then presents it as present_N does (vGLP 118e04d,
  /// Definition "Construct Process": "each widget position the program has
  /// not yet written whole --- the root, a field, ... --- drawn as pending
  /// ... so that the questions beside it are offered at once"; "on a grant
  /// for a path at which it holds no question, it binds nothing").  An
  /// alternative holding no question is one widget position, drawn pending
  /// until written whole ([_presentStruct]).
  String _present(_Node n, InteractiveType it) {
    final name = _name('present', n);
    if (!_emitted.add('present:${n.typeKey}')) return name;
    final seq = _seqOf(n);
    if (seq != null) {
      _presentSeq(n, seq.elem, name, it);
      return name;
    }
    if (_streamElement(n) != null) {
      // A recursive type of [] and [E | S] that is not a Stream or a List
      // is refused before it is presented, the type not flat (_checkFlat).
      throw StateError('present_N meets ${n.typeKey}, which _checkFlat '
          'refuses');
    }
    final t = n.typeKey;
    final p = _declParams(n);
    final done = gen.doneType;
    final gs = gen.inputsType;
    final wait = _name('present', n, 'wait');
    final clauses = <String>[];
    // present_N_wait's clauses: X once written, each alternative's as
    // present_N takes it.
    final forwards = <String>[];
    final later = <void Function()>[];
    String forward(String head, String term, [String guard = '']) =>
        '$wait(P, $head, Gs, Gs1?, Vs?, Done?) :- $guard'
        '$name(P?, $term, Gs?, Gs1, Vs, Done).';
    for (final a in _nodeAlts(n)) {
      if (a is _ConstAlt) {
        final c = constantSource(a.value);
        clauses.add('$name(_, $c, Gs, Gs?, [$c], done).');
        forwards.add(forward(c, c));
      } else if (a is _NilAlt) {
        clauses.add('$name(_, [], Gs, Gs?, [[]], done).');
        forwards.add(forward('[]', '[]'));
      } else if (a is _LeafAlt) {
        final g = _guard(a.leaf);
        clauses.add('$name(_, X, Gs, Gs?, [X?], done) :- $g(X?) | true.');
        forwards.add(forward('X', 'X?', '$g(X?) | '));
      } else {
        clauses.add(_presentStruct(n, a, name, later, it));
        // The alternative's term as present_N's clause reads it, each
        // position the person writes a writer the construct holds.
        final kids = _children(n, a);
        final hs = [
          for (var i = 0; i < kids.length; i++)
            kids[i].person ? 'H${i + 1}?' : 'H${i + 1}'
        ];
        final ts = [
          for (var i = 0; i < kids.length; i++)
            kids[i].person ? 'H${i + 1}' : 'H${i + 1}?'
        ];
        forwards.add(forward(_apply(a, hs), _apply(a, ts)));
      }
    }
    clauses.add('$name(P, X, Gs, Gs1?, [pending(P?) | Vs?], Done?) :- '
        'ground(P?) | $wait(P?, X?, Gs?, Gs1, Vs, Done).');
    forwards.add('$wait(P, X, [G | Gs], [G? | Gs1?], Vs?, Done?) :- '
        '$wait(P?, X?, Gs?, Gs1, Vs, Done).');
    _out.writeln('procedure$p $name(${gen.pathType}?, $t?, '
        '$gs?, $gs, Stream(_), $done).');
    for (final c in clauses) {
      _out.writeln(c);
    }
    _out
      ..writeln()
      ..writeln('procedure$p $wait(${gen.pathType}?, $t?, $gs?, $gs, '
          'Stream(_), $done).');
    for (final c in forwards) {
      _out.writeln(c);
    }
    _out.writeln();
    for (final f in later) {
      f();
    }
    return name;
  }

  /// present_N(P, Xs?, Gs?, Gs1, Vs, Done) of node [n], a stream or a list
  /// the program writes whose elements, of type [elem], hold questions:
  /// "Present hands the person each element with the writers inside it, each
  /// such writer a question, drawn by the Definition (the stream as a list
  /// or a thread, each question by its type's widget); each element drawn as
  /// it is presented, the answers to its questions taken as any grant" (vGLP
  /// #8 Cowork, 2026-10-11 15:47 UTC, item 2, Task D; vGLP at 66b6500,
  /// Definition "vmaGLP Transition System", Present; Definition "Question":
  /// "a writer presented inside the program's output"; Definition "Construct
  /// Process": "as the program's output arrives on a reader it holds, each
  /// writer inside it at a position the person writes is added to its
  /// questions ... It has the bridge draw the widget of T with the output so
  /// far, again as more arrives").
  ///
  /// The element of a list cell at position P is at P with 1 at its end, and
  /// the cell's tail at P with 2 at its end, the path of argument indices
  /// from the root of the interactive variable's term ('.'(H, T), GLP-Spec
  /// appendix-lp.tex, Definition "Logic Programs Syntax"): so the k-th
  /// element is at P with k - 1 2s and then 1 at its end, and its questions
  /// below it.  Each
  /// element is presented as it arrives, at its own position --- by its
  /// present_N where the program writes it, by its answer_N where the
  /// element is itself a question --- and its questions take their grants
  /// as any question does, the grants an element does not take passed on to
  /// those after it.  The views are the elements' views so far, in the
  /// stream's order, again as each cell and each element is drawn again
  /// (cons_views/3 in programs/vglp/dispatcher.glp), as thread/3 shows a
  /// stream the program writes that holds no question: an element the
  /// program has not yet written whole drawn pending by its present_N, and
  /// the stream's unwritten tail not drawn, the stream shown as its elements
  /// so far, [] where it has none; the unwritten tail passes the grants on
  /// until it is written (vGLP 118e04d, Definition "Construct Process":
  /// "each widget position the program has not yet written whole --- ... an
  /// element of any stream ... --- drawn as pending, and a stream's
  /// unwritten tail not drawn, so that the questions beside it are offered
  /// at once").  Whether the tail is written when it is drawn is decided by
  /// the first clause that can reduce (GLP-Spec, Definition "cGLP Transition
  /// System", Reduce).  Done once the stream is closed and every element's
  /// questions are answered; a stream never closed keeps its construct, "no
  /// reader at a position whose output may hold one" never holding.
  void _presentSeq(_Node n, TypeExpr elem, String name, InteractiveType it) {
    final e = n.child(elem);
    final tail = _name('present', n, 'tail');
    final cell = _name('present', n, 'cell');
    final t = n.typeKey;
    final p = _declParams(n);
    final done = gen.doneType;
    final pt = gen.pathType;
    final gs = gen.inputsType;
    final path = gen.path;
    // The element at P with 1 at its end: its pattern in the list cell, the
    // argument that hands it to the cell's procedure, that argument's
    // pattern and type there, its view's stream and its goals.
    final String inCell, toCell, atCell, cellType, v1;
    final List<String> elemGoals;
    if (e.person) {
      // The element is itself a question: the program hands the person its
      // writer, and the view marks it.
      _checkPersonWritable(e, it);
      inCell = 'X?';
      toCell = 'X';
      atCell = 'X?';
      cellType = e.typeKey;
      v1 = '[input(M1?)]';
      elemGoals = [
        '$path(P?, 1, M1)',
        '$path(P?, 1, Q1)',
        '${_answer(e)}(Q1?, X, Gs?, Gs1, D1)',
      ];
    } else {
      inCell = 'X';
      toCell = 'X?';
      atCell = 'X';
      cellType = '${e.typeKey}?';
      v1 = 'V1?';
      elemGoals = [
        '$path(P?, 1, Q1)',
        '${_present(e, it)}(Q1?, X?, Gs?, Gs1, V1, D1)',
      ];
    }
    // The views are lists of the elements' views, Stream(Stream(_)), as
    // thread/3's are.
    const vs = 'Stream(Stream(_))';
    _out
      ..writeln('procedure$p $name($pt?, $t?, $gs?, $gs, $vs, $done).')
      ..writeln('$name(P, [$inCell | Xs], Gs, Gs1?, Vs?, Done?) :- '
          '$cell(P?, $toCell, Xs?, Gs?, Gs1, Vs, Done).')
      ..writeln('$name(_, [], Gs, Gs?, [[]], done).')
      ..writeln('$name(P, Xs, Gs, Gs1?, [[] | Vs?], Done?) :- '
          '$tail(P?, Xs?, Gs?, Gs1, Vs, Done).')
      ..writeln()
      // The stream drawn with no element yet, as its tail is written; until
      // it is, the grants pass on.
      ..writeln('procedure$p $tail($pt?, $t?, $gs?, $gs, $vs, $done).')
      ..writeln('$tail(P, [$inCell | Xs], Gs, Gs1?, Vs?, Done?) :- '
          '$cell(P?, $toCell, Xs?, Gs?, Gs1, Vs, Done).')
      ..writeln('$tail(_, [], Gs, Gs?, [], done).')
      ..writeln('$tail(P, Xs, [G | Gs], [G? | Gs1?], Vs?, Done?) :- '
          '$tail(P?, Xs?, Gs?, Gs1, Vs, Done).')
      ..writeln()
      // The list cell [X | Xs] at P: its element at P with 1 at its end, its
      // tail at P with 2 at its end.
      ..writeln('procedure$p $cell($pt?, $cellType, $t?, $gs?, $gs, $vs, '
          '$done).')
      ..writeln('$cell(P, $atCell, Xs, Gs, Gs2?, Vs?, Done?) :- ground(P?) | '
          '${elemGoals.join(', ')}, $path(P?, 2, Q2), '
          '$name(Q2?, Xs?, Gs1?, Gs2, V2, D2), '
          '${gen.consViews}($v1, V2?, Vs), '
          '${gen.allDone}([D1?, D2?], Done).')
      ..writeln();
  }

  /// present_N's clause of the structure alternative [a] of node [n].  An
  /// alternative holding no question is one widget position, its widget
  /// shown ([_altWidget]): drawn pending until it is written whole, by
  /// shown/3.
  String _presentStruct(_Node n, _Alt a, String name,
      List<void Function()> later, InteractiveType it) {
    final kids = _children(n, a);
    if (!kids.any(_hasQuestion)) {
      final hs = [for (var i = 0; i < kids.length; i++) 'H${i + 1}'];
      return '$name(P, ${_apply(a, hs)}, Gs, Gs?, Vs?, done) :- '
          '${gen.shown}(P?, ${_apply(a, [for (final h in hs) '$h?'])}, Vs).';
    }
    final head = <String>[];
    final goals = <String>[];
    final guards = <String>[];
    final views = <String>[];
    final dones = <String>[];
    var gsIn = 'Gs';
    var g = 0;
    // The position of the argument i of this alternative is the node's own,
    // P, with i at its end: a question's is read twice, for its mark in the
    // view and for its answer_N, and a position the program writes once, for
    // its present_N, its shown/3 or its thread/3, each drawing it pending
    // until it is written.  P is ground, so where it is read more than once
    // the clause guards on it.
    var pathReads = 0;
    // A structure of questions alone, none holding a stream or a list, has
    // one view; any other is drawn again as its arguments are.
    final direct = kids.every((k) => k.person && !_holdsNested(k));
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
        // A field the program writes holding no question: shown, or a
        // thread where it is a stream, pending until written whole, a
        // stream's unwritten tail not drawn.
        head.add(h);
        final q = 'Q$idx';
        final shows = _streamElement(k) != null ? gen.thread : gen.shown;
        goals
          ..add('${gen.path}(P?, $idx, $q)')
          ..add('$shows($q?, $h?, V$idx)');
        pathReads += 1;
        views.add('V$idx?');
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
    // An alternative holding a question reads its position, for the
    // question's mark or for the present_N, shown/3 or thread/3 of the
    // argument holding it.
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
      // A recursive type other than a Stream or a List that holds a question
      // is refused before its widget is drawn (_checkFlat), and one holding
      // none is shown or a thread, not walked: no widget is ref(T).
      throw StateError('The widget of ${n.moded} meets ${n.typeKey} again, '
          'which _checkFlat refuses');
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
        // A value the program writes, "shown and not edited", drawn by the
        // widget its type's constructors build, as one the person writes is:
        // a tuple a form, "one field per argument, each labelled by the name
        // of its declared type", a tuple inside it a form of its own, by the
        // induction of Proposition "Default Widgets"; a union a menu, "each
        // drawn by its widget"; a stream a thread, thread(W) where its
        // elements are tuples, W their form; a constant or a primitive
        // shown (Definition "Widget Declaration, Default Widget"; vGLP #8
        // Cowork, 2026-10-13 08:15 UTC).  So the declared types of a written
        // value reach the bridge, each field's label its type's name.
        final seq = _seqOf(n);
        if (seq != null) {
          final w = _widget(n.child(seq.elem), visiting);
          return w == 'shown' ? 'thread' : 'thread($w)';
        }
        if (_streamElement(n) != null) return 'thread';
        final alts = _nodeAlts(n);
        final ws = [for (final a in alts) _altWidget(n, a, visiting)];
        if (ws.every((w) => w == 'shown')) return 'shown';
        if (alts.length == 1) return ws.single;
        return 'menu([${ws.join(', ')}])';
      }
      // A stream or a list the program writes whose elements hold
      // questions: a thread of its elements, each drawn by the widget of
      // the element type, W, and each question in it by the field of its
      // position (Definition "Widget Declaration, Default Widget": "a stream
      // the program writes: a list or a thread"; vGLP #8 Cowork, 2026-10-11
      // 15:47 UTC, item 2).
      final seq = _seqOf(n);
      if (seq != null) return 'thread(${_widget(n.child(seq.elem), visiting)})';
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
    if (a is _ConsAlt) {
      // A list cell holding a question outside a Stream or a List is
      // refused, the type not flat (_checkFlat): no widget is form('.',
      // ...).
      throw StateError('The widget of ${n.moded} meets a list cell, which '
          '_checkFlat refuses');
    }
    final kids = _children(n, a);
    final ws = [
      for (final k in kids)
        '${_isNestedSeq(n, k) ? 'question' : 'field'}'
            '(${constantSource(k.typeKey)}, ${_widget(k, visiting)})'
    ].join(', ');
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
        return 'text';
      case _Leaf.any:
        // The type _ where the person writes is none of the types an
        // interactive type is built from: refused before its widget is
        // drawn (_checkFlat; vGLP #8 Cowork, 2026-10-11 15:47 UTC, item 3).
        throw StateError('The type _ where the person writes, which '
            '_checkFlat refuses');
      case _Leaf.param:
        // "no type parameter at a position the person writes" (vGLP at
        // 66b6500, Definition "Volitional Procedure, Interactive Type,
        // ..."): refused before its widget is drawn (_checkFlat).
        throw StateError('A type parameter where the person writes, which '
            '_checkFlat refuses');
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
