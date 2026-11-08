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
//   - the output the program writes into the variable is drawn once written
//     whole, with its questions: "A construct process has the bridge draw
//     the widget of T once every reader in the output it holds is assigned,
//     with that output and its questions" (vGLP, Section 6.3, "The construct
//     process"; Definition "Reduce and Communicate in vmaGLP", Communicate
//     delivering an assignment once the term it assigns has no reader); a
//     stream or a list the program writes is written whole once closed; no
//     position of it is drawn pending, "pending(Pos) at each widget position
//     not yet written whole" being drawn only "as output arrives inside the
//     person's answers" (ibid.), a position the construct of a type with one
//     is refused (WHAT IS NOT BUILT, below).  A question is answered only
//     once drawn ("On a grant input(Id, Pos, R) for a question W it holds
//     and has drawn", ibid.): the grants reach the questions after the
//     first draw (run_f/7);
//   - a position the person writes is a QUESTION, shown as input(P), P its
//     position: the path of argument indices from the root of the
//     interactive variable's term to it, [] the root, a stream the person
//     writes having the stream's own path, each submission one element
//     (vGLP #5 Cowork, 2026-10-04 09:05 UTC, C); a grant forms a term of its
//     type and binds it, a stream type taking one element per grant, an
//     input box;
//   - the views are drawn as they change, draw(Id, W, f(V)), W the widget of
//     T --- its default D, or named(W', D) where a declaration names it W'
//     (vGLP 92e6d3b, the construct process in the appendix of proofs) ---
//     and f the functor of its ask, so that every drawn value is of the
//     program's T, the type of its person channel: the union of the values
//     drawn, each under its ask's functor, construct 0's under output, and of
//     the grants' raw inputs, the types of its questions (vGLP, Definition
//     "Construct Process"; vGLP 2026-10-14 11:36 UTC, Udi: yes).  The drawn
//     value of a construct whose root the person writes, holding no stream
//     or list inside it, is input([]), of its view type, T ; input(Path)
//     ("V is of the view type of T: T with the type S of each position the
//     person writes replaced by S ; input(Path)", vGLP e22e2ad, Section 6.3,
//     "The construct process"); every other drawn value is of the view type
//     the compilation forms for its node, a position the person writes
//     Questioned(S), S ; input(Path), one the program writes holding no
//     question its type S, a stream or a list of positions holding one the
//     list of its elements' views, and a structure holding a question the
//     union of its alternatives' views (_presentView, _openView;
//     programs/vglp/views.glp);
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
// input, which is of T, the type of the person channel, where the type
// formed is a question's, and of the type formed where it is a position of
// one (_resolveFormed); a primitive's former, in views.glp, is typed over
// the input's type: a constant by matching it, a primitive by its
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
// button grants [end], a list cell, which no element of an interactive type
// can be
// (listEnd; 14:31 UTC, item 3).  A stream or a list inside a form the
// person writes is a question of its own: the form is formed with [] there,
// its answer carries a fresh variable there (open_N), answered at its own
// position from the grants that follow, and the construct is drawn again
// with the answer; its entry in the form's widget is question(L, W) and
// every other field(L, W), L the name of the field's declared type.  A
// position of type Key, or of a type defined as Key, is a peer field.
//
// THE INTERACTIVE TYPES (vGLP 092c9fe, Definition "Volitional Procedure,
// Interactive Type": "An interactive type is built from constants, String,
// Integer, Real, Key, Constant, Module, named types so built, tuples and
// unions, and Stream or List as the whole type or as an argument of a tuple,
// never under another Stream or List, and a Stream only at a position the
// person writes; a named type in it is recursive only if it has no mode
// inversion, and it has no type parameter" (vGLP 2f1dc92, which dropped "at
// a position the person writes")).  Refused, each naming the type, over the whole interactive type,
// a structure the program writes included, one holding no question too
// (vGLP #7 Cowork, 2026-10-11 14:31 UTC, items 1 and 2; vGLP #8 Cowork,
// 2026-10-11 15:47 UTC, items 1 and 3; _checkInteractive): a stream or a
// list in an element of a stream or a list, reached through a recursive
// type too; a list cell outside a Stream or a List, a tuple '.'(H, T) being
// one; a recursive named type with a mode inversion in it, a position marked
// ? (TGLP, well-typing.tex, Definition "Simple Prefix": "A mode inversion
// point: any position whose type is marked with ?"), in its own definition
// or in one it reaches; a position the person writes of a type parameter,
// which has no widget; and a position of the type _, whoever writes it,
// none of the Definition's building blocks (vGLP #8 Cowork, 2026-10-11
// 17:24 UTC, Q2).  So the compilation emits no form('.', ...).  And a
// Stream the program writes: "a Stream only at a position the person writes"
// (vGLP 27bae5d; vGLP #24, Code_inbox.md, 2026-11-07 22:27 UTC, 19:39 item
// 3), a List the program writes admitted.
//
// RECURSIVE TYPES (vGLP 092c9fe, Definition "Volitional Procedure,
// Interactive Type", "a named type in it is recursive only if it has no
// mode inversion", and the paragraph after it: "A speech act forwarded with
// its provenance is signed again at each forward, so its type is recursive,
// and a request to forward it carries it.  With no mode inversion, a
// recursive type is written whole by the program or whole by the person";
// vGLP #14 Cowork, 2026-10-18 11:50 UTC, Udi approved).  A recursive named
// type, mutual recursion included --- SpeechAct ::= speech(Utterance, Key),
// Utterance ::= text(String) ; SpeechAct --- is admitted where no position
// reachable from it is marked ? ([_Generator._recursive],
// [_Generator._inversionIn]); Stream and List are not these named types,
// the Definition admitting them by their own clause, a Stream(YesNo?) the
// program writes among them (_presentSeq).  The person writes a recursive
// type whole: its forming clauses form the answer from the raw input by
// recursion on the type's definition, form_N calling form_N again where the
// definition names the type again (_former).  The program writes it whole:
// it holds no question, and is drawn once written whole.  Its
// widget is finite by a reference back to itself: rec(T, W) binds the name
// T of a recursive type to its widget W, within which ref(T) is T again,
// and a field of a form drawn by either is "a field of a recursive type
// drawn when the person opens it" (Definition "Default Widget"), so every
// finite answer is formed by finitely many acts (_widget; the bridge,
// glp_multiagent/lib/ui_runtime/bridge.dart, recCtor and refCtor).
//
// A STREAM OR A LIST THE PROGRAM WRITES WHOSE ELEMENTS HOLD QUESTIONS (vGLP
// #8 Cowork, 2026-10-11 15:47 UTC, item 2, Task D): each element presented
// at its own position, its questions answered as any grant; its widget
// thread(W), W the element type's, its views the elements' views, drawn
// once the list is written whole, closed and each element written whole
// (_presentSeq).
//
// A POSITION THE PROGRAM WRITES INSIDE ONE THE PERSON WRITES (vGLP, Section
// 6.3, "The construct process"; Definition "Answer"; the chat of Section 3,
// Line ::= line(String, Status?)): the answer holds a fresh writer there, the
// construct holds its reader and draws the position pending(Pos) until it is
// written whole, and as written after (fill_N, pend_N; the section
// "positions the program writes inside the person's answers" below); the
// form shows it and does not edit it, its field drawn by the widget of a
// value the program writes, shown, and the bridge granting hole there, the
// raw input of such a question of its skeleton type (skel_N).
//
// WHAT IS NOT BUILT, each a compile error naming the type
// (_checkPersonWritable): a position the program writes inside the person's
// answer holding a question; an answer holding both such a position and a
// stream or a list of its own; such a position in a stream or a list inside
// a form, or in an answer inside the program's output; and a difference
// list.
// A position the person writes of type Real is built, formed by real/1 and
// drawn as a number field (vGLP #5 Cowork, 2026-10-03 08:16 UTC, item 6;
// Definition "Widget Declaration, Default Widget" at c2e8b57: "a number
// (Integer or Real)").

import '../compiler/error.dart';
import '../analysis/type_checker/type_ast.dart';
import '../compiler/glp_printer.dart' show constantSource, functorNameSource;
import 'mediator.dart' show typeSource;
import 'type_walk.dart';
import 'dispatcher.dart'
    show
        drawTypeName,
        inputTypeName,
        pathTypeName,
        personChannelTypeName,
        widgetTypeName;
import 'canonical.dart' show DeclaredWidget;

/// The Definition an interactive type is refused by where it is not one.
const _definition = 'vGLP, Definition "Volitional Procedure, Interactive Type"';

/// The term the end button of a list the person writes grants, R in
/// input(Id, P, R): "a list the person writes: the same, with a button that
/// ends the list" (vGLP at 66b6500, Definition "Widget Declaration, Default
/// Widget"), its grant "a term of its own that no element can be" (vGLP #7
/// Cowork, 2026-10-11 14:31 UTC, item 3).  It is a list cell, and no element
/// of a list the person writes is one: a list cell occurs in an interactive
/// type only as a Stream's or a List's, and a Stream or a List never under
/// another ([_Generator._checkInteractive]); `[]`, a constant
/// (GLP-Spec appendix-lp.tex: "a constant (numbers, strings, or the empty
/// list [])"), may be an element.  The bridge's end button grants the same
/// (glp_multiagent/lib/ui_runtime/bridge.dart, listEnd).
const listEnd = '[end]';

/// A moded interactive type of the program: its type as written, its mode,
/// the functor the compilation gives it in Question, and the type parameters
/// of the procedure that declares it.  They are the linked program's, every
/// module's: "the questions of a program are those of all its modules" (vGLP
/// at a89a751, sections/parts/canonical-compilation.tex).
class InteractiveType {
  final TypeExpr type;
  final bool readerMode;
  final String functor;
  final List<String> params;
  final int line, column;

  /// The widget declarations in scope where the volitional procedure asking
  /// it is declared, by moded type (vGLP ed75782, Definition "Default
  /// Widget": "the one named by the widget declaration in scope where the
  /// volitional procedure asking the question is declared"); null where they
  /// are those of the source compiled.
  final Map<String, String>? widgets;

  InteractiveType(this.type, this.readerMode, this.functor, this.params,
      this.line, this.column, {this.widgets});

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

  /// The view type of the construct of the ask functor f, a type name fresh
  /// against the program's (vGLP, Section 6.3, "The construct process").
  final String Function(String functor) view;

  /// A procedure name fresh against the program's, for the drawing of the
  /// program's output.
  final String Function(String stem) output;
  final String consViews;
  final String allDone;
  final String path;
  final Map<_Leaf, String> _leafFormers;
  final String formedType;
  final String doneType;

  /// The view of a position the person writes: Questioned(S), S ;
  /// input(Path) (programs/vglp/views.glp; vGLP, Section 6.3, "The construct
  /// process": "V is of the view type of T").
  final String questionedType;
  /// The type of what the person channel carries, T, the union the
  /// compilation forms; and the types at T the construct processes name:
  /// Draw(T), Input(T), Inputs(T), Path and PersonChannel(T) as applied.
  final String drawn;
  final String inputsType;
  final String spawnType;
  final String askType;

  String get drawType => '$drawTypeName($drawn)';
  String get inputType => '$inputTypeName($drawn)';
  String get pathType => pathTypeName;
  String get personChannelType => '$personChannelTypeName($drawn)';

  GenericNames(
      {required this.dispatch,
      required this.constructs,
      required this.run,
      required this.runEnd,
      required this.view,
      required this.output,
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
      required this.questionedType,
      required this.drawn,
      required this.inputsType,
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
TypeExpr _subst(TypeExpr t, Map<String, TypeExpr> m) => mapTypeRefs(t, (ref) {
      final r = m[ref.name];
      if (r != null && ref.typeArgs.isEmpty) {
        if (!ref.isInput) return r;
        if (r is TypeRef) {
          return TypeRef(r.name, r.line, r.column,
              isInput: !r.isInput, typeArgs: r.typeArgs);
        }
        if (r is PrimitiveModeAlt) {
          return PrimitiveModeAlt(!r.isInput, r.line, r.column);
        }
        return r;
      }
      return TypeRef(ref.name, ref.line, ref.column,
          isInput: ref.isInput,
          typeArgs: [for (final a in ref.typeArgs) _subst(a, m)]);
    });

// ---------------------------------------------------------------------------
// The generator
// ---------------------------------------------------------------------------

/// What a clause over the arguments of an alternative accumulates, argument
/// by argument (open_N, [_Generator._open]; present_N,
/// [_Generator._presentStruct]): the [goals] and [guards] of its body, the
/// Done of each argument holding a question ([dones]), the grants chained
/// through those arguments ([gsIn] the stream the next one takes, [nextGs]
/// the one it passes on) and how many times the clause reads its position P
/// ([pathReads]).
class _Arguments {
  final goals = <String>[];
  final guards = <String>[];
  final dones = <String>[];
  var gsIn = 'Gs';
  var pathReads = 0;
  var _g = 0;

  /// The grant stream an argument passes on, the next one's [gsIn].
  String nextGs() => 'Gs${++_g}';

  /// The head's Done: that of the one argument holding a question, or Done,
  /// with [allDone] over all of them added to the goals.
  String doneHead(String allDone) {
    if (dones.length == 1) return '${dones.single}?';
    goals.add('$allDone([${dones.map((d) => '$d?').join(', ')}], Done)');
    return 'Done?';
  }

  /// The clause's guard, `ground(P?)` first where P is read more than once,
  /// with the bar; empty where there is none.
  String guardPrefix() {
    if (pathReads > 1) guards.insert(0, 'ground(P?)');
    return guards.isEmpty ? '' : '${guards.join(', ')} | ';
  }
}

class _Generator {
  final GenericNames gen;
  final TypeDef? Function(String) resolve;

  /// The widget declarations of the source compiled, by moded type, and
  /// those in force while one question's widget is drawn: the declarations
  /// in scope where its volitional procedure is declared ([InteractiveType.
  /// widgets]).
  final Map<String, String> sourceDeclared;
  Map<String, String> declared;
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

  /// The labels of the declaration whose widget is being drawn, from its
  /// declared type down to a type a declaration of its own names; null
  /// where none is (vGLP 1126f4f, Section 6.2, "The words a platform shows":
  /// "a widget term carries a label beside each constructor of Definition
  /// Default Widget").
  DeclaredWidget? _labels;

  /// The types of the grants' raw inputs, R in input(Id, P, R): each
  /// question's type, or its element type where it is a stream or a list,
  /// and the end button's grant of a list (vGLP, Definition "Construct
  /// Process": "the grants' raw inputs are of the types of its questions").
  final _rTypes = <String, _Node>{};
  var _rListEnd = false;

  /// The drawing procedure of each ask functor, run_f/5.
  final _runs = <String, String>{};

  /// The view type of each ask functor's construct, the type of the values
  /// it draws under the functor (vGLP, Section 6.3, "The construct process":
  /// "V is of the view type of T"), and the view types formed, by node.
  final _rootViews = <String, String>{};
  final _viewMemo = <String, String>{};

  /// A former's argument, the person's raw input or a position of it, marked
  /// by the type it forms: the type of the person channel, T, where the type
  /// is one of the grants' raw inputs, whose answer_N hands it R of T, and
  /// the type itself where it is only a position of one ([_resolveFormed]).
  static const _formedMark = '\u0001';

  _Generator(this.gen, this.resolve, this.sourceDeclared, this.fresh,
      this.questionType, this.constructName)
      : declared = sourceDeclared;

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
    // The program's end of the person channel receives no grant and is
    // typed Stream(Input(T)) all the same, and a program that defines no
    // Output has its unused output typed Stream(Draw(T)), so the dispatcher,
    // and every program that imports it, has no open type (vGLP 305c8d0,
    // Section 6.3, the dispatcher's listing).
    final ch = output == null
        ? 'Channel(Stream(${gen.inputType}), Stream(${gen.drawType}))'
        : 'Channel(Stream(${gen.inputType}), Stream($outputTypeName))';
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
              '$d(Asks?, PCh?, MIn, ODs?, G0s, Ss), '
              '$out($outputWidget, MOut?, G0s?, ODs), $cs(Ss?).')
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
      _checkInteractive(t, root);
      declared = t.widgets ?? sourceDeclared;
      try {
        // The widget of the type: the one in scope where its volitional
        // procedure is declared.
        final w = _widget(root, {});
        widgets[t.written] = w;
        hook.writeln(_constructClause(t, root, w));
        // The views its construct draws, each under the functor of its ask,
        // are of its view type, run_f's (vGLP, Section 6.3, "The construct
        // process"); in T they stay typed _, since every GLP module that
        // imports the dispatcher writes T again by hand, with f(_), and an
        // import agrees with its export only where their types are equal
        // (TGLP modules.tex, "Cross-module type checking").  The paper has
        // the compilation write the types it forms into each such module
        // (vGLP, Section 6.3, "The canonical compilation": "the compilation
        // writes into each module that imports a compiled procedure or the
        // dispatcher the types it forms for them, among them the type of the
        // asks, the view types and the parameter of the person channel"),
        // which is not built: until it is, T's alternative of each ask
        // functor is f(_).
        drawnAlts.add('${t.functor}(_)');
      } finally {
        declared = sourceDeclared;
      }
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
    return ConstructProcesses(
        _resolveFormed('$drawnDef$hook$_out'), widgets);
  }

  /// Each former's argument marked by the type it forms ([_formedMark]): T,
  /// the type of the person channel, where that type is one of the grants'
  /// raw inputs, which the union T holds (_rUnion), and the type itself
  /// otherwise.
  String _resolveFormed(String text) => text.replaceAllMapped(
      RegExp('$_formedMark([^$_formedMark]*)$_formedMark'),
      (m) => _rTypes.containsKey(m[1]) ? gen.drawn : m[1]!);

  /// run_f(Id?, W?, Views?, Done?, Gs?, Gs1, Ds): draw the first view V of
  /// construct Id with its widget W, under the functor f of its ask,
  /// draw(Id, W, f(V)) (vGLP, Section 6.3, "The construct process": "with V
  /// under the functor t of its ask"), and only then hand its questions
  /// their grants, Gs1 the grants input(Id, P, R) of Gs: "On a grant
  /// input(Id, Pos, R) for a question W it holds and has drawn" (ibid.), a
  /// question being drawn first once the output of the question is written
  /// whole ("A construct process has the bridge draw the widget of T once
  /// every reader in the output it holds is assigned", ibid.).  A grant that
  /// comes before the draw waits for it, and binds nothing until then.  Then
  /// run_f_more/8 draws each view after it.  Views that end with none drawn,
  /// which no construct process writes, pass the grants on and wait to
  /// withdraw, the clause covering the stream's [].  Its views are of [view]
  /// where the compiler prints that type ([_rootView]).
  ///
  /// run_f_more(Id?, W?, V?, Views?, Done?, Gs?, Gs1, Ds): V the view last
  /// drawn; draw each view after it as it arrives; on a refresh draw V again,
  /// "On refresh, which the dispatcher passes to every construct process, it
  /// draws again what it last drew" (vGLP d433f71, Section 6.3, "The
  /// construct process"); pass each grant input(Id, P, R) on to the
  /// questions, Gs1, a refresh reaching none of them; withdraw when Done is
  /// bound, every question the construct holds answered and no question
  /// still to come, and on nothing else.  Once the views end, run_f_last/7
  /// holds the last view on screen, drawing it again on a refresh, until it
  /// withdraws.  run_f_grants(Gs?, Gs1) passes the grants of a construct
  /// that drew nothing on to its questions, and no refresh.
  String _runFor(String f, String view) {
    final known = _runs[f];
    if (known != null) return known;
    final r = gen.run(f);
    final more = gen.run('${f}_more');
    final last = gen.run('${f}_last');
    final grants = gen.run('${f}_grants');
    _runs[f] = r;
    final vs = 'Stream($view)?';
    final gs = gen.inputsType;
    final ds = 'Stream(${gen.drawType})';
    final dn = gen.doneType;
    _out
      ..writeln('procedure $r(Integer?, $widgetTypeName?, $vs, '
          '$dn?, $gs?, $gs, $ds).')
      ..writeln('$r(Id, W, [V | Vs], Done, Gs, Gs1?, '
          '[draw(Id?, W?, $f(V?)) | Ds?]) :- '
          'ground(W?), ground(V?) | '
          '$more(Id?, W?, V?, Vs?, Done?, Gs?, Gs1, Ds).')
      ..writeln('$r(Id, _, [], Done, Gs, Gs1?, Ds?) :- '
          '$grants(Gs?, Gs1), ${gen.runEnd}(Id?, Done?, Ds).')
      ..writeln()
      ..writeln('procedure $more(Integer?, $widgetTypeName?, $view?, $vs, '
          '$dn?, $gs?, $gs, $ds).')
      ..writeln('$more(Id, _, _, _, done, Gs, Gs?, [withdraw(Id?)]).')
      ..writeln('$more(Id, W, _, [V | Vs], Done, Gs, Gs1?, '
          '[draw(Id?, W?, $f(V?)) | Ds?]) :- ground(W?), ground(V?) | '
          '$more(Id?, W?, V?, Vs?, Done?, Gs?, Gs1, Ds).')
      ..writeln('$more(Id, W, V, Vs, Done, [refresh | Gs], Gs1?, '
          '[draw(Id?, W?, $f(V?)) | Ds?]) :- ground(W?), ground(V?) | '
          '$more(Id?, W?, V?, Vs?, Done?, Gs?, Gs1, Ds).')
      ..writeln('$more(Id, W, V, Vs, Done, [input(I, P, R) | Gs], '
          '[input(I?, P?, R?) | Gs1?], Ds?) :- '
          '$more(Id?, W?, V?, Vs?, Done?, Gs?, Gs1, Ds).')
      ..writeln('$more(Id, W, V, [], Done, Gs, Gs1?, Ds?) :- '
          '$last(Id?, W?, V?, Done?, Gs?, Gs1, Ds).')
      ..writeln()
      ..writeln('procedure $last(Integer?, $widgetTypeName?, $view?, '
          '$dn?, $gs?, $gs, $ds).')
      ..writeln('$last(Id, _, _, done, Gs, Gs?, [withdraw(Id?)]).')
      ..writeln('$last(Id, W, V, Done, [refresh | Gs], Gs1?, '
          '[draw(Id?, W?, $f(V?)) | Ds?]) :- ground(W?), ground(V?) | '
          '$last(Id?, W?, V?, Done?, Gs?, Gs1, Ds).')
      ..writeln('$last(Id, W, V, Done, [input(I, P, R) | Gs], '
          '[input(I?, P?, R?) | Gs1?], Ds?) :- '
          '$last(Id?, W?, V?, Done?, Gs?, Gs1, Ds).')
      ..writeln()
      ..writeln('procedure $grants($gs?, $gs).')
      ..writeln('$grants([input(I, P, R) | Gs], [input(I?, P?, R?) | Gs1?]) :- '
          '$grants(Gs?, Gs1).')
      ..writeln('$grants([refresh | Gs], Gs1?) :- $grants(Gs?, Gs1).')
      ..writeln();
    return r;
  }

  /// output(W?, Out?, Gs?, Draws): the program's output to its person, drawn
  /// as the construct 0 by its widget W, a thread, its value the elements so
  /// far under the functor output: drawn at once with no element, and again
  /// as each element arrives whole.  It holds no question and never
  /// withdraws (vGLP 169b008, Definition "Canonical Compilation"; Definition
  /// "Construct Process").  Gs are the grants the dispatcher routes to
  /// construct 0: on each refresh it draws again what it last drew, "on
  /// refresh its last element is drawn again" (vGLP d433f71, Section 6.3, "The
  /// construct process"), also once the output has ended, its last draw
  /// staying on screen; a grant input(Id, P, R) for construct 0 answers no
  /// question, construct 0 holding none.
  String _outputDrawing() {
    final o = gen.output('output');
    final more = gen.output('output_more');
    final draw = gen.output('output_draw');
    final add = gen.output('output_add');
    final end = gen.output('output_end');
    const ms = 'Stream($outputTypeName)';
    const es = 'List($outputTypeName)';
    final ds = 'Stream(${gen.drawType})';
    final gs = gen.inputsType;
    _out
      ..writeln('procedure $o($widgetTypeName?, $ms?, $gs?, $ds).')
      ..writeln('$o(W, Ms, Gs, [draw(0, W?, output([])) | Ds?]) :- '
          'ground(W?) | $more(W?, Ms?, [], Gs?, Ds).')
      ..writeln()
      ..writeln('procedure $more($widgetTypeName?, $ms?, $es?, $gs?, $ds).')
      ..writeln('$more(W, [M | Ms], Es, Gs, Ds?) :- ground(M?), ground(Es?) | '
          '$add(Es?, M?, Es1), $draw(W?, Ms?, Es1?, Gs?, Ds).')
      ..writeln('$more(W, Ms, Es, [refresh | Gs], Ds?) :- '
          '$draw(W?, Ms?, Es?, Gs?, Ds).')
      ..writeln('$more(W, Ms, Es, [input(_, _, _) | Gs], Ds?) :- '
          '$more(W?, Ms?, Es?, Gs?, Ds).')
      ..writeln('$more(W, [], Es, Gs, Ds?) :- $end(W?, Es?, Gs?, Ds).')
      ..writeln()
      ..writeln('procedure $draw($widgetTypeName?, $ms?, $es?, $gs?, $ds).')
      ..writeln('$draw(W, Ms, Es, Gs, [draw(0, W?, output(Es?)) | Ds?]) :- '
          'ground(W?), ground(Es?) | $more(W?, Ms?, Es?, Gs?, Ds).')
      ..writeln()
      ..writeln('procedure $end($widgetTypeName?, $es?, $gs?, $ds).')
      ..writeln('$end(W, Es, [refresh | Gs], [draw(0, W?, output(Es?)) | Ds?]) '
          ':- ground(W?), ground(Es?) | $end(W?, Es?, Gs?, Ds).')
      ..writeln('$end(W, Es, [input(_, _, _) | Gs], Ds?) :- '
          '$end(W?, Es?, Gs?, Ds).')
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
  Set<String> _covers(_Node n) =>
      {for (final a in _nodeAlts(n)) ..._altCovers(a)};

  /// The grants' raw inputs as alternatives of T: the type of each question,
  /// or its element type where it is a stream or a list, and the end
  /// button's grant of a list.  "The union is formed over the alternatives
  /// of these types, an alternative common to two of them taken once, and
  /// the compilation refuses a program two of whose alternatives have one
  /// functor and arity and differ, since the alternatives of a union have
  /// distinct functors" (vGLP, sections/parts/dispatcher.tex, after the
  /// dispatcher; vGLP, 2026-10-16 12:05 UTC, Udi: yes).  A type none of whose
  /// alternatives another has is named whole; one sharing an alternative
  /// with a type already in the union adds its other alternatives, each by
  /// itself, so YesNo and Pick ::= yes(MyChild) ; no give YesNo ;
  /// yes(MyChild), the union yes ; no ; yes(MyChild).  A primitive is no
  /// functor: a type whose terms another already covers is that other's
  /// (Date beside Integer), one covering others replaces them (Constant
  /// beside String), and two primitives that overlap with neither covering
  /// the other are refused, naming them, as are an ask functor and a raw
  /// input of one functor.
  List<String> _rUnion(List<String> drawnAlts) {
    // The union's members, each a type named whole or an alternative by
    // itself, with the terms it covers.
    final kept = <(String, Set<String>)>[];
    // Each alternative of a functor, a constant or [] in the union, by what
    // it covers, as written, with the type it is of.
    final altOf = <String, (String, String)>{};
    for (final MapEntry(key: name, value: node) in _rTypes.entries) {
      final alts = [
        for (final a in _nodeAlts(node)) (a, _altCovers(a), _altText(a))
      ];
      for (final (a, c, text) in alts) {
        if (a is _LeafAlt) continue;
        final k = c.single;
        final known = altOf[k];
        if (known != null && known.$1 != text) {
          throw CompileError(
              'The person\'s raw inputs to questions of types ${known.$2} and '
              '$name have the alternatives ${known.$1} and $text, of one '
              'functor and arity, which differ: the union of the person '
              'channel\'s type is formed over the alternatives of these types, '
              'and the compilation refuses a program two of whose alternatives '
              'have one functor and arity and differ, since the alternatives '
              'of a union have distinct functors (vGLP, sections/parts/'
              'dispatcher.tex)',
              0, 0, phase: 'analyzer');
        }
      }
      final c = _covers(node);
      final all = {for (final k in kept) ...k.$2};
      if (all.containsAll(c)) continue;
      kept.removeWhere((k) => c.containsAll(k.$2));
      final shared = {
        for (final k in kept) ...k.$2.intersection(c)
      };
      if (shared.isEmpty) {
        kept.add((name, c));
      } else {
        // An alternative of a functor, a constant or [] common to two
        // types is taken once; a primitive is none of these.
        final leaves = <String>{
          for (final (a, ac, _) in alts)
            if (a is _LeafAlt) ...ac
        };
        final clash = shared.intersection(leaves).isNotEmpty ||
            shared.any((k) => !altOf.containsKey(k));
        if (clash) {
          final other = kept.firstWhere(
              (k) => k.$2.intersection(c).isNotEmpty);
          throw CompileError(
              'The person\'s raw inputs to questions of types ${other.$1} and '
              '$name overlap, and neither type holds the other\'s, so the '
              'type of the person channel, their union, is not formed (vGLP, '
              'Definition "Construct Process")',
              0, 0, phase: 'analyzer');
        }
        for (final (_, ac, text) in alts) {
          if (all.containsAll(ac)) continue;
          kept.add((text, ac));
        }
      }
      for (final (a, c, text) in alts) {
        if (a is _LeafAlt) continue;
        altOf.putIfAbsent(c.single, () => (text, name));
      }
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

  /// The terms the alternative [a] covers, as [_covers] gives them.
  Set<String> _altCovers(_Alt a) {
    if (a is _ConstAlt) return {'c:${constantSource(a.value)}'};
    if (a is _StructAlt) return {'f:${a.functor}/${a.args.length}'};
    if (a is _NilAlt) return {'[]'};
    if (a is _ConsAlt) return {'.'};
    final leaf = (a as _LeafAlt).leaf;
    switch (leaf) {
      case _Leaf.integer:
        return {'integer'};
      case _Leaf.real:
        return {'real'};
      case _Leaf.number:
        return {'integer', 'real'};
      case _Leaf.string:
        return {'string'};
      case _Leaf.module:
        return {'module'};
      case _Leaf.constant:
        return {'integer', 'real', 'string', 'module'};
      default:
        return {'any'};
    }
  }

  /// The alternative [a] as a type definition writes it.
  String _altText(_Alt a) {
    if (a is _ConstAlt) return constantSource(a.value);
    if (a is _StructAlt) {
      return '${functorNameSource(a.functor)}('
          '${a.args.map(typeSource).join(', ')})';
    }
    if (a is _NilAlt) return '[]';
    if (a is _ConsAlt) {
      return '[${typeSource(a.head)} | ${typeSource(a.tail)}]';
    }
    final l = a as _LeafAlt;
    switch (l.leaf) {
      case _Leaf.integer:
        return 'Integer';
      case _Leaf.real:
        return 'Real';
      case _Leaf.number:
        return 'Number';
      case _Leaf.string:
        return 'String';
      case _Leaf.module:
        return 'Module';
      case _Leaf.constant:
        return 'Constant';
      case _Leaf.param:
        return l.name!;
      default:
        return '_';
    }
  }

  /// The widget the program's output is drawn by: a thread whose elements
  /// are drawn by the widget of Output, `thread(W)`, or `thread` where W is
  /// `shown`; `named(W', D)`, D that thread, where a declaration names the
  /// widget W' of Stream(Output).  Output is a value the program writes,
  /// refused where it is not an interactive type (_checkInteractive), and
  /// refused, naming it, where the person writes a position of it: the
  /// output is "outside a question" (vGLP, 2026-10-13 19:25 UTC, item 4).
  String _outputWidget(TypeDef output) {
    final t = TypeRef(outputTypeName, output.line, output.column);
    final it = InteractiveType(t, false, 'output', const [], output.line,
        output.column);
    final root = _Node(t, false, const {});
    _checkInteractive(it, root);
    if (_hasQuestion(root)) {
      throw CompileError(
          'The type $outputTypeName, the type of the program\'s output to its '
          'person, has a position the person writes, and the output is outside '
          'a question: the dispatcher draws it as a thread whose elements are '
          'drawn by the widget of $outputTypeName (vGLP 169b008, Definition '
          '"Canonical Compilation"; vGLP, 2026-10-13 19:25 UTC, item 4)',
          output.line, output.column, phase: 'analyzer');
    }
    // The labels of a declaration of Stream(Output) hold within its widget.
    final named = declared['Stream($outputTypeName)'];
    final dw = named == null ? null : DeclaredWidget.parse(named);
    final outer = _labels;
    if (dw != null) _labels = dw;
    final String w;
    try {
      w = _widget(root, {});
    } finally {
      _labels = outer;
    }
    // The widget of Stream(Output), by default a thread of the elements, each
    // drawn by the widget of Output; named(W, D) where a declaration in scope
    // names it W, D that thread (vGLP 92e6d3b, Section 6.3; vGLP 99c7dc8,
    // sections/parts/canonical-compilation.tex: "the compiled program draws
    // that output, construct 0, by the widget of its type Stream(Output), by
    // default a thread whose elements are drawn by the widget of Output";
    // Definition "Default Widget": "A value the program writes to its person
    // outside a question is drawn likewise, by the widget of its declared
    // type").  The program writes it, so the declaration is of the moded type
    // Stream(Output), with no `?`.
    final d = w == 'shown' ? 'thread' : 'thread($w)';
    return dw == null ? d : 'named(${constantSource(dw.drawnAs)}, $d)';
  }

  // --- the construct clause -------------------------------------------------

  /// The construct clause of [t]: the root of its interactive variable's term
  /// is at the path [].
  String _constructClause(InteractiveType t, _Node root, String w) {
    final f = t.functor;
    if (root.person) {
      _checkPersonWritable(root, t, root: true);
      if (_seqWritten(root)) {
        // A stream the person writes whose element holds a position the
        // program writes: drawn as its elements answered and its open tail.
        final v = _rootViews[f] = _seqView(root);
        return '$constructName(Id, $f(X?), Gs, Ds?) :- '
            '${_answerSeqWritten(root)}([], [], X, Gs1?, _, Vs, Done), '
            '${_runFor(f, v)}(Id?, $w, Vs?, Done?, Gs?, Gs1, Ds).';
      }
      if (_holdsWritten(root)) {
        final v = _rootViews[f] = _personView(root);
        return '$constructName(Id, $f(X?), Gs, Ds?) :- '
            '${_answer(root)}([], X, Gs1?, _, Vs, Done), '
            '${_runFor(f, v)}(Id?, $w, [input([]) | Vs?], Done?, Gs?, Gs1, Ds).';
      }
      final answer = _answer(root);
      if (_holdsNested(root)) {
        // The form's answer leaves its streams and lists open, each a
        // question of its own drawn under it: the view after the answer.
        final v = _rootViews[f] = _qView(root);
        return '$constructName(Id, $f(X?), Gs, Ds?) :- '
            '$answer([], X, Gs1?, _, Vs, Done), '
            '${_runFor(f, v)}(Id?, $w, [input([]) | Vs?], Done?, Gs?, Gs1, '
            'Ds).';
      }
      final v = _rootViews[f] = _rootView(f, root);
      return '$constructName(Id, $f(X?), Gs, Ds?) :- '
          '$answer([], X, Gs1?, _, Done), '
          '${_runFor(f, v)}(Id?, $w, [input([])], Done?, Gs?, Gs1, Ds).';
    }
    if (!_hasQuestion(root)) {
      // A type the person writes nowhere holds no question, so its construct
      // has every question it holds answered at once, and withdraws (Definition
      // "Construct, Submission, Complete Widget": a construct is presented
      // while its question is open).
      // It draws nothing; its functor in T is of its type.
      _rootViews[f] = root.typeKey;
      return '$constructName(Id, $f(_), _, [withdraw(Id?)]).';
    }
    final present = _present(root, t);
    final v = _rootViews[f] = _presentView(root);
    return '$constructName(Id, $f(X), Gs, Ds?) :- '
        '$present([], X?, Gs1?, _, Vs, Done), '
        '${_runFor(f, v)}(Id?, $w, Vs?, Done?, Gs?, Gs1, Ds).';
  }

  /// The view type of the construct of [f], whose root [root] the person
  /// writes and which holds no stream or list inside it, so that the one
  /// view it draws is input([]), the question at the root: T ; input(Path)
  /// (vGLP e22e2ad, Section 6.3, "The construct process": "V is of the view
  /// type of T: T with the type S of each position the person writes replaced
  /// by S ; input(Path)").
  String _rootView(String f, _Node root) {
    final v = gen.view(f);
    _out.writeln('$v ::= ${root.typeKey} ; input(${gen.pathType}).');
    return v;
  }

  // --- view types -------------------------------------------------------------

  /// The view type of a construct's positions (vGLP, Section 6.3, "The
  /// construct process": "V is of the view type of T: T with the type S of
  /// each position the person writes replaced by S ; input(Path), and of
  /// each position the program writes inside the person's answers by S ;
  /// pending(Path), so no value drawn is typed by a wildcard"), as the
  /// construct draws them: a position the person writes Questioned(S); a
  /// position the program writes holding no question S itself, drawn once
  /// written whole, as the output of the question is ("A construct process
  /// has the bridge draw the widget of T once every reader in the output it
  /// holds is assigned", ibid.; Definition "Reduce and Communicate in
  /// vmaGLP"); and a position the program writes holding a question its own
  /// view type, [_presentView].  A position the program writes inside the
  /// person's answers is S ; pending(Path) ([_pendingView]), the answer
  /// holding it its own view type ([_writtenView]).

  /// The view of [k], a child of a structure or a stream, as its parent's
  /// clause draws it.
  String _childView(_Node k) {
    if (k.person) return _qView(k);
    if (!_hasQuestion(k)) return k.typeKey;
    return _presentView(k);
  }

  /// The view of a question [k]: S ; input(Path), S its type, or, where its
  /// answer leaves a stream or a list open under it, the answer's view
  /// ([_openView]).
  String _qView(_Node k) => '${gen.questionedType}('
      '${_holdsNested(k) ? _openView(k) : k.typeKey})';

  /// The view of the answer of [n], a question holding a stream or a list
  /// inside a form the person writes: the term answered, each such position
  /// drawn input(Q) (open_N), a type formed for [n].
  String _openView(_Node n) {
    final key = 'open:${n.key}';
    final memo = _viewMemo[key];
    if (memo != null) return memo;
    final ref = '${gen.view('open_${_typeStem(n.type)}')}${_declParams(n)}';
    _viewMemo[key] = ref;
    final alts = <String>[];
    for (final a in _nodeAlts(n)) {
      if (a is _StructAlt) {
        final kids = _children(n, a);
        alts.add(_apply(a, [
          for (final k in kids)
            _isNestedSeq(n, k)
                ? '${gen.questionedType}(${k.typeKey})'
                : _holdsNested(k)
                    ? _openView(k)
                    : k.typeKey
        ]));
      } else {
        alts.add(_plainAlt(n, a));
      }
    }
    _out.writeln('$ref ::= ${alts.join(' ; ')}.');
    return ref;
  }

  /// The view of [n], a position the program writes holding a question, as
  /// present_N draws it once written whole: each alternative holding a
  /// question with its arguments' views, each other alternative as written;
  /// a stream or a list the list of its elements' views.
  String _presentView(_Node n) {
    final key = 'present:${n.key}';
    final memo = _viewMemo[key];
    if (memo != null) return memo;
    final seq = _seqOf(n);
    if (seq != null) {
      final e = n.child(seq.elem);
      final r = 'List(${e.person ? _qView(e) : _presentView(e)})';
      return _viewMemo[key] = r;
    }
    final ref = '${gen.view(_typeStem(n.type))}${_declParams(n)}';
    _viewMemo[key] = ref;
    final alts = <String>[];
    for (final a in _nodeAlts(n)) {
      if (a is _StructAlt && _children(n, a).any(_hasQuestion)) {
        alts.add(_apply(a, [for (final k in _children(n, a)) _childView(k)]));
      } else {
        alts.add(_plainAlt(n, a));
      }
    }
    _out.writeln('$ref ::= ${alts.join(' ; ')}.');
    return ref;
  }

  /// An alternative of [n] as a view holds it whole, each argument of its
  /// type with no mode: the person or the program writes it whole before it
  /// is drawn.
  String _plainAlt(_Node n, _Alt a) {
    if (a is _StructAlt || a is _ConsAlt) {
      return _apply(a, [for (final k in _children(n, a)) k.typeKey]);
    }
    return _altText(a);
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
  /// ::= Stream(String)`, one of the "named types so built" (vGLP,
  /// Definition "Volitional Procedure, Interactive Type"), which draws as
  /// the stream it names: "A named type, recursive or not, is its
  /// definition" (the proof of Proposition "Default Widgets"; vGLP #7
  /// Cowork, 2026-10-11 14:31 UTC, item 2); null for any other type.
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

  /// The positions the program writes inside the person's answers that the
  /// construct builds (fill_N): each holding no question, in an answer
  /// holding no stream or list of its own, at the root of the interactive
  /// variable or in the element of a stream or a list the person writes
  /// there.  Any other is refused, naming the type: not built.  [root] is
  /// whether [n] is the root.
  void _checkPersonWritable(_Node n, InteractiveType t, {bool root = false}) {
    void refuse(String what) => throw CompileError(
        'The construct of the interactive type ${t.written} is not built: '
        'the person writes ${n.typeKey}, $what',
        t.line, t.column, phase: 'analyzer');
    if (!root && (_holdsWritten(n) || _seqWritten(n))) {
      refuse('and a position of it is written by the program, which an '
          'answer inside the program\'s output does not carry');
    }
    final seen = <String>{};
    void walk(_Node m, bool top) {
      if (!m.person || !seen.add(m.key)) return;
      final seq = _personSeq(m);
      if (seq != null) {
        final e = m.child(seq.elem);
        if (!e.person) {
          refuse('and the element of ${m.typeKey} is written by the program');
        }
        if (!top && _holdsWritten(e)) {
          refuse('and a stream or a list in it, ${m.typeKey}, holds a '
              'position the program writes, which an answer left open '
              'inside a form does not carry');
        }
        walk(e, top);
        return;
      }
      if (_holdsWritten(m) && _holdsNested(m)) {
        refuse('and ${m.typeKey} holds both a position the program writes '
            'and a stream or a list of its own');
      }
      for (final a in _nodeAlts(m)) {
        for (final c in _children(m, a)) {
          if (_isWritten(m, c)) {
            if (_hasQuestion(c)) {
              refuse('and a position of it the program writes, '
                  '${c.typeKey}, holds a question');
            }
          } else {
            walk(c, false);
          }
        }
      }
    }
    walk(n, true);
  }

  /// Refuse the interactive type [it], whose root is [root], where it is
  /// not one by the Definition: "An interactive type is built from
  /// constants, String, Integer, Real, Key, Constant, Module, named types so
  /// built, tuples and unions, and Stream or List as the whole type or as an
  /// argument of a tuple, never under another Stream or List, and a Stream
  /// only at a position the person writes; a named type in it is recursive
  /// only if it has no mode inversion, and it has no type parameter" (vGLP
  /// 2f1dc92, Definition "Volitional Procedure, Interactive Type").  Each is refused over the whole
  /// interactive type, a structure the program writes included, one holding
  /// no question too (vGLP #7 Cowork, 2026-10-11 14:31 UTC, items 1 and 2;
  /// vGLP #8 Cowork, 2026-10-11 15:47 UTC, item 1: "The flat refusals hold
  /// over the whole interactive type, a program-written structure holding
  /// no question included"), each message quoting the Definition's clause:
  ///
  ///   - a stream or a list in an element of a stream or a list, whoever
  ///     writes it, the element itself one or holding one, reached through
  ///     a recursive type too (Forum ::= forum(String, List(Forum)));
  ///   - a list cell outside a Stream or a List: a list cell type, a tuple
  ///     '.'(H, T), or a Stream or a List as an alternative of a union, none
  ///     the whole type nor an argument of a tuple;
  ///   - a recursive named type with a mode inversion: a position marked ?
  ///     (TGLP, well-typing.tex, Definition "Simple Prefix": "A mode
  ///     inversion point: any position whose type is marked with ?")
  ///     reachable from it, in its own definition or in one of a type it
  ///     names, since "with no mode inversion, a recursive type is written
  ///     whole by the program or whole by the person" (vGLP 092c9fe, the
  ///     paragraph after the Definition); one with none is admitted, mutual
  ///     recursion included ([_recursive], [_inversionIn]).  Stream and List
  ///     are not these named types: the Definition admits them by their own
  ///     clause, a Stream(YesNo?) the program writes among them;
  ///   - a position the person writes of a type parameter, refused outright
  ///     (14:31 UTC, item 1);
  ///   - a position of the type `_`, whoever writes it, which is none of the
  ///     types an interactive type is built from (15:47 UTC, item 3: "The
  ///     type `_` where the person writes: refused, with the flat refusals;
  ///     it is not among the Definition's building blocks"; vGLP #8 Cowork,
  ///     2026-10-11 17:24 UTC, Q2: "the type _ refused everywhere in an
  ///     interactive type, the building blocks excluding it").
  ///
  /// A stream the program writes whose elements hold questions is presented
  /// element by element ([_presentSeq]).  Each position is walked once in
  /// each of its two contexts, in an element of a stream or a list and not,
  /// so a recursive type is walked once.
  void _checkInteractive(InteractiveType it, _Node root) {
    // The Definition's clauses, each as the refusal of it quotes it.
    const builtFrom = 'an interactive type is "built from constants, String, '
        'Integer, Real, Key, Constant, Module, named types so built, tuples '
        'and unions, and Stream or List"';
    const wholeOrTuple = 'an interactive type has "Stream or List as the '
        'whole type or as an argument of a tuple"';
    const notUnder = 'an interactive type has a Stream or a List "never '
        'under another Stream or List"';
    const noInversion = 'a named type in an interactive type "is recursive '
        'only if it has no mode inversion"';
    const personStream = 'an interactive type has "a Stream only at a '
        'position the person writes"';
    Never refuse(String what, String rule) => throw CompileError(
        'The interactive type ${it.written} is refused: $what, and $rule '
        '($_definition)',
        it.line, it.column, phase: 'analyzer');
    String who(_Node n) => n.person ? 'the person' : 'the program';
    String holds(_Node n) =>
        n.person || !_hasQuestion(n) ? '' : ', holding a question';
    final visited = <String>{};

    // [inSeq] the stream or the list that [n] is in an element of, with
    // that element.
    void walk(_Node n, (_Node, _Node)? inSeq) {
      if (!visited.add('${n.key}|${inSeq != null}')) return;
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
        // "a Stream only at a position the person writes" (vGLP 27bae5d,
        // Definition "Volitional Procedure, Interactive Type"; vGLP #24,
        // Code_inbox.md, 2026-11-07 22:27 UTC, 19:39 item 3): a stream the
        // program writes is refused, a list it writes admitted.
        if (!seq.list && !n.person) {
          refuse('the program writes ${n.typeKey}, a Stream${holds(n)}',
              personStream);
        }
        final e = n.child(seq.elem);
        walk(e, (n, e));
        return;
      }
      if (_recursive(n)) {
        final inversion = _inversionIn(n);
        if (inversion != null) {
          refuse(
              '${who(n)} writes ${n.typeKey}, a recursive type with a mode '
              'inversion, $inversion',
              noInversion);
        }
      }
      for (final a in _nodeAlts(n)) {
        // "it has no type parameter" (vGLP 2f1dc92), at a position either
        // writes.
        if (a is _LeafAlt && a.leaf == _Leaf.param) {
          throw CompileError(
              'The interactive type ${it.written} is refused: ${who(n)} '
              'writes a position of type ${a.name}, a type parameter, and an '
              'interactive type "has no type parameter" ($_definition)',
              it.line, it.column, phase: 'analyzer');
        }
        if (a is _LeafAlt && a.leaf == _Leaf.any) {
          refuse(
              '${who(n)} writes a position of type _, none of the types '
              'an interactive type is built from',
              builtFrom);
        }
        if (a is _ConsAlt) {
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
          walk(k, inSeq);
        }
      }
    }

    walk(root, null);
  }

  /// The alternatives of the named type [u] as its definition gives them,
  /// its type arguments substituted for the definition's parameters; none
  /// where it is not defined ([_flatten] refuses it then).
  List<TypeExpr> _alternativesOf(TypeRef u) {
    final def = resolve(u.name);
    if (def == null) return const [];
    final m = <String, TypeExpr>{
      for (var i = 0; i < def.typeParams.length && i < u.typeArgs.length; i++)
        def.typeParams[i]: u.typeArgs[i]
    };
    return [for (final a in def.alternatives) m.isEmpty ? a : _subst(a, m)];
  }

  /// The positions of [t] one step down, for what a type reaches: for a
  /// named type, those of each alternative of its definition --- a tuple's
  /// arguments, a list cell's head and tail, a difference list's content and
  /// hole, and a named type that is an alternative, the union's member,
  /// itself; for a tuple or a list cell written in place, its arguments;
  /// none for a constant, a primitive, [] or a type parameter in [params].
  List<TypeExpr> _positions(TypeExpr t, Set<String> params) {
    if (t is StructAlt) return t.args;
    if (t is ListConsAlt) return [t.head, t.tail];
    if (t is DiffListAlt) return [t.content, t.hole];
    if (t is! TypeRef) return const [];
    final u = _unmoded(t) as TypeRef;
    if (_leafOf(u.name, params) != null || u.name == 'MutualRef') {
      return const [];
    }
    return [
      for (final a in _alternativesOf(u))
        if (a is TypeRef) a else ..._positions(a, params)
    ];
  }

  final _recursiveMemo = <String, bool>{};
  final _inversionMemo = <String, String?>{};

  /// Whether [n] is of a recursive named type: its definition reaches the
  /// type again, through the named types it uses, their type arguments and
  /// the alternatives of the unions among them --- SpeechAct ::=
  /// speech(Utterance, Key), Utterance ::= text(String) ; SpeechAct, each
  /// recursive through the other.  Stream and List are not these: the
  /// Definition admits them by their own clause, and the recursion of
  /// `Stream(X) ::= [] ; [X | Stream(X)]` is the stream's own.
  bool _recursive(_Node n) {
    final t = n.type;
    if (t is! TypeRef ||
        _seqName(t.name) != null ||
        _leafOf(t.name, n.params) != null) {
      return false;
    }
    final target = n.typeKey;
    return _recursiveMemo.putIfAbsent(target, () {
      final seen = <String>{};
      bool reaches(TypeExpr from) {
        for (final c in _positions(from, n.params)) {
          final u = _unmoded(c);
          final key = typeSource(u);
          if (u is TypeRef && key == target) return true;
          if (!seen.add(key)) continue;
          if (reaches(u)) return true;
        }
        return false;
      }

      return reaches(t);
    });
  }

  /// The first mode inversion in the type of [n], a position marked `?`
  /// reachable from it --- in its own definition or in that of a named type
  /// it reaches, Stream and List and their type arguments included --- as
  /// the refusal names it, `YesNo? in node(Tree, YesNo?)` or `YesNo? in
  /// ask(YesNo?) of Utterance`; null where there is none.
  String? _inversionIn(_Node n) {
    final key = n.typeKey;
    if (_inversionMemo.containsKey(key)) return _inversionMemo[key];
    final seen = <String>{};
    late final String? Function(TypeExpr t, String alt, String of) within;
    // [c] at a position of the alternative [alt] of the named type [of].
    String? at(TypeExpr c, String alt, String of) {
      if (_isInput(c)) {
        return '${typeSource(c)} in $alt${of == key ? '' : ' of $of'}';
      }
      return within(c, alt, of);
    }

    within = (TypeExpr t, String alt, String of) {
      if (t is StructAlt || t is ListConsAlt || t is DiffListAlt) {
        for (final c in _positions(t, n.params)) {
          final r = at(c, alt, of);
          if (r != null) return r;
        }
        return null;
      }
      if (t is! TypeRef) return null;
      if (_leafOf(t.name, n.params) != null || t.name == 'MutualRef') {
        return null;
      }
      final name = typeSource(t);
      if (!seen.add(name)) return null;
      for (final a in _alternativesOf(t)) {
        // A union's member in the other mode is an inversion itself.
        if (_isInput(a)) {
          return '${typeSource(a)}, an alternative of $name';
        }
        final r = within(a, typeSource(a), name);
        if (r != null) return r;
      }
      return null;
    };
    return _inversionMemo[key] = within(n.type, key, key);
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

  /// Whether the view of [n] can change once drawn: one the person writes
  /// whose answer leaves a stream or a list open under it, drawn again once
  /// answered; one the program writes holding such a question, drawn again
  /// as that question is.  A position the program writes is drawn once
  /// written whole, and again only as the questions in it are drawn again
  /// (vGLP, Section 6.3, "The construct process").
  bool _multi(_Node n) {
    if (n.person) return _holdsNested(n);
    if (!_hasQuestion(n)) return false;
    final memo = _multiMemo[n.key];
    if (memo != null) return memo;
    _multiMemo[n.key] = false; // least fixpoint through recursive types
    final seq = _seqOf(n);
    final result = seq != null
        ? _multi(n.child(seq.elem))
        : _nodeAlts(n).any((a) => _children(n, a).any(_multi));
    return _multiMemo[n.key] = result;
  }

  final _multiMemo = <String, bool>{};

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
  /// refused clause, whole (2026-10-04 09:05 UTC, C).  A refresh, an Input
  /// too (vGLP d433f71, Section 6.3), never reaches a question, run_f_more
  /// taking it, and would go on the same way.
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
    // An element holding a stream or a list is refused, not an interactive
    // type (_checkInteractive).
    final seq = _personSeq(n);
    final e = seq == null ? null : n.child(seq.elem);
    final nested = _holdsNested(n);
    // An answer holding a position the program writes is drawn again as
    // that position is written (fill_N).
    final written = _holdsWritten(n);
    final fill = written ? _fill(n) : null;
    final vs = nested || written ? ', Vs' : '';
    final vsOut = nested || written ? ', Vs?' : '';
    final vsType = nested
        ? ', Stream(${_qView(n)})'
        : written
            ? ', Stream(${_personView(n)})'
            : '';
    final form = _former(e ?? n);
    _rTypes.putIfAbsent(_formedKey(e ?? n), () => e ?? n);
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
      ..writeln('$name(P, X?, [G | Gs], Gs1?$vsOut, Done?) :- '
          'otherwise | '
          '$take(refused, P?, X, G?, Gs?, Gs1$vs, Done).')
      ..writeln()
      ..writeln('procedure$p $take($formed(${_formedKey(e ?? n)})?, $pt?, $t, '
          '${gen.inputType}?, $gs?, $gs$vsType, $done).');
    if (e != null) {
      _out.writeln('$take(formed(V), P, [V? | X1?], _, Gs, Gs1?, Done?) :- '
          '$name(P?, X1, Gs?, Gs1, Done).');
    } else if (nested) {
      _out.writeln('$take(formed(V), P, X?, _, Gs, Gs1?, [W?], Done?) :- '
          '$open(V?, P?, X, Gs?, Gs1, W, Done).');
    } else if (written) {
      // "it withdraws once it holds no question and no reader at a position
      // whose output may hold one" (vGLP, Section 6.3, "The construct
      // process"): a position the program writes inside the answer holds no
      // question, so the answer is done at once.
      _out.writeln('$take(formed(V), P, X?, _, Gs, Gs?, Vs?, done) :- '
          '$fill(V?, P?, X, Vs).');
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
      final acc = _Arguments();
      for (var i = 0; i < kids.length; i++) {
        final k = kids[i];
        final idx = i + 1;
        if (_isNestedSeq(n, k)) {
          final y = 'Y$idx', m = 'M$idx', q = 'Q$idx', d = 'D$idx';
          final gsOut = acc.nextGs();
          head.add('_');
          xs.add('$y?');
          views.add('input($m?)');
          acc.goals
            ..add('${gen.path}(P?, $idx, $m)')
            ..add('${gen.path}(P?, $idx, $q)')
            ..add('${_answer(k)}($q?, $y, ${acc.gsIn}?, $gsOut, $d)');
          acc.pathReads += 2;
          acc.gsIn = gsOut;
          acc.dones.add(d);
        } else if (_holdsNested(k)) {
          final h = 'H$idx', x = 'X$idx', v = 'V$idx', q = 'Q$idx';
          final d = 'D$idx';
          final gsOut = acc.nextGs();
          head.add(h);
          xs.add('$x?');
          views.add('$v?');
          final open = _open(k);
          acc.goals
            ..add('${gen.path}(P?, $idx, $q)')
            ..add('$open($h?, $q?, $x, ${acc.gsIn}?, $gsOut, $v, $d)');
          acc.pathReads += 1;
          acc.gsIn = gsOut;
          acc.dones.add(d);
        } else {
          final h = 'H$idx';
          head.add(h);
          xs.add('$h?');
          views.add('$h?');
          acc.guards.add('ground($h?)');
        }
      }
      final doneHead = acc.doneHead(gen.allDone);
      final g0 = acc.guardPrefix();
      clauses.add('$name(${_apply(a, head)}, P, ${_apply(a, xs)}, Gs, '
          '${acc.gsIn}?, ${_apply(a, views)}, $doneHead) :- '
          '$g0${acc.goals.join(', ')}.');
    }
    _out.writeln('procedure$p $name($t?, ${gen.pathType}?, $t, $gs?, $gs, '
        '${_openView(n)}, ${gen.doneType}).');
    for (final c in clauses) {
      _out.writeln(c);
    }
    _out
      ..writeln('$name(V, _, X?, Gs, Gs?, W?, done) :- '
          'ground(V?) | X = V?, W = V?.')
      ..writeln();
    return name;
  }

  // --- positions the program writes inside the person's answers ---------------
  //
  // vGLP, Definition "Answer": "t a term of the type of X, ground but for
  // variables of V_p occurring nowhere in the configuration: a reader Y_i? at
  // each position the person writes of type Stream(T') or List(T') ... and a
  // writer W_j at each outermost position the program writes"; Section 6.3,
  // "The construct process": "u is a term of type T' with R at the positions
  // the person writes and with fresh variables where Answer has them ... it
  // holds the readers of the new variables at the positions the program
  // writes"; "A construct process has the bridge draw the widget of T ...
  // again as output arrives inside the person's answers, each position there
  // not yet written whole drawn as pending"; "V is of the view type of T: T
  // with the type S of each position the person writes replaced by S ;
  // input(Path), and of each position the program writes inside the person's
  // answers by S ; pending(Path)"; Proposition "Default Widgets", its proof:
  // "a fresh writer at a position the program writes, which the form shows
  // and does not edit; the form forms the tuple".  The chat of Section 3,
  // Line ::= line(String, Status?), is the case (vGLP 27bae5d,
  // sections/parts/vglp-chat.tex; vGLP #24, Code_inbox.md, 2026-11-07 22:27
  // UTC, 19:39 item 1).
  //
  // The former forms the person's positions and holds hole, a constant, at
  // each position the program writes (skel_N, the skeleton of the answer),
  // the bridge granting hole there, the raw input of such a question being of
  // its skeleton type, since no value of the type the program writes is the
  // person's to give; fill_N puts a
  // fresh writer at each hole, the answer, and keeps its reader, drawing the
  // position pending(Q) until it is written whole and then as written
  // (pend_N), the views of the answer combined as each position is
  // (comb_N).  A stream the person writes whose element holds such a
  // position is drawn as the elements answered, each so, then input(P) at
  // its tail, the question still open (ans_N).  Not built, each a compile
  // error: a position the program writes holding a question inside an
  // answer; an answer holding both such a position and a stream or a list of
  // its own; a stream or a list inside a form, or inside the program's
  // output, whose answers hold such a position.

  /// Whether [k], a child of [n], is a position the program writes inside
  /// one the person writes.
  bool _isWritten(_Node n, _Node k) => n.person && !k.person;

  final _writtenMemo = <String, bool>{};

  /// Whether [n], a position the person writes that is not a stream or a
  /// list, holds a position the program writes, in an argument or below one
  /// the person writes.
  bool _holdsWritten(_Node n) {
    if (!n.person || _personSeq(n) != null) return false;
    final memo = _writtenMemo[n.key];
    if (memo != null) return memo;
    _writtenMemo[n.key] = false; // least fixpoint through recursive types
    var result = false;
    for (final a in _nodeAlts(n)) {
      if (a is! _StructAlt) continue;
      for (final c in _children(n, a)) {
        if (_isWritten(n, c) || (!_isNestedSeq(n, c) && _holdsWritten(c))) {
          result = true;
        }
      }
    }
    return _writtenMemo[n.key] = result;
  }

  /// Whether [n] is a stream or a list the person writes whose element holds
  /// a position the program writes.
  bool _seqWritten(_Node n) {
    final seq = _personSeq(n);
    if (seq == null) return false;
    final e = n.child(seq.elem);
    return e.person && _holdsWritten(e);
  }

  /// The type a former of [c] forms: its skeleton where it holds a position
  /// the program writes, and its own type otherwise.
  String _formedKey(_Node c) => _holdsWritten(c) ? _skel(c) : c.typeKey;

  String? _holeType;

  /// The type of the hole a skeleton holds where the program writes.
  String _hole() {
    final h = _holeType;
    if (h != null) return h;
    final t = _holeType = gen.view('hole');
    _out.writeln('$t ::= hole.');
    return t;
  }

  /// skel_N: the type of [n] with hole at each position the program writes.
  String _skel(_Node n) {
    final key = 'skel:${n.key}';
    final memo = _viewMemo[key];
    if (memo != null) return memo;
    final ref = gen.view('skel_${_typeStem(n.type)}');
    _viewMemo[key] = ref;
    final alts = <String>[];
    for (final a in _nodeAlts(n)) {
      if (a is _StructAlt) {
        alts.add(_apply(a, [
          for (final k in _children(n, a))
            _isWritten(n, k)
                ? _hole()
                : _isNestedSeq(n, k)
                    ? k.typeKey
                    : _formedKey(k)
        ]));
      } else {
        alts.add(_plainAlt(n, a));
      }
    }
    _out.writeln('$ref ::= ${alts.join(' ; ')}.');
    return ref;
  }

  /// The view type of [n], a position the person writes holding one the
  /// program writes: its alternatives with S ; input(Path) at each position
  /// the person writes and S ; pending(Path) at each the program writes.
  String _writtenView(_Node n) {
    final key = 'written:${n.key}';
    final memo = _viewMemo[key];
    if (memo != null) return memo;
    final ref = gen.view('written_${_typeStem(n.type)}');
    _viewMemo[key] = ref;
    final alts = <String>[];
    for (final a in _nodeAlts(n)) {
      if (a is _StructAlt) {
        alts.add(_apply(a, [
          for (final k in _children(n, a))
            _isWritten(n, k) ? _pendingView(k) : _personView(k)
        ]));
      } else {
        alts.add(_plainAlt(n, a));
      }
    }
    _out.writeln('$ref ::= ${alts.join(' ; ')}.');
    return ref;
  }

  /// The view of a position the person writes inside such an answer:
  /// Questioned(S), S its own view where it holds a position the program
  /// writes.
  String _personView(_Node k) => '${gen.questionedType}('
      '${_holdsWritten(k) ? _writtenView(k) : k.typeKey})';

  /// The view of a position the program writes inside the person's answer:
  /// S ; pending(Path).
  String _pendingView(_Node k) {
    final key = 'pending:${k.key}';
    final memo = _viewMemo[key];
    if (memo != null) return memo;
    final ref = gen.view('pending_${_typeStem(k.type)}');
    _viewMemo[key] = ref;
    _out.writeln('$ref ::= ${k.typeKey} ; pending(${gen.pathType}).');
    return ref;
  }

  /// pend_N(W?, Q?, Vs): the reader W of a position [k] the program writes
  /// inside the person's answer, at Q: drawn pending(Q), and again as
  /// written once written whole.
  String _pend(_Node k) {
    final name = _name('pend', k);
    if (!_emitted.add('pend:${k.key}')) return name;
    final one = _name('pend', k, 'whole');
    final v = _pendingView(k);
    _out
      ..writeln('procedure $name(${k.typeKey}?, ${gen.pathType}?, '
          'Stream($v)).')
      ..writeln('$name(W, Q, [pending(Q?) | Vs?]) :- $one(W?, Vs).')
      ..writeln()
      ..writeln('procedure $one(${k.typeKey}?, Stream($v)).')
      ..writeln('$one(W, [W?]) :- ground(W?) | true.')
      ..writeln();
    return name;
  }

  /// fill_N(V?, P?, X, Vs): X the answer of node [n] at P, from V, the
  /// skeleton the former formed: V with a fresh writer at each hole, whose
  /// reader pend_N holds, and each argument holding such a position filled
  /// likewise; Vs its views, drawn again as each such position is written.
  String _fill(_Node n) {
    final name = _name('fill', n);
    if (!_emitted.add('fill:${n.key}')) return name;
    final view = _personView(n);
    final clauses = <String>[];
    for (final a in _nodeAlts(n)) {
      if (a is _ConstAlt) {
        final c = constantSource(a.value);
        clauses.add('$name($c, _, $c, [$c]).');
        continue;
      }
      if (a is _NilAlt) {
        clauses.add('$name([], _, [], [[]]).');
        continue;
      }
      if (a is _LeafAlt) {
        clauses.add('$name(V, _, X?, Vs?) :- ${_guard(a.leaf)}(V?) | '
            'X = V?, Vs = [V?].');
        continue;
      }
      if (a is! _StructAlt) continue;
      final kids = _children(n, a);
      final head = <String>[], xs = <String>[], goals = <String>[];
      final guards = <String>[], streams = <String>[], plain = <String>[];
      final slots = <String>[];
      var pathReads = 0;
      for (var i = 0; i < kids.length; i++) {
        final k = kids[i];
        final idx = i + 1;
        if (_isWritten(n, k)) {
          head.add('hole');
          xs.add('W$idx');
          goals
            ..add('${gen.path}(P?, $idx, Q$idx)')
            ..add('${_pend(k)}(W$idx?, Q$idx?, S$idx)');
          pathReads++;
          streams.add('S$idx?');
          slots.add('s');
        } else if (_holdsWritten(k)) {
          head.add('H$idx');
          xs.add('X$idx?');
          goals
            ..add('${gen.path}(P?, $idx, Q$idx)')
            ..add('${_fill(k)}(H$idx?, Q$idx?, X$idx, S$idx)');
          pathReads++;
          streams.add('S$idx?');
          slots.add('s');
        } else {
          head.add('H$idx');
          xs.add('H$idx?');
          guards.add('ground(H$idx?)');
          plain.add('H$idx?');
          slots.add('h');
        }
      }
      if (streams.isEmpty) {
        // An alternative holding no such position is its own view.
        clauses.add('$name(${_apply(a, head)}, _, ${_apply(a, xs)}, '
            '[${_apply(a, xs)}])${guards.isEmpty ? '.' : ' :- ${guards.join(', ')} | true.'}');
        continue;
      }
      if (pathReads > 1) guards.insert(0, 'ground(P?)');
      goals.add('${_comb(n, a, kids, slots)}('
          '${[...streams, ...plain].join(', ')}, Vs)');
      final g = guards.isEmpty ? '' : '${guards.join(', ')} | ';
      clauses.add('$name(${_apply(a, head)}, P, ${_apply(a, xs)}, Vs?) :- '
          '$g${goals.join(', ')}.');
    }
    _out.writeln('procedure $name(${_formedKey(n)}?, ${gen.pathType}?, '
        '${n.typeKey}, Stream($view)).');
    for (final c in clauses) {
      _out.writeln(c);
    }
    _out.writeln();
    return name;
  }

  /// comb_N(S1?, ..., Sm?, H1?, ..., Vs): the views of alternative [a] of
  /// [n] from the views of its arguments that change, S1 ... Sm, each a
  /// stream, and the values of those that do not, H1 ...: the first once
  /// each has its first, and again as any is drawn again; done when each
  /// stream is.  [slots] says, argument by argument, which is which.
  String _comb(_Node n, _Alt a, List<_Node> kids, List<String> slots) {
    final suffix = _altSuffix(n, a);
    final name = _name('comb', n, suffix);
    if (!_emitted.add('comb:${n.key}:$suffix')) return name;
    final more = _name('comb', n, '${suffix}_more');
    final view = _personView(n);
    final sIdx = [for (var i = 0; i < kids.length; i++) if (slots[i] == 's') i];
    final hIdx = [for (var i = 0; i < kids.length; i++) if (slots[i] == 'h') i];
    String elemType(int i) => _isWritten(n, kids[i])
        ? _pendingView(kids[i])
        : _personView(kids[i]);
    final sTypes = [for (final i in sIdx) 'Stream(${elemType(i)})?'];
    final aTypes = [for (final i in sIdx) '${elemType(i)}?'];
    final hTypes = [for (final i in hIdx) '${kids[i].typeKey}?'];
    String viewOf(Map<int, String> at) =>
        _apply(a, [for (var i = 0; i < kids.length; i++) '${at[i]}?']);
    final aNames = {for (final i in sIdx) i: 'A${i + 1}'};
    final hNames = {for (final i in hIdx) i: 'H${i + 1}'};
    final hGuards = [for (final i in hIdx) 'ground(H${i + 1}?)'];
    final aGuards = [for (final i in sIdx) 'ground(A${i + 1}?)'];
    // The first view, once each stream has its first.
    final firstHead = [
      for (final i in sIdx) '[A${i + 1} | S${i + 1}]',
      for (final i in hIdx) 'H${i + 1}',
      '[${viewOf({...aNames, ...hNames})} | Vs?]',
    ];
    final moreArgs = [
      for (final i in sIdx) 'A${i + 1}?',
      for (final i in sIdx) 'S${i + 1}?',
      for (final i in hIdx) 'H${i + 1}?',
    ];
    _out
      ..writeln('procedure $name(${[...sTypes, ...hTypes, 'Stream($view)'].join(', ')}).')
      ..writeln('$name(${firstHead.join(', ')}) :- '
          '${[...aGuards, ...hGuards].join(', ')} | '
          '$more(${[...moreArgs, 'Vs'].join(', ')}).');
    // A stream that ends before its first view: none is drawn.
    for (final j in sIdx) {
      _out.writeln('$name(${[
        for (final i in sIdx) i == j ? '[]' : '_',
        for (final _ in hIdx) '_',
        '[]',
      ].join(', ')}).');
    }
    _out
      ..writeln()
      ..writeln('procedure $more(${[...aTypes, ...sTypes, ...hTypes, 'Stream($view)'].join(', ')}).');
    // Again as any stream is drawn again, the others' last views kept.
    for (final j in sIdx) {
      final at = {...aNames, ...hNames, j: 'B${j + 1}'};
      final h = [
        for (final i in sIdx) i == j ? '_' : 'A${i + 1}',
        for (final i in sIdx) i == j ? '[B${j + 1} | S${i + 1}]' : 'S${i + 1}',
        for (final i in hIdx) 'H${i + 1}',
        '[${viewOf(at)} | Vs?]',
      ];
      final args = [
        for (final i in sIdx) i == j ? 'B${j + 1}?' : 'A${i + 1}?',
        for (final i in sIdx) 'S${i + 1}?',
        for (final i in hIdx) 'H${i + 1}?',
        'Vs',
      ];
      final gs = [
        for (final i in sIdx) i == j ? 'ground(B${j + 1}?)' : 'ground(A${i + 1}?)',
        ...hGuards,
      ];
      _out.writeln('$more(${h.join(', ')}) :- ${gs.join(', ')} | '
          '$more(${args.join(', ')}).');
    }
    // Done when each stream is.
    _out
      ..writeln('$more(${[
        for (final _ in sIdx) '_',
        for (final _ in sIdx) '[]',
        for (final _ in hIdx) '_',
        '[]',
      ].join(', ')}).')
      ..writeln();
    return name;
  }

  /// The view type of a stream or a list [n] the person writes whose
  /// element holds a position the program writes: the elements answered,
  /// each by its view, then input(Path) at the tail, the question still
  /// open, and [] once a list is ended.
  String _seqView(_Node n) {
    final key = 'seq:${n.key}';
    final memo = _viewMemo[key];
    if (memo != null) return memo;
    final seq = _personSeq(n)!;
    final e = n.child(seq.elem);
    final ref = gen.view('answered_${_typeStem(n.type)}');
    _viewMemo[key] = ref;
    _out.writeln('$ref ::= input(${gen.pathType})'
        '${seq.list ? ' ; []' : ''} ; [${_personView(e)} | $ref].');
    return ref;
  }

  /// ans_N(P?, C?, X, Gs?, Gs1, Vs, Done): the question X of a stream or a
  /// list [n] the person writes whose element holds a position the program
  /// writes, at P, whose grants name P, its next cell at C: drawn input(P)
  /// at once; each element answered from a grant, filled (fill_N) at C with
  /// 1 at its end, the tail at C with 2 at its end a question again; the
  /// views the elements' and the tail's, combined (scons_N); a list ended by
  /// the end button, [] its last view, and Done.
  String _answerSeqWritten(_Node n) {
    final name = _name('ans', n);
    if (!_emitted.add('ans:${n.key}')) return name;
    final seq = _personSeq(n)!;
    final e = n.child(seq.elem);
    final one = _name('ans', n, 'one');
    final take = _name('ans', n, 'take');
    final cons = _name('ans', n, 'cons');
    final consMore = _name('ans', n, 'cons_more');
    final t = n.typeKey;
    final pt = gen.pathType;
    final gs = gen.inputsType;
    final done = gen.doneType;
    final sv = _seqView(n);
    final ev = _personView(e);
    final form = _former(e);
    final fill = _fill(e);
    _rTypes.putIfAbsent(_formedKey(e), () => e);
    if (seq.list) _rListEnd = true;
    _out
      ..writeln('procedure $name($pt?, $pt?, $t, $gs?, $gs, Stream($sv), $done).')
      ..writeln('$name(P, C, X?, Gs, Gs1?, [input(P?) | Vs?], Done?) :- '
          'ground(P?) | $one(P?, C?, X, Gs?, Gs1, Vs, Done).')
      ..writeln()
      ..writeln('procedure $one($pt?, $pt?, $t, $gs?, $gs, Stream($sv), $done).');
    if (seq.list) {
      _out.writeln('$one(P, _, [], [input(_, P1, $listEnd) | Gs], Gs?, [[]], '
          'done) :- P1? =?= P? | true.');
    }
    _out
      ..writeln('$one(P, C, X?, [input(Id, P1, R) | Gs], Gs1?, Vs?, Done?) :- '
          'P1? =?= P?, ground(R?) | $form(R?, F), '
          '$take(F?, P?, C?, X, input(Id?, P1?, R?), Gs?, Gs1, Vs, Done).')
      ..writeln('$one(P, C, X?, [G | Gs], Gs1?, Vs?, Done?) :- otherwise | '
          '$take(refused, P?, C?, X, G?, Gs?, Gs1, Vs, Done).')
      ..writeln()
      ..writeln('procedure $take(${gen.formedType}(${_formedKey(e)})?, $pt?, '
          '$pt?, $t, ${gen.inputType}?, $gs?, $gs, Stream($sv), $done).')
      ..writeln('$take(formed(V), P, C, [X1? | X2?], _, Gs, Gs1?, Vs?, Done?) :- '
          'ground(C?) | ${gen.path}(C?, 1, C1), ${gen.path}(C?, 2, C2), '
          '$fill(V?, C1?, X1, Es), '
          '$name(P?, C2?, X2, Gs?, Gs1, Ts, Done), $cons(Es?, Ts?, Vs).')
      ..writeln('$take(refused, P, C, X?, G, Gs, [G? | Gs1?], Vs?, Done?) :- '
          '$one(P?, C?, X, Gs?, Gs1, Vs, Done).')
      ..writeln()
      ..writeln('procedure $cons(Stream($ev)?, Stream($sv)?, Stream($sv)).')
      ..writeln('$cons([E | Es], [T | Ts], [[E? | T?] | Vs?]) :- '
          'ground(E?), ground(T?) | $consMore(E?, T?, Es?, Ts?, Vs).')
      ..writeln('$cons([], _, []).')
      ..writeln('$cons(_, [], []).')
      ..writeln()
      ..writeln('procedure $consMore($ev?, $sv?, Stream($ev)?, Stream($sv)?, '
          'Stream($sv)).')
      ..writeln('$consMore(_, T, [E | Es], Ts, [[E? | T?] | Vs?]) :- '
          'ground(E?), ground(T?) | $consMore(E?, T?, Es?, Ts?, Vs).')
      ..writeln('$consMore(E, _, Es, [T | Ts], [[E? | T?] | Vs?]) :- '
          'ground(E?), ground(T?) | $consMore(E?, T?, Es?, Ts?, Vs).')
      ..writeln('$consMore(_, _, [], [], []).')
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
      // before any position is formed (_checkInteractive), so no construct calls a
      // former of either, and form_any is not emitted (vGLP #8 Cowork,
      // 2026-10-11 17:24 UTC, NOTED).
      final f = gen._leafFormers[leaf];
      if (f == null) {
        throw StateError('A position the person writes of type '
            '${n.typeKey}, which _checkInteractive refuses, is formed');
      }
      return f;
    }
    final name = _name('form', n);
    if (!_emitted.add('form:${n.typeKey}')) return name;
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
        // A position the program writes is not formed either: it holds
        // hole in the skeleton formed, and the answer a fresh writer there
        // (fill_N).
        final kids = _children(n, a);
        final k = kids.length;
        final own = [
          for (var i = 0; i < k; i++)
            if (!_isNestedSeq(n, kids[i]) && !_isWritten(n, kids[i])) i
        ];
        final rs = [
          for (var i = 0; i < k; i++) own.contains(i) ? 'R${i + 1}' : '_'
        ];
        if (own.isEmpty) {
          lines.add('$name(${_apply(a, rs)}, '
              'formed(${_apply(a, [for (final c in kids) _isWritten(n, c) ? 'hole' : '[]'])})).');
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
    _out.writeln('procedure$p $name($_formedMark${_formedKey(n)}$_formedMark?, '
        '$formed(${_formedKey(n)})).');
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
        if (!_isNestedSeq(n, c) && !_isWritten(n, c)) c
    ];
    final k = own.length;
    final ins = [for (final c in own) '$formed(${_formedKey(c)})?'];
    var j = 0;
    final args = [
      for (final c in kids)
        _isNestedSeq(n, c)
            ? '[]'
            : _isWritten(n, c)
                ? 'hole'
                : 'X${++j}?'
    ];
    _out
      ..writeln('procedure$p $name(${ins.join(', ')}, $formed(${_formedKey(n)})).')
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
  /// Its first view is drawn once X is written whole, every position the
  /// program writes in it written: "A construct process has the bridge draw
  /// the widget of T once every reader in the output it holds is assigned,
  /// with that output and its questions" (vGLP, Section 6.3, "The construct
  /// process"; Definition "Reduce and Communicate in vmaGLP": Communicate
  /// delivers an assignment once the term it assigns, its readers replaced
  /// by what is assigned to them, has no reader).  So present_N suspends on
  /// X until it is written, each alternative's clause waiting for the
  /// positions the program writes in it ([_presentStruct]), and the views of
  /// a structure are combined once each argument has its first; no position
  /// is drawn pending.  The grants reach the questions only once the first
  /// view is drawn (run_f/7, [_runFor]).
  String _present(_Node n, InteractiveType it) {
    final name = _name('present', n);
    if (!_emitted.add('present:${n.typeKey}')) return name;
    final seq = _seqOf(n);
    if (seq != null) {
      _presentSeq(n, seq.elem, name, it);
      return name;
    }
    if (_streamElement(n) != null) {
      // A type of [] and [E | S] that is not a Stream or a List is refused
      // before it is presented, a list cell outside a Stream or a List
      // (_checkInteractive).
      throw StateError('present_N meets ${n.typeKey}, which '
          '_checkInteractive refuses');
    }
    final t = n.typeKey;
    final p = _declParams(n);
    final done = gen.doneType;
    final gs = gen.inputsType;
    final clauses = <String>[];
    final later = <void Function()>[];
    for (final a in _nodeAlts(n)) {
      if (a is _ConstAlt) {
        final c = constantSource(a.value);
        clauses.add('$name(_, $c, Gs, Gs?, [$c], done).');
      } else if (a is _NilAlt) {
        clauses.add('$name(_, [], Gs, Gs?, [[]], done).');
      } else if (a is _LeafAlt) {
        final g = _guard(a.leaf);
        clauses.add('$name(_, X, Gs, Gs?, [X?], done) :- $g(X?) | true.');
      } else {
        clauses.add(_presentStruct(n, a, name, later, it));
      }
    }
    final view = _presentView(n);
    _out.writeln('procedure$p $name(${gen.pathType}?, $t?, '
        '$gs?, $gs, Stream($view), $done).');
    for (final c in clauses) {
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
  /// questions").
  ///
  /// The element of a list cell at position P is at P with 1 at its end, and
  /// the cell's tail at P with 2 at its end, the path of argument indices
  /// from the root of the interactive variable's term ('.'(H, T), GLP-Spec
  /// appendix-lp.tex, Definition "Logic Programs Syntax"): so the k-th
  /// element is at P with k - 1 2s and then 1 at its end, and its questions
  /// below it.  Each
  /// element is presented at its own position --- by its present_N where
  /// the program writes it, by its answer_N where the element is itself a
  /// question --- and its questions take their grants as any question does,
  /// the grants an element does not take passed on to those after it.  The
  /// views are the elements' views, in the list's order, again as an
  /// element is drawn again (cons_views/3 in programs/vglp/views.glp).  It
  /// is drawn once written whole, closed and each element written whole:
  /// "A construct process has the bridge draw the widget of T once every
  /// reader in the output it holds is assigned" (vGLP, Section 6.3, "The
  /// construct process"), and an unwritten tail is such a reader; only the
  /// output, construct 0, "is drawn element by element as it arrives, its
  /// unwritten tail not drawn" (ibid.).  So the first view of the list is
  /// formed once its [] is, the tail's first view, and each element's first.
  /// Done once every element's questions are answered.
  void _presentSeq(_Node n, TypeExpr elem, String name, InteractiveType it) {
    final e = n.child(elem);
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
    // The views are lists of the elements' views.
    final vs = 'Stream(${_presentView(n)})';
    _out
      ..writeln('procedure$p $name($pt?, $t?, $gs?, $gs, $vs, $done).')
      ..writeln('$name(P, [$inCell | Xs], Gs, Gs1?, Vs?, Done?) :- '
          '$cell(P?, $toCell, Xs?, Gs?, Gs1, Vs, Done).')
      ..writeln('$name(_, [], Gs, Gs?, [[]], done).')
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

  /// present_N's clause of the structure alternative [a] of node [n], drawn
  /// once written whole ([_present]): the clause waits, ground(H?), for each
  /// argument the program writes that holds no question, and a structure
  /// holding no question is one value, drawn as it is written.
  String _presentStruct(_Node n, _Alt a, String name,
      List<void Function()> later, InteractiveType it) {
    final kids = _children(n, a);
    if (!kids.any(_hasQuestion)) {
      final hs = [for (var i = 0; i < kids.length; i++) 'H${i + 1}'];
      final ground = [for (final h in hs) 'ground($h?)'].join(', ');
      return '$name(_, ${_apply(a, hs)}, Gs, Gs?, '
          '[${_apply(a, [for (final h in hs) '$h?'])}], done)'
          '${hs.isEmpty ? '' : ' :- $ground | true'}.';
    }
    final head = <String>[];
    final views = <String>[];
    // The position of the argument i of this alternative is the node's own,
    // P, with i at its end: a question's is read twice, for its mark in the
    // view and for its answer_N, and a position the program writes holding a
    // question once, for its present_N; one the program writes holding no
    // question is drawn as it is written, its position not read.  P is
    // ground, so where it is read more than once the clause guards on it.
    final acc = _Arguments();
    // A structure of questions, none holding a stream or a list, and of
    // positions the program writes holding none, has one view; any other is
    // drawn again as its arguments are.
    final direct = kids.every((k) => k.person
        ? !_holdsNested(k)
        : !_hasQuestion(k));
    for (var i = 0; i < kids.length; i++) {
      final k = kids[i];
      final h = 'H${i + 1}';
      final idx = i + 1;
      if (k.person) {
        _checkPersonWritable(k, it);
        head.add('$h?');
        final gsOut = acc.nextGs();
        final d = 'D${i + 1}';
        final m = 'M$idx', q = 'Q$idx';
        // A question holding a stream or a list inside a form the person
        // writes is drawn again once answered, that one open under it.
        final nested = _holdsNested(k);
        final w = 'W$idx';
        acc.goals
          ..add('${gen.path}(P?, $idx, $m)')
          ..add('${gen.path}(P?, $idx, $q)')
          ..add('${_answer(k)}($q?, $h, ${acc.gsIn}?, $gsOut'
              '${nested ? ', $w' : ''}, $d)');
        acc.pathReads += 2;
        acc.gsIn = gsOut;
        acc.dones.add(d);
        views.add(direct
            ? 'input($m?)'
            : nested
                ? '[input($m?) | $w?]'
                : '[input($m?)]');
      } else if (!_hasQuestion(k)) {
        // A field the program writes holding no question, a stream or a
        // list among them: drawn as it is written, once written whole.
        head.add(h);
        acc.guards.add('ground($h?)');
        views.add(direct ? '$h?' : '[$h?]');
      } else {
        head.add(h);
        final gsOut = acc.nextGs();
        final d = 'D${i + 1}';
        final q = 'Q$idx';
        acc.goals
          ..add('${gen.path}(P?, $idx, $q)')
          ..add('${_present(k, it)}($q?, $h?, ${acc.gsIn}?, $gsOut, '
              'V${i + 1}, $d)');
        acc.pathReads += 1;
        acc.gsIn = gsOut;
        acc.dones.add(d);
        views.add('V${i + 1}?');
      }
    }
    final pattern = _apply(a, head);
    final gsHead = acc.gsIn == 'Gs' ? 'Gs?' : '${acc.gsIn}?';
    final doneHead = acc.dones.isEmpty ? 'done' : acc.doneHead(gen.allDone);
    String vsHead;
    if (direct) {
      vsHead = '[${_apply(a, views)}]';
    } else {
      final start = _combiner(n, a, kids);
      acc.goals.add('$start(${views.join(', ')}, Vs)');
      vsHead = 'Vs?';
    }
    final g0 = acc.guardPrefix();
    final body = acc.goals.isEmpty
        ? (acc.guards.isEmpty ? '' : ' :- $g0' 'true')
        : ' :- $g0${acc.goals.join(', ')}';
    // An alternative holding a question reads its position, for the
    // question's mark or for the present_N of the argument holding it.
    final at = acc.pathReads == 0 ? '_' : 'P';
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
    final views = [for (final c in kids) _childView(c)];
    final streams = [for (final v in views) 'Stream($v)?'].join(', ');
    final out = 'Stream(${_presentView(n)})';
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
        ..writeln('procedure$p $start($streams, $out).')
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
      ..writeln('procedure$p $start($streams, $out).')
      ..writeln('$start(${[for (var i = 0; i < k; i++) '[${vs[i]} | ${ss[i]}]'].join(', ')}, '
          '[${_apply(a, vr)} | Vs?]) :- $ground | '
          '$comb(${vr.join(', ')}, ${sr.join(', ')}, Vs).');
    closed.forEach(_out.writeln);
    _out
      ..writeln()
      ..writeln('procedure$p $comb(${[for (final v in views) '$v?'].join(', ')}, '
          '$streams, $out).');
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

  /// The widget of node [n]: its default D (Definition "Widget Declaration,
  /// Default Widget"), as the term vGLP's Q3 names (2026-10-01 23:55 UTC, E);
  /// and where a declaration in scope names a widget W for its moded type,
  /// `named(W, D)`, the declared name with the default widget of the type,
  /// from which the named widget reads the type's constructors (vGLP
  /// 92e6d3b, Section 6.2, the paragraph after Proposition "Default
  /// Widgets": "It is drawn with the default widget of its type beside its
  /// name, from which it reads the type's constructors"; Section 6.3: "for a
  /// declared widget, the term named(W, D) of the name W its declaration
  /// gives and the default widget D of its type"; the construct process in
  /// the appendix of proofs: "named(W', D) where a declaration names it W',
  /// D the default widget of T").  So at every position, the question's
  /// root, a field, an element, and the program's output alike.
  ///
  /// A recursive type's widget is finite by a reference back to itself
  /// (RECURSIVE TYPES above): met again within its own widget, [n]'s
  /// default is `ref(T)`, T its type's name, and the default widget of a
  /// recursive type is `rec(T, W)`, binding T to its widget W around every
  /// `ref(T)` in it; so a form's field of a recursive type is told by its
  /// widget, "drawn when the person opens it" (Definition "Default Widget").
  /// A declared recursive type is `named(W, rec(T, ...))`, each occurrence
  /// of it within `named(W, ref(T))`, every widget of the type named.
  ///
  /// A declaration's labels hold within its widget: each constructor of its
  /// type, and of the types under it, that the declaration labels is drawn
  /// as `label(S, W)`, W its widget and S its label, a constant in a row of
  /// buttons `label(S, C)`, and a field the declaration names is entered as
  /// `field(S, W)`, S its name in place of the name of its declared type,
  /// which labels a field "where it is given none" (vGLP 1126f4f, Section 6.2, "The words a
  /// platform shows": "a widget term carries a label beside each
  /// constructor of Definition Default Widget.  A construct family draws the
  /// label it is given, and the name of the type where it is given none").
  String _widget(_Node n, Set<String> visiting) {
    final name = constantSource(n.typeKey);
    final named = declared[n.moded];
    final dw = named == null ? null : DeclaredWidget.parse(named);
    final outer = _labels;
    if (dw != null) _labels = dw;
    final String d;
    try {
      if (visiting.contains(n.key)) {
        d = 'ref($name)';
      } else {
        visiting.add(n.key);
        try {
          final w = _defaultWidget(n, visiting);
          d = _recursive(n) ? 'rec($name, $w)' : w;
        } finally {
          visiting.remove(n.key);
        }
      }
    } finally {
      _labels = outer;
    }
    return dw == null ? d : 'named(${constantSource(dw.drawnAs)}, $d)';
  }

  /// [w], the widget of the constructor [f], with the label the declaration
  /// in force gives [f], `label(S, W)`; [w] itself where it gives none.
  String _labelled(String f, String w) {
    final l = _labels?.ofConstructor(f);
    return l == null ? w : 'label(${constantSource(l)}, $w)';
  }

  /// The name of the field at the place [i] of the constructor [f]: the
  /// name the declaration in force gives it, or [type], the name of its
  /// declared type, where it gives none (Definition "Default Widget": "one
  /// field per argument, each labelled by the name of its declared type").
  String _fieldName(String f, int i, String type) =>
      _labels?.ofPlace(f, i) ?? type;

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
      if (alts.length == 1) return _altWidget(n, alts.single, visiting);
      final cs = [
        for (final a in alts)
          _labelled(_constKey((a as _ConstAlt).value),
              constantSource(a.value))
      ];
      return 'buttons([${cs.join(', ')}])';
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
      return n.person
          ? _labelled(_constKey(a.value), 'button(${constantSource(a.value)})')
          : 'shown';
    }
    if (a is _NilAlt) return n.person ? 'button([])' : 'shown';
    if (a is _LeafAlt) return n.person ? _leafWidget(a.leaf) : 'shown';
    if (a is _ConsAlt) {
      // A list cell outside a Stream or a List is refused, not an
      // interactive type (_checkInteractive): no widget is form('.', ...).
      throw StateError('The widget of ${n.moded} meets a list cell, which '
          '_checkInteractive refuses');
    }
    final kids = _children(n, a);
    final f = (a as _StructAlt).functor;
    final ws = [
      for (var i = 0; i < kids.length; i++)
        '${_isNestedSeq(n, kids[i]) ? 'question' : 'field'}'
            '(${constantSource(_fieldName(f, i + 1, kids[i].typeKey))}, '
            '${_widget(kids[i], visiting)})'
    ].join(', ');
    return _labelled(f, 'form(${constantSource(f)}, [$ws])');
  }

  /// The key of a constant among a declaration's labels: the constant as
  /// the declaration writes it, a string as itself.
  static String _constKey(Object? c) => c is String ? c : '$c';

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
        // drawn (_checkInteractive; vGLP #8 Cowork, 2026-10-11 15:47 UTC,
        // item 3).
        throw StateError('The type _ where the person writes, which '
            '_checkInteractive refuses');
      case _Leaf.param:
        // "it has no type parameter" (vGLP 2f1dc92, Definition "Volitional
        // Procedure, Interactive Type"): refused before its widget is drawn
        // (_checkInteractive).
        throw StateError('A type parameter where the person writes, which '
            '_checkInteractive refuses');
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
