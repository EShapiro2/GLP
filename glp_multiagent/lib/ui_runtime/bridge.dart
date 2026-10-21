/// The Dart bridge of vGLP (`shapiro2026volition`, sections/elicitation.tex,
/// subsection "The Implementation: Compiling vGLP onto GLP", the paragraph
/// "The implementation"): "the bridge draws each construct's widget and
/// returns the person's raw input as grants".
///
/// A compiled module runs by the goals of Section "The Grassroots App",
/// `M # agent(MCh?, NetCh?, Asks), M # dispatch(Asks?, PersonCh?, MCh)`,
/// with the person channel `PersonCh = ch(Grants?, Draws)` (Definition
/// "Person Channel, Person Writer, GLP with Persons, Grant"): the person
/// holds the grant writer and reads the draws.  The host hands this bridge
/// each term of `Draws` as it arrives and extends `Grants` with each grant it
/// returns.  What crosses is the vocabulary of the compiled dispatcher
/// (`programs/vglp/dispatcher.glp`):
///
///   - `draw(Id, W, f(V))`: draw construct `Id` with the widget `W` names in
///     the construct family in use --- a default widget, or `named(W', D)`
///     where a declaration names it `W'`, `D` the default widget of its type
///     ([namedCtor]) --- over the view `V`, under the functor `f`
///     of its ask, construct 0's under `output`, each position the
///     person writes marked `input(P)`, `P` its path, and each widget
///     position the program has not yet written whole marked `pending(P)`,
///     a stream shown as its elements so far, its unwritten tail not shown
///     (vGLP, Definition "Construct Process"); drawn again as more of the
///     view arrives;
///   - `withdraw(Id)`: remove it, every question it holds being answered;
///   - `input(Id, P, R)`: the grant of the person's raw input `R`, a ground
///     term, to the question at position `P` of construct `Id`.
///
/// "Everything crossing to Dart is a ground term or a construct identifier,
/// and answers and channels stay on the GLP side": the bridge forms no answer
/// and checks no type --- the construct process forms the answer from `R`, a
/// grant forming no term of the question's type answering nothing --- and it
/// removes a construct only on its withdrawal.  Any other term on `Draws` is
/// the program's own output to the person, which the dispatcher merges with
/// the draws, and is not the bridge's: [UiRuntime] shows it as it shows every
/// screen message.
///
/// This file holds the bridge's state and its vocabulary and no Flutter; the
/// construct family that draws the widgets is `construct_family.dart`.
library;

import 'term.dart';

/// The constructors of the person channel of a compiled module, its draws
/// and its grants (the root's `Draw(T) ::= draw(Integer, Widget, T) ;
/// withdraw(Integer).` and `Input(T) ::= input(Integer, Path, T).`,
/// `programs/self.glp`).
const String drawCtor = 'draw';
const String withdrawCtor = 'withdraw';
const String inputCtor = 'input';

/// The mark of a widget position the program has not yet written whole in a
/// view, `pending(P)`, `P` its path, beside `input(P)`: "each widget position
/// the program has not yet written whole --- the root, a field, an element of
/// any stream, the thread's included --- drawn as pending ... so that the
/// questions beside it are offered at once" (vGLP 118e04d, Definition
/// "Construct Process"; `programs/vglp/dispatcher.glp`, shown/3).
const String pendingCtor = 'pending';

/// The raw input `R` the end button of a list the person writes grants,
/// `input(Id, P, [end])`: "a list the person writes: the same, with a button
/// that ends the list" (vGLP, Definition "Widget Declaration, Default
/// Widget"), its grant a term of its own that no element can be (vGLP #7
/// Cowork, 2026-10-11 14:31 UTC, item 3).  A list cell, which no element of
/// an interactive type is --- a list cell occurs in one only as a Stream's
/// or a List's, never under another (vGLP, Definition "Volitional
/// Procedure, Interactive Type") --- where `[]`, a constant, may be one; the
/// construct process's answer clause takes the same
/// (`glp_runtime/lib/vglp/constructs.dart`, `listEnd`).
const GTerm listEnd = GList([GAtom('end')]);

/// The widget of a recursive type, finite by a reference back to itself:
/// `rec(T, W)` binds the name `T` of the type to its widget `W`, within
/// which `ref(T)` is `T` again.  "A named type in it is recursive only if it
/// has no mode inversion" (vGLP 092c9fe, Definition "Volitional Procedure,
/// Interactive Type"), so a recursive type is written whole by the program
/// or whole by the person, and the widget of a form's field of one is "a
/// field of a recursive type drawn when the person opens it" (Definition
/// "Default Widget"): the construct family draws such a field closed, with
/// the act that opens it, so that the widget unfolds only as far as the
/// person's answer goes and every finite answer is formed by finitely many
/// acts (construct_family.dart).  The compiler emits both
/// (`glp_runtime/lib/vglp/constructs.dart`, `_widget`).
const String recCtor = 'rec';
const String refCtor = 'ref';

/// The widget of a declared widget, `named(W, D)`: the name `W` the
/// widget declaration `T =::= W` gives and the default widget `D` of `T`,
/// "drawn with the default widget of its type beside its name, from which it
/// reads the type's constructors" (vGLP 92e6d3b, Section 6.2, the paragraph
/// after Proposition "Default Widgets"; Section 6.3: "for a declared widget,
/// the term named(W, D) of the name W its declaration gives and the default
/// widget D of its type").  The compiler emits it wherever a declaration in
/// scope names the widget of a position's moded type
/// (`glp_runtime/lib/vglp/constructs.dart`, `_widget`).
const String namedCtor = 'named';

/// The name `W` and the default widget `D` of [w], where it is
/// `named(W, D)`; null for any other widget.
(String, GTerm)? namedWidget(GTerm w) {
  final (k, ws) = ctorArgs(w);
  if (k != namedCtor || ws.length != 2) return null;
  return (_recName(ws[0]), ws[1]);
}

/// Whether [w] is the widget of a recursive type, `rec(T, W)` or `ref(T)`,
/// or a declared widget of one, `named(W, D)` with `D` either: a field of a
/// form drawn by it is drawn when the person opens it.
bool recursiveWidget(GTerm w) {
  final (k, ws) = ctorArgs(w);
  if (k == namedCtor && ws.length == 2) return recursiveWidget(ws[1]);
  return (k == recCtor && ws.length == 2) || (k == refCtor && ws.length == 1);
}

/// [w] with the bindings and references of recursive types resolved at its
/// top: `rec(T, W)` is `W`, with `T` bound to `W` in the bindings returned,
/// which hold within `W`; `ref(T)` is the widget [recs] binds `T` to; any
/// other widget is itself, with [recs].  Null where a `ref(T)` names a type
/// no `rec` around it binds, which the compilation does not emit.
(GTerm, Map<String, GTerm>)? unfoldWidget(GTerm w, Map<String, GTerm> recs) {
  var cur = w;
  var env = recs;
  // A binding is a default widget and never a reference at its top, so the
  // resolution ends; the bound stops a term the compilation does not emit.
  for (var step = 0; step < 64; step++) {
    final (k, ws) = ctorArgs(cur);
    if (k == recCtor && ws.length == 2) {
      env = {...env, _recName(ws[0]): ws[1]};
      cur = ws[1];
    } else if (k == refCtor && ws.length == 1) {
      final bound = env[_recName(ws[0])];
      if (bound == null) return null;
      cur = bound;
    } else {
      return (cur, env);
    }
  }
  return null;
}

/// The name `T` of `rec(T, W)` and `ref(T)`, and `W` of `named(W, D)`, the
/// constant the compiler writes the name as.
String _recName(GTerm t) => switch (t) {
      GAtom(:final name) => name,
      GString(:final value) => value,
      _ => formatTerm(t),
    };

/// A construct on screen: its identifier, the widget its latest draw names,
/// and the view that draw carries.
class OpenConstruct {
  final int id;
  GTerm widget;
  GTerm view;
  OpenConstruct(this.id, this.widget, this.view);
}

/// The bridge of one person channel: the constructs open on it, in the order
/// they were first drawn, and the grants the person makes on them.
class Bridge {
  /// Where a grant goes: onto the person channel's grants, `Gs`.
  final void Function(GTerm grant) onGrant;

  final Map<int, OpenConstruct> _open = {};

  Bridge({required this.onGrant});

  /// The open constructs, in the order they were first drawn.
  Iterable<OpenConstruct> get constructs => _open.values;

  /// The open construct [id], or null.
  OpenConstruct? construct(int id) => _open[id];

  /// Read one term of the person channel's draws.  A `draw(Id, W, V)` opens
  /// construct `Id`, or redraws it where it is open; a `withdraw(Id)`
  /// removes it.  Returns whether [t] was one of the two, every other term
  /// being the program's output to the person.
  bool read(GTerm t) {
    final (ctor, args) = ctorArgs(t);
    if (ctor == drawCtor && args.length == 3 && args[0] is GInt) {
      final id = (args[0] as GInt).value;
      final view = drawnView(args[2]);
      final open = _open[id];
      if (open == null) {
        _open[id] = OpenConstruct(id, args[1], view);
      } else {
        open
          ..widget = args[1]
          ..view = view;
      }
      return true;
    }
    if (ctor == withdrawCtor && args.length == 1 && args[0] is GInt) {
      _open.remove((args[0] as GInt).value);
      return true;
    }
    return false;
  }

  /// The person's raw input [raw] at the question at position [path] of
  /// construct [id], granted as `input(Id, P, R)`.  A construct no longer
  /// open takes no grant: its widget is gone from the screen.
  void grant(int id, GTerm path, GTerm raw) {
    if (!_open.containsKey(id)) return;
    onGrant(GStruct(inputCtor, [GInt(id), path, raw]));
  }
}

/// The view a draw carries, `V` of `draw(Id, W, f(V))`: every drawn value is
/// under the functor of its construct's ask, construct 0's under `output`,
/// so that what the person channel carries is of one type, the program's T
/// (vGLP, Definition "Construct Process": "with V under the functor t of its
/// ask; the output thread's value is under the functor output").  An ask's
/// functor is the name of its interactive type in lowercase, its type
/// arguments appended, with `_w` for writer mode and `_r` for reader mode
/// (vGLP, Definition "Canonical Compilation", the asking clause), so the
/// wrapper is told by its name; a value under no such functor is the view
/// itself.
GTerm drawnView(GTerm drawn) => drawn is GStruct &&
        drawn.args.length == 1 &&
        (drawn.functor == outputCtor ||
            drawn.functor.endsWith('_w') ||
            drawn.functor.endsWith('_r'))
    ? drawn.args.single
    : drawn;

/// The functor of the program's output thread's value, construct 0's.
const String outputCtor = 'output';

/// The position `P` of the question a view marks, where [view] is `input(P)`
/// and `P` a path of argument indices; null for any other view, a position
/// the program writes.
GList? questionAt(GTerm view) {
  if (view is! GStruct || view.functor != inputCtor || view.args.length != 1) {
    return null;
  }
  final p = view.args.single;
  if (p is! GList || !p.items.every((i) => i is GInt)) return null;
  return p;
}

/// The position `P` of the widget position a view marks pending, where
/// [view] is `pending(P)` and `P` a path of argument indices; null for any
/// other view.
GList? pendingAt(GTerm view) {
  if (view is! GStruct ||
      view.functor != pendingCtor ||
      view.args.length != 1) {
    return null;
  }
  final p = view.args.single;
  if (p is! GList || !p.items.every((i) => i is GInt)) return null;
  return p;
}
