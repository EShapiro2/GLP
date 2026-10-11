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
///   - `draw(Id, W, V)`: draw construct `Id` with the widget `W` names in
///     the construct family in use, over the view `V`, each position the
///     person writes marked `input(P)`, `P` its path; drawn again as more of
///     the view arrives;
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
/// and its grants (`programs/vglp/dispatcher.glp`: `Draw ::= draw(Integer, _,
/// _) ; withdraw(Integer).`, `Input ::= input(_, _, _).`).
const String drawCtor = 'draw';
const String withdrawCtor = 'withdraw';
const String inputCtor = 'input';

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
      final open = _open[id];
      if (open == null) {
        _open[id] = OpenConstruct(id, args[1], args[2]);
      } else {
        open
          ..widget = args[1]
          ..view = args[2];
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
