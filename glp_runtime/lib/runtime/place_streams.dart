/// The declared places of one agent and their event streams: the runtime's
/// bookkeeping behind `place_declare/3` and `place_remove/1`, layer or none.
///
/// IGLP madglp-spec.tex, "A place event is bridged below the madGLP layer":
/// "the runtime assigns the tail writer of the named place's stream ... The
/// stream is fed serializer-fashion ...: the runtime retains the new tail
/// rather than closing the stream, so one declaration yields one stream however
/// many entries and exits follow.  The stream is closed, its tail assigned
/// `[]`, in exactly two cases: `place_remove`, and a further declaration of
/// the same place, which supersedes the earlier one.  An event for a place
/// removed or superseded is dropped."  GLP-Spec appendix-guards.tex,
/// "Networking seam": "A further declaration of the same place supersedes the
/// earlier one and closes its stream.  `place_remove(P)` ends the declaration
/// of `P`, and does nothing where `P` is not declared."
///
/// Closing a place's stream is the runtime's and not the layer's, so it holds
/// where no layer is bound as where one is: with none, `place_remove` and a
/// further declaration close the stream as ever, and only the reporting is
/// absent (GLP, 2026-10-11 12:59 UTC, "23:12" (a); IGLP
/// appendix-implementation-notes.tex, "One interface").  Until 2026-10-12 the
/// bookkeeping was MadContext's alone, kept where a layer was bound, and a
/// stream declared with none was never closed.
library;

import 'heap_fcp.dart' show HeapCell;
import 'machine_state.dart' show GoalRef;
import 'runtime.dart';
import 'saved_state.dart';
import 'terms.dart';

class PlaceStreams {
  PlaceStreams(this._rt);

  final GlpRuntime _rt;

  /// The current tail writer of each declared place's event stream, by place
  /// name.  A place is here exactly while its declaration stands.
  final Map<String, HeapCell> _tails = {};

  /// Which declaration of each place is the standing one, incremented on every
  /// [declare], so an answer the layer returns late can be told from the
  /// declaration it answers: the tail writer moves with every event and cannot
  /// identify a declaration, this does.
  final Map<String, int> _declarations = {};

  /// Whether [place]'s declaration stands.
  bool isDeclared(String place) => _tails.containsKey(place);

  /// The places whose declarations stand.
  Iterable<String> get places => _tails.keys;

  /// Whether [declaration], as [declare] numbered it, is [place]'s standing one.
  bool isStanding(String place, int declaration) =>
      _tails.containsKey(place) && _declarations[place] == declaration;

  /// Declare [place], its stream growing from [streamWriter], the writer of
  /// the `E` of `place_declare(P, R, E)`; a declaration of [place] already
  /// standing is superseded and its stream closed.  Returns the number of this
  /// declaration, for [isStanding].
  int declare(String place, HeapCell streamWriter) {
    close(place);
    _tails[place] = streamWriter;
    final declaration = (_declarations[place] ?? 0) + 1;
    _declarations[place] = declaration;
    return declaration;
  }

  /// Deliver [event] on [place]'s stream, serializer-fashion: bind the current
  /// tail writer to `[event | E'?]`, retain the fresh writer as the new tail,
  /// and reactivate whatever suspended on the stream.  Returns false, having
  /// done nothing, where [place] is not declared: its event is dropped.
  bool deliver(String place, Term event) {
    final current = _tails[place];
    if (current == null) return false;
    final (freshWriter, freshReader) = _rt.heap.allocateVariable();
    _tails[place] = freshWriter;
    _wake(_rt.heap.bindVariable(
        current, StructTerm('.', [event, VarRef(freshReader)])));
    return true;
  }

  /// Close [place]'s stream, its tail assigned `[]`, and end its declaration.
  /// Returns false, having done nothing, where [place] is not declared.
  bool close(String place) {
    final writer = _tails.remove(place);
    if (writer == null) return false;
    _wake(_rt.heap.bindVariable(writer, ConstTerm(nil)));
    return true;
  }

  /// Write the places declared, each with its stream's tail writer, and the
  /// number of each place's standing declaration (IGLP, Implementation Notes,
  /// "Persistence": "the places ... declared").
  void saveState(StateWriter w) {
    w.uint(_tails.length);
    for (final MapEntry(key: place, value: tail) in _tails.entries) {
      w.str(place);
      w.cell(tail);
    }
    w.uint(_declarations.length);
    for (final MapEntry(key: place, value: n) in _declarations.entries) {
      w.str(place);
      w.uint(n);
    }
  }

  /// Read back what [saveState] wrote.
  void restoreState(StateReader r) {
    _tails.clear();
    _declarations.clear();
    for (var i = r.uint(); i > 0; i--) {
      final place = r.str();
      _tails[place] = r.cell();
    }
    for (var i = r.uint(); i > 0; i--) {
      final place = r.str();
      _declarations[place] = r.uint();
    }
  }

  void _wake(List<GoalRef> activations) {
    for (final act in activations) {
      _rt.enqueueReactivatedGoal(act);
    }
  }
}
