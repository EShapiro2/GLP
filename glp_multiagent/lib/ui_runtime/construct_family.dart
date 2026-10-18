/// The construct family the bridge draws with (vGLP, sections/elicitation.tex,
/// Remark "Construct Families" and Definition "Widget Declaration, Default
/// Widget").
///
/// A draw `draw(Id, W, V)` names its widget `W` and carries its view `V`, each
/// position the person writes marked `input(P)`, each widget position the
/// program has not yet written whole marked `pending(P)`, and a stream the
/// program writes its elements so far, its unwritten tail not drawn
/// (bridge.dart).  The family draws a pending position by the widget of its
/// type, as a placeholder that takes no input, and the questions beside it
/// as ever, offered at once (vGLP 118e04d, Definition "Construct Process":
/// "each widget position the program has not yet written whole --- the
/// root, a field, an element of any stream, the thread's included --- drawn
/// as pending, and a stream's unwritten tail not drawn, so that the
/// questions beside it are offered at once").  "A question's
/// type fixes a construct's semantic content --- what it shows, what it
/// takes, and the answer each submission forms --- and nothing of its
/// layout": the family decides what each widget looks like, and this file is
/// the grassroots app's.  Its widgets are the Definition's default widgets,
/// by the terms the compiled construct processes name them with:
///
///   - a constant: a button, `button(C)`; a union of constants: a row of
///     buttons, `buttons([C1, ..., Cn])`; a position of type `Constant` or
///     `Module`: a text field, `text` (an interactive type has "no type
///     parameter at a position the person writes", vGLP, Definition
///     "Volitional Procedure, Interactive Type");
///   - `String`, a number (`Integer` or `Real`), `Date` and an agent
///     identifier: a text field, a number field, a date picker and a peer
///     field, `text`, `number`, `date` and `peer`;
///   - a tuple: a form headed by its functor, with one field per argument,
///     each labelled by the name of its declared type, `form(F, [E1, ...,
///     En])`, `F` the heading, in both modes (vGLP f9b63d6), each `Ei`
///     `field(L, W)`, `L` that name; a stream or list inside a form the
///     person writes: submitted as a question of its own and drawn under the
///     form, its entry `question(L, W)` and no field of the form; a union of
///     tuples: a menu of forms, `menu([W1, ..., Wn])`, each alternative's
///     chip naming its functor, and its arity where two share a functor
///     (Remark "Construct Families");
///   - a value the program writes: shown and not edited, `shown` where it is
///     a constant or a primitive, and otherwise by the widget its type's
///     constructors build --- a tuple a form of shown fields, each labelled
///     by the name of its declared type, a tuple inside it a form of its own
///     set in under a rule, a union a menu, a stream `thread(W)` where its
///     elements are tuples (vGLP #8 Cowork, 2026-10-13 08:15 UTC); a list the
///     program writes, with a choice coming back: a picker, `picker`;
///   - a stream the person writes: the widget of its element type, kept
///     open, one element per submission, `input_box(W)`; a list the person
///     writes: the same, with a button that ends the list, `input_list(W)`,
///     the button granting [listEnd], `[end]`; a stream the program writes:
///     a thread, `thread`, and one whose elements hold questions,
///     `thread(W)`, each element drawn as it arrives by `W`, the widget of
///     its type, each question in it by the field of its position (vGLP #8
///     Cowork, 2026-10-11 15:47 UTC, item 2);
///   - a recursive type, "a named type in it is recursive only if it has no
///     mode inversion" (vGLP 092c9fe, Definition "Volitional Procedure,
///     Interactive Type"), so written whole by the program or whole by the
///     person: its widget is finite by a reference back to itself, `rec(T,
///     W)` binding the type's name `T` to its widget `W`, within which
///     `ref(T)` is `T` again (bridge.dart, [recCtor], [refCtor],
///     [unfoldWidget]); a form's field drawn by either is "a field of a
///     recursive type drawn when the person opens it" (Definition "Default
///     Widget"), drawn closed, with the button that opens it, in the
///     person's form and in the program's alike, so that the widget unfolds
///     only as far as the person goes and every finite answer is formed by
///     finitely many acts; at the root of a question, or as the element of
///     a stream or a list, `rec(T, W)` is drawn at once as `W`.
///
/// A widget a declaration names (`T =::= W`) is a constant naming a Dart
/// widget: "In the grassroots app, W names a Dart widget, a small
/// illustrative mapping" --- [ConstructFamily.named].  A widget the family
/// does not have is drawn as such, named, and takes no input: nothing stands
/// in for it.
///
/// The person's input is returned raw: a button grants its constant, a text
/// field a `String`, a number field an `Integer` or a `Real` as typed, a date
/// picker the `Integer` of the local date it picks (`Date` is "a named type
/// over `Integer`, the local date an agent counts"), a peer field the
/// identifier typed, a form the tuple of its fields --- `[]` at a question
/// of its own, which the form's answer leaves open --- a menu the
/// alternative chosen, and a list's end button [listEnd], `[end]`.  The
/// construct process forms the answer from it, and refuses one that forms no
/// term of the question's type; the family checks no type.
library;

import 'package:flutter/material.dart';

import 'bridge.dart';
import 'term.dart';

/// A widget a declaration names: drawn over the construct's view at position
/// [at], its input granted through [grant] at the question's path.
typedef NamedWidgetBuilder = Widget Function(BuildContext context,
    OpenConstruct construct, GTerm view, List<int> at,
    void Function(GTerm path, GTerm raw) grant);

/// A construct family: the default widgets, drawn below, and the named
/// widgets a declaration may choose among.
class ConstructFamily {
  /// The Dart widget each declared widget name names.
  final Map<String, NamedWidgetBuilder> named;

  const ConstructFamily({this.named = const {}});

  /// The grassroots app's family: the default widgets, and no named one.
  static const ConstructFamily standard = ConstructFamily();
}

/// The key of construct [id] on screen.
ValueKey<String> constructKey(int id) => ValueKey('construct:$id');

/// The key of the [kind] widget of construct [id] at position [at], the
/// path of argument indices from the root of the construct's term.
ValueKey<String> nodeKey(int id, String kind, List<int> at) =>
    ValueKey('$id:$kind:${pathText(at)}');

/// The key of the submit button of the question at [at] of construct [id].
ValueKey<String> submitKey(int id, List<int> at) =>
    ValueKey('$id:submit:${pathText(at)}');

/// The key of the button that ends the list at [at] of construct [id].
ValueKey<String> endKey(int id, List<int> at) =>
    ValueKey('$id:end:${pathText(at)}');

/// The key of the button that opens the field of a recursive type at [at]
/// of construct [id], drawn closed until the person opens it.
ValueKey<String> openKey(int id, List<int> at) =>
    ValueKey('$id:open:${pathText(at)}');

/// A path as the boundary writes it, `[]`, `[2]`, `[1, 2]`.
String pathText(List<int> at) => '[${at.join(', ')}]';

/// The position of element [k], from 0, of the list at [at]: a list cell is
/// `'.'(H, T)`, its element at its argument 1 and its tail at its argument 2,
/// so [at] with 2 for each element before it and then 1 (vGLP, Definition
/// "Construct Process": each question "at its path from the root of the
/// interactive variable's term"; `glp_runtime/lib/vglp/constructs.dart`,
/// `_presentSeq`).
List<int> elementAt(List<int> at, int k) =>
    [...at, for (var j = 0; j < k; j++) 2, 1];

/// One construct, drawn by [family] from its widget and view.
class ConstructView extends StatefulWidget {
  final OpenConstruct construct;
  final ConstructFamily family;

  /// The person's raw input at the question at a path of the construct.
  final void Function(GTerm path, GTerm raw) grant;

  const ConstructView({
    super.key,
    required this.construct,
    required this.grant,
    this.family = ConstructFamily.standard,
  });

  @override
  State<ConstructView> createState() => _ConstructViewState();
}

class _ConstructViewState extends State<ConstructView> {
  /// The text of each text, number and peer field, by position.
  final Map<String, TextEditingController> _texts = {};

  /// The choice made in each row of buttons and each menu inside a form,
  /// by position.
  final Map<String, GTerm> _chosen = {};

  /// The alternative chosen in each menu, by position.
  final Map<String, int> _alternative = {};

  /// The local date each date picker is at, by position.
  final Map<String, int> _days = {};

  /// The fields of a recursive type the person has opened, by position
  /// (bridge.dart, [recursiveWidget]).
  final Set<String> _opened = {};

  int get _id => widget.construct.id;

  @override
  void dispose() {
    for (final c in _texts.values) {
      c.dispose();
    }
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final c = widget.construct;
    return Card(
      margin: const EdgeInsets.fromLTRB(12, 6, 12, 6),
      elevation: 1,
      child: Padding(
        padding: const EdgeInsets.fromLTRB(14, 12, 14, 12),
        child: _node(c.widget, c.view, const []),
      ),
    );
  }

  // === A position of the view ==============================================

  /// The widget [w] over the view [v] at position [at]: a question where the
  /// view marks one, `input(P)`; a position the program has not yet written
  /// whole where it marks one pending, `pending(P)`; and otherwise what the
  /// program wrote.  [recs] binds the names of the recursive types whose
  /// widgets [w] is within, `rec(T, W)` above it, for the `ref(T)` in it
  /// (bridge.dart, [unfoldWidget]).
  Widget _node(GTerm w, GTerm v, List<int> at,
      [Map<String, GTerm> recs = const {}]) {
    final u = unfoldWidget(w, recs);
    if (u == null) return _undrawable(w, at);
    final (w1, recs1) = u;
    final p = questionAt(v);
    if (p != null) {
      return _question(
          w1, p, [for (final i in p.items) (i as GInt).value], recs1);
    }
    final q = pendingAt(v);
    if (q != null) return _pending(w1, q);
    return _shown(w1, v, at, recs1);
  }

  /// The widget position at `P` that the program has not yet written whole,
  /// `pending(P)`, drawn by [w], the widget of its type: a placeholder naming
  /// the form where [w] is one, taking no input, and drawn again once the
  /// program writes the position.
  Widget _pending(GTerm w, GList p) {
    final at = [for (final i in p.items) (i as GInt).value];
    final (kind0, ws0) = ctorArgs(w);
    final (kind, ws) =
        kind0 == recCtor && ws0.length == 2 ? ctorArgs(ws0[1]) : (kind0, ws0);
    final form = kind == 'form' && _isForm(ws) ? _name(ws[0]) : null;
    return _keyed(
        'pending',
        at,
        Text(form == null ? '\u2026' : '$form \u2026',
            style: TextStyle(
                color: Colors.grey.shade600, fontStyle: FontStyle.italic)));
  }

  /// A position the program writes, drawn by its widget.
  Widget _shown(GTerm w, GTerm v, List<int> at,
      [Map<String, GTerm> recs = const {}]) {
    final u = unfoldWidget(w, recs);
    if (u == null) return _undrawable(w, at);
    final (w1, recs1) = u;
    w = w1;
    final (kind, ws) = ctorArgs(w);
    switch (kind) {
      case 'shown' when ws.isEmpty:
        return _keyed('shown', at, Text(displayText(v)));
      case 'thread' when ws.isEmpty:
        // Its elements so far, each shown, or pending where the program has
        // not yet written it whole, at the position it carries.
        final items = v is GList ? v.items : [v];
        return _keyed(
            'thread',
            at,
            Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                for (final e in items)
                  Padding(
                    padding: const EdgeInsets.symmetric(vertical: 2),
                    child: pendingAt(e) != null
                        ? _pending(const GAtom('shown'), pendingAt(e)!)
                        : Text(displayText(e)),
                  ),
              ],
            ));
      case 'thread' when ws.length == 1 && v is GList:
        // A stream or a list the program writes whose elements hold
        // questions: each element drawn by the widget of its type, at its
        // own position --- the path of argument indices through the list
        // cells, 2 for each element before it and then 1 --- and each
        // question in it by the field of its position.
        return _keyed(
            'thread',
            at,
            Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                for (var k = 0; k < v.items.length; k++)
                  Padding(
                    padding: const EdgeInsets.symmetric(vertical: 3),
                    child: _node(ws[0], v.items[k], elementAt(at, k), recs1),
                  ),
              ],
            ));
      case 'picker' when ws.isEmpty:
        return _picker(v, at);
      case 'form' when _isForm(ws):
        final entries = _entries(ws);
        final f = _name(ws[0]);
        final (vf, vs) = _viewArgs(v);
        if (vf != f || vs.length != entries.length) break;
        // The form's fields, each labelled, and under them its questions of
        // their own, each drawn once the form's answer leaves it open.
        // A shown scalar sits beside its label, on one line; anything else
        // under it.  A field of a recursive type is drawn when the person
        // opens it.
        Widget entry(int i) {
          final w = entries[i].widget;
          final atI = [...at, i + 1];
          if (recursiveWidget(w) && !_opened.contains(pathText(atI))) {
            return Padding(
                padding: const EdgeInsets.symmetric(vertical: 3),
                child: _closed(entries[i].label, atI));
          }
          final node = _node(w, vs[i], atI, recs1);
          final scalar = w is GAtom && w.name == 'shown';
          return Padding(
            padding: const EdgeInsets.symmetric(vertical: 3),
            child: scalar
                ? Row(
                    crossAxisAlignment: CrossAxisAlignment.baseline,
                    textBaseline: TextBaseline.alphabetic,
                    children: [
                      Text(entries[i].label,
                          style: const TextStyle(fontSize: 12)),
                      const SizedBox(width: 8),
                      Flexible(child: node),
                    ],
                  )
                : _labelled(entries[i].label, node),
          );
        }
        // "a form headed by its functor" (vGLP f9b63d6, Definition "Widget
        // Declaration, Default Widget"), as the person's form is headed in
        // its editor: the heading, then the fields.
        final fields = Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            _keyed('heading', at, _heading(f)),
            for (var i = 0; i < entries.length; i++)
              if (!entries[i].own) entry(i),
            for (var i = 0; i < entries.length; i++)
              if (entries[i].own) entry(i),
          ],
        );
        // A form inside the construct's term --- a tuple the program wrote
        // inside a tuple, or an element of a thread --- is set in under a
        // rule, so that its fields read as its own and not its parent's.
        return _keyed(
            'form',
            at,
            at.isEmpty
                ? fields
                : Container(
                    padding: const EdgeInsets.only(left: 10),
                    decoration: BoxDecoration(
                        border: Border(
                            left: BorderSide(
                                color: Colors.grey.shade400, width: 2))),
                    child: fields,
                  ));
      case 'menu' when ws.length == 1 && ws[0] is GList:
        // The program wrote one alternative: its form, where one has its
        // functor and arity, or a value shown.
        final (vf, vs) = _viewArgs(v);
        final alts = (ws[0] as GList).items;
        for (final a in alts) {
          final (ak, aws) = ctorArgs(a);
          if (ak == 'form' &&
              _isForm(aws) &&
              _name(aws[0]) == vf &&
              _entries(aws).length == vs.length) {
            return _keyed('menu', at, _shown(a, v, at, recs1));
          }
        }
        if (alts.any((a) => a is GAtom && a.name == 'shown')) {
          return _keyed('menu', at, Text(displayText(v)));
        }
      // A position the person has answered, inside a form whose answer left
      // a question of its own open: the answer, shown and no more edited.
      case 'text' || 'number' || 'date' || 'peer' when ws.isEmpty:
        return _keyed('shown', at, Text(displayText(v)));
      case 'button' || 'buttons' || 'input_box' || 'input_list'
          when ws.length == 1:
        return _keyed('shown', at, Text(displayText(v)));
    }
    final named = _named(w, v, at);
    if (named != null) return named;
    return _undrawable(w, at);
  }

  /// A picker: the list the program writes, each element a button granting
  /// itself at the choice's position, an element the program has not yet
  /// written whole drawn pending and granting nothing.
  Widget _picker(GTerm v, List<int> at) {
    final (_, args) = ctorArgs(v);
    GList? list;
    GList? choice;
    for (final a in args) {
      final p = questionAt(a);
      if (p != null) {
        choice = p;
      } else if (a is GList) {
        list = a;
      }
    }
    if (args.length != 2 || list == null || choice == null) {
      return _undrawable(const GAtom('picker'), at);
    }
    final p = choice;
    return _keyed(
        'picker',
        at,
        Wrap(
          spacing: 8,
          runSpacing: 4,
          children: [
            for (final e in list.items)
              if (pendingAt(e) != null)
                _pending(const GAtom('shown'), pendingAt(e)!)
              else
                OutlinedButton(
                  onPressed: () => widget.grant(p, e),
                  child: Text(displayText(e)),
                ),
          ],
        ));
  }

  // === A question ==========================================================

  /// The question at path [at], drawn by its widget [w].
  Widget _question(GTerm w, GList p, List<int> at,
      [Map<String, GTerm> recs = const {}]) {
    final u = unfoldWidget(w, recs);
    if (u == null) return _undrawable(w, at);
    final (w1, recs1) = u;
    w = w1;
    final (kind, ws) = ctorArgs(w);
    switch (kind) {
      case 'button' when ws.length == 1:
        return _keyed('button', at, _button(ws[0], () => widget.grant(p, ws[0])));
      case 'buttons' when ws.length == 1 && ws[0] is GList:
        return _keyed(
            'buttons',
            at,
            Wrap(spacing: 8, runSpacing: 4, children: [
              for (final c in (ws[0] as GList).items)
                _button(c, () => widget.grant(p, c)),
            ]));
      case 'input_box' when ws.length == 1:
        // The widget of the element type, kept open: each submission grants
        // one element at the stream's own position, and the box clears for
        // the next.
        final (ek, _) = ctorArgs(ws[0]);
        if (ek == 'button' || ek == 'buttons') {
          return _keyed('input_box', at, _question(ws[0], p, at, recs1));
        }
        return _keyed('input_box', at,
            _submitting(ws[0], p, at, keepOpen: true, recs: recs1));
      case 'input_list' when ws.length == 1:
        // The same, with a button that ends the list: it grants [end], the
        // list's end (listEnd), at the list's position.
        final (ek, _) = ctorArgs(ws[0]);
        return _keyed(
            'input_list',
            at,
            Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                if (ek == 'button' || ek == 'buttons')
                  _question(ws[0], p, at, recs1)
                else
                  _submitting(ws[0], p, at, keepOpen: true, recs: recs1),
                const SizedBox(height: 6),
                Align(
                  alignment: Alignment.centerRight,
                  child: OutlinedButton(
                    key: endKey(_id, at),
                    onPressed: () => widget.grant(p, listEnd),
                    child: const Text('End'),
                  ),
                ),
              ],
            ));
      case 'text' || 'number' || 'date' || 'peer' when ws.isEmpty:
        return _submitting(w, p, at, keepOpen: false, recs: recs1);
      case 'form' when _isForm(ws):
        return _submitting(w, p, at, keepOpen: false, recs: recs1);
      case 'menu' when ws.length == 1 && ws[0] is GList:
        return _submitting(w, p, at, keepOpen: false, recs: recs1);
    }
    final named = _named(w, questionView(p), at);
    if (named != null) return named;
    return _undrawable(w, at);
  }

  /// A question answered by a submission: its editor and a button granting
  /// what the editor holds, enabled once it holds an input of the widget.
  Widget _submitting(GTerm w, GList p, List<int> at,
      {required bool keepOpen, Map<String, GTerm> recs = const {}}) {
    final value = _value(w, at, recs);
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        _editor(w, at, null, recs),
        const SizedBox(height: 6),
        Align(
          alignment: Alignment.centerRight,
          child: ElevatedButton(
            key: submitKey(_id, at),
            onPressed: value == null
                ? null
                : () {
                    widget.grant(p, value);
                    if (keepOpen) setState(() => _clear(at));
                  },
            child: Text(keepOpen ? 'Send' : 'Submit'),
          ),
        ),
      ],
    );
  }

  // === Editors: the person's input before it is submitted ==================

  /// The editor of widget [w] at position [at], labelled [label] where it
  /// is a field of a form, by the name of the field's declared type.
  Widget _editor(GTerm w, List<int> at, String? label,
      [Map<String, GTerm> recs = const {}]) {
    final u = unfoldWidget(w, recs);
    if (u == null) return _undrawable(w, at);
    final (w1, recs1) = u;
    w = w1;
    final (kind, ws) = ctorArgs(w);
    final k = pathText(at);
    switch (kind) {
      case 'text' || 'number' || 'peer' when ws.isEmpty:
        return _keyed(
            kind,
            at,
            TextField(
              controller: _text(k),
              keyboardType: kind == 'number'
                  ? const TextInputType.numberWithOptions(
                      signed: true, decimal: true)
                  : TextInputType.text,
              decoration: InputDecoration(labelText: label, isDense: true),
              onChanged: (_) => setState(() {}),
            ));
      case 'date' when ws.isEmpty:
        final day = _days[k] ?? 0;
        return _keyed(
            'date',
            at,
            Row(children: [
              if (label != null) Text(label),
              IconButton(
                key: ValueKey('$_id:date:$k:earlier'),
                icon: const Icon(Icons.chevron_left),
                onPressed: day == 0 ? null : () => setState(() => _days[k] = day - 1),
              ),
              Text('day $day'),
              IconButton(
                key: ValueKey('$_id:date:$k:later'),
                icon: const Icon(Icons.chevron_right),
                onPressed: () => setState(() => _days[k] = day + 1),
              ),
            ]));
      case 'button' when ws.length == 1:
        return _keyed('button', at,
            _labelled(label, Chip(label: Text(displayText(ws[0])))));
      case 'buttons' when ws.length == 1 && ws[0] is GList:
        final chosen = _chosen[k];
        return _keyed(
            'buttons',
            at,
            _labelled(label, Wrap(spacing: 8, children: [
              for (final c in (ws[0] as GList).items)
                ChoiceChip(
                  label: Text(displayText(c)),
                  selected: chosen != null && formatTerm(chosen) == formatTerm(c),
                  onSelected: (_) => setState(() => _chosen[k] = c),
                ),
            ])));
      case 'form' when _isForm(ws):
        // Its fields, each labelled by the name of its declared type; a
        // question of its own is no field, and is drawn under the form once
        // the form's answer leaves it open; a field of a recursive type is
        // drawn when the person opens it.
        final entries = _entries(ws);
        return _keyed(
            'form',
            at,
            _labelled(
                label,
                Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    _keyed('heading', at, _heading(_name(ws[0]))),
                    for (var i = 0; i < entries.length; i++)
                      if (!entries[i].own)
                        Padding(
                          padding: const EdgeInsets.only(top: 4),
                          child: recursiveWidget(entries[i].widget) &&
                                  !_opened.contains(pathText([...at, i + 1]))
                              ? _closed(entries[i].label, [...at, i + 1])
                              : _editor(entries[i].widget, [...at, i + 1],
                                  entries[i].label, recs1),
                        ),
                  ],
                )));
      case 'menu' when ws.length == 1 && ws[0] is GList:
        final alts = (ws[0] as GList).items;
        final chosen = _alternative[k];
        return _keyed(
            'menu',
            at,
            Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Wrap(spacing: 8, children: [
                  for (var i = 0; i < alts.length; i++)
                    ChoiceChip(
                      label: Text(_alternativeLabel(alts, i)),
                      selected: chosen == i,
                      // Another alternative is another form: what was
                      // typed into the last one goes with it.
                      onSelected: (_) => setState(() {
                        _clear(at);
                        _alternative[k] = i;
                      }),
                    ),
                ]),
                if (chosen != null &&
                    (ctorArgs(alts[chosen]).$1 == 'form' ||
                        recursiveWidget(alts[chosen])))
                  Padding(
                    padding: const EdgeInsets.only(top: 6),
                    child: _editor(alts[chosen], at, label, recs1),
                  ),
              ],
            ));
    }
    return _undrawable(w, at);
  }

  /// The input the editor of [w] at [at] holds, or null where it holds none
  /// that the widget forms.
  GTerm? _value(GTerm w, List<int> at, [Map<String, GTerm> recs = const {}]) {
    final u = unfoldWidget(w, recs);
    if (u == null) return null;
    final (w1, recs1) = u;
    w = w1;
    final (kind, ws) = ctorArgs(w);
    final k = pathText(at);
    switch (kind) {
      case 'text' when ws.isEmpty:
        return GString(_text(k).text);
      case 'number' when ws.isEmpty:
        final s = _text(k).text.trim();
        final i = int.tryParse(s);
        if (i != null) return GInt(i);
        final d = double.tryParse(s);
        return d == null || !d.isFinite ? null : GReal(d);
      case 'peer' when ws.isEmpty:
        final s = _text(k).text.trim();
        return _identifier.hasMatch(s) ? GAtom(s) : null;
      case 'date' when ws.isEmpty:
        return GInt(_days[k] ?? 0);
      case 'button' when ws.length == 1:
        return ws[0];
      case 'buttons' when ws.length == 1:
        return _chosen[k];
      case 'form' when _isForm(ws):
        final entries = _entries(ws);
        final values = <GTerm>[];
        for (var i = 0; i < entries.length; i++) {
          if (entries[i].own) {
            // A question of its own: no input here, the form's answer
            // leaving it open.
            values.add(const GList([]));
            continue;
          }
          // A field of a recursive type the person has not opened holds no
          // input.
          if (recursiveWidget(entries[i].widget) &&
              !_opened.contains(pathText([...at, i + 1]))) {
            return null;
          }
          final v = _value(entries[i].widget, [...at, i + 1], recs1);
          if (v == null) return null;
          values.add(v);
        }
        return GStruct(_name(ws[0]), values);
      case 'menu' when ws.length == 1 && ws[0] is GList:
        final chosen = _alternative[k];
        if (chosen == null) return null;
        return _value((ws[0] as GList).items[chosen], at, recs1);
    }
    return null;
  }

  /// An agent identifier: a name, or a key, the hex string `Key` carries
  /// (programs/self.glp, `Key ::= String`) --- letters, digits and `_`, and
  /// nothing else.
  static final RegExp _identifier = RegExp(r'^[A-Za-z0-9_]+$');

  /// Clear every editor at [at] and below it, for the next submission.
  void _clear(List<int> at) {
    final own = pathText(at);
    final prefix = '${own.substring(0, own.length - 1)}, ';
    bool under(String k) =>
        at.isEmpty || k == own || k.startsWith(prefix);
    for (final e in _texts.entries) {
      if (under(e.key)) e.value.clear();
    }
    _chosen.removeWhere((k, _) => under(k));
    _alternative.removeWhere((k, _) => under(k));
    _days.removeWhere((k, _) => under(k));
    // The fields of recursive types below [at] close again; one at [at]
    // itself stays open, its menu choosing within it.
    _opened.removeWhere((k) => k != own && under(k));
  }

  TextEditingController _text(String k) =>
      _texts.putIfAbsent(k, TextEditingController.new);

  // === Pieces ==============================================================

  Widget _button(GTerm c, VoidCallback onPressed) => ElevatedButton(
        onPressed: onPressed,
        child: Text(displayText(c)),
      );

  /// A field of a recursive type the person has not opened, at [at]: its
  /// label and the button that opens it, on which the field is drawn by its
  /// widget --- "a field of a recursive type drawn when the person opens it"
  /// (vGLP, Definition "Default Widget") --- so that the widget of a
  /// recursive type, finite by its reference back to itself (bridge.dart,
  /// [recCtor] and [refCtor]), unfolds only as far as the person goes.
  Widget _closed(String? label, List<int> at) => _keyed(
      'closed',
      at,
      Row(children: [
        if (label != null) Text(label, style: const TextStyle(fontSize: 12)),
        if (label != null) const SizedBox(width: 8),
        OutlinedButton(
          key: openKey(_id, at),
          onPressed: () => setState(() => _opened.add(pathText(at))),
          child: const Text('Open'),
        ),
      ]));

  /// A widget a declaration names, where the family has it.
  Widget? _named(GTerm w, GTerm v, List<int> at) {
    if (w is! GAtom) return null;
    final b = widget.family.named[w.name];
    if (b == null) return null;
    return KeyedSubtree(
        key: nodeKey(_id, w.name, at),
        child: b(context, widget.construct, v, at, widget.grant));
  }

  /// A widget this family does not have: named, and taking no input.
  Widget _undrawable(GTerm w, List<int> at) => _keyed(
      'undrawable',
      at,
      Text('No widget ${formatTerm(w)} in this construct family',
          style: TextStyle(color: Colors.red.shade700)));

  Widget _keyed(String kind, List<int> at, Widget child) =>
      KeyedSubtree(key: nodeKey(_id, kind, at), child: child);

  /// A form, `form(F, [E1, ..., En])`, each entry `field(L, W)` or
  /// `question(L, W)`.
  static bool _isForm(List<GTerm> ws) =>
      ws.length == 2 &&
      ws[1] is GList &&
      (ws[1] as GList).items.every((e) {
        final (k, es) = ctorArgs(e);
        return (k == 'field' || k == 'question') && es.length == 2;
      });

  /// The entries of a form, one per argument: its label, the name of the
  /// argument's declared type; its widget; and whether it is a question of
  /// its own, `question(L, W)`, and no field of the form.
  static List<({String label, GTerm widget, bool own})> _entries(
          List<GTerm> ws) =>
      [
        for (final e in (ws[1] as GList).items)
          (
            label: displayText(ctorArgs(e).$2[0]),
            widget: ctorArgs(e).$2[1],
            own: ctorArgs(e).$1 == 'question',
          )
      ];

  /// A form's heading, its functor (vGLP f9b63d6, Definition "Widget
  /// Declaration, Default Widget": "a tuple: a form headed by its functor").
  static Widget _heading(String functor) => Text(functor,
      style: const TextStyle(fontWeight: FontWeight.bold));

  /// [child] under [label], where there is one.
  static Widget _labelled(String? label, Widget child) => label == null
      ? child
      : Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(label, style: const TextStyle(fontSize: 12)),
            child,
          ],
        );

  /// The functor a form names.
  static String _name(GTerm f) => ctorArgs(f).$1;

  /// The functor and arguments of a view a form is drawn over, a list among
  /// them as the list cell `'.'(H, T)` it is: a draw naming `form('.', [Wh,
  /// Wt])` is drawn over the list the program wrote, which reaches the screen
  /// whole (isolate_protocol.dart, `screenTermOf`) and is a [GList] there
  /// where it ends in [].  The compilation emits no such widget: a list cell
  /// occurs in an interactive type only as a Stream's or a List's, one
  /// outside them refused (vGLP, Definition "Volitional Procedure,
  /// Interactive Type"; vGLP #7 Cowork, 2026-10-11 14:31 UTC, item 2).
  static (String, List<GTerm>) _viewArgs(GTerm v) =>
      v is GList && v.items.isNotEmpty
          ? ('.', [v.items.first, GList(v.items.sublist(1))])
          : ctorArgs(v);

  /// What the menu's alternative [i] of [alts] is called: its form's
  /// functor, and its arity where another of its forms has that functor
  /// (Remark "Construct Families"; vGLP #6 Cowork, 2026-10-11 12:55 UTC,
  /// item 3, Q3); or its button's constant.
  static String _alternativeLabel(List<GTerm> alts, int i) {
    final (k, ws) = ctorArgs(alts[i]);
    if (k == 'form' && _isForm(ws)) {
      final f = _name(ws[0]);
      final shared = alts.where((a) {
        final (ak, aws) = ctorArgs(a);
        return ak == 'form' && _isForm(aws) && _name(aws[0]) == f;
      }).length;
      return shared > 1 ? '$f/${_entries(ws).length}' : f;
    }
    if (k == 'button' && ws.length == 1) return displayText(ws[0]);
    if (recursiveWidget(alts[i])) return displayText(ws[0]);
    return formatTerm(alts[i]);
  }
}

/// The view `input(P)` of the question at [p].
GTerm questionView(GList p) => GStruct(inputCtor, [p]);
