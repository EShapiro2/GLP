/// The construct family the bridge draws with (vGLP, sections/elicitation.tex,
/// Remark "Construct Families" and Definition "Widget Declaration, Default
/// Widget").
///
/// A draw `draw(Id, W, V)` names its widget `W` and carries its view `V`, each
/// position the person writes marked `input(P)` (bridge.dart).  "A question's
/// type fixes a construct's semantic content --- what it shows, what it
/// takes, and the answer each submission forms --- and nothing of its
/// layout": the family decides what each widget looks like, and this file is
/// the grassroots app's.  Its widgets are the Definition's default widgets,
/// by the terms the compiled construct processes name them with:
///
///   - a constant: a button, `button(C)`; a union of constants: a row of
///     buttons, `buttons([C1, ..., Cn])`;
///   - `String`, a number (`Integer` or `Real`), `Date` and an agent
///     identifier: a text field, a number field, a date picker and a peer
///     field, `text`, `number`, `date` and `peer`;
///   - a tuple: a form with one field per argument, `form(F, [W1, ...,
///     Wn])`; a union of tuples: a menu of forms, `menu([W1, ..., Wn])`;
///   - a value the program writes: shown and not edited, `shown`; a list the
///     program writes, with a choice coming back: a picker, `picker`;
///   - a stream the person writes: the widget of its element type, kept
///     open, one element per submission, `input_box(W)`; a stream the
///     program writes: a thread, `thread`.
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
/// identifier typed, a form the tuple of its fields and a menu the
/// alternative chosen.  The construct process forms the answer from it, and
/// refuses one that forms no term of the question's type; the family checks
/// no type.
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

/// A path as the boundary writes it, `[]`, `[2]`, `[1, 2]`.
String pathText(List<int> at) => '[${at.join(', ')}]';

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
  /// view marks one, `input(P)`, and otherwise what the program wrote.
  Widget _node(GTerm w, GTerm v, List<int> at) {
    final p = questionAt(v);
    if (p != null) {
      return _question(w, p, [for (final i in p.items) (i as GInt).value]);
    }
    return _shown(w, v, at);
  }

  /// A position the program writes, drawn by its widget.
  Widget _shown(GTerm w, GTerm v, List<int> at) {
    final (kind, ws) = ctorArgs(w);
    switch (kind) {
      case 'shown' when ws.isEmpty:
        return _keyed('shown', at, Text(displayText(v)));
      case 'thread' when ws.isEmpty:
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
                    child: Text(displayText(e)),
                  ),
              ],
            ));
      case 'picker' when ws.isEmpty:
        return _picker(v, at);
      case 'form' when _isForm(ws):
        final fields = (ws[1] as GList).items;
        final f = _name(ws[0]);
        final (vf, vs) = ctorArgs(v);
        if (vf != f || vs.length != fields.length) break;
        return _keyed(
            'form',
            at,
            Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                for (var i = 0; i < fields.length; i++)
                  Padding(
                    padding: const EdgeInsets.symmetric(vertical: 3),
                    child: _node(fields[i], vs[i], [...at, i + 1]),
                  ),
              ],
            ));
      case 'menu' when ws.length == 1 && ws[0] is GList:
        // The program wrote one alternative: its form, where one has its
        // functor and arity, or a value shown.
        final (vf, vs) = ctorArgs(v);
        final alts = (ws[0] as GList).items;
        for (final a in alts) {
          final (ak, aws) = ctorArgs(a);
          if (ak == 'form' &&
              _isForm(aws) &&
              _name(aws[0]) == vf &&
              (aws[1] as GList).items.length == vs.length) {
            return _keyed('menu', at, _shown(a, v, at));
          }
        }
        if (alts.any((a) => a is GAtom && a.name == 'shown')) {
          return _keyed('menu', at, Text(displayText(v)));
        }
    }
    final named = _named(w, v, at);
    if (named != null) return named;
    return _undrawable(w, at);
  }

  /// A picker: the list the program writes, each element a button granting
  /// itself at the choice's position.
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
              OutlinedButton(
                onPressed: () => widget.grant(p, e),
                child: Text(displayText(e)),
              ),
          ],
        ));
  }

  // === A question ==========================================================

  /// The question at path [at], drawn by its widget [w].
  Widget _question(GTerm w, GList p, List<int> at) {
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
          return _keyed('input_box', at, _question(ws[0], p, at));
        }
        return _keyed('input_box', at, _submitting(ws[0], p, at, keepOpen: true));
      case 'text' || 'number' || 'date' || 'peer' when ws.isEmpty:
        return _submitting(w, p, at, keepOpen: false);
      case 'form' when _isForm(ws):
        return _submitting(w, p, at, keepOpen: false);
      case 'menu' when ws.length == 1 && ws[0] is GList:
        return _submitting(w, p, at, keepOpen: false);
    }
    final named = _named(w, questionView(p), at);
    if (named != null) return named;
    return _undrawable(w, at);
  }

  /// A question answered by a submission: its editor and a button granting
  /// what the editor holds, enabled once it holds an input of the widget.
  Widget _submitting(GTerm w, GList p, List<int> at, {required bool keepOpen}) {
    final value = _value(w, at);
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        _editor(w, at),
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

  /// The editor of widget [w] at position [at].
  Widget _editor(GTerm w, List<int> at) {
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
              decoration: InputDecoration(labelText: kind, isDense: true),
              onChanged: (_) => setState(() {}),
            ));
      case 'date' when ws.isEmpty:
        final day = _days[k] ?? 0;
        return _keyed(
            'date',
            at,
            Row(children: [
              const Text('date'),
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
        return _keyed('button', at, Chip(label: Text(displayText(ws[0]))));
      case 'buttons' when ws.length == 1 && ws[0] is GList:
        final chosen = _chosen[k];
        return _keyed(
            'buttons',
            at,
            Wrap(spacing: 8, children: [
              for (final c in (ws[0] as GList).items)
                ChoiceChip(
                  label: Text(displayText(c)),
                  selected: chosen != null && formatTerm(chosen) == formatTerm(c),
                  onSelected: (_) => setState(() => _chosen[k] = c),
                ),
            ]));
      case 'form' when _isForm(ws):
        final fields = (ws[1] as GList).items;
        return _keyed(
            'form',
            at,
            Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(_name(ws[0]),
                    style: const TextStyle(fontWeight: FontWeight.bold)),
                for (var i = 0; i < fields.length; i++)
                  Padding(
                    padding: const EdgeInsets.only(top: 4),
                    child: _editor(fields[i], [...at, i + 1]),
                  ),
              ],
            ));
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
                      label: Text(_alternativeLabel(alts[i])),
                      selected: chosen == i,
                      // Another alternative is another form: what was
                      // typed into the last one goes with it.
                      onSelected: (_) => setState(() {
                        _clear(at);
                        _alternative[k] = i;
                      }),
                    ),
                ]),
                if (chosen != null && ctorArgs(alts[chosen]).$1 == 'form')
                  Padding(
                    padding: const EdgeInsets.only(top: 6),
                    child: _editor(alts[chosen], at),
                  ),
              ],
            ));
    }
    return _undrawable(w, at);
  }

  /// The input the editor of [w] at [at] holds, or null where it holds none
  /// that the widget forms.
  GTerm? _value(GTerm w, List<int> at) {
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
        final fields = (ws[1] as GList).items;
        final values = <GTerm>[];
        for (var i = 0; i < fields.length; i++) {
          final v = _value(fields[i], [...at, i + 1]);
          if (v == null) return null;
          values.add(v);
        }
        return GStruct(_name(ws[0]), values);
      case 'menu' when ws.length == 1 && ws[0] is GList:
        final chosen = _alternative[k];
        if (chosen == null) return null;
        return _value((ws[0] as GList).items[chosen], at);
    }
    return null;
  }

  /// An agent identifier, as GLP writes a constant unquoted.
  static final RegExp _identifier = RegExp(r'^[a-z][A-Za-z0-9_]*$');

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
  }

  TextEditingController _text(String k) =>
      _texts.putIfAbsent(k, TextEditingController.new);

  // === Pieces ==============================================================

  Widget _button(GTerm c, VoidCallback onPressed) => ElevatedButton(
        onPressed: onPressed,
        child: Text(displayText(c)),
      );

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

  static bool _isForm(List<GTerm> ws) => ws.length == 2 && ws[1] is GList;

  /// The functor a form names.
  static String _name(GTerm f) => ctorArgs(f).$1;

  /// What a menu's alternative is called: its form's functor, or its
  /// button's constant.
  static String _alternativeLabel(GTerm a) {
    final (k, ws) = ctorArgs(a);
    if (k == 'form' && _isForm(ws)) return _name(ws[0]);
    if (k == 'button' && ws.length == 1) return displayText(ws[0]);
    return formatTerm(a);
  }
}

/// The view `input(P)` of the question at [p].
GTerm questionView(GList p) => GStruct(inputCtor, [p]);
