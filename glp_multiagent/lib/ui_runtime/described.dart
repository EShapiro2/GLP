/// The screens of an app drawn from an interface description in the UI language of the Grassroots UI paper,
/// in place of the screen of tabs (tab_screen.dart), where the app is given one ([DescribedUi]).
///
/// The description is written in the contract's words and compiled, with the contract, into the form read
/// here ([UiDescription]): its screens, each about nothing (the start), a person or a value, or a fact of the
/// person's; their components, the questions, the facts they show, the lists that open other screens, and the
/// actions, each an act of the contract by its schema, with a source for each place of its request but the
/// person's own: the screen's subject, a list to pick from, or a field the person fills.  An interface description
/// reads the person's state and answers her questions, and nothing else: here the last view on construct 0
/// ([SocialState]), the approval questions as the construct family draws them, and the request question, which
/// an action answers with `propose(R)` as the request widget does ([SocialAsk]).  Which bindings an action offers
/// is the platform's: the ways the person's state satisfies the act at her role ([ActInfo.ways]), kept to those
/// the action's sources allow, and a request made only where the act is not forbidden ([ActInfo.lacks],
/// [ActInfo.distinct]).
library;

import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:glp_runtime/vglp/canonical.dart' show DeclaredAct, DeclaredField;

import 'named_widgets.dart';
import 'term.dart';

/// A source of a place: the subject of the screen, whole or the argument at [pos]; a list to pick from, of
/// everyone or of the values of a predicate's facts; or a field the person fills.
class PlaceSource {
  final bool subject;
  final int? pos;
  final String? from;
  final bool typed;
  const PlaceSource({this.subject = false, this.pos, this.from, this.typed = false});

  static PlaceSource of(Map<String, dynamic> j) => j.containsKey('subject')
      ? PlaceSource(subject: true, pos: j['subject'] as int?)
      : j.containsKey('from')
          ? PlaceSource(from: j['from'] as String)
          : const PlaceSource(typed: true);
}

/// A component of a screen, by its [kind]: `questions`, `show`, `thread`, `list` or `action`.
class UiComponent {
  final String kind;
  final Map<String, dynamic> j;
  UiComponent(this.kind, this.j);

  List<String> get preds => [for (final p in (j['preds'] ?? j['with'] ?? const []) as List) p as String];
  String get schema => j['schema'] as String;
  String get button => j['button'] as String;
  String? get sentence => j['sentence'] as String?;
  int get me => j['me'] as int;
  int get arity => j['arity'] as int;
  Map<int, PlaceSource> get places => {
        for (final e in (j['places'] as Map<String, dynamic>).entries)
          int.parse(e.key): PlaceSource.of(e.value as Map<String, dynamic>)
      };
}

class UiScreen {
  final String name;

  /// What the screen is about: null for the start screen; `fact` the predicate of a fact of the person's;
  /// `name` a person or a value.
  final String? fact;
  final bool about;
  final List<UiComponent> components;
  const UiScreen(this.name, this.about, this.fact, this.components);
}

/// An interface description, compiled with its contract.
class UiDescription {
  final String start;
  final Map<String, UiScreen> screens;

  /// The positions of each predicate's facts that hold persons.
  final Map<String, List<int>> personPos;
  const UiDescription(this.start, this.screens, this.personPos);

  static UiDescription parse(String source) {
    final j = jsonDecode(source) as Map<String, dynamic>;
    final screens = <String, UiScreen>{};
    for (final e in (j['screens'] as Map<String, dynamic>).entries) {
      final s = e.value as Map<String, dynamic>, about = s['about'] as Map<String, dynamic>?;
      screens[e.key] = UiScreen(e.key, about != null, about?['fact'] as String?, [
        for (final c in s['components'] as List)
          UiComponent((c as Map<String, dynamic>)['kind'] as String, c)
      ]);
    }
    return UiDescription(j['start'] as String, screens, {
      for (final e in ((j['personPos'] ?? const {}) as Map<String, dynamic>).entries)
        e.key: [for (final p in e.value as List) p as int]
    });
  }
}

/// The interface description the screens below it are drawn from.
class DescribedUi extends InheritedWidget {
  final UiDescription ui;
  const DescribedUi({super.key, required this.ui, required super.child});

  static DescribedUi? maybeOf(BuildContext context) =>
      context.dependOnInheritedWidgetOfExactType<DescribedUi>();

  @override
  bool updateShouldNotify(DescribedUi old) => old.ui != ui;
}

/// The key of a control of a described screen.
ValueKey<String> describedKey(String what) => ValueKey('ui:$what');

/// The screens of the description over the last view, the questions and the request question.
class DescribedScreen extends StatefulWidget {
  final UiDescription ui;
  final SocialState state;
  final SocialAsk? ask;

  /// The questions put to the person, each drawn by the construct family.
  final List<Widget> questions;

  const DescribedScreen(
      {super.key, required this.ui, required this.state, required this.ask, required this.questions});

  @override
  State<DescribedScreen> createState() => _DescribedScreenState();
}

/// A way an action offers: the values of the places a way of the act binds, and of those the subject fixes.
typedef _Way = Map<int, GTerm>;

class _DescribedScreenState extends State<DescribedScreen> {
  /// The screens the person came through, each with its subject; the last is on the screen.
  final List<(String, GTerm?)> _stack = [];
  final Map<String, TextEditingController> _fields = {};

  @override
  void dispose() {
    for (final c in _fields.values) {
      c.dispose();
    }
    super.dispose();
  }

  SocialState get s => widget.state;
  String _k(GTerm t) => formatTerm(t);
  bool _same(GTerm a, GTerm b) => _k(a) == _k(b);

  /// A person or a value as the person reads it: a key with its underscores as spaces, capitalized.
  String _show(GTerm t) {
    if (_same(t, s.me)) return 'you';
    final x = displayText(t).replaceAll('_', ' ');
    return x.isEmpty ? x : x[0].toUpperCase() + x.substring(1);
  }

  /// What the person reads for a predicate: the name of the list the contract gives it, or the predicate.
  String _label(String p) {
    for (final l in s.lists) {
      if (l.predicate == p) return l.name;
    }
    return p.replaceAll('_', ' ');
  }

  List<GTerm> _args(GTerm a) => ctorArgs(a).$2;
  String _pred(GTerm a) => ctorArgs(a).$1;

  /// Whether the fact [a] names [v]: every argument of [v] where [v] is a fact, [v] itself otherwise.
  bool _names(GTerm a, GTerm v, {bool fact = false}) => fact
      ? _args(v).every((x) => _args(a).any((y) => _same(x, y)))
      : _args(a).any((y) => _same(v, y));

  /// The persons named in the person's state: those she knows, and those at a person's position of a fact.
  List<GTerm> _everyone() {
    final out = <String, GTerm>{};
    for (final k in s.known) {
      out[_k(k)] = k;
    }
    for (final a in s.atoms) {
      final ps = widget.ui.personPos[_pred(a)] ?? const [];
      final xs = _args(a);
      for (final i in ps) {
        if (i <= xs.length) out[_k(xs[i - 1])] = xs[i - 1];
      }
    }
    out.remove(_k(s.me));
    return out.values.toList();
  }

  List<GTerm> _valuesOf(String p) {
    final out = <String, GTerm>{};
    for (final a in s.atoms) {
      if (_pred(a) == p) {
        for (final x in _args(a)) {
          out[_k(x)] = x;
        }
      }
    }
    return out.values.toList();
  }

  /// The items of a list, on a screen about [subject].
  List<GTerm> _items(UiComponent c, GTerm? subject) {
    final items = c.j['items'] as Map<String, dynamic>;
    if (items['everyone'] == true) return _everyone();
    if (items['from'] != null) return _valuesOf(items['from'] as String);
    final p = items['facts'] as String, keep = items['keep'] as int?;
    final kept = keep != null && subject != null && keep <= _args(subject).length ? _args(subject)[keep - 1] : null;
    return [
      for (final a in s.atoms)
        if (_pred(a) == p && (kept == null || _names(a, kept))) a
    ];
  }

  /// The value of the subject the source [src] takes.
  GTerm? _ofSubject(PlaceSource src, GTerm? subject) {
    if (subject == null) return null;
    if (src.pos == null) return subject;
    final xs = _args(subject);
    return src.pos! <= xs.length ? xs[src.pos! - 1] : null;
  }

  /// The act of [c] at the person's role, as the platform's request type declares it.
  DeclaredAct? _act(UiComponent c) {
    final o = widget.ask?.offers;
    if (o == null) return null;
    for (final b in o.buttons) {
      for (final a in b.acts) {
        if (a.schema == c.schema && a.proposer == c.me) return a;
      }
    }
    return null;
  }

  /// The ways [c] offers on a screen about [subject]: those of its act the person's state satisfies, each
  /// with the values the subject fixes, kept to those its lists allow.
  List<_Way> _ways(UiComponent c, DeclaredAct act, GTerm? subject) {
    final info = ActInfo(act), fixed = <int, GTerm>{};
    for (final e in c.places.entries) {
      if (e.value.subject) {
        final v = _ofSubject(e.value, subject);
        if (v == null) return const [];
        fixed[e.key] = v;
      }
    }
    final ways = act.binds.isEmpty ? [<int, GTerm>{}] : info.ways(s.atoms, s.me);
    final out = <_Way>[];
    for (final w in ways) {
      if (fixed.entries.any((f) => w[f.key] != null && !_same(w[f.key]!, f.value))) continue;
      final x = {...w, ...fixed};
      var ok = true;
      for (final e in c.places.entries) {
        final from = e.value.from;
        if (from == null || x[e.key] == null) continue;
        final allowed = from == 'everyone' ? _everyone() : _valuesOf(from);
        if (!allowed.any((v) => _same(v, x[e.key]!))) ok = false;
      }
      if (ok) out.add(x);
    }
    return out;
  }

  /// The places of [act] the person fills: its fields, each from a list or typed.
  List<DeclaredField> _fieldsOf(DeclaredAct act, _Way w) => [
        for (final f in act.fields)
          if (w[f.place] == null) f
      ];

  /// The value a field's text gives, as the request widget forms it.
  GTerm? _textValue(DeclaredField f, String text) {
    final t = text.trim();
    if (t.isEmpty) return null;
    switch (f.type) {
      case 'number':
        final v = int.tryParse(t);
        return v == null ? null : GInt(v);
      case 'key':
      case 'agent':
      case 'person':
        final k = t.toLowerCase().replaceAll(' ', '_');
        return RegExp(r'^[a-z][a-z0-9_]*$').hasMatch(k) ? GAtom(k) : null;
      case 'set':
        final es = {for (final e in t.split(',')) if (e.trim().isNotEmpty) e.trim()};
        return es.isEmpty ? null : GList([for (final e in es) GString(e)]);
    }
    return GString(t);
  }

  /// The request of [act] at [at], or null where a place is unfilled or the act is not allowed there.
  GTerm? _request(DeclaredAct act, Map<int, GTerm> at) {
    final o = widget.ask?.offers;
    if (o == null || widget.ask?.at == null) return null;
    final key = o.keyOf(act.schema);
    if (key == null) return null;
    final n = int.parse(key.substring(key.lastIndexOf('/') + 1));
    final args = [for (var i = 1; i <= n; i++) i == act.proposer ? (at[i] ?? s.me) : at[i]];
    final full = {for (var i = 1; i <= n; i++) i: args[i - 1]!};
    if (args.contains(null)) return null;
    final info = ActInfo(act);
    if (!info.lacks(s.atoms, full) || !info.distinct(full)) return null;
    return GStruct(act.schema, [
      for (var i = 1; i <= n; i++) o.isSet(act.schema, i) && args[i - 1] is! GList ? GList([args[i - 1]!]) : args[i - 1]!
    ]);
  }

  void _toast(String m) => ScaffoldMessenger.maybeOf(context)?.showSnackBar(SnackBar(content: Text(m)));

  /// The person taps the action [c]: where nothing is left to choose or fill, the request is made; otherwise a
  /// sheet asks for what is left.
  void _tap(UiComponent c, DeclaredAct act, List<_Way> ways, GTerm? subject) {
    final w0 = ways.first;
    final fields = _fieldsOf(act, w0);
    final varying = [
      for (final p in act.binds)
        if (ways.map((w) => w[p] == null ? '' : _k(w[p]!)).toSet().length > 1) p
    ];
    if (ways.length == 1 && fields.isEmpty) {
      final r = _request(act, w0);
      if (r == null) return _toast('${c.button}: not allowed now');
      widget.ask!.propose(r);
      return;
    }
    showModalBottomSheet<void>(
      context: context,
      isScrollControlled: true,
      builder: (ctx) => _Sheet(
        title: c.button,
        sentence: c.sentence,
        ways: ways,
        varying: varying,
        fields: fields,
        sources: c.places,
        show: _show,
        options: (f) {
          final src = c.places[f.place];
          if (src?.from == null) return null;
          return src!.from == 'everyone' ? _everyone() : _valuesOf(src.from!);
        },
        textValue: _textValue,
        submit: (w, given) {
          final r = _request(act, {...w, ...given});
          if (r == null) return false;
          widget.ask!.propose(r);
          return true;
        },
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    final ui = widget.ui;
    if (_stack.isEmpty || ui.screens[_stack.last.$1] == null) {
      _stack
        ..clear()
        ..add((ui.start, null));
    }
    final (name, subject) = _stack.last;
    final scr = ui.screens[name]!;
    final theme = Theme.of(context);
    final body = <Widget>[];
    final actions = <Widget>[];
    for (final c in scr.components) {
      switch (c.kind) {
        case 'questions':
          body.addAll(widget.questions);
        case 'show':
          final of = c.j['of'] as String, pos = c.j['pos'] as int?;
          final facts = of == 'me'
              ? [for (final a in s.atoms) if (c.preds.contains(_pred(a)) && _args(a).isEmpty) a]
              : subject == null
                  ? const <GTerm>[]
                  : [
                      for (final a in s.atoms)
                        if (c.preds.contains(_pred(a)) &&
                            (pos != null
                                ? _names(a, _args(subject)[pos - 1])
                                : _names(a, subject, fact: scr.fact != null)))
                          a
                    ];
          if (facts.isEmpty) break;
          body.add(Padding(
            padding: const EdgeInsets.fromLTRB(16, 8, 16, 0),
            child: Wrap(spacing: 6, runSpacing: 6, children: [
              for (final p in {for (final a in facts) _pred(a)})
                Chip(label: Text(_label(p)), visualDensity: VisualDensity.compact),
            ]),
          ));
        case 'thread':
          final lines = [
            for (final a in s.atoms)
              if (c.preds.contains(_pred(a)) && subject != null && _names(a, subject, fact: scr.fact != null))
                for (final x in _args(a))
                  if (x is GString) x.value
          ];
          body.add(Padding(
            padding: const EdgeInsets.all(16),
            child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
              if (lines.isEmpty) Text('No messages yet.', style: TextStyle(color: Colors.grey.shade600)),
              for (final l in lines)
                Container(
                  margin: const EdgeInsets.only(bottom: 6),
                  padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                  decoration: BoxDecoration(
                      color: theme.colorScheme.primaryContainer, borderRadius: BorderRadius.circular(14)),
                  child: Text(l),
                ),
            ]),
          ));
        case 'list':
          final items = _items(c, subject);
          final opens = c.j['opens'] as String?;
          body.add(Padding(
            padding: const EdgeInsets.fromLTRB(16, 14, 16, 4),
            child: Text((c.j['title'] as String).toUpperCase(),
                style: theme.textTheme.labelSmall?.copyWith(color: Colors.grey.shade600, letterSpacing: .8)),
          ));
          if (items.isEmpty) {
            body.add(Padding(
                padding: const EdgeInsets.symmetric(horizontal: 16),
                child: Text('None yet.', style: TextStyle(color: Colors.grey.shade600))));
          }
          for (final it in items) {
            final isFact = c.j['items']['facts'] != null;
            final lead = isFact
                ? (_args(it).isEmpty ? GAtom(_pred(it)) : _args(it).first)
                : it;
            final chips = [
              for (final p in c.preds)
                if (s.atoms.any((a) => _pred(a) == p && _names(a, it, fact: isFact))) _label(p)
            ];
            body.add(ListTile(
              key: describedKey('row:${_k(it)}'),
              leading: CircleAvatar(child: Text(_show(lead).isEmpty ? '?' : _show(lead)[0])),
              title: Text(_show(lead)),
              subtitle: isFact && _args(it).length > 1
                  ? Text(_args(it).skip(1).map(_show).join(' · '))
                  : chips.isEmpty
                      ? null
                      : Text(chips.join(' · ')),
              onTap: opens == null ? null : () => setState(() => _stack.add((opens, it))),
            ));
          }
        case 'action':
          final act = _act(c);
          if (act == null) break;
          final ways = _ways(c, act, subject);
          if (ways.isEmpty) break;
          actions.add(FilledButton(
            key: describedKey('act:${c.schema}${subject == null ? '' : ':${_k(subject)}'}'),
            onPressed: widget.ask?.at == null ? null : () => _tap(c, act, ways, subject),
            child: Text(c.button),
          ));
      }
    }
    final title = subject == null
        ? _show(s.me) == 'you' ? displayText(s.me) : _show(s.me)
        : _show(scr.fact != null && _args(subject).isNotEmpty ? _args(subject).first : subject);
    return Column(children: [
      Material(
        color: theme.colorScheme.surface,
        elevation: 1,
        child: SizedBox(
          height: 52,
          child: Row(children: [
            if (_stack.length > 1)
              IconButton(
                  key: describedKey('back'),
                  icon: const Icon(Icons.arrow_back),
                  onPressed: () => setState(() => _stack.removeLast()))
            else
              const SizedBox(width: 16),
            Expanded(
                child: Text(title.isEmpty ? title : title[0].toUpperCase() + title.substring(1),
                    style: theme.textTheme.titleMedium)),
          ]),
        ),
      ),
      Expanded(child: ListView(padding: const EdgeInsets.only(bottom: 8), children: body)),
      if (actions.isNotEmpty)
        SafeArea(
          top: false,
          child: Padding(
            padding: const EdgeInsets.fromLTRB(12, 6, 12, 10),
            child: Wrap(spacing: 8, runSpacing: 8, children: actions),
          ),
        ),
    ]);
  }
}

/// The sheet of an action with something left to choose or fill.
class _Sheet extends StatefulWidget {
  final String title;
  final String? sentence;
  final List<_Way> ways;
  final List<int> varying;
  final List<DeclaredField> fields;
  final Map<int, PlaceSource> sources;
  final String Function(GTerm) show;
  final List<GTerm>? Function(DeclaredField) options;
  final GTerm? Function(DeclaredField, String) textValue;
  final bool Function(_Way, Map<int, GTerm>) submit;
  const _Sheet(
      {required this.title,
      required this.sentence,
      required this.ways,
      required this.varying,
      required this.fields,
      required this.sources,
      required this.show,
      required this.options,
      required this.textValue,
      required this.submit});
  @override
  State<_Sheet> createState() => _SheetState();
}

class _SheetState extends State<_Sheet> {
  int _way = 0;
  final Map<int, GTerm> _picked = {};
  final Map<int, TextEditingController> _text = {};
  String? _error;

  @override
  void dispose() {
    for (final c in _text.values) {
      c.dispose();
    }
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final ws = widget.ways, theme = Theme.of(context);
    return Padding(
      padding: EdgeInsets.fromLTRB(20, 16, 20, 16 + MediaQuery.of(context).viewInsets.bottom),
      child: Column(mainAxisSize: MainAxisSize.min, crossAxisAlignment: CrossAxisAlignment.start, children: [
        Text(widget.title, style: theme.textTheme.titleMedium),
        if (widget.sentence != null)
          Padding(
              padding: const EdgeInsets.only(top: 4),
              child: Text(widget.sentence!, style: TextStyle(color: Colors.grey.shade700))),
        const SizedBox(height: 10),
        if (widget.varying.isNotEmpty)
          Wrap(spacing: 6, runSpacing: 6, children: [
            for (var i = 0; i < ws.length; i++)
              ChoiceChip(
                key: describedKey('pick:${widget.varying.map((p) => formatTerm(ws[i][p]!)).join(',')}'),
                label: Text(widget.varying.map((p) => widget.show(ws[i][p]!)).join(' · ')),
                selected: _way == i,
                onSelected: (_) => setState(() => _way = i),
              ),
          ]),
        for (final f in widget.fields)
          if (widget.options(f) case final opts?)
            Wrap(spacing: 6, runSpacing: 6, children: [
              for (final o in opts)
                ChoiceChip(
                  key: describedKey('pick:${formatTerm(o)}'),
                  label: Text(widget.show(o)),
                  selected: _picked[f.place] != null && formatTerm(_picked[f.place]!) == formatTerm(o),
                  onSelected: (_) => setState(() => _picked[f.place] = o),
                ),
            ])
          else
            TextField(
              key: describedKey('field:${f.place}'),
              controller: _text.putIfAbsent(f.place, TextEditingController.new),
              decoration: InputDecoration(labelText: f.type == 'key' ? '${f.label} (their key)' : f.label),
            ),
        if (_error != null) Padding(padding: const EdgeInsets.only(top: 8), child: Text(_error!, style: TextStyle(color: Colors.red.shade700))),
        const SizedBox(height: 14),
        FilledButton(
          key: describedKey('submit'),
          onPressed: () {
            final given = <int, GTerm>{..._picked};
            for (final f in widget.fields) {
              if (given.containsKey(f.place)) continue;
              final v = widget.textValue(f, _text[f.place]?.text ?? '');
              if (v == null) return setState(() => _error = 'Fill in ${f.label}.');
              given[f.place] = v;
            }
            if (widget.submit(ws[_way], given)) {
              Navigator.of(context).pop();
            } else {
              setState(() => _error = 'Not allowed with these values.');
            }
          },
          child: Text(widget.title),
        ),
      ]),
    );
  }
}
