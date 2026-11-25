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

  /// The look of a typed place: `as` long, short, slider (`from`, `to`, and `typed` for a box beside it) or date.
  final Map<String, dynamic>? look;
  const PlaceSource({this.subject = false, this.pos, this.from, this.typed = false, this.look});

  static PlaceSource of(Map<String, dynamic> j) => j.containsKey('subject')
      ? PlaceSource(subject: true, pos: j['subject'] as int?)
      : j.containsKey('from')
          ? PlaceSource(from: j['from'] as String)
          : PlaceSource(typed: true, look: j['look'] as Map<String, dynamic>?);
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

  /// The look of an action, `menu`, `bar` or `primary`, and of a list, `grid`, `rows` or `chips`.
  String? get where => j['where'] as String?;
  String? get look => j['look'] as String?;
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

  /// The look of the whole app: `theme`, `shape`, `density`, `home`, `convo`, `questions`, `actions`.
  final Map<String, String> look;
  const UiDescription(this.start, this.screens, this.personPos, [this.look = const {}]);

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
      for (final e in ((j['personPos'] ?? const <String, dynamic>{}) as Map<String, dynamic>).entries)
        e.key: [for (final p in e.value as List) p as int]
    }, {
      for (final e in ((j['look'] ?? const <String, dynamic>{}) as Map<String, dynamic>).entries) e.key: e.value as String
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

typedef DescribedWay = Map<int, GTerm>;

/// What a described screen offers, over the person's last view and her request question: the logic of the
/// screens, apart from their drawing, so that a test can walk every screen.
class DescribedCore {
  final UiDescription ui;
  final SocialState s;
  final SocialAsk? ask;
  DescribedCore(this.ui, this.s, this.ask);

  String key(GTerm t) => formatTerm(t);
  bool same(GTerm a, GTerm b) => key(a) == key(b);

  /// A person or a value as the person reads it: a key with its underscores as spaces, capitalized.
  String show(GTerm t) {
    if (same(t, s.me)) return 'you';
    final x = displayText(t).replaceAll('_', ' ');
    return x.isEmpty ? x : x[0].toUpperCase() + x.substring(1);
  }

  /// What the person reads for a predicate: the name of the list the contract gives it, or the predicate.
  String label(String p) {
    for (final l in s.lists) {
      if (l.predicate == p) return l.name;
    }
    return p.replaceAll('_', ' ');
  }

  List<GTerm> args(GTerm a) => ctorArgs(a).$2;
  String pred(GTerm a) => ctorArgs(a).$1;

  /// Whether the fact [a] names [v]: every argument of [v] where [v] is a fact, [v] itself otherwise.
  bool names(GTerm a, GTerm v, {bool fact = false}) => fact
      ? args(v).every((x) => args(a).any((y) => same(x, y)))
      : args(a).any((y) => same(v, y));

  /// The persons named in the person's state: those she knows, and those at a person's position of a fact.
  List<GTerm> everyone() {
    final out = <String, GTerm>{};
    for (final k in s.known) {
      out[key(k)] = k;
    }
    for (final a in s.atoms) {
      final ps = ui.personPos[pred(a)] ?? const [];
      final xs = args(a);
      for (final i in ps) {
        if (i <= xs.length) out[key(xs[i - 1])] = xs[i - 1];
      }
    }
    out.remove(key(s.me));
    return out.values.toList();
  }

  List<GTerm> valuesOf(String p) {
    final out = <String, GTerm>{};
    for (final a in s.atoms) {
      if (pred(a) == p) {
        for (final x in args(a)) {
          out[key(x)] = x;
        }
      }
    }
    return out.values.toList();
  }

  /// The items of a list, on a screen about [subject].
  List<GTerm> items(UiComponent c, GTerm? subject) {
    final items = c.j['items'] as Map<String, dynamic>;
    if (items['everyone'] == true) return everyone();
    if (items['from'] != null) return valuesOf(items['from'] as String);
    final p = items['facts'] as String, keep = items['keep'] as int?;
    final kept = keep != null && subject != null && keep <= args(subject).length ? args(subject)[keep - 1] : null;
    return [
      for (final a in s.atoms)
        if (pred(a) == p && (kept == null || names(a, kept))) a
    ];
  }

  /// The value of the subject the source [src] takes.
  GTerm? ofSubject(PlaceSource src, GTerm? subject) {
    if (subject == null) return null;
    if (src.pos == null) return subject;
    final xs = args(subject);
    return src.pos! <= xs.length ? xs[src.pos! - 1] : null;
  }

  /// The act of [c] at the person's role, as the platform's request type declares it.
  DeclaredAct? act(UiComponent c) {
    final o = ask?.offers;
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
  List<DescribedWay> ways(UiComponent c, DeclaredAct act, GTerm? subject) {
    final info = ActInfo(act), fixed = <int, GTerm>{};
    for (final e in c.places.entries) {
      if (e.value.subject) {
        final v = ofSubject(e.value, subject);
        if (v == null) return const [];
        fixed[e.key] = v;
      }
    }
    final ways = info.ways(s.atoms, s.me);
    final out = <DescribedWay>[];
    for (final w in ways) {
      if (fixed.entries.any((f) => w[f.key] != null && !same(w[f.key]!, f.value))) continue;
      final x = {...w, ...fixed};
      var ok = true;
      for (final e in c.places.entries) {
        final from = e.value.from;
        if (from == null || x[e.key] == null) continue;
        final allowed = from == 'everyone' ? everyone() : valuesOf(from);
        if (!allowed.any((v) => same(v, x[e.key]!))) ok = false;
      }
      // A way the act forbids, where the person's state determines it, is not offered (ActInfo.lacks).
      if (ok && info.lacks(s.atoms, {...x, act.proposer: s.me})) out.add(x);
    }
    return out;
  }

  /// The places of [act] the person fills: its fields, each from a list or typed.
  List<DeclaredField> fieldsOf(DeclaredAct act, DescribedWay w) => [
        for (final f in act.fields)
          if (w[f.place] == null) f
      ];

  /// The value a field's text gives, as the request widget forms it.
  GTerm? textValue(DeclaredField f, String text, {String? schema}) {
    final t = text.trim();
    if (t.isEmpty) return null;
    // A speech act's text is the text signed by the person, as the request widget forms it.
    final o = ask?.offers, k = schema == null || o == null ? null : o.keyOf(schema);
    final kinds = k == null ? const <String>[] : o!.kinds[k] ?? const [];
    if (f.place <= kinds.length && kinds[f.place - 1] == 'speech') {
      return GStruct('speech', [GStruct('utterance', [GString(t)]), s.me]);
    }
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
  GTerm? request(DeclaredAct act, Map<int, GTerm> at) {
    final o = ask?.offers;
    if (o == null || ask?.at == null) return null;
    final key = o.keyOf(act.schema);
    if (key == null) return null;
    final n = int.parse(key.substring(key.lastIndexOf('/') + 1));
    final args = [for (var i = 1; i <= n; i++) i == act.proposer ? (at[i] ?? s.me) : at[i]];
    if (args.contains(null)) return null;
    final full = {for (var i = 1; i <= n; i++) i: args[i - 1]!};
    final info = ActInfo(act);
    if (!info.lacks(s.atoms, full) || !info.distinct(full)) return null;
    return GStruct(act.schema, [
      for (var i = 1; i <= n; i++) o.isSet(act.schema, i) && args[i - 1] is! GList ? GList([args[i - 1]!]) : args[i - 1]!
    ]);
  }


  /// Every screen the person can reach from the start, each with its subject, to a depth of four.
  List<(String, GTerm?)> screens() {
    final out = <(String, GTerm?)>[], seen = <String>{};
    void walk(String name, GTerm? subject, int depth) {
      if (!seen.add('$name|${subject == null ? '' : key(subject)}')) return;
      out.add((name, subject));
      if (depth >= 4) return;
      for (final c in ui.screens[name]!.components) {
        final opens = c.j['opens'] as String?;
        if (c.kind != 'list' || opens == null) continue;
        for (final it in items(c, subject)) {
          walk(opens, it, depth + 1);
        }
      }
    }

    walk(ui.start, null, 0);
    return out;
  }
}

/// A way an action offers: the values of the places a way of the act binds, and of those the subject fixes.


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
  DescribedCore get c0 => DescribedCore(widget.ui, widget.state, widget.ask);

  void _toast(String m) => ScaffoldMessenger.maybeOf(context)?.showSnackBar(SnackBar(content: Text(m)));

  /// The person taps the action [c]: where nothing is left to choose or fill, the request is made; otherwise a
  /// sheet asks for what is left.
  void _tap(UiComponent c, DeclaredAct act, List<DescribedWay> ways, GTerm? subject) {
    final w0 = ways.first;
    final fields = c0.fieldsOf(act, w0);
    final varying = [
      for (final p in act.binds)
        if (ways.map((w) => w[p] == null ? '' : c0.key(w[p]!)).toSet().length > 1) p
    ];
    if (ways.length == 1 && fields.isEmpty) {
      final r = c0.request(act, w0);
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
        show: c0.show,
        options: (f) {
          final src = c.places[f.place];
          if (src?.from == null) return null;
          return src!.from == 'everyone' ? c0.everyone() : c0.valuesOf(src.from!);
        },
        textValue: (f, t) => c0.textValue(f, t, schema: act.schema),
        submit: (w, given) {
          final r = c0.request(act, {...w, ...given});
          if (r == null) return false;
          widget.ask!.propose(r);
          return true;
        },
      ),
    );
  }

  /// The text field of the screen's composer, by the screen and its subject.
  TextEditingController _composer(String k) => _fields.putIfAbsent(k, TextEditingController.new);

  /// An avatar: a person's initial in a colour of its own, a value's in grey.
  Widget _avatar(GTerm t, {double r = 18}) {
    final mine = c0.same(t, s.me);
    final name = mine ? displayText(t) : c0.show(t), person = mine || c0.everyone().any((x) => c0.same(x, t));
    const colors = [Color(0xFFEF6F4F), Color(0xFF2A9D8F), Color(0xFF8E63CF), Color(0xFFE0A030), Color(0xFF3B82C4)];
    final color = person ? colors[c0.key(t).codeUnits.fold(0, (a, b) => a + b) % colors.length] : Colors.blueGrey.shade300;
    return CircleAvatar(
        radius: r,
        backgroundColor: color,
        child: Text(name.isEmpty ? '?' : name[0].toUpperCase(),
            style: TextStyle(color: Colors.white, fontWeight: FontWeight.w600, fontSize: r * .85)));
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
    final theme = Theme.of(context), muted = TextStyle(color: Colors.grey.shade600, fontSize: 13);
    final c = c0;
    final body = <Widget>[], actions = <Widget>[], primary = <Widget>[], menu = <(String, VoidCallback)>[];
    Widget? composer;
    final thread = scr.components.any((x) => x.kind == 'thread');
    final here = '$name|${subject == null ? '' : c.key(subject)}';
    for (final comp in scr.components) {
      switch (comp.kind) {
        case 'questions':
          if (widget.questions.isNotEmpty) {
            body.add(Padding(padding: const EdgeInsets.only(top: 4), child: Column(children: widget.questions)));
          }
        case 'show':
          final of = comp.j['of'] as String, pos = comp.j['pos'] as int?;
          final facts = of == 'me'
              ? [for (final a in s.atoms) if (comp.preds.contains(c.pred(a)) && c.args(a).isEmpty) a]
              : subject == null
                  ? const <GTerm>[]
                  : [
                      for (final a in s.atoms)
                        if (comp.preds.contains(c.pred(a)) &&
                            (pos != null ? c.names(a, c.args(subject)[pos - 1]) : c.names(a, subject, fact: scr.fact != null)))
                          a
                    ];
          if (facts.isEmpty) break;
          final labels = {for (final a in facts) () { final l = c.label(c.pred(a)); return l.isEmpty ? l : l[0].toUpperCase() + l.substring(1); }()};
          body.add(Padding(
            padding: const EdgeInsets.fromLTRB(16, 10, 16, 2),
            child: Wrap(spacing: 6, runSpacing: 6, children: [
              for (final l in labels)
                Container(
                  padding: const EdgeInsets.symmetric(horizontal: 10, vertical: 3),
                  decoration: BoxDecoration(color: theme.colorScheme.primary.withValues(alpha: .10), borderRadius: BorderRadius.circular(99)),
                  child: Text(l, style: TextStyle(fontSize: 12.5, color: theme.colorScheme.primary, fontWeight: FontWeight.w600)),
                ),
            ]),
          ));
        case 'thread':
          final lines = [
            for (final a in s.atoms)
              if (comp.preds.contains(c.pred(a)) && subject != null && c.names(a, subject, fact: scr.fact != null))
                for (final x in c.args(a))
                  if (x is GString || (x is GStruct && x.functor == 'speech')) (a, x)
          ];
          body.add(Padding(
            padding: const EdgeInsets.fromLTRB(12, 12, 12, 4),
            child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
              if (lines.isEmpty) Center(child: Padding(padding: const EdgeInsets.all(24), child: Text('No messages yet.', style: muted))),
              for (final (a, x) in lines)
                Builder(builder: (_) {
                  final text = x is GString ? x.value : displayText(c.args(x).first).replaceAll('utterance', '').trim();
                  final mine = x is GStruct && x.args.length > 1 && c.same(x.args[1], s.me) || c.pred(a).startsWith('sent');
                  return Align(
                    alignment: mine ? Alignment.centerRight : Alignment.centerLeft,
                    child: Container(
                      margin: const EdgeInsets.only(bottom: 6),
                      constraints: const BoxConstraints(maxWidth: 280),
                      padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                      decoration: BoxDecoration(
                          color: mine ? theme.colorScheme.primary.withValues(alpha: .14) : Colors.grey.shade100,
                          borderRadius: BorderRadius.circular(16)),
                      child: Text(text),
                    ),
                  );
                }),
            ]),
          ));
        case 'list':
          final items = c.items(comp, subject);
          final opens = comp.j['opens'] as String?;
          final isFact = comp.j['items']['facts'] != null;
          body.add(Padding(
            padding: const EdgeInsets.fromLTRB(16, 18, 16, 6),
            child: Text((comp.j['title'] as String).toUpperCase(),
                style: theme.textTheme.labelSmall?.copyWith(color: Colors.grey.shade600, letterSpacing: .9, fontWeight: FontWeight.w600)),
          ));
          if (items.isEmpty) {
            body.add(Padding(padding: const EdgeInsets.fromLTRB(16, 0, 16, 6), child: Text('None yet.', style: muted)));
          }
          final rows = <Widget>[];
          for (final it in items) {
            final lead = isFact ? (c.args(it).isEmpty ? GAtom(c.pred(it)) : c.args(it).first) : it;
            final chips = [
              for (final p in comp.preds)
                if (s.atoms.any((a) => c.pred(a) == p && c.names(a, it, fact: isFact))) c.label(p)
            ];
            final sub = isFact && c.args(it).length > 1
                ? c.args(it).skip(1).map(c.show).join(' · ')
                : chips.join(' · ');
            if (comp.look == 'chips') {
              rows.add(ActionChip(
                  key: describedKey('row:${c.key(it)}'),
                  avatar: _avatar(lead, r: 10),
                  label: Text(c.show(lead)),
                  onPressed: opens == null ? null : () => setState(() => _stack.add((opens, it)))));
              continue;
            }
            final row = Padding(
              padding: EdgeInsets.symmetric(horizontal: comp.look == 'grid' ? 0 : 10, vertical: 3),
              child: Material(
                color: Colors.white,
                shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(14), side: BorderSide(color: Colors.grey.shade200)),
                child: ListTile(
                  key: describedKey('row:${c.key(it)}'),
                  shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(14)),
                  leading: _avatar(lead),
                  title: Text(c.show(lead), style: const TextStyle(fontWeight: FontWeight.w600)),
                  subtitle: sub.isEmpty ? null : Text(sub, style: muted),
                  trailing: opens == null ? null : Icon(Icons.chevron_right, color: Colors.grey.shade400),
                  onTap: opens == null ? null : () => setState(() => _stack.add((opens, it))),
                ),
              ),
            );
            if (comp.look == 'grid') {
              rows.add(SizedBox(width: 168, child: row));
            } else {
              body.add(row);
            }
          }
          if (rows.isNotEmpty) {
            body.add(Padding(padding: const EdgeInsets.symmetric(horizontal: 12), child: Wrap(spacing: 8, runSpacing: 4, children: rows)));
          }
        case 'action':
          final act = c.act(comp);
          if (act == null) break;
          final ways = c.ways(comp, act, subject);
          if (ways.isEmpty) break;
          final open = widget.ask?.at != null;
          final fields = c.fieldsOf(act, ways.first);
          // On a conversation, the act of one way and one text field is its composer; the other acts go in a menu.
          if (thread && composer == null && ways.length == 1 && fields.length == 1 && fields.single.type == 'text' && comp.places[fields.single.place]?.from == null) {
            final f = fields.single, tc = _composer(here);
            void send() {
              final v = c.textValue(f, tc.text, schema: act.schema);
              if (v == null) return;
              final r = c.request(act, {...ways.first, f.place: v});
              if (r == null) return _toast('${comp.button}: not allowed now');
              widget.ask!.propose(r);
              tc.clear();
            }

            composer = Padding(
              padding: const EdgeInsets.fromLTRB(10, 6, 10, 10),
              child: Row(children: [
                Expanded(
                  child: TextField(
                    key: describedKey('compose:${comp.schema}'),
                    controller: tc,
                    enabled: open,
                    minLines: 1,
                    maxLines: comp.places[f.place]?.look?['as'] == 'long' ? 4 : 1,
                    onSubmitted: (_) => send(),
                    decoration: InputDecoration(
                        hintText: comp.button,
                        isDense: true,
                        filled: true,
                        fillColor: Colors.grey.shade100,
                        border: OutlineInputBorder(borderRadius: BorderRadius.circular(22), borderSide: BorderSide.none)),
                  ),
                ),
                IconButton.filled(key: describedKey('send:${comp.schema}'), onPressed: open ? send : null, icon: const Icon(Icons.send, size: 18)),
              ]),
            );
            break;
          }
          void go() => _tap(comp, act, ways, subject);
          final k = describedKey('act:${comp.schema}${subject == null ? '' : ':${c.key(subject)}'}');
          // The look puts an action in the menu, in the bar, or first and marked; on a conversation, the menu.
          final where = comp.where ?? (thread ? 'menu' : 'bar');
          if (where == 'menu') {
            // The menu's actions are marked on the screen, so that a walk finds what it offers.
            menu.add((comp.button, go));
            body.add(SizedBox.shrink(key: k));
          } else if (where == 'primary') {
            primary.add(FilledButton(key: k, onPressed: open ? go : null, child: Text(comp.button)));
          } else {
            actions.add(actions.whereType<ButtonStyleButton>().isEmpty && primary.isEmpty && comp.where == null
                ? FilledButton(key: k, onPressed: open ? go : null, child: Text(comp.button))
                : OutlinedButton(key: k, onPressed: open ? go : null, child: Text(comp.button)));
          }
      }
    }
    final title = subject == null
        ? c.show(s.me) == 'you' ? displayText(s.me) : c.show(s.me)
        : c.show(scr.fact != null && c.args(subject).isNotEmpty ? c.args(subject).first : subject);
    final shown = title.isEmpty ? title : title[0].toUpperCase() + title.substring(1);
    return Theme(
      data: _looked(theme),
      child: ColoredBox(
      color: _background(),
      child: Column(children: [
        Material(
          color: Colors.white,
          child: Container(
            height: 56,
            decoration: BoxDecoration(border: Border(bottom: BorderSide(color: Colors.grey.shade200))),
            child: Row(children: [
              if (_stack.length > 1)
                IconButton(key: describedKey('back'), icon: const Icon(Icons.arrow_back_ios_new, size: 18), onPressed: () => setState(() => _stack.removeLast()))
              else
                const SizedBox(width: 14),
              _avatar(subject == null ? s.me : (scr.fact != null && c.args(subject).isNotEmpty ? c.args(subject).first : subject), r: 16),
              const SizedBox(width: 10),
              Expanded(child: Text(shown, style: theme.textTheme.titleMedium?.copyWith(fontWeight: FontWeight.w600))),
              if (menu.isNotEmpty)
                PopupMenuButton<int>(
                  key: describedKey('menu'),
                  icon: const Icon(Icons.more_horiz),
                  onSelected: (i) => menu[i].$2(),
                  itemBuilder: (_) => [for (var i = 0; i < menu.length; i++) PopupMenuItem(value: i, child: Text(menu[i].$1))],
                ),
            ]),
          ),
        ),
        Expanded(child: ListView(padding: const EdgeInsets.only(bottom: 10), children: body)),
        if (composer != null)
          Material(color: Colors.white, child: SafeArea(top: false, child: composer))
        else if (actions.whereType<ButtonStyleButton>().isNotEmpty || primary.isNotEmpty)
          Material(
            color: Colors.white,
            child: SafeArea(
              top: false,
              child: Container(
                width: double.infinity,
                decoration: BoxDecoration(border: Border(top: BorderSide(color: Colors.grey.shade200))),
                padding: const EdgeInsets.fromLTRB(12, 10, 12, 12),
                child: Wrap(spacing: 8, runSpacing: 8, children: [...primary, ...actions]),
              ),
            ),
          ),
      ]),
    ));
  }

  /// The look of the whole app: a theme's colors, and square corners.
  static const _seeds = {'fresh': Color(0xFF14806E), 'night': Color(0xFF2A9D8F), 'paper': Color(0xFFB4532A), 'candy': Color(0xFF8E44E0)};
  ThemeData _looked(ThemeData t) {
    final look = widget.ui.look, seed = _seeds[look['theme']];
    var d = t;
    if (seed != null) {
      d = d.copyWith(colorScheme: ColorScheme.fromSeed(seedColor: seed, brightness: look['theme'] == 'night' ? Brightness.dark : Brightness.light));
    }
    if (look['shape'] == 'square') {
      final sq = RoundedRectangleBorder(borderRadius: BorderRadius.circular(4));
      d = d.copyWith(
          filledButtonTheme: FilledButtonThemeData(style: FilledButton.styleFrom(shape: sq)),
          outlinedButtonTheme: OutlinedButtonThemeData(style: OutlinedButton.styleFrom(shape: sq)));
    }
    return d;
  }

  Color _background() => switch (widget.ui.look['theme']) {
        'night' => const Color(0xFF14201E),
        'paper' => const Color(0xFFF4EFE6),
        'candy' => const Color(0xFFF7F1FD),
        _ => const Color(0xFFF6F6F4),
      };
}

/// The sheet of an action with something left to choose or fill.
class _Sheet extends StatefulWidget {
  final String title;
  final String? sentence;
  final List<DescribedWay> ways;
  final List<int> varying;
  final List<DeclaredField> fields;
  final Map<int, PlaceSource> sources;
  final String Function(GTerm) show;
  final List<GTerm>? Function(DeclaredField) options;
  final GTerm? Function(DeclaredField, String) textValue;
  final bool Function(DescribedWay, Map<int, GTerm>) submit;
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

  /// A typed place, drawn as its look says: a long box, a slider (with a box beside it when also typed) or a
  /// calendar, whose day is the number of days since 1970; a short box otherwise.
  Widget _field(DeclaredField f) {
    final look = widget.sources[f.place]?.look ?? const {}, tc = _text.putIfAbsent(f.place, TextEditingController.new);
    final label = f.type == 'key' ? '${f.label} (their key)' : f.label;
    switch (look['as']) {
      case 'slider':
        final lo = (look['from'] as num).toDouble(), hi = (look['to'] as num).toDouble();
        final v = (double.tryParse(tc.text) ?? lo).clamp(lo, hi);
        if (tc.text.isEmpty) tc.text = lo.round().toString();
        return Row(children: [
          Text(label),
          Expanded(
            child: Slider(
                key: describedKey('slider:${f.place}'),
                min: lo,
                max: hi,
                divisions: (hi - lo).round().clamp(1, 1000),
                value: v,
                label: v.round().toString(),
                onChanged: (x) => setState(() => tc.text = x.round().toString())),
          ),
          if (look['typed'] == true)
            SizedBox(width: 72, child: TextField(key: describedKey('field:${f.place}'), controller: tc, keyboardType: TextInputType.number, onChanged: (_) => setState(() {})))
          else
            SizedBox(width: 40, child: Text(tc.text, textAlign: TextAlign.end)),
        ]);
      case 'date':
        final days = int.tryParse(tc.text);
        return OutlinedButton.icon(
          key: describedKey('date:${f.place}'),
          icon: const Icon(Icons.event, size: 18),
          label: Text(days == null ? label : DateTime.fromMillisecondsSinceEpoch(days * 86400000, isUtc: true).toIso8601String().substring(0, 10)),
          onPressed: () async {
            final now = DateTime.now();
            final d = await showDatePicker(context: context, firstDate: DateTime(1970), lastDate: DateTime(now.year + 50), initialDate: now);
            if (d != null) setState(() => tc.text = (DateTime.utc(d.year, d.month, d.day).millisecondsSinceEpoch ~/ 86400000).toString());
          },
        );
      default:
        return TextField(
          key: describedKey('field:${f.place}'),
          controller: tc,
          minLines: look['as'] == 'long' ? 3 : 1,
          maxLines: look['as'] == 'long' ? 6 : 1,
          decoration: InputDecoration(labelText: label),
        );
    }
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
            _field(f),
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
