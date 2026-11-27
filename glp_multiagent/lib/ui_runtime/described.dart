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

import 'dart:async';
import 'dart:convert';

import 'package:flutter/foundation.dart' show setEquals;
import 'package:flutter/material.dart';
import 'package:glp_runtime/vglp/canonical.dart' show ActCondition, DeclaredAct, DeclaredField;

import 'named_widgets.dart';
import 'qr_code.dart';
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

  /// The name the sentence gives the place, which the steps of a chain share.
  final String? name;

  /// For a place picked from a list whose name the screen's subject also has: the position of the subject's fact
  /// that holds it, to whose value the choice is kept; 0 where the name is the subject's own.
  final int? same;
  const PlaceSource({this.subject = false, this.pos, this.from, this.typed = false, this.look, this.name, this.same});

  static PlaceSource of(Map<String, dynamic> j) => j.containsKey('subject')
      ? PlaceSource(subject: true, pos: j['subject'] as int?, name: j['name'] as String?)
      : j.containsKey('from')
          ? PlaceSource(from: j['from'] as String, name: j['name'] as String?, same: j['same'] as int?)
          : PlaceSource(typed: true, look: j['look'] as Map<String, dynamic>?, name: j['name'] as String?);
}

/// A component of a screen, by its [kind]: `questions`, `show`, `thread`, `list`, `action`, or `chain`, one button
/// that takes the acts of its [steps] in turn.
class UiComponent {
  final String kind;
  final Map<String, dynamic> j;
  UiComponent(this.kind, this.j);

  List<String> get preds => [for (final p in (j['preds'] ?? j['with'] ?? const []) as List) p as String];
  String get schema => j['schema'] as String;
  String get button => j['button'] as String;

  /// What the button reads: the plain label its UI style calls it by, or its name.
  String get label => (j['label'] as String?) ?? button;

  /// The screen the person is taken to once the act is done: the start screen or a landmark.
  String? get then => j['then'] as String?;

  /// THEN IT: the screen opened once the act is done, `screen`, about the person's newest fact of `pred`, or its
  /// value where `of` is `value`, the way Back going to `from`.
  Map<String, dynamic>? get thenIt => j['thenIt'] as Map<String, dynamic>?;

  /// Whether the UI style marks the action IMPORTANT: where it is not offered, what is in its way is shown in its
  /// place, with the first step of a way to it.
  bool get important => j['important'] == true;

  /// The actions of a chain, in turn.
  List<UiComponent> get steps => [for (final x in (j['steps'] ?? const []) as List) UiComponent('action', x as Map<String, dynamic>)];
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

/// What an act does to the facts of the persons it binds, from the contract's effects: the predicates of the facts
/// it adds and of those it removes.
typedef UiEffect = ({String button, List<String> adds, List<String> removes});

/// An act that adds a fact of a predicate, and whether it adds it to the person who takes it ([UiEffect]).
typedef UiAdder = ({String schema, String button, bool mine});

/// An interface description, compiled with its contract.
class UiDescription {
  /// Each act's effects, by its schema.
  final Map<String, UiEffect> effects;

  /// The acts that add each predicate's facts.
  final Map<String, List<UiAdder>> addedBy;

  final String start;
  final Map<String, UiScreen> screens;

  /// The landmarks: the other screens about the person herself, each reached from every screen.
  final List<String> landmarks;

  /// The positions of each predicate's facts that hold persons.
  final Map<String, List<int>> personPos;

  /// The look of the whole app: `theme`, `shape`, `density`, `home`, `convo`, `questions`, `actions`.
  final Map<String, String> look;

  /// Whether a place anywhere is drawn as scanned: the person then has a code to show.
  bool get scanned => screens.values.any((sc) => sc.components.any((c) => [c, ...c.steps]
      .any((x) => (x.kind == 'action' || x.kind == 'chain') && x.j['places'] != null && x.places.values.any((p) => p.look?['as'] == 'scanned'))));

  /// The contract's phrase of each predicate, its first variable the holder: "x trusts y as a developer".
  final Map<String, ({String phrase, List<String> vars})> vocab;
  const UiDescription(this.start, this.screens, this.personPos,
      [this.look = const {}, this.landmarks = const [], this.effects = const {}, this.addedBy = const {}, this.vocab = const {}]);

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
    }, [for (final l in (j['landmarks'] ?? const []) as List) l as String], {
      for (final e in ((j['effects'] ?? const <String, dynamic>{}) as Map<String, dynamic>).entries)
        e.key: (
          button: (e.value as Map<String, dynamic>)['button'] as String,
          adds: [for (final x in (e.value['adds'] ?? const []) as List) x as String],
          removes: [for (final x in (e.value['removes'] ?? const []) as List) x as String],
        )
    }, {
      for (final e in ((j['addedBy'] ?? const <String, dynamic>{}) as Map<String, dynamic>).entries)
        e.key: [
          for (final x in e.value as List)
            (schema: x['schema'] as String, button: x['button'] as String, mine: x['mine'] == true)
        ]
    }, {
      for (final e in ((j['vocab'] ?? const <String, dynamic>{}) as Map<String, dynamic>).entries)
        e.key: (
          phrase: (e.value as Map<String, dynamic>)['phrase'] as String,
          vars: [for (final v in (e.value['vars'] ?? const []) as List) v as String]
        )
    });
  }

  /// A fact of the person in the contract's words, where its vocabulary has a phrase: the holder read "you", a
  /// place not chosen "someone", the verb after "you" without its s ("you trust", "you have", "you are").
  String? phrase(String pred, List<String> args) {
    final v = vocab[pred];
    if (v == null || v.vars.isEmpty) return null;
    final val = {v.vars.first: 'you'};
    for (var i = 1; i < v.vars.length && i - 1 < args.length; i++) {
      val[v.vars[i]] = args[i - 1];
    }
    var out = v.phrase.replaceAllMapped(RegExp(r'\b\w+\b'), (m) => val[m[0]!] ?? m[0]!);
    out = out.replaceAllMapped(RegExp(r'^you (\w+)'), (m) {
      final w = m[1]!;
      final verb = switch (w) {
        'has' => 'have',
        'is' => 'are',
        'does' => 'do',
        _ => w.endsWith('ss') || !w.endsWith('s') ? w : w.endsWith('ies') ? '${w.substring(0, w.length - 3)}y' : w.substring(0, w.length - 1),
      };
      return 'you $verb';
    });
    return out;
  }

  /// Whether the act [schema] only removes facts, ends something: it is taken on a second tap.
  bool ends(String schema) {
    final e = effects[schema];
    return e != null && e.adds.isEmpty && e.removes.isNotEmpty;
  }

  /// The components of [screen] as drawn, each with the steps after it: a chain as its first step under its own
  /// button, label and look, its other steps asked in turn.
  static List<(UiComponent, List<UiComponent>)> drawn(UiScreen screen) => [
        for (final x in screen.components)
          if (x.kind == 'chain' && x.steps.isNotEmpty)
            (
              UiComponent('action', {
                ...x.steps.first.j,
                'button': x.button,
                'label': x.label,
                'where': x.where,
                'then': x.then,
                'thenIt': x.thenIt,
                'important': x.important,
                'chain': true,
              }),
              x.steps.sublist(1)
            )
          else
            (x, const <UiComponent>[])
      ];

  /// The key of an action as drawn ([drawn]): of its act, or of its chain.
  static String keyOf(UiComponent c) => c.j['chain'] == true ? 'chain:${c.button}' : 'act:${c.schema}';
}

/// The interface description the screens below it are drawn from.
class DescribedUi extends InheritedWidget {
  final UiDescription ui;

  /// The person's routing rules, where the host keeps them ([DescribedScreen.rules]).
  final ValueNotifier<Map<String, bool>>? rules;

  /// Where the app is, as the host names it ([DescribedScreen.where]).
  final List<String> where;
  const DescribedUi({super.key, required this.ui, this.rules, this.where = const [], required super.child});

  static DescribedUi? maybeOf(BuildContext context) =>
      context.dependOnInheritedWidgetOfExactType<DescribedUi>();

  @override
  bool updateShouldNotify(DescribedUi old) => old.ui != ui || old.rules != rules || old.where != where;
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

  /// The routing rules the person chose, by schema, yes or no: after she takes, on the start screen, an act of one
  /// place, herself, that every other act she may take needs (as Establish), she is asked once "Do “Establish” for
  /// you in each app you open?".  Within one app nothing more is done with the answer; a host that opens apps (the
  /// super-app) listens here, and where an app it opens offers the act and the answer is yes, takes it for her
  /// there: that hook is where opening an app applies the rule.
  final ValueNotifier<Map<String, bool>>? rules;

  /// Where the app is, as its host names it: the super-app and the app ("Grassroots", "Chess"), the first of the
  /// line that says where the person is; empty where the app runs alone.
  final List<String> where;
  const DescribedScreen(
      {super.key,
      required this.ui,
      required this.state,
      required this.ask,
      required this.questions,
      this.rules,
      this.where = const []});

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

  /// The items of a list in the order its UI style gives: `newest` first, `oldest` first, or `alpha` by how they
  /// are shown; with none, as the person's facts hold them.
  List<GTerm> ordered(UiComponent c, List<GTerm> xs) => switch (c.j['order']) {
        'newest' => xs.reversed.toList(),
        'alpha' => [...xs]..sort((a, b) => show(a).compareTo(show(b))),
        _ => xs,
      };

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
        // A choice kept to the screen's own value.
        final k = e.value.same;
        // The position 0 is the subject itself, a person's screen keeping a choice to that person.
        if (k != null && subject != null) {
          final want = k == 0 ? subject : (k <= args(subject).length ? args(subject)[k - 1] : null);
          if (want == null || !same(want, x[e.key]!)) ok = false;
        }
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

    for (final root in [ui.start, ...ui.landmarks]) {
      walk(root, null, 0);
    }
    return out;
  }

  /// The name a typed place starts filled with, its UI style's `NEW AS Stem1`: the stem and the first number
  /// from the one given whose name is no name in the person's facts.
  String? freshName(Map<String, dynamic>? look) {
    final f = look?['fresh'] as Map<String, dynamic>?;
    if (f == null) return null;
    final stem = f['stem'] as String;
    final taken = {
      for (final a in s.atoms)
        for (final x in args(a)) displayText(x).toLowerCase()
    };
    var n = f['n'] as int;
    while (taken.contains('$stem$n'.toLowerCase())) {
      n++;
    }
    return '$stem$n';
  }

  /// The lines of a conversation drawn from the predicates [preds], about [subject]: each fact's text or speech
  /// act, with the fact.
  List<(GTerm, GTerm)> lines(List<String> preds, GTerm? subject, {bool fact = false}) => [
        for (final a in s.atoms)
          if (preds.contains(pred(a)) && (subject == null || names(a, subject, fact: fact)))
            for (final x in args(a))
              if (x is GString || (x is GStruct && x.functor == 'speech')) (a, x)
      ];

  /// The text of a line, and whether the person wrote it.
  (String, bool) line(GTerm a, GTerm x) {
    final text = x is GString ? x.value : displayText(args(x).first).replaceAll('utterance', '').trim();
    final mine = x is GStruct && x.args.length > 1 && same(x.args[1], s.me) || pred(a).startsWith('sent');
    return (text, mine);
  }

  /// Who wrote a line the person did not: the signer of its speech act, or the first person its fact names.
  GTerm? author(GTerm a, GTerm x) {
    if (x is GStruct && x.args.length > 1) return x.args[1];
    final ps = ui.personPos[pred(a)] ?? const [], xs = args(a);
    for (final i in ps) {
      if (i <= xs.length && !same(xs[i - 1], s.me)) return xs[i - 1];
    }
    return null;
  }

  /// Whether a notice or a pending approval names the request [r]: its schema and, of its arguments, those [r] has.
  bool namesRequest(GTerm n, GTerm r) {
    if (pred(n) != pred(r)) return false;
    if (same(n, r)) return true;
    final ks = {for (final x in args(r)) key(x)};
    return args(n).every((x) => ks.contains(key(x)) || x is GList);
  }

  // ---------- what is in the way of an act, and the way to it ----------

  /// Every act at the person's role the platform declares, one by schema.
  List<DeclaredAct> get declared {
    final out = <String, DeclaredAct>{};
    for (final b in ask?.offers.buttons ?? const []) {
      for (final a in b.acts) {
        out.putIfAbsent(a.schema, () => a);
      }
    }
    return out.values.toList();
  }

  /// The label the act [schema] goes by: that of its action on [screen], else on the start screen, else, where
  /// [anywhere], on any screen, a chain's step's own; else its button.  A label kept to another screen
  /// (`Join ON Group`) is not the act's name off it, so a fill hint takes the button there.
  String actLabel(String schema, {String? screen, String? button, bool anywhere = true}) {
    String? on(UiScreen? sc) {
      for (final c in sc?.components ?? const <UiComponent>[]) {
        if (c.kind == 'action' && c.schema == schema) return c.label;
        if (c.kind == 'chain') {
          for (final st in c.steps) {
            if (st.schema == schema) return st.label;
          }
        }
      }
      return null;
    }

    return on(ui.screens[screen]) ??
        on(ui.screens[ui.start]) ??
        (anywhere ? ui.screens.values.map(on).nonNulls.firstOrNull : null) ??
        button ??
        ui.effects[schema]?.button ??
        schema;
  }

  /// How the items of an empty list come, from the contract: the acts that add a fact of its kind, each by its
  /// label, with "(by someone else)" where it adds the fact to another than the person who takes it.
  String fillHint(UiComponent list, {String? screen}) {
    final items = list.j['items'] as Map<String, dynamic>? ?? const {};
    final p = (items['facts'] ?? items['from']) as String?;
    final by = <String>{
      for (final a in ui.addedBy[p] ?? const <UiAdder>[])
        '“${actLabel(a.schema, screen: screen, button: a.button, anywhere: false)}”${a.mine ? '' : ' (by someone else)'}'
    };
    return by.isEmpty ? '' : ' They come with ${by.join(', ')}.';
  }

  static bool _determined(ActCondition c, Map<int, GTerm> at) => c.args
      .every((x) => x is int ? at.containsKey(x) : x is (int, int) && at.containsKey(x.$1) && at.containsKey(x.$2));

  /// What is in the way of [act] with the places [fixed] given, the person at her role, the other places open:
  /// the first fact it requires there that her state lacks, with the places the facts before it bind, or else the
  /// first fact it forbids that her state holds (`held`); null where nothing is.  A fact of a person not yet
  /// chosen is no fact of hers, and the platform's request type holds none.  A condition on values (a guard) is not
  /// looked at.
  ({ActCondition c, Map<int, GTerm> at, bool held})? inWay(DeclaredAct act, Map<int, GTerm> fixed) {
    final info = ActInfo(act), reqs = [for (final c in act.pre) if (c.kind == 'req') c];
    var best = -1;
    var bestAt = <int, GTerm>{};
    Map<int, GTerm>? clear, blocked;
    void from(int i, Map<int, GTerm> at) {
      if (clear != null) return;
      if (i > best) {
        best = i;
        bestAt = at;
      }
      if (i == reqs.length) {
        if (info.lacks(s.atoms, at) && info.distinct(at)) {
          clear = at;
        } else {
          blocked ??= at;
        }
        return;
      }
      for (final a in s.atoms) {
        final next = ActInfo.matched(a, reqs[i], at);
        if (next != null) from(i + 1, next);
      }
    }

    from(0, {...fixed, act.proposer: s.me});
    if (clear != null) return null;
    if (best < reqs.length) return (c: reqs[best], at: bestAt, held: false);
    final at = blocked ?? bestAt;
    for (final c in act.pre) {
      if (c.kind != 'forb' && c.kind != 'unless') continue;
      if (c.kind == 'forb' && !_determined(c, at)) continue;
      if (s.atoms.any((a) => ActInfo.matched(a, c, at) != null)) return (c: c, at: at, held: true);
    }
    return null;
  }

  /// The fact of [w] in words, said of the person, a place not bound read "someone": "you have an identity".
  String factSaid(({ActCondition c, Map<int, GTerm> at, bool held}) w) {
    final xs = [
      for (final x in w.c.args)
        x is int && w.at[x] != null ? w.at[x]! : SocialNotice.someone
    ];
    final said = ui.phrase(w.c.pred, [for (final x in xs) same(x, SocialNotice.someone) ? 'someone' : show(x)]);
    if (said != null) return said;
    final atom = xs.isEmpty ? GAtom(w.c.pred) : GStruct(w.c.pred, xs);
    return SocialNotice.factWords(atom, s.me, true, (q) => same(q, s.me));
  }

  /// [w] as what is in the way: "it needs that you have an identity", or "you and Bob are friends now".
  String whySaid(({ActCondition c, Map<int, GTerm> at, bool held}) w) =>
      w.held ? '${factSaid(w)} now' : 'it needs that ${factSaid(w)}';

  /// The actions of [schema] the person reaches from the screen [here]: on it, on the start screen and on the
  /// landmarks, a step of a chain by its chain ([UiDescription.drawn]), each with the screen it is on.
  /// And on the screen about a value [about] names, such as the developer one must trust: the step is taken there,
  /// `about` its subject.
  List<({String at, UiComponent c, List<UiComponent> rest, GTerm? about})> placings(String schema, String here,
      [List<GTerm> about = const []]) {
    final out = <({String at, UiComponent c, List<UiComponent> rest, GTerm? about})>[];
    void add(String at, GTerm? of) {
      final sc = ui.screens[at];
      if (sc == null) return;
      for (final (c, rest) in UiDescription.drawn(sc)) {
        if (c.kind != 'action') continue;
        final steps = [c, ...rest];
        if (steps.any((x) => x.schema == schema)) out.add((at: at, c: c, rest: rest, about: of));
      }
    }
    for (final at in {here, ui.start, ...ui.landmarks}) {
      add(at, null);
    }
    for (final v in about) {
      for (final e in ui.screens.entries) {
        if (e.key == here || e.key == ui.start || ui.landmarks.contains(e.key) || !e.value.about || e.value.fact != null) continue;
        add(e.key, v);
      }
    }
    return out;
  }

  /// The places of [c] its screen's [subject] fixes; null where one is not fixed.
  Map<int, GTerm>? fixedBy(UiComponent c, GTerm? subject) {
    final out = <int, GTerm>{};
    for (final e in c.places.entries) {
      if (!e.value.subject) continue;
      final v = ofSubject(e.value, subject);
      if (v == null) return null;
      out[e.key] = v;
    }
    return out;
  }

  /// The way to [act], not offered on the screen [here] about [subject]: what is in its way ([inWay]), and the
  /// person's own acts that add that fact (or remove it, where it is in the way by holding), searched backward, each
  /// act's own missing fact in turn, three deep; the first act on the way she can take now, from this screen, the
  /// start screen or a landmark, is `first`, the acts after it `then`.  A fact no act of hers adds comes when
  /// another person takes an act that adds it, `other`.  Null where nothing is in the way, or the act is in the
  /// way of itself, done already.
  ({
    String why,
    ({String at, UiComponent c, List<UiComponent> rest, DeclaredAct act, List<DescribedWay> ways, GTerm? about})? first,
    List<String> then,
    String? other
  })? wayTo(DeclaredAct act, Map<int, GTerm> fixed, String here, GTerm? subject, {int depth = 1}) {
    final w = inWay(act, fixed);
    if (w == null) return null;
    if (w.held && (ui.effects[act.schema]?.adds ?? const []).contains(w.c.pred)) return null;
    final why = whySaid(w);
    if (depth > 3) return (why: why, first: null, then: const [], other: null);
    final bound = [
      for (final x in w.c.args)
        if (x is int && w.at[x] != null && !same(w.at[x]!, s.me)) w.at[x]!
    ];
    final mine = w.held
        ? [
            for (final e in ui.effects.entries)
              if (e.value.removes.contains(w.c.pred)) e.key
          ]
        : [
            for (final a in ui.addedBy[w.c.pred] ?? const <UiAdder>[])
              if (a.mine) a.schema
          ];
    final tried = <({String at, UiComponent c, List<UiComponent> rest, DeclaredAct act, Map<int, GTerm> fixed, GTerm? subject})>[];
    for (final schema in mine) {
      if (schema == act.schema) continue;
      for (final p in placings(schema, here, bound)) {
        final a2 = this.act(p.c);
        if (a2 == null) continue;
        final sub = p.about ?? (p.at == here ? subject : null), fx = fixedBy(p.c, sub);
        if (fx == null) continue;
        tried.add((at: p.at, c: p.c, rest: p.rest, act: a2, fixed: fx, subject: sub));
        final ways = this.ways(p.c, a2, sub);
        // The way brings the fact about the values it names, or leaves a place to fill with them.
        final fit = [
          for (final v in ways)
            if (fieldsOf(a2, v).isNotEmpty || bound.every((b) => v.values.any((x) => same(x, b)))) v
        ];
        if (fit.isNotEmpty) {
          return (why: why, first: (at: p.at, c: p.c, rest: p.rest, act: a2, ways: fit, about: p.about), then: const [], other: null);
        }
      }
    }
    for (final t in tried) {
      final r = wayTo(t.act, t.fixed, here, t.subject, depth: depth + 1);
      if (r != null && r.first != null) return (why: why, first: r.first, then: [...r.then, t.c.label], other: null);
    }
    if (!w.held) {
      for (final a in ui.addedBy[w.c.pred] ?? const <UiAdder>[]) {
        if (!a.mine) return (why: why, first: null, then: const [], other: actLabel(a.schema, screen: here, button: a.button));
      }
    }
    return (why: why, first: null, then: const [], other: null);
  }

  /// The act [schema] is one every other act the person may take needs: it adds a fact each of them requires.
  bool neededByAll(String schema) {
    final adds = ui.effects[schema]?.adds ?? const [];
    final others = [for (final a in declared) if (a.schema != schema) a];
    return adds.isNotEmpty &&
        others.isNotEmpty &&
        others.every((a) => a.pre.any((c) => c.kind == 'req' && adds.contains(c.pred)));
  }

  /// The words of [act]'s request form with the values of [w]: a place the person's "you", another its value, or
  /// "…" where open; null where the form has no words.
  String? said(DeclaredAct act, DescribedWay w) {
    if (act.text.isEmpty) return null;
    final t = [
      for (final x in act.text)
        x is int ? (x == act.proposer ? 'you' : w[x] == null ? '…' : show(w[x]!)) : x as String
    ].join(' ').replaceAll(RegExp(r' +([,.?!])'), r'$1');
    return t.isEmpty ? null : t[0].toUpperCase() + t.substring(1);
  }

  /// Help: each act the UI structure places, by its label; its words; the screens offering it; and whether the
  /// person can take it now, or what is in its way.
  List<({String schema, String label, String? sentence, List<String> screens, String status})> help() {
    final where = <String, List<String>>{}, comp = <String, UiComponent>{};
    for (final sc in ui.screens.values) {
      for (final c in sc.components) {
        for (final a in c.kind == 'chain' ? c.steps : c.kind == 'action' ? [c] : const <UiComponent>[]) {
          final on = where[a.schema] ??= [];
          if (!on.contains(sc.name)) on.add(sc.name);
          comp.putIfAbsent(a.schema, () => a);
        }
      }
    }
    final acts = {for (final a in declared) a.schema: a};
    return [
      for (final e in where.entries)
        if (acts[e.key] case final act?)
          () {
            final c = comp[e.key]!, now = ActInfo(act).ways(s.atoms, s.me).isNotEmpty;
            final w = now ? null : inWay(act, const {});
            return (
              schema: e.key,
              label: actLabel(e.key),
              sentence: c.sentence ?? said(act, const {}),
              screens: e.value,
              status: now
                  ? 'You can do this now.'
                  : w == null
                      ? 'Not now.'
                      : 'Not now: ${whySaid(w)}.'
            );
          }()
    ];
  }
}

/// A way an action offers: the values of the places a way of the act binds, and of those the subject fixes.


/// An act the person asked for, followed until it is done or refused: the request, the button's label, the screen
/// to go to once done, the steps of its chain still to take, the values by the names its sentence gives them, the
/// subject of its screen, the person's facts when it was asked, and whether it was seen pending.
class _Asked {
  final GTerm req;
  final String label;
  final String? then;
  final List<UiComponent> rest;
  final Map<String, GTerm> named;
  final GTerm? subject;
  final Set<String> atoms;

  /// THEN IT, where the action says it ([UiComponent.thenIt]); and whether it was asked on the start screen.
  final Map<String, dynamic>? thenIt;
  final bool onStart;
  bool pending = false;
  _Asked(this.req, this.label, this.then, this.rest, this.named, this.subject, this.atoms, this.thenIt, this.onStart);
}

class _DescribedScreenState extends State<DescribedScreen> {
  /// The screens the person came through, each with its subject; the last is on the screen.
  final List<(String, GTerm?)> _stack = [];

  /// The label of the action a hint sent the person to this screen for, drawn lit.
  String? _glow;

  /// The person's code, for another phone to scan: her key, as the app says it.
  void _myCode() => showModalBottomSheet<void>(
      context: context,
      builder: (ctx) => Padding(
            key: describedKey('mycode:sheet'),
            padding: const EdgeInsets.all(20),
            child: Column(mainAxisSize: MainAxisSize.min, children: [
              Text('My code', style: Theme.of(ctx).textTheme.titleMedium),
              const SizedBox(height: 12),
              QrView(formatTerm(widget.state.me)),
              const SizedBox(height: 8),
              Text('Another phone scans it to give your key.', style: TextStyle(color: Colors.grey.shade700)),
            ]),
          ));

  /// The line that says where the person is, each step a tap back to it.
  Widget _crumbLine(List<(String, VoidCallback?)> crumbs, ThemeData theme) {
    final small = theme.textTheme.bodySmall?.copyWith(color: Colors.grey.shade600, fontSize: 11.5);
    return SingleChildScrollView(
      key: describedKey('where'),
      scrollDirection: Axis.horizontal,
      child: Row(children: [
        for (var i = 0; i < crumbs.length; i++) ...[
          if (i > 0) Text('  ›  ', style: small?.copyWith(color: Colors.grey.shade400)),
          crumbs[i].$2 == null
              ? Text(crumbs[i].$1, style: small)
              : GestureDetector(
                  key: describedKey('where:$i'), onTap: crumbs[i].$2, child: Text(crumbs[i].$1, style: small?.copyWith(color: Colors.grey.shade800))),
        ],
      ]),
    );
  }

  /// [button] lit, where a hint sent the person here for the action [label].
  Widget _lit(String label, Widget button) => label != _glow
      ? button
      : DecoratedBox(
          key: describedKey('lit:$label'),
          decoration: BoxDecoration(
              borderRadius: BorderRadius.circular(24), boxShadow: [BoxShadow(color: Colors.amber.shade300, spreadRadius: 3)]),
          child: button);
  final Map<String, TextEditingController> _fields = {};

  /// The acts asked and not yet done or refused.
  final List<_Asked> _asked = [];

  /// The notes the layer tells the person: after each act she asked, that it is done or why it is not; and each new
  /// message.  Each fades after a while.
  final List<({String text, bool bad})> _notes = [];
  final List<Timer> _timers = [];

  /// The lines of conversations the person has seen, those she was told of, and the refusals she was told of.
  final Set<String> _seen = {}, _told = {}, _toldRefusals = {};

  /// The items of each list its UI style tells of (`TOLD`), by its title, as last seen.
  final Map<String, Set<String>> _known = {};

  /// The routing rules the person chose ([DescribedScreen.rules]), and the one she is being asked about.
  late final ValueNotifier<Map<String, bool>> _rules = widget.rules ?? ValueNotifier(<String, bool>{});
  ({String schema, String label})? _rule;

  /// The lists the UI style tells of, on the screens about the person herself.
  Iterable<UiComponent> get _toldLists => [
        for (final sc in [widget.ui.start, ...widget.ui.landmarks])
          for (final c in widget.ui.screens[sc]?.components ?? const <UiComponent>[])
            if (c.kind == 'list' && c.j['told'] == true) c
      ];

  @override
  void initState() {
    super.initState();
    // What is there when the screens open is not news.
    for (final (a, _) in c0.lines(_threadPreds, null)) {
      _seen.add(formatTerm(a));
    }
    for (final n in s.notices) {
      _toldRefusals.add('${formatTerm(n.part)}|${formatTerm(n.by)}');
    }
    for (final l in _toldLists) {
      _known[l.j['title'] as String] = {for (final x in c0.items(l, null)) formatTerm(x)};
    }
  }

  @override
  void didUpdateWidget(DescribedScreen old) {
    super.didUpdateWidget(old);
    if (!identical(old.state, widget.state)) _observe();
  }

  @override
  void dispose() {
    for (final c in _fields.values) {
      c.dispose();
    }
    for (final t in _timers) {
      t.cancel();
    }
    super.dispose();
  }

  SocialState get s => widget.state;
  DescribedCore get c0 => DescribedCore(widget.ui, widget.state, widget.ask);

  /// The predicates every conversation of the description is drawn from.
  List<String> get _threadPreds => {
        for (final sc in widget.ui.screens.values)
          for (final c in sc.components)
            if (c.kind == 'thread') ...c.preds
      }.toList();

  void _note(String text, {bool bad = false}) {
    final n = (text: text, bad: bad);
    _notes.add(n);
    _timers.add(Timer(const Duration(seconds: 8), () {
      if (mounted) setState(() => _notes.remove(n));
    }));
  }

  /// A new view: the refusals of acts the person asked, the acts done, and the new messages.
  void _observe() {
    final c = c0;
    for (final n in s.notices) {
      if (!_toldRefusals.add('${formatTerm(n.part)}|${formatTerm(n.by)}')) continue;
      _asked.removeWhere((q) => c.namesRequest(n.part, q.req));
      _note(n.sentence(s.me), bad: true);
    }
    final now = {for (final a in s.atoms) c.key(a)};
    var own = false;
    for (final q in [..._asked]) {
      if (s.pending.any((p) => c.namesRequest(p, q.req))) {
        q.pending = true;
        continue;
      }
      // Not yet taken up by the agent: neither pending nor any change to the person's facts.
      if (!q.pending && setEquals(now, q.atoms)) continue;
      _asked.remove(q);
      _done(q);
      own = true;
    }
    // Each new item of a list the UI style tells of, but what came with the person's own act.
    for (final l in _toldLists) {
      final title = l.j['title'] as String, xs = c.items(l, null), had = _known[title];
      if (had != null && !own) {
        for (final x in xs) {
          if (had.contains(c.key(x))) continue;
          final isFact = l.j['items']['facts'] != null, lead = isFact && c.args(x).isNotEmpty ? c.args(x).first : x;
          _note('$title: ${c.show(lead)}');
        }
      }
      _known[title] = {for (final x in xs) c.key(x)};
    }
    final (name, subject) = _stack.isEmpty ? (widget.ui.start, null) : _stack.last;
    final here = widget.ui.screens[name];
    for (final (a, x) in c.lines(_threadPreds, null)) {
      final k = formatTerm(a);
      if (_seen.contains(k) || _told.contains(k)) continue;
      final (text, mine) = c.line(a, x);
      final open = here != null &&
          subject != null &&
          here.components.any((t) => t.kind == 'thread' && t.preds.contains(c.pred(a)) && c.names(a, subject, fact: here.fact != null));
      if (mine || open) {
        _seen.add(k);
        continue;
      }
      _told.add(k);
      final who = c.author(a, x);
      _note(who == null ? text : '${c.show(who)}: $text');
    }
  }

  /// An act asked for is done: the person is told so, the next step of its chain is asked, or, the last done, she
  /// is taken to the screen its action names.
  void _done(_Asked q) {
    _note('${q.label}: done');
    // The routing rule: an act of one place, herself, that every other act needs, taken on the start screen.
    final c = c0, schema = c.pred(q.req);
    if (q.onStart && c.args(q.req).length == 1 && !_rules.value.containsKey(schema) && _rule == null && c.neededByAll(schema)) {
      _rule = (schema: schema, label: q.label);
    }
    // The next step is asked after this frame: asking writes to the agent, which draws again.
    if (q.rest.isNotEmpty) {
      WidgetsBinding.instance.addPostFrameCallback((_) {
        if (mounted) setState(() => _next(q));
      });
      return;
    }
    // THEN IT: the screen about the newest fact of the predicate, the one the act added where there is one.
    final ti = q.thenIt;
    if (ti != null && widget.ui.screens[ti['screen']] != null) {
      final p = ti['pred'] as String, all = [for (final a in s.atoms) if (c.pred(a) == p) a];
      final added = [for (final a in all) if (!q.atoms.contains(c.key(a))) a];
      final it = added.isNotEmpty ? added.last : all.lastOrNull;
      final from = (ti['from'] as String?) ?? widget.ui.start;
      _stack
        ..clear()
        ..add((widget.ui.screens[from] != null ? from : widget.ui.start, null));
      if (it != null) {
        _stack.add((ti['screen'] as String, ti['of'] == 'value' && c.args(it).isNotEmpty ? c.args(it).first : it));
      }
      return;
    }
    final t = q.then;
    if (t != null && widget.ui.screens[t] != null) {
      _stack
        ..clear()
        ..add((t, null));
    }
  }

  /// The next step of a chain, with the values the steps share by name.
  void _next(_Asked q) {
    final c = c0, step = q.rest.first, act = c.act(step);
    if (act == null) return _note('${step.label}: not offered now', bad: true);
    final at = <int, GTerm>{
      for (final e in step.places.entries)
        if (e.value.name != null && q.named[e.value.name] != null) e.key: q.named[e.value.name]!
    };
    final ways = c.ways(step, act, q.subject);
    final w = [...ways, <int, GTerm>{}].firstWhere((w) => at.entries.every((e) => w[e.key] == null || c.same(w[e.key]!, e.value)));
    final r = c.request(act, {...w, ...at});
    if (r == null) return _note('${step.label}: not allowed now', bad: true);
    _propose(r, step.label, then: q.then, thenIt: q.thenIt, rest: q.rest.sublist(1), named: q.named, subject: q.subject);
  }

  /// Ask for the act of the request [r], and follow it.
  void _propose(GTerm r, String label,
      {String? then,
      Map<String, dynamic>? thenIt,
      List<UiComponent> rest = const [],
      Map<String, GTerm> named = const {},
      GTerm? subject}) {
    widget.ask!.propose(r);
    final onStart = _stack.isEmpty || (_stack.last.$1 == widget.ui.start && _stack.last.$2 == null);
    _asked.add(_Asked(r, label, then, rest, named, subject, {for (final a in s.atoms) c0.key(a)}, thenIt, onStart));
  }

  /// The values of a way by the names the action's sentence gives its places.
  Map<String, GTerm> _named(UiComponent c, Map<int, GTerm> vals) => {
        for (final e in c.places.entries)
          if (e.value.name != null && vals[e.key] != null) e.value.name!: vals[e.key]!
      };

  /// The person taps the action [c]: where nothing is left to choose or fill, the request is made; otherwise a
  /// sheet asks for what is left.  The steps of its chain, [rest], are asked in turn once it is done.
  void _tap(UiComponent c, DeclaredAct act, List<DescribedWay> ways, GTerm? subject, {List<UiComponent> rest = const []}) {
    final w0 = ways.first;
    final fields = c0.fieldsOf(act, w0);
    final varying = [
      for (final p in act.binds)
        if (ways.map((w) => w[p] == null ? '' : c0.key(w[p]!)).toSet().length > 1) p
    ];
    if (ways.length == 1 && fields.isEmpty) {
      final r = c0.request(act, w0);
      if (r == null) return setState(() => _note('${c.label}: not allowed now', bad: true));
      void take() => setState(() => _propose(r, c.label, then: c.then, thenIt: c.thenIt, rest: rest, named: _named(c, w0), subject: subject));
      // An act that only removes facts is shown in its words, and taken on a second tap.
      if (rest.isEmpty && widget.ui.ends(act.schema)) {
        showModalBottomSheet<void>(
          context: context,
          builder: (ctx) => Padding(
            key: describedKey('ending:${act.schema}'),
            padding: const EdgeInsets.fromLTRB(20, 16, 20, 20),
            child: Column(mainAxisSize: MainAxisSize.min, crossAxisAlignment: CrossAxisAlignment.start, children: [
              Text(c.label, style: Theme.of(ctx).textTheme.titleMedium),
              Padding(
                padding: const EdgeInsets.only(top: 6, bottom: 14),
                child: Text('${c0.said(act, w0) ?? c.sentence ?? c.label}.'.replaceAll('..', '.'),
                    style: TextStyle(color: Colors.grey.shade700)),
              ),
              Row(children: [
                FilledButton(
                    key: describedKey('confirm'),
                    onPressed: () {
                      Navigator.of(ctx).pop();
                      take();
                    },
                    child: Text(c.label)),
                const SizedBox(width: 8),
                TextButton(key: describedKey('cancel'), onPressed: () => Navigator.of(ctx).pop(), child: const Text('Cancel')),
              ]),
            ]),
          ),
        );
        return;
      }
      return take();
    }
    showModalBottomSheet<void>(
      context: context,
      isScrollControlled: true,
      builder: (ctx) => _Sheet(
        title: c.label,
        sentence: c.sentence,
        ways: ways,
        varying: varying,
        fields: fields,
        sources: c.places,
        show: c0.show,
        fresh: (f) => c0.freshName(c.places[f.place]?.look),
        options: (f) {
          final src = c.places[f.place];
          if (src?.from == null) return null;
          return src!.from == 'everyone' ? c0.everyone() : c0.valuesOf(src.from!);
        },
        textValue: (f, t) => c0.textValue(f, t, schema: act.schema),
        submit: (w, given) {
          final vals = {...w, ...given}, r = c0.request(act, vals);
          if (r == null) return false;
          setState(() => _propose(r, c.label, then: c.then, thenIt: c.thenIt, rest: rest, named: _named(c, vals), subject: subject));
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

  /// Help, from the contract alone: each act the UI structure places, its label, its words, the screens offering
  /// it, and whether the person can take it now or what is in its way ([DescribedCore.help]).
  void _help() {
    final rows = c0.help();
    showModalBottomSheet<void>(
      context: context,
      isScrollControlled: true,
      builder: (ctx) => SafeArea(
        child: ConstrainedBox(
          key: describedKey('help:sheet'),
          constraints: BoxConstraints(maxHeight: MediaQuery.of(ctx).size.height * .8),
          child: ListView(shrinkWrap: true, padding: const EdgeInsets.fromLTRB(20, 16, 20, 16), children: [
            Text('What you can do here', style: Theme.of(ctx).textTheme.titleMedium),
            for (final r in rows)
              Padding(
                key: describedKey('help:${r.schema}'),
                padding: const EdgeInsets.only(top: 12),
                child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
                  Text(r.label, style: const TextStyle(fontWeight: FontWeight.w700)),
                  if (r.sentence != null) Text(r.sentence!, style: TextStyle(color: Colors.grey.shade700)),
                  Text('${r.status} On ${r.screens.join(', ')}.',
                      style: TextStyle(fontSize: 13, color: r.status.startsWith('You can') ? Colors.green.shade800 : Colors.grey.shade700)),
                ]),
              ),
          ]),
        ),
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
    final theme = Theme.of(context), muted = TextStyle(color: Colors.grey.shade600, fontSize: 13);
    final c = c0;
    final body = <Widget>[], actions = <Widget>[], primary = <Widget>[], menu = <(String, VoidCallback)>[];
    Widget? composer;
    final thread = scr.components.any((x) => x.kind == 'thread');
    final here = '$name|${subject == null ? '' : c.key(subject)}';
    // A chain is drawn as its first step, under its own button, the other steps asked in turn.
    final comps = UiDescription.drawn(scr);
    final open = widget.ask?.at != null;
    // The ways each action offers.
    final offered = <int, (DeclaredAct, List<DescribedWay>)>{};
    for (final (i, (comp, _)) in comps.indexed) {
      if (comp.kind != 'action') continue;
      final act = c.act(comp);
      if (act != null) offered[i] = (act, c.ways(comp, act, subject));
    }
    // An important action not offered: in its place, what is in its way and the first step of a way to it, whose
    // button, where it is on this screen, is the hint's.  An action of the same label offered here is no gap.
    final hints = <int, Widget>{}, hintFirst = <int, String>{}, byHint = <String>{}, usedFirst = <String>{};
    final offeredLabels = {
      for (final e in offered.entries)
        if (e.value.$2.isNotEmpty) comps[e.key].$1.label
    };
    for (final e in offered.entries) {
      final comp = comps[e.key].$1, act = e.value.$1, ways = e.value.$2;
      if (!comp.important || ways.isNotEmpty || offeredLabels.contains(comp.label)) continue;
      final fx = c.fixedBy(comp, subject);
      if (fx == null) continue;
      final r = c.wayTo(act, fx, name, subject);
      if (r == null) continue;
      final f = r.first;
      Widget? way;
      if (f != null) {
        final fk = UiDescription.keyOf(f.c), then = [...r.then, comp.label].join(', then ');
        final fs = f.at == name ? subject : null;
        if (f.about != null) {
          final about = f.about!;
          way = Wrap(crossAxisAlignment: WrapCrossAlignment.center, spacing: 8, children: [
            FilledButton.tonal(
                key: describedKey('page:${f.at}:${c.key(about)}'),
                onPressed: () => setState(() {
                  _stack.add((f.at, about));
                  _glow = f.c.label;
                }),
                child: Text('${c.show(about)}’s page ›')),
            Text('there “${f.c.label}”, then $then', style: muted),
          ]);
        } else if (usedFirst.add(fk)) {
          hintFirst[e.key] = f.c.label;
          if (f.at == name) byHint.add(fk);
          way = Wrap(crossAxisAlignment: WrapCrossAlignment.center, spacing: 8, children: [
            FilledButton.tonal(
                key: describedKey('$fk${fs == null ? '' : ':${c.key(fs)}'}'),
                onPressed: open ? () => _tap(f.c, f.act, f.ways, fs, rest: f.rest) : null,
                child: Text(f.c.label)),
            Text('then $then', style: muted),
          ]);
        } else {
          way = Text('First “${f.c.label}”, then $then.', style: muted);
        }
      } else if (r.other != null) {
        way = Text('It comes when another person takes “${r.other}”.', style: muted);
      }
      hints[e.key] = Container(
        key: describedKey('important:${UiDescription.keyOf(comp)}'),
        width: double.infinity,
        margin: const EdgeInsets.only(top: 6),
        padding: const EdgeInsets.fromLTRB(12, 10, 12, 10),
        decoration: BoxDecoration(
            color: Colors.amber.shade50, borderRadius: BorderRadius.circular(12), border: Border.all(color: Colors.amber.shade200)),
        child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
          Text.rich(TextSpan(children: [
            const TextSpan(text: 'To '),
            TextSpan(text: comp.label, style: const TextStyle(fontWeight: FontWeight.w700)),
            TextSpan(text: ': ${r.why}.'),
          ])),
          if (way != null) Padding(padding: const EdgeInsets.only(top: 6), child: way),
        ]),
      );
    }
    final hintCards = <Widget>[];
    // An empty screen points to where to start: its first marked action, or else its first.
    var lists = 0, filled = 0, emptyAt = -1, emptyFills = false;
    String? firstMarked, firstAction;
    for (final (i, (comp, rest)) in comps.indexed) {
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
          final lines = subject == null ? const <(GTerm, GTerm)>[] : c.lines(comp.preds, subject, fact: scr.fact != null);
          // The conversation on the screen is read.
          for (final (a, _) in lines) {
            _seen.add(c.key(a));
          }
          body.add(Padding(
            padding: const EdgeInsets.fromLTRB(12, 12, 12, 4),
            child: Column(crossAxisAlignment: CrossAxisAlignment.stretch, children: [
              if (lines.isEmpty) Center(child: Padding(padding: const EdgeInsets.all(24), child: Text('No messages yet.', style: muted))),
              for (final (a, x) in lines)
                Builder(builder: (_) {
                  final (text, mine) = c.line(a, x);
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
          final items = c.ordered(comp, c.items(comp, subject));
          final opens = comp.j['opens'] as String?;
          final isFact = comp.j['items']['facts'] != null;
          body.add(Padding(
            padding: const EdgeInsets.fromLTRB(16, 18, 16, 6),
            child: Text((comp.j['title'] as String).toUpperCase(),
                style: theme.textTheme.labelSmall?.copyWith(color: Colors.grey.shade600, letterSpacing: .9, fontWeight: FontWeight.w600)),
          ));
          lists++;
          if (items.isNotEmpty) filled++;
          if (items.isEmpty) {
            // How its items come, from the contract's effects; where it says, the hint to start goes after it.
            final fill = c.fillHint(comp, screen: name);
            if (emptyAt < 0) {
              emptyAt = body.length;
              emptyFills = fill.isNotEmpty;
            }
            body.add(Padding(
                key: describedKey('empty:${comp.j['title']}'),
                padding: const EdgeInsets.fromLTRB(16, 0, 16, 6),
                child: Text('None yet.$fill', style: muted)));
          }
          // The conversation a list item opens, whose new lines mark the item.
          final talk = opens == null ? null : ui.screens[opens];
          final talkPreds = [
            for (final t in talk?.components ?? const <UiComponent>[])
              if (t.kind == 'thread') ...t.preds
          ];
          final rows = <Widget>[];
          for (final it in items) {
            final lead = isFact ? (c.args(it).isEmpty ? GAtom(c.pred(it)) : c.args(it).first) : it;
            final chips = [
              for (final p in comp.preds)
                if (s.atoms.any((a) => c.pred(a) == p && c.names(a, it, fact: isFact))) c.label(p)
            ];
            final unread = talkPreds.isEmpty
                ? 0
                : c.lines(talkPreds, it, fact: talk!.fact != null).where((l) => !_seen.contains(c.key(l.$1)) && !c.line(l.$1, l.$2).$2).length;
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
                  trailing: opens == null
                      ? null
                      : Row(mainAxisSize: MainAxisSize.min, children: [
                          if (unread > 0)
                            Container(
                              key: describedKey('unread:${c.key(it)}'),
                              padding: const EdgeInsets.symmetric(horizontal: 7, vertical: 2),
                              decoration: BoxDecoration(color: theme.colorScheme.primary, borderRadius: BorderRadius.circular(99)),
                              child: Text('$unread', style: const TextStyle(color: Colors.white, fontSize: 12, fontWeight: FontWeight.w700)),
                            ),
                          Icon(Icons.chevron_right, color: Colors.grey.shade400),
                        ]),
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
          if (hints[i] case final h?) {
            hintCards.add(h);
            if (hintFirst[i] case final fl?) {
              if (comp.where == 'primary') {
                firstMarked ??= fl;
              } else {
                firstAction ??= fl;
              }
            }
            break;
          }
          if (byHint.contains(UiDescription.keyOf(comp))) break;
          final o = offered[i];
          if (o == null) break;
          final (act, ways) = o;
          if (ways.isEmpty) break;
          final fields = c.fieldsOf(act, ways.first);
          // On a conversation, the act of one way and one text field is its composer; the other acts go in a menu.
          if (thread && composer == null && ways.length == 1 && fields.length == 1 && fields.single.type == 'text' && comp.places[fields.single.place]?.from == null) {
            final f = fields.single, tc = _composer(here);
            void send() {
              final v = c.textValue(f, tc.text, schema: act.schema);
              if (v == null) return;
              final r = c.request(act, {...ways.first, f.place: v});
              if (r == null) return setState(() => _note('${comp.label}: not allowed now', bad: true));
              setState(() => _propose(r, comp.label, then: comp.then, thenIt: comp.thenIt, subject: subject));
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
                        hintText: comp.label,
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
          void go() => _tap(comp, act, ways, subject, rest: rest);
          if (comp.where == 'primary') firstMarked ??= comp.label;
          if (comp.where != 'menu') firstAction ??= comp.label;
          final k = describedKey('${UiDescription.keyOf(comp)}${subject == null ? '' : ':${c.key(subject)}'}');
          // The look puts an action in the menu, in the bar, or first and marked; on a conversation, the menu.
          final where = comp.where ?? (thread ? 'menu' : 'bar');
          if (where == 'menu') {
            // The menu's actions are marked on the screen, so that a walk finds what it offers.
            menu.add((comp.label, go));
            body.add(SizedBox.shrink(key: k));
          } else if (where == 'primary') {
            primary.add(_lit(comp.label, FilledButton(key: k, onPressed: open ? go : null, child: Text(comp.label))));
          } else {
            actions.add(_lit(
                comp.label,
                actions.whereType<ButtonStyleButton>().isEmpty && primary.isEmpty && comp.where == null
                    ? FilledButton(key: k, onPressed: open ? go : null, child: Text(comp.label))
                    : OutlinedButton(key: k, onPressed: open ? go : null, child: Text(comp.label))));
          }
      }
    }
    final start = firstMarked ?? firstAction;
    if (lists > 0 && filled == 0 && emptyAt >= 0) {
      final h = Padding(
          key: describedKey('hint'),
          padding: const EdgeInsets.fromLTRB(16, 0, 16, 6),
          child: Text(start == null ? 'Nothing here yet.' : 'Nothing here yet. Start with “$start”.', style: muted));
      if (emptyFills) {
        body.insert(emptyAt + 1, h);
      } else {
        body[emptyAt] = h;
      }
    }
    // The routing rule, asked once, on the start screen.
    final rule = _rule;
    if (rule != null && name == ui.start && subject == null) {
      void answer(bool yes) => setState(() {
            _rules.value = {..._rules.value, rule.schema: yes};
            _rule = null;
          });
      body.insert(
          0,
          Container(
            key: describedKey('rule'),
            margin: const EdgeInsets.fromLTRB(12, 10, 12, 0),
            padding: const EdgeInsets.fromLTRB(14, 12, 14, 10),
            decoration: BoxDecoration(color: Colors.white, borderRadius: BorderRadius.circular(14), border: Border.all(color: Colors.grey.shade300)),
            child: Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
              Text('Do “${rule.label}” for you in each app you open?'),
              const SizedBox(height: 8),
              Row(children: [
                FilledButton(key: describedKey('rule:yes'), onPressed: () => answer(true), child: const Text('Yes')),
                const SizedBox(width: 8),
                TextButton(key: describedKey('rule:no'), onPressed: () => answer(false), child: const Text('No')),
              ]),
            ]),
          ));
    }
    final title = subject == null
        ? c.show(s.me) == 'you' ? displayText(s.me) : c.show(s.me)
        : c.show(scr.fact != null && c.args(subject).isNotEmpty ? c.args(subject).first : subject);
    final shown = title.isEmpty ? title : title[0].toUpperCase() + title.substring(1);
    // The landmarks, with the start screen, placed as the UI style says: on top, at the bottom or in a menu.
    final marks = ui.landmarks.isEmpty ? const <String>[] : [ui.start, ...ui.landmarks], placed = ui.look['landmarks'] ?? 'top';
    void land(String l) => setState(() => _stack
      ..clear()
      ..add((l, null)));
    // Where the person is: the host's names, then the screens she came through, each a step back, then this one.
    final crumbs = <(String, VoidCallback?)>[
      for (final w in widget.where) (w, null),
      for (var i = 0; i < _stack.length - 1; i++)
        if (i > 0 || _stack[i].$1 != ui.start || _stack[i].$2 != null)
          (
            _stack[i].$2 == null ? _stack[i].$1 : '${_stack[i].$1} ${c.show(_stack[i].$2!)}',
            () => setState(() {
                  _stack.removeRange(i + 1, _stack.length);
                  _glow = null;
                })
          ),
      if (_stack.length > 1 || name != ui.start) (name, null),
    ];
    if (widget.where.isNotEmpty && _stack.length > 1) {
      crumbs[widget.where.length - 1] = (
        widget.where.last,
        () => setState(() {
              _stack.removeRange(1, _stack.length);
              _glow = null;
            })
      );
    }
    return Theme(
      data: _looked(theme),
      child: ColoredBox(
      color: _background(),
      child: Column(children: [
        Material(
          color: Colors.white,
          child: Container(
            height: crumbs.length > 1 ? 64 : 56,
            decoration: BoxDecoration(border: Border(bottom: BorderSide(color: Colors.grey.shade200))),
            child: Row(children: [
              if (_stack.length > 1)
                IconButton(
                    key: describedKey('back'),
                    icon: const Icon(Icons.arrow_back_ios_new, size: 18),
                    onPressed: () => setState(() {
                          _stack.removeLast();
                          _glow = null;
                        }))
              else
                const SizedBox(width: 14),
              _avatar(subject == null ? s.me : (scr.fact != null && c.args(subject).isNotEmpty ? c.args(subject).first : subject), r: 16),
              const SizedBox(width: 10),
              Expanded(
                  child: Column(mainAxisAlignment: MainAxisAlignment.center, crossAxisAlignment: CrossAxisAlignment.start, children: [
                if (crumbs.length > 1) _crumbLine(crumbs, theme),
                Text(shown, style: theme.textTheme.titleMedium?.copyWith(fontWeight: FontWeight.w600)),
              ])),
              if (name == ui.start && ui.scanned)
                IconButton(key: describedKey('mycode'), tooltip: 'My code', icon: const Icon(Icons.qr_code), onPressed: _myCode),
              IconButton(key: describedKey('help'), tooltip: 'Help', icon: const Icon(Icons.help_outline), onPressed: _help),
              if (marks.isNotEmpty && placed == 'menu')
                PopupMenuButton<String>(
                  key: describedKey('landmarks'),
                  icon: const Icon(Icons.menu),
                  onSelected: land,
                  itemBuilder: (_) => [for (final l in marks) PopupMenuItem(value: l, child: Text(l))],
                ),
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
        if (marks.isNotEmpty && placed == 'top')
          Material(
            color: Colors.white,
            child: Padding(
              padding: const EdgeInsets.fromLTRB(12, 6, 12, 6),
              child: Wrap(spacing: 6, children: [
                for (final l in marks)
                  ChoiceChip(key: describedKey('landmark:$l'), label: Text(l), selected: name == l, onSelected: (_) => land(l)),
              ]),
            ),
          ),
        Expanded(child: ListView(padding: const EdgeInsets.only(bottom: 10), children: body)),
        // The notes, above the buttons the person tapped.
        if (_notes.isNotEmpty)
          Padding(
            padding: const EdgeInsets.fromLTRB(12, 4, 12, 4),
            child: Column(children: [
              for (final (i, n) in _notes.indexed)
                Container(
                  key: describedKey('note:$i'),
                  width: double.infinity,
                  margin: const EdgeInsets.only(top: 4),
                  padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                  decoration: BoxDecoration(
                      color: n.bad ? Colors.red.shade50 : theme.colorScheme.primary.withValues(alpha: .10),
                      borderRadius: BorderRadius.circular(10)),
                  child: Text(n.text, style: TextStyle(fontSize: 13, color: n.bad ? Colors.red.shade800 : theme.colorScheme.primary)),
                ),
            ]),
          ),
        // What is in the way of the important actions, and the first step to each.
        if (hintCards.isNotEmpty) Padding(padding: const EdgeInsets.fromLTRB(12, 0, 12, 6), child: Column(children: hintCards)),
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
        if (marks.isNotEmpty && placed == 'bottom')
          Material(
            color: Colors.white,
            child: SafeArea(
              top: false,
              child: Container(
                decoration: BoxDecoration(border: Border(top: BorderSide(color: Colors.grey.shade200))),
                child: Row(children: [
                  for (final l in marks)
                    Expanded(
                      child: TextButton(
                        key: describedKey('landmark:$l'),
                        onPressed: () => land(l),
                        child: Text(l,
                            style: TextStyle(
                                fontWeight: name == l ? FontWeight.w700 : FontWeight.w400,
                                color: name == l ? theme.colorScheme.primary : Colors.grey.shade700)),
                      ),
                    ),
                ]),
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

  /// The name a typed place starts filled with, where its UI style gives one (`NEW AS`).
  final String? Function(DeclaredField) fresh;
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
      required this.fresh,
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
    final look = widget.sources[f.place]?.look ?? const {},
        tc = _text.putIfAbsent(f.place, () => TextEditingController(text: widget.fresh(f) ?? ''));
    // The label the UI style gives the field (`Name IS CALLED "label"`), else the contract's.
    final named = look['label'] as String? ?? f.label;
    final label = f.type == 'key' && look['label'] == null ? '$named (their key)' : named;
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
      case 'scanned':
        // A value given face to face: typed, or read off the other phone's code.
        return Row(children: [
          Expanded(child: TextField(key: describedKey('field:${f.place}'), controller: tc, decoration: InputDecoration(labelText: label))),
          IconButton(
              key: describedKey('scan:${f.place}'),
              tooltip: 'Scan their code',
              icon: const Icon(Icons.qr_code_scanner),
              onPressed: () async {
                final v = await scanCode(context);
                if (v != null) setState(() => tc.text = v);
              }),
        ]);
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
              if (v == null) return setState(() => _error = 'Fill in ${widget.sources[f.place]?.look?['label'] ?? f.label}.');
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
