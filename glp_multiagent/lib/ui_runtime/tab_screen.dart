/// The screen of tabs of the grassroots app's construct family: the layout of the constructs of a canonical platform's app, its tabs those the view names.  "A question's type fixes a construct's semantic content --- what it shows, what it takes, and the answer each submission forms --- and nothing of its layout" (vGLP, Remark "Construct Families"), so the tabs are the family's layout, and no construct's content changes with them.
///
/// The screen is drawn where an open construct is drawn by `last_state` or `social_menu` (named_widgets.dart).  Its tabs are the lists the last view on construct 0 names, each named by the contract, since the compiler carries "the tab's name and its predicate into the view the agent writes" (vGLP, Section 5.4, "The contract's words"), and the approval questions are drawn at the top of the first (Remark "Construct Families": a question's type fixes "nothing of its layout"; `_TabScreenState._worded`).  Before a view naming a list is written, the screen has no tabs, and draws its constructs one under another.  Each tab holds, in order:
///
///   1. in the first, at its top, the approval questions, each drawn by its widget;
///   2. in the first, every other question that is neither an approval question nor the request question, by its widget;
///   3. the request question, drawn by `social_menu` in the tab, where it is open; and otherwise construct 0, drawn by `last_state` in the tab.
///
/// Where the request question is open, construct 0 is not drawn a second time: `social_menu` is drawn with the last value drawn on construct 0 ("A declared widget is drawn also with the last value drawn on construct 0, and is drawn again when that value changes, so that a request form can show the state the program last wrote to its person", vGLP e924f73, Section 6.2) and draws that view in the tab, with the request's controls in it, so what construct 0 shows is on the screen.  A construct 0 not drawn by `last_state` is drawn in the first tab, by its widget, where no request question is open.
///
/// The screen holds the person's own key, the view's first argument (vGLP, Section 5.4, "The compilation": "its own key k"), and what the person typed into each field, across the request questions the agent asks one after another, so that what they typed stays.
library;

import 'package:flutter/material.dart';

import 'bridge.dart';
import 'construct_family.dart';
import 'named_widgets.dart';
import 'term.dart';

/// The person's gesture that grants `refresh` on the person channel, an Input addressed to no construct, on which the dispatcher has every construct draw again what it last drew ([Bridge.refresh]; vGLP d433f71, Section 6.3).  A screen of the bridge's constructs gives it to the widgets drawn on it; a widget the construct family draws a refresh in reads it here.
class PersonRefresh extends InheritedWidget {
  final VoidCallback refresh;

  const PersonRefresh({super.key, required this.refresh, required super.child});

  /// The gesture [context] is drawn under; null where no screen gives one.
  static PersonRefresh? maybeOf(BuildContext context) =>
      context.dependOnInheritedWidgetOfExactType<PersonRefresh>();

  @override
  bool updateShouldNotify(PersonRefresh old) => refresh != old.refresh;
}

/// The tab a widget is drawn in: the list it draws, the person's own key, and the fields the screen holds.
class TabPlace extends InheritedWidget {
  /// The predicate of the list the tab draws, one of those the view names (named_widgets.dart, `SocialState.lists`).
  final String list;

  /// The person's own key, as the last view drawn carries it; null where none has been drawn.
  final GTerm? me;

  /// The text of the screen's field [key], held across the request questions the agent asks one after another, so that what the person typed stays.
  final TextEditingController Function(String key)? field;

  const TabPlace({
    super.key,
    required this.list,
    this.me,
    this.field,
    required super.child,
  });

  /// The tab [context] is drawn in; null outside a screen of tabs.
  static TabPlace? maybeOf(BuildContext context) =>
      context.dependOnInheritedWidgetOfExactType<TabPlace>();

  @override
  bool updateShouldNotify(TabPlace old) =>
      old.list != list || (old.me == null) != (me == null);
}

/// The key of the tab of the list of the predicate [predicate], on the screen of a platform compiled from a contract with pragmas.
ValueKey<String> listTabKey(String predicate) =>
    ValueKey('tab:list:$predicate');

/// The key of the declared widget of [c], its term's functor and arity,
/// where it has one ([namedKey]).
String? _declared(OpenConstruct c) => namedKey(c.widget);

/// Whether [c] is an approval question, `approval(Attempt, YesNo?)`, or, on a platform compiled from a contract with pragmas, `approval(Attempt, Ws, YesNo?)`.
bool _isApproval(OpenConstruct c) {
  final (f, args) = ctorArgs(c.view);
  return f == 'approval' && (args.length == 2 || args.length == 3);
}

/// Whether the approval question [c] is open: its answer's position, its last, marked `input(P)`.
bool _open(OpenConstruct c) => questionAt(ctorArgs(c.view).$2.last) != null;

/// The family's layout of [constructs]: the screen of tabs, where one of them is drawn by `last_state` or `social_menu`, and null otherwise ([ScreenLayout]).
Widget? tabbedScreen(
    List<OpenConstruct> constructs,
    ConstructFamily family,
    void Function(int id, GTerm path, GTerm raw) grant) {
  final tabbed = constructs.any((c) {
    final n = _declared(c);
    return n == lastStateKey || n == socialMenuKey;
  });
  if (!tabbed) return null;
  return TabScreen(constructs: constructs, family: family, grant: grant);
}

/// The screen of tabs.
class TabScreen extends StatefulWidget {
  final List<OpenConstruct> constructs;
  final ConstructFamily family;
  final void Function(int id, GTerm path, GTerm raw) grant;

  const TabScreen(
      {super.key,
      required this.constructs,
      required this.family,
      required this.grant});

  @override
  State<TabScreen> createState() => _TabScreenState();
}

class _TabScreenState extends State<TabScreen> {
  /// The person's own key, as the last view drawn on construct 0 carries it ([SocialState.me]).
  GTerm? _me;

  /// The screen's fields, by key ([TabPlace.field]).
  final Map<String, TextEditingController> _fields = {};

  @override
  void dispose() {
    for (final c in _fields.values) {
      c.dispose();
    }
    super.dispose();
  }

  TextEditingController _field(String key) =>
      _fields.putIfAbsent(key, TextEditingController.new);

  @override
  Widget build(BuildContext context) {
    final cs = widget.constructs;
    OpenConstruct? c0;
    OpenConstruct? request;
    final approvals = <OpenConstruct>[];
    final others = <OpenConstruct>[];
    for (final c in cs) {
      if (c.id == 0) {
        c0 = c;
      } else if (request == null && _declared(c) == socialMenuKey) {
        request = c;
      } else if (_isApproval(c)) {
        approvals.add(c);
      } else {
        others.add(c);
      }
    }
    final output = c0?.view;
    final (_, state) = SocialState.read(output);
    // The person's own key, the view's first argument (vGLP 12f629d,
    // Section 5.4, "The compilation": "its own key k").
    if (state != null) _me = state.me;
    // The tabs are the lists the view names (vGLP, Section 5.4, "The
    // contract's words").
    final lists = state?.lists ?? const [];
    if (lists.isNotEmpty) {
      return _worded(lists, c0, request, approvals, others, output);
    }
    // No view naming a list is written yet: no tabs, the constructs one under
    // another.
    return ListView(
      key: const ValueKey('tab-view:none'),
      padding: const EdgeInsets.symmetric(vertical: 8),
      children: [
        for (final a in approvals) _view(a, output),
        for (final o in others) _view(o, output),
        if (request != null)
          _view(request, output, card: false)
        else if (c0 != null)
          _view(c0, output, card: _declared(c0) != lastStateKey),
      ],
    );
  }

  Widget _view(OpenConstruct c, GTerm? output, {bool card = true}) =>
      ConstructView(
        key: constructKey(c.id),
        construct: c,
        family: widget.family,
        output: output,
        card: card,
        grant: (path, raw) => widget.grant(c.id, path, raw),
      );

  /// The screen of a platform compiled from a contract with pragmas: a tab for each list its view names, in their order, named by the contract (vGLP 1126f4f, Section 5.4, "The contract's words": "for each list the app shows, its name and what it lists"), the first holding at its top the approval questions and the other questions, and each the request question drawn in the tab, or construct 0 where none is open.
  Widget _worded(
      List<({String name, String predicate, bool counted})> lists,
      OpenConstruct? c0,
      OpenConstruct? request,
      List<OpenConstruct> approvals,
      List<OpenConstruct> others,
      GTerm? output) {
    final first = lists.first.predicate;
    final byState = c0 != null && _declared(c0) == lastStateKey;
    Widget view(OpenConstruct c, {bool card = true}) =>
        _view(c, output, card: card);
    Widget tab(String pred) => TabPlace(
          list: pred,
          me: _me,
          field: _field,
          child: ListView(
            key: ValueKey('tab-view:list:$pred'),
            padding: const EdgeInsets.symmetric(vertical: 8),
            children: [
              if (pred == first) ...[
                for (final a in approvals) view(a),
                for (final o in others) view(o),
              ],
              if (request != null)
                view(request, card: false)
              else if (c0 != null && (byState || pred == first))
                view(c0, card: !byState),
            ],
          ),
        );
    Widget label(({String name, String predicate, bool counted}) l) {
      final n =
          l.predicate == first ? approvals.where(_open).length : 0;
      return Tab(
        key: listTabKey(l.predicate),
        child: Badge(
          isLabelVisible: n > 0,
          label: Text('$n'),
          offset: const Offset(14, -6),
          child: Text(l.name),
        ),
      );
    }

    return DefaultTabController(
      key: ValueKey('tabs:list:${lists.length}'),
      length: lists.length,
      child: Column(
        children: [
          Material(
            color: Colors.white,
            // More than four lists do not fit a phone's width at the tab bar's default padding, and their names would break inside a word ("Contac ts"): the bar then scrolls, each name on one line, with a narrower padding.
            child: lists.length > 4
                ? TabBar(
                    isScrollable: true,
                    tabAlignment: TabAlignment.center,
                    labelPadding: const EdgeInsets.symmetric(horizontal: 8),
                    tabs: [for (final l in lists) label(l)])
                : TabBar(tabs: [for (final l in lists) label(l)]),
          ),
          Expanded(
            child: TabBarView(
                children: [for (final l in lists) tab(l.predicate)]),
          ),
        ],
      ),
    );
  }
}
