/// The screen of tabs of the grassroots app's construct family: the layout of the constructs of the apps of GSC's contracts, "one screen with tabs, Friends and, in GSN, Groups too, drawn from the person's state; the questions are drawn inside the tab they concern" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC).  "A question's type fixes a construct's semantic content --- what it shows, what it takes, and the answer each submission forms --- and nothing of its layout" (vGLP, Remark "Construct Families"), so the tabs are the family's layout, and no construct's content changes with them.
///
/// The screen is drawn where an open construct is drawn by `last_state` or `social_menu` (named_widgets.dart).  The tabs are Friends and, where the request type offers `create(Me, N)` or the person is in a group, Groups; once drawn, Groups stays.  Each tab holds, in order:
///
///   1. the approval questions it concerns, at its top, each drawn by its widget: an approval of a class of a group, `create`, `join`, `leave`, `close`, `post_own`, `post` or `deliver`, in Groups, and every other in Friends;
///   2. in Friends, every other question that is neither an approval question nor the request question, by its widget;
///   3. the request question, drawn by `social_menu` in the tab, where it is open; and otherwise construct 0, drawn by `last_state` in the tab.
///
/// Where the request question is open, construct 0 is not drawn a second time: `social_menu` is drawn with the last value drawn on construct 0 ("A declared widget is drawn also with the last value drawn on construct 0, and is drawn again when that value changes, so that a request form can show the state the program last wrote to its person", vGLP e924f73, Section 6.2) and draws that view in the tab, with the request's controls in it, so what construct 0 shows is on the screen.  A construct 0 not drawn by `last_state` is drawn in Friends, by its widget, where no request question is open.
///
/// The screen holds the chat open in Friends, opened by tapping a friend and closed by its back arrow, across the request questions the agent asks one after another, so that a chat stays open as its messages are sent.
library;

import 'package:flutter/material.dart';

import 'bridge.dart';
import 'construct_family.dart';
import 'named_widgets.dart';
import 'term.dart';

/// A tab of the screen.
enum ScreenTab { friends, groups }

/// The tab a widget is drawn in, and the chat open in Friends.
class TabPlace extends InheritedWidget {
  final ScreenTab tab;

  /// The friend whose chat is open in Friends; null where none is.
  final GTerm? chat;

  /// Open the chat with a friend, or, given null, close it.
  final void Function(GTerm? friend) openChat;

  const TabPlace({
    super.key,
    required this.tab,
    required this.chat,
    required this.openChat,
    required super.child,
  });

  /// The tab [context] is drawn in; null outside a screen of tabs.
  static TabPlace? maybeOf(BuildContext context) =>
      context.dependOnInheritedWidgetOfExactType<TabPlace>();

  @override
  bool updateShouldNotify(TabPlace old) =>
      old.tab != tab ||
      (old.chat == null) != (chat == null) ||
      (chat != null && formatTerm(old.chat!) != formatTerm(chat!));
}

/// The key of the tab [tab] in the tab bar.
ValueKey<String> tabKey(ScreenTab tab) => ValueKey('tab:${tab.name}');

/// The classes whose approval question concerns a group (GSN:2 to GSN:5, programs/gsc/GSN.glp).
const Set<String> _groupClasses = {
  'create',
  'join',
  'leave',
  'close',
  'post_own',
  'post',
  'deliver',
};

/// The name of the declared widget of [c], where it has one.
String? _declared(OpenConstruct c) => namedWidget(c.widget)?.$1;

/// Whether [c] is an approval question, `approval(Attempt, YesNo?)`.
bool _isApproval(OpenConstruct c) {
  final (f, args) = ctorArgs(c.view);
  return f == 'approval' && args.length == 2;
}

/// Whether the approval question [c] concerns a group: the class of its attempt, `approval(attempt(I, N, K, C), Q)`, is one of [_groupClasses].
bool _concernsGroup(OpenConstruct c) {
  final (_, args) = ctorArgs(c.view);
  final (af, xs) = ctorArgs(args[0]);
  if (af != 'attempt' || xs.length != 4) return false;
  return _groupClasses.contains(ctorArgs(xs[3]).$1);
}

/// Whether the approval question [c] is open: its answer's position marked `input(P)`.
bool _open(OpenConstruct c) => questionAt(ctorArgs(c.view).$2[1]) != null;

/// The family's layout of [constructs]: the screen of tabs, where one of them is drawn by `last_state` or `social_menu`, and null otherwise ([ScreenLayout]).
Widget? tabbedScreen(
    List<OpenConstruct> constructs,
    ConstructFamily family,
    void Function(int id, GTerm path, GTerm raw) grant) {
  final tabbed = constructs.any((c) {
    final n = _declared(c);
    return n == lastStateName || n == socialMenuName;
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
  /// The friend whose chat is open in Friends.
  GTerm? _chat;

  /// Whether Groups has been drawn: once drawn, it stays.
  bool _groups = false;

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
      } else if (request == null && _declared(c) == socialMenuName) {
        request = c;
      } else if (_isApproval(c)) {
        approvals.add(c);
      } else {
        others.add(c);
      }
    }
    final output = c0?.view;
    final (_, state) = SocialState.read(output);
    final offers = request == null
        ? null
        : RequestOffers.of(namedWidget(request.widget)!.$2);
    _groups = _groups ||
        (offers?.has('create', 2) ?? false) ||
        (state?.groups.isNotEmpty ?? false) ||
        approvals.any(_concernsGroup);
    final tabs = [ScreenTab.friends, if (_groups) ScreenTab.groups];
    ScreenTab tabOf(OpenConstruct a) =>
        _groups && _concernsGroup(a) ? ScreenTab.groups : ScreenTab.friends;
    final byState = c0 != null && _declared(c0) == lastStateName;

    Widget view(OpenConstruct c, {bool card = true}) => ConstructView(
          key: constructKey(c.id),
          construct: c,
          family: widget.family,
          output: output,
          card: card,
          grant: (path, raw) => widget.grant(c.id, path, raw),
        );

    Widget tab(ScreenTab t) => TabPlace(
          tab: t,
          chat: t == ScreenTab.friends ? _chat : null,
          openChat: (f) => setState(() => _chat = f),
          child: ListView(
            key: ValueKey('tab-view:${t.name}'),
            padding: const EdgeInsets.symmetric(vertical: 8),
            children: [
              for (final a in approvals)
                if (tabOf(a) == t) view(a),
              if (t == ScreenTab.friends)
                for (final o in others) view(o),
              if (request != null)
                view(request, card: false)
              else if (c0 != null && (byState || t == ScreenTab.friends))
                view(c0, card: !byState),
            ],
          ),
        );

    Widget label(ScreenTab t) {
      final n = approvals.where((a) => tabOf(a) == t && _open(a)).length;
      return Tab(
        key: tabKey(t),
        child: Badge(
          isLabelVisible: n > 0,
          label: Text('$n'),
          offset: const Offset(14, -6),
          child: Text(t == ScreenTab.friends ? 'Friends' : 'Groups'),
        ),
      );
    }

    return DefaultTabController(
      key: ValueKey('tabs:${tabs.length}'),
      length: tabs.length,
      child: Column(
        children: [
          Material(
            color: Colors.white,
            child: TabBar(tabs: [for (final t in tabs) label(t)]),
          ),
          Expanded(
            child: TabBarView(children: [for (final t in tabs) tab(t)]),
          ),
        ],
      ),
    );
  }
}
