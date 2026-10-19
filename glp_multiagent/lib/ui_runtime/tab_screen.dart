/// The screen of tabs of the grassroots app's construct family: the layout of the constructs of the apps of GSC's contracts, "one screen with tabs, Friends and, in GSN, Groups too, drawn from the person's state; the questions are drawn inside the tab they concern" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC).  "A question's type fixes a construct's semantic content --- what it shows, what it takes, and the answer each submission forms --- and nothing of its layout" (vGLP, Remark "Construct Families"), so the tabs are the family's layout, and no construct's content changes with them.
///
/// The screen is drawn where an open construct is drawn by `last_state` or `social_menu` (named_widgets.dart).  The tabs are Friends and, where the request type offers `create(Me, N)`, the person is in a group, or a notice of a class of a group is written, Groups; once drawn, Groups stays.  Each tab holds, in order:
///
///   1. the approval questions it concerns, at its top, each drawn by its widget: an approval of a class of a group ([groupClasses]) in Groups, and every other in Friends;
///   2. in Friends, every other question that is neither an approval question nor the request question, by its widget;
///   3. the request question, drawn by `social_menu` in the tab, where it is open; and otherwise construct 0, drawn by `last_state` in the tab.
///
/// Where the request question is open, construct 0 is not drawn a second time: `social_menu` is drawn with the last value drawn on construct 0 ("A declared widget is drawn also with the last value drawn on construct 0, and is drawn again when that value changes, so that a request form can show the state the program last wrote to its person", vGLP e924f73, Section 6.2) and draws that view in the tab, with the request's controls in it, so what construct 0 shows is on the screen.  A construct 0 not drawn by `last_state` is drawn in Friends, by its widget, where no request question is open.
///
/// The screen holds the chat open in Friends, opened by tapping a friend and closed by its back arrow, across the request questions the agent asks one after another, so that a chat stays open as its messages are sent.  It holds also, across them, the message of each Send in GSNP's chat, proposed on a later request question than the sign that precedes it, and the friend each item the person authored was sent to, in whose chat it is drawn (named_widgets.dart, `_chat`).
library;

import 'package:flutter/material.dart';

import 'bridge.dart';
import 'construct_family.dart';
import 'named_widgets.dart';
import 'term.dart';

/// A tab of the screen.
enum ScreenTab { friends, groups }

/// A message held for a later request question: the item the person signed, the friend it is for, the request question that took the sign, [askedOn], and the count of notices written when it was held, [notices].
class HeldMessage {
  final GTerm item, friend;
  final int askedOn, notices;
  const HeldMessage(this.item, this.friend, this.askedOn, this.notices);
}

/// The tab a widget is drawn in, the chat open in Friends, and the messages the screen holds.
class TabPlace extends InheritedWidget {
  final ScreenTab tab;

  /// The friend whose chat is open in Friends; null where none is.
  final GTerm? chat;

  /// Open the chat with a friend, or, given null, close it.
  final void Function(GTerm? friend) openChat;

  /// The messages held, in the order held.
  final List<HeldMessage> held;

  /// Hold the message of the item [item] to [friend], its sign proposed on the request question [askedOn] when [notices] notices were written.
  final void Function(GTerm item, GTerm friend, int askedOn, int notices)
      hold;

  /// Take the message of [item] off the held, and record the friend it is sent to; false where it is not held, so that it is proposed once.
  final bool Function(GTerm item) release;

  /// The friend the person sent, or is to send, the item [item] they authored; null where it was not sent from a chat on this screen.
  final GTerm? Function(GTerm item) destination;

  /// The count of changes to the held and the sent, so that a change is drawn.
  final int version;

  const TabPlace({
    super.key,
    required this.tab,
    required this.chat,
    required this.openChat,
    this.held = const [],
    required this.hold,
    required this.release,
    required this.destination,
    this.version = 0,
    required super.child,
  });

  /// The tab [context] is drawn in; null outside a screen of tabs.
  static TabPlace? maybeOf(BuildContext context) =>
      context.dependOnInheritedWidgetOfExactType<TabPlace>();

  @override
  bool updateShouldNotify(TabPlace old) =>
      old.tab != tab ||
      old.version != version ||
      (old.chat == null) != (chat == null) ||
      (chat != null && formatTerm(old.chat!) != formatTerm(chat!));
}

/// The key of the tab [tab] in the tab bar.
ValueKey<String> tabKey(ScreenTab tab) => ValueKey('tab:${tab.name}');

/// The name of the declared widget of [c], where it has one.
String? _declared(OpenConstruct c) => namedWidget(c.widget)?.$1;

/// Whether [c] is an approval question, `approval(Attempt, YesNo?)`.
bool _isApproval(OpenConstruct c) {
  final (f, args) = ctorArgs(c.view);
  return f == 'approval' && args.length == 2;
}

/// Whether the approval question [c] concerns a group: the class of its attempt, `approval(attempt(I, N, K, C), Q)`, is one of [groupClasses].
bool _concernsGroup(OpenConstruct c) {
  final (_, args) = ctorArgs(c.view);
  final (af, xs) = ctorArgs(args[0]);
  if (af != 'attempt' || xs.length != 4) return false;
  return groupClasses.contains(ctorArgs(xs[3]).$1);
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

  /// The messages held for a later request question ([TabPlace.hold]).
  final List<HeldMessage> _held = [];

  /// The friend each item the person authored was sent to, by the item's term as written.
  final Map<String, GTerm> _sent = {};

  /// The count of changes to [_held] and [_sent].
  int _version = 0;

  void _hold(GTerm item, GTerm friend, int askedOn, int notices) =>
      setState(() {
        _held.add(HeldMessage(item, friend, askedOn, notices));
        _version++;
      });

  bool _release(GTerm item) {
    final k = formatTerm(item);
    final i = _held.indexWhere((h) => formatTerm(h.item) == k);
    if (i < 0) return false;
    setState(() {
      _sent[k] = _held.removeAt(i).friend;
      _version++;
    });
    return true;
  }

  GTerm? _destination(GTerm item) {
    final k = formatTerm(item);
    return _sent[k] ??
        [
          for (final h in _held)
            if (formatTerm(h.item) == k) h.friend
        ].firstOrNull;
  }

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
        (state?.notices.any((n) => n.concernsGroup) ?? false) ||
        approvals.any(_concernsGroup);
    // A message whose sign closed without committing, its notice written
    // after the message was held, is held no longer.
    if (state != null) {
      _held.removeWhere((h) => state.notices.skip(h.notices).any((n) {
            final (f, xs) = ctorArgs(n.part);
            return f == 'sign' &&
                xs.length == 2 &&
                formatTerm(xs[1]) == formatTerm(h.item);
          }));
    }
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
          held: List.unmodifiable(_held),
          hold: _hold,
          release: _release,
          destination: _destination,
          version: _version,
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
