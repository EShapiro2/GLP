/// The screen of tabs of the grassroots app's construct family: the layout of the constructs of the apps of GSC's contracts, "one screen with tabs, Friends and, in GSN, Groups too, drawn from the person's state; the questions are drawn inside the tab they concern" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC).  "A question's type fixes a construct's semantic content --- what it shows, what it takes, and the answer each submission forms --- and nothing of its layout" (vGLP, Remark "Construct Families"), so the tabs are the family's layout, and no construct's content changes with them.
///
/// The screen is drawn where an open construct is drawn by `last_state` or `social_menu` (named_widgets.dart).  The tabs are Friends; where the request type offers `create(Me, N)`, the person is in a group, or a notice of a class of a group is written, Groups; and, in GAI's app, where the request type offers `enter(Me, R, X)` or the person is the principal of an AI agent, AI agents (GAI's paper, Figure "The GAI app": "her AI agents tab", Code_inbox.md, GAI #5 Cowork, 2026-10-19 20:36 UTC, (a)); once drawn, Groups and AI agents stay.  Each tab holds, in order:
///
///   1. the approval questions it concerns, at its top, each drawn by its widget: an approval of a class of a group ([groupClasses]) in Groups, and every other in Friends;
///   2. in Friends, every other question that is neither an approval question nor the request question, by its widget;
///   3. the request question, drawn by `social_menu` in the tab, where it is open; and otherwise construct 0, drawn by `last_state` in the tab.
///
/// Where the request question is open, construct 0 is not drawn a second time: `social_menu` is drawn with the last value drawn on construct 0 ("A declared widget is drawn also with the last value drawn on construct 0, and is drawn again when that value changes, so that a request form can show the state the program last wrote to its person", vGLP e924f73, Section 6.2) and draws that view in the tab, with the request's controls in it, so what construct 0 shows is on the screen.  A construct 0 not drawn by `last_state` is drawn in Friends, by its widget, where no request question is open.
///
/// The screen holds the chat open in Friends, opened by tapping a friend and closed by its back arrow, across the request questions the agent asks one after another, so that a chat stays open as its messages are sent.  It holds, in CSSN's app, the line of each request the person composed and sent (named_widgets.dart, `_composers`), drawn on every later request question, and the class of each attempt the person approved, from which a parent names their child's groups (named_widgets.dart, `_childGroupComposers`).  It holds the person's own key, which each request question shows (vGLP, Section 5.4, "The compilation": the request question "shows the agent's own key and the persons it knows"), so that construct 0, drawn by `last_state` where no request question is open, draws a group the person leads with its members, as `social_menu` does.  It holds also, across them, the message of each Send in GSNP's chat, proposed on a later request question than the sign that precedes it, and the friend each item the person authored was sent to, in whose chat it is drawn (named_widgets.dart, `_chat`); in GAI's app, the creation of each AI agent, its enter proposed on a later request question than the sign of its code, and what the person typed into the form Create an AI agent (named_widgets.dart, `_create`).
library;

import 'package:flutter/foundation.dart' show setEquals;
import 'package:flutter/material.dart';

import 'bridge.dart';
import 'construct_family.dart';
import 'named_widgets.dart';
import 'term.dart';

/// A tab of the screen.
enum ScreenTab { friends, groups, agents }

/// A message held for a later request question: the item the person signed, the friend it is for, the request question that took the sign, [askedOn], and the count of notices written when it was held, [notices], those before the last view among them ([SocialState.written]).
class HeldMessage {
  final GTerm item, friend;
  final int askedOn, notices;
  const HeldMessage(this.item, this.friend, this.askedOn, this.notices);
}

/// An AI agent's creation held for a later request question: the code the person signed, [item], the key of the AI agent, [agent], the request question that took the sign, [askedOn], and the count of notices written when it was held, [notices] (named_widgets.dart, `_create`).
class HeldEnter {
  final GTerm item, agent;
  final int askedOn, notices;
  const HeldEnter(this.item, this.agent, this.askedOn, this.notices);
}

/// The tab a widget is drawn in, the chat open in Friends, and the messages the screen holds.
class TabPlace extends InheritedWidget {
  final ScreenTab tab;

  /// The friend whose chat is open in Friends; null where none is.
  final GTerm? chat;

  /// The person's own key, as the last request question drawn showed it; null where none has been drawn.
  final GTerm? me;

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

  /// The creations of AI agents held, in the order held.
  final List<HeldEnter> heldEnters;

  /// Hold the creation of the AI agent [agent] with the code [item], its sign proposed on the request question [askedOn] when [notices] notices were written.
  final void Function(GTerm item, GTerm agent, int askedOn, int notices)?
      holdEnter;

  /// Take the creation with the code [item] off the held; false where it is not held, so that it is proposed once.
  final bool Function(GTerm item)? releaseEnter;

  /// The text of the screen's field [key], held across the request questions the agent asks one after another, so that what the person typed stays.
  final TextEditingController Function(String key)? field;

  /// The requests the person composed and sent in CSSN's app, each its kind and the line drawn for it once sent, in the order sent (named_widgets.dart, `_composers`).
  final List<(String, String)> sent;

  /// Hold the line [line] of a request of the kind [kind] the person sent.
  final void Function(String kind, String line)? addSent;

  /// The classes of the attempts the person approved, answering yes, in the order approved, each as its attempt names it: in CSSN's app a parent's state holds nothing of their child's groups (CSSN, Section 4), so the groups a parent names in ending a child's membership or closing a child's group are those of their own approvals of the child's joining (named_widgets.dart, `_childGroupComposers`).
  final List<GTerm> approved;

  /// The AI agents an open approval question names as AI agents, by their terms as written: `R` of `agent_meet(R, Q, P, Y)` and of `agent_befriend(R, Q, P, Y)`, and `R` and `S` of `agents_meet(R, S, P, Q, Y, Z)` and of `agents_befriend(R, S, P, Q, Y, Z)` (programs/gai/GAI.glp), so that their rows offer no Meet and no Befriend, `meet` and `befriend` forbidding `agent` at both parties, before the person's view records them.
  final Set<String> agentsAsked;

  /// The count of changes to the held and the sent, so that a change is drawn.
  final int version;

  const TabPlace({
    super.key,
    required this.tab,
    required this.chat,
    this.me,
    required this.openChat,
    this.held = const [],
    required this.hold,
    required this.release,
    required this.destination,
    this.heldEnters = const [],
    this.holdEnter,
    this.releaseEnter,
    this.field,
    this.sent = const [],
    this.addSent,
    this.approved = const [],
    this.agentsAsked = const {},
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
      !setEquals(old.agentsAsked, agentsAsked) ||
      (old.me == null) != (me == null) ||
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

/// Whether the approval question [c] concerns a group: the class of its attempt, `approval(attempt(I, N, K, C, R), Q)`, is one of [groupClasses].
bool _concernsGroup(OpenConstruct c) {
  final (_, args) = ctorArgs(c.view);
  final (af, xs) = ctorArgs(args[0]);
  if (af != 'attempt' || xs.length != 5) return false;
  return groupClasses.contains(ctorArgs(xs[3]).$1);
}

/// The AI agents the approval question [c] names as AI agents: `R` of `agent_meet(R, Q, P, Y)` and of `agent_befriend(R, Q, P, Y)`, and `R` and `S` of `agents_meet(R, S, P, Q, Y, Z)` and of `agents_befriend(R, S, P, Q, Y, Z)`, each by its term as written.
Iterable<String> _agentsNamed(OpenConstruct c) {
  final (_, args) = ctorArgs(c.view);
  final (af, xs) = ctorArgs(args[0]);
  if (af != 'attempt' || xs.length != 5) return const [];
  final (f, ys) = ctorArgs(xs[3]);
  return switch ((f, ys.length)) {
    ('agent_meet' || 'agent_befriend', 4) => [formatTerm(ys[0])],
    ('agents_meet' || 'agents_befriend', 6) => [
        formatTerm(ys[0]),
        formatTerm(ys[1])
      ],
    _ => const [],
  };
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

  /// Whether AI agents has been drawn: once drawn, it stays.
  bool _agents = false;

  /// Whether the first tab is Contacts, the request type last drawn offering meeting and no friendship (GKK's app); Friends otherwise.
  bool _contacts = false;

  /// The person's own key, as the last request question drawn showed it.
  GTerm? _me;

  /// The messages held for a later request question ([TabPlace.hold]).
  final List<HeldMessage> _held = [];

  /// The friend each item the person authored was sent to, by the item's term as written.
  final Map<String, GTerm> _sent = {};

  /// The creations of AI agents held for a later request question ([TabPlace.holdEnter]).
  final List<HeldEnter> _heldEnters = [];

  /// The screen's fields, by key ([TabPlace.field]).
  final Map<String, TextEditingController> _fields = {};

  /// The requests the person composed and sent ([TabPlace.sent]).
  final List<(String, String)> _sentLines = [];

  /// The classes of the attempts the person approved ([TabPlace.approved]).
  final List<GTerm> _approved = [];

  /// The count of changes to [_held], [_sent], [_heldEnters], [_sentLines] and [_approved].
  int _version = 0;

  @override
  void dispose() {
    for (final c in _fields.values) {
      c.dispose();
    }
    super.dispose();
  }

  TextEditingController _field(String key) =>
      _fields.putIfAbsent(key, TextEditingController.new);

  void _hold(GTerm item, GTerm friend, int askedOn, int notices) =>
      setState(() {
        _held.add(HeldMessage(item, friend, askedOn, notices));
        _version++;
      });

  void _holdEnter(GTerm item, GTerm agent, int askedOn, int notices) =>
      setState(() {
        _heldEnters.add(HeldEnter(item, agent, askedOn, notices));
        _version++;
      });

  void _addSent(String kind, String line) => setState(() {
        _sentLines.add((kind, line));
        _version++;
      });

  bool _releaseEnter(GTerm item) {
    final k = formatTerm(item);
    final i = _heldEnters.indexWhere((h) => formatTerm(h.item) == k);
    if (i < 0) return false;
    setState(() {
      _heldEnters.removeAt(i);
      _version++;
    });
    return true;
  }

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
    if (request != null) {
      final (f, xs) = ctorArgs(request.view);
      if (f == 'menu' && xs.length == 3 && pendingAt(xs[0]) == null) {
        _me = xs[0];
      }
    }
    final offers = request == null
        ? null
        : RequestOffers.of(namedWidget(request.widget)!.$2);
    _groups = _groups ||
        (offers?.has('create', 2) ?? false) ||
        (state?.inGroup ?? false) ||
        (state?.notices.any((n) => n.concernsGroup) ?? false) ||
        approvals.any(_concernsGroup);
    _agents = _agents ||
        (offers?.has('enter', 3) ?? false) ||
        (state?.aiAgents.isNotEmpty ?? false);
    // GKK's app: the request type offers meeting and no friendship, so its
    // first tab lists the person's contacts (programs/gsc/GKK.glp; GSC #5
    // Cowork, Code_inbox.md, 2026-10-20 17:53 UTC: "his contacts then showing
    // Carol").
    if (offers != null) {
      _contacts = offers.has('meet', 2) && !offers.has('befriend', 2);
    }
    // A message or a creation whose sign closed without committing, its
    // notice written after it was held, is held no longer.
    if (state != null) {
      bool signClosed(GTerm item, int notices) =>
          state.written.skip(notices).any((n) {
            final (f, xs) = ctorArgs(n.part);
            return f == 'sign' &&
                xs.length == 2 &&
                formatTerm(xs[1]) == formatTerm(item);
          });
      _held.removeWhere((h) => signClosed(h.item, h.notices));
      _heldEnters.removeWhere((h) => signClosed(h.item, h.notices));
    }
    final tabs = [
      ScreenTab.friends,
      if (_groups) ScreenTab.groups,
      if (_agents) ScreenTab.agents,
    ];
    ScreenTab tabOf(OpenConstruct a) =>
        _groups && _concernsGroup(a) ? ScreenTab.groups : ScreenTab.friends;
    final byState = c0 != null && _declared(c0) == lastStateName;

    Widget view(OpenConstruct c, {bool card = true}) => ConstructView(
          key: constructKey(c.id),
          construct: c,
          family: widget.family,
          output: output,
          card: card,
          grant: (path, raw) {
            // An approval answered yes: its attempt's class is held.
            if (_isApproval(c) && formatTerm(raw) == 'yes') {
              final (af, xs) = ctorArgs(ctorArgs(c.view).$2[0]);
              if (af == 'attempt' && xs.length == 5) {
                setState(() {
                  _approved.add(xs[3]);
                  _version++;
                });
              }
            }
            widget.grant(c.id, path, raw);
          },
        );

    Widget tab(ScreenTab t) => TabPlace(
          tab: t,
          chat: t == ScreenTab.friends ? _chat : null,
          me: _me,
          openChat: (f) => setState(() => _chat = f),
          held: List.unmodifiable(_held),
          hold: _hold,
          release: _release,
          destination: _destination,
          heldEnters: List.unmodifiable(_heldEnters),
          holdEnter: _holdEnter,
          releaseEnter: _releaseEnter,
          field: _field,
          sent: List.unmodifiable(_sentLines),
          addSent: _addSent,
          approved: List.unmodifiable(_approved),
          agentsAsked: {
            for (final a in approvals)
              if (_open(a)) ..._agentsNamed(a)
          },
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
          child: Text(switch (t) {
            ScreenTab.friends => _contacts ? 'Contacts' : 'Friends',
            ScreenTab.groups => 'Groups',
            ScreenTab.agents => 'AI agents',
          }),
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
