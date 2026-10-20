// The screen of tabs of the apps of GSC's contracts and its two named
// widgets (lib/ui_runtime/tab_screen.dart and named_widgets.dart):
// `social_menu`, the request question `Menu ::= menu(Key, List(Key),
// Request?)` drawn with the last state on construct 0 in the tabs, and
// `last_state`, construct 0, the last view the agent wrote (Code_inbox.md,
// GAI #3 Cowork, 2026-10-19 10:45 UTC, items 1, 2 and 4).  "On the
// program's output to its person ... the agent goal writes its view view(s,
// K), the machine state s of its app state and the persons K it knows, when
// it starts and whenever either changes" (vGLP b7e3517, Section 5.4, "The
// compilation"); "A declared widget is drawn also with the last value drawn
// on construct 0, and is drawn again when that value changes, so that a
// request form can show the state the program last wrote to its person"
// (vGLP e924f73, Section 6.2).  GSNP's items, Forward with its warranty
// question, and Send signing the text before it sends it (GAI #3 Cowork,
// 2026-10-19 13:31 UTC, items 1 to 3), and the notices of approvals that end
// by a refusal, "a notice refused([t]_j, q, d): [t]_j the part of the refused
// attempt, q its refuser and d the reason", drawn by "a widget that draws the
// last view written and the notices written after it" (vGLP f0acbc1, Section
// 5.4, "The compilation").
//
// Each screen is a phone, 390 by 844 logical pixels, the person's agent
// surface over a bridge handed the draws a compiled app writes, in the form
// the compiler prints them: `draw(0, named(last_state, D0), output(Views))`,
// each view `view(S, K)`, `S` a list of the contract's atoms
// (programs/gsc/GSG.glp, GSN.glp, GSNP.glp) and `K` the persons known, `draw(Id, named(social_menu, D), menu_w(menu(Me, Known,
// input([3]))))`, D the default widget of Menu as the canonical compilation
// emits it for programs/apps/gsg and gsn, and the approval questions drawn by
// `approval_sentence`.  The grants are those the bridge returns, `input(Id,
// P, R)`.  Where the environment names a directory in SOCIAL_TABS_SHOTS, each
// screen is written there as a PNG, rendered at a device pixel ratio of 3 in
// Roboto from the Flutter SDK's copies; otherwise nothing is written.
import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/tab_screen.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'fixture_term.dart';

const Size _phone = Size(390, 844);
const double _ratio = 3;

// --- The default widgets, as the canonical compilation emits them ----------

/// The default widget of Speech where the person writes it.
const _speechW = "rec('Speech', form(speech, [field('Utterance', "
    "rec('Utterance', menu([form(utterance, [field('String', text)]), "
    "form(speech, [field('Utterance', ref('Utterance')), "
    "field('Peer', peer)])]))), field('Peer', peer)]))";

const _p = "field('Peer', peer)";
const _s = "field('Speech', $_speechW)";

const _gsgProposals = [
  "form(establish, [$_p])",
  "form(befriend, [$_p, $_p])",
  "form(unfriend, [$_p, $_p])",
];

const _gsnProposals = [
  ..._gsgProposals,
  "form(message, [$_p, $_p, $_s])",
  "form(create, [$_p, $_s])",
  "form(join, [$_p, $_p, $_s])",
  "form(leave, [$_p, $_p, $_s])",
  "form(close, [$_p, $_s])",
  "form(post_own, [$_p, $_s, $_s])",
  "form(post, [$_p, $_p, $_s, $_s])",
];

/// GSNP's, as programs/apps/gsnp/platform.vglp prints its Proposal: GSG's,
/// sign, message and forward, and GSN's groups with post_forward.
const _gsnpProposals = [
  ..._gsgProposals,
  "form(sign, [$_p, $_s])",
  "form(message, [$_p, $_p, $_s])",
  "form(forward, [$_p, $_p, $_s])",
  "form(create, [$_p, $_s])",
  "form(join, [$_p, $_p, $_s])",
  "form(leave, [$_p, $_p, $_s])",
  "form(close, [$_p, $_s])",
  "form(post_own, [$_p, $_s, $_s])",
  "form(post, [$_p, $_p, $_s, $_s])",
  "form(post_forward, [$_p, $_p, $_s, $_s])",
];

/// GAI's, as programs/apps/gai/platform.vglp prints its Proposal: GSG's with
/// unfriend one per role, the wards core's for an AI agent and its
/// principal, retire, and GSNP's sign, message and forward.
const _gaiProposals = [
  "form(establish, [$_p])",
  "form(befriend, [$_p, $_p])",
  "form(unfriend_p, [$_p, $_p])",
  "form(unfriend_q, [$_p, $_p])",
  "form(enter, [$_p, $_p, $_s])",
  "form(agent_befriend, [$_p, $_p, $_p, $_s])",
  "form(agents_befriend, [$_p, $_p, $_p, $_p, $_s, $_s])",
  "form(agent_unfriend, [$_p, $_p, $_p])",
  "form(retire, [$_p, $_p])",
  "form(sign, [$_p, $_s])",
  "form(message, [$_p, $_p, $_s])",
  "form(forward, [$_p, $_p, $_s])",
];

/// CSSN's, as programs/cssn/platform.vglp prints its Proposal after GKK
/// (CSSN.glp at GLP 6ff16d78): GSN's, with GKK's meet and introduce, and
/// those of the children, child_meet and child_meet_2 among them
/// (programs/cssn/CSSN.glp).
const _cssnProposals = [
  "form(establish, [$_p])",
  "form(meet, [$_p, $_p])",
  "form(introduce, [$_p, $_p, $_p])",
  "form(befriend, [$_p, $_p])",
  "form(unfriend, [$_p, $_p])",
  "form(message, [$_p, $_p, $_s])",
  "form(create, [$_p, $_s])",
  "form(join, [$_p, $_p, $_s])",
  "form(leave, [$_p, $_p, $_s])",
  "form(close, [$_p, $_s])",
  "form(post_own, [$_p, $_s, $_s])",
  "form(post, [$_p, $_p, $_s, $_s])",
  "form(enter, [$_p, $_p])",
  "form(enter_2, [$_p, $_p, $_p])",
  "form(of_age, [$_p, $_p])",
  "form(of_age_2, [$_p, $_p, $_p])",
  "form(child_meet, [$_p, $_p, $_p])",
  "form(child_meet_2, [$_p, $_p, $_p, $_p])",
  "form(child_befriend, [$_p, $_p, $_p, $_p])",
  "form(child_adult_befriend, [$_p, $_p, $_p])",
  "form(child_unfriend, [$_p, $_p, $_p])",
  "form(parent_unfriend, [$_p, $_p])",
  "form(parent_unfriend_2, [$_p, $_p, $_p])",
  "form(child_join, [$_p, $_p, $_p, $_s])",
  "form(child_join_2, [$_p, $_p, $_p, $_p, $_s])",
  "form(child_leave, [$_p, $_p, $_p, $_s])",
  "form(child_close, [$_p, $_p, $_s])",
];

/// The default widget of `Menu ::= menu(Key, List(Key), Request?)` over the
/// proposals [ps].
String _menuD(List<String> ps) => "form(menu, [field('Key', shown), "
    "field('List(Key)', thread), field('Request', menu([form(propose, "
    "[field('Proposal', menu([${ps.join(', ')}]))]), "
    "form(known, [field('Peer', peer)])]))])";

/// The default widget of a Speech the program writes.
const _speechShown = "rec('Speech', form(speech, [field('Utterance', "
    "rec('Utterance', menu([form(utterance, [field('String', shown)]), "
    "form(speech, [field('Utterance', ref('Utterance')), "
    "field('Peer', shown)])]))), field('Peer', shown)]))";

/// The default widget of the request an attempt names, `Names ::= none ;
/// request(Rho, Integer)`, `Rho ::= one(Class) ; rho(Proposal)`, over the
/// classes [classes], each `form(F, [...])`, standing for the proposals too
/// (vGLP f0acbc1, Definition "Attempt"; the construct of `approval_w` in
/// cascade/GSG/platform.glp as cascade.sh prints it).
String _namesW(String classes) => "field('Names', menu([shown, "
    "form(request, [field('Rho', menu([form(one, [field('Class', "
    "menu([$classes]))]), form(rho, [field('Proposal', menu([$classes]))])])), "
    "field('Integer', shown)])]))";

const _approvalClasses = "form(befriend, [field('Peer', shown), "
    "field('Peer', shown)]), form(join, [field('Peer', shown), "
    "field('Peer', shown), field('Speech', $_speechShown)])";

/// The widget of the approval question, `named(approval_sentence, D)`.
final _approvalW = "named(approval_sentence, form(approval, "
    "[field('Attempt', form(attempt, [field('Peer', shown), "
    "field('Integer', shown), field('Integer', shown), field('Class', "
    "menu([$_approvalClasses])), ${_namesW(_approvalClasses)}])), "
    "field('YesNo', buttons([yes, no]))]))";

const _gaiApprovalClasses = "form(agent_befriend, [field('Peer', shown), "
    "field('Peer', shown), field('Peer', shown), "
    "field('Speech', $_speechShown)])";

/// The widget of GAI's approval question, `named(approval_sentence, D)`,
/// over agent_befriend.
final _gaiApprovalW = "named(approval_sentence, form(approval, "
    "[field('Attempt', form(attempt, [field('Peer', shown), "
    "field('Integer', shown), field('Integer', shown), field('Class', "
    "menu([$_gaiApprovalClasses])), ${_namesW(_gaiApprovalClasses)}])), "
    "field('YesNo', buttons([yes, no]))]))";

/// CSSN's classes of a child's membership, `child_join(a?, s?, p? ; n)`,
/// `child_join_2(r?, s?, p?, q? ; n)`, `child_leave(a, s, p? ; n)` and
/// `child_close(r, p?, r[listed(*, n)] ; n)` (programs/cssn/CSSN.glp), as
/// programs/cssn/platform.vglp prints their Class.
const _cssnGroupClasses = "form(child_join, [field('Peer', shown), "
    "field('Peer', shown), field('Peer', shown), "
    "field('Speech', $_speechShown)]), "
    "form(child_join_2, [field('Peer', shown), field('Peer', shown), "
    "field('Peer', shown), field('Peer', shown), "
    "field('Speech', $_speechShown)]), "
    "form(child_leave, [field('Peer', shown), field('Peer', shown), "
    "field('Peer', shown), field('Speech', $_speechShown)]), "
    "form(child_close, [field('Peer', shown), field('Peer', shown), "
    "field('List(Peer)', thread), field('Speech', $_speechShown)])";

/// The widget of CSSN's approval question over its classes of a child's
/// membership, `named(approval_sentence, D)`.
final _cssnGroupApprovalW = "named(approval_sentence, form(approval, "
    "[field('Attempt', form(attempt, [field('Peer', shown), "
    "field('Integer', shown), field('Integer', shown), field('Class', "
    "menu([$_cssnGroupClasses])), ${_namesW(_cssnGroupClasses)}])), "
    "field('YesNo', buttons([yes, no]))]))";

/// The widget of construct 0, `named(last_state, D0)`.
const _lastStateW = 'named(last_state, thread)';

// --- The views ----------------------------------------------------------------

/// The view of the machine state [s] and the persons known [k].
String _v(String s, [String k = '[]']) => 'view($s, $k)';

String _sp(String text, String by) => 'speech(utterance(${_q(text)}), $by)';
String _q(String s) => "'$s'";

const _friends = 'speech(utterance(friends), alice)';
const _hikers = 'speech(utterance(hikers), bob)';

/// A member atom of the group [name] led by [leader]: its community name,
/// `<leader>.name`, one argument (programs/apps/gsn/platform.vglp, `Atom`
/// and `CName`).
String _member(String leader, String name) =>
    'member(dot(community($leader), $name))';

/// Alice's GSN state: her identity; Bob and Carol her friends; a message
/// from Bob; the group friends, which she leads, Bob listed in it, Bob's
/// message delivered in it; and the group hikers, which Bob leads, his
/// message delivered in it.
final _gsnState = '[identity, friend(bob), friend(carol), '
    'received(${_sp('Lunch at noon?', 'bob')}, bob), '
    '${_member('alice', _friends)}, listed(alice, $_friends), '
    'listed(bob, $_friends), '
    'received_in(${_sp('See you all on Sunday', 'bob')}, alice, $_friends), '
    '${_member('bob', _hikers)}, '
    'received_in(${_sp('Trail at nine', 'bob')}, bob, $_hikers)]';

// --- The phone ------------------------------------------------------------------

Future<void> _loadFonts() async {
  final roots = [
    if (Platform.environment['FLUTTER_ROOT'] case final String r) r,
    '/opt/homebrew/share/flutter',
  ];
  final dir = roots
      .map((r) => '$r/bin/cache/artifacts/material_fonts')
      .firstWhere((d) => Directory(d).existsSync(),
          orElse: () => fail('no material_fonts under $roots'));
  Future<ByteData> bytes(String f) async =>
      ByteData.view(File('$dir/$f').readAsBytesSync().buffer);
  final roboto = FontLoader('Roboto');
  for (final f in [
    'Roboto-Regular.ttf',
    'Roboto-Italic.ttf',
    'Roboto-Medium.ttf',
    'Roboto-Bold.ttf',
  ]) {
    roboto.addFont(bytes(f));
  }
  await roboto.load();
  final icons = FontLoader('MaterialIcons')
    ..addFont(bytes('MaterialIcons-Regular.otf'));
  await icons.load();
}

/// One person's phone: their bridge, handed the draws, and the grants it
/// returns.  The person is [me], Alice unless named.
class _Phone {
  final WidgetTester tester;
  final String app;
  final String me;
  final List<String> grants = [];
  final GlobalKey shot = GlobalKey();
  late final UiRuntime r;

  _Phone(this.tester, this.app, this.me) {
    r = UiRuntime(
      manifest: Manifest(
        title: app,
        panels: [Panel(id: app, name: app.toUpperCase(), constructs: true)],
        activity: const [],
      ),
      onSend: (t) => grants.add(formatTerm(t)),
    );
  }

  static Future<_Phone> open(WidgetTester tester, String app,
      {String me = 'alice'}) async {
    await _loadFonts();
    // Shadows drawn as shadows, not as the test's solid outlines; restored
    // at the end of the test's body by [_phoneTest].
    debugDisableShadows = false;
    tester.view.physicalSize = _phone;
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);
    final p = _Phone(tester, app, me);
    await tester.pumpWidget(MaterialApp(
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
          useMaterial3: true,
          brightness: Brightness.light,
          fontFamily: 'Roboto',
          colorScheme: ColorScheme.fromSeed(seedColor: Colors.green),
          elevatedButtonTheme: ElevatedButtonThemeData(
              style: ElevatedButton.styleFrom(
                  backgroundColor: Colors.green,
                  foregroundColor: Colors.white))),
      home: RepaintBoundary(
        key: p.shot,
        child: MediaQuery(
          data: const MediaQueryData(size: _phone, devicePixelRatio: _ratio),
          child: AgentSurface(agentId: me, runtime: p.r, muteNotices: true),
        ),
      ),
    ));
    return p;
  }

  /// Hand the bridge the draw [source] and draw the screen again.
  Future<void> draw(String source) async {
    r.handle(fixtureTerm(source));
    await tester.pumpAndSettle();
  }

  /// Draw construct 0 with the views [views], the last written last.
  Future<void> views(List<String> views) =>
      draw('draw(0, $_lastStateW, output([${views.join(', ')}]))');

  /// Draw the request question [id] of the person, showing [known] as the
  /// persons known when it was asked, whose type offers the proposals [ps].
  Future<void> menu(int id, List<String> ps, {String known = '[]'}) => draw(
      'draw($id, named(social_menu, ${_menuD(ps)}), '
      'menu_w(menu($me, $known, input([3]))))');

  Finder key(int id, String what) => find.byKey(socialKey(id, what));

  /// The texts on the screen, in order.
  List<String> texts() => [
        for (final t in tester.widgetList<Text>(find.byType(Text)))
          if (t.data != null) t.data!,
      ];

  Future<void> tap(Finder f) async {
    await reveal(f);
    expect(f, findsOneWidget);
    // Built but below the screen, as a request composed low in the tab is.
    await tester.ensureVisible(f);
    await tester.pumpAndSettle();
    await tester.tap(f);
    await tester.pumpAndSettle();
  }

  Future<void> type(Finder f, String text) async {
    await reveal(f);
    expect(f, findsOneWidget);
    await tester.enterText(f, text);
    await tester.pumpAndSettle();
  }

  Future<void> tab(ScreenTab t) => tap(find.byKey(tabKey(t)));

  /// Scroll the open tab until [f] is built, the list building only what is
  /// near the screen.
  Future<void> reveal(Finder f) async {
    if (f.evaluate().isNotEmpty) return;
    final list = find.byWidgetPredicate((w) =>
        w is ListView &&
        w.key is ValueKey<String> &&
        (w.key as ValueKey<String>).value.startsWith('tab-view:'));
    await tester.scrollUntilVisible(f, 120,
        scrollable:
            find.descendant(of: list, matching: find.byType(Scrollable)).first);
    await tester.pumpAndSettle();
  }

  /// The tab's list scrolled to its top.
  Future<void> top() => _scroll(false);

  /// The tab's list scrolled to its end.
  Future<void> end() => _scroll(true);

  Future<void> _scroll(bool toEnd) async {
    FocusManager.instance.primaryFocus?.unfocus();
    final list = find.byWidgetPredicate((w) =>
        w is ListView &&
        w.key is ValueKey<String> &&
        (w.key as ValueKey<String>).value.startsWith('tab-view:'));
    final s = tester.state<ScrollableState>(
        find.descendant(of: list, matching: find.byType(Scrollable)).first);
    s.position.jumpTo(toEnd ? s.position.maxScrollExtent : 0);
    await tester.pumpAndSettle();
  }

  /// The phone as it stands, rendered; written to SOCIAL_TABS_SHOTS/[file]
  /// where the environment names that directory.
  Future<void> shoot(String file) async {
    FocusManager.instance.primaryFocus?.unfocus();
    await tester.pumpAndSettle();
    final boundary =
        shot.currentContext!.findRenderObject()! as RenderRepaintBoundary;
    final png = await tester.runAsync(() async {
      final image = await boundary.toImage(pixelRatio: _ratio);
      final data = await image.toByteData(format: ui.ImageByteFormat.png);
      image.dispose();
      return data!.buffer.asUint8List();
    });
    expect(png, isNotNull);
    final out = Platform.environment['SOCIAL_TABS_SHOTS'];
    if (out != null && out.isNotEmpty) {
      Directory(out).createSync(recursive: true);
      File('$out/$file').writeAsBytesSync(png!);
    }
  }

  /// Nothing on the screen is a term, a functor, a type name, an attempt
  /// number or a Submit button.
  void plain() {
    final all = texts().join('\n');
    for (final w in [
      'Submit',
      'propose',
      'menu',
      'Key',
      'Request',
      'Proposal',
      'Peer',
      'Speech',
      'utterance',
      'speech(',
      'attempt',
      'befriend(',
      'friend(',
      'identity',
      'received',
      'member(',
      'listed',
    ]) {
      expect(all.contains(w), isFalse, reason: '"$w" on the screen:\n$all');
    }
  }
}

/// A test on a phone: [body], and the shadows restored at its end, as the
/// test binding requires of a painting debug variable.
void _phoneTest(String description, Future<void> Function(WidgetTester) body) =>
    testWidgets(description, (tester) async {
      try {
        await body(tester);
      } finally {
        debugDisableShadows = true;
      }
    });

void main() {
  group('reading the type and the state', () {
    test('the requests a Menu offers are read off its default widget', () {
      final gsg = RequestOffers.of(fixtureTerm(_menuD(_gsgProposals)));
      expect(gsg.proposals, {'establish/1', 'befriend/2', 'unfriend/2'});
      expect(gsg.known, isTrue);
      final gsn = RequestOffers.of(fixtureTerm(_menuD(_gsnProposals)));
      expect(gsn.has('message', 3), isTrue);
      expect(gsn.has('post', 4), isTrue);
      expect(gsn.has('sign', 2), isFalse);
      // Declared once, it reads the alternatives of each program it is
      // declared for: the default widget handed with the name.
      final named = RequestOffers.of(fixtureTerm(
          'named(social_menu, ${_menuD(_gsgProposals)})'));
      expect(named.proposals, gsg.proposals);
    });

    test('the last view is the last element of the stream written whole',
        () {
      var (ok, s) = SocialState.read(fixtureTerm('[view([identity], [bob]), '
          'view([identity, friend(bob)], [bob, carol]), pending([2, 2, 1])]'));
      expect(ok, isTrue);
      expect(s!.friends.map(formatTerm), ['bob']);
      expect(s.known.map(formatTerm), ['bob', 'carol']);
      (ok, s) = SocialState.read(fixtureTerm('[]'));
      expect(ok, isTrue);
      expect(s, isNull);
      // A thread of transactions, or of bare states, holds no view.
      (ok, s) = SocialState.read(
          fixtureTerm('[propose(attempt(alice, 1, 1, establish(alice)))]'));
      expect(ok, isFalse);
      (ok, s) = SocialState.read(fixtureTerm('[[identity]]'));
      expect(ok, isFalse);
      (ok, s) = SocialState.read(null);
      expect(ok, isFalse);
    });

    test('a GSN state: chats, groups with leader, members and messages', () {
      final (_, s) = SocialState.read(fixtureTerm('[${_v(_gsnState)}]'));
      expect(s!.identity, isTrue);
      expect(s.received(const GAtom('bob')).map((x) => SpeechAct.of(x)!.text),
          ['Lunch at noon?']);
      final gs = s.groups(me: const GAtom('alice'));
      expect(gs.map((g) => g.title), ['friends', 'hikers']);
      expect(formatTerm(gs[0].leader), 'alice');
      expect(gs[0].members.map(formatTerm), ['alice', 'bob']);
      expect(gs[0].messages.map((x) => SpeechAct.of(x)!.text),
          ['See you all on Sunday']);
      expect(gs[1].members, isEmpty);
      expect(formatTerm(gs[1].leader), 'bob');
      expect(gs[1].messages.map((x) => SpeechAct.of(x)!.text),
          ['Trail at nine']);
      // The members are drawn only in a group the person leads, the
      // leader's state listing them; where the person is not known, in none.
      expect(s.groups().map((g) => g.members), [isEmpty, isEmpty]);
      expect(s.inGroup, isTrue);
    });

    test('a member\'s state: the group, its leader and the message delivered '
        'in it, and no member', () {
      final (_, s) = SocialState.read(fixtureTerm('[${_v('[identity, '
          'friend(alice), ${_member('alice', _friends)}, '
          "received_in(${_sp('Hi all', 'bob')}, alice, $_friends)]")}]'));
      final g = s!.groups(me: const GAtom('bob')).single;
      expect(g.title, 'friends');
      expect(formatTerm(g.leader), 'alice');
      expect(g.members, isEmpty);
      expect(g.messages.map((x) => SpeechAct.of(x)!.text), ['Hi all']);
    });
  });

  group('GSG', () {
    _phoneTest(
        'before her identity is established: her name, Join, which proposes '
        'establish(alice), and the field Add a person by key; one tab',
        (tester) async {
      final p = await _Phone.open(tester, 'gsg');
      await p.views([_v('[]')]);
      await p.menu(1, _gsgProposals);
      expect(find.byKey(tabKey(ScreenTab.friends)), findsOneWidget);
      expect(find.byKey(tabKey(ScreenTab.groups)), findsNothing);
      expect(find.text('GSG'), findsOneWidget);
      expect(p.key(1, 'name'), findsOneWidget);
      expect(find.text('Alice'), findsOneWidget);
      expect(find.widgetWithText(ElevatedButton, 'Join'), findsOneWidget);
      expect(find.text('Add a person by key'), findsOneWidget);
      p.plain();
      await p.shoot('gsg-1-before-join.png');
      await p.tap(p.key(1, 'join'));
      expect(p.grants, ['input(1, [3], propose(establish(alice)))']);
    });

    _phoneTest(
        'after it: the persons known, a friend with End friendship and any '
        'other with Befriend, and Add answering known(Q)', (tester) async {
      final p = await _Phone.open(tester, 'gsg');
      await p.views([
        _v('[]'),
        _v('[identity]', '[bob]'),
        _v('[identity, friend(bob)]', '[bob, carol]'),
      ]);
      await p.menu(2, _gsgProposals, known: '[bob]');
      expect(find.widgetWithText(ElevatedButton, 'Join'), findsNothing);
      expect(
          find.descendant(
              of: p.key(2, 'person:bob'),
              matching: find.widgetWithText(OutlinedButton, 'End friendship')),
          findsOneWidget);
      expect(
          find.descendant(
              of: p.key(2, 'person:carol'),
              matching: find.widgetWithText(ElevatedButton, 'Befriend')),
          findsOneWidget);
      p.plain();
      await p.shoot('gsg-2-friends.png');
      await p.tap(p.key(2, 'befriend:carol'));
      await p.tap(p.key(2, 'unfriend:bob'));
      // Add is enabled once a key is typed.
      expect(
          tester.widget<ElevatedButton>(p.key(2, 'add')).onPressed, isNull);
      await p.type(p.key(2, 'add-field'), 'dan');
      await p.tap(p.key(2, 'add'));
      expect(p.grants, [
        'input(2, [3], propose(befriend(alice, carol)))',
        'input(2, [3], propose(unfriend(alice, bob)))',
        'input(2, [3], known(dan))',
      ]);
    });

    _phoneTest(
        'the request question is drawn again with each view drawn on '
        'construct 0: Carol known once the view says so, though the question '
        'shows only Bob, and Bob a friend once the state says so',
        (tester) async {
      final p = await _Phone.open(tester, 'gsg');
      await p.views([_v('[identity]', '[bob]')]);
      await p.menu(3, _gsgProposals, known: '[bob]');
      expect(p.key(3, 'befriend:bob'), findsOneWidget);
      expect(p.key(3, 'unfriend:bob'), findsNothing);
      expect(p.key(3, 'person:carol'), findsNothing);
      await p.views([
        _v('[identity]', '[bob]'),
        _v('[identity]', '[bob, carol]'),
      ]);
      expect(p.key(3, 'befriend:carol'), findsOneWidget);
      await p.views([
        _v('[identity]', '[bob]'),
        _v('[identity]', '[bob, carol]'),
        _v('[identity, friend(bob)]', '[bob, carol]'),
      ]);
      expect(p.key(3, 'befriend:bob'), findsNothing);
      expect(p.key(3, 'unfriend:bob'), findsOneWidget);
    });

    _phoneTest(
        'an approval question for befriend at the top of Friends, the tab '
        'badged, Accept granting yes', (tester) async {
      final p = await _Phone.open(tester, 'gsg');
      await p.views([_v('[identity, friend(bob)]', '[bob, carol]')]);
      await p.menu(4, _gsgProposals, known: '[bob, carol]');
      await p.draw('draw(5, $_approvalW, approval_w(approval(attempt(carol, '
          '7, 7, befriend(carol, alice), none), input([2]))))');
      final ys = [
        tester.getTopLeft(find.text('Carol asks to be your friend.')).dy,
        tester.getTopLeft(p.key(4, 'name')).dy,
      ];
      expect(ys[0], lessThan(ys[1]));
      expect(
          find.descendant(
              of: find.byKey(tabKey(ScreenTab.friends)),
              matching: find.text('1')),
          findsOneWidget);
      p.plain();
      await p.shoot('gsg-3-approval.png');
      await p.tap(find.widgetWithText(ElevatedButton, 'Accept'));
      expect(p.grants, ['input(5, [2], yes)']);
    });

    _phoneTest(
        'no request question open: construct 0 drawn by last_state, the '
        'friends, and no control', (tester) async {
      final p = await _Phone.open(tester, 'gsg');
      await p.views(
          [_v('[identity, friend(bob), friend(carol)]', '[bob, carol, dan]')]);
      expect(p.key(0, 'person:bob'), findsOneWidget);
      expect(p.key(0, 'person:carol'), findsOneWidget);
      expect(find.byType(ElevatedButton), findsNothing);
      expect(find.byType(OutlinedButton), findsNothing);
      expect(find.byType(TextField), findsNothing);
      p.plain();
      await p.shoot('gsg-4-last-state.png');
    });

    _phoneTest(
        'social_menu with no view on construct 0, a thread of transactions: '
        'named, and taking no input', (tester) async {
      final p = await _Phone.open(tester, 'gsg');
      await p.draw('draw(0, thread, output([propose(attempt(alice, 1, 1, '
          'establish(alice)))]))');
      await p.menu(1, _gsgProposals);
      expect(find.textContaining('social_menu cannot draw'), findsOneWidget);
      expect(find.byType(ElevatedButton), findsNothing);
    });

    _phoneTest(
        'each button of social_menu grants what the default widget of Menu '
        'grants for the same request', (tester) async {
      final p = await _Phone.open(tester, 'gsg');
      // The default widget: the request type's menu, the alternative and
      // its fields chosen and typed, and Submit.
      final d = _menuD(_gsnProposals);
      await p.draw('draw(7, $d, menu_w(menu(alice, [bob], input([3]))))');
      Finder node(String kind, List<int> at) =>
          find.byKey(nodeKey(7, kind, at));
      Future<void> choose(List<int> at, String label) => p.tap(find
          .descendant(
              of: node('menu', at),
              matching: find.widgetWithText(ChoiceChip, label))
          .first);
      Future<void> type(String kind, List<int> at, String t) => p.type(
          find.descendant(of: node(kind, at), matching: find.byType(TextField)),
          t);
      await choose([3], 'propose');
      await choose([3, 1], 'befriend');
      await type('peer', [3, 1, 1], 'alice');
      await type('peer', [3, 1, 2], 'bob');
      await p.tap(find.byKey(submitKey(7, [3])));
      await choose([3], 'propose');
      await choose([3, 1], 'message');
      await type('peer', [3, 1, 1], 'alice');
      await type('peer', [3, 1, 2], 'bob');
      await p.tap(find.byKey(openKey(7, [3, 1, 3])));
      await p.tap(find.byKey(openKey(7, [3, 1, 3, 1])));
      await choose([3, 1, 3, 1], 'utterance');
      await type('text', [3, 1, 3, 1, 1], 'Lunch?');
      await type('peer', [3, 1, 3, 2], 'alice');
      await p.tap(find.byKey(submitKey(7, [3])));
      await choose([3], 'known');
      await type('peer', [3, 1], 'dan');
      await p.tap(find.byKey(submitKey(7, [3])));
      final byDefault = [...p.grants];
      p.grants.clear();
      // social_menu over the same view, with a state on construct 0.
      await p.draw('withdraw(7)');
      await p.views([_v('[identity]', '[bob]')]);
      await p.menu(7, _gsnProposals, known: '[bob]');
      await p.tap(p.key(7, 'befriend:bob'));
      await p.views([_v('[identity, friend(bob)]', '[bob]')]);
      await p.tap(p.key(7, 'person:bob'));
      await p.type(p.key(7, 'chat-field:bob'), 'Lunch?');
      await p.tap(p.key(7, 'send:bob'));
      await p.tap(p.key(7, 'back'));
      await p.type(p.key(7, 'add-field'), 'dan');
      await p.tap(p.key(7, 'add'));
      expect(byDefault, [
        'input(7, [3], propose(befriend(alice, bob)))',
        'input(7, [3], propose(message(alice, bob, '
            "speech(utterance('Lunch?'), alice))))",
        'input(7, [3], known(dan))',
      ]);
      expect(p.grants, byDefault);
    });
  });

  group('GSN', () {
    _phoneTest(
        'Friends: the persons, the chat with a friend opened by tapping them, '
        'its messages each with its author under it, and Send proposing '
        'message(alice, Q, x); a befriend approval at the top', (tester) async {
      final p = await _Phone.open(tester, 'gsn');
      await p.views([_v(_gsnState, '[bob, carol, dan]')]);
      await p.menu(1, _gsnProposals, known: '[bob, carol, dan]');
      await p.draw('draw(2, $_approvalW, approval_w(approval(attempt(dan, '
          '4, 4, befriend(dan, alice), none), input([2]))))');
      expect(find.byKey(tabKey(ScreenTab.groups)), findsOneWidget);
      expect(find.text('Dan asks to be your friend.'), findsOneWidget);
      expect(p.key(1, 'unfriend:bob'), findsOneWidget);
      expect(p.key(1, 'befriend:dan'), findsOneWidget);
      p.plain();
      await p.shoot('gsn-1-friends.png');
      await p.tap(p.key(1, 'person:bob'));
      expect(p.key(1, 'chat:bob'), findsOneWidget);
      expect(find.text('Lunch at noon?'), findsOneWidget);
      expect(
          find.descendant(
              of: p.key(1, 'message:${formatTerm(fixtureTerm(_sp('Lunch at noon?', 'bob')))}'),
              matching: find.text('Bob')),
          findsOneWidget);
      await p.type(p.key(1, 'chat-field:bob'), 'Yes, see you');
      p.plain();
      await p.shoot('gsn-2-chat.png');
      await p.tap(p.key(1, 'send:bob'));
      expect(p.grants, [
        'input(1, [3], propose(message(alice, bob, '
            "speech(utterance('Yes, see you'), alice))))",
      ]);
      // The chat stays open as the agent asks its next request question.
      await p.draw('withdraw(1)');
      await p.menu(3, _gsnProposals, known: '[bob, carol, dan]');
      expect(p.key(3, 'chat:bob'), findsOneWidget);
      await p.tap(p.key(3, 'back'));
      expect(p.key(3, 'person:bob'), findsOneWidget);
    });

    _phoneTest(
        'Groups: a join approval at the top, New group, the group she leads '
        'with Add member and Close group, the group she is in with Leave '
        'group, each with its leader, members, messages and Send',
        (tester) async {
      final p = await _Phone.open(tester, 'gsn');
      await p.views([_v(_gsnState, '[bob, carol]')]);
      await p.menu(1, _gsnProposals, known: '[bob, carol]');
      await p.draw('draw(2, $_approvalW, approval_w(approval(attempt(dan, '
          '5, 5, join(dan, alice, speech(utterance(chess), dan)), none), '
          'input([2]))))');
      // The join approval badges Groups and is not in Friends.
      expect(find.text('Dan asks you to join the group chess.'), findsNothing);
      expect(
          find.descendant(
              of: find.byKey(tabKey(ScreenTab.groups)),
              matching: find.text('1')),
          findsOneWidget);
      await p.tab(ScreenTab.groups);
      expect(find.text('Dan asks you to join the group chess.'), findsOneWidget);
      final f = groupKey(const GAtom('alice'), fixtureTerm(_friends));
      final h = groupKey(const GAtom('bob'), fixtureTerm(_hikers));
      expect(
          tester.getTopLeft(find.text('Dan asks you to join the group chess.')).dy,
          lessThan(tester.getTopLeft(p.key(1, 'group-field')).dy));
      Finder inGroup(String g, Finder x) =>
          find.descendant(of: p.key(1, 'group:$g'), matching: x);
      expect(inGroup(f, find.text('friends')), findsOneWidget);
      expect(inGroup(f, find.text('Led by Alice')), findsOneWidget);
      expect(inGroup(f, find.text('Members: Alice, Bob')), findsOneWidget);
      expect(inGroup(f, find.text('See you all on Sunday')), findsOneWidget);
      expect(inGroup(f, find.text('Bob')), findsOneWidget);
      expect(inGroup(f, find.widgetWithText(ActionChip, 'Carol')),
          findsOneWidget);
      expect(inGroup(f, find.widgetWithText(ActionChip, 'Bob')), findsNothing);
      expect(inGroup(f, find.text('Leave group')), findsNothing);
      p.plain();
      await p.shoot('gsn-3-groups.png');
      await p.type(p.key(1, 'group-field'), 'book club');
      await p.tap(p.key(1, 'create'));
      await p.tap(p.key(1, 'join:$f:carol'));
      await p.type(p.key(1, 'post-field:$f'), 'Bring snacks');
      await p.tap(p.key(1, 'post:$f'));
      await p.tap(p.key(1, 'close:$f'));
      await p.reveal(p.key(1, 'group:$h'));
      expect(inGroup(h, find.text('Led by Bob')), findsOneWidget);
      expect(inGroup(h, find.text('Trail at nine')), findsOneWidget);
      expect(inGroup(h, find.text('Close group')), findsNothing);
      expect(inGroup(h, find.byType(ActionChip)), findsNothing);
      await p.type(p.key(1, 'post-field:$h'), 'I will come');
      await p.tap(p.key(1, 'post:$h'));
      await p.tap(p.key(1, 'leave:$h'));
      await p.end();
      p.plain();
      await p.shoot('gsn-4-groups-member.png');
      expect(p.grants, [
        'input(1, [3], propose(create(alice, '
            "speech(utterance('book club'), alice))))",
        'input(1, [3], propose(join(alice, carol, $_friends)))',
        'input(1, [3], propose(post_own(alice, '
            "speech(utterance('Bring snacks'), alice), $_friends)))",
        'input(1, [3], propose(close(alice, $_friends)))',
        'input(1, [3], propose(post(alice, bob, '
            "speech(utterance('I will come'), alice), $_hikers)))",
        'input(1, [3], propose(leave(bob, alice, $_hikers)))',
      ]);
      await p.top();
      await p.tap(find.widgetWithText(ElevatedButton, 'Join'));
      expect(p.grants.last, 'input(2, [2], yes)');
    });

    _phoneTest(
        'no request question open: last_state draws each tab, the chat '
        'and the groups, with no control', (tester) async {
      final p = await _Phone.open(tester, 'gsn');
      await p.views([_v(_gsnState, '[bob, carol]')]);
      expect(find.byKey(tabKey(ScreenTab.groups)), findsOneWidget);
      await p.tap(p.key(0, 'person:bob'));
      expect(find.text('Lunch at noon?'), findsOneWidget);
      expect(find.byType(TextField), findsNothing);
      await p.tap(p.key(0, 'back'));
      await p.tab(ScreenTab.groups);
      expect(find.text('Led by Alice'), findsOneWidget);
      expect(find.byType(TextField), findsNothing);
      expect(find.byType(OutlinedButton), findsNothing);
      p.plain();
      await p.shoot('gsn-5-last-state-groups.png');
    });
  });

  group('GSNP', () {
    _phoneTest(
        'Friends as GSG\'s; an item from a friend in the chat with them, '
        'with its author and who forwarded it; one from a person who is not '
        'a friend under Messages; and in a group no text and Send, a post '
        'carrying an item the person signed', (tester) async {
      final p = await _Phone.open(tester, 'gsnp');
      final flood = 'speech(${_sp('Flood on Main Street', 'dan')}, bob)';
      final road = _sp('Road closed', 'dan');
      await p.views([
        _v(
            '[identity, friend(bob), item($flood), item($road), '
            '${_member('alice', _friends)}, listed(alice, $_friends)]',
            '[bob]')
      ]);
      await p.menu(1, _gsnpProposals, known: '[bob]');
      expect(p.key(1, 'unfriend:bob'), findsOneWidget);
      // Dan is not a friend: his item is under Messages.
      expect(p.key(1, 'items'), findsOneWidget);
      expect(find.text('Road closed'), findsOneWidget);
      Finder inItem(String x, Finder f) => find.descendant(
          of: p.key(1, 'item:${formatTerm(fixtureTerm(x))}'), matching: f);
      // Bob's item is not under Messages; his row shows it as the last of
      // the chat with him.
      expect(p.key(1, 'item:${formatTerm(fixtureTerm(flood))}'), findsNothing);
      expect(
          find.descendant(
              of: p.key(1, 'person:bob'),
              matching: find.text('Flood on Main Street')),
          findsOneWidget);
      expect(inItem(road, find.text('Author: Dan')), findsOneWidget);
      expect(inItem(road, find.textContaining('via')), findsNothing);
      // Bob's item is in the chat with Bob, the friend it came from.
      await p.tap(p.key(1, 'person:bob'));
      expect(p.key(1, 'chat:bob'), findsOneWidget);
      expect(inItem(flood, find.text('Flood on Main Street')), findsOneWidget);
      expect(inItem(flood, find.text('Author: Dan')), findsOneWidget);
      expect(inItem(flood, find.text('via Bob')), findsOneWidget);
      expect(find.text('Road closed'), findsNothing);
      p.plain();
      await p.tap(p.key(1, 'back'));
      await p.tab(ScreenTab.groups);
      final f = groupKey(const GAtom('alice'), fixtureTerm(_friends));
      expect(p.key(1, 'group:$f'), findsOneWidget);
      expect(p.key(1, 'post-field:$f'), findsNothing);
      expect(p.key(1, 'close:$f'), findsOneWidget);
    });

    // The play of GAI's GSNP figure (GAI #3 Cowork, 2026-10-19 13:31 UTC,
    // item 4): Dan writes "Flood on Main Street" and sends it to Eve, Eve
    // forwards it to Carol, and Carol forwards it to Bob; Bob's friends are
    // Carol and Fay.
    final bobFlood = 'speech(speech(${_sp('Flood on Main Street', 'dan')}, '
        'eve), carol)';
    final bobState = '[identity, friend(carol), friend(fay), '
        'item($bobFlood)]';
    Future<_Phone> bobInChat(WidgetTester tester) async {
      final p = await _Phone.open(tester, 'gsnp', me: 'bob');
      await p.views([_v(bobState, '[carol, fay]')]);
      await p.menu(1, _gsnpProposals, known: '[carol, fay]');
      await p.tap(p.key(1, 'person:carol'));
      return p;
    }

    _phoneTest(
        '(a) Bob\'s chat with Carol: "Flood on Main Street", "Author: Dan", '
        '"via Eve, via Carol", and Forward under it', (tester) async {
      final p = await bobInChat(tester);
      final k = formatTerm(fixtureTerm(bobFlood));
      expect(p.key(1, 'chat:carol'), findsOneWidget);
      final item = p.key(1, 'item:$k');
      expect(
          find.descendant(of: item, matching: find.byType(Text))
              .evaluate()
              .map((e) => (e.widget as Text).data)
              .toList(),
          ['Flood on Main Street', 'Author: Dan', 'via Eve, via Carol',
           'Forward']);
      expect(p.key(1, 'forward:$k'), findsOneWidget);
      expect(p.key(1, 'send:carol'), findsOneWidget);
      p.plain();
      await p.shoot('gsnp-a-bob-chat.png');
    });

    _phoneTest(
        '(b) Forward opens a picker of Bob\'s friends; picking Fay asks '
        '"Forward to Fay?  You warrant that you know Carol personally.", '
        'No closing it and Yes proposing forward(bob, fay, x)',
        (tester) async {
      final p = await bobInChat(tester);
      final k = formatTerm(fixtureTerm(bobFlood));
      await p.tap(p.key(1, 'forward:$k'));
      expect(p.key(1, 'forward-picker:$k'), findsOneWidget);
      expect(p.key(1, 'forward-to:$k:carol'), findsOneWidget);
      expect(p.key(1, 'forward-to:$k:fay'), findsOneWidget);
      // Forward again closes the picker.
      await p.tap(p.key(1, 'forward:$k'));
      expect(p.key(1, 'forward-picker:$k'), findsNothing);
      await p.tap(p.key(1, 'forward:$k'));
      await p.tap(p.key(1, 'forward-to:$k:fay'));
      expect(p.key(1, 'forward-picker:$k'), findsNothing);
      final w = p.key(1, 'warranty:$k');
      expect(
          find.descendant(of: w, matching: find.byType(Text))
              .evaluate()
              .map((e) => (e.widget as Text).data)
              .toList(),
          ['Forward to Fay?', 'You warrant that you know Carol personally.',
           'Yes', 'No']);
      p.plain();
      await p.shoot('gsnp-b-bob-forward.png');
      await p.tap(p.key(1, 'forward-no:$k'));
      expect(w, findsNothing);
      expect(p.grants, isEmpty);
      await p.tap(p.key(1, 'forward:$k'));
      await p.tap(p.key(1, 'forward-to:$k:fay'));
      await p.tap(p.key(1, 'forward-yes:$k'));
      expect(w, findsNothing);
      expect(p.grants,
          ['input(1, [3], propose(forward(bob, fay, $bobFlood)))']);
    });

    _phoneTest(
        'Send in a chat signs the text, sign(bob, x), and proposes '
        'message(bob, carol, x) on the next request question once the view '
        'holds authored(x); the item drawn in the chat, with no Forward',
        (tester) async {
      final p = await bobInChat(tester);
      final x = _sp('Water rising on Elm', 'bob');
      final kx = formatTerm(fixtureTerm(x));
      await p.type(p.key(1, 'chat-field:carol'), 'Water rising on Elm');
      await p.tap(p.key(1, 'send:carol'));
      expect(p.grants, ['input(1, [3], propose(sign(bob, $x)))']);
      // The sign commits: the view holds item(x) and authored(x).  The
      // question that took the sign takes no second request.
      await p.views([
        _v(bobState, '[carol, fay]'),
        _v('[item($x), authored($x), ${bobState.substring(1)}', '[carol, fay]'),
      ]);
      expect(p.grants, hasLength(1));
      // The next request question: the message, proposed on it, once.
      await p.draw('withdraw(1)');
      await p.menu(2, _gsnpProposals, known: '[carol, fay]');
      await p.views([
        _v(bobState, '[carol, fay]'),
        _v('[item($x), authored($x), ${bobState.substring(1)}', '[carol, fay]'),
      ]);
      expect(p.grants, [
        'input(1, [3], propose(sign(bob, $x)))',
        'input(2, [3], propose(message(bob, carol, $x)))',
      ]);
      // The chat stays open; the item Bob authored is in it, his name its
      // author, and no Forward under it.
      expect(p.key(2, 'chat:carol'), findsOneWidget);
      expect(
          find.descendant(
              of: p.key(2, 'item:$kx'), matching: find.text('Author: Bob')),
          findsOneWidget);
      expect(p.key(2, 'forward:$kx'), findsNothing);
      expect(p.key(2, 'forward:${formatTerm(fixtureTerm(bobFlood))}'),
          findsOneWidget);
      await p.draw('withdraw(2)');
      await p.menu(3, _gsnpProposals, known: '[carol, fay]');
      expect(p.grants, hasLength(2));
      // Out of the chat, the item is not under Messages.
      await p.tap(p.key(3, 'back'));
      expect(p.key(3, 'items'), findsNothing);
    });

    _phoneTest(
        'a message held for a sign that did not go through is dropped, so '
        'the same text sent again goes once', (tester) async {
      final p = await bobInChat(tester);
      final x = _sp('Water rising on Elm', 'bob');
      final authored =
          _v('[item($x), authored($x), ${bobState.substring(1)}', '[carol, fay]');
      Future<void> send() async {
        await p.type(p.key(p.grants.length + 1, 'chat-field:carol'),
            'Water rising on Elm');
        await p.tap(p.key(p.grants.length + 1, 'send:carol'));
      }

      await send();
      await p.draw('withdraw(1)');
      await p.views([
        _v(bobState, '[carol, fay]'),
        'refused(sign(bob, $x), bob, unable([identity], []))',
      ]);
      await p.menu(2, _gsnpProposals, known: '[carol, fay]');
      expect(p.grants, ['input(1, [3], propose(sign(bob, $x)))']);
      // Sent again: the second sign commits, and one message goes.
      await send();
      await p.draw('withdraw(2)');
      await p.views([
        _v(bobState, '[carol, fay]'),
        'refused(sign(bob, $x), bob, unable([identity], []))',
        authored,
      ]);
      await p.menu(3, _gsnpProposals, known: '[carol, fay]');
      await p.draw('withdraw(3)');
      await p.menu(4, _gsnpProposals, known: '[carol, fay]');
      expect(p.grants, [
        'input(1, [3], propose(sign(bob, $x)))',
        'input(2, [3], propose(sign(bob, $x)))',
        'input(3, [3], propose(message(bob, carol, $x)))',
      ]);
    });
  });

  group('notices', () {
    const flood = "speech(utterance('Flood on Main Street'), dan)";
    String say(String n, [String? me = 'alice']) =>
        SocialNotice.of(fixtureTerm(n))!
            .sentence(me == null ? null : GAtom(me));

    test('declined: the refuser named, and the transaction as the person '
        'told reads it, whether they requested it or answered yes', () {
      // Alice requested it; Bob, asked, answered no.
      expect(say('refused(befriend(alice, bob), bob, declined)'),
          'Bob declined your friendship.');
      expect(say('refused(join(alice, bob, $_friends), bob, declined)'),
          'Bob declined to join the group friends.');
      // Alice requested Carol and Dave's befriending; Bob and Carol answered
      // yes, and Dave answered no: each is told, Alice's request and their
      // answers yes ending by the refusal.
      const cb = 'refused(child_befriend(carol, dave, alice, bob), dave, '
          'declined)';
      expect(say(cb), 'Dave declined the friendship of Carol and Dave.');
      expect(say(cb, 'bob'), 'Dave declined the friendship of Carol and Dave.');
      expect(say(cb, 'carol'), 'Dave declined your friendship.');
      // A parent declining the group of their child's leader.
      expect(
          say('refused(child_join(carol, dave, bob, $_friends), bob, '
              'declined)', 'dave'),
          'Bob declined your joining the group friends.');
      // The other parent declining an entering by two.
      expect(say('refused(enter_2(bob, grace, dave), grace, declined)', 'bob'),
          'Grace declined your entering Dave as your child, with Grace.');
      expect(say('refused(befriend(alice, bob), bob, declined)', null),
          'Bob declined the friendship of Alice and Bob.');
      expect(
          SocialNotice.of(fixtureTerm('refused(join(alice, bob, $_friends), '
                  'bob, declined)'))!
              .concernsGroup,
          isTrue);
      expect(
          SocialNotice.of(
                  fixtureTerm('refused(befriend(alice, bob), bob, declined)'))!
              .concernsGroup,
          isFalse);
    });

    test('unable: the refuser\'s machine refused it, and the atoms its state '
        'lacks and has, in words', () {
      expect(say('refused(befriend(alice, bob), bob, unable([identity], []))'),
          "Bob's machine refused your friendship: Bob has no identity.");
      expect(
          say('refused(befriend(alice, bob), bob, unable([], [child, '
              'retired]))'),
          "Bob's machine refused your friendship: Bob is a child; Bob is "
          'retired.');
      expect(
          say('refused(child_befriend(carol, dave, alice, bob), dave, '
              'unable([parent(bob)], [friend(carol)]))'),
          "Dave's machine refused the friendship of Carol and Dave: Bob is "
          "not Dave's parent; Dave and Carol are friends.");
      expect(
          say('refused(join(alice, bob, $_friends), bob, unable([], '
              '[${_member('alice', _friends)}]))'),
          "Bob's machine refused Bob's joining the group friends: Bob is a "
          'member of the group friends.');
      expect(
          say("refused(message(alice, carol, speech(utterance('Hi'), alice)), "
              'carol, unable([friend(alice)], []))'),
          "Carol's machine refused your message to Carol: Carol and you are "
          'not friends.');
      expect(
          say('refused(forward(bob, fay, $flood), fay, unable([identity], []))',
              'bob'),
          "Fay's machine refused the forwarding of \"Flood on Main Street\" "
          'to Fay: Fay has no identity.');
      expect(
          say('refused(of_age(bob, dave), dave, unable([parent(bob)], []))',
              'bob'),
          "Dave's machine refused your declaring Dave of age: you are not "
          "Dave's parent.");
      // An adult's request to meet a child, offered on the child's row, the
      // adult's state not recording them as one: the child's machine refuses
      // it, meet forbidding child at both roles, and the adult is told (CSSN,
      // Section 4.6.2).
      expect(say('refused(meet(alice, dave), dave, unable([], [child]))'),
          "Dave's machine refused your meeting: Dave is a child.");
      // No atoms: a vote no on a request no longer pending.
      expect(say('refused(befriend(alice, bob), bob, unable([], []))'),
          "Bob's machine refused your friendship.");
      // Any other atom, by its predicate and its arguments.
      expect(
          say("refused(befriend(alice, bob), bob, unable([], "
              "[received(speech(utterance('Hi'), alice), alice)]))"),
          "Bob's machine refused your friendship: Bob holds received \"Hi\" "
          'you.');
      // A class not worded here: the act, its schema and its persons.
      expect(
          say('refused(drop_all(alice, bob), bob, unable([identity], []))'),
          "Bob's machine refused the act drop all of you and Bob: Bob has "
          'no identity.');
    });

    test('the refuser the person: their own machine refused it at once', () {
      // A Propose that refuses: no part of the class admits Alice's state.
      expect(
          say("refused(message(alice, carol, speech(utterance('Hi'), alice)), "
              'alice, unable([friend(carol)], []))'),
          'Your machine refused your message to Carol: you and Carol are not '
          'friends.');
      expect(
          say('refused(enter(alice, carol), alice, unable([identity], '
              '[child]))'),
          'Your machine refused your entering Carol as your child: you have '
          'no identity; you are a child.');
      expect(
          say('refused(sign(bob, $flood), bob, unable([], [retired]))', 'bob'),
          'Your machine refused the signing of "Flood on Main Street": you '
          'are retired.');
      // A guard's vote no, at Start: the request it names no longer pending.
      expect(
          say('refused(child_close(carol, alice, [dave], $_friends), alice, '
              'unable([], []))'),
          'Your machine refused the closing of the group friends.');
      // The child asked, at their own phone.
      expect(
          say('refused(enter(alice, carol), carol, unable([], [identity]))',
              'carol'),
          "Your machine refused Alice's entering you as their child: you have "
          'an identity.');
    });

    test('a notice is refused(P, Q, D) and nothing else: refused/2 and '
        'yielded/1 are not of Output', () {
      expect(SocialNotice.of(fixtureTerm('view([], [])')), isNull);
      expect(SocialNotice.of(fixtureTerm('refused(befriend(alice, bob), bob)')),
          isNull);
      expect(SocialNotice.of(fixtureTerm('yielded(befriend(alice, bob))')),
          isNull);
      expect(
          SocialNotice.of(
              fixtureTerm('refused(befriend(alice, bob), bob, maybe)')),
          isNull);
      final (ok, _) = SocialState.read(fixtureTerm(
          '[view([identity], [bob]), yielded(befriend(alice, bob))]'));
      expect(ok, isFalse);
    });

    test('the last view is read past the notices written after it; those '
        'notices are drawn, and every notice is kept in the order written',
        () {
      final (ok, s) = SocialState.read(fixtureTerm('[view([identity], [bob]), '
          'refused(befriend(alice, bob), bob, declined), '
          'view([identity], [bob, carol]), '
          'refused(befriend(alice, carol), carol, unable([identity], [])), '
          'pending([2, 2, 2, 2, 1])]'));
      expect(ok, isTrue);
      expect(s!.known.map(formatTerm), ['bob', 'carol']);
      // "a widget that draws the last view written and the notices written
      // after it" (vGLP f0acbc1, Section 5.4, "The compilation").
      expect(s.notices.map((n) => formatTerm(n.part)),
          ['befriend(alice, carol)']);
      expect(s.notices.map((n) => (formatTerm(n.by), n.declined)),
          [('carol', false)]);
      expect(s.notices.single.lacks.map(formatTerm), ['identity']);
      expect(s.written.map((n) => formatTerm(n.part)),
          ['befriend(alice, bob)', 'befriend(alice, carol)']);
    });

    _phoneTest(
        'a notice is one line at the top of the tab it concerns, the latest '
        'first: a befriend in Friends, a join in Groups', (tester) async {
      final p = await _Phone.open(tester, 'gsnp');
      final state = _v('[identity, friend(carol), ${_member('alice', _friends)}, '
          'listed(alice, $_friends)]', '[bob, carol]');
      await p.views([
        state,
        'refused(befriend(alice, bob), bob, declined)',
      ]);
      await p.menu(1, _gsnpProposals, known: '[bob, carol]');
      expect(find.text('Bob declined your friendship.'), findsOneWidget);
      expect(
          tester.getTopLeft(find.text('Bob declined your friendship.')).dy,
          lessThan(tester.getTopLeft(p.key(1, 'person:bob')).dy));
      // The person may befriend Bob again.
      expect(p.key(1, 'befriend:bob'), findsOneWidget);
      p.plain();
      await p.shoot('notice-bob-declined.png');
      const toCarol = "Carol's machine refused your message to Carol: Carol "
          'and you are not friends.';
      final notices = [
        'refused(befriend(alice, bob), bob, declined)',
        'refused(join(alice, carol, $_friends), carol, declined)',
        "refused(message(alice, carol, speech(utterance('Hi'), alice)), "
            'carol, unable([friend(alice)], []))',
      ];
      await p.views([state, ...notices]);
      final friends = p.texts();
      expect(friends.indexOf(toCarol),
          lessThan(friends.indexOf('Bob declined your friendship.')));
      expect(find.text('Carol declined to join the group friends.'),
          findsNothing);
      // In the chat with Carol, the notices of Friends at its top.
      await p.tap(p.key(1, 'person:carol'));
      expect(find.text(toCarol), findsOneWidget);
      await p.tap(p.key(1, 'back'));
      await p.tab(ScreenTab.groups);
      expect(find.text('Carol declined to join the group friends.'),
          findsOneWidget);
      expect(find.text('Bob declined your friendship.'), findsNothing);
      p.plain();
      // A notice is drawn until the next view is written (vGLP f0acbc1).
      await p.views([state, ...notices, state]);
      expect(find.text('Carol declined to join the group friends.'),
          findsNothing);
      await p.tab(ScreenTab.friends);
      expect(find.text('Bob declined your friendship.'), findsNothing);
      expect(find.text(toCarol), findsNothing);
    });

    _phoneTest(
        'with no request question open, last_state draws the notices',
        (tester) async {
      final p = await _Phone.open(tester, 'gsg');
      // No request question drawn, the person's own key is not shown, and
      // each person is named.
      const said = 'Bob declined the friendship of Alice and Bob.';
      await p.views([
        _v('[identity]', '[bob]'),
        'refused(befriend(alice, bob), bob, declined)',
      ]);
      expect(find.text(said), findsOneWidget);
      expect(find.byType(ElevatedButton), findsNothing);
      await p.views([
        _v('[identity]', '[bob]'),
        'refused(befriend(alice, bob), bob, declined)',
        _v('[identity]', '[bob, carol]'),
      ]);
      expect(find.text(said), findsNothing);
    });
  });

  // GAI's app (programs/apps/gai), the screens of GAI's figure (GAI's paper,
  // Figure "The GAI app"; Code_inbox.md, GAI #5 Cowork, 2026-10-19 20:36 UTC):
  // the AI agents tab, the approval of agent_befriend at the principal and at
  // the other party, and the developer read from the attestation, y of
  // agent_of(r, p, y), its first signer.
  group('GAI', () {
    const code = "speech(speech(utterance('The code of Alf'), acme_ltd), alice)";
    const attestation = 'speech($code, alice)';
    final summary = _sp('Summary of the news', 'alf');
    const aliceState = '[principal_of(alf), friend(alf), item($code), '
        'authored($code), friend(bob), identity]';
    const agentBefriend = 'agent_befriend(alf, bob, alice, $attestation)';

    /// The texts construct [id] draws, in order.
    List<String?> textsOf(WidgetTester tester, int id) => [
          for (final t in tester.widgetList<Text>(find.descendant(
              of: find.byKey(constructKey(id)), matching: find.byType(Text))))
            t.data,
        ];

    _phoneTest(
        '(a) Alice\'s AI agents tab: Create an AI agent, Name and the code, '
        '"Code signed by Acme Ltd", "You attest that Alf runs it." and Create, '
        'proposing sign(alice, k) and, once her view holds item(k), '
        'enter(alice, alf, k); what she typed stays; then Alf listed with '
        'Retire, proposing retire(alice, alf); once retired, "Retired"',
        (tester) async {
      final k = formatTerm(fixtureTerm(code));
      final p = await _Phone.open(tester, 'gai');
      await p.views([_v('[friend(bob), identity]', '[alf, bob]')]);
      await p.menu(1, _gaiProposals, known: '[alf, bob]');
      expect(find.byKey(tabKey(ScreenTab.friends)), findsOneWidget);
      expect(find.byKey(tabKey(ScreenTab.groups)), findsNothing);
      expect(find.byKey(tabKey(ScreenTab.agents)), findsOneWidget);
      expect(find.text('AI agents'), findsOneWidget);
      await p.tab(ScreenTab.agents);
      expect(find.text('Create an AI agent'), findsOneWidget);
      expect(find.text('Code signed by its developer'), findsOneWidget);
      expect(find.text('You attest that the AI agent runs it.'), findsOneWidget);
      Future<void> createEnabled(int id, bool enabled) async => expect(
          tester.widget<ElevatedButton>(p.key(id, 'create-agent')).onPressed !=
              null,
          enabled);
      await createEnabled(1, false);
      await p.type(p.key(1, 'agent-name'), 'alf');
      expect(find.text('You attest that Alf runs it.'), findsOneWidget);
      await createEnabled(1, false);
      await p.type(p.key(1, 'agent-code'), 'The code of Alf');
      await p.type(p.key(1, 'agent-developer'), 'acme_ltd');
      expect(find.text('Code signed by Acme Ltd'), findsOneWidget);
      await createEnabled(1, true);
      p.plain();
      await p.tap(p.key(1, 'create-agent'));
      expect(p.grants, ['input(1, [3], propose(sign(alice, $k)))']);
      // The sign commits: her view holds item(k); on the next request
      // question the creation is proposed, and only then.
      await p.draw('withdraw(1)');
      await p.views([
        _v('[friend(bob), identity]', '[alf, bob]'),
        _v('[item($code), authored($code), friend(bob), identity]',
            '[alf, bob]'),
      ]);
      expect(p.grants, hasLength(1));
      await p.menu(2, _gaiProposals, known: '[alf, bob]');
      expect(p.grants, [
        'input(1, [3], propose(sign(alice, $k)))',
        'input(2, [3], propose(enter(alice, alf, $k)))',
      ]);
      // Once enter commits, Alice is Alf's principal: Alf listed, with
      // Retire; the form keeps what she typed, and Create is disabled, Alf
      // being her AI agent.
      await p.draw('withdraw(2)');
      await p.views([_v(aliceState, '[alf, bob]')]);
      await p.menu(3, _gaiProposals, known: '[alf, bob]');
      await p.tab(ScreenTab.agents);
      expect(find.text('You attest that Alf runs it.'), findsOneWidget);
      expect(find.text('Code signed by Acme Ltd'), findsOneWidget);
      await createEnabled(3, false);
      expect(
          find.descendant(
              of: p.key(3, 'agent:alf'),
              matching: find.widgetWithText(OutlinedButton, 'Retire')),
          findsOneWidget);
      expect(p.texts(),
          containsAllInOrder(['Create an AI agent', 'Create', 'Alf', 'Retire']));
      expect(p.grants, hasLength(2));
      p.plain();
      await p.tap(p.key(3, 'retire:alf'));
      expect(p.grants.last, 'input(3, [3], propose(retire(alice, alf)))');
      // Once retired, Alf is listed as retired, with no Retire.
      await p.draw('withdraw(3)');
      await p.views([
        _v('[retired_of(alf), principal_of(alf), item($code), '
            'authored($code), friend(bob), identity]', '[alf, bob]')
      ]);
      await p.menu(4, _gaiProposals, known: '[alf, bob]');
      await p.tab(ScreenTab.agents);
      expect(
          find.descendant(of: p.key(4, 'agent:alf'), matching: find.text('Retired')),
          findsOneWidget);
      expect(p.key(4, 'retire:alf'), findsNothing);
    });

    _phoneTest(
        'Alice\'s own AI agent: in Friends, Alf\'s row offers no End '
        'friendship, unfriend_p forbidding principal_of(alf) at her, while '
        'Bob\'s does; in AI agents, Alf\'s row has Retire', (tester) async {
      final p = await _Phone.open(tester, 'gai');
      await p.views([_v(aliceState, '[alf, bob]')]);
      await p.menu(1, _gaiProposals, known: '[alf, bob]');
      expect(p.key(1, 'person:alf'), findsOneWidget);
      expect(p.key(1, 'unfriend:alf'), findsNothing);
      expect(
          find.descendant(
              of: p.key(1, 'person:alf'),
              matching: find.text('End friendship')),
          findsNothing);
      expect(p.key(1, 'befriend:alf'), findsNothing);
      // She holds principal_of(alf), not agent_of(alf, p, y): her row of Alf
      // names no principal and no developer.
      expect(
          find.descendant(
              of: p.key(1, 'person:alf'),
              matching: find.textContaining('an AI agent of')),
          findsNothing);
      expect(
          find.descendant(
              of: p.key(1, 'person:alf'),
              matching: find.textContaining('Developer')),
          findsNothing);
      expect(
          find.descendant(
              of: p.key(1, 'person:bob'),
              matching: find.widgetWithText(OutlinedButton, 'End friendship')),
          findsOneWidget);
      await p.tab(ScreenTab.agents);
      expect(
          find.descendant(
              of: p.key(1, 'agent:alf'),
              matching: find.widgetWithText(OutlinedButton, 'Retire')),
          findsOneWidget);
      p.plain();
    });

    _phoneTest(
        'Alice after retiring Alf: Alf\'s row offers no Befriend, befriend '
        'forbidding agent at both parties, while Carol\'s, a person, does',
        (tester) async {
      final p = await _Phone.open(tester, 'gai');
      await p.views([
        _v('[retired_of(alf), principal_of(alf), item($code), '
            'authored($code), friend(bob), identity]', '[alf, bob, carol]')
      ]);
      await p.menu(1, _gaiProposals, known: '[alf, bob, carol]');
      expect(p.key(1, 'person:alf'), findsOneWidget);
      expect(p.key(1, 'befriend:alf'), findsNothing);
      expect(p.key(1, 'unfriend:alf'), findsNothing);
      expect(
          find.descendant(
              of: p.key(1, 'person:alf'), matching: find.byType(ElevatedButton)),
          findsNothing);
      expect(p.key(1, 'befriend:carol'), findsOneWidget);
      expect(p.key(1, 'unfriend:bob'), findsOneWidget);
      p.plain();
    });

    _phoneTest(
        'Bob after ending his friendship with Alf: Alf\'s row, Alf recorded as '
        'Alice\'s AI agent, offers no Befriend and still names its developer, '
        'while Carol\'s, a person, offers Befriend', (tester) async {
      final p = await _Phone.open(tester, 'gai', me: 'bob');
      await p.views([
        _v('[friend(alice), item($summary), agent_of(alf, alice, '
            '$attestation), identity]', '[alf, alice, carol]')
      ]);
      await p.menu(1, _gaiProposals, known: '[alf, alice, carol]');
      expect(p.key(1, 'person:alf'), findsOneWidget);
      expect(p.key(1, 'befriend:alf'), findsNothing);
      expect(p.key(1, 'unfriend:alf'), findsNothing);
      expect(
          find.descendant(
              of: p.key(1, 'person:alf'), matching: find.text('Developer: Acme Ltd')),
          findsOneWidget);
      expect(
          find.descendant(
              of: p.key(1, 'person:alf'),
              matching: find.text('an AI agent of Alice')),
          findsOneWidget);
      expect(p.key(1, 'befriend:carol'), findsOneWidget);
      expect(p.key(1, 'unfriend:alice'), findsOneWidget);
      p.plain();
    });

    _phoneTest(
        'Bob asked to befriend Alf: while the question, which names Alf an AI '
        'agent of Alice, is open, Alf\'s row offers no Befriend, though his '
        'view does not yet record Alf; Carol\'s does; once the question is '
        'withdrawn, Alf\'s row offers it again', (tester) async {
      final p = await _Phone.open(tester, 'gai', me: 'bob');
      await p.views([_v('[friend(alice), identity]', '[alf, alice, carol]')]);
      await p.menu(1, _gaiProposals, known: '[alf, alice, carol]');
      expect(p.key(1, 'befriend:alf'), findsOneWidget);
      await p.draw('draw(5, $_gaiApprovalW, approval_w(approval(attempt(alf, '
          '3, 3, $agentBefriend, none), input([2]))))');
      await p.top();
      expect(p.key(1, 'person:alf'), findsOneWidget);
      expect(p.key(1, 'befriend:alf'), findsNothing);
      expect(p.key(1, 'befriend:carol'), findsOneWidget);
      expect(p.key(1, 'unfriend:alice'), findsOneWidget);
      p.plain();
      await p.draw('withdraw(5)');
      expect(p.key(1, 'befriend:alf'), findsOneWidget);
    });

    _phoneTest(
        'a person without an AI agent has the form; an AI agent has no Create; '
        'and with no request question open, last_state lists the AI agents, '
        'with no form and no Retire', (tester) async {
      final p = await _Phone.open(tester, 'gai', me: 'bob');
      await p.views([_v('[friend(alice), identity]', '[alf, alice]')]);
      await p.menu(1, _gaiProposals, known: '[alf, alice]');
      await p.tab(ScreenTab.agents);
      expect(find.text('Create an AI agent'), findsOneWidget);
      await p.draw('withdraw(1)');
      await p.views([
        _v('[agent, principal(alice), item($code), authored($code), '
            'identity]', '[alice]')
      ]);
      await p.menu(2, _gaiProposals, known: '[alice]');
      await p.tab(ScreenTab.agents);
      expect(find.text('Create an AI agent'), findsNothing);
      await p.draw('withdraw(2)');
      await p.views([_v(aliceState, '[alf, bob]')]);
      await p.tab(ScreenTab.agents);
      expect(p.key(0, 'agent:alf'), findsOneWidget);
      expect(find.text('Create an AI agent'), findsNothing);
      expect(find.text('Retire'), findsNothing);
    });

    _phoneTest(
        '(b) Alice asked, as Alf\'s principal, to approve Alf befriending Bob: '
        '"Alf asks to befriend Bob.", "Approve?", Yes and No, Yes granting yes',
        (tester) async {
      final p = await _Phone.open(tester, 'gai');
      await p.views([_v(aliceState, '[alf, bob]')]);
      await p.menu(1, _gaiProposals, known: '[alf, bob]');
      await p.draw('draw(5, $_gaiApprovalW, approval_w(approval(attempt(alf, '
          '3, 3, $agentBefriend, none), input([2]))))');
      await p.top();
      expect(textsOf(tester, 5), ['Alf asks to befriend Bob.', 'Approve?', 'Yes', 'No']);
      p.plain();
      await p.tap(find.byKey(answerKey(5, 'yes', [2])));
      expect(p.grants, ['input(5, [2], yes)']);
    });

    _phoneTest(
        '(c) Bob asked to befriend Alf: "Alf, an AI agent of Alice, asks to '
        'befriend you.", "Developer: Acme Ltd", "Befriend Alf?", Yes and No; then '
        'Alf\'s row and its item, each with "Developer: Acme Ltd"', (tester) async {
      final p = await _Phone.open(tester, 'gai', me: 'bob');
      await p.views([_v('[friend(alice), identity]', '[alf, alice]')]);
      await p.menu(1, _gaiProposals, known: '[alf, alice]');
      await p.draw('draw(5, $_gaiApprovalW, approval_w(approval(attempt(alf, '
          '3, 3, $agentBefriend, none), input([2]))))');
      await p.top();
      expect(textsOf(tester, 5), [
        'Alf, an AI agent of Alice, asks to befriend you.',
        'Developer: Acme Ltd',
        'Befriend Alf?',
        'Yes',
        'No',
      ]);
      // At the top of Friends, before the person's own name.
      expect(
          tester
              .getTopLeft(
                  find.text('Alf, an AI agent of Alice, asks to befriend you.'))
              .dy,
          lessThan(tester.getTopLeft(p.key(1, 'name')).dy));
      p.plain();
      await p.tap(find.byKey(answerKey(5, 'yes', [2])));
      expect(p.grants, ['input(5, [2], yes)']);
      // Once the befriending commits and Alf's item reaches him: Alf's row
      // names its developer, and so does the item in the chat with Alf,
      // under its author.
      await p.draw('withdraw(5)');
      await p.draw('withdraw(1)');
      await p.views([
        _v('[item($summary), friend(alf), agent_of(alf, alice, '
            '$attestation), friend(alice), identity]', '[alf, alice]')
      ]);
      await p.menu(2, _gaiProposals, known: '[alf, alice]');
      final row = [
        for (final t in tester.widgetList<Text>(find.descendant(
            of: p.key(2, 'person:alf'), matching: find.byType(Text))))
          t.data,
      ];
      expect(row,
          containsAllInOrder(['Alf', 'an AI agent of Alice', 'Developer: Acme Ltd']));
      p.plain();
      await p.tap(p.key(2, 'person:alf'));
      expect(p.key(2, 'chat:alf'), findsOneWidget);
      final item = p.key(2, 'item:${formatTerm(fixtureTerm(summary))}');
      expect(
          [
            for (final t in tester.widgetList<Text>(
                find.descendant(of: item, matching: find.byType(Text))))
              t.data,
          ],
          [
            'Summary of the news',
            'Author: Alf, an AI agent of Alice',
            'Developer: Acme Ltd',
            'Forward',
          ]);
      p.plain();
      // Alice is no AI agent: her row names no principal and no developer.
      await p.tap(p.key(2, 'back'));
      expect(
          find.descendant(
              of: p.key(2, 'person:alice'),
              matching: find.textContaining('Developer')),
          findsNothing);
      expect(
          find.descendant(
              of: p.key(2, 'person:alice'),
              matching: find.textContaining('an AI agent of')),
          findsNothing);
    });

    _phoneTest(
        '(e) Bob\'s Friends tab after Alf befriends him: Alf\'s row, under his '
        'name, "an AI agent of Alice" and "Developer: Acme Ltd", read from '
        'agent_of(alf, alice, y), with End friendship, proposing '
        'unfriend_p(bob, alf), and no Befriend; Alice his friend, with End '
        'friendship and neither line', (tester) async {
      // GAI #5 Cowork, Code_inbox.md, 2026-10-20 08:01 UTC; GAI's paper,
      // Section 3.1: "every friend of an AI agent sees its principal and its
      // developer".
      final p = await _Phone.open(tester, 'gai', me: 'bob');
      await p.views([
        _v('[item($summary), friend(alf), agent_of(alf, alice, '
            '$attestation), friend(alice), identity]', '[alf, alice]')
      ]);
      await p.menu(1, _gaiProposals, known: '[alf, alice]');
      await p.top();
      List<String?> row(String q) => [
            for (final t in tester.widgetList<Text>(find.descendant(
                of: p.key(1, 'person:$q'), matching: find.byType(Text))))
              t.data,
          ];
      expect(row('alf'), [
        'A',
        'Alf',
        'an AI agent of Alice',
        'Developer: Acme Ltd',
        'Summary of the news',
        'End friendship',
      ]);
      expect(p.key(1, 'befriend:alf'), findsNothing);
      expect(row('alice'), ['A', 'Alice', 'End friendship']);
      expect(p.key(1, 'befriend:alice'), findsNothing);
      p.plain();
      await p.tap(p.key(1, 'unfriend:alf'));
      expect(p.grants, ['input(1, [3], propose(unfriend_p(bob, alf)))']);
    });
  });

  // CSSN's app (programs/cssn, self.vglp), in the words Udi gave for CSSN's
  // Figure 3 (reported by CSSN #8 Cowork, Code_inbox.md, 2026-10-20 10:10
  // UTC): each person with what the state says of them, and the requests
  // composed as sentences, a person chosen into each place, and Submit.
  group('CSSN', () {
    /// The rows' names on construct [id], in order.
    List<String?> rows(WidgetTester tester, int id) => [
          for (final t in tester.widgetList<ListTile>(find.byWidgetPredicate(
              (w) =>
                  w is ListTile &&
                  w.key is ValueKey<String> &&
                  (w.key as ValueKey<String>)
                      .value
                      .startsWith('$id:social:person:'))))
            (t.title as Text).data,
        ];

    /// The sentence of the request [kind] on construct [id], each place as
    /// the person chosen into it reads.
    String sentence(WidgetTester tester, int id, String kind) {
      final f = find.byKey(socialKey(id, 'sentence:$kind'));
      var plain = tester.widget<Text>(f).textSpan!.toPlainText();
      for (final t in tester.widgetList<Text>(
          find.descendant(of: f, matching: find.byType(Text)))) {
        if (t.data != null) plain = plain.replaceFirst('￼', t.data!);
      }
      return plain;
    }

    Future<void> compose(_Phone p, int id, String kind, List<String> qs) async {
      for (var i = 0; i < qs.length; i++) {
        await p.tap(p.key(id, 'slot:$kind:$i'));
        await p.tap(p.key(id, 'pick:$kind:$i:${qs[i]}'));
      }
    }

    _phoneTest(
        '(1) Alice\'s menu: "Carol, your child", "Bob, your friend", "Dave", '
        'her child\'s row with no End friendship, and Dave\'s, whom she has '
        'not met, with Meet, as in every app that extends GKK; and '
        'the requests she composes, (2) "Ask that Carol and Dave befriend, '
        'with Bob." and Submit, proposing child_befriend(carol, dave, alice, '
        'bob), and once sent "You asked that Carol and Dave befriend, with '
        'Bob."', (tester) async {
      final p = await _Phone.open(tester, 'cssn');
      await p.views([
        _v('[friend(bob), knows(bob), parenting(carol), friend(carol), '
            'knows(carol), identity]',
            '[bob, carol, dave]')
      ]);
      // The request question shows the persons known when it was asked; the
      // tab reads them off the last view.
      await p.menu(1, _cssnProposals, known: '[bob, carol]');
      await p.top();
      expect(rows(tester, 1), ['Carol, your child', 'Bob, your friend', 'Dave']);
      expect(p.key(1, 'unfriend:carol'), findsNothing);
      expect(p.key(1, 'unfriend:bob'), findsOneWidget);
      // Dave is not a contact: befriend requires that the two know each
      // other (GSG:1), and his row offers Meet (GKK:2).
      expect(p.key(1, 'befriend:dave'), findsNothing);
      expect(p.key(1, 'meet:dave'), findsOneWidget);
      expect(p.key(1, 'compose:meet'), findsNothing);
      expect(sentence(tester, 1, 'introduce'), 'Introduce … to ….');
      expect(sentence(tester, 1, 'child-meet'), 'Ask that … and … meet.');
      expect(sentence(tester, 1, 'child-meet-2'),
          'Ask that … and … meet, with ….');
      expect(sentence(tester, 1, 'child-befriend'),
          'Ask that … and … befriend, with ….');
      expect(sentence(tester, 1, 'of-age'), 'Declare … of age.');
      expect(sentence(tester, 1, 'enter'), 'Enter … as your child.');
      expect(sentence(tester, 1, 'enter-2'), 'Enter … as your child, with ….');
      final submit = p.key(1, 'submit:child-befriend');
      expect(tester.widget<ElevatedButton>(submit).onPressed, isNull);
      // A place's picker offers the persons it takes: her children; a person
      // she knows; a friend of hers who is not her child.
      await p.tap(p.key(1, 'slot:child-befriend:2'));
      expect(
          [
            for (final c in tester.widgetList<ActionChip>(find.descendant(
                of: p.key(1, 'pick:child-befriend:2'),
                matching: find.byType(ActionChip))))
              (c.label as Text).data
          ],
          ['Bob']);
      await p.tap(p.key(1, 'slot:child-befriend:2'));
      await compose(p, 1, 'child-befriend', ['carol', 'dave', 'bob']);
      expect(sentence(tester, 1, 'child-befriend'),
          'Ask that Carol and Dave befriend, with Bob.');
      expect(p.grants, isEmpty);
      await p.tap(submit);
      expect(p.grants, [
        'input(1, [3], propose(child_befriend(carol, dave, alice, bob)))'
      ]);
      // The next request question: what she sent, and the request to
      // compose again.
      await p.draw('withdraw(1)');
      await p.menu(2, _cssnProposals, known: '[bob, carol, dave]');
      await p.reveal(p.key(2, 'sent:child-befriend:0'));
      expect(
          tester.widget<Text>(p.key(2, 'sent:child-befriend:0')).data,
          'You asked that Carol and Dave befriend, with Bob.');
      expect(sentence(tester, 2, 'child-befriend'),
          'Ask that … and … befriend, with ….');
    });

    _phoneTest(
        '(6) Bob composes "Declare Dave of age." and Submit, proposing '
        'of_age(bob, dave); once sent "Dave is of age."', (tester) async {
      final p = await _Phone.open(tester, 'cssn', me: 'bob');
      await p.views([
        _v('[friend(alice), parenting(dave), friend(dave), identity]',
            '[alice, carol, dave]')
      ]);
      await p.menu(1, _cssnProposals, known: '[alice, carol, dave]');
      expect(rows(tester, 1), ['Dave, your child', 'Alice, your friend', 'Carol']);
      await compose(p, 1, 'of-age', ['dave']);
      expect(sentence(tester, 1, 'of-age'), 'Declare Dave of age.');
      await p.tap(p.key(1, 'submit:of-age'));
      expect(p.grants, ['input(1, [3], propose(of_age(bob, dave)))']);
      // Nobody is asked: the declaration commits, and the view no longer
      // holds Dave his child; what he sent stays drawn.
      await p.draw('withdraw(1)');
      await p.views([
        _v('[friend(alice), parenting(dave), friend(dave), identity]',
            '[alice, carol, dave]'),
        _v('[friend(alice), friend(dave), identity]', '[alice, carol, dave]'),
      ]);
      await p.menu(2, _cssnProposals, known: '[alice, carol, dave]');
      await p.reveal(p.key(2, 'sent:of-age:0'));
      expect(tester.widget<Text>(p.key(2, 'sent:of-age:0')).data,
          'Dave is of age.');
      expect(p.key(2, 'sentence:of-age'), findsNothing);
      expect(rows(tester, 2), ['Alice, your friend', 'Dave, your friend', 'Carol']);
    });

    _phoneTest(
        'Alice composes "Enter Carol as your child.", a person she knows who '
        'is not her friend, proposing enter(alice, carol); once sent "You '
        'entered Carol as your child." (CSSN #8 Cowork, Code_inbox.md, '
        '2026-10-20 13:13 UTC, item 4)', (tester) async {
      final p = await _Phone.open(tester, 'cssn');
      await p.views([_v('[identity]', '[carol]')]);
      await p.menu(1, _cssnProposals, known: '[carol]');
      expect(rows(tester, 1), ['Carol']);
      // No child yet: nothing to declare of age, and no child to befriend;
      // no friend, so no other parent to enter a child with.
      expect(p.key(1, 'compose:of-age'), findsNothing);
      expect(p.key(1, 'sentence:child-befriend'), findsNothing);
      expect(p.key(1, 'compose:enter-2'), findsNothing);
      await compose(p, 1, 'enter', ['carol']);
      expect(sentence(tester, 1, 'enter'), 'Enter Carol as your child.');
      await p.tap(p.key(1, 'submit:enter'));
      expect(p.grants, ['input(1, [3], propose(enter(alice, carol)))']);
      await p.draw('withdraw(1)');
      await p.menu(2, _cssnProposals, known: '[carol]');
      await p.reveal(p.key(2, 'sent:enter:0'));
      expect(tester.widget<Text>(p.key(2, 'sent:enter:0')).data,
          'You entered Carol as your child.');
    });

    _phoneTest(
        'Bob composes "Enter Dave as your child, with Grace.", a person he '
        'knows who is not his friend and a person he has met, enter_2 '
        'requiring that the two parents know each other, proposing '
        'enter_2(bob, grace, dave); once sent "You entered Dave as your '
        'child, with Grace." (CSSN #8 Cowork, Code_inbox.md, 2026-10-20 13:13 '
        'UTC, item 4; CSSN.glp at GLP 6ff16d78)', (tester) async {
      final p = await _Phone.open(tester, 'cssn', me: 'bob');
      await p.views([_v('[knows(grace), identity]', '[dave, grace]')]);
      await p.menu(1, _cssnProposals, known: '[dave, grace]');
      expect(sentence(tester, 1, 'enter-2'), 'Enter … as your child, with ….');
      // The other parent's place offers the persons he has met.
      await p.tap(p.key(1, 'slot:enter-2:1'));
      expect(
          [
            for (final c in tester.widgetList<ActionChip>(find.descendant(
                of: p.key(1, 'pick:enter-2:1'),
                matching: find.byType(ActionChip))))
              (c.label as Text).data
          ],
          ['Grace']);
      await p.tap(p.key(1, 'slot:enter-2:1'));
      await compose(p, 1, 'enter-2', ['dave', 'grace']);
      expect(sentence(tester, 1, 'enter-2'),
          'Enter Dave as your child, with Grace.');
      await p.tap(p.key(1, 'submit:enter-2'));
      expect(p.grants, ['input(1, [3], propose(enter_2(bob, grace, dave)))']);
      await p.draw('withdraw(1)');
      await p.menu(2, _cssnProposals, known: '[dave, grace]');
      await p.reveal(p.key(2, 'sent:enter-2:0'));
      expect(tester.widget<Text>(p.key(2, 'sent:enter-2:0')).data,
          'You entered Dave as your child, with Grace.');
    });

    _phoneTest(
        '(5) Carol\'s menu: "Alice, your parent", "Dave, your friend", "Bob"; '
        'End friendship on Dave\'s row only, no Befriend, a child, and no '
        'request composed', (tester) async {
      final p = await _Phone.open(tester, 'cssn', me: 'carol');
      await p.views([
        _v('[friend(dave), friend(alice), child, parent(alice), identity]',
            '[alice, bob, dave]')
      ]);
      await p.menu(1, _cssnProposals, known: '[]');
      expect(rows(tester, 1), ['Alice, your parent', 'Dave, your friend', 'Bob']);
      expect(p.key(1, 'unfriend:alice'), findsNothing);
      expect(p.key(1, 'befriend:bob'), findsNothing);
      expect(p.key(1, 'compose:child-befriend'), findsNothing);
      expect(p.key(1, 'compose:enter'), findsNothing);
      expect(p.key(1, 'compose:enter-2'), findsNothing);
      p.plain();
      await p.tap(p.key(1, 'unfriend:dave'));
      expect(p.grants, ['input(1, [3], propose(unfriend(carol, dave)))']);
    });

    // child_join, child_join_2, child_leave and child_close "belong in
    // Groups, as join, leave and close do" (CSSN #8 Cowork, Code_inbox.md,
    // 2026-10-20 14:58 UTC).
    const kids = 'speech(utterance(kids_chat), carol)';
    const chess = 'speech(utterance(chess), dave)';
    final childGroupParts = [
      'child_join(alice, dave, bob, $_friends)',
      'child_join_2(carol, dave, alice, bob, $kids)',
      'child_leave(carol, dave, bob, $kids)',
      'child_close(dave, bob, [carol], $chess)',
    ];

    test('the classes of a child\'s membership are classes of a group: a '
        'notice of each concerns Groups', () {
      expect(
          groupClasses.containsAll(
              ['child_join', 'child_join_2', 'child_leave', 'child_close']),
          isTrue);
      for (final part in childGroupParts) {
        for (final d in ['declined', 'unable([], [])']) {
          expect(
              SocialNotice.of(fixtureTerm('refused($part, bob, $d)'))!
                  .concernsGroup,
              isTrue,
              reason: part);
        }
      }
    });

    _phoneTest(
        'Bob, Dave\'s parent: an approval of child_join, child_join_2, '
        'child_leave and child_close each at the top of Groups, badging it, '
        'and none in Friends', (tester) async {
      final p = await _Phone.open(tester, 'cssn', me: 'bob');
      await p.views([
        _v('[friend(alice), parenting(dave), friend(dave), identity]',
            '[alice, carol, dave]')
      ]);
      await p.menu(1, _cssnProposals, known: '[alice, carol, dave]');
      final initiators = ['alice', 'carol', 'dave', 'dave'];
      for (var i = 0; i < childGroupParts.length; i++) {
        await p.draw('draw(${i + 2}, $_cssnGroupApprovalW, '
            'approval_w(approval(attempt(${initiators[i]}, ${i + 4}, '
            '${i + 4}, ${childGroupParts[i]}, none), input([2]))))');
      }
      // In Friends, none; the four badge Groups, and none badges Friends.
      for (var id = 2; id <= 5; id++) {
        expect(find.byKey(constructKey(id)), findsNothing, reason: '$id');
      }
      expect(p.key(1, 'person:alice'), findsOneWidget);
      expect(
          find.descendant(
              of: find.byKey(tabKey(ScreenTab.groups)),
              matching: find.text('4')),
          findsOneWidget);
      expect(
          find.descendant(
              of: find.byKey(tabKey(ScreenTab.friends)),
              matching: find.byType(Badge)),
          findsOneWidget);
      expect(
          tester
              .widget<Badge>(find.descendant(
                  of: find.byKey(tabKey(ScreenTab.friends)),
                  matching: find.byType(Badge)))
              .isLabelVisible,
          isFalse);
      // In Groups, each at the top, above the request question's view.
      await p.tab(ScreenTab.groups);
      await p.top();
      Finder built(int id) => find.byKey(constructKey(id), skipOffstage: false);
      for (var id = 2; id <= 5; id++) {
        expect(built(id), findsOneWidget, reason: '$id');
        expect(tester.getTopLeft(built(id)).dy,
            lessThan(tester.getTopLeft(built(1)).dy),
            reason: '$id');
      }
    });

    _phoneTest(
        'Dave: a notice of child_join, child_join_2, child_leave and '
        'child_close each in Groups, and none in Friends', (tester) async {
      final p = await _Phone.open(tester, 'cssn', me: 'dave');
      final state = _v('[friend(bob), child, parent(bob), identity]',
          '[alice, bob, carol]');
      final notices = [
        for (final part in childGroupParts) 'refused($part, bob, declined)',
      ];
      await p.views([state, ...notices]);
      await p.menu(1, _cssnProposals, known: '[alice, bob, carol]');
      final said = [
        for (final n in notices)
          SocialNotice.of(fixtureTerm(n))!.sentence(const GAtom('dave'))
      ];
      expect(said.toSet(), hasLength(4));
      for (final s in said) {
        expect(find.text(s), findsNothing, reason: s);
      }
      expect(p.key(1, 'person:bob'), findsOneWidget);
      await p.tab(ScreenTab.groups);
      for (final s in said) {
        await p.reveal(find.text(s));
        expect(find.text(s), findsOneWidget, reason: s);
      }
    });
    // The words of CSSN's acts (CSSN #9 Cowork, Code_inbox.md, 2026-10-20
    // 19:44 UTC, item 4), each a request composed and Submit.
    List<String?> picks(WidgetTester tester, _Phone p, int id, String at) => [
          for (final c in tester.widgetList<ActionChip>(find.descendant(
              of: p.key(id, 'pick:$at'), matching: find.byType(ActionChip))))
            (c.label as Text).data
        ];

    _phoneTest(
        'In CSSN\'s app as in every app that extends GKK (GSC #5 Cowork, '
        'Code_inbox.md, 2026-10-20 20:21 UTC): Meet on the row of a person '
        'Alice knows whom she has not met, proposing meet(alice, bob); and '
        '"Introduce Carol to Bob.", two persons she has met, and Introduce, '
        'proposing introduce(alice, bob, carol)', (tester) async {
      final p = await _Phone.open(tester, 'cssn');
      await p.views([
        _v('[parenting(carol), friend(carol), knows(carol), identity]',
            '[bob, carol]')
      ]);
      await p.menu(1, _cssnProposals, known: '[bob, carol]');
      // Carol is a contact; Bob is not, and only his row offers Meet.
      expect(p.key(1, 'meet:carol'), findsNothing);
      expect(p.key(1, 'compose:meet'), findsNothing);
      // Fewer than two contacts: nothing to introduce.
      expect(p.key(1, 'sentence:introduce'), findsNothing);
      await p.tap(p.key(1, 'meet:bob'));
      expect(p.grants, ['input(1, [3], propose(meet(alice, bob)))']);
      await p.draw('withdraw(1)');
      await p.views([
        _v('[parenting(carol), friend(carol), knows(carol), identity]',
            '[bob, carol]'),
        _v('[knows(bob), parenting(carol), friend(carol), knows(carol), '
            'identity]', '[bob, carol]'),
      ]);
      await p.menu(2, _cssnProposals, known: '[bob, carol]');
      // Bob is now a contact: Befriend on his row, and no Meet.
      expect(p.key(2, 'befriend:bob'), findsOneWidget);
      expect(p.key(2, 'meet:bob'), findsNothing);
      await compose(p, 2, 'introduce', ['carol', 'bob']);
      expect(sentence(tester, 2, 'introduce'), 'Introduce Carol to Bob.');
      expect(
          find.descendant(
              of: p.key(2, 'submit:introduce'),
              matching: find.text('Introduce')),
          findsOneWidget);
      await p.tap(p.key(2, 'submit:introduce'));
      expect(p.grants.last,
          'input(2, [3], propose(introduce(alice, bob, carol)))');
    });

    _phoneTest(
        'Alice composes "Ask that Carol and Frank meet.", her child and a '
        'person she has met, proposing child_meet(carol, frank, alice), and '
        '"Ask that Carol and Dave meet, with Bob.", proposing '
        'child_meet_2(carol, dave, alice, bob); once sent "You asked that '
        'Carol and Frank meet." and "You asked that Carol and Dave meet, with '
        'Bob."', (tester) async {
      final p = await _Phone.open(tester, 'cssn');
      await p.views([
        _v('[friend(bob), knows(bob), knows(frank), parenting(carol), '
            'friend(carol), knows(carol), identity]',
            '[bob, carol, dave, frank]')
      ]);
      await p.menu(1, _cssnProposals, known: '[bob, carol, dave, frank]');
      await p.tap(p.key(1, 'slot:child-meet:0'));
      expect(picks(tester, p, 1, 'child-meet:0'), ['Carol']);
      await p.tap(p.key(1, 'pick:child-meet:0:carol'));
      // The other place: the persons she has met, her child not among them.
      await p.tap(p.key(1, 'slot:child-meet:1'));
      expect(picks(tester, p, 1, 'child-meet:1'), ['Bob', 'Frank']);
      await p.tap(p.key(1, 'pick:child-meet:1:frank'));
      expect(sentence(tester, 1, 'child-meet'), 'Ask that Carol and Frank meet.');
      await p.tap(p.key(1, 'submit:child-meet'));
      expect(p.grants,
          ['input(1, [3], propose(child_meet(carol, frank, alice)))']);
      await p.draw('withdraw(1)');
      await p.menu(2, _cssnProposals, known: '[bob, carol, dave, frank]');
      await p.reveal(p.key(2, 'sent:child-meet:0'));
      expect(tester.widget<Text>(p.key(2, 'sent:child-meet:0')).data,
          'You asked that Carol and Frank meet.');
      // The other parent's place: the persons she has met who are neither
      // her child nor her parent.
      await p.tap(p.key(2, 'slot:child-meet-2:2'));
      expect(picks(tester, p, 2, 'child-meet-2:2'), ['Bob', 'Frank']);
      await p.tap(p.key(2, 'slot:child-meet-2:2'));
      await compose(p, 2, 'child-meet-2', ['carol', 'dave', 'bob']);
      expect(sentence(tester, 2, 'child-meet-2'),
          'Ask that Carol and Dave meet, with Bob.');
      await p.tap(p.key(2, 'submit:child-meet-2'));
      expect(p.grants.last,
          'input(2, [3], propose(child_meet_2(carol, dave, alice, bob)))');
      await p.draw('withdraw(2)');
      await p.menu(3, _cssnProposals, known: '[bob, carol, dave, frank]');
      await p.reveal(p.key(3, 'sent:child-meet-2:0'));
      expect(tester.widget<Text>(p.key(3, 'sent:child-meet-2:0')).data,
          'You asked that Carol and Dave meet, with Bob.');
    });

    _phoneTest(
        'A child composes no meeting or meeting of a child, and composes '
        'an introduction; her rows offer no Meet, and her screen no field '
        'Add a person by key (CSSN, Section 4.6.2)', (tester) async {
      final p = await _Phone.open(tester, 'cssn', me: 'carol');
      await p.views([
        _v('[friend(alice), knows(alice), knows(dave), child, parent(alice), '
            'identity]', '[alice, bob, dave]')
      ]);
      await p.menu(1, _cssnProposals, known: '[alice, bob, dave]');
      expect(p.key(1, 'meet:bob'), findsNothing);
      expect(p.key(1, 'befriend:bob'), findsNothing);
      for (final k in ['meet', 'child-meet', 'child-meet-2']) {
        expect(p.key(1, 'compose:$k'), findsNothing, reason: k);
      }
      expect(p.key(1, 'compose:introduce'), findsOneWidget);
      expect(p.key(1, 'add-field'), findsNothing);
      expect(p.key(1, 'add'), findsNothing);
    });

    _phoneTest(
        'A child\'s phone at its setup, before the entering, its state '
        'holding no child, has the field Add a person by key, by which its '
        'agent discovers its parents\' (CSSN, Appendix C.4)', (tester) async {
      final p = await _Phone.open(tester, 'cssn', me: 'carol');
      await p.views([_v('[]', '[]')]);
      await p.menu(1, _cssnProposals, known: '[]');
      expect(p.key(1, 'add-field'), findsOneWidget);
      await p.type(p.key(1, 'add-field'), 'alice');
      await p.tap(p.key(1, 'add'));
      expect(p.grants, ['input(1, [3], known(alice))']);
    });

    const study = 'speech(utterance(study), frank)';

    _phoneTest(
        'In Groups, Frank, an adult leading study, composes "Invite Carol to '
        'study, with Alice.", a friend not in it and the child\'s parent, '
        'proposing child_join(frank, carol, alice, study), beside Add member; '
        'once sent "You invited Carol to study, with Alice."',
        (tester) async {
      final p = await _Phone.open(tester, 'cssn', me: 'frank');
      await p.views([
        _v('[friend(carol), knows(carol), knows(alice), '
            'member(dot(community(frank), $study)), listed(frank, $study), '
            'identity]', '[alice, carol]')
      ]);
      await p.menu(1, _cssnProposals, known: '[alice, carol]');
      await p.tab(ScreenTab.groups);
      const k = 'child-join:frank:$study';
      expect(p.key(1, 'add-member:frank:$study'), findsOneWidget);
      await compose(p, 1, k, ['carol', 'alice']);
      expect(sentence(tester, 1, k), 'Invite Carol to study, with Alice.');
      await p.tap(p.key(1, 'submit:$k'));
      expect(p.grants,
          ['input(1, [3], propose(child_join(frank, carol, alice, $study)))']);
      await p.draw('withdraw(1)');
      await p.menu(2, _cssnProposals, known: '[alice, carol]');
      await p.reveal(p.key(2, 'sent:$k:0'));
      expect(tester.widget<Text>(p.key(2, 'sent:$k:0')).data,
          'You invited Carol to study, with Alice.');
    });

    _phoneTest(
        'In Groups, Carol, a child leading kids_chat, has no Add member and '
        'composes "Invite Dave to kids_chat, with Alice and Bob.", a friend '
        'not in it, her parent and the other child\'s, proposing '
        'child_join_2(carol, dave, alice, bob, kids_chat)', (tester) async {
      final p = await _Phone.open(tester, 'cssn', me: 'carol');
      await p.views([
        _v('[friend(alice), knows(alice), friend(dave), knows(dave), child, '
            'parent(alice), member(dot(community(carol), $kids)), '
            'listed(carol, $kids), identity]', '[alice, bob, dave]')
      ]);
      await p.menu(1, _cssnProposals, known: '[alice, bob, dave]');
      await p.tab(ScreenTab.groups);
      const k = 'child-join-2:carol:$kids';
      expect(p.key(1, 'add-member:carol:$kids'), findsNothing);
      await p.tap(p.key(1, 'slot:$k:1'));
      expect(picks(tester, p, 1, '$k:1'), ['Alice']);
      await p.tap(p.key(1, 'slot:$k:1'));
      await compose(p, 1, k, ['dave', 'alice', 'bob']);
      expect(sentence(tester, 1, k),
          'Invite Dave to kids_chat, with Alice and Bob.');
      await p.tap(p.key(1, 'submit:$k'));
      expect(p.grants, [
        'input(1, [3], propose(child_join_2(carol, dave, alice, bob, $kids)))'
      ]);
    });

    _phoneTest(
        'In Groups, Alice, a parent, having approved Carol\'s joining of '
        'study and Dave\'s of kids_chat, which Carol leads, composes "End '
        'Carol\'s membership of study.", proposing child_leave(frank, carol, '
        'alice, study), and "Close Carol\'s group kids_chat.", proposing '
        'child_close(carol, alice, kids_chat); nobody is asked',
        (tester) async {
      final p = await _Phone.open(tester, 'cssn');
      await p.views([
        _v('[friend(bob), knows(bob), parenting(carol), friend(carol), '
            'knows(carol), identity]', '[bob, carol, dave, frank]')
      ]);
      await p.menu(1, _cssnProposals, known: '[bob, carol, dave, frank]');
      await p.tab(ScreenTab.groups);
      // Nothing approved: no group of hers to name.
      expect(p.key(1, 'compose:child-leave'), findsNothing);
      expect(p.key(1, 'compose:child-close'), findsNothing);
      await p.draw('draw(2, $_cssnGroupApprovalW, approval_w(approval('
          'attempt(carol, 4, 4, child_join_2(carol, dave, alice, bob, $kids), '
          'none), input([2]))))');
      await p.draw('draw(3, $_cssnGroupApprovalW, approval_w(approval('
          'attempt(frank, 5, 5, child_join(frank, carol, alice, $study), '
          'none), input([2]))))');
      for (final id in [2, 3]) {
        await p.tap(find.descendant(
            of: find.byKey(constructKey(id)),
            matching: find.widgetWithText(ElevatedButton, 'Yes')));
      }
      expect(p.grants, ['input(2, [2], yes)', 'input(3, [2], yes)']);
      await p.draw('withdraw(2)');
      await p.draw('withdraw(3)');
      await p.top();
      await p.tap(p.key(1, 'slot:child-leave:0'));
      expect(picks(tester, p, 1, 'child-leave:0'), ['Carol']);
      await p.tap(p.key(1, 'pick:child-leave:0:carol'));
      await p.tap(p.key(1, 'slot:child-leave:1'));
      expect(picks(tester, p, 1, 'child-leave:1'), ['study']);
      await p.tap(p.key(1, 'pick:child-leave:1:group(frank, $study)'));
      expect(sentence(tester, 1, 'child-leave'),
          "End Carol's membership of study.");
      await p.tap(p.key(1, 'submit:child-leave'));
      expect(p.grants.last,
          'input(1, [3], propose(child_leave(frank, carol, alice, $study)))');
      await p.draw('withdraw(1)');
      await p.menu(4, _cssnProposals, known: '[bob, carol, dave, frank]');
      await p.reveal(p.key(4, 'sent:child-leave:0'));
      expect(tester.widget<Text>(p.key(4, 'sent:child-leave:0')).data,
          "You ended Carol's membership of study.");
      await compose(p, 4, 'child-close', ['carol', 'group(carol, $kids)']);
      expect(sentence(tester, 4, 'child-close'),
          "Close Carol's group kids_chat.");
      await p.tap(p.key(4, 'submit:child-close'));
      expect(p.grants.last,
          'input(4, [3], propose(child_close(carol, alice, $kids)))');
      await p.draw('withdraw(4)');
      await p.menu(5, _cssnProposals, known: '[bob, carol, dave, frank]');
      await p.reveal(p.key(5, 'sent:child-close:0'));
      expect(tester.widget<Text>(p.key(5, 'sent:child-close:0')).data,
          "You closed Carol's group kids_chat.");
    });
  });
}
