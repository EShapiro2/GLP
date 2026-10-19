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
// (vGLP e924f73, Section 6.2).
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

/// GSNP's: GSG's, sign and forward, and GSN's groups.
const _gsnpProposals = [
  ..._gsgProposals,
  "form(sign, [$_p, $_s])",
  "form(forward, [$_p, $_p, $_s])",
  "form(create, [$_p, $_s])",
  "form(join, [$_p, $_p, $_s])",
  "form(leave, [$_p, $_p, $_s])",
  "form(close, [$_p, $_s])",
  "form(post_own, [$_p, $_s, $_s])",
  "form(post, [$_p, $_p, $_s, $_s])",
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

/// The widget of the approval question, `named(approval_sentence, D)`.
const _approvalW = "named(approval_sentence, form(approval, "
    "[field('Attempt', form(attempt, [field('Peer', shown), "
    "field('Integer', shown), field('Integer', shown), field('Class', "
    "menu([form(befriend, [field('Peer', shown), field('Peer', shown)]), "
    "form(join, [field('Peer', shown), field('Peer', shown), "
    "field('Speech', $_speechShown)])]))])), "
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

/// Alice's GSN state: her identity; Bob and Carol her friends; a message
/// from Bob; the group friends, which she leads, Bob listed in it, Bob's
/// message delivered in it; and the group hikers, which Bob leads, his
/// message delivered in it.
final _gsnState = '[identity, friend(bob), friend(carol), '
    'received(${_sp('Lunch at noon?', 'bob')}, bob), '
    'member(alice, $_friends), listed(alice, $_friends), '
    'listed(bob, $_friends), '
    'received_in(${_sp('See you all on Sunday', 'bob')}, alice, $_friends), '
    'member(bob, $_hikers), '
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
/// returns.
class _Phone {
  final WidgetTester tester;
  final String app;
  final List<String> grants = [];
  final GlobalKey shot = GlobalKey();
  late final UiRuntime r;

  _Phone(this.tester, this.app) {
    r = UiRuntime(
      manifest: Manifest(
        title: app,
        panels: [Panel(id: app, name: app.toUpperCase(), constructs: true)],
        activity: const [],
      ),
      onSend: (t) => grants.add(formatTerm(t)),
    );
  }

  static Future<_Phone> open(WidgetTester tester, String app) async {
    await _loadFonts();
    // Shadows drawn as shadows, not as the test's solid outlines; restored
    // at the end of the test's body by [_phoneTest].
    debugDisableShadows = false;
    tester.view.physicalSize = _phone;
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);
    final p = _Phone(tester, app);
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
          child: AgentSurface(agentId: 'alice', runtime: p.r, muteNotices: true),
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

  /// Draw the request question [id] of Alice, showing [known] as the
  /// persons known when it was asked, whose type offers the proposals [ps].
  Future<void> menu(int id, List<String> ps, {String known = '[]'}) => draw(
      'draw($id, named(social_menu, ${_menuD(ps)}), '
      'menu_w(menu(alice, $known, input([3]))))');

  Finder key(int id, String what) => find.byKey(socialKey(id, what));

  /// The texts on the screen, in order.
  List<String> texts() => [
        for (final t in tester.widgetList<Text>(find.byType(Text)))
          if (t.data != null) t.data!,
      ];

  Future<void> tap(Finder f) async {
    await reveal(f);
    expect(f, findsOneWidget);
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
      final gs = s.groups;
      expect(gs.map((g) => g.title), ['friends', 'hikers']);
      expect(gs[0].members.map(formatTerm), ['alice', 'bob']);
      expect(gs[0].messages.map((x) => SpeechAct.of(x)!.text),
          ['See you all on Sunday']);
      expect(gs[1].members, isEmpty);
      expect(formatTerm(gs[1].leader), 'bob');
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
          '7, 7, befriend(carol, alice)), input([2]))))');
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
          '4, 4, befriend(dan, alice)), input([2]))))');
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
          '5, 5, join(dan, alice, speech(utterance(chess), dan))), '
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
        'Friends as GSG\'s, its items under Messages, and in a group no text '
        'and Send, a post carrying an item the person signed', (tester) async {
      final p = await _Phone.open(tester, 'gsnp');
      await p.views([
        _v(
            '[identity, friend(bob), '
            'item(speech(${_sp('Flood on Main Street', 'dan')}, bob)), '
            'member(alice, $_friends), listed(alice, $_friends)]',
            '[bob]')
      ]);
      await p.menu(1, _gsnpProposals, known: '[bob]');
      expect(p.key(1, 'unfriend:bob'), findsOneWidget);
      expect(p.key(1, 'items'), findsOneWidget);
      expect(find.text('Flood on Main Street'), findsOneWidget);
      expect(find.text('Dan, via Bob'), findsOneWidget);
      // No chat: GSNP has no message(Me, Q, X).
      await p.tap(p.key(1, 'person:bob'));
      expect(p.key(1, 'chat:bob'), findsNothing);
      await p.tab(ScreenTab.groups);
      final f = groupKey(const GAtom('alice'), fixtureTerm(_friends));
      expect(p.key(1, 'group:$f'), findsOneWidget);
      expect(p.key(1, 'post-field:$f'), findsNothing);
      expect(p.key(1, 'close:$f'), findsOneWidget);
    });
  });
}
