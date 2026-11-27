/// What the web demo of the Grassroots UI does that the described screens now do too (lib/ui_runtime/described.dart),
/// read only from the compiled descriptions `programs/gce/ui/*.ui.json` and the platform's request type: help,
/// from the contract alone; how an empty list's items come; an IMPORTANT action not offered, with what is in its
/// way and the first step to it; THEN IT; a field's label; an act that only removes facts, taken on a second tap;
/// and the routing rule.  Real GLP agents, GSG's (Friendship) and GSN's (Groups), on two phones, as
/// `described_constructs_test.dart` runs them; and the screens on phones and tablets of several sizes.  Where
/// `DESCRIBED_SHOTS` names a directory, a PNG of the phones is written after each step.
library;

import 'dart:convert';
import 'dart:io';
import 'dart:ui' as dui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart' show runtimeTermOf, screenTermOf;
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/described.dart';
import 'package:glp_multiagent/ui_runtime/look.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/external_io.dart' show InputInjector;
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' as rt;

import 'programs_dir.dart';

final String? _shots = Platform.environment['DESCRIBED_SHOTS'];

Future<void> _loadFonts() async {
  final root = Platform.environment['FLUTTER_ROOT'] ?? '/opt/homebrew/share/flutter';
  final dir = '$root/bin/cache/artifacts/material_fonts';
  Future<ByteData> bytes(String f) async => ByteData.view(File('$dir/$f').readAsBytesSync().buffer);
  final roboto = FontLoader('Roboto');
  for (final f in ['Roboto-Regular.ttf', 'Roboto-Medium.ttf', 'Roboto-Bold.ttf']) {
    roboto.addFont(bytes(f));
  }
  await roboto.load();
  await (FontLoader('MaterialIcons')..addFont(bytes('MaterialIcons-Regular.otf'))).load();
}

class _Person {
  final String name;
  final InputInjector grants;
  final HeapCell draws;
  final GlobalKey shot = GlobalKey();
  final List<GTerm> unsent = [];
  final ValueNotifier<Map<String, bool>> rules = ValueNotifier({});
  int fed = 0;
  late final UiRuntime ui = UiRuntime(
      manifest: const Manifest(title: 'app', panels: [Panel(id: 'app', name: 'app', constructs: true)], activity: []),
      onSend: unsent.add);
  _Person(this.name, this.grants, this.draws);

  SocialState get state => SocialState.read(ui.bridge.construct(0)?.view).$2!;
  List<String> get atoms => [for (final a in state.atoms) formatTerm(a)];
  List<OpenConstruct> get approvals => [
        for (final c in ui.bridge.constructs)
          if (c.id != 0 && ctorArgs(c.view).$1 == 'approval') c
      ];
}

/// Two persons, Alice and Bob, each running [app]'s mini-app with a conversation to the other, each phone [size],
/// drawn from [ui].
class _Play {
  final WidgetTester tester;
  final String tag;
  late final List<_Person> persons;
  late final GlpEngine engine;
  late final dynamic play;
  final GlobalKey all = GlobalKey();
  int shot = 0;
  _Play(this.tester, this.tag);

  _Person get alice => persons[0];
  _Person get bob => persons[1];

  Future<void> start(String app, UiDescription ui, Size size) async {
    final programs = programsDir();
    engine = GlpEngine(rootSelfGlpPath: '$programs/self.glp');
    expect(engine.loadProgram('$programs/apps/$app'), isTrue);
    play = engine.postGoal(
        'miniapp(alice, ch(G0?, D0), ch([conversation(bob, ch(C_bob_alice?, C_alice_bob))], U0)), '
        'miniapp(bob, ch(G1?, D1), ch([conversation(alice, ch(C_alice_bob?, C_bob_alice))], U1))',
        inputs: ['G0', 'G1']);
    persons = [
      _Person('alice', play.inputs['G0']!, play.variables['D0']!),
      _Person('bob', play.inputs['G1']!, play.variables['D1']!),
    ];
    tester.view.physicalSize = Size(2 * size.width, size.height);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);
    drain();
    feed();
    await tester.pumpWidget(MaterialApp(
      debugShowCheckedModeBanner: false,
      theme: _shots == null ? null : Looks.readSync('$programs/self.glp').of(app).theme(fontFamily: 'Roboto'),
      home: Scaffold(
        body: RepaintBoundary(
          key: all,
          child: Row(children: [
            for (final p in persons)
              DescribedUi(
                ui: ui,
                rules: p.rules,
                where: const ['Grassroots', 'App'],
                child: RepaintBoundary(
                  key: p.shot,
                  child: SizedBox.fromSize(size: size, child: AgentSurface(agentId: p.name, runtime: p.ui, muteNotices: true)),
                ),
              ),
          ]),
        ),
      ),
    ));
    await tester.pumpAndSettle();
  }

  void drain() {
    final r = play.scheduler.drainToQuiescence(maxCycles: 20000000);
    expect(r.status, isNot(ExecutionStatus.failed));
  }

  rt.Term deep(rt.Term t) {
    final d = engine.runtime.heap.dereference(t);
    return d is rt.StructTerm ? rt.StructTerm(d.functor, [for (final a in d.args) deep(a)]) : d;
  }

  void feed() {
    for (final p in persons) {
      var cell = engine.runtime.heap.dereference(rt.VarRef(p.draws));
      var i = 0;
      while (cell is rt.StructTerm && cell.functor == '.' && cell.args.length == 2) {
        if (i++ >= p.fed) {
          p.ui.handle(screenTermOf(deep(cell.args[0])));
          p.fed++;
        }
        cell = engine.runtime.heap.dereference(cell.args[1]);
      }
    }
  }

  Future<void> settle() async {
    for (var more = true; more;) {
      more = false;
      for (final p in persons) {
        while (p.unsent.isNotEmpty) {
          more = true;
          p.grants.inject(runtimeTermOf(p.unsent.removeAt(0))).forEach(engine.runtime.gq.enqueue);
          drain();
        }
      }
      feed();
      await tester.pump();
    }
    await tester.pumpAndSettle();
  }

  Finder on(_Person p, Finder f) => find.descendant(of: find.byKey(p.shot), matching: f);

  Future<void> tap(_Person p, Finder f) async {
    expect(f, findsOneWidget, reason: '${p.name}: $f');
    await tester.ensureVisible(f);
    await tester.pumpAndSettle();
    await tester.tap(f);
    await tester.pumpAndSettle();
    await settle();
  }

  Future<void> yes(_Person p) async {
    expect(p.approvals, hasLength(1), reason: '${p.name}: one question');
    await tap(
        p,
        on(
            p,
            find.descendant(
                of: find.byKey(constructKey(p.approvals.single.id)),
                matching: find.byWidgetPredicate((w) => w is ElevatedButton && w.child is Text && (w.child! as Text).data == 'yes'))));
  }

  Future<void> snap(String what) async {
    if (_shots == null) return;
    FocusManager.instance.primaryFocus?.unfocus();
    await tester.pumpAndSettle();
    final b = all.currentContext!.findRenderObject()! as RenderRepaintBoundary;
    final png = await tester.runAsync(() async {
      final image = await b.toImage(pixelRatio: 2);
      final data = await image.toByteData(format: dui.ImageByteFormat.png);
      image.dispose();
      return data!.buffer.asUint8List();
    });
    Directory(_shots!).createSync(recursive: true);
    File('${_shots!}/$tag-${(++shot).toString().padLeft(2, '0')}-$what.png').writeAsBytesSync(png!);
  }

  /// The text of every Text widget on [p]'s phone, joined.
  String texts(_Person p) => [
        for (final e in on(p, find.byType(Text)).evaluate())
          if ((e.widget as Text).data case final d?) d else (e.widget as Text).textSpan?.toPlainText() ?? ''
      ].join('\n');
}

String _programs() => programsDir();
UiDescription _read(String n) => UiDescription.parse(File('${_programs()}/gce/ui/$n.ui.json').readAsStringSync());

void main() {
  setUpAll(() async {
    if (_shots != null) await _loadFonts();
  });

  group('the compiled description', () {
    test('effects and addedBy are read; an act that only removes facts ends something', () {
      final g = _read('Groups');
      expect(g.effects['create']!.adds, containsAll(['leader', 'member', 'listed']));
      expect(g.effects['unfriend']!.removes, contains('friend'));
      expect([for (final a in g.addedBy['member']!) (a.schema, a.mine)], [('create', true), ('join', false)]);
      expect(g.ends('unfriend'), isTrue);
      expect(g.ends('leave'), isTrue);
      expect(g.ends('create'), isFalse);
      expect(g.ends('message'), isFalse, reason: 'it neither adds nor removes: nothing ends');
    });

    test('IMPORTANT and THEN IT are read off the components', () {
      final f = _read('Friendship'), g = _read('Groups');
      final chain = f.screens['Home']!.components.firstWhere((c) => c.kind == 'chain');
      expect(chain.important, isTrue);
      final drawn = UiDescription.drawn(f.screens['Home']!).map((x) => x.$1).where((c) => c.kind == 'action').toList();
      expect([for (final c in drawn) UiDescription.keyOf(c)], ['act:establish', 'chain:Make friends', 'act:acquaint', 'act:befriend']);
      expect(drawn[1].important, isTrue, reason: 'a chain drawn as its first step keeps its look');
      final create = g.screens['Chats']!.components.firstWhere((c) => c.kind == 'action' && c.schema == 'create');
      expect(create.then, 'IT');
      expect(create.thenIt, {'screen': 'Group', 'pred': 'member', 'of': 'fact', 'from': 'Chats'});
      final post = g.screens['Group']!.components.firstWhere((c) => c.kind == 'action' && c.schema == 'post');
      expect(post.important, isTrue);
    });

    test('an empty list says how its items come, the act of someone else marked', () {
      final g = _read('Groups');
      final core = DescribedCore(g, const SocialState(GAtom('alice'), [], [], []), null);
      final groups = g.screens['Chats']!.components.firstWhere((c) => c.kind == 'list' && c.j['title'] == 'Groups');
      // Join's label is kept to the screens of a group and of a person: off them it is Join.
      expect(core.fillHint(groups, screen: 'Chats'), ' They come with “New group”, “Join” (by someone else).');
      final contacts = g.screens['Chats']!.components.firstWhere((c) => c.kind == 'list' && c.j['title'] == 'Contacts');
      expect(core.fillHint(contacts, screen: 'Chats'), '', reason: 'everyone: no one predicate');
      expect(core.actLabel('join', screen: 'Group'), 'Add to group');
      expect(core.actLabel('post_own'), 'Post');
    });

    test('a step on the page of someone the act names: Trust on the developer\'s screen', () {
      final app = _read('App');
      final core = DescribedCore(app, const SocialState(GAtom('alice'), [], [], []), null);
      final here = core.placings('trust', 'Program');
      expect(here, isEmpty, reason: 'not on the program\'s screen, nor on Home or More');
      final there = core.placings('trust', 'Program', [const GAtom('carol')]);
      expect([for (final p in there) (p.at, formatTerm(p.about!))], contains(('Person', 'carol')));
    });

    test('a fact in words, a place not chosen read someone', () {
      bool me(GTerm q) => formatTerm(q) == 'alice';
      expect(SocialNotice.factWords(const GAtom('identity'), const GAtom('alice'), true, me), 'you have an identity');
      expect(SocialNotice.factWords(const GStruct('friend', [SocialNotice.someone]), const GAtom('alice'), true, me),
          'you and someone are friends');
      expect(SocialNotice.factWords(const GStruct('knows', [GAtom('bob')]), const GAtom('alice'), true, me), 'you know Bob');
    });
  });

  testWidgets('Friendship: help, the important Make friends, the routing rule, and an ending act on a second tap', (tester) async {
    final t = _Play(tester, 'friendship');
    await t.start('gsg', _read('Friendship'), const Size(390, 844));
    final alice = t.alice, bob = t.bob;
    await t.snap('start');

    // Make friends is important and not offered: what is in its way, and the first step, Set up my identity, whose
    // own button is the hint's.
    final hint = t.on(alice, find.byKey(describedKey('important:chain:Make friends')));
    expect(hint, findsOneWidget);
    expect(t.texts(alice), contains('To Make friends: it needs that you have an identity.'));
    expect(t.texts(alice), contains('then Make friends'));
    expect(t.on(alice, find.text('Set up my identity')), findsOneWidget, reason: 'one button, the hint\'s');
    expect(find.descendant(of: hint, matching: find.byKey(describedKey('act:establish'))), findsOneWidget);
    expect(t.on(alice, find.text('Nothing here yet. Start with “Set up my identity”.')), findsOneWidget);

    // Her code, for another phone to scan: the start screen offers it, the contacts being scanned.
    await t.tap(alice, t.on(alice, find.byKey(describedKey('mycode'))));
    expect(find.byKey(describedKey('mycode:sheet')), findsOneWidget);
    expect(find.text('Another phone scans it to give your key.'), findsOneWidget);
    await t.snap('mycode');
    await tester.tapAt(const Offset(10, 10));
    await tester.pumpAndSettle();

    // Help: each act the UI structure places, and whether she can take it now.
    await t.tap(alice, t.on(alice, find.byKey(describedKey('help'))));
    expect(find.byKey(describedKey('help:sheet')), findsOneWidget);
    String row(String schema) => [
          for (final e in find.descendant(of: find.byKey(describedKey('help:$schema')), matching: find.byType(Text)).evaluate())
            (e.widget as Text).data ?? ''
        ].join(' | ');
    expect(row('establish'), contains('You can do this now. On Home.'));
    expect(row('acquaint'), contains('Not now: it needs that you have an identity.'));
    expect(row('unfriend'), contains('On Person.'));
    expect(row('unfriend'), startsWith('Unfriend'));
    await t.snap('help');
    await tester.tapAt(const Offset(10, 10));
    await tester.pumpAndSettle();

    // The first step, from the hint; then the routing rule is asked once, Establish being what every other act needs.
    await t.tap(alice, t.on(alice, find.byKey(describedKey('act:establish'))));
    expect(alice.atoms, contains('identity'));
    expect(t.on(alice, find.byKey(describedKey('important:chain:Make friends'))), findsNothing);
    expect(t.on(alice, find.byKey(describedKey('chain:Make friends'))), findsOneWidget);
    expect(t.on(alice, find.byKey(describedKey('rule'))), findsOneWidget);
    expect(t.texts(alice), contains('Do “Set up my identity” for you in each app you open?'));
    await t.snap('rule');
    await t.tap(alice, t.on(alice, find.byKey(describedKey('rule:yes'))));
    expect(alice.rules.value, {'establish': true});
    expect(t.on(alice, find.byKey(describedKey('rule'))), findsNothing);
    // Bob says no.
    await t.tap(bob, t.on(bob, find.byKey(describedKey('act:establish'))));
    await t.tap(bob, t.on(bob, find.byKey(describedKey('rule:no'))));
    expect(bob.rules.value, {'establish': false});

    // Friends, as the chain makes them.
    await t.tap(alice, t.on(alice, find.byKey(describedKey('chain:Make friends'))));
    expect(find.byKey(describedKey('scan:2')), findsOneWidget, reason: 'a contact is typed or scanned');
    await tester.enterText(find.byKey(describedKey('field:2')), 'bob');
    await tester.pumpAndSettle();
    await t.tap(alice, find.byKey(describedKey('submit')));
    await t.yes(bob);
    await t.settle();
    await t.yes(bob);
    await t.settle();
    expect(alice.atoms, contains('friend(bob)'));

    // Unfriend only removes a fact: shown in its words, taken on a second tap.
    await t.tap(alice, t.on(alice, find.byKey(describedKey('row:bob'))));
    // Where she is: the super-app, the app, this screen; the app's name a tap back to its start.
    expect(t.on(alice, find.byKey(describedKey('where'))), findsOneWidget);
    expect(t.texts(alice), contains('Grassroots'));
    expect(t.texts(alice), contains('Person'));
    await t.tap(alice, t.on(alice, find.byKey(describedKey('menu'))));
    await t.tap(alice, find.text('Unfriend').last);
    expect(find.byKey(describedKey('ending:unfriend')), findsOneWidget);
    expect(alice.atoms, contains('friend(bob)'), reason: 'not yet taken');
    await t.snap('ending');
    await t.tap(alice, find.byKey(describedKey('cancel')));
    expect(alice.atoms, contains('friend(bob)'));
    await t.tap(alice, t.on(alice, find.byKey(describedKey('menu'))));
    await t.tap(alice, find.text('Unfriend').last);
    await t.tap(alice, find.byKey(describedKey('confirm')));
    for (var i = 0; i < 3 && bob.approvals.isNotEmpty; i++) {
      await t.yes(bob);
      await t.settle();
    }
    expect(alice.atoms, isNot(contains('friend(bob)')));
    await t.snap('unfriended');
  }, timeout: Timeout.none);

  testWidgets('Groups: how a list fills, a field\'s label, and THEN IT', (tester) async {
    // A field's label, as the UI style gives it (`Name IS CALLED "label"`): New group's name.
    final j = jsonDecode(File('${_programs()}/gce/ui/Groups.ui.json').readAsStringSync()) as Map<String, dynamic>;
    for (final c in j['screens']['Chats']['components'] as List) {
      if (c['schema'] == 'create') ((c['places'] as Map)['2']['look'] as Map)['label'] = 'Group name';
    }
    final t = _Play(tester, 'groups');
    await t.start('gsn', UiDescription.parse(jsonEncode(j)), const Size(390, 844));
    final alice = t.alice;
    await t.tap(alice, t.on(alice, find.byKey(describedKey('act:establish'))));
    // Groups is empty: how its items come.
    expect(t.texts(alice), contains('None yet. They come with “New group”, “Join” (by someone else).'));
    await t.snap('empty');
    await t.tap(alice, t.on(alice, find.byKey(describedKey('act:create'))));
    expect(find.widgetWithText(TextField, 'Group name'), findsOneWidget);
    await t.snap('field-label');
    await tester.enterText(find.byKey(describedKey('field:2')), 'hikers');
    await tester.pumpAndSettle();
    await t.tap(alice, find.byKey(describedKey('submit')));
    expect(alice.atoms.where((a) => a.startsWith('member(')), isNotEmpty);
    // THEN IT: the group's screen, the new member fact its subject; Back to Chats.
    expect(t.on(alice, find.byKey(describedKey('back'))), findsOneWidget);
    expect(t.on(alice, find.text('Hikers')), findsWidgets);
    expect(t.on(alice, find.byKey(describedKey('compose:post_own'))), findsOneWidget, reason: 'the group\'s own screen');
    // Post is important, and Post own, of the same label, is offered: no hint.
    expect(t.on(alice, find.byKey(describedKey('important:act:post'))), findsNothing);
    await t.snap('then-it');
    await t.tap(alice, t.on(alice, find.byKey(describedKey('back'))));
    expect(t.on(alice, find.byKey(describedKey('row:${alice.atoms.firstWhere((a) => a.startsWith('member('))}'))), findsOneWidget);
  }, timeout: Timeout.none);

  // The screens on phones and tablets: the start screen and its hint, and help, at each size, nothing overflowing.
  for (final (name, size) in const [
    ('phone-small', Size(360, 640)),
    ('phone', Size(390, 844)),
    ('phone-large', Size(430, 932)),
    ('tablet', Size(820, 1180)),
    ('tablet-landscape', Size(1194, 834)),
  ]) {
    testWidgets('every device: $name ${size.width.toInt()}x${size.height.toInt()}', (tester) async {
      final t = _Play(tester, 'device-$name');
      await t.start('gsn', _read('Groups'), size);
      final alice = t.alice;
      expect(t.on(alice, find.byKey(describedKey('help'))), findsOneWidget);
      await t.snap('start');
      await t.tap(alice, t.on(alice, find.byKey(describedKey('act:establish'))));
      await t.tap(alice, t.on(alice, find.byKey(describedKey('help'))));
      expect(find.byKey(describedKey('help:create')), findsOneWidget);
      await t.snap('help');
      await tester.tapAt(const Offset(5, 5));
      await tester.pumpAndSettle();
      await t.tap(alice, t.on(alice, find.byKey(describedKey('act:create'))));
      await tester.enterText(find.byKey(describedKey('field:2')), 'hikers');
      await tester.pumpAndSettle();
      await t.tap(alice, find.byKey(describedKey('submit')));
      await t.snap('group');
      expect(tester.takeException(), isNull);
    }, timeout: Timeout.none);
  }
}
