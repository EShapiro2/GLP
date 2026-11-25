/// The screens of GSG's app, Friendship's, drawn from an interface description in the UI language of the
/// Grassroots UI paper (`programs/gce/ui/Friendship.ui`, compiled with the contract into
/// `Friendship.ui.json`; lib/ui_runtime/described.dart): Alice's and Bob's phones, each running the app's
/// mini-app over one conversation, as `apps_screens_test.dart` runs them, every act a tap on a control the
/// description draws.
///
/// Alice and Bob establish their identities and Alice comes to know Bob by his key, from the start screen;
/// Bob, asked, says yes; Alice opens Bob's screen from her contacts and befriends him there; Bob, asked, says
/// yes; each then holds `friend` of the other, and Bob's screen offers Alice Unfriend and no longer
/// Befriend.
library;

import 'dart:io';
import 'dart:ui' as dui;

import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:glp_multiagent/ui_runtime/look.dart';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart' show runtimeTermOf, screenTermOf;
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/described.dart';
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

const Size _phone = Size(390, 844);

/// Where the screens are written, a PNG of both phones after each step, where `DESCRIBED_SHOTS` names a
/// directory; nothing is written otherwise.
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
  int fed = 0;
  late final UiRuntime ui = UiRuntime(
      manifest: const Manifest(title: 'gsg', panels: [Panel(id: 'gsg', name: 'GSG', constructs: true)], activity: []),
      onSend: unsent.add);
  _Person(this.name, this.grants, this.draws);

  SocialState get state => SocialState.read(ui.bridge.construct(0)?.view).$2!;
  List<String> get atoms => [for (final a in state.atoms) formatTerm(a)];
  List<OpenConstruct> get approvals => [
        for (final c in ui.bridge.constructs)
          if (c.id != 0 && ctorArgs(c.view).$1 == 'approval') c
      ];
}

void main() {
  testWidgets('Friendship from its interface description: acquaint, befriend, on two phones', (tester) async {
    final programs = programsDir();
    if (_shots != null) await _loadFonts();
    final all = GlobalKey();
    var shot = 0;
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
      File('${_shots!}/${(++shot).toString().padLeft(2, '0')}-$what.png').writeAsBytesSync(png!);
    }
    final ui = UiDescription.parse(File('$programs/gce/ui/Friendship.ui.json').readAsStringSync());
    final engine = GlpEngine(rootSelfGlpPath: '$programs/self.glp');
    expect(engine.loadProgram('$programs/apps/gsg'), isTrue);
    final play = engine.postGoal(
        'miniapp(alice, ch(G0?, D0), ch([conversation(bob, ch(C_bob_alice?, C_alice_bob))], U0)), '
        'miniapp(bob, ch(G1?, D1), ch([conversation(alice, ch(C_alice_bob?, C_bob_alice))], U1))',
        inputs: ['G0', 'G1']);
    final alice = _Person('alice', play.inputs['G0']!, play.variables['D0']!);
    final bob = _Person('bob', play.inputs['G1']!, play.variables['D1']!);
    final persons = [alice, bob];

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

    tester.view.physicalSize = Size(2 * _phone.width, _phone.height);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);
    drain();
    feed();
    await tester.pumpWidget(MaterialApp(
      debugShowCheckedModeBanner: false,
      theme: _shots == null ? null : Looks.readSync('$programs/self.glp').of('gsg').theme(fontFamily: 'Roboto'),
      home: Scaffold(
        body: DescribedUi(
          ui: ui,
          child: RepaintBoundary(key: all, child: Row(children: [
            for (final p in persons)
              RepaintBoundary(
                key: p.shot,
                child: SizedBox.fromSize(size: _phone, child: AgentSurface(agentId: p.name, runtime: p.ui, muteNotices: true)),
              ),
          ])),
        ),
      ),
    ));
    await snap('start');
    await tester.pumpAndSettle();

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
      await tap(p, on(p, find.descendant(
          of: find.byKey(constructKey(p.approvals.single.id)),
          matching: find.byWidgetPredicate((w) => w is ElevatedButton && w.child is Text && (w.child! as Text).data == 'yes'))));
    }

    // Each establishes an identity, from the start screen.
    await tap(alice, on(alice, find.byKey(describedKey('act:establish'))));
    await tap(bob, on(bob, find.byKey(describedKey('act:establish'))));
    expect(alice.atoms, containsAll(['identity', 'person']));
    expect(on(alice, find.byKey(describedKey('act:establish'))), findsNothing);
    await snap('established');

    // Alice acquaints Bob by his key: the field the description types, then the request; Bob is asked.
    await tap(alice, on(alice, find.byKey(describedKey('act:acquaint'))));
    await tester.enterText(find.byKey(describedKey('field:2')), 'bob');
    await tester.pumpAndSettle();
    if (_shots != null) {
      final b = all.currentContext!.findRenderObject()! as RenderRepaintBoundary;
      final png = await tester.runAsync(() async {
        final image = await b.toImage(pixelRatio: 2);
        final data = await image.toByteData(format: dui.ImageByteFormat.png);
        image.dispose();
        return data!.buffer.asUint8List();
      });
      File('${_shots!}/${(++shot).toString().padLeft(2, '0')}-acquaint-form.png').writeAsBytesSync(png!);
    }
    await tap(alice, find.byKey(describedKey('submit')));
    await snap('bob-asked');
    await yes(bob);
    expect(alice.atoms, contains('knows(bob)'));

    // Bob in Alice's contacts; his screen offers Befriend, from the subject, with nothing to fill.
    await snap('contacts');
    await tap(alice, on(alice, find.byKey(describedKey('row:bob'))));
    await snap('bob-screen');
    expect(on(alice, find.byKey(describedKey('act:unfriend:bob'))), findsNothing);
    await tap(alice, on(alice, find.byKey(describedKey('act:befriend:bob'))));
    await snap('befriend-asked');
    await yes(bob);
    expect(alice.atoms, contains('friend(bob)'));
    expect(bob.atoms, contains('friend(alice)'));

    // Now Unfriend, and no Befriend.
    expect(on(alice, find.byKey(describedKey('act:unfriend:bob'))), findsOneWidget);
    expect(on(alice, find.byKey(describedKey('act:befriend:bob'))), findsNothing);

    await snap('friends');
    // Back to the start screen.
    await tap(alice, on(alice, find.byKey(describedKey('back'))));
    expect(on(alice, find.byKey(describedKey('row:bob'))), findsOneWidget);
  }, timeout: Timeout.none);
}
