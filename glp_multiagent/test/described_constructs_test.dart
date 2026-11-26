/// The constructs of the UI structure and the UI style the app draws, on GSG's app, Friendship's, with
/// `programs/gce/ui/Friendship.ui` and `.style` (lib/ui_runtime/described.dart), on Alice's and Bob's phones as
/// `described_screen_test.dart` runs them: an empty screen points to where to start; a button by its plain label
/// (`IS CALLED`); a chain, `Make friends: Acquaint AND Befriend`, whose second act is asked once the first is
/// done; and the notes that tell the person that an act she asked is done.
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
  testWidgets('Friendship: the hint, labels, the chain Make friends, and the notes', (tester) async {
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

    // The start screen is empty: it points to its first action, by its label.
    expect(on(alice, find.byKey(describedKey('hint'))), findsOneWidget);
    expect(on(alice, find.text('Nothing here yet. Start with “Set up my identity”.')), findsOneWidget);
    expect(on(alice, find.text('Set up my identity')), findsOneWidget);

    await tap(alice, on(alice, find.byKey(describedKey('act:establish'))));
    await tap(bob, on(bob, find.byKey(describedKey('act:establish'))));
    expect(on(alice, find.text('Set up my identity: done')), findsOneWidget);
    // Now the first marked action is offered, and the hint points to it.
    expect(on(alice, find.text('Nothing here yet. Start with “Make friends”.')), findsOneWidget);
    expect(on(alice, find.text('Add someone')), findsOneWidget);

    // Make friends: Bob's key, once; Acquaint is asked, then, once Bob says yes, Befriend.
    await tap(alice, on(alice, find.byKey(describedKey('chain:Make friends'))));
    await tester.enterText(find.byKey(describedKey('field:2')), 'bob');
    await tester.pumpAndSettle();
    await tap(alice, find.byKey(describedKey('submit')));
    await yes(bob);
    expect(alice.atoms, contains('knows(bob)'));
    await settle();
    expect(on(alice, find.text('Make friends: done')), findsOneWidget);
    await yes(bob);
    await settle();
    expect(alice.atoms, contains('friend(bob)'));
    expect(bob.atoms, contains('friend(alice)'));
    expect(on(alice, find.text('Befriend: done')), findsOneWidget);
    // The notes fade.
    await tester.pump(const Duration(seconds: 9));
    expect(on(alice, find.text('Befriend: done')), findsNothing);
  }, timeout: Timeout.none);
}
