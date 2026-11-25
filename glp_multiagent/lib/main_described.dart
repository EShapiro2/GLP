/// A contract's app drawn from an interface description, on two phones side by side: the contract's
/// mini-app (`programs/apps/<app>`, its canonical platform compiled from the contract) running for Alice and
/// for Bob in one engine, over one conversation, as the app's plays run it, and each phone drawn from the
/// description in the UI language of the Grassroots UI paper, compiled with the contract
/// (`programs/gce/ui/<NAME>.ui.json`; lib/ui_runtime/described.dart).  The agents are GLP's; nothing on the
/// screens is simulated.  From `glp_multiagent/`, on a desktop with the repository on disk:
///
///     flutter run -d macos -t lib/main_described.dart
///
/// with `--dart-define=APP=gsg --dart-define=UI=gce/ui/Friendship.ui.json`, the default.
library;

import 'dart:io';

import 'package:flutter/material.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/external_io.dart' show InputInjector;
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/terms.dart' as rt;

import 'glp_sources.dart';
import 'isolate_protocol.dart' show runtimeTermOf, screenTermOf;
import 'ui_runtime/agent_surface.dart';
import 'ui_runtime/described.dart';
import 'ui_runtime/look.dart';
import 'ui_runtime/manifest.dart';
import 'ui_runtime/runtime.dart';
import 'ui_runtime/term.dart';

const String _app = String.fromEnvironment('APP', defaultValue: 'gsg');
const String _ui = String.fromEnvironment('UI', defaultValue: 'gce/ui/Friendship.ui.json');

void main() => runApp(const DescribedDemo());

class DescribedDemo extends StatefulWidget {
  const DescribedDemo({super.key});
  @override
  State<DescribedDemo> createState() => _DescribedDemoState();
}

/// A person at a phone: their grants, the draws their agent writes, and their screen.
class _Phone {
  final String name;
  final InputInjector grants;
  final HeapCell draws;
  final List<GTerm> unsent = [];
  int fed = 0;
  late final UiRuntime ui;
  _Phone(this.name, this.grants, this.draws, void Function() changed) {
    ui = UiRuntime(
        manifest: const Manifest(title: _app, panels: [Panel(id: _app, name: _app, constructs: true)], activity: []),
        onSend: (g) {
          unsent.add(g);
          changed();
        });
  }
}

class _DescribedDemoState extends State<DescribedDemo> {
  GlpEngine? _engine;
  PostedGoal? _play;
  final List<_Phone> _phones = [];
  UiDescription? _description;
  ThemeData? _theme;
  String _status = 'Starting…';

  @override
  void initState() {
    super.initState();
    WidgetsBinding.instance.addPostFrameCallback((_) => _start());
  }

  Future<void> _start() async {
    try {
      final glp = await resolveGlpPaths();
      final programs = File(glp.rootSelfGlp).parent.path;
      _description = UiDescription.parse(File('$programs/$_ui').readAsStringSync());
      try {
        _theme = Looks.readSync(glp.rootSelfGlp).of(_app).theme();
      } catch (_) {}
      final engine = GlpEngine(rootSelfGlpPath: glp.rootSelfGlp);
      if (!engine.loadProgram('$programs/apps/$_app')) throw 'programs/apps/$_app does not load';
      final play = engine.postGoal(
          'miniapp(alice, ch(G0?, D0), ch([conversation(bob, ch(C_bob_alice?, C_alice_bob))], U0)), '
          'miniapp(bob, ch(G1?, D1), ch([conversation(alice, ch(C_alice_bob?, C_bob_alice))], U1))',
          inputs: ['G0', 'G1']);
      _engine = engine;
      _play = play;
      _phones
        ..add(_Phone('alice', play.inputs['G0']!, play.variables['D0']!, _step))
        ..add(_Phone('bob', play.inputs['G1']!, play.variables['D1']!, _step));
      _drain();
      _feed();
      setState(() => _status = 'Ready');
    } catch (e) {
      setState(() => _status = 'Error: $e');
    }
  }

  void _drain() => _play!.scheduler.drainToQuiescence(maxCycles: 20000000);

  rt.Term _deep(rt.Term t) {
    final d = _engine!.runtime.heap.dereference(t);
    return d is rt.StructTerm ? rt.StructTerm(d.functor, [for (final a in d.args) _deep(a)]) : d;
  }

  /// Hand each phone what its agent drew since last time.
  void _feed() {
    for (final p in _phones) {
      var cell = _engine!.runtime.heap.dereference(rt.VarRef(p.draws));
      var i = 0;
      while (cell is rt.StructTerm && cell.functor == '.' && cell.args.length == 2) {
        if (i++ >= p.fed) {
          p.ui.handle(screenTermOf(_deep(cell.args[0])));
          p.fed++;
        }
        cell = _engine!.runtime.heap.dereference(cell.args[1]);
      }
    }
  }

  /// Put each phone's grants on its person channel and run the agents until they rest.
  void _step() {
    Future.microtask(() {
      for (var more = true; more;) {
        more = false;
        for (final p in _phones) {
          while (p.unsent.isNotEmpty) {
            more = true;
            p.grants.inject(runtimeTermOf(p.unsent.removeAt(0))).forEach(_engine!.runtime.gq.enqueue);
            _drain();
          }
        }
        _feed();
      }
      if (mounted) setState(() {});
    });
  }

  @override
  Widget build(BuildContext context) {
    final d = _description;
    return MaterialApp(
      debugShowCheckedModeBanner: false,
      theme: _theme ?? ThemeData(useMaterial3: true),
      home: Scaffold(
        backgroundColor: const Color(0xFF2B2B33),
        body: d == null || _phones.isEmpty
            ? Center(child: Text(_status, style: const TextStyle(color: Colors.white70)))
            : DescribedUi(
                ui: d,
                child: Center(
                  child: SingleChildScrollView(
                    scrollDirection: Axis.horizontal,
                    child: Row(children: [
                      for (final p in _phones)
                        Padding(
                          padding: const EdgeInsets.all(14),
                          child: Column(mainAxisSize: MainAxisSize.min, children: [
                            Text("${p.name[0].toUpperCase()}${p.name.substring(1)}'s phone",
                                style: const TextStyle(color: Colors.white70)),
                            const SizedBox(height: 8),
                            Container(
                              width: 390,
                              height: 760,
                              padding: const EdgeInsets.all(10),
                              decoration: BoxDecoration(color: Colors.black, borderRadius: BorderRadius.circular(40)),
                              child: ClipRRect(
                                borderRadius: BorderRadius.circular(30),
                                child: ColoredBox(
                                  color: Colors.white,
                                  child: ScaffoldMessenger(
                                    child: Scaffold(body: AgentSurface(agentId: p.name, runtime: p.ui, muteNotices: true)),
                                  ),
                                ),
                              ),
                            ),
                          ]),
                        ),
                    ]),
                  ),
                ),
              ),
      ),
    );
  }
}
