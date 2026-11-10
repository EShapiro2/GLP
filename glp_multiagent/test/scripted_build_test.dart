/// The scripted build (lib/main_scripted.dart, lib/scripted_person.dart;
/// GLP/phones/docs/debugging.md, "The scripted build"): two phones in one
/// process, each the app's host with its agent over a simulated layer
/// (helpers/air.dart), its person the scripted person its build names, whose
/// first act on the person channel is `sg_init(Me, Mode, Ks)` in place of
/// `superapp(Me, [])`, and its reporter on.
///
/// As nearby_boot.glp has it under :boot: alice is encounter_befriend and bob
/// encounter; both open the place; each is nearby the other; alice befriends
/// bob by picking him, bob answers yes on his card, both are told
/// `connected(K)`, and alice closes the place.  Each phone's reporter holds
/// the runtime's lines, the layer's discovery and trust levels, and the
/// notices the scripted person was shown.
///
/// A scripted phone keeps its agent's state across launches in a store of its
/// own; its scripted person acts at a launch with no saved state, and takes
/// no act at a launch that restores one ("The scripted build", item 4).  The
/// waits are on events, with no clock.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/debuglog/app_reporting.dart';
import 'package:glp_multiagent/isolate_protocol.dart' show screenTermOf;
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_multiagent/scripted_person.dart';
import 'package:glp_multiagent/superapp_host.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show generateKeyPair;

import 'helpers/air.dart';
import 'programs_dir.dart';

/// The store of a launch that is terminated as a phone's app is: the app
/// saves when it goes to the background ([SuperAppHost.save]; IGLP d70ec2e,
/// Implementation Notes, "Persistence"), and once [terminate] is called the
/// store takes no save, [terminate] completing once every save begun before
/// it is written, so that the next launch reads what this one saved and
/// nothing writes the file after.  A save begun after termination never
/// completes, as in a process that has ended.
class _Launched implements AgentStateStore {
  final FileAgentStateStore store;
  final Set<Future<void>> _writing = {};
  bool _terminated = false;

  _Launched(String path) : store = FileAgentStateStore(path);

  Future<void> _write(Future<void> Function() save) {
    if (_terminated) return Completer<void>().future;
    final f = save();
    _writing.add(f);
    return f.whenComplete(() => _writing.remove(f));
  }

  Future<void> terminate() async {
    _terminated = true;
    await Future.wait([for (final f in _writing) f.catchError((Object _) {})]);
  }

  @override
  Future<Uint8List?> load() => store.load();
  @override
  Future<void> save(Uint8List state) => _write(() => store.save(state));
  @override
  Future<AgentKeyPair?> loadKeyPair() => store.loadKeyPair();
  @override
  Future<void> saveKeyPair(AgentKeyPair keyPair) =>
      _write(() => store.saveKeyPair(keyPair));
  @override
  Future<Uint8List?> loadProgram() => store.loadProgram();
  @override
  Future<void> saveProgram(Uint8List program) =>
      _write(() => store.saveProgram(program));
  @override
  Future<bool> loadRunning() => store.loadRunning();
  @override
  Future<void> saveRunning(bool running) =>
      _terminated ? Future.value() : store.saveRunning(running);
}

/// One phone of the scripted build: its host, its layer, its reporter, and
/// the notices its scripted person was shown.
class _Phone {
  final String name;
  final SuperAppHost host;
  final AirNetwork net;
  final DebugReporter reporter;
  final List<String> notices = [];
  final List<String> errors = [];

  /// The agent's transcript.
  final List<String> lines = [];
  final List<(bool Function(), Completer<void>)> _waits = [];

  _Phone(this.name, this.host, this.net, this.reporter) {
    reportHost(reporter, host, ScreenReporter(reporter));
    final reported = host.onUserOutput;
    host.onUserOutput = (term) {
      reported?.call(term);
      notices.add(formatTerm(screenTermOf(term)));
      _changed();
    };
    host.onOutput = (line) {
      lines.add(line);
      reporter.runtimeOutput(line);
      if (line.startsWith('[ERROR]')) {
        errors.add(line);
        for (final (_, c) in _waits) {
          c.completeError(StateError('$name: $line'));
        }
        _waits.clear();
      }
    };
    net.onDeclared = _changed;
  }

  String get key => host.key;

  void _changed() => _waits.removeWhere((w) {
        if (!w.$1()) return false;
        w.$2.complete();
        return true;
      });

  Future<void> until(bool Function() test) {
    if (test()) return Future.value();
    final c = Completer<void>();
    _waits.add((test, c));
    return c.future;
  }

  /// Whether the person was shown a notice of [what] naming [peer].
  bool shown(String what, String peer) =>
      notices.any((n) => n.contains('$what(') && n.contains(peer));

  /// The reporter's buffer, event by event.
  List<Map<String, dynamic>> events() => [
        for (final l in File('${reporter.dir.path}/${reporter.run}.jsonl')
            .readAsLinesSync())
          jsonDecode(l) as Map<String, dynamic>,
      ];
}

void main() {
  test('ScriptedPerson: the first act is sg_init(Me, Mode, Ks), Ks from '
      'GLP_KNOWN, and a mode no play runs or a key that is not one is '
      'refused', () {
    final k = 'ab' * 32;
    final me = 'cd' * 32;
    final p = ScriptedPerson.parse('befriend_known', ' $k ,');
    expect(p.known, [k]);
    expect(
        formatTerm(screenTermOf(p.firstAct(me))),
        formatTerm(GStruct('sg_init', [
          GAtom(me),
          const GAtom('befriend_known'),
          GList([GAtom(k)]),
        ])));
    expect(ScriptedPerson.parse('encounter', '').known, isEmpty);
    expect(() => ScriptedPerson.parse('invite', ''), throwsFormatException);
    expect(() => ScriptedPerson.parse('', ''), throwsFormatException);
    expect(() => ScriptedPerson.parse('encounter', 'abc'), throwsFormatException);
    expect(scriptedMode, isEmpty, reason: 'no GLP_PERSON in this build');
  });

  test('encounter_befriend and encounter, scripted on two phones over the '
      'layer, befriend as nearby_boot.glp does, and the reporter holds the '
      'events', () async {
    final programs = programsDir();
    final tmp = Directory.systemTemp.createTempSync('glp_scripted_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final air = Air();

    Future<_Phone> phone(String name, String mode) async {
      final kp = generateKeyPair();
      final net = AirNetwork(kp, air);
      final reporter = await DebugReporter.open(
        dir: Directory('${tmp.path}/$name'),
        password: 'p',
        base: 'http://127.0.0.1:9/log',
        period: const Duration(hours: 1),
      );
      addTearDown(reporter.close);
      final script = ScriptedPerson(mode);
      final host = SuperAppHost(
        agentId: name,
        program: '$programs/social/graph/core',
        rootSelfGlpPath: '$programs/self.glp',
        network: ReportingGlpNetwork(net, reporter),
        keyPair: kp,
        manifest: superappManifest,
        firstAct: script.firstAct,
      );
      addTearDown(host.dispose);
      return _Phone(name, host, net, reporter);
    }

    final alice = await phone('alice', 'encounter_befriend');
    final bob = await phone('bob', 'encounter');
    await alice.host.start();
    await bob.host.start();

    await alice.until(() => alice.shown('connected', bob.key));
    await bob.until(() => bob.shown('connected', alice.key));
    await alice.until(() => alice.net.declared.length >= 6);

    expect(alice.shown('nearby', bob.key), isTrue);
    expect(bob.shown('nearby', alice.key), isTrue);
    expect(bob.shown('befriend', alice.key), isTrue);
    expect(alice.net.declared, [
      'pan closed', 'lan closed', // at start
      'pan open', 'lan open', // the place opened, GPE:1
      'pan closed', 'lan closed', // closed once they are friends
    ]);
    expect(bob.net.declared, ['pan closed', 'lan closed', 'pan open', 'lan open']);
    expect(alice.errors, isEmpty);
    expect(bob.errors, isEmpty);

    for (final p in [alice, bob]) {
      final other = p == alice ? bob : alice;
      final kinds = [for (final e in p.events()) '${e['source']}/${e['kind']}'];
      expect(kinds, contains('runtime/output'), reason: p.name);
      expect(kinds, contains('runtime/log'), reason: p.name);
      expect(
          p.events().any((e) =>
              e['kind'] == 'userOutput' &&
              (e['term'] as String).contains('connected(') &&
              (e['term'] as String).contains(other.key)),
          isTrue,
          reason: '${p.name}: the scripted person was shown connected');
      expect(
          p.events().any((e) =>
              e['kind'] == 'onPeerDiscovered' && e['peer'] == other.key),
          isTrue,
          reason: '${p.name}: the layer discovered ${other.name}');
      expect(
          p.events().where((e) => e['kind'] == 'setTrustLevel').length,
          p.net.declared.length,
          reason: '${p.name}: every trust level as set');
    }
  }, timeout: const Timeout(Duration(minutes: 3)));

  test('a scripted phone keeps its agent\'s state: its scripted person acts '
      'at the launch with no saved state and takes no act at the launch '
      'that restores it', () async {
    final programs = programsDir();
    final tmp = Directory.systemTemp.createTempSync('glp_scripted_state_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final air = Air();

    Future<(_Phone, _Launched)> launch(
        String name, String mode, AirKeyPair kp) async {
      final net = AirNetwork(kp, air);
      final store =
          _Launched('${tmp.path}/$name/agents/person-$mode.state');
      final reporter = await DebugReporter.open(
        dir: Directory('${tmp.path}/$name-log'),
        password: 'p',
        base: 'http://127.0.0.1:9/log',
        period: const Duration(hours: 1),
      );
      addTearDown(reporter.close);
      final host = SuperAppHost(
        agentId: name,
        program: '$programs/social/graph/core',
        rootSelfGlpPath: '$programs/self.glp',
        network: ReportingGlpNetwork(net, reporter),
        keyPair: kp,
        manifest: superappManifest,
        firstAct: ScriptedPerson(mode).firstAct,
        stateStore: store,
      );
      addTearDown(host.dispose);
      return (_Phone(name, host, net, reporter), store);
    }

    final aliceKey = generateKeyPair();
    final (alice, aliceStore) =
        await launch('alice', 'encounter_befriend', aliceKey);
    final (bob, _) = await launch('bob', 'encounter', generateKeyPair());
    await alice.host.start();
    await bob.host.start();
    expect(alice.host.agent.firstStart, isTrue);
    expect(alice.lines.where((l) => l.startsWith('> sg_init(')), hasLength(1),
        reason: 'the scripted person\'s first act, at the launch with no '
            'saved state');
    await alice.until(() => alice.shown('connected', bob.key));
    await alice.until(() => alice.net.declared.length >= 6);
    expect(alice.errors, isEmpty);

    // Alice's app goes to the background, which saves her agent's state, and
    // is terminated: her phone leaves the air, and her store takes no save
    // after what is being written.
    expect(await alice.host.save(), isTrue);
    alice.net.leave();
    await aliceStore.terminate();
    alice.host.dispose();

    // The next launch, from the same store and under the same key.
    final (again, _) = await launch('alice', 'encounter_befriend', aliceKey);
    await again.host.start();
    expect(again.host.agent.firstStart, isFalse);
    expect(again.lines, contains('[INIT] Restored the agent\'s saved state'));
    expect(again.lines.where((l) => l.startsWith('> ')), isEmpty,
        reason: 'the scripted person takes no act at a launch that restores '
            'the state');
    expect(again.errors, isEmpty);
  }, timeout: const Timeout(Duration(minutes: 3)));
}
