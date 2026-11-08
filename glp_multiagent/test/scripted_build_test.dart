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
/// notices the scripted person was shown.  The waits are on events, with no
/// clock.
library;

import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/debuglog/app_reporting.dart';
import 'package:glp_multiagent/isolate_protocol.dart' show screenTermOf;
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_multiagent/scripted_person.dart';
import 'package:glp_multiagent/superapp_host.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show generateKeyPair;

import 'helpers/air.dart';
import 'programs_dir.dart';

/// One phone of the scripted build: its host, its layer, its reporter, and
/// the notices its scripted person was shown.
class _Phone {
  final String name;
  final SuperAppHost host;
  final AirNetwork net;
  final DebugReporter reporter;
  final List<String> notices = [];
  final List<String> errors = [];
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
}
